# Set Election and previous election
res_co_el <- vector("list", length = 3)

for (election in c(2009, 2013, 2017)) {
  #election <- 2013
  election_l1 <- election - 4
  
  
  
  
  
  
  # Split into training and test data.
  
  train <-
    full_df[(full_df$election < election &
               full_df$election != 1990) |
              (full_df$election == 1990 & full_df$east != 1) , ]
  
  test <- full_df[full_df$election == election,]
  
  test$weight <- 1
  
  # Update swing weights if available.
  
  if (weights) {
    for (i in 1:nrow(test)) {
      if (paste0(test$partei[i], "_Z") %in% colnames(wkrweights0913) &
          test$wkr_name[i] %in% rownames(wkrweights0913)) {
        test$weight[i] <-
          wkrweights0913[rownames(wkrweights0913) == test$wkr_name[i], colnames(wkrweights0913) == paste0(test$partei[i], "_Z")]
      }
      
      if (paste0(test$partei[i], "_Z") %in% colnames(weights0913) &
          !test$wkr_name[i] %in% rownames(weights0913)) {
        test$weight[i] <-
          weights0913[weights0913$L_code == test$land[i], colnames(weights0913) == paste0(test$partei[i], "_Z")]
      }
      
      if (paste0(test$partei[i], "_Z") == "CSU_Z" &
          test$wkr_name[i] %in% rownames(wkrweights0913)) {
        test$weight[i] <-
          wkrweights0913[rownames(wkrweights0913) == test$wkr_name[i], colnames(wkrweights0913) == "CDU_Z"]
      }
      
      if (paste0(test$partei[i], "_Z") == "CSU_Z" &
          !test$wkr_name[i] %in% rownames(weights0913)) {
        test$weight[i] <-
          weights0913[weights0913$L_code == test$land[i], colnames(weights0913) == "CDU_Z"]
      }
      
      
    }
  }
  
  
  
  
  
  ff <-
    "resp_E ~ ncand + propPlatz + resp_Z + res_l1_E + formercand + east + female + incumbent + akad + incumbent_in_wkr"
  
  reg <-
    lm(ff,
       data = train)
  
  # Number of simulations for model uncertainty (lm and neural net)
  
  mu_nsim <- 25
  
  S <- mvrnorm(n = mu_nsim, coef(reg), vcov(reg))
  
  rf_df <-
    model.frame(as.formula(ff),
                data = train)
  
  
  rf_df <- rf_df[sample(1:nrow(rf_df), nrow(rf_df)), ]
  
  
  # The test set without vote share predictions is using the last result.
  
  ff_test <-
    "resp_E ~ ncand + propPlatz + res_l1_Z + res_l1_E + formercand + east + female + incumbent + akad + incumbent_in_wkr"
  
  rf_test <-
    model.frame(as.formula(ff_test),
                data = test)
  
  # That it works with lm the variables in training and test have to have the
  # same names though.
  
  colnames(rf_df) <-
    colnames(rf_test) <-
    c(
      "resp_E",
      "ncand",
      "propPlatz",
      "resp_Z",
      "res_l1_E",
      "formercand",
      "east",
      "female",
      "incumbent",
      "akad",
      "incumbent_in_wkr"
    )
  
  
  y <- rf_df$resp_E
  
  # The neural net needs the predictor variables in a matrix.
  
  x <- as.matrix(rf_df[, 2:(ncol(rf_df))])
  
  # Rescale the data for better convergence of the neural net.
  
  means <- apply(x , 2, mean, na.rm = T)
  sds <- apply(x , 2, sd, na.rm = T)
  
  # Don't rescale categorical/binary features
  
  means[c(2, 5:10)] <- 0
  sds[c(2, 5:10)] <- 1
  
  x_rescaled <-
    as.matrix(sweep(sweep(x, 2, means), 2, sds, "/"))
  
  test_y <- rf_test$resp_E
  
  test_x <- as.matrix(rf_test[, 2:(ncol(rf_test))])
  
  # Rescale the test set with the same means and sds as the training set.
  
  test_x_rescaled <-
    as.matrix(sweep(sweep(test_x, 2, means), 2, sds, "/"))
  
  k_clear_session()
  
  # Dropout to prevent overfitting and for model uncertainty of the neural net.
  
  drop_out <- layer_dropout(rate = 0.1)
  
  input <- layer_input(shape = c(ncol(test_x)))
  
  output <- input %>%
    layer_dense(
      units = 128,
      activation = "tanh",
      kernel_initializer = "glorot_normal",
      input_shape = c(ncol(x))
    ) %>%
    
    drop_out(training = T) %>%
    layer_dense(
      units = 64,
      activation = "tanh",
      kernel_initializer = "glorot_normal"
    ) %>%
    drop_out(training = T) %>%
    
    layer_dense(units = 1, activation = "sigmoid")
  
  model <- keras_model(input, output)
  
  
  model %>% compile(optimizer = "adam",
                    loss = "mse")
  
  callbacks_list <- list(
    callback_early_stopping(monitor = "loss",
                            patience = 25),
    callback_reduce_lr_on_plateau(
      factor = 0.2,
      patience = 5,
      min_lr = 0.0001
    )
  )
  
  shuf_ind <- sample(nrow(x_rescaled), nrow(x_rescaled))
  
  nn_history <- model %>% fit(
    x = x_rescaled[shuf_ind,],
    y = y[shuf_ind],
    validation_split = 0.1,
    epochs = 500,
    batch_size = 500,
    callbacks = callbacks_list
  )
  
  # Cutoff has to be one of 92, 36 or 2
  
  res_co <- vector("list", length = 3)
  
  for (cutoff in c(92, 36, 2)) {
    draws <-
      readRDS(paste0(
        "../raw-data/draws_forcast_levels_",
        election,
        "_",
        cutoff,
        ".RDS"
      ))
    
    forecast <- draws$forecast
    
    #nsim <- nrow(forecast)
    
    nsim <- 500
    adjustOrder <-
      match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"),
            draws$party_names)
    
    forecast <- draws$forecast[, adjustOrder]
    
    
    colnames(forecast) <-
      c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")
    
    
    res_el <- btw_bund_res[paste0(election_l1),]
    
    
    sim.swing <- -sweep(-forecast, 2, -res_el)
    sim.prop.swing <- t(apply(sim.swing, 1, function(x)
      x / res_el))
    
    zs_pred <- matrix(0, nrow = nrow(test), ncol = nrow(sim.swing))
    
    
    for (i in 1:nrow(test)) {
      if (test[i, "partei"] %in% colnames(sim.prop.swing)) {
        zs_pred[i, ] <-
          test[i, "res_l1_Z"] + sim.prop.swing[, colnames(sim.prop.swing) %in% test[i, "partei"]] * test[i, "res_l1_Z"] * (test[i, "weight"])
      }
      if (test[i, "partei"] == "CSU") {
        zs_pred[i, ] <-
          test[i, "res_l1_Z"] + sim.prop.swing[, "CDU"] * test[i, "res_l1_Z"] * (test[i, "weight"])
      }
      
      if (!(test[i, "partei"] %in% colnames(sim.prop.swing)) &
          test[i, "partei"] != "CSU") {
        zs_pred[i, ] <-
          test[i, "res_l1_Z"] + sim.prop.swing[, "And"] * test[i, "res_l1_Z"] * (test[i, "weight"])
      }
    }
    
    
    
    
    test_sim <-
      data.frame(
        test$resp_E,
        test$ncand,
        test$propPlatz,
        zs_pred[, 2],
        test$res_l1_E,
        test$formercand,
        test$east,
        test$female,
        test$incumbent,
        test$akad,
        test$incumbent_in_wkr
      )
    
    
    
    
    colnames(test_sim) <-
      c(
        "resp_E",
        "ncand",
        "propPlatz",
        "resp_Z",
        "res_l1_E",
        "formercand",
        "east",
        "female",
        "incumbent",
        "akad",
        "incumbent_in_wkr"
      )
    
    test_y <- test_sim$resp_E
    
    vals <-
      t(sapply(aggregate(test, list(test$wkr), function(x)
        x)$partei, '[', seq(max(
          sapply(aggregate(test, list(test$wkr), function(x)
            x)$partei, length)
        ))))
    
    res_list <- vector("list", length = nsim)
    
    for (zsim in 1:nsim) {
      test_sim$resp_Z <- zs_pred[, zsim]
      
      
      
      test_x <- as.matrix(test_sim[, 2:(ncol(test_sim))])
      
      test_x_rescaled <-
        as.matrix(sweep(sweep(test_x, 2, means), 2, sds, "/"))
      
      
      
      preds <- vector("list", length = mu_nsim)
      
      for (i in 1:mu_nsim) {
        preds[[i]] <- predict(model, x = test_x_rescaled)
      }
      
      preds <- do.call("cbind", preds)
      
      
      rmse_nn <- mean(apply(preds, 2, rmse, obs = test_y))
      
      
      for (j in 1:299) {
        preds[test$wkr == j,] <-
          sweep(preds[test$wkr == j, ], 2, colSums(preds[test$wkr == j,]), "/")
      }
      
      
      
      
      tmp_winner <- future_lapply(1:mu_nsim, function(x) {
        select <-
          cbind(1:299, unlist(lapply(
            aggregate(-preds[, x], list(test$wkr), order)[, 2], `[[`, 1
          )))
        
        vals[select]
      })
      
      
      reg_test_mat <- cbind(1, test_x)
      
      reg_preds <-
        reg_test_mat %*% t(S) + matrix(rnorm(nrow(reg_test_mat) * mu_nsim, 0, sd(residuals(reg))),
                                       nrow = nrow(reg_test_mat),
                                       ncol = mu_nsim)
      
      rmse_reg <- apply(reg_preds, 2, rmse, obs = test_y)
      
      reg_preds[reg_preds < 0] <- 0
      
      for (j in 1:299) {
        reg_preds[test$wkr == j,] <-
          sweep(reg_preds[test$wkr == j, ], 2, colSums(reg_preds[test$wkr == j,]), "/")
      }
      
      
      
      tmp_reg_winner <- future_lapply(1:mu_nsim, function(x) {
        select <-
          cbind(1:299, unlist(lapply(
            aggregate(-reg_preds[, x], list(test$wkr), order)[, 2], `[[`, 1
          )))
        
        vals[select]
      })
      
      res_list[[zsim]] <-
        list(
          winner_nn = tmp_winner,
          winner_reg = tmp_reg_winner,
          rmse_nn = rmse_nn,
          rmse_reg = rmse_reg
        )
      cat(zsim, "of", nsim, "\n")
      
    }
    res_co[[paste0(cutoff)]] <- res_list
  }
  res_co_el[[paste0(election)]] <- res_co
}

saveRDS(res_co_el, "../processed-data/final_res_09_17.RDS")