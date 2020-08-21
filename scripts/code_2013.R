# Source all the libraries needed for this analysis.
# Automatically installs if the package isn't installed.

source("01-load-packages.R")

# If the main data file does not exist, this will preprocess the data from the
# original source data files.

if (!"btw_candidates_1983-2017.csv" %in% list.files("data")) {
  source("02-data-preprocessing.R")
}



source("res05_13.R")

rmse <- function(pred, obs) {
  sqrt(mean((pred
             - obs) ^ 2))
}

# Set Election and previous election
res_co_el <- vector("list", length = 3)

for(election in c(2009, 2013, 2017)){
#election <- 2013
election_l1 <- election - 4




# Read in the data.

full_df <-
  read.csv2("data/btw_candidates_1983-2017.csv", stringsAsFactors = F)

# Split into training and test data.

train <-
  full_df[(full_df$election < election &
             full_df$election != 1990) |
            (full_df$election == 1990 & full_df$east != 1) ,]

test <- full_df[full_df$election == election, ]

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


rf_df <- rf_df[sample(1:nrow(rf_df), nrow(rf_df)),]


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
  layer_dense(units = 64,
              activation = "tanh",
              kernel_initializer = "glorot_normal") %>%
  drop_out(training = T) %>%
  
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(input, output)


model %>% compile(optimizer = "adam",
                  loss = "mse")

callbacks_list <- list(
  callback_early_stopping(monitor = "loss",
                          patience = 25),
  callback_reduce_lr_on_plateau(factor = 0.2, patience = 5, min_lr = 0.0001)
)

shuf_ind <- sample(nrow(x_rescaled), nrow(x_rescaled))

nn_history <- model %>% fit(
  x = x_rescaled[shuf_ind, ],
  y = y[shuf_ind],
  validation_split = 0.1,
  epochs = 500,
  batch_size = 500,
  callbacks = callbacks_list
)


# preds <- matrix(NA, nrow = nrow(test_x), ncol = mu_nsim)
# for (i in 1:mu_nsim) {
#   preds[, i] <- predict(model, x = test_x_rescaled)
#   cat(i, "\n")
# }
# 
# par(mfrow = c(1, 1))
# plot(
#   rowMeans(preds),
#   test_y,
#   main = paste0("Neural Net - Wahlkreisvorhersage ", election,  " oos"),
#   ylab = paste0("Ergebnis ", election),
#   xlab = "Vorhersage",
#   xlim = c(0, 1),
#   ylim = c(0, 1),
#   pch = ifelse(test$partei == "AFD", 2, 1),
#   col = ifelse(
#     test$east == 1,
#     adjustcolor("red", 0.7),
#     adjustcolor("black", 0.7)
#   ),
#   bty = "n",
#   las = 1
# )
# 
# 
# segments(
#   x0 = apply(preds, 1, quantile, 0.025),
#   x1 = apply(preds, 1, quantile, 0.975),
#   y0 = test_y,
#   col = adjustcolor("black", alpha = 0.3)
# )
# 
# for (i in 1:mu_nsim) {
#   points(
#     preds[, i],
#     test_y,
#     pch = 1,
#     col = ifelse(
#       test$east == 1,
#       adjustcolor("red", 0.01),
#       adjustcolor("black", 0.01)
#     )
#   )
# }
# 
# abline(0, 1, lty = "dashed")
# 
# reg_test_mat <- cbind(1, test_x)
# 
# reg_preds <- reg_test_mat %*% t(S) + matrix(rnorm(nrow(reg_test_mat)*mu_nsim, 0, sd(residuals(reg))), nrow = nrow(reg_test_mat), ncol = mu_nsim)
# 
# plot(
#   rowMeans(reg_preds),
#   test$resp_E,
#   main = "Linear Model - Wahlkreisvorhersage 2017 oos",
#   ylab = "Ergebnis 2017",
#   xlab = "Vorhersage",
#   xlim = c(0, 1),
#   ylim = c(0, 1),
#   pch = ifelse(test$partei == "AFD", 2, 1),
#   col = ifelse(
#     test$east == 1,
#     adjustcolor("red", 0.7),
#     adjustcolor("black", 0.7)
#   ),
#   bty = "n",
#   las = 1
# )
# 
# segments(
#   x0 = apply(reg_preds, 1, quantile, 0.025),
#   x1 = apply(reg_preds, 1, quantile, 0.975),
#   y0 = test_y,
#   col = adjustcolor("black", alpha = 0.3)
# )
# 
# for (i in 1:mu_nsim) {
#   points(
#     reg_preds[, i],
#     test_y,
#     pch = 1,
#     col = ifelse(
#       test$east == 1,
#       adjustcolor("red", 0.01),
#       adjustcolor("black", 0.01)
#     )
#   )
# }
# 
# abline(0, 1, lty = "dashed")
# 
# 
# 
# rmse(rowMeans(preds), test_y)
# 
# rmse(rowMeans(reg_preds), test_y)



# Cutoff has to be one of 92, 36 or 2

res_co <- vector("list", length = 3)

for(cutoff in c(92, 36, 2)) {

draws <-
  readRDS(paste0("data/draws_forcast_levels_", election, "_", cutoff, ".RDS"))

forecast <- draws$forecast

#nsim <- nrow(forecast)

nsim <- 500
adjustOrder <-
  match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"),
        draws$party_names)

forecast <- draws$forecast[, adjustOrder]


colnames(forecast) <-
  c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")


res_el <- btw_bund_res[paste0(election_l1), ]


sim.swing <- -sweep(-forecast, 2,-res_el)
sim.prop.swing <- t(apply(sim.swing, 1, function(x)
  x / res_el))

zs_pred <- matrix(0, nrow = nrow(test), ncol = nrow(sim.swing))


for (i in 1:nrow(test)) {
  if (test[i, "partei"] %in% colnames(sim.prop.swing)) {
    zs_pred[i,] <-
      test[i, "res_l1_Z"] + sim.prop.swing[, colnames(sim.prop.swing) %in% test[i, "partei"]] * test[i, "res_l1_Z"] * (test[i, "weight"])
  }
  if (test[i, "partei"] == "CSU") {
    zs_pred[i,] <-
      test[i, "res_l1_Z"] + sim.prop.swing[, "CDU"] * test[i, "res_l1_Z"] * (test[i, "weight"])
  }
  
  if (!(test[i, "partei"] %in% colnames(sim.prop.swing)) &
      test[i, "partei"] != "CSU") {
    zs_pred[i,] <-
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
    preds[test$wkr == j, ] <-
      sweep(preds[test$wkr == j,], 2, colSums(preds[test$wkr == j, ]), "/")
  }
  
  
  
  
  tmp_winner <- future_lapply(1:mu_nsim, function(x) {
    select <-
      cbind(1:299, unlist(lapply(
        aggregate(-preds[, x], list(test$wkr), order)[, 2], `[[`, 1
      )))
    
    vals[select]
  })
  
  
  reg_test_mat <- cbind(1, test_x)
  
  reg_preds <- reg_test_mat %*% t(S) + matrix(rnorm(nrow(reg_test_mat)*mu_nsim, 0, sd(residuals(reg))), nrow = nrow(reg_test_mat), ncol = mu_nsim)
  
  rmse_reg <- apply(reg_preds, 2, rmse, obs = test_y)
  
  reg_preds[reg_preds < 0] <- 0
  
  for (j in 1:299) {
    reg_preds[test$wkr == j, ] <-
      sweep(reg_preds[test$wkr == j,], 2, colSums(reg_preds[test$wkr == j, ]), "/")
  }
  
  
  
  tmp_reg_winner <- future_lapply(1:mu_nsim, function(x) {
    select <-
      cbind(1:299, unlist(lapply(
        aggregate(-reg_preds[, x], list(test$wkr), order)[, 2], `[[`, 1
      )))
    
    vals[select]
  })
  
  res_list[[zsim]] <- list(winner_nn = tmp_winner, winner_reg = tmp_reg_winner, rmse_nn = rmse_nn, rmse_reg = rmse_reg)
  cat(zsim, "of", nsim, "\n")

}
res_co[[paste0(cutoff)]] <- res_list
}
res_co_el[[paste0(election)]] <- res_co
}

saveRDS(res_co_el, "final_res_09_17.RDS")


get_correct_preds <- function(a) {
  ir <- match(rownames(a), colnames(a))
  ic <- match(colnames(a), rownames(a))
  res <- NULL
  if(length(ir) <= length(ic)){
    for(idx in 1:length(ir)){
      res <- c(res, a[ic[ir[idx]],ir[idx]])
    }
  } else {
    for(idx in 1:length(ic)){
      res <- c(res, a[ic[idx], ir[ic[idx]]])
    }
  }
  return(res)
}


elections <- c(2017, 2013, 2009)
cutoffs <- c(92, 36, 2)

res_mat <- matrix(NA, nrow = 6, ncol = 9)
counter <- 1
for(election in elections){
  for(cutoff in cutoffs){
    res_list <- res_co_el[[paste0(election)]][[paste0(cutoff)]]
    test <- full_df[full_df$election == election, ]
    
    res_mat[1, counter] <- mean(unlist(lapply(res_list, "[[", "rmse_nn")))
    res_mat[2, counter] <- mean(unlist(lapply(res_list, "[[", "rmse_reg")))
    
    winner <- matrix(unlist(lapply(res_list, "[[", "winner_nn")), nrow = nsim*mu_nsim, ncol = 299, byrow = T)
    reg_winner <- matrix(unlist(lapply(res_list, "[[", "winner_reg")), nrow = nsim*mu_nsim, ncol = 299, byrow = T)
    
    win_margin <-
      test[test$winner == 1, c("resp_E")][order(test$wkr[test$winner == 1])] -
      test[test$second == 1, c("resp_E")][order(test$wkr[test$second == 1])]
    
    obs_win <- NULL
    
    for (i in 1:299) {
      obs_win <-
        rbind(obs_win,
              c("AFD", "CDU", "CSU", "FDP", "GRUENE", "LINKE", "SPD") %in% test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])][i])
    }
    
    prob_win <- NULL
    
    win_probs <- apply(winner, 2, function(x)
      - sort(-table(x) / nrow(winner)))
    
    reg_win_probs <- apply(reg_winner, 2, function(x)
      - sort(-table(x) / nrow(reg_winner)))
    
    
    prob_win <- matrix(0, nrow = 299, ncol = 7)
    reg_prob_win <- matrix(0, nrow = 299, ncol = 7)
    colnames(prob_win) <- colnames(reg_prob_win) <-
      c("AFD", "CDU", "CSU", "FDP", "GRUENE", "LINKE", "SPD")
    
    for (i in 1:299) {
      for (j in 1:length(win_probs[[i]])) {
        prob_win[i, colnames(prob_win) %in% names(win_probs[[i]][j])] <-
          win_probs[[i]][j]
        reg_prob_win[i, colnames(reg_prob_win) %in% names(reg_win_probs[[i]][j])] <-
          reg_win_probs[[i]][j]
      }
    }
    
    
    
    epcp_nn <- sum(t(prob_win)[t(obs_win), drop = F]) / 299
    
    epcp_reg <- sum(t(reg_prob_win)[t(obs_win), drop = F]) / 299
    
    res_mat[3, counter] <- epcp_nn
    res_mat[4, counter] <- epcp_reg
    
    pred_df <-
      data.frame(
        obs = test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])],
        margin = win_margin,
        east = test[test$winner == 1, c("east")][order(test$wkr[test$winner == 1])],
        nn_pred = sapply(apply(winner, 2, function(x)
          names(
            -sort(-table(x) / nrow(winner))
          )), `[[`, 1),
        nn_prob = sapply(apply(winner, 2, function(x)
          - sort(
            -table(x) / nrow(winner)
          )), `[[`, 1),
        nn_pred2 = sapply(apply(winner, 2, function(x)
          names(
            -sort(-table(x) / nrow(winner))
          )), `[`, 2),
        nn_prob2 = sapply(apply(winner, 2, function(x)
          - sort(
            -table(x) / nrow(winner)
          )), `[`, 2),
        reg_pred = sapply(apply(reg_winner, 2, function(x)
          names(
            -sort(-table(x) / nrow(reg_winner))
          )), `[[`, 1),
        reg_prob = sapply(apply(reg_winner, 2, function(x)
          - sort(-table(x) / nrow(reg_winner))), `[[`, 1),
        reg_pred2 = sapply(apply(reg_winner, 2, function(x)
          names(
            -sort(-table(x) / nrow(reg_winner))
          )), `[`, 2),
        reg_prob2 = sapply(apply(reg_winner, 2, function(x)
          - sort(-table(x) / nrow(reg_winner))), `[`, 2)
      )
    
    preds_nn <- table(pred = sapply(apply(winner, 2, function(x)
      names(
        -sort(-table(x) / nrow(winner))
      )), `[[`, 1), obs = test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])])
    
    
    preds_reg <- table(pred = sapply(apply(reg_winner, 2, function(x)
      names(
        -sort(-table(x) / nrow(reg_winner))
      )), `[[`, 1), obs = test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])])
    
    res_mat[5, counter] <- sum(get_correct_preds(preds_nn))/sum(preds_nn)
    res_mat[6, counter] <- sum(get_correct_preds(preds_reg))/sum(preds_reg)
    
    counter <- counter + 1
  }
}
round(res_mat,3)

colnames(res_mat) <- unlist(lapply(elections, function(x) paste0(x, " ", cutoffs)))

rownames(res_mat) <- unlist(lapply(c("RMSE", "ePCP", "PCP"), function(x) paste0(c("NN", "LM"), " ", x)))
  
res_table <- round(res_mat, 4)

knitr::kable(res_table)

rres_baseline <- NULL
for(election in c(2017, 2013, 2009)){
  
  win_l1 <- full_df$partei[full_df$election==election-4][full_df$winner[full_df$election==election-4] == 1]
  win_l1 <- cbind(win_l1, full_df$wkr_name[full_df$election==election-4][full_df$winner[full_df$election==election-4] == 1], full_df$wkr[full_df$election==election-4][full_df$winner[full_df$election==election-4] == 1])
  
  win_now <- full_df$partei[full_df$election==election][full_df$winner[full_df$election==election] == 1]
  win_now <- cbind(win_now, full_df$wkr_name[full_df$election==election][full_df$winner[full_df$election==election] == 1], full_df$wkr[full_df$election==election][full_df$winner[full_df$election==election] == 1])
  
  win_match <- cbind(win_now, win_l1[match(win_now[,2], win_l1[,2])])
  
  preds_baseline <- table(pred = win_match[,4], obs = win_match[,1])
  
  res_baseline <- c(res_baseline, sum(get_correct_preds(preds_baseline))/299)
}


names(res_baseline) <- c(2017, 2013, 2009)
round(res_baseline, 4)




election <- 2017
cutoff <- 2

res_list <- res_co_el[[paste0(election)]][[paste0(cutoff)]]
test <- full_df[full_df$election == election, ]


winner <- matrix(unlist(lapply(res_list, "[[", "winner_nn")), nrow = nsim*mu_nsim, ncol = 299, byrow = T)
reg_winner <- matrix(unlist(lapply(res_list, "[[", "winner_reg")), nrow = nsim*mu_nsim, ncol = 299, byrow = T)

win_margin <-
  test[test$winner == 1, c("resp_E")][order(test$wkr[test$winner == 1])] -
  test[test$second == 1, c("resp_E")][order(test$wkr[test$second == 1])]

obs_win <- NULL

for (i in 1:299) {
  obs_win <-
    rbind(obs_win,
          c("AFD", "CDU", "CSU", "FDP", "GRUENE", "LINKE", "SPD") %in% test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])][i])
}

prob_win <- NULL

win_probs <- apply(winner, 2, function(x)
  - sort(-table(x) / nrow(winner)))

reg_win_probs <- apply(reg_winner, 2, function(x)
  - sort(-table(x) / nrow(reg_winner)))


prob_win <- matrix(0, nrow = 299, ncol = 7)
reg_prob_win <- matrix(0, nrow = 299, ncol = 7)
colnames(prob_win) <- colnames(reg_prob_win) <-
  c("AFD", "CDU", "CSU", "FDP", "GRUENE", "LINKE", "SPD")

for (i in 1:299) {
  for (j in 1:length(win_probs[[i]])) {
    prob_win[i, colnames(prob_win) %in% names(win_probs[[i]][j])] <-
      win_probs[[i]][j]
    reg_prob_win[i, colnames(reg_prob_win) %in% names(reg_win_probs[[i]][j])] <-
      reg_win_probs[[i]][j]
  }
}



pred_df <-
  data.frame(
    obs = test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])],
    margin = win_margin,
    east = test[test$winner == 1, c("east")][order(test$wkr[test$winner == 1])],
    nn_pred = sapply(apply(winner, 2, function(x)
      names(
        -sort(-table(x) / nrow(winner))
      )), `[[`, 1),
    nn_prob = sapply(apply(winner, 2, function(x)
      - sort(
        -table(x) / nrow(winner)
      )), `[[`, 1),
    nn_pred2 = sapply(apply(winner, 2, function(x)
      names(
        -sort(-table(x) / nrow(winner))
      )), `[`, 2),
    nn_prob2 = sapply(apply(winner, 2, function(x)
      - sort(
        -table(x) / nrow(winner)
      )), `[`, 2),
    reg_pred = sapply(apply(reg_winner, 2, function(x)
      names(
        -sort(-table(x) / nrow(reg_winner))
      )), `[[`, 1),
    reg_prob = sapply(apply(reg_winner, 2, function(x)
      - sort(-table(x) / nrow(reg_winner))), `[[`, 1),
    reg_pred2 = sapply(apply(reg_winner, 2, function(x)
      names(
        -sort(-table(x) / nrow(reg_winner))
      )), `[`, 2),
    reg_prob2 = sapply(apply(reg_winner, 2, function(x)
      - sort(-table(x) / nrow(reg_winner))), `[`, 2)
  )

preds_nn <- table(pred = sapply(apply(winner, 2, function(x)
  names(
    -sort(-table(x) / nrow(winner))
  )), `[[`, 1), obs = test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])])


preds_reg <- table(pred = sapply(apply(reg_winner, 2, function(x)
  names(
    -sort(-table(x) / nrow(reg_winner))
  )), `[[`, 1), obs = test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])])

res_mat[5, counter] <- sum(get_correct_preds(preds_nn))/sum(preds_nn)
res_mat[6, counter] <- sum(get_correct_preds(preds_reg))/sum(preds_reg)




table(pred_df$nn_pred, pred_df$obs)

#saveRDS(pred_df, paste0("pred_df_", cutoff, ".RDS"))
par(mfrow = c(2, 1))
plot(
  pred_df$nn_prob * 100,
  pred_df$margin * 100,
  col = ifelse(
    as.character(pred_df$obs) == as.character(pred_df$nn_pred),
    "black",
    "red"
  ),
  pch = ifelse(pred_df$east == 1, 17, 19),
  bty = "n",
  xlab = "vorhergesagte Gewinnwahrscheinlichkeit in %",
  ylab = "Differenz zwischen Gewinnern und Zweitplatzierten in Prozentpunkten",
  las = 1,
  main = paste0("Wahlkreis-Prognose ", cutoff, " Tage vor der Wahl")
)


pred_df$cat <- 0

pred_df$cat <- ifelse(pred_df$nn_prob <= 0.65, "Offen", ifelse(pred_df$nn_prob > 0.65 & pred_df$nn_prob <= 0.85, "Tendenz", ifelse(pred_df$nn_prob > 0.85 & pred_df$nn_prob <= 0.99, "Wahrscheinlich", ifelse(pred_df$nn_prob > 0.99, "Sicher", NA)) ))

table(pred_df$cat, pred_df$nn_pred)

open_wkr <- data.frame(
WkrNr = wahl1317[which(pred_df$cat == "Offen"),"Wkr.Nr."],
Wkr = wahl1317[which(pred_df$cat == "Offen"),"Wahlkreisname"],
Vorhersage = pred_df[which(pred_df$cat == "Offen"),"nn_pred"],
Wahrsch = round(pred_df[which(pred_df$cat == "Offen"),"nn_prob"]*100,1),
Beobachtet = pred_df[which(pred_df$cat == "Offen"),"obs"],
Vorsprung = round(pred_df[which(pred_df$cat == "Offen"),"margin"]*100, 1)
)

open_wkr[order(open_wkr$Wahrsch),]

plot(
  pred_df$reg_prob * 100,
  pred_df$margin * 100,
  col = ifelse(
    as.character(pred_df$obs) == as.character(pred_df$reg_pred),
    "black",
    "red"
  ),
  pch = ifelse(pred_df$east == 1, 17, 19),
  bty = "n",
  xlab = "vorhergesagte Gewinnwahrscheinlichkeit in %",
  ylab = "Differenz zwischen Gewinnern und Zweitplatzierten in Prozentpunkten",
  las = 1,
  main = paste0("Wahlkreis-Prognose ", cutoff, " Tage vor der Wahl")
)
