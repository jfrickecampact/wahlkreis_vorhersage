# Load in all model results.
# Be careful, the file is 586.3 Mb
res_co_el <- readRDS("../processed-data/final_res_09_17.RDS")

elections <- c(2017, 2013, 2009)
cutoffs <- c(92, 36, 2)

nsim <- 500
mu_nsim <- 25


res_baseline <- NULL
for (election in c(2017, 2013, 2009)) {
  win_l1 <-
    full_df$partei[full_df$election == election - 4][full_df$winner[full_df$election ==
                                                                      election - 4] == 1]
  win_l1 <-
    cbind(win_l1, full_df$wkr_name[full_df$election == election - 4][full_df$winner[full_df$election ==
                                                                                      election - 4] == 1], full_df$wkr[full_df$election == election - 4][full_df$winner[full_df$election ==
                                                                                                                                                                          election - 4] == 1])
  
  win_now <-
    full_df$partei[full_df$election == election][full_df$winner[full_df$election ==
                                                                  election] == 1]
  win_now <-
    cbind(win_now, full_df$wkr_name[full_df$election == election][full_df$winner[full_df$election ==
                                                                                   election] == 1], full_df$wkr[full_df$election == election][full_df$winner[full_df$election ==
                                                                                                                                                               election] == 1])
  
  win_match <-
    cbind(win_now, win_l1[match(win_now[, 2], win_l1[, 2])])
  
  preds_baseline <- table(pred = win_match[, 4], obs = win_match[, 1])
  
  res_baseline <-
    c(res_baseline, sum(get_correct_preds(preds_baseline)) / 299)
}


names(res_baseline) <- c(2017, 2013, 2009)

cat("\n Results for Tab. 1: \n")
print(round(res_baseline, 4))


res_mat <- matrix(NA, nrow = 6, ncol = 9)
counter <- 1
for (election in elections) {
  for (cutoff in cutoffs) {
    res_list <- res_co_el[[paste0(election)]][[paste0(cutoff)]]
    test <- full_df[full_df$election == election,]
    
    res_mat[1, counter] <-
      mean(unlist(lapply(res_list, "[[", "rmse_nn")))
    res_mat[2, counter] <-
      mean(unlist(lapply(res_list, "[[", "rmse_reg")))
    
    winner <-
      matrix(
        unlist(lapply(res_list, "[[", "winner_nn")),
        nrow = nsim * mu_nsim,
        ncol = 299,
        byrow = T
      )
    reg_winner <-
      matrix(
        unlist(lapply(res_list, "[[", "winner_reg")),
        nrow = nsim * mu_nsim,
        ncol = 299,
        byrow = T
      )
    
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
          - sort(-table(x) / nrow(winner))), `[[`, 1),
        nn_pred2 = sapply(apply(winner, 2, function(x)
          names(
            -sort(-table(x) / nrow(winner))
          )), `[`, 2),
        nn_prob2 = sapply(apply(winner, 2, function(x)
          - sort(-table(x) / nrow(winner))), `[`, 2),
        reg_pred = sapply(apply(reg_winner, 2, function(x)
          names(-sort(-table(x) / nrow(reg_winner)))), `[[`, 1),
        reg_prob = sapply(apply(reg_winner, 2, function(x)
          - sort(-table(x) / nrow(reg_winner))), `[[`, 1),
        reg_pred2 = sapply(apply(reg_winner, 2, function(x)
          names(-sort(-table(x) / nrow(reg_winner)))), `[`, 2),
        reg_prob2 = sapply(apply(reg_winner, 2, function(x)
          - sort(-table(x) / nrow(reg_winner))), `[`, 2)
      )
    
    preds_nn <- table(pred = sapply(apply(winner, 2, function(x)
      names(
        -sort(-table(x) / nrow(winner))
      )), `[[`, 1), obs = test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])])
    
    
    preds_reg <-
      table(pred = sapply(apply(reg_winner, 2, function(x)
        names(-sort(-table(x) / nrow(reg_winner)))), `[[`, 1), obs = test[test$winner == 1, c("partei")][order(test$wkr[test$winner == 1])])
    
    res_mat[5, counter] <-
      sum(get_correct_preds(preds_nn)) / sum(preds_nn)
    res_mat[6, counter] <-
      sum(get_correct_preds(preds_reg)) / sum(preds_reg)
    
    counter <- counter + 1
  }
}


colnames(res_mat) <-
  unlist(lapply(elections, function(x)
    paste0(x, " ", cutoffs)))

rownames(res_mat) <-
  unlist(lapply(c("RMSE", "ePCP", "PCP"), function(x)
    paste0(c("NN", "LM"), " ", x)))

res_table <- round(res_mat, 4)

cat("\n Results for Tab. 2: \n")
print(knitr::kable(res_table))






election <- 2017
cutoff <- 2

res_list <- res_co_el[[paste0(election)]][[paste0(cutoff)]]
test <- full_df[full_df$election == election,]


winner <-
  matrix(
    unlist(lapply(res_list, "[[", "winner_nn")),
    nrow = nsim * mu_nsim,
    ncol = 299,
    byrow = T
  )
reg_winner <-
  matrix(
    unlist(lapply(res_list, "[[", "winner_reg")),
    nrow = nsim * mu_nsim,
    ncol = 299,
    byrow = T
  )

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

cat("\n Results for Tab. 3: \n")
print(table(pred_df$nn_pred, pred_df$obs))


pred_df$cat <- 0

pred_df$cat <-
  ifelse(
    pred_df$nn_prob <= 0.65,
    "Offen",
    ifelse(
      pred_df$nn_prob > 0.65 &
        pred_df$nn_prob <= 0.85,
      "Tendenz",
      ifelse(
        pred_df$nn_prob > 0.85 &
          pred_df$nn_prob <= 0.99,
        "Wahrscheinlich",
        ifelse(pred_df$nn_prob > 0.99, "Sicher", NA)
      )
    )
  )

cat("\n Results for Tab. 4: \n")
print(table(pred_df$cat, pred_df$nn_pred)[c(2, 4, 3, 1), ])
