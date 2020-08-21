###########################
# Plot win probabilities on maps
###########################

old_par <- par()

nsim <- 500
mu_nsim <- 25
election <- 2017
cutoff <- 2

res_co_el <- readRDS("../processed-data/final_res_09_17.RDS")

res_list <- res_co_el[[paste0(election)]][[paste0(cutoff)]]


winner <-
  matrix(
    unlist(lapply(res_list, "[[", "winner_nn")),
    nrow = nsim * mu_nsim,
    ncol = 299,
    byrow = T
  )


plot_col <- rgb(0, 0, 0, maxColorValue = 255)

wkr17 <- readOGR(dsn = "../raw-data/shp", stringsAsFactors = F)

win_probs <- apply(winner, 2, function(x)
  - sort(-table(x) / nrow(winner)))


if (save_figures) {
  tiff(
    "../figures/Abbildung2.tiff",
    height = 9 * 0.75,
    width = 16 * 0.75,
    units = "in",
    res = 300
  )
}
par(mfrow = c(2, 3),
    oma = c(0, 0, 4, 0),
    lheight = 1)

col_vec <- NULL

for (i in 1:299) {
  tmp <- win_probs[[i]]
  if ("CDU" %in% names(tmp) | "CSU" %in% names(tmp)) {
    col_vec <-
      c(col_vec, adjustcolor(plot_col, alpha = tmp[names(tmp) == "CDU" |
                                                     names(tmp) == "CSU"]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(
  wkr17,
  col = col_vec,
  border = "lightgrey",
  lwd = 0.2,
  main = "CDU/CSU"
)

col_vec <- NULL

for (i in 1:299) {
  tmp <- win_probs[[i]]
  if ("SPD" %in% names(tmp)) {
    col_vec <-
      c(col_vec, adjustcolor(plot_col, alpha = tmp[names(tmp) == "SPD"]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(
  wkr17,
  col = col_vec,
  border = "lightgrey",
  lwd = 0.2,
  main = "SPD"
)

col_vec <- NULL

for (i in 1:299) {
  tmp <- win_probs[[i]]
  if ("AFD" %in% names(tmp)) {
    col_vec <-
      c(col_vec, adjustcolor(plot_col, alpha = tmp[names(tmp) == "AFD"]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(
  wkr17,
  col = col_vec,
  border = "lightgrey",
  lwd = 0.2,
  main = "AfD"
)


col_vec <- NULL

for (i in 1:299) {
  tmp <- win_probs[[i]]
  if ("FDP" %in% names(tmp)) {
    col_vec <-
      c(col_vec, adjustcolor(plot_col, alpha = tmp[names(tmp) == "FDP"]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(
  wkr17,
  col = col_vec,
  border = "lightgrey",
  lwd = 0.2,
  main = "FDP"
)


col_vec <- NULL

for (i in 1:299) {
  tmp <- win_probs[[i]]
  if ("LINKE" %in% names(tmp)) {
    col_vec <-
      c(col_vec, adjustcolor(plot_col, alpha = tmp[names(tmp) == "LINKE"]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(
  wkr17,
  col = col_vec,
  border = "lightgrey",
  lwd = 0.2,
  main = "LINKE"
)



col_vec <- NULL

for (i in 1:299) {
  tmp <- win_probs[[i]]
  if ("GRUENE" %in% names(tmp)) {
    col_vec <-
      c(col_vec, adjustcolor(plot_col, alpha = tmp[names(tmp) == "GRUENE"]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(
  wkr17,
  col = col_vec,
  border = "lightgrey",
  lwd = 0.2,
  main = "GRÜNE"
)

mtext(
  "Gewinnwahrscheinlichkeiten der Kandidierenden der jeweiligen Parteien in den Wahlkreisen \n 2 Tage vor der Bundestagswahl 2017",
  outer = T,
  font = 2
)
if (save_figures) {
  dev.off()
}
par(old_par)