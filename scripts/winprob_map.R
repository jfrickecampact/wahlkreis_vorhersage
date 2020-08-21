###########################
# Plot win probabilities on maps
###########################

library(rgdal)

require(geojsonio)
require(rmapshaper)

nsim <- 500
mu_nsim <- 25
election <- 2017
cutoff <- 2

res_co_el <- readRDS("final_res_09_17.RDS")

res_list <- res_co_el[[paste0(election)]][[paste0(cutoff)]]


winner <- matrix(unlist(lapply(res_list, "[[", "winner_nn")), nrow = nsim*mu_nsim, ncol = 299, byrow = T)


CDU_col <- rgb(0, 0, 0, maxColorValue = 255)
GREENS_col <- rgb(70, 150, 43, maxColorValue = 255)
SPD_col <- rgb(227, 0, 15, maxColorValue = 255)
FDP_col <- rgb(255, 237, 0, maxColorValue = 255)
LINKE_col <- "magenta"#rgb(227, 0, 15, maxColorValue = 255)
AfD_col <- rgb(0, 158, 224, maxColorValue = 255)
Sonstige_col <- "#999999"
party_cols <- c(CDU_col, SPD_col, GRÜNE_col, FDP_col, LINKE_col, AfD_col, Sonstige_col)

wkr17 <- readOGR(dsn = "data/shp", stringsAsFactors = F)

win_probs <- apply(winner, 2, function(x)
  - sort(-table(x) / nrow(winner)))


png("Abb2.png", width = 1600*0.75, height = 900*0.75)
tiff("Abbildung2.tiff", width = 16*0.75, height = 9*0.75, units = "in", res = 300)
par(mfrow = c(2, 3), oma = c(0, 0, 4, 0), lheight = 1)

col_vec <- NULL

for(i in 1:299){
tmp <- win_probs[[i]]
if("CDU" %in% names(tmp) | "CSU" %in% names(tmp)) {
col_vec <- c(col_vec, adjustcolor(CDU_col, alpha = tmp[names(tmp) == "CDU" | names(tmp) == "CSU"]))
}
else{
  col_vec <- c(col_vec, NA)
}
}

plot(wkr17, col = col_vec, border = "lightgrey", lwd = 0.2, main = "CDU/CSU")

col_vec <- NULL

for(i in 1:299){
  tmp <- win_probs[[i]]
  if("SPD" %in% names(tmp) ) {
    col_vec <- c(col_vec, adjustcolor(CDU_col, alpha = tmp[names(tmp) == "SPD" ]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(wkr17, col = col_vec, border = "lightgrey", lwd = 0.2, main = "SPD")

col_vec <- NULL

for(i in 1:299){
  tmp <- win_probs[[i]]
  if("AFD" %in% names(tmp) ) {
    col_vec <- c(col_vec, adjustcolor(CDU_col, alpha = tmp[names(tmp) == "AFD" ]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(wkr17, col = col_vec, border = "lightgrey", lwd = 0.2, main = "AfD")


col_vec <- NULL

for(i in 1:299){
  tmp <- win_probs[[i]]
  if("FDP" %in% names(tmp) ) {
    col_vec <- c(col_vec, adjustcolor(CDU_col, alpha = tmp[names(tmp) == "FDP" ]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(wkr17, col = col_vec, border = "lightgrey", lwd = 0.2, main = "FDP")


col_vec <- NULL

for(i in 1:299){
  tmp <- win_probs[[i]]
  if("LINKE" %in% names(tmp) ) {
    col_vec <- c(col_vec, adjustcolor(CDU_col, alpha = tmp[names(tmp) == "LINKE" ]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(wkr17, col = col_vec, border = "lightgrey", lwd = 0.2, main = "LINKE")



col_vec <- NULL

for(i in 1:299){
  tmp <- win_probs[[i]]
  if("GRUENE" %in% names(tmp) ) {
    col_vec <- c(col_vec, adjustcolor(CDU_col, alpha = tmp[names(tmp) == "GRUENE" ]))
  }
  else{
    col_vec <- c(col_vec, NA)
  }
}

plot(wkr17, col = col_vec, border = "lightgrey", lwd = 0.2, main = "GRÜNE")

mtext("Gewinnwahrscheinlichkeiten der Kandidierenden der jeweiligen Parteien in den Wahlkreisen \n 2 Tage vor der Bundestagswahl 2017", outer = T, font =2)
dev.off()