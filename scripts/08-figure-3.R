election <- 2017
cutoff <- 2
election_l1 <- election - 4

res_co_el <- readRDS("../processed-data/final_res_09_17.RDS")

res_list <- res_co_el[[paste0(election)]][[paste0(cutoff)]]

draws <-
  readRDS(paste0(
    "../raw-data/draws_forcast_levels_",
    election,
    "_",
    cutoff,
    ".RDS"
  ))

wber <- read.csv2("../raw-data/btw17_wber.csv")

wahl1317 <-
  read.csv2(
    "../raw-data/btwkr17_umrechnung_btw13.csv",
    skip = 4,
    fileEncoding = "ISO-8859-1",
    na.strings = "",
    stringsAsFactors = F
  )

forecast <- draws$forecast

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




wahl1317 <- wahl1317[-1, ]
wahl1317 <-
  wahl1317[!(wahl1317[, 3] == "Land" | wahl1317[, 3] == "Insgesamt"), ]
wahl1317$Land1 <- as.numeric(as.factor(wahl1317$Land))
wahl13_Z <- as.matrix(cbind(wahl1317[, c(9, seq(11, 21, 2), 52)]))
class(wahl13_Z) <- "numeric"
wahl13_Zp <- wahl13_Z[, 2:8] / wahl13_Z[, 1] * 100

wahl13_E <- as.matrix(cbind(wahl1317[, c(8, seq(10, 20, 2), 51)]))
class(wahl13_E) <- "numeric"
wahl13_Ep <- wahl13_E[, 2:8] / wahl13_E[, 1] * 100



wahl13_Zp[, 1] <-
  ifelse(wahl13_Zp[, 1] == 0, wahl13_Zp[, 6], wahl13_Zp[, 1])
wahl13_Zp <- wahl13_Zp[, c(1, 2, 4, 5, 3, 7)]

wahl13_Ep[, 1] <-
  ifelse(wahl13_Ep[, 1] == 0, wahl13_Ep[, 6], wahl13_Ep[, 1])
wahl13_Ep <- wahl13_Ep[, c(1, 2, 4, 5, 3, 7)]

propEZ <- wahl13_Ep / wahl13_Zp

# Mean imputation for districts with no AfD candidate 2013
propEZ[propEZ[, 6] == 0, 6] <- mean(propEZ[propEZ[, 6] > 0, 6])

nsims <- 500

mu_nsims <- 25

simZ <- array(NA, dim = c(nsims * mu_nsims, 6, 299))

counter <- 1

cat("\n Simulate District Results: \n")
pb <- txtProgressBar(min = 0, max = 299, style = 3)
for (wkr in 1:299) {
  for (party in 1:6) {
    for (nsim in 1:nsims) {
      for (mu_nsim in 1:mu_nsims) {
        simZ[counter, party, wkr] <-
          wahl13_Zp[wkr, party] + sim.prop.swing[nsim, party] * wahl13_Zp[wkr, party]
        counter <- counter + 1
        if (counter > (nsims * mu_nsims)) {
          counter <- 1
        }
      }
    }
  }
  setTxtProgressBar(pb, wkr)
}

close(pb)

Bev <- c(
  2673803,
  1525090,
  7278789,
  568510,
  15707569,
  5281198,
  3661245,
  9365001,
  11362245,
  899748,
  2975745,
  2391746,
  1548400,
  3914671,
  2145671,
  2077901
)

Land <-
  c(
    "SH",
    "HH",
    "NI",
    "HB",
    "NW",
    "HE",
    "RP",
    "BW",
    "BY",
    "SL",
    "BE",
    "BB",
    "MV",
    "SN",
    "ST",
    "TH"
  )

div1 <- round(sum(Bev) / 598, 0)


while (sum(round(Bev / div1, 0)) < 598) {
  div1 <- div1 - 1
}

while (sum(round(Bev / div1, 0)) > 598) {
  div1 <- div1 + 1
}

lsitze <- round(Bev / div1, 0)

lsitze <- data.frame(as.numeric(as.factor(Land)), lsitze)

lsitze <- lsitze[, 2][order(lsitze[, 1])]

df.forecast <-
  lapply(1:nsims, function (x)
    t(replicate(mu_nsims, forecast[x, ])))

df.forecast <- do.call("rbind", df.forecast)

df.forecast <- df.forecast * 100
klausel <- df.forecast >= 5
klausel[, 7] <- TRUE

tmpZstimme <- array(NA, c(nsims * mu_nsims, 7, 299))

wbet <- wahl1317$Wähler / wahl1317$Wahlberechtigte

simZCsu <- simZ

simZCsu <-
  abind(simZ, array(0, replace(dim(simZ), 2, 1)), along = 2)

simZCsu[, 7, wahl1317$Wkr.Nr.[wahl1317$Land1 == 4]] <-
  simZCsu[, 1, wahl1317$Wkr.Nr.[wahl1317$Land1 == 4]]

simZCsu[, 1, wahl1317$Wkr.Nr.[wahl1317$Land1 == 4]] <- 0


cat("\n Incorporate 5% Hurdle: \n")
pb <- txtProgressBar(min = 0,
                     max = nsims * mu_nsims,
                     style = 3)

for (sim in 1:(nsims * mu_nsims)) {
  for (party in 1:7) {
    tmpZstimme[sim, party, ] <-
      if (klausel[sim, party])
        round(simZCsu[sim, party, ] / 100 * wbet * wber$Wber, 0)
    else
      rep(0, 299)
  }
  setTxtProgressBar(pb, sim)
}
close(pb)


finZstimme <- array(NA, c(299, 8, nsims * mu_nsims))



for (i in 1:(nsims * mu_nsims)) {
  tmp <- t(tmpZstimme[i, , ])
  finZstimme[, , i] <- cbind(tmp, wahl1317$Land1)
}

# Stimmen aggregiert pro Land

Lergebnis <- array(NA, c(16, 7, nsims * mu_nsims))

cat("\n Calculate Results by Land: \n")
pb <- txtProgressBar(min = 0,
                     max = nsims * mu_nsims,
                     style = 3)

for (k in 1:(nsims * mu_nsims)) {
  for (j in 1:16) {
    for (i in 1:7) {
      Lergebnis[j, i, k] <- sum(finZstimme[, i, k][finZstimme[, 8, k] == j])
    }
  }
  setTxtProgressBar(pb, k)
}
close(pb)

cat("\n Calculate Seat Distribution by Party and Land: \n This might take a while...\n")
pb <- txtProgressBar(min = 0,
                     max = nsims * mu_nsims,
                     style = 3)

div2 <- matrix(NA, nsims * mu_nsims, 16)

for (j in 1:(nsims * mu_nsims)) {
  for (i in 1:16) {
    div2[j, i] <- round(sum(Lergebnis[i, , j]) / lsitze[i], 0)
    
    
    while (sum(round(Lergebnis[i, , j] / div2[j, i], 0)) < lsitze[i]) {
      div2[j, i] <- div2[j, i] - 1
    }
    
    while (sum(round(Lergebnis[i, , j] / div2[j, i], 0)) > lsitze[i]) {
      div2[j, i] <- div2[j, i] + 1
    }
  }
  setTxtProgressBar(pb, j)
}
close(pb)

Psitze <- array(NA, c(16, 7, nsims * mu_nsims))

for (j in 1:(nsims * mu_nsims)) {
  for (i in 1:16) {
    Psitze[i, , j] <- round(Lergebnis[i, , j] / div2[j, i], 0)
  }
}


adj.simWin <- array(NA, dim = c(nsims * mu_nsims, 7, 299))





winner <-
  matrix(
    unlist(lapply(res_list, "[[", "winner_nn")),
    nrow = nsims * mu_nsims,
    ncol = 299,
    byrow = T
  )
cat("\n Get District Winners:\n")
pb <- txtProgressBar(min = 0, max = 299, style = 3)
for (wkr in 1:299) {
  for (nsim in 1:(nsims * mu_nsims)) {
    adj.simWin[nsim, , wkr] <-
      c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "CSU") %in% winner[nsim, wkr]
  }
  setTxtProgressBar(pb, wkr)
}
close(pb)

o.dir <- adj.simWin

# Direktmandate auf Wahlkreisebene

new.dir <- array(NA, c(299, 7, nsims * mu_nsims))

for (i in 1:(nsims * mu_nsims)) {
  new.dir[, , i] <- t(o.dir[i, , ])
}


# mit Länder ID

new.dirid <- array(NA, c(299, 8, nsims * mu_nsims))

for (i in 1:(nsims * mu_nsims)) {
  new.dirid[, , i] <- cbind(t(o.dir[i, , ]), wahl1317$Land1)
}

Pdirect <- array(NA, c(16, 7, nsims * mu_nsims))


for (j in 1:(nsims * mu_nsims)) {
  for (i in 1:16) {
    for (k in 1:7) {
      Pdirect[i, k, j] <- sum(new.dirid[, k, j][new.dirid[, 8, j] == i])
    }
  }
}

##############################
# Mindestanzahl der Sitze pro Partei aus Direkt und Zweitstimmen
###############################

Pmin <- array(NA, c(16, 7, nsims * mu_nsims))

for (k in 1:(nsims * mu_nsims)) {
  for (j in 1:16) {
    for (i in 1:7) {
      Pmin[j, i, k] <-
        ifelse(Psitze[j, i, k] >= Pdirect[j, i, k], Psitze[j, i, k], Pdirect[j, i, k])
    }
  }
}


totfin <- matrix(NA, nsims * mu_nsims, 8)
for (i in 1:(nsims * mu_nsims)) {
  totfin[i, ] <- apply(finZstimme[, , i], 2, sum)
}

# Die Summe der Länder IDs wird rausgeworfen

totfin <- totfin[, 1:7]

totPmin <- matrix(NA, nsims * mu_nsims, 7)
for (i in 1:(nsims * mu_nsims)) {
  totPmin[i, ] <- apply(Pmin[, , i], 2, sum)
}


# Der kleinste Divisor aller Parteien bestimmt die endgültige Sitzanzahl

bundsitze <- matrix(NA, nsims * mu_nsims, 7)
for (i in 1:(nsims * mu_nsims)) {
  bundsitze[i, ] <-
    round(totfin[i, ] / min(round((
      totfin[i, ] / (totPmin[i, ] - 0.48)
    )[(totfin[i, ] / (totPmin[i, ] - 0.48)) > 0], 0)), 0)
}

bt <- apply(bundsitze, 1, sum)

# Set to TRUE if you want the final distribution. Takes a while.

if (FALSE) {
  div3 <- matrix(NA, nsims * mu_nsims, 7)
  
  
  
  for (j in 1:(nsims * mu_nsims)) {
    cat("\n", j, "\t")
    for (i in 1:7) {
      cat(i)
      div3[j, i] <-
        ifelse(round(sum(Lergebnis[, i, j]) / bundsitze[j, i], 0) == "NaN",
               1,
               round(sum(Lergebnis[, i, j]) / bundsitze[j, i], 0))
      if (div3[j, i] != 1) {
        while (sum(apply(rbind(
          round(Lergebnis[, i, j] / div3[j, i], 0)
          , Pdirect[, i, j]
        ), 2, max)) > bundsitze[j, i]) {
          div3[j, i] <- div3[j, i] + 1
        }
        while (sum(apply(rbind(
          round(Lergebnis[, i, j] / div3[j, i], 0)
          , Pdirect[, i, j]
        ), 2, max)) < bundsitze[j, i]) {
          div3[j, i] <- div3[j, i] - 1
        }
      }
    }
  }
  
  # Endgültige Sitzverteilung
  
  finsitze <- array(NA, c(7, 16, nsims * mu_nsims))
  
  for (j in 1:(nsims * mu_nsims)) {
    for (i in 1:7) {
      finsitze[i, , j] <-   apply(rbind(round(Lergebnis[, i, j] / div3[j, i], 0)
                                        , Pdirect[, i, j]), 2, max)
    }
  }
  
  
  saveRDS(finsitze,
          paste0("../processed-data/PVS_finsitze_wbet13.RDS"))
}

old_par <- par()
if (save_figures) {
  tiff(
    "../figures/Abbildung3.tiff",
    height = 9 * 0.75,
    width = 16 * 0.75,
    units = "in",
    res = 300
  )
}
plot(
  table(bt),
  main = "Simulierte Größen des Bundestags \n Prognose 2 Tage vor der Bundestagswahl 2017",
  xlab = "Anzahl der Sitze",
  ylab = "Häufigkeit",
  las = 1,
  xaxt = "n",
  bty = "n",
  col = "grey"
)
axis(side = 1)
if (save_figures) {
  dev.off()
}
par(old_par)
