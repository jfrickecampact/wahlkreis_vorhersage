election <- 2017
cutoff <- 2
election_l1 <- election - 4


source("res05_13.R")

draws <-
  readRDS(paste0("data/draws_forcast_levels_", election, "_", cutoff, ".RDS"))

forecast <- draws$forecast

#nsim <- nrow(forecast)

nsims <- 500
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


wahl1317 <- read.csv2("data/btwkr17_umrechnung_btw13.csv", skip = 4, fileEncoding = "ISO-8859-1", na.strings = "", stringsAsFactors = F )

wahl1317 <- wahl1317[-1,]
wahl1317 <- wahl1317[!(wahl1317[,3] == "Land" |wahl1317[,3] == "Insgesamt"),]
wahl1317$Land1 <- as.numeric(as.factor(wahl1317$Land))
wahl13_Z <- as.matrix(cbind(wahl1317[,c(9,seq(11,21,2),52)]))
class(wahl13_Z) <- "numeric"
wahl13_Zp <- wahl13_Z[,2:8]/wahl13_Z[,1]*100

wahl13_E <- as.matrix(cbind(wahl1317[,c(8,seq(10,20,2),51)]))
class(wahl13_E) <- "numeric"
wahl13_Ep <- wahl13_E[,2:8]/wahl13_E[,1]*100



wahl13_Zp[,1] <- ifelse(wahl13_Zp[,1]==0,wahl13_Zp[,6],wahl13_Zp[,1])
wahl13_Zp <- wahl13_Zp[,c(1,2,4,5,3,7)]

wahl13_Ep[,1] <- ifelse(wahl13_Ep[,1]==0,wahl13_Ep[,6],wahl13_Ep[,1])
wahl13_Ep <- wahl13_Ep[,c(1,2,4,5,3,7)]

propEZ <- wahl13_Ep/wahl13_Zp

# Mean imputation for districts with no AfD candidate 2013
propEZ[propEZ[,6]==0,6] <- mean(propEZ[propEZ[,6]>0,6])

nsims <- 500

mu_nsims <- 25

simZ <- array(NA, dim = c(nsims*mu_nsims,6,299))

counter <- 1

for(wkr in 1:299){
  for(party in 1:6){
    for(nsim in 1:nsims){
      for(mu_nsim in 1:mu_nsims){
      simZ[counter, party, wkr] <- wahl13_Zp[wkr,party] + sim.prop.swing[nsim,party]*wahl13_Zp[wkr,party]
      counter <- counter + 1
      if(counter > (nsims*mu_nsims)) {
        counter <- 1
      }
      }
    }
  }
}


Bev <- c(2673803,
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
         2077901)

Land <- c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH")

div1 <- round(sum(Bev)/598,0)


while(sum(round(Bev/div1,0))<598){
  div1 <- div1-1
}

while(sum(round(Bev/div1,0))>598){
  div1 <- div1+1
}

lsitze <- round(Bev/div1,0)
sum(lsitze)

table(wahl1317$Land)

lsitze <- data.frame(as.numeric(as.factor(Land)), lsitze)

lsitze <- lsitze[,2][order(lsitze[,1])]

df.forecast <- lapply(1:nsims, function (x) t(replicate(mu_nsims, forecast[x,])))

df.forecast <- do.call("rbind", df.forecast)

df.forecast <- df.forecast*100
klausel <- df.forecast >=5
klausel[,7] <- TRUE

tmpZstimme <- array(NA,c(nsims*mu_nsims,7,299))

wbet <- wahl1317$Wähler/wahl1317$Wahlberechtigte

wber <- read.csv2("data/btw17_wber.csv")

wahl1317$Wkr.Nr.[wahl1317$Land1==4]
simZCsu <- simZ

library(abind)

simZCsu <- abind(simZ, array(0, replace(dim(simZ), 2, 1)), along = 2)

simZCsu[,7,wahl1317$Wkr.Nr.[wahl1317$Land1==4]] <- simZCsu[,1,wahl1317$Wkr.Nr.[wahl1317$Land1==4]]

simZCsu[,1,wahl1317$Wkr.Nr.[wahl1317$Land1==4]] <- 0


for(sim in 1:(nsims*mu_nsims)){
  for(party in 1:7){
    tmpZstimme[sim,party,] <- if(klausel[sim,party]) round(simZCsu[sim,party,]/100*wbet*wber$Wber,0) else rep(0,299)
  }}

tmpZstimme[,,212]

finZstimme <- array(NA,c(299,8,nsims*mu_nsims))



for(i in 1:(nsims*mu_nsims)){
  tmp <- t(tmpZstimme[i,,])
  finZstimme[,,i] <- cbind(tmp,wahl1317$Land1)
}

# Stimmen aggregiert pro Land
#nsims <- 3000
Lergebnis <- array(NA,c(16,7,nsims*mu_nsims))

for(k in 1:(nsims*mu_nsims)){
  for(j in 1:16){
    for(i in 1:7){
      Lergebnis[j,i,k] <- sum(finZstimme[,i,k][finZstimme[,8,k]==j])
    }}}

div2 <- matrix(NA,nsims*mu_nsims,16)

# 23.07 - 23.59
# 21.36 - 

for(j in 1:(nsims*mu_nsims)){
  for(i in 1:16){
    div2[j,i] <- round(sum(Lergebnis[i,,j])/lsitze[i],0)
    
    
    while(sum(round(Lergebnis[i,,j]/div2[j,i],0))<lsitze[i]){
      div2[j,i] <- div2[j,i]-1
    }
    
    while(sum(round(Lergebnis[i,,j]/div2[j,i],0))>lsitze[i]){
      div2[j,i] <- div2[j,i]+1
    }   
  }}

Psitze <- array(NA,c(16,7,nsims*mu_nsims))

for(j in 1:(nsims*mu_nsims)){
  for(i in 1:16){
    Psitze[i,,j] <- round(Lergebnis[i,,j]/div2[j,i],0)
  }}


adj.simWin <- array(NA, dim = c(nsims*mu_nsims,7,299))

res_co_el <- readRDS("final_res_09_17.RDS")

res_list <- res_co_el$`2017`$`2`

winner <- matrix(unlist(lapply(res_list, "[[", "winner_nn")), nrow = nsims*mu_nsims, ncol = 299, byrow = T)

for(wkr in 1:299){
  for(nsim in 1:(nsims*mu_nsims)){
    adj.simWin[nsim,,wkr] <- c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "CSU") %in% winner[nsim,wkr]
  }}

o.dir <- adj.simWin

# Direktmandate auf Wahlkreisebene

new.dir <- array(NA,c(299,7,nsims*mu_nsims))

for(i in 1:(nsims*mu_nsims)){
  new.dir[,,i] <- t(o.dir[i,,])
}


# mit Länder ID

new.dirid <- array(NA,c(299,8,nsims*mu_nsims))

for(i in 1:(nsims*mu_nsims)){
  new.dirid[,,i] <- cbind(t(o.dir[i,,]),wahl1317$Land1)
}

Pdirect <- array(NA,c(16,7,nsims*mu_nsims))


for(j in 1:(nsims*mu_nsims)){
  for(i in 1:16){
    for(k in 1:7){
      Pdirect[i,k,j] <- sum(new.dirid[,k,j][new.dirid[,8,j]==i])
    }}}
##############################
# Mindestanzahl der Sitze pro Partei aus Direkt und Zweitstimmen
###############################

Pmin <- array(NA,c(16,7,nsims*mu_nsims))

for(k in 1:(nsims*mu_nsims)){
  for(j in 1:16){
    for(i in 1:7){
      Pmin[j,i,k] <- ifelse(Psitze[j,i,k]>=Pdirect[j,i,k],Psitze[j,i,k],Pdirect[j,i,k])
    }}}


totfin <- matrix(NA,nsims*mu_nsims,8)
for(i in 1:(nsims*mu_nsims)){
  totfin[i,] <- apply(finZstimme[,,i],2,sum)
}

# Die Summe der Länder IDs wird rausgeworfen

totfin <- totfin[,1:7]

totPmin <- matrix(NA,nsims*mu_nsims,7)
for(i in 1:(nsims*mu_nsims)){
  totPmin[i,] <- apply(Pmin[,,i],2,sum)
}


# Der kleinste Divisor aller Parteien bestimmt die endgültige Sitzanzahl

bundsitze <- matrix(NA,nsims*mu_nsims,7)
for(i in 1:(nsims*mu_nsims)){
  bundsitze[i,] <- round(totfin[i,]/min( round(  (totfin[i,]/(totPmin[i,]-0.48))[(totfin[i,]/(totPmin[i,]-0.48))>0],0)  ),0)
}

bt <- apply(bundsitze,1,sum)
which(bt>=700)


size <- 720
scenariolarge <- matrix(NA, nrow= length(which(bt>=size)), ncol = 6)
for(i in 1:length(which(bt>=size))){
  scenariolarge[i,] <- round(apply(simZ[which(bt>=size)[i],,],1,mean),1)
}
colnames(scenariolarge) <- c("Union", "SPD", "Linke", "Grüne", "FDP", "AfD")
plot(density(bt))
mean(bt)
quantile(bt, c(1/12,11/12))

div3 <- matrix(NA,nsims*mu_nsims,7)



for(j in 1:(nsims*mu_nsims)){
  cat("\n", j, "\t")
  for(i in 1:7){
    cat(i)
    div3[j,i] <-  ifelse(round(sum(Lergebnis[,i,j])/bundsitze[j,i],0)=="NaN",1,round(sum(Lergebnis[,i,j])/bundsitze[j,i],0))
    if(div3[j,i]!=1){
      while(sum(apply(rbind(round(Lergebnis[,i,j]/div3[j,i],0)
                            ,Pdirect[,i,j]),2,max))>bundsitze[j,i]){
        div3[j,i] <- div3[j,i]+1
      }
      while(sum(apply(rbind(round(Lergebnis[,i,j]/div3[j,i],0)
                            ,Pdirect[,i,j]),2,max))<bundsitze[j,i]){
        div3[j,i] <- div3[j,i]-1
      }
    }
  }}

# Endgültige Sitzverteilung

finsitze <- array(NA,c(7,16,nsims*mu_nsims))

for(j in 1:(nsims*mu_nsims)){
  for(i in 1:7){
    finsitze[i,,j] <-   apply(rbind(round(Lergebnis[,i,j]/div3[j,i],0)
                                    ,Pdirect[,i,j]),2,max)
  }}


saveRDS(finsitze, paste0("PVS_finsitze_wbet13.RDS"))

bt <- apply(finsitze,3,sum)
mean(bt)
quantile(bt, c(1/12,11/12))

#png("Appendix_Abb2.png", width = 1600*0.75, height = 900*0.75)
tiff("figures/Abbildung3.tiff", width = 16*0.75, height = 9*0.75, units = "in", res = 300)
plot(table(bt),
     main = "Simulierte Größen des Bundestags \n Prognose 2 Tage vor der Bundestagswahl 2017",
     xlab = "Anzahl der Sitze",
     ylab ="Häufigkeit",
     las = 1,
     xaxt = "n",
     bty = "n", 
     col = "grey")
axis(side = 1)
dev.off()
max(bt)
median(bt)
sum(bt >= 631)/length(bt)
sum(bt >= 700)/length(bt)
bt_partei <- t(apply(finsitze, c(1,3), sum))

apply(bt_partei, 2, mean)
apply(bt_partei, 2, quantile, c(1/12, 11/12))

seat.table.lo <- t(apply(finsitze,c(1,2), quantile, 1/12))
seat.table.hi <- t(apply(finsitze,c(1,2), quantile, 11/12))

seat.table.ci <- matrix(NA, nrow = 16, ncol = 7)
for(i in 1:7){
  seat.table.ci[,i] <- paste0(seat.table.lo[,i],"-" ,round(seat.table.hi[,i],0))
}

colnames(seat.table.ci) <- c("CDU", "SPD", "Linke", "Grüne", "FDP", "AfD", "CSU")

rownames(seat.table.ci) <- unique(wahl1317$Land[order(wahl1317$Land1)])

landci <- rep(NA, 16)
for(i in 1:16){
  landci[i] <- paste0(round(quantile(apply(finsitze[,i,],2, sum), 1/12),0),"-",round(quantile(apply(finsitze[,i,],2, sum), 11/12),0))
}

seat.table.ci <- cbind(seat.table.ci, Insgesamt = landci)

parteici <- rep(NA, 7)

for(i in 1:7){
  parteici[i] <- paste0(quantile(apply(finsitze[i,,],2,sum), 1/12),"-",round(quantile(apply(finsitze[i,,],2,sum), 11/12),0))
}

seat.table.ci <- rbind(seat.table.ci,Insgesamt = c(parteici,paste0(quantile(bt,1/12),"-",quantile(bt,11/12))))
seat.table.ci
saveRDS(seat.table.ci, "PVS_stc.RDS")

htmlTable(seat.table.ci)

