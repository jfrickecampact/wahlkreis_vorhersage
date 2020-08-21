# Load results data
library(stringi)

lres <- readRDS("../raw-data/lres.RDS")

all_res <- NULL

elections <- c(1980, 1983, 1987, 1990, 1994, 1998, 2002, 2005, 2009, 2013, 2017)


for(el in 1:length(elections)){
  election <- elections[el]
  res <- read.csv2(paste0("../raw-data/btw",substr(election, start = 3, stop = 4),"_cleaned.csv"), stringsAsFactors = F)
  
  
  res <- res[!res$Wkr > 500,]
  if(election == 2017) res <- res[!res$Land > 16,]
  res <- res[!is.na(res$Wkr), ]
  res[is.na(res)] <- 0
  
  
  if(election >= 1998){
    res$Land <- as.factor(res$Land)
    
    if(election == 1998) levels(res$Land) <- c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "MV", "BB", "ST", "TH", "SN")
    
    if(election >= 2002) levels(res$Land) <- c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH")
    
    
    res$Land <- as.character(res$Land)
  }
  
  res$Wkr_name <- stri_replace_all_fixed(
    tolower(res$Wkr_name), 
    c("ä", "ö", "ü", "ß", "\u0096"), 
    c("ae", "oe", "ue", "ss", "-"), 
    vectorize_all = FALSE
  )
  
  res$Wkr_name <- gsub("\\s", "", res$Wkr_name)
  
  all_res[[paste0(election)]] <- res
}  



full_df <- NULL


#election <- 1980
election <- 1980

res <- read.csv2(paste0("../raw-data/btw",substr(election, start = 3, stop = 4),"_cleaned.csv"), stringsAsFactors = F)


res <- res[!res$Wkr > 500,]

res <- res[!is.na(res$Wkr), ]
res[is.na(res)] <- 0



# Load candidate data
cand <-  read.csv2(paste0("../raw-data/btw",election,"bewerb_clean.csv"), stringsAsFactors = F)

all_cand <- cand

# How many candidates on each list by state?



if(election < 2017){
  
  for(i in 1:nrow(cand)){
    if(cand$Land[i] == "") {
      cand$Land[i] <- res$Land[res$Wkr == cand$Wkr[i]] 
    }
  }
  
  maxlist <- NULL
  states <- unique(cand$Land)[!(unique(cand$Land) %in% "")]
  for(land in states){
    maxlist <- rbind(maxlist, as.numeric(table(as.factor(cand$Partei)[cand$Land==land])))
    rownames(maxlist)[which(states == land)] <- land
  }
  colnames(maxlist) <- names(table(as.factor(cand$Partei)))
}


cand <- cand[!is.na(cand$Wkr),]




# Number of candidates per Wkr

ncand <- data.frame(table(cand$Wkr))

colnames(ncand) <- c("Wkr", "ncand")


cand$res_E <- NA
cand$resp_E <- NA
cand$res_Z <- NA
cand$resp_Z <- NA
cand$ncand <- NA 
cand$res_l1 <- 0
cand$propPlatz <- 0
cand$W <- NA
cand$Wkr_name <- NA
cand$election <- election




for(i in 1:nrow(cand)){
  cand$res_E[i] <- res[res$Wkr == cand$Wkr[i], paste0(cand$Partei[i],"_E")]
  cand$resp_E[i] <- res[res$Wkr == cand$Wkr[i], paste0(cand$Partei[i],"_E")] /
    res[res$Wkr == cand$Wkr[i], "G_E"]
  
  cand$W[i] <- res[res$Wkr == cand$Wkr[i], "W"]
  
  cand$Wkr_name[i] <- tolower(res$Wkr_name[res$Wkr == cand$Wkr[i]])
  
  
  if(paste0(cand$Partei[i],"_Z") %in% colnames(res)){
    cand$res_Z[i] <- res[res$Wkr == cand$Wkr[i], paste0(cand$Partei[i],"_Z")]
    cand$resp_Z[i] <- res[res$Wkr == cand$Wkr[i], paste0(cand$Partei[i],"_Z")] /
      res[res$Wkr == cand$Wkr[i], "G_Z"]
  }
  
  
  if(election < 2017){
    if(!is.na(cand$Platz[i])){
      cand$propPlatz[i] <-  1 - cand$Platz[i] / maxlist[cand$Land[i], cand$Partei[i]]
    }
  }
  

  
  cand$ncand[i] <- ncand$ncand[ncand$Wkr == cand$Wkr[i]]
}


cand$alsoList <- (cand$propPlatz == 0) * 1

cand$winner <- NA

for(wkr in 1:max(cand$Wkr)){
  cand$winner[cand$Wkr == wkr] <- (cand[cand$Wkr == wkr, "res_E"] == max(cand[cand$Wkr == wkr, "res_E"]))*1
}


female <- as.numeric(as.factor(cand$Geschlecht)) - 1

all_cand_old <- all_cand

cand1980 <- cand

for(el in 2:length(elections)){
  election <- elections[el]
res <- read.csv2(paste0("../raw-data/btw",substr(election, start = 3, stop = 4),"_cleaned.csv"), stringsAsFactors = F)


res <- res[!res$Wkr > 500,]
if(election == 2017) res <- res[!res$Land > 16,]
res <- res[!is.na(res$Wkr), ]
res[is.na(res)] <- 0


if(election >= 1998){
res$Land <- as.factor(res$Land)

if(election == 1998) levels(res$Land) <- c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "MV", "BB", "ST", "TH", "SN")

if(election >= 2002) levels(res$Land) <- c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH")


res$Land <- as.character(res$Land)
}



# Load candidate data
cand <-  read.csv2(paste0("../raw-data/btw",election,"bewerb_clean.csv"), stringsAsFactors = F)



if(election == 2017) {
  cand$Namenszusatz <- ifelse(cand$Namenszusatz == "", cand$Namenszusatz, paste0(" ",cand$Namenszusatz))
  cand$Titel <- ifelse(cand$Titel == "", cand$Titel, paste0(" ",cand$Titel))
  cand$Name <- trimws(paste0(cand$Name,cand$Namenszusatz, ",",cand$Titel, " ", cand$Vorname))
}

cand$Name <- stri_replace_all_fixed(
  tolower(cand$Name), 
  c("ä", "ö", "ü", "ß"), 
  c("ae", "oe", "ue", "ss"), 
  vectorize_all = FALSE
)

all_cand <- cand

# How many candidates on each list by state?



if(election < 2017){
  
  for(i in 1:nrow(cand)){
    if(cand$Land[i] == "") {
      cand$Land[i] <- res$Land[res$Wkr == cand$Wkr[i]] 
    }
  }
  
maxlist <- NULL
states <- unique(cand$Land)[!(unique(cand$Land) %in% "")]
for(land in states){
  maxlist <- rbind(maxlist, as.numeric(table(as.factor(cand$Partei)[cand$Land==land])))
  rownames(maxlist)[which(states == land)] <- land
}
colnames(maxlist) <- names(table(as.factor(cand$Partei)))
}

if(election == 2017){
  cand$Land <- ifelse(cand$Land == "", cand$Liste_Land, cand$Land)
  cand$Partei <- ifelse(cand$Partei == "", cand$Partei_Liste, cand$Partei)
  maxlist <- NULL
  states <- unique(cand$Land)[!(unique(cand$Land) %in% "")]
  for(land in states){
    maxlist <- rbind(maxlist, as.numeric(table(as.factor(cand$Partei)[cand$Land==land])))
    rownames(maxlist)[which(states == land)] <- land
  }
  colnames(maxlist) <- names(table(as.factor(cand$Partei)))
}

cand <- cand[!is.na(cand$Wkr),]




# Number of candidates per Wkr

ncand <- data.frame(table(cand$Wkr))

colnames(ncand) <- c("Wkr", "ncand")


cand$res_E <- NA
cand$resp_E <- NA
cand$res_Z <- NA
cand$resp_Z <- NA
cand$ncand <- NA 
cand$res_l1_Z <- 0
cand$res_l1_E <- 0
cand$propPlatz <- 0
cand$W <- NA
cand$Wkr_name <- NA
cand$election <- election


for(i in 1:nrow(cand)){
cand$res_E[i] <- res[res$Wkr == cand$Wkr[i], paste0(cand$Partei[i],"_E")]
cand$resp_E[i] <- res[res$Wkr == cand$Wkr[i], paste0(cand$Partei[i],"_E")] /
  res[res$Wkr == cand$Wkr[i], "G_E"]

cand$W[i] <- res[res$Wkr == cand$Wkr[i], "W"]

#cand$Wkr_name[i] <- tolower(res$Wkr_name[res$Wkr == cand$Wkr[i]])

cand$Wkr_name[i] <- stri_replace_all_fixed(
  tolower(res$Wkr_name[res$Wkr == cand$Wkr[i]]), 
  c("ä", "ö", "ü", "ß", "\u0096"), 
  c("ae", "oe", "ue", "ss", "-"), 
  vectorize_all = FALSE
)

cand$Wkr_name[i] <- gsub("\\s", "", cand$Wkr_name[i])

if(paste0(cand$Partei[i],"_Z") %in% colnames(lres[[paste(elections[which(elections %in% election) - 1])]])) {
  if( res[res$Wkr==cand$Wkr[i], "Land"] %in% rownames(lres[[paste(elections[which(elections %in% election) - 1])]]) ){
  cand$res_l1_Z[i] <- cand$res_l1_E[i] <- lres[[paste(elections[which(elections %in% election) - 1])]][rownames(lres[[paste(elections[which(elections %in% election) - 1])]]) == res[res$Wkr==cand$Wkr[i], "Land"], paste0(cand$Partei[i],"_Z")]
  }
}

if(paste0(cand$Partei[i],"_Z") %in% colnames(res)){
cand$res_Z[i] <- res[res$Wkr == cand$Wkr[i], paste0(cand$Partei[i],"_Z")]
cand$resp_Z[i] <- res[res$Wkr == cand$Wkr[i], paste0(cand$Partei[i],"_Z")] /
  res[res$Wkr == cand$Wkr[i], "G_Z"]
}



if(election < 2017){
if(!is.na(cand$Platz[i])){
  cand$propPlatz[i] <-  1 - cand$Platz[i] / maxlist[cand$Land[i], cand$Partei[i]]
}
}

if(election == 2017){
  if(!is.na(cand$Liste_Platz[i])){
    cand$propPlatz[i] <-  1 - cand$Liste_Platz[i] / maxlist[cand$Land[i], cand$Partei[i]]
  }
}

cand$ncand[i] <- ncand$ncand[ncand$Wkr == cand$Wkr[i]]
}


cand$alsoList <- (cand$propPlatz == 0) * 1

cand$winner <- NA

for(wkr in 1:max(cand$Wkr)){
cand$winner[cand$Wkr == wkr] <- (cand[cand$Wkr == wkr, "res_E"] == max(cand[cand$Wkr == wkr, "res_E"]))*1
}

cand$second <- NA

for(wkr in 1:max(cand$Wkr)){
  cand$second[cand$Wkr == wkr] <-(cand[cand$Wkr == wkr, "res_E"] == -sort(-cand[cand$Wkr == wkr, "res_E"])[2])*1
}

cand$formercand <- (tolower(paste(cand$Name, cand$Geburtsjahr, cand$Geburtsort)) %in% tolower(paste(all_cand_old$Name, all_cand_old$Geburtsjahr, all_cand_old$Geburtsort)))*1


female <- as.numeric(as.factor(cand$Geschlecht)) - 1

all_cand_old <- rbind(all_cand_old[, c("Name", "Geburtsjahr", "Geburtsort")], all_cand[, c("Name", "Geburtsjahr", "Geburtsort")])




full_df <- rbind(full_df, model.frame(cand$resp_E~cand$ncand + cand$res_l1_Z +  cand$res_l1_E +
                                        cand$propPlatz + cand$election + 
                                        cand$alsoList + cand$winner + cand$second +
                                        cand$formercand + cand$Name + 
                                        cand$Land + cand$Wkr + cand$Partei + 
                                        cand$Wkr_name + cand$resp_Z + female))

}


colnames(full_df) <- c("resp_E", "ncand", "res_l1_Z", "res_l1_E","propPlatz", "election", "alsoList", "winner", "second", "formercand", "name", "land", "wkr", "partei", "wkr_name", "resp_Z", "female")

full_df$east <- (full_df$land %in% c("BE", "BB", "MV", "SN", "ST", "TH"))*1

full_df$partei[full_df$partei=="B90"] <- "GRUENE"
full_df$partei[full_df$partei=="PDS"] <- "LINKE"

full_df$incumbent <- 0

colnames(full_df)

full_df$incumbent[full_df$formercand == 1 & full_df$election == 1983] <- tolower(
  paste(full_df[full_df$formercand == 1 & full_df$election == 1983, "name"], full_df[full_df$formercand == 1 & full_df$election == 1983, "partei"])) %in% tolower(paste(cand1980$Name[cand1980$winner == 1], cand1980$Partei[cand1980$winner == 1]))*1

elections <- c(1980, 1983, 1987, 1990, 1994, 1998, 2002, 2005, 2009, 2013, 2017)
election <- 1980

for(el in 3:length(elections)){

full_df$incumbent[full_df$formercand == 1 & full_df$election == elections[el]] <- tolower(
  paste(full_df[full_df$formercand == 1 & full_df$election == elections[el], "name"],full_df[full_df$formercand == 1 & full_df$election == elections[el], "partei"] )) %in% tolower(paste(full_df$name[full_df$winner == 1 & full_df$election == elections[el-1]],full_df$partei[full_df$winner == 1 & full_df$election == elections[el-1]]))*1

}

full_df$akad <- (gsub(".*,\\s(.*)\\..*", "\\1", full_df$name) != full_df$name)*1


for(i in 1:nrow(full_df)){
  if(full_df[i,"wkr_name"] %in% all_res[[which(elections == full_df[i,"election"])-1]]$Wkr_name){
    if(paste0(full_df[i,"partei"],"_Z") %in% colnames(all_res[[which(elections == full_df[i,"election"])-1]]) ) {
      
      if(all_res[[which(elections == full_df[i,"election"])-1]][all_res[[which(elections == full_df[i,"election"])-1]]$Wkr_name == full_df[i,"wkr_name"],paste0(full_df[i,"partei"],"_E")] >= 0){
        
        full_df$res_l1_Z[i] <-   all_res[[which(elections == full_df[i,"election"])-1]][all_res[[which(elections == full_df[i,"election"])-1]]$Wkr_name == full_df[i,"wkr_name"],paste0(full_df[i,"partei"],"_Z")] / 
          all_res[[which(elections == full_df[i,"election"])-1]][all_res[[which(elections == full_df[i,"election"])-1]]$Wkr_name == full_df[i,"wkr_name"],"G_Z"]
        full_df$res_l1_E[i] <-   all_res[[which(elections == full_df[i,"election"])-1]][all_res[[which(elections == full_df[i,"election"])-1]]$Wkr_name == full_df[i,"wkr_name"],paste0(full_df[i,"partei"],"_E")] / 
          all_res[[which(elections == full_df[i,"election"])-1]][all_res[[which(elections == full_df[i,"election"])-1]]$Wkr_name == full_df[i,"wkr_name"],"G_E"]
      }
      
    } 
  }
}





incumbent_in_wkr <- aggregate(full_df$incumbent, list(paste(full_df$wkr, full_df$election)), sum)

full_df$incumbent_in_wkr <- 0

for(i in 1:nrow(full_df)){
full_df$incumbent_in_wkr[i] <- incumbent_in_wkr$x[incumbent_in_wkr$Group.1 ==  paste(full_df$wkr[i], full_df$election[i])]
}



and <- full_df[!full_df$partei %in% c("CDU", "CSU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And"),]
colnames(and)

for(el in elections[2:length(elections)]){
  for(wkr in unique(and$wkr[and$election == el])){

and[and$election == el & and$wkr == wkr,c(1,3,16)][1,] <- apply(and[and$election == el & and$wkr == wkr,c(1,3,16)],2,sum)
and[and$election == el & and$wkr == wkr,c("propPlatz", "female", "formercand", "akad")][1,] <- apply(and[and$election == el & and$wkr == wkr,c("propPlatz", "female", "formercand", "akad")],2,mean)
and[and$election == el & and$wkr == wkr,c(11,14)][1,] <- "And"
}
}

and <- and[and$name=="And",]

full_df <- full_df[full_df$partei %in% c("CDU", "CSU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And"),]
full_df <- rbind(full_df, and)

write.csv2(full_df, "../raw-data/btw_candidates_1983-2017.csv", row.names = F)
rm(list = ls())
