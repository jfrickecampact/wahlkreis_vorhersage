# Handcoded Election Results for 09, 13 and 17

c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")
res13 <- c(41.5, 25.7, 8.6, 8.4, 4.8, 4.7, 6.3) / 100
res09 <- c(33.8, 23, 11.9, 10.7, 14.6, 0.1, 6) / 100
res05 <- c(35.2, 34.2, 8.7, 8.1, 9.8, 0.1, 3.9) / 100

btw_bund_res <- rbind(res13, res09, res05)

colnames(btw_bund_res) <-
  c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")
rownames(btw_bund_res) <- c("2013", "2009", "2005")


# Read in the candidate data.

full_df <-
  read.csv2("../raw-data/btw_candidates_1983-2017.csv",
            stringsAsFactors = F)