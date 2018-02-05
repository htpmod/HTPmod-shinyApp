## S4 <- read.table("Wang_2015_Supp_Table_S4.txt", skip=1, header = T, comment.char = "", check.names = F)
S5 <- read.table("Wang_2015_Supp_Table_S5.txt", skip=1, header = T, comment.char = "", check.names = F)
S5$group <- paste0("CS", S5$group)
out <- S5[, -(1:4)]
write.csv(out, 'Wang_2015_chromatin_state.csv', row.names = F)
