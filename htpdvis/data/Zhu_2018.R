mmc1 <- read.table("Zhu_2018_mmc1.txt", row.names = 1, sep="\t", header = T, comment.char = "", check.names = F, quote = "")
mmc2 <- read.table("Zhu_2018_mmc2.txt", row.names = 1, sep="\t", header = T, comment.char = "", check.names = F, quote = "")
mmc2 <- mmc2[mmc2[, "Compound name"] != "", c("Compound name", "Class")]
M1 <- read.table("Zhu_2018_Table_M1.txt", sep="\t", header = T, comment.char = "", check.names = F)
M1 <- M1[, apply(M1, 2, function(x){sum(is.na(x))/length(x) < 0.5})]
metabolites <- colnames(M1)[grep("SlFM", colnames(M1))]
M1avg <- aggregate(M1[,metabolites], list(Accession=M1[,"Accessions"]), mean, an.rm=T)
rownames(M1avg) <- M1avg$Accession

resBPCA <- suppressWarnings(pca(as.matrix(M1avg[,metabolites]), method="bpca", nPcs=5))
## Get the estimated complete observations
M1avg[,metabolites] <- round(completeObs(resBPCA), 2)

withname <- colnames(M1avg)[colnames(M1avg) %in% rownames(mmc2)]
colnames(M1avg)[colnames(M1avg) %in% withname] <- paste(mmc2[withname, "Compound name"], mmc2[withname, "Class"], sep="|")
M1avg[, 'Group'] <- mmc1[rownames(M1avg), "Group"]
M1avg[, 'Botanical variety'] <- mmc1[rownames(M1avg), "Botanical variety"]
write.csv(M1avg, 'Zhu_2018_metabolites.csv', row.names = F)
