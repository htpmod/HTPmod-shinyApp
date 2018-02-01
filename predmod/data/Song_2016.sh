awk 'BEGIN{while(getline<"ath.tss.bed"){a[$4]=$1"\t"$2"\t"$3}}{if(a[$1]) print a[$1]"\t"$1"\t"$3}' Song_2016_aag1550_Table_S1.txt | sed -e 's/Chr/chr/' | bedtools sort -i - > DEgene.tss.bed
for bed in *narrowPeak_p16.bed; do 
    out=$(echo $bed | sed -e 's/GSE80564_//; s/_optimal_narrowPeak_p16.bed//')
    echo $out 
    awk -vFS="\t" -vOFS="\t" '{p=$2+$10;print $1,p,p+1,$7}' $bed | bedtools sort -i - | bedtools closest -a DEgene.tss.bed -b - -d -t first | awk -vFS="\t" -vOFS="\t" '{if(($(NF)<3000)){print $4,$5,$9,$(NF)}else{print $4,$5,0,$(NF)}}' > $out.target.out 
done 

exit 0 

### R
DEgenes <- read.table('DEgene.tss.bed', sep="\t", row.names=4, head=F)
out <- data.frame(Gene=rownames(DEgenes), DE=DEgenes[,4])
files <- list.files(pattern="target.out")
for(TF in files){
    TFout <- read.table(TF, sep="\t", row.names=1, head=F)
    out <- cbind(out, TFout[,2]*exp(-TFout[,3]/3000))
}
TFs <- gsub(".target.out", "", files)
TFs <- gsub("_ABA", "|ABA", TFs)
TFs <- gsub("_EtOH", "|EtOH", TFs)
TFs <- gsub("AT.*_", "", TFs)
colnames(out) <- c("Gene", "log2FC", TFs)
write.table(out, "Song_2016_DE_regression.csv", row.names=F, sep=',')
