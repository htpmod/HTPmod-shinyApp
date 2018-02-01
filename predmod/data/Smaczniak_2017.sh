bedtools getfasta -fi /home/data/genomes/tair10/fasta/tair10.fa -bed AP1.bed -fo AP1.fa 
bedtools getfasta -fi /home/data/genomes/tair10/fasta/tair10.fa -bed AG.bed -fo AG.fa 

## cat ~/works/cezary/selex/Documents/*.fasta | grep -v ">" | sort | uniq -c | awk '{print ">"NR"_"$1"\n"$2}' > libALL.fa 

cut -d"," -f1,8 Smaczniak_2017_SELEX.csv | awk -vFS="," '(NR>1){print ">"$2"_"NR"\n"$1}' > Smaczniak_2017_SELEX_12mer.fa 
bowtie-build AP1.fa db/AP1.fa 
bowtie-build AG.fa db/AG.fa 

bowtie -v 0 -f db/AP1 Smaczniak_2017_SELEX_12mer.fa > AP1.aln 
bowtie -v 0 -f db/AG  Smaczniak_2017_SELEX_12mer.fa > AG.aln 

awk -vFS="\t" '{if(a[$1]){a[$1]=a[$1]";"FILENAME}else{a[$1]=FILENAME}}END{for(i in a){print i"\t"a[i]}}' AP1.aln AG.aln | sed -e 's/_/\t/;s/.aln//g' > Smaczniak_2017_SELEX_12mer.stat 

## R 

stat <- read.table('Smaczniak_2017_SELEX_12mer.stat', sep="\t")
dat <- read.csv('Smaczniak_2017_SELEX.csv', check.name=F)
dat[,'Group'] <- 'UN'
dat[stat$V2-1, 'Cluster'] == stat$V1
dat[stat$V2-1, 'Group'] <- as.character(stat$V3) 
write.csv(dat, 'Smaczniak_2017_SELEX_2.csv', row.names=F)

