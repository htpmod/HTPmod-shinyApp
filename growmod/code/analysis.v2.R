################################################
# Author: Dijun Chen (chend@ipk-gatersleben.de)
# Update on Sept 2, 2014
################################################
cat("loading analysis.R \n")

############# fetch untils #############
## download a list of files from the server and save them in a local directory
download.files <- function(server.url, local.path, files) {
	for (fn in files) {
		fserv <- paste(server.url, fn, sep="")
		fdest <- paste(local.path, fn, sep="")
		my.try <- try(
			download.file(url=fserv, destfile=fdest)
		)
		if(is(my.try,"try-error")){
			stop(paste("can not download", fserv, "into", fdest, ":", my.try))
		}
	}
}
## download a zip file and unzip it in the current working directory
download.zipfile <- function(zipfile){
	# create a temporary directory and a placeholder file
	tf <- tempfile(tmpdir=tempdir(), fileext=".zip")
	# download into the placeholder file
	download.file(zipfile, tf)
	# get the name of the first file in the zip archive
	fnames <- unzip(tf, list=TRUE)$Name
	# unzip the file to the working directory
	unzip(tf, files=fnames, exdir=getwd(), overwrite=TRUE)
	unlink(tf)
	# fpath - is the full path to the extracted file
	# fpath <- file.path(getwd(), fnames)
	# Return value
	fnames
}

## If you want to source() a bunch of files, something like
## the following may be useful:
sourceDir <- function(path, trace=TRUE, ...) {
	for(nm in list.files(path, pattern="[.][RrSsQq]$")) {
		if(trace) cat(nm, ":")
		source(file.path(path, nm), ...)
		if(trace) cat("\n")
	}
}

## Function for outlier removal based on Grubbs tests
## fill=TRUE: fill the outlier by mean value
## fill=FALSE: set the outlier by NA
grubbs.test.fill.outliers <- function(x, fill=TRUE){
	if(is.null(x)) return(x)
	require(outliers)
	if (is.matrix(x)){
		apply(x, 2, grubbs.test.fill.outliers, fill=fill)
	}else if (is.data.frame(x)){
		as.data.frame(sapply(x, grubbs.test.fill.outliers, fill=fill))
	}else{
		if (all(is.na(x)) || length(unique(x)) < 2 
				|| sum(is.na(x)) > length(x)/2 
				|| sum(!is.na(x)) < 6) return(x)
		while(grubbs.test(x)$p.value < 0.01){
			if(length(unique(x[!is.na(x)])) < 2) break
			if(fill){ ## filled by mean value
				x[which(x == outlier(x))] <- mean(x[-which(x == outlier(x))], na.rm=TRUE) 
			}else{ ## filled by NA
				x[which(x == outlier(x))] <- NA
			}
		}
		return(x)
	}
}

## check if the column exists 
column.check <- function(x, ...){
	columns <- list(...)
	lapply(columns, function(cn){
		if (!(cn %in% names(x))) 
			stop(paste("Column \"", cn, "\" not found!", sep=""))
	})
	invisible(TRUE)
}

## Adopt Grubbs’ test to detect outliers based on the assumption of normal distribution 
## of phenotypic data points for repeated measures on replicated iplants of a single 
## genotype for each trait. 
outlier.detection <- function(input, col.day="DAS", col.genotype="Genotype", 
							col.treatment="Treatment", fill=FALSE){
	column.check(input, col.day, col.genotype, col.treatment)
	library(tcltk)
	pb <- tkProgressBar("Analysis status", "<Processing>: genotype \"\t\t\" on day \t", 0, 100, 0)
	ind <- 0
	genotypes <- unique(as.character(input[, col.genotype]))
	treatments <- unique(as.character(input[, col.treatment]))
	days <- unique(input[, col.day])
	result <- input
	for(genotype in genotypes){
		for(day in days){
			if(length(treatments) > 0){
				for(treatment in treatments){
					plant.rows <- which(input[, col.genotype] %in% genotype & 
										input[, col.treatment] %in% treatment & 
										input[, col.day] == day)
					plant.data <- input[plant.rows, traits]
					result[plant.rows, traits] <- grubbs.test.fill.outliers(plant.data, fill=fill)
				}
			}else{
				plant.rows <- which(input[, col.genotype] %in% genotype & 
									input[, col.day] == day)
				plant.data <- input[plant.rows, traits]
				result[plant.rows, traits] <- grubbs.test.fill.outliers(plant.data, fill=fill)
			}
			## show status in a progress bar
			i <- (ind <- ind + 1)/length(genotypes)/length(days) * 100
			info <- sprintf("%d%% done", round(i))
			msg <- sprintf("<Processing>: genotype \"%s\" on day %s", genotype, day)
			setTkProgressBar(pb, i, info, msg)
		}
	}
	close(pb)
	invisible(result)
}

## phenotypic similarity tree
phenotypic.tree.plot <- function(distance, direction='leftwards', 
                                 node.cols=1, node.pchs=1, ...){
  require(ape)
  hc <- hclust(distance)
	plot(as.phylo(hc), "c", FALSE, pch=16, tip.color=node.cols, label.offset=1, 
       adj=ifelse(direction=='leftwards', 1, 0), no.margin=TRUE, edge.width=2, 
       direction=direction, edge.color='black', xpd=TRUE, ...)
	tiplabels(pch=node.pchs, col=node.cols, bg=node.cols, cex=3, 
            adj=ifelse(direction=='leftwards', 0, 1))
}

## ANOVA
anova.analysis <- function(input, traits, col.genotype="Genotype", 
							col.day="DAS", col.treatment="Treatment"){
	column.check(input, col.day, col.genotype, col.treatment)
	library(tcltk)
	pb <- tkProgressBar("Analysis status", "<Processing>: trait: \"\t\t\t\t\"", 0, 100, 0)
	ind <- 0
	days <- unique(input[, col.day])
	genotypes <- unique(as.character(input[, col.genotype]))
	treatments <- unique(as.character(input[, col.treatment]))
	result <- expand.grid(Trait=traits, DAS=days, g.pval=NA, t.pval=NA, i.pval=NA, 
							g.sq=NA, t.sq=NA, i.sq=NA)
	for (trait in traits){
		for (day in days){
			rid <- which(result$Trait == trait & result$DAS == day)
			aov.data <- input[which(input[, col.day] == day), 
								c(col.genotype, col.treatment, trait)]
			colnames(aov.data) <- c("Genotype", "Treatment", "Trait")
			aov.data <- na.omit(aov.data)
			if(nlevels(factor(aov.data$Treatment)) > 1 & 
					nlevels(factor(aov.data$Genotype)) > 1 & 
					nrow(aov.data) > length(genotypes)*2){ 
				aov.obj <- aov(Trait~Genotype*Treatment, data=aov.data)
				aov.pvals <- summary(aov.obj)[[1]][["Pr(>F)"]]
				result[rid, c("g.pval", "t.pval", "i.pval")] <- aov.pvals[1:3]
				aov.sq <- summary(aov.obj)[[1]][["Sum Sq"]]
				aov.sq <- aov.sq * 100 / sum(aov.sq, na.rm=TRUE)
				result[rid, c("g.sq", "t.sq", "i.sq")] <- aov.sq[1:3]
			}else if(nlevels(factor(aov.data$Genotype)) > 1 & 
					nrow(aov.data) > length(genotypes)*2){ ## test for "Genotype"
				aov.obj <- aov(Trait~Genotype, data=aov.data)
				aov.pvals <- summary(aov.obj)[[1]][["Pr(>F)"]]
				result[rid, "g.pval"] <- aov.pvals[1]
				aov.sq <- summary(aov.obj)[[1]][["Sum Sq"]]
				aov.sq <- aov.sq * 100 / sum(aov.sq, na.rm=TRUE)
				result[rid, "g.sq"] <- aov.sq[1]
			}else if(nlevels(factor(aov.data$Treatment)) > 1 & 
					nrow(aov.data) > length(treatments)*2){ ## test for "Treatment"
				aov.obj <- aov(Trait~Treatment, data=aov.data)
				aov.pvals <- summary(aov.obj)[[1]][["Pr(>F)"]]
				result[rid, "t.pval"] <- aov.pvals[1]
				aov.sq <- summary(aov.obj)[[1]][["Sum Sq"]]
				aov.sq <- aov.sq * 100 / sum(aov.sq, na.rm=TRUE)
				result[rid, "t.sq"] <- aov.sq[1]
			}
		}
		## show status in a progress bar
		i <- (ind <- ind + 1)/length(traits) * 100
		info <- sprintf("%d%% done", round(i))
		msg <- sprintf("<Processing>: trait \"%s\"", trait)
		setTkProgressBar(pb, i, info, msg)
	}
	## Adjust p-values for multiple comparisons
	result[, 'g.fdr'] <- p.adjust(result[, 'g.pval'], method="fdr")
	result[, 't.fdr'] <- p.adjust(result[, 't.pval'], method="fdr")
	result[, 'i.fdr'] <- p.adjust(result[, 'i.pval'], method="fdr")
	close(pb)
	invisible(result)
}

## heatmap plot for the anova analysis result
anova.result.heatmap.plot <- function(anova.result, fdr, trait.col, day.col, ...){
	require(gplots)
	fdr.mat <- reshape(anova.result[, c('Trait', 'DAS', fdr)], idvar="Trait", 
						timevar="DAS", direction="wide")
	colnames(fdr.mat) <- gsub('.*\\.', '', colnames(fdr.mat))
	rownames(fdr.mat) <- 1:nrow(fdr.mat)
	fdr.mat <- fdr.mat[, -1]
	fdr.mat[is.na(fdr.mat)] <- 1
	fdr.mat[fdr.mat==0] <- min(fdr.mat[fdr.mat!=0])
	lod.mat <- -log(fdr.mat, 10)
	lod.cols <- colorRampPalette(c("#FFFFFF", "#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", 
									"#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15"))(100)
	heatmap.2(as.matrix(lod.mat), col=lod.cols, scale='none', Colv=FALSE,
				RowSideColors=trait.col, ColSideColors=day.col, trace='none',
				key=TRUE, symkey=FALSE, density.info='none', ...)
}

## PCA over time
pca.analysis <- function(input, day=NULL, traits=NULL, treatment=NULL, nPcs=6, 
							col.plant="PlantID", col.genotype="Genotype", col.day="DAS",  
							col.treatment="Treatment", min.data.points=10) {
	column.check(input, col.day, col.plant, col.genotype, col.treatment)
	
	require(pcaMethods)
	if(is.nothing(treatment) || treatment=='all'){
		pca.data <- input[which(input[, col.day] == day), 
							c(col.plant, col.genotype, col.treatment, traits)]
	}else{
		pca.data <- input[which(input[, col.day] == day & input[, col.treatment] == treatment), 
							c(col.plant, col.genotype, col.treatment, traits)]
	}
	if(nrow(pca.data) < min.data.points){
		warning(">>> [PCA] No data, ", col.day, "=", day, "\n");
		return(NULL)
	}
	rownames(pca.data) <- pca.data[, col.plant]
	pca.data[, traits][is.nan(as.matrix(pca.data[, traits]))] <- NA
	## removing NA rows
	pca.data <- pca.data[rowSums(is.na(pca.data)) < length(traits), ]
	## removing NA cols
	pca.data <- pca.data[, colSums(is.na(pca.data)) < nrow(pca.data)/2]
	iplants <- rownames(pca.data)
	itraits <- intersect(colnames(pca.data), traits)
	if(nrow(pca.data) < min.data.points){
		warning(">>> [PCA] No data, ", col.day, "=", day, "\n");
		return(NULL)
	}
	## Perform Bayesian PCA with * components
	resBPCA <- pca(as.matrix(pca.data[, itraits]), method="bpca", nPcs=2*nPCs)
	## Get the estimated complete observations
	imputed <- completeObs(resBPCA)
	## overview of the first six PCs
	pca.out <- pca(imputed, center=TRUE, scale="uv", scan=FALSE, nPcs=nPCs)
	invisible(pca.out)
}

##
plot.trait.overtime <- function(input, trait, col=NA, pch=NA, 
								col.day="DAS", col.treatment="Treatment"){
	column.check(input, trait, col.day, col.treatment)
	if(length(trait) > 1){
		trait <- trait[1]
	}
	
	plot.data <- input[, c(col.day, col.treatment, trait)]
	plot.data <- aggregate(plot.data[, trait], plot.data[, c(col.day, col.treatment)],
							mean, na.rm=TRUE)
	das <- plot.data[, col.day]
	plot(0, type='n', xlab=col.day, xlim=range(das, na.rm=TRUE), 
			ylab=trait, ylim=range(plot.data[, 3], na.rm=TRUE))
	xseq <- seq(min(das, na.rm=TRUE), max(das, na.rm=TRUE), length=100)
	treatments <- unique(as.character(plot.data[, col.treatment]))
	if(any(is.na(col[treatments]))){
		col <- rainbow(n=length(treatments))
		names(col) <- treatments
	}
	if(any(is.na(pch[treatments]))){
		pch <- (1:length(treatments)) %% 25
		names(pch) <- treatments
	}
	level <- 0.95
	std <- qnorm(level/2 + 0.5)
	for(treatment in treatments){
		color <- col[treatment]
		m.data <- plot.data[plot.data[, col.treatment]==treatment, ]
		m.data <- data.frame(x=m.data[, col.day], y=m.data[, 3])
		dat.lo <- loess(y~x, m.data, span=0.5)
		pred <- predict(dat.lo, data.frame(x=xseq), se=TRUE)
		points(m.data, type='p', col=color, pch=pch[treatment])
		sub.days <- seq(from=10, to=max(das, na.rm=TRUE), by=5)
		cols <- col2rgb(color)/255
		cols <- rgb(cols[1], cols[2], cols[3], 0.5)
		polygon(x=c(xseq, rev(xseq)), y=c(pred$fit-std*pred$se, rev(pred$fit+std*pred$se)), 
					col=cols, border=NA)
		lines(xseq, pred$fit, lwd=2, col=color)
	}
	my.legend("bottomright", legend=treatments, pch=pch, col=col, text.col=col, bg=NA)
}

## analysis of trait reproducibility
## genotype.num: the number of randomly selected genotypes for comparison (default: the half number of total genotypes)
trait.reproducibility.calculation <- function(input, traits, genotype.num=NA, min.reps=5, col.plant="PlantID", 
							col.genotype="Genotype", col.day="DAS", col.treatment="Treatment"){
	column.check(input, col.day, col.plant, col.genotype, col.treatment)
	
	treatments <- unique(as.character(input[, col.treatment]))
	library(tcltk)
	pb <- tkProgressBar("Analysis status", "<Processing>: treatment: \"\t\t\", trait: \"\t\t\t\t\"", 0, 100, 0)
	ind <- 0
	nt <- length(traits)
	result <- list()
	for(treatment in treatments){
		days <- unique(input[, col.day])
		data <- input[which(input[, col.treatment]==treatment & input[, col.day] %in% days), ]
		genotypes <- unique(as.character(data[, col.genotype]))
		summary.table <- matrix(nrow=nt, ncol=3, dimnames=list(traits, c("p.value", "median.cor", "random.cor")))
		detailed.table <- random.table <- matrix(nrow=length(traits), ncol=length(genotypes), 
											dimnames=list(traits, genotypes))
		if(is.na(genotype.num) || !is.numeric(genotype.num)){
			genotype.num <- ceiling(length(genotypes)/2)
		}
		if(genotype.num > length(genotypes)-1){
			genotype.num <- length(genotypes)-1
		}
		for (trait in traits){
			##cat(">>> trait: ", trait, "                              \r")
			## flush.console()
			for (genotype in genotypes){
				row.ind <- which(data[, col.genotype]==genotype)
				sub.data <- data[row.ind, c(col.plant, col.day, trait)]
				mean.data <- tapply(sub.data[, trait], list(sub.data[, col.day], as.character(sub.data[, col.plant])), 
									mean, na.rm=TRUE)
				rep.num <- ncol(mean.data)
				if(rep.num >= min.reps){ ## at least five iplants
					innerCors <- cor(mean.data, use="pairwise.complete.obs")
					detailed.table[trait, genotype] <- median(innerCors[lower.tri(innerCors)], na.rm=TRUE)
				}
				## randomly select another group of genotypes for comparison
				another <- sample(genotypes[genotypes!=genotype], genotype.num, replace=FALSE)
				## if (another == genotype) {next;}
				row.ind <- which(data[, col.genotype] %in% c(genotype, another))
				sub.data <- data[row.ind, c(col.plant, col.day, col.genotype, trait)]
				plant2genotype <- unique(sub.data[, c(col.plant, col.genotype)])
				rownames(plant2genotype) <- plant2genotype[, col.plant]
				mean.data <- tapply(sub.data[, trait], list(sub.data[, col.day], as.character(sub.data[, col.plant])), 
								mean, na.rm=TRUE)
				rep.num <- ncol(mean.data)
				if(rep.num >= min.reps){ ## at least five iplants
					cors <- cor(mean.data, use="pairwise.complete.obs")
					sub.rows <- as.character(plant2genotype[which(plant2genotype[, col.genotype]==genotype), col.plant])
					sub.cols <- as.character(plant2genotype[which(plant2genotype[, col.genotype] %in% another), col.plant])
					random.table[trait, genotype] <- median(cors[sub.rows, sub.cols], na.rm=TRUE)
				}
			}
			if(sum(!is.na(detailed.table[trait,])) > 5 & sum(!is.na(random.table[trait,])) > 5){
				p <- t.test(detailed.table[trait,], random.table[trait,], alternative="greater", paired=TRUE)$p.value
				summary.table[trait, "p.value"] <- p
				summary.table[trait, "median.cor"] <- median(detailed.table[trait,], na.rm=TRUE)
				summary.table[trait, "random.cor"] <- median(random.table[trait,], na.rm=TRUE)
			}
			## show status in a progress bar
			i <- (ind <- ind + 1)/nt/length(treatments) * 100
			info <- sprintf("%d%% done", round(i))
			msg <- sprintf("<Processing>: treatment: \"%s\"\ntrait: \"%s\"", treatment, trait)
			setTkProgressBar(pb, i, info, msg)
		}
		result[[treatment]] <- list(table=detailed.table, summary=summary.table)
	}
	## Sys.sleep(0.1)
	close(pb)
	invisible(result)
}

## is.na + is.null + length==0 + ""
is.nothing <- function(x, false.triggers=FALSE){
	if(is.function(x)) return(FALSE)
	return(
		is.null(x) || length(x) == 0 || all(is.na(x)) || all(x=="") || (false.triggers && all(!x))
	)
}

## growth modelling for control iplants
control.plant.growth.modeling <- function(input, plant.id=NULL, genotype.id=NULL, treatment=NULL, min.data.points=10,
									proxy.trait="volume.fluo.iap", col.plant="PlantID", col.treatment="Treatment", 
									col.genotype="Genotype", col.day="DAS", col.time="Realtime", 
									xlab=col.day, ylab=proxy.trait, completeness.check=TRUE, plot=TRUE, echo=TRUE){
	column.check(input, col.plant, col.genotype, col.day, col.time, proxy.trait)
	msg.plot <- function(id=NULL, level="plant"){
		if(echo){
			cat("*********** Control plant growth modeling:\nNo data for ", id, " *************\n")
		}
		plot(1, type="n", xaxt="n", yaxt="n", frame=TRUE, xlab="", ylab="", 
				main=paste(level, "=", id, ", treatment=", treatment, sep=""))
		text(1, paste("No data for modeling\n(", tolower(level), " \"", id, "\")", sep=""), cex=2, col=2)
	}
	id <- NULL
	level <- "Plant"
	if(!is.nothing(plant.id)){
		id <- plant.id
		if(is.nothing(treatment)){
			modeling.data <- input[input[, col.plant]==plant.id, c(col.plant, col.genotype, col.day, col.time, proxy.trait)]
		}else{
			modeling.data <- input[input[, col.plant]==plant.id & input[, col.treatment]==treatment, 
								c(col.plant, col.genotype, col.day, col.time, proxy.trait)]
		}
	}else if(!is.nothing(genotype.id)){
		id <- genotype.id
		level <- "Genotype"
		if(is.nothing(treatment)){
			modeling.data <- input[input[, col.genotype]==genotype.id, c(col.plant, col.genotype, col.day, col.time, proxy.trait)]
		}else{
			modeling.data <- input[input[, col.genotype]==genotype.id & input[, col.treatment]==treatment, 
								c(col.plant, col.genotype, col.day, col.time, proxy.trait)]
		}
	}else{
		cat("You should specify a plant.id or a genotype.id\n")
		return(NULL)
	}
	## checking data
	colnames(modeling.data) <- c(col.plant, col.genotype, col.day, "Time", "Trait")
	modeling.data <- modeling.data[order(modeling.data[, col.plant], modeling.data[, 'Time']), ]
	data.y <- modeling.data[, "Trait"]
	data.y[data.y==0] <- NA
	completed <- TRUE
	if(is.null(modeling.data) | nrow(modeling.data) < 1 | ncol(modeling.data) < 1 | sum(!is.na(data.y)) < 1){
		if(plot) {msg.plot(id, level=level)}
		completed <- FALSE
		invisible(NULL)
	}
	if(sum(!is.na(data.y)) < min.data.points){
		completed <- FALSE
	}
	absmax <- max(abs(input[, proxy.trait]), na.rm=TRUE)
	data.y <- data.y / absmax ## scale the data to [0, 1]
	data.time <- modeling.data[, "Time"]
	iplants <- unique(as.character(modeling.data[, col.plant]))
	genotypes <- unique(as.character(modeling.data[, col.genotype]))
	pred.time <- seq(min(modeling.data[, col.day]), ceiling(max(modeling.data[, col.day])*1.2))
	
	if(completeness.check){ ## check if the data points cover most of the time scale
		days <- unique(modeling.data[, col.day])
		first.check.point <- days[ceiling(length(days)/4)]
		last.check.point <- days[ceiling(length(days)*3/4)]
		compl.time <- na.omit(modeling.data)[, "Time"]
		if(all(compl.time > first.check.point, na.rm=TRUE) | 
			all(compl.time < last.check.point, na.rm=TRUE)){
			completed <- FALSE
		}
	}
	results <- NULL
	if(completed){
		# mlinear <- linear.model(data.y, data.time, pred.time)
		mexponential <- exponential.model(data.y, data.time, pred.time)
		mmonomolecular <- monomolecular.model(data.y, data.time, pred.time) # , kmax=mlogistic$kmax
		mlogistic <- logistic.model(data.y, data.time, pred.time)
		mgompetz <- gompetz.model(data.y, data.time, pred.time) # , kmax=mlogistic$kmax
		mweibull <- weibull.model(data.y, data.time, pred.time)
		results <- list(Plant=paste(iplants, collapse="|"), Genotype=paste(genotypes, sep="|"), base=absmax, 
					#Inflection=mlogistic$inflection, Kmax=mlogistic$kmax*absmax, IntrinsicGrowth=mlogistic$r, 
					#InflectionGR=mlogistic$inflection.growth.rate*absmax, InflectionRGR=mlogistic$inflection.relative.growth.rate, 
					#InflectionGrowth=mlogistic$inflection.growth*absmax, Prediction=cbind(Time=pred.time, Pred=mlogistic$prediction*absmax),
					exponential=mexponential, monomolecular=mmonomolecular,
					gompetz=mgompetz, weibull=mweibull, logistic=mlogistic)
		yrange <- range(data.y, mweibull$kmax*1.1, na.rm=TRUE) * absmax
	}else{
		yrange <- range(data.y, na.rm=TRUE) * absmax
	}
	if(plot){
		xrange <- range(0, pred.time, na.rm=TRUE)
		cols <- rainbow(length(iplants))
		if(length(iplants) == 1) cols[1] <- "#005a32"
		ltys <- 1:length(iplants)
		pchs <- c(16, 9:15, 17:25, 0:8)
		pchs <- pchs[{ind<-1:length(iplants)%%26;ind[ind==0]<-26;ind}]
		names(cols) <- names(ltys) <- names(pchs) <- iplants
		plot(xrange, yrange, type="n", xlab=col.day, ylab=proxy.trait, 
				main=paste(level, "=", id, ", treatment=", treatment, sep=""))
		for (plant in iplants) {
			lines(modeling.data[modeling.data[, col.plant]==plant, "Time"], 
					modeling.data[modeling.data[, col.plant]==plant, "Trait"], 
					type="b", lty=ltys[plant], col=cols[plant], pch=pchs[plant])
		}
		if(length(iplants) > 1){
			my.legend("bottomleft", legend=iplants, cex=0.8, col=cols, text.col=cols, 
					pch=pchs, lty=ltys, title=paste("Plants of \"", id, "\"", sep=""))
		}
		if(completed){
			# lines(pred.time, mlinear$prediction*absmax, lwd=1.5, lty=mlinear$lty, col=mlinear$color)
			lines(pred.time, mexponential$prediction*absmax, lwd=1.5, lty=mexponential$lty, col=mexponential$color)
			lines(pred.time, mmonomolecular$prediction*absmax, lwd=1.5, lty=mmonomolecular$lty, col=mmonomolecular$color)
			lines(pred.time, mgompetz$prediction*absmax, lwd=1.5, lty=mgompetz$lty, col=mgompetz$color)
			lines(pred.time, mlogistic$prediction*absmax, lwd=1.5, lty=mlogistic$lty, col=mlogistic$color)
			lines(pred.time, mweibull$prediction*absmax, lwd=1.5, lty=mweibull$lty, col=mweibull$color)
			
			abline(v=mweibull$inflection, h=mweibull$kmax*absmax, lty=mweibull$lty, col=mweibull$color)
			kmax.exp <- inflection.exp <- vector('expression',1)
			inflection.exp[1] <- substitute(expression(italic(T)[inflection] == II), 
							list(II=format(mweibull$inflection, digits=4)))[2]
			kmax.exp[1] <- substitute(expression(italic(K)[max] == KK), 
							list(KK=format(mweibull$kmax*absmax, digits=4, scientific=TRUE)))[2]
			text(mweibull$inflection, yrange[1], labels=inflection.exp, pos=4)
			text(xrange[2]*0.8, mweibull$kmax*absmax, labels=kmax.exp, pos=3)
			
			ltys <- c(mexponential$lty, mmonomolecular$lty, mgompetz$lty, mlogistic$lty, mweibull$lty)
			cols <- c(mexponential$color, mmonomolecular$color, mgompetz$color, mlogistic$color, mweibull$color)
			lgds <- vector('expression',5)
			# lgds[1] <- mlinear$legend
			lgds[1] <- mexponential$legend
			lgds[2] <- mmonomolecular$legend
			lgds[3] <- mgompetz$legend
			lgds[4] <- mlogistic$legend
			lgds[5] <- mweibull$legend
			my.legend("topleft", legend=lgds, cex=0.8, col=cols, text.col=cols, lty=ltys, title="Models")
		}else{
			usrs <- par('usr')
			text(mean(xrange), mean(yrange), 
				paste("No enough data for modeling\n(", tolower(level), " \"", id, "\")", sep=""), cex=2, col=2)
		}
	}
	invisible(results)
}

## growth modelling for stressed iplants
stressed.plant.growth.modeling <- function(input, plant.id=NULL, genotype.id=NULL, treatment='stress', 
									first.stress.day=NA, last.stress.day=NA, included.days.before.stress=5, min.data.points=10,
									proxy.trait="volume.fluo.iap", col.plant="PlantID", col.treatment="Treatment", 
									col.genotype="Genotype", col.day="DAS", col.time="Realtime",
									xlab=col.day, ylab=proxy.trait, completeness.check=TRUE, plot=TRUE, echo=TRUE){
	column.check(input, col.plant, col.genotype, col.day, col.time, proxy.trait)
	msg.plot <- function(id=NULL, level="plant"){
		if(echo){
			cat("*********** Stressed plant growth modeling:\nNo data for ", id, " *************\n")
		}
		plot(1, type="n", xaxt="n", yaxt="n", frame=TRUE, xlab="", ylab="", main=id)
		text(1, paste("No data for modeling\n", tolower(level), " \"", id, "\"", sep=""), cex=2, col=2)
	}
	if(!is.numeric(first.stress.day) | !is.numeric(last.stress.day)){
		stop("For stressed plant growth modeling, you should specify correct stress period
				\ni.e., parameters 'first.stress.day' and 'last.stress.day'\n")
	}
	
	id <- NULL
	level <- "Plant"
	if(!is.nothing(plant.id)){
		id <- plant.id
		if(is.nothing(treatment)){
			modeling.data <- input[input[, col.plant]==plant.id, c(col.plant, col.genotype, col.day, col.time, proxy.trait)]
		}else{
			modeling.data <- input[input[, col.plant]==plant.id & input[, col.treatment]==treatment, 
								c(col.plant, col.genotype, col.day, col.time, proxy.trait)]
		}
	}else if(!is.nothing(genotype.id)){
		id <- genotype.id
		level <- "Genotype"
		if(is.nothing(treatment)){
			modeling.data <- input[input[, col.genotype]==genotype.id, c(col.plant, col.genotype, col.day, col.time, proxy.trait)]
		}else{
			modeling.data <- input[input[, col.genotype]==genotype.id & input[, col.treatment]==treatment, 
								c(col.plant, col.genotype, col.day, col.time, proxy.trait)]
		}
	}else{
		cat("You should specify a plant.id or a genotype.id\n")
		invisible(NULL)
	}
	## checking data
	colnames(modeling.data) <- c(col.plant, col.genotype, col.day, "Time", "Trait")
	modeling.data <- modeling.data[order(modeling.data[, col.plant], modeling.data[, 'Time']), ]
	
	data.y <- modeling.data[, "Trait"]
	data.y[data.y==0] <- NA
	completed <- TRUE
	if(is.null(modeling.data) | nrow(modeling.data) < 1 | ncol(modeling.data) < 1 | sum(!is.na(data.y)) < 1){
		if(plot) {msg.plot(id, level=level)}
		completed <- FALSE
		return(NULL)
	}
	
	if(sum(!is.na(data.y)) < min.data.points){
		completed <- FALSE
	}
	
	data.time <- modeling.data[, "Time"]
	iplants <- unique(as.character(modeling.data[, col.plant]))
	genotypes <- unique(as.character(modeling.data[, col.genotype]))
	pred.time <- seq(min(modeling.data[, col.day]), ceiling(max(modeling.data[, col.day])*1.05))
	
	phase1 <- which(data.time >= first.stress.day-included.days.before.stress & data.time <= last.stress.day)
	phase2 <- which(data.time > last.stress.day)
	data.time1 <- data.time[phase1]
	data.time2 <- data.time[phase2]
	data.y1 <- data.y[phase1]
	data.y2 <- data.y[phase2]
	pred.time1 <- pred.time[pred.time <= last.stress.day]
	pred.time2 <- pred.time[pred.time > last.stress.day]
	tmax.obs <- data.time1[which.max(data.y1)]
	
	if(completeness.check){ ## check if the data fit bell-shape
		if(all(data.time1 >= tmax.obs, na.rm=TRUE) | 
			all(data.time1 <= tmax.obs, na.rm=TRUE)){
			completed <- FALSE
		}
		if(is.nothing(data.time2) || length(data.time2) < 4){
			completed <- FALSE
		}
	}
	
	results <- NULL
	if(completed){
		mquadratic <- quadratic.model(data.y1, data.time1, pred.time)
		mbellshape1 <- bellshape1.model(data.y1, data.time1, pred.time)
		mbellshape2 <- bellshape2.model(data.y1, data.time1, pred.time)
		mbellshape3 <- bellshape3.model(data.y1, data.time1, pred.time)
		mlinear <- linear.model(data.y2, data.time2, pred.time2)
		
		results <- list(Plant=paste(iplants, collapse="|"), Genotype=paste(genotypes, collapse="|"), stress.tmax.obs=tmax.obs, 
			quadratic=mquadratic, bellshape1=mbellshape1, bellshape2=mbellshape2, 
			bellshape3=mbellshape3, linear=mlinear)
		
		yrange <- range(data.y, mlinear$prediction*1.1, na.rm=TRUE)
	}else{
		yrange <- range(data.y, na.rm=TRUE)
	}
	if(plot){
		xrange <- range(0, pred.time, na.rm=TRUE)
		
		cols <- rainbow(length(iplants))
		if(length(iplants) == 1) cols[1] <- "#005a32"
		ltys <- 1:length(iplants)
		pchs <- c(16, 9:15, 17:25, 0:8)
		pchs <- pchs[{ind<-1:length(iplants)%%26;ind[ind==0]<-26;ind}]
		names(cols) <- names(ltys) <- names(pchs) <- iplants
		plot(xrange, yrange, type="n", xlab=col.day, ylab=proxy.trait, 
				main=paste(level, "=", id, ", treatment=", treatment, sep=""))
		for (plant in iplants) {
			lines(modeling.data[modeling.data[, col.plant]==plant, "Time"], 
					modeling.data[modeling.data[, col.plant]==plant, "Trait"], 
					type="b", lty=ltys[plant], col=cols[plant], pch=pchs[plant])
		}
		if(length(iplants) > 1){
			my.legend("bottomleft", legend=iplants, cex=0.8, col=cols, text.col=cols, 
					pch=pchs, lty=ltys, title=paste("Plants of \"", id, "\"", sep=""))
		}
		abline(v=c(first.stress.day, last.stress.day), lty=5, col='lightgrey')
		
		if(completed){
			lines(pred.time, mquadratic$prediction, lwd=1.5, lty=mquadratic$lty, col=mquadratic$color)
			lines(pred.time, mbellshape1$prediction, lwd=1.5, lty=mbellshape1$lty, col=mbellshape1$color)
			lines(pred.time, mbellshape2$prediction, lwd=1.5, lty=mbellshape2$lty, col=mbellshape2$color)
			lines(pred.time, mbellshape3$prediction, lwd=1.5, lty=mbellshape3$lty, col=mbellshape3$color)
			lines(pred.time2, mlinear$prediction, lwd=1.5, lty=mlinear$lty, col=mlinear$color)
			inflection.exp <- vector('expression',3)
			tmax.pred <- mbellshape3$predict.fun(mbellshape3$model$t.max)
			IPs <- c(mbellshape3$model$IP1, mbellshape3$model$IP2, mbellshape3$model$t.max)
			inflection.exp[1] <- substitute(expression(italic(T)[before] == II), 
							list(II=format(IPs[1], digits=4)))[2]
			inflection.exp[2] <- substitute(expression(italic(T)[after] == II), 
							list(II=format(IPs[2], digits=4)))[2]
			inflection.exp[3] <- substitute(expression(italic(T)[max] == II), 
							list(II=format(IPs[3], digits=4)))[2]
			tmax.exp <- vector('expression',1)
			tmax.exp[1] <- substitute(expression(Biomass~at~italic(t)[max] == AA), 
							list(AA=format(tmax.pred, digits=4, scientific=TRUE)))[2]
			text(mbellshape3$model$t.max, tmax.pred, labels=tmax.exp, pos=3)
			# text(IPs, rep(yrange[1], 3), labels=inflection.exp, pos=c(2,4,3))
			text(IPs[3], yrange[1], labels=inflection.exp[3], pos=3)
			abline(v=IPs, h=tmax.pred, lty=mbellshape3$lty, col=mbellshape3$color)
			####
			# abline(v=tmax.obs, lty=mbellshape3$lty, col='red3')
			# col.alpha <- col2rgb(mbellshape3$color)/255
			# col.alpha <- rgb(col.alpha[1], col.alpha[2], col.alpha[3], 0.5)
			# rect(tmax.obs, 0, mbellshape3$model$t.max, tmax.pred, col=col.alpha, border=mbellshape3$color)
			
			ltys <- c(mquadratic$lty, mbellshape1$lty, mbellshape2$lty, mbellshape3$lty, mlinear$lty)
			cols <- c(mquadratic$color, mbellshape1$color, mbellshape2$color, mbellshape3$color, mlinear$color)
			lgds <- vector('expression',5)
			lgds[1] <- mquadratic$legend
			lgds[2] <- mbellshape1$legend
			lgds[3] <- mbellshape2$legend
			lgds[4] <- mbellshape3$legend
			lgds[5] <- mlinear$legend
			my.legend("topleft", legend=lgds, cex=0.8, col=cols, text.col=cols, lty=ltys, title="Models")
		}else{
			usrs <- par('usr')
			text(mean(xrange), mean(yrange), 
				paste("Not suitable for modeling\n(", tolower(level), " \"", id, "\")", sep=""), cex=2, col=2)
		}
	}
	invisible(results)
}

## plot the result of trait reproducibility
trait.reproducibility.plot <- function(input, treatment, cutoff.p=0.001, cutoff.cor=0.8, main=treatment, ...){
	column.check(input, treatment)
	input <- na.omit(input[[treatment]]$summary)
	trait.cla <- unlist(lapply(rownames(input), trait.class.map)) ## trait class
	input <- input[order(trait.cla), ]
	trait.cor <- input[, "median.cor"] ## correlation
	trait.cla <- unlist(lapply(rownames(input), trait.class.map)) ## again, trait class
	trait.col <- unlist(lapply(rownames(input), trait.color.map)) ## trait color
	flag <- input[, "p.value"] < cutoff.p & input[, "median.cor"] > cutoff.cor
	itraits <- rownames(input)[flag]
	
	plot(trait.cor, col=trait.col, cex=trait.cor+0.5, pch=c(1, 19)[flag+1], ylim=c(0, 1), 
		xlab="Traits", ylab="Median of pearson correlation", main=main)
	abline(h=cutoff.cor, lwd=1.5, lty=2)
	
	trait.stat <- table(trait.cla)
	median.cor <- aggregate(trait.cor, list(Cat=trait.cla), median, na.rm=TRUE)
	rownames(median.cor) <- median.cor[, 'Cat']
	lgd <- paste(names(trait.stat), " (", round(median.cor[names(trait.stat), 'x'], 3), ", N=", trait.stat, ")", sep="")
	lgd.col <- trait.class.color[names(trait.stat)]
	my.legend("bottomleft", legend=lgd, col=lgd.col, text.col=lgd.col, lty=1, ## title.col="black", cex=0.8, 
			title=paste("Traits", " (", round(median(trait.cor), 3), ", N=", sum(trait.stat), ")", sep=""))
	num.traits <- aggregate(flag, list(Cat=trait.cla), sum, na.rm=TRUE)
	rownames(num.traits) <- num.traits[, 'Cat']
	lgd <- paste(rownames(num.traits), ": ", num.traits[, 'x'], sep="")
	lgd.col <- trait.class.color[rownames(num.traits)]
	my.legend("bottomright", legend=lgd, col=lgd.col, text.col=lgd.col, title=paste("Filtered: ", sum(flag)))
	#output <- cbind(input, trait.col, flag)
	#write.table(output, paste("Table 2 - ", treatment, ".trait.summary.out", sep=""), sep="\t", dec=".")
	invisible(itraits)
}


## stepwise VIF function
vif.wrapper <- function(input, thresh=10, trace=TRUE){
	require(fmsb)
	if(class(input) != 'data.frame') input <- data.frame(input)
	## some colnames may contain blank, use new colnames instead
	colnms <- names(input)
	names(input) <- paste("V", 1:ncol(input), sep="")
	names(colnms) <- names(input)
	## get initial vif value for all comparisons of variables
	vif.init <- NULL
	for(val in names(input)){
		form.in <- formula(paste(val, ' ~ .'))
		vif.init <- rbind(vif.init, c(val, VIF(lm(form.in, data=input))))
	}
	vif.max <- max(as.numeric(vif.init[, 2]))
	 
	if(vif.max < thresh){
		if(trace==TRUE){ #print output of each iteration
			prmatrix(vif.init, collab=c('var', 'vif'), rowlab=rep('', nrow(vif.init)), quote=FALSE)
			cat('\n')
			cat(paste('All variables have VIF < ', thresh, ', max VIF ', round(vif.max, 2), sep=''), '\n\n')
		}
		return(colnms[names(input)])
	}else{
		in.data <- input
		#backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
		while(vif.max >= thresh){
			vif.vals <- NULL
			for(val in names(in.data)){
				form.in <- formula(paste(val, ' ~ .'))
				vif.add <- VIF(lm(form.in, data=in.data))
				vif.vals <- rbind(vif.vals, c(val, vif.add))
			}
			max_row <- which(vif.vals[, 2] == max(as.numeric(vif.vals[, 2])))[1]
			vif.max <- as.numeric(vif.vals[max_row, 2])
			if(vif.max < thresh) break
			if(trace == T){ #print output of each iteration
				prmatrix(vif.vals, collab=c('var', 'vif'), rowlab=rep('', nrow(vif.vals)), quote=FALSE)
				cat('\n')
				cat('removed: ', vif.vals[max_row, 1], vif.max, '\n\n')
				flush.console()
			}
			in.data <- in.data[, !names(in.data) %in% vif.vals[max_row, 1]]
		}
		return(colnms[names(in.data)])
	}
}

## for the function of pairs 
panel.cor <- function(x, y, digits=4, prefix="", cex.cor, ...){
	usr <- par("usr"); on.exit(par(usr))
	r <- cor(x, y, use="pairwise.complete.obs")
	par(usr=c(0, 1, 0, 1))
	txt <- ifelse(r >= 0, 
				sprintf(paste0("%.", digits, "f"), r), ## format(c(r, 0.123456789), digits=digits)[1]
				txt <- sprintf(paste0("%.", digits-1, "f"), r)
			)
	txt <- paste(prefix, txt, sep="")
	if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt, cex=cex.cor * 0.9) ## r
}

## 
my.legend <- function(..., inset=0.01, pch=19, box.col=NA, title.col='black', bg="#EFEFEF"){
		legend(..., inset=inset, pch=pch, box.col=box.col, bg=bg, title.col=title.col)
}

### pie chart with percentage
my.pie <- function (x, labels=names(x), edges=200, radius=0.8, clockwise=FALSE, 
					init.angle=if (clockwise) 90 else 0, density=NULL, angle=45, 
					col=NULL, border=NULL, lty=NULL, main=NULL, percentage=TRUE, 
					rawNumber=FALSE, digits=3, cutoff=0.01, legend=FALSE, 
					legendpos="topright", legendcol=2, ...) {
	if (!is.numeric(x) || any(is.na(x) | x < 0)) 
		stop("'x' values must be positive.")
	if (is.null(labels)) 
		labels <- as.character(seq_along(x))
	else labels <- as.graphicsAnnot(labels)
	
	rawX <- x
	x <- c(0, cumsum(x)/sum(x))
	dx <- diff(x)
	nx <- length(dx)
	plot.new()
	pin <- par("pin")
	xlim <- ylim <- c(-1, 1)
	if (pin[1L] > pin[2L]) 
		xlim <- (pin[1L]/pin[2L]) * xlim
	else ylim <- (pin[2L]/pin[1L]) * ylim
	
	dev.hold()
	on.exit(dev.flush())
	plot.window(xlim, ylim, "", asp=1)
	if (is.null(col)) 
		col <- if (is.null(density)) 
			c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk", "pink")
		else par("fg")
	if (!is.null(col)) 
		col <- rep_len(col, nx)
	if (!is.null(border)) 
		border <- rep_len(border, nx)
	if (!is.null(lty)) 
		lty <- rep_len(lty, nx)
	
	angle <- rep(angle, nx)
	if (!is.null(density)) 
		density <- rep_len(density, nx)
	twopi <- if (clockwise) -2 * pi else 2 * pi
	t2xy <- function(t) {
		t2p <- twopi * t + init.angle * pi/180
		list(x=radius * cos(t2p), y=radius * sin(t2p))
	}
	for (i in 1L:nx) {
		n <- max(2, floor(edges * dx[i]))
		P <- t2xy(seq.int(x[i], x[i + 1], length.out=n))
		polygon(c(P$x, 0), c(P$y, 0), density=density[i], angle=angle[i], 
		border=border[i], col=col[i], lty=lty[i])
		if(!legend){
			P <- t2xy(mean(x[i + 0:1]))
			lab <- as.character(labels[i])
			if (!is.na(lab) && nzchar(lab)) {
				lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
				text(1.1 * P$x, 1.1 * P$y, labels[i], xpd=TRUE, 
				adj=ifelse(P$x < 0, 1, 0), ...)
			}
		}
	}
	if (percentage) {
		for (i in 1L:nx){
			if(dx[i]>cutoff){
			P <- t2xy(mean(x[i + 0:1]))
			text(.8 * P$x, .8 * P$y, paste(formatC(dx[i]*100, digits=digits), "%", sep=""), 
				xpd=TRUE, adj=.5, ...)
			}
		}
	}else{
		if(rawNumber){
			for (i in 1L:nx){
				if(dx[i]>cutoff){
					P <- t2xy(mean(x[i + 0:1]))
					text(.8 * P$x, .8 * P$y, rawX[i], xpd=TRUE, adj=.5, ...)
				}
			}
		}
	}
	if(legend) legend(legendpos, legend=labels, fill=col, border="black", bty="n", ncol=legendcol)
	
	title(main=main, ...)
	invisible(NULL)
}


### pie chart with percentage
my.pie <- function (x, labels=names(x), edges=200, radius=0.8, clockwise=FALSE, 
					init.angle=if (clockwise) 90 else 0, density=NULL, angle=45, 
					col=NULL, border=NULL, lty=NULL, main=NULL, percentage=TRUE, 
					rawNumber=FALSE, digits=3, cutoff=0.01, legend=FALSE, 
					legendpos="topright", legendcol=2, ...) {
	if (!is.numeric(x) || any(is.na(x) | x < 0)) 
		stop("'x' values must be positive.")
	if (is.null(labels)) 
		labels <- as.character(seq_along(x))
	else labels <- as.graphicsAnnot(labels)
	
	rawX <- x
	x <- c(0, cumsum(x)/sum(x))
	dx <- diff(x)
	nx <- length(dx)
	plot.new()
	pin <- par("pin")
	xlim <- ylim <- c(-1, 1)
	if (pin[1L] > pin[2L]) 
		xlim <- (pin[1L]/pin[2L]) * xlim
	else ylim <- (pin[2L]/pin[1L]) * ylim
	
	dev.hold()
	on.exit(dev.flush())
	plot.window(xlim, ylim, "", asp=1)
	if (is.null(col)) 
		col <- if (is.null(density)) 
			c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk", "pink")
		else par("fg")
	if (!is.null(col)) 
		col <- rep_len(col, nx)
	if (!is.null(border)) 
		border <- rep_len(border, nx)
	if (!is.null(lty)) 
		lty <- rep_len(lty, nx)
	
	angle <- rep(angle, nx)
	if (!is.null(density)) 
		density <- rep_len(density, nx)
	twopi <- if (clockwise) -2 * pi else 2 * pi
	t2xy <- function(t) {
		t2p <- twopi * t + init.angle * pi/180
		list(x=radius * cos(t2p), y=radius * sin(t2p))
	}
	for (i in 1L:nx) {
		n <- max(2, floor(edges * dx[i]))
		P <- t2xy(seq.int(x[i], x[i + 1], length.out=n))
		polygon(c(P$x, 0), c(P$y, 0), density=density[i], angle=angle[i], 
		border=border[i], col=col[i], lty=lty[i])
		if(!legend){
			P <- t2xy(mean(x[i + 0:1]))
			lab <- as.character(labels[i])
			if (!is.na(lab) && nzchar(lab)) {
				lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
				text(1.1 * P$x, 1.1 * P$y, labels[i], xpd=TRUE, 
				adj=ifelse(P$x < 0, 1, 0), ...)
			}
		}
	}
	if (percentage) {
		for (i in 1L:nx){
			if(dx[i]>cutoff){
			P <- t2xy(mean(x[i + 0:1]))
			text(.8 * P$x, .8 * P$y, paste(formatC(dx[i]*100, digits=digits), "%", sep=""), 
				xpd=TRUE, adj=.5, ...)
			}
		}
	}else{
		if(rawNumber){
			for (i in 1L:nx){
				if(dx[i]>cutoff){
					P <- t2xy(mean(x[i + 0:1]))
					text(.8 * P$x, .8 * P$y, rawX[i], xpd=TRUE, adj=.5, ...)
				}
			}
		}
	}
	if(legend) legend(legendpos, legend=labels, fill=col, border="black", bty="n", ncol=legendcol)
	
	title(main=main, ...)
	invisible(NULL)
}

############# trait classification #############
color.map <- list(FLUO="#F8B62D", NIR="#727171", Color="#006934", Geometric="#3778AD")
trait.class.color <- unlist(color.map)

trait.color.map <- function(trait) {
	tcol <- "#3778AD"
	if (length(grep(".fluo",trait))>0) tcol <- color.map$FLUO
	if (length(grep(".nir",trait))>0) tcol <- color.map$NIR
	if (length(grep(".vis",trait))>0 || length(grep("ndvi..relative.",trait))>0) tcol <- color.map$Color
	if (length(grep("volume|area|height|length|width|bloom|hull|filled|compactness|leaf|..px.3.",trait))>0) tcol <- color.map$Geometric
	return(tcol)
}

trait.class.map <- function(trait) {
	tcol <- "Geometric"
	if (length(grep(".fluo",trait))>0) tcol <- "FLUO"
	if (length(grep(".nir",trait))>0) tcol <- "NIR"
	if (length(grep(".vis",trait))>0 || length(grep("ndvi..relative.",trait))>0) tcol <- "Color"
	if (length(grep("volume|area|height|length|width|bloom|hull|filled|compactness|leaf|..px.3.",trait))>0) tcol <- "Geometric"
	return(tcol)
}

trait.view.map <- function(trait) {
	tcol <- "Both"
	if (length(grep("top.",trait))>0) tcol <- "Top"
	if (length(grep("side.",trait))>0) tcol <- "Side"
	return(tcol)
}

trait.camera.map <- function(trait) {
	tcol <- "VIS"
	if (length(grep("fluo.",trait))>0) tcol <- "FLUO"
	if (length(grep("nir.",trait))>0) tcol <- "NIR"
	if (length(grep("bloom",trait))>0) tcol <- "VIS,FLUO"
	return(tcol)
}
