################################################
# Author: Dijun Chen (chend@ipk-gatersleben.de)
# Update on Sept 2, 2013
################################################
cat("loading pca.plot.R \n")

### plot PCA results
pca.outlier.scale <- function(dat, alpha=0.01, fold=1.2){
	if(alpha > 1 | alpha < 0){alpha <- 0.01}
	if(alpha > 0.5){alpha <- 1 - alpha}
	lim <- quantile(dat, probs=c(alpha, 1-alpha)) * fold
	dat[which(dat < lim[1])] <- lim[1]
	dat[which(dat > lim[2])] <- lim[2]
	return(dat)
}

## pca biplot
pca.biplot <- function(pca.scores, pca.loadings, R2, pcs, sp=17, lp=NA, sc="blue", lc="red", 
				label=FALSE, id.label=FALSE, ...){ ## label for loading label
	panel <- function(x, y, i, j, ext.loadings=NULL, ext.scores=NULL, lc='red', ## col='blue', 
				loading.label=FALSE, score.label=FALSE, cex=0.6, ...) {
		abline(h=0, v=0, lty=2)
		if(!missing(ext.loadings)){
			segments(0, 0, ext.loadings[, i], ext.loadings[, j], col=lc, lwd=1)
			if(loading.label){
				text(ext.loadings[, i], ext.loadings[, j], rownames(pca.loadings), 
					adj=c(0.5, 0), col=lc, cex=cex)
			}
			if(! any(is.na(lp))){
				points(ext.loadings[, i], ext.loadings[, j], col=lc, pch=lp)
			}
		}
		suppressWarnings(points(x, y, cex=cex, ...))
		if(score.label && !missing(ext.scores)){
			text(ext.scores[, i], ext.scores[, j], rownames(pca.scores), 
				adj=c(0.5, 0), col=col, cex=cex)
		}
		
	}
	pc.labels <- paste("PC", pcs, "\n(", round(R2[pcs]*100, 2), "%)", sep="")
	for(index in 1:ncol(pca.scores)){
		pca.scores[, index] <- pca.outlier.scale(pca.scores[, index])
	}
	pca.scores <- apply(pca.scores[, pcs], 2, function(x)(x / max(abs(x))))
	pca.loadings <- apply(pca.loadings[, pcs], 2, function(x)(x / max(abs(x))))
	suppressWarnings(my.pairs(pca.scores, labels=pc.labels, xlim=c(-1, 1), ylim=c(-1, 1), 
			ext.loadings=pca.loadings, ext.scores=pca.scores, 
			lower.panel=panel, upper.panel=NULL, pch=sp, bg=sc, col=sc, lp=lp, lc=lc, 
			loading.label=label, score.label=id.label, ...))
}

pca.triplot <- function(pca.scores, pca.loadings, R2, pcs, sp=17, lp=1, sc="blue", lc="red", 
				label=FALSE, id.label=FALSE, number=FALSE, main=NULL, ...){ ## label for loading label
	require(klaR)
	if(ncol(pca.scores) < 3 || ncol(pca.loadings) < 3 || length(R2) < 3){
		stop('The dimension of scores, loadings and R2 should be more than 2.')
	}
	for(index in 1:ncol(pca.scores)){
		pca.scores[, index] <- pca.outlier.scale(pca.scores[, index])
	}
	rangesc <- range(pca.scores[, pcs])
	rangeld <- range(pca.loadings[, pcs])
	pca.scores <- (pca.scores[, pcs] - rangesc[1])/((rangesc[2]-rangesc[1]) * 1.2)
	pca.loadings <- (pca.loadings[, pcs] - rangeld[1])/((rangeld[2]-rangeld[1]) * 1.2)
	# pca.scores <- apply(pca.scores[, pcs], 2, function(x)((x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))
	# pca.loadings <- apply(pca.loadings[, pcs], 2, function(x)((x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))
	npca.scores <- nrow(pca.scores)
	npca.loadings <- nrow(pca.loadings)
	point1 <- cbind(pca.scores[, 1], pca.scores[, 2], pca.scores[, 3])
	point2 <- cbind(pca.loadings[, 1], pca.loadings[, 2], pca.loadings[, 3])
	point3 <- cbind(c(0.6, 0.6, 0), c(0, 0.6, 0.6), c(0.6, 0, 0.6))
	cps <- paste("PC", pcs, "\n(", round(R2[pcs]*100, 2), "%)", sep="")
	triplot(grid=TRUE, label="", center=FALSE, frame=TRUE)
	if(!missing(main)){
		title(main=main)
	}
	trilines(centerlines(3), lty=2)
	
	tripoints(point2, pch=lp, col=lc, cex=0.5)
	if(id.label){
		if (number == TRUE) 
			text(tritrafo(point1), as.character(1:npca.scores), adj=c(0.5, 0), col="blue", cex=0.6)
		if (number == FALSE) 
			text(tritrafo(point1), rownames(pca.scores), adj=c(0.5, 0), col="blue", cex=0.6)
	}
	if(label){
		text(tritrafo(point2), rownames(pca.loadings), adj=c(0.5, 0), col="red", cex=0.6)
	}
	text(tritrafo(point3), cps, adj=c(0.5, 0), cex=1)
	for (i in 1:npca.loadings) {
		ci <- i%%length(lc)
		if(ci == 0){
			ci=length(lc)
		}
		trilines(c(point2[i, 1], 1/3), c(point2[i, 2], 1/3), c(point2[i, 3], 1/3), lwd=0.75, col=lc[ci], lty=1)
	}
	tripoints(point1, pch=sp, col=sc, bg=sc, cex=1)
}

my.pca.plot <- function(object, type='biplot', ...){
	if('pcaRes' %in% class(object)){
		require(pcaMethods)
		if(type == 'triplot'){
			if(object@nPcs >= 3){
				if(object@nPcs >= 6){
					#pca.triplot(scores(object), loadings(object), object@R2, 1:3, ...)
					#pca.triplot(scores(object), loadings(object), object@R2, 4:6, ...)
					pca.triplot(object@scores, object@loadings, object@R2, 1:3, ...)
					pca.triplot(object@scores, object@loadings, object@R2, 4:6, ...)
				}else if (object@nPcs >= 4){
					pca.triplot(object@scores, object@loadings, object@R2, 1:3, ...)
					pca.triplot(object@scores, object@loadings, object@R2, (object@nPcs-2):object@nPcs, ...)
				}else{
					pca.triplot(object@scores, object@loadings, object@R2, 1:3, ...)
				}
			}else{
				stop('No enough PCs to show.\n*** PCs should be > 3.')
			}
		}else{
			pca.biplot(object@scores, object@loadings, object@R2, 1:object@nPcs, ...)
		}
	}else if(any(class(object) == 'dudi')){
		require(ade4)
		if(type == 'triplot'){
			if(object$nf >= 3){
				if(object$nf >= 6){
					pca.triplot(object$li, object$co, object$eig/sum(object$eig), 1:3, ...)
					pca.triplot(object$li, object$co, object$eig/sum(object$eig), 4:6, ...)
				}else if (object$nf >= 4){
					pca.triplot(object$li, object$co, object$eig/sum(object$eig), 1:3, ...)
					pca.triplot(object$li, object$co, object$eig/sum(object$eig), (object$nf-2):object$nf, ...)
				}else{
					pca.triplot(object$li, object$co, object$eig/sum(object$eig), 1:3, ...)
				}
			}else{
				stop('No enough PCs to show.\n*** PCs should be > 3.')
			}
		}else{
			pca.biplot(object$li, object$co, object$eig/sum(object$eig), 1:object$nf, ...)
		}
	}else{
		plot(object, ...)
	}
}

bubble.plot <- function(mat, pos.col='#FF8A00', neg.col='#00A0E9', label=FALSE, 
		col.gradient=TRUE, legend=FALSE, inches=FALSE, fg="gray", xlab="", ylab="", ...){
	require(reshape2)
	ifelse(is.null(colnames(mat)), {xlabs <- 1:ncol(mat)}, {xlabs <- colnames(mat)})
	ifelse(is.null(rownames(mat)), {ylabs <- 1:nrow(mat)}, {ylabs <- rownames(mat)})
	na.ind <- is.na(mat)
	mat[is.na(mat)] <- 0
	bg.cols <- rep(NA, length(mat))
	if(col.gradient){
		pos.cols <- colorRampPalette(c('white', '#FF8A00'))(sum(mat > 0))
		neg.cols <- colorRampPalette(c('#00A0E9', 'white'))(sum(mat < 0))
		mat.ords <- order(mat)
		bg.cols[tail(mat.ords, sum(mat > 0))] <- pos.cols
		bg.cols[head(mat.ords, sum(mat < 0))] <- neg.cols
	}else{
		bg.cols[which(mat > 0)] <- '#FF8A00'
		bg.cols[which(mat < 0)] <- '#00A0E9'
	}
	mat[na.ind] <- NA
	long.mat <- melt(abs(mat))
	colnames(long.mat) <- c('Ylab', 'Xlab', 'Val')
	with(long.mat, {
		if(legend){
			layout(matrix(c(1, 2), ncol=2), widths=c(9, 1))
			par(mar=c(5, 6, 4, 1))
		}else{
			par(mar=c(5, 6, 4, 2))
		}
		symbols(ordered(Xlab, levels=xlabs), ordered(Ylab, levels=ylabs), panel.first=grid(), circles=sqrt(Val/pi), 
				inches=inches, xaxt="n", yaxt="n", bg=bg.cols, fg=fg, xlab=xlab, ylab=ylab, ...)
		axis(1, 1:length(xlabs), xlabs, las=1)
		axis(2, 1:length(ylabs), ylabs, las=2)
		if(legend){
			par(mar=c(7, 0.5, 7, 2))
			min.raw <- min(mat, na.rm=TRUE)
			max.raw <- max(mat, na.rm=TRUE)
			cols <- c(neg.cols, pos.cols)
			z <- seq(min.raw, max.raw, length=length(cols))
			image(z=matrix(z, nrow=1), col=cols, xaxt="n", yaxt="n", frame=FALSE)
			mtext(side=1, 'Low')
			mtext(side=3, 'High')
			## axis(4, at=c(0, 1), labels=c('Low', 'High'), las=2)
		}
		if(label){
			val.pos <- which(mat!=0, arr.ind=TRUE)
			val.lab <- round(mat[mat!=0], digits=2)
			text(val.pos[, "col"], val.pos[, "row"], labels=val.lab)
		}
	})
}


my.pairs <- function(x, ...) UseMethod("my.pairs")
my.pairs.formula <- function(formula, data=NULL, ..., subset, na.action=stats::na.pass){
    m <- match.call(expand.dots=FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action # force in even if default
    m[[1L]] <- quote(stats::model.frame)
    require(stats, quietly=TRUE)
    mf <- eval(m, parent.frame())
    my.pairs(mf, ...)
}

#################################################
## some of the changes are from code
## Copyright (C) 1999 Dr. Jens Oehlschlaegel-Akiyoshi
## Others are by BDR and MM
## This version distributed under GPL (version 2 or later)
#################################################
my.pairs.default <- function (x, labels, panel=points, ..., 
          lower.panel=panel, upper.panel=panel, 
          diag.panel=NULL, text.panel=textPanel, 
          label.pos=0.5 + has.diag/3, line.main=3, 
          cex.labels=NULL, font.labels=1, 
          row1attop=TRUE, gap=1, log="") {
    if(doText <- missing(text.panel) || is.function(text.panel))
	textPanel <- 
	    function(x=0.5, y=0.5, txt, cex, font)
		text(x, y, txt, cex=cex, font=font)

    localAxis <- function(side, x, y, xpd, bg, col=NULL, main, oma, ...) {
      ## Explicitly ignore any color argument passed in as
      ## it was most likely meant for the data points and
      ## not for the axis.
        xpd <- NA
        if(side %% 2L == 1L && xl[j]) xpd <- FALSE
        if(side %% 2L == 0L && yl[i]) xpd <- FALSE
        if(side %% 2L == 1L) Axis(x, side=side, xpd=xpd, ...)
        else Axis(y, side=side, xpd=xpd, ...)
    }

    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localLowerPanel <- function(..., main, oma, font.main, cex.main)
        suppressWarnings(lower.panel(...))
    localUpperPanel <- function(..., main, oma, font.main, cex.main)
        suppressWarnings(upper.panel(...))

    localDiagPanel <- function(..., main, oma, font.main, cex.main)
        suppressWarnings(diag.panel(...))

    dots <- list(...); nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for(i in seq_along(names(x))) {
            if(is.factor(x[[i]]) || is.logical(x[[i]]))
               x[[i]] <- as.numeric(x[[i]])
            if(!is.numeric(unclass(x[[i]])))
                stop("non-numeric argument to 'my.pairs'")
        }
    } else if (!is.numeric(x)) stop("non-numeric argument to 'my.pairs'")
    panel <- match.fun(panel)
    if((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
        lower.panel <- match.fun(lower.panel)
    if((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
        upper.panel <- match.fun(upper.panel)
    if((has.diag  <- !is.null(diag.panel)) && !missing(diag.panel))
        diag.panel <- match.fun(diag.panel)

    if(row1attop) {
        tmp <- lower.panel; lower.panel <- upper.panel; upper.panel <- tmp
        tmp <- has.lower; has.lower <- has.upper; has.upper <- tmp
    }

    nc <- ncol(x)
    if (nc < 2) stop("only one column in the argument to 'my.pairs'")
    if(doText) {
	if (missing(labels)) {
	    labels <- colnames(x)
	    if (is.null(labels)) labels <- paste("var", 1L:nc)
	}
	else if(is.null(labels)) doText <- FALSE
    }
    oma <- if("oma" %in% nmdots) dots$oma
    main <- if("main" %in% nmdots) dots$main
    if (is.null(oma))
	oma <- c(4, 4, if(!is.null(main)) 6 else 4, 4)
    opar <- par(mfrow=c(nc, nc), mar=rep.int(gap/2, 4), oma=oma)
    on.exit(par(opar))
    dev.hold(); on.exit(dev.flush(), add=TRUE)

    xl <- yl <- logical(nc)
    if (is.numeric(log)) xl[log] <- yl[log] <- TRUE
    else {xl[] <- grepl("x", log); yl[] <- grepl("y", log)}
    for (i in if(row1attop) 1L:nc else nc:1L)
        for (j in 1L:nc) {
            l <- paste0(ifelse(xl[j], "x", ""), ifelse(yl[i], "y", ""))
            localPlot(x[, j], x[, i], xlab="", ylab="", 
                      axes=FALSE, type="n", ..., log=l)
            if(i == j || (i < j && has.lower) || (i > j && has.upper) ) {
                box()
                if(i == 1  && (!(j %% 2L) || !has.upper || !has.lower ))
                    localAxis(1L + 2L*row1attop, x[, j], x[, i], ...)
                if(i == nc && (  j %% 2L  || !has.upper || !has.lower ))
                    localAxis(3L - 2L*row1attop, x[, j], x[, i], ...)
                if(j == 1  && (!(i %% 2L) || !has.upper || !has.lower ))
                    localAxis(2L, x[, j], x[, i], ...)
                if(j == nc && (  i %% 2L  || !has.upper || !has.lower ))
                    localAxis(4L, x[, j], x[, i], ...)
                mfg <- par("mfg")
                if(i == j) {
                    if (has.diag) localDiagPanel(as.vector(x[, i]), ...)
		    if (doText) {
                        par(usr=c(0, 1, 0, 1))
                        if(is.null(cex.labels)) {
                            l.wid <- strwidth(labels, "user")
                            cex.labels <- max(0.8, min(2, .9 / max(l.wid)))
                        }
                        xlp <- if(xl[i]) 10^0.5 else 0.5
                        ylp <- if(yl[j]) 10^label.pos else label.pos
                        text.panel(xlp, ylp, labels[i], 
                                   cex=cex.labels, font=font.labels)
                    }
                } else if(i < j)
                    localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), j, i, ...)
                else
                    localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), j, i, ...)
                if (any(par("mfg") != mfg))
                    stop("the 'panel' function made a new plot")
            } else par(new=FALSE)
        }
    if (!is.null(main)) {
        font.main <- if("font.main" %in% nmdots) dots$font.main else par("font.main")
        cex.main <- if("cex.main" %in% nmdots) dots$cex.main else par("cex.main")
        mtext(main, 3, line.main, outer=TRUE, at=0.5, cex=cex.main, font=font.main)
    }
    invisible(NULL)
}
