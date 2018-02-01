################################################
# Author: Dijun Chen (chend@ipk-gatersleben.de)
# Update on June 2, 2014
################################################
cat("loading models.R \n")

require(hydroGOF) ## loading the function of RMSE

## Compute the squared relative error
rmsre <- function(observed, predicted){
	sqrt(mean(((observed - predicted)/observed)^2, na.rm=TRUE))
}

########### various regression models ###########
## http://www.kaggle.com/c/overfitting/forums/t/456/modelling-algorithms-in-r/2770#post2770
## regression with generalized boosted regression models (GBM)
gbm.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	require(gbm)
	## build a gbm model, shrinkage=0.01
	n.minobsinnode <- floor((nrow(train.x) * 0.5 - 1) / 4) - 1
	cat("***********", n.minobsinnode, ">>>\n")
	if(n.minobsinnode > 10) {
		n.minobsinnode <- 10
		cat("***********", n.minobsinnode, "<<<\n")
	}else{
		n.minobsinnode <- 8
		cat("***********", n.minobsinnode, "<<<\n")
	}
	gbm.model <- gbm(y~., data=cbind(y=train.y, train.x), cv.folds=2, n.minobsinnode=n.minobsinnode,
				n.trees=1000, distribution='gaussian', verbose=FALSE) 
	result <- list(model=gbm.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	## Computing the relative importance / influence of each variable
	weight <- summary(gbm.model, plotit=FALSE)[colnames(train.x), "rel.inf"]
	names(weight) <- colnames(train.x)
	result$weight <- weight
	if(evaluation){
		ntrees <- gbm.perf(gbm.model, method="cv", plot.it=FALSE)
		## prediction based on train data
		pred.sum <- prediction.summary(gbm.model, train.x, train.y, n.trees=ntrees)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
		
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		test.y <- predict(gbm.model, newdata=test.x)
		result$test.y <- test.y
	}
	return(result)
}

## regression with multivariate adaptive regression splines (MARS)
mars.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	require(earth)
	## build a mars model
	mars.model <- earth(train.x, train.y)
	result <- list(model=mars.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	## relative importance
	## weight <- evimp(mars.model, trim=FALSE)[, "nsubsets"]
	relimp <- evimp(mars.model, trim=FALSE)
	weight <- relimp[,'nsubsets']/length(mars.model$selected.terms)
	names(weight) <- gsub("-unused", "", rownames(relimp))
	result$weight <- weight[colnames(train.x)]
	result$relimp_metrics <- "Fraction of model subsets incl. the variable"
	if(evaluation){
		## prediction based on train data
		pred.sum <- prediction.summary(mars.model, train.x, train.y)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		test.y <- predict(mars.model, newdata=test.x)
		result$test.y <- test.y
	}
	return(result)
}

## neural networks for regression
nnet.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	require(nnet)
	## build a nnet model
	train.x <- apply(train.x, 2, scale)
	nnet.model <- nnet(y~., data=cbind(y=train.y, train.x), size=ceiling(ncol(train.x)/2), linout=TRUE, trace=FALSE)
	result <- list(model=nnet.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	## relative importance
	weight <- gar.fun('y', nnet.model)$rel.imp
	result$weight <- weight
	if(evaluation){
		## prediction based on train data
		pred.sum <- prediction.summary(nnet.model, train.x, train.y)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		test.x <- apply(test.x, 2, scale)
		test.y <- predict(nnet.model, newdata=test.x)
		result$test.y <- test.y
	}
	return(result)
}

## SVM (Support Vector Machines) for regression
svm.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	require(e1071)
	## build a svm model
	## train.x <- apply(train.x, 2, scale)
	svm.model <- svm(y~., data=cbind(y=train.y, train.x), probability=TRUE, scale=TRUE, cost=100, ...) ##, cross=5
	result <- list(model=svm.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	## relative importance
	relimp <- t(svm.model$SV) %*% svm.model$coefs
	weight <- relimp[, 1]
	weight <- weight * 100 / max(abs(weight)) ## normalization
	result$weight <- weight
	if(evaluation){
		## prediction based on train data
		pred.sum <- prediction.summary(svm.model, train.x, train.y)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		## test.x <- apply(test.x, 2, scale)
		test.y <- predict(svm.model, newdata=test.x)
		result$test.y <- test.y
	}
	return(result)
}

## RPART: Recursive Partitioning and Regression Trees 
rpart.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	require(rpart)
	## build a rpart model
	rpart.model <- rpart(y~., data=cbind(y=train.y, train.x))
	result <- list(model=rpart.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	## relative importance
	weight <- rep(0, ncol(train.x))
	names(weight) <- colnames(train.x)
	relimp <- rpart.model$variable.importance
	weight[names(relimp)] <- relimp
	weight <- weight * 100 / sum(weight)
	result$weight <- weight
	if(evaluation){
		## prediction based on train data
		pred.sum <- prediction.summary(rpart.model, train.x, train.y)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		test.y <- predict(rpart.model, newdata=test.x)
		result$test.y <- test.y
	}
	return(result)
}

## random forests for regression
rf.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	require(randomForest)
	## build a random forest model
	rf.model <- randomForest(train.x, train.y, importance=TRUE)
	result <- list(model=rf.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	## relative importance
	relimp <- importance(rf.model, type=1)
	weight <- relimp[,1]
	names(weight) <- rownames(relimp)
	result$weight <- weight
	result$relimp_metrics <- "Mean decrease in accuracy (%IncMSE)"
	if(evaluation){
		## prediction based on train data
		pred.sum <- prediction.summary(rf.model, train.x, train.y)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		test.y <- predict(rf.model, newdata=test.x)
		result$test.y <- test.y
	}
	return(result)
}

## calculating the relative feature importance for MLR
relweights2 <- function(fit, plot=FALSE, ...){
	R <- cor(fit$model)
	nvar <- ncol(R)
	rxx <- R[2:nvar, 2:nvar]
	rxy <- R[2:nvar, 1]
	svd <- eigen(rxx)
	evec <- svd$vectors
	ev <- svd$values
	delta <- diag(sqrt(ev))
	lambda <- evec %*% delta %*% t(evec)
	lambdasq <- lambda ^ 2
	beta <- solve(lambda) %*% rxy
	rsquare <- colSums(beta ^ 2)
	rawwgt <- lambdasq %*% beta ^ 2
	import <- (rawwgt / rsquare) * 100
	lbls <- names(fit$model[2:nvar])
	rownames(import) <- lbls
	colnames(import) <- "Weights"
	if(plot){
		r2cv <- shrinkage(fit, k=10)
		barx <- barplot(t(import), names.arg=lbls, xaxt="n", 
			ylab="% of R-Square", xlab="Predictor variables", 
			sub=paste0("Original R-Square=", round(rsquare, digits=3), 
			'\nFold Cross-Validated R-square=', round(r2cv$r2cv, 3)), ...)
		axis(1, at=barx, labels=lbls, las=2, cex=0.8)
	}
	return(import)
}

relweights <- function(fit){
	require(caret) 
	import <- varImp(fit)
	colnames(import) <- "Weights"
	return(import)
}

## multivariate linear regression
ml.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	train <- data.frame(y=train.y, train.x)
	formula <- paste('y~', paste(colnames(train.x), collapse='+'))
	## build a multivariate linear model
	lm.model <- lm(formula, data=train)
	result <- list(model=lm.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	## relative importance
	relimport <- relweights(lm.model)
	weight <- relimport[,1]
	names(weight) <- rownames(relimport)
	result$weight <- weight
	result$relimp_metrics <- "Relative contribution to R^2"
	if(evaluation){
		## prediction based on train data
		pred.sum <- prediction.summary(lm.model, train.x, train.y)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		test.y <- predict(lm.model, newdata=test.x)
		result$test.y <- test.y
	}
	return(result)
}

## random generalized linear model
rglm.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	require(randomGLM)
	train.x <- apply(train.x, 2, scale)
	## build a random generalized linear model
	rglm.model <- randomGLM(train.x, train.y, classify=FALSE, nCandidateCovariates=ceiling(ncol(train.x)/2), nBags=50, keepModels=TRUE)
	result <- list(model=rglm.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	## relative importance
	weight <- as.vector(rglm.model$timesSelectedByForwardRegression)
	weight <- weight*100 / sum(weight, na.rm=TRUE) ## normalization 
	names(weight) <- colnames(train.x)
	result$weight <- weight
	if(evaluation){
		## prediction based on train data
		pred.sum <- prediction.summary(rglm.model, train.x, train.y)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		test.x <- apply(test.x, 2, scale)
		test.y <- predict(rglm.model, newdata=test.x)
		result$test.y <- test.y
	}
	return(result)
}

## Penalized regression model, Bridge regression
bridge.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, alpha=0.5, ...){
	require(glmnet)
	train.x <- as.matrix(train.x)
	## build a penalized regression model
	bridge.model <- glmnet(train.x, train.y, family="gaussian", alpha=alpha)
	result <- list(model=bridge.model, predicted=NULL, r=NULL, p.value=NULL, weight=NULL, relimp_metrics="Relative importance")
	s <- median(bridge.model$lambda) ## Median value of the penalty parameter lambda
	## relative importance
	#weight <- as.vector(coef(bridge.model, s=s))[-1]
	#names(weight) <- colnames(train.x)
	#result$weight <- weight
	if(evaluation){
		## prediction based on train data
		pred.sum <- prediction.summary(bridge.model, train.x, train.y, s=s)
		result$r <- pred.sum$r
		result$p.value <- pred.sum$p.value
		result$predicted <- pred.sum$predicted
	}
	## predicting based on the testing set (if provided)
	if(!is.null(test.x)){
		test.x <- as.matrix(test.x)
		test.y <- predict(bridge.model, newx=test.x, s=s)
		result$test.y <- test.y
	}
	return(result)
}

## Penalized regression model: ridge regression
ridge.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	bridge.regression(train.x, train.y, test.x, evaluation, alpha=0, ...)
}

## Penalized regression model: elastic net
elasticnet.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	bridge.regression(train.x, train.y, test.x, evaluation, alpha=0.5, ...)
}

## Penalized regression model: lasso
lasso.regression <- function(train.x, train.y, test.x=NULL, evaluation=TRUE, ...){
	bridge.regression(train.x, train.y, test.x, evaluation, alpha=1, ...)
}

## summary of prediction with a regression model
prediction.summary <- function(model, train.x, train.y, ...){
	if(any('elnet' %in% class(model)) || any('glmnet' %in% class(model))){
		predicted <- predict(model, newx=train.x, ...)
	}else{
		predicted <- predict(model, newdata=train.x, ...)
	}
	## test for association/correlation between observed and predicted values
	ctest <- cor.test(predicted, train.y)
	r <- ctest$estimate
	p.value <- ctest$p.value
	if(!is.na(p.value) && p.value == 0) p.value <- 2.2e-16
	p.value <- as.numeric(format(p.value, scientific=TRUE))
	list(predicted=predicted, r=r, p.value=p.value)
}

## criteria of evaluation
evaluation.criteria <- regression.criteria <- function(observed, predicted) {
	if(length(observed) < 2 | length(predicted) < 2){return(NULL)}
	if(length(observed) != length(predicted)){return(NULL)}
	ctest <- cor.test(predicted, observed)
	PCC <- ctest$estimate[['cor']]
	p.value <- ctest$p.value
	if(!is.na(p.value) && p.value == 0) p.value <- 2.2e-16
	p.value <- as.numeric(format(p.value, scientific=TRUE))
	R2 <- summary(lm(predicted ~ observed))$r.squared
	RMSRE <- rmsre(observed, predicted)
	mu <- mean((predicted - observed) / observed)
	result <- list(PCC=PCC, R2=R2, RMSRE=RMSRE, p.value=p.value, mu=mu)
	return(result)
}

## prediction performance
regression.metrics <- prediction.model <- function(model, testset) {
	testdata <- testset[,-1]
	if(is.null(dim(testdata))){
		testdata <- as.data.frame(testdata)
		colnames(testdata) <- colnames(testset)[-1]
	}
	if(any('elnet' %in% class(model)) || any('glmnet' %in% class(model))){
		s <- median(model$lambda) ## Median value of the penalty parameter lambda
		predicted <- predict(object=model, newx=testdata, s=s)
	}else{
		predicted <- predict(object=model, newdata=testdata)
	}
	observed <- testset[, 1]
	result <- append(list(obs=observed, pred=predicted), regression.criteria(observed, predicted))
	return(result)
}

## criteria of classification
classification.criteria <- function(observed, predicted) {
	if(length(observed) < 2 | length(predicted) < 2){return(NULL)}
	if(length(observed) != length(predicted)){return(NULL)}
	contab <- table(observed, predicted)
	acc <- sum(diag(contab)) / sum(contab)
	result <- list(confusion=contab, accuracy=acc)
	return(result)
}

## classification performance
classification.metrics <- function(model, testset) {
	require(pROC)
	testset[,1] <- factor(testset[,1])
	observed <- testset[, 1]
	predicted <- predict(object=model, testset) 
	if(length(levels(observed)) > 2 && "earth" %in% class(model$finalModel)){
		probs <- predict(object=model$finalModel, testset)
	}else{
		probs <- predict(object=model, testset, type='prob')
	}
	## Calculate custom one-vs-all stats for each class
	## ROC 
	roc_stats <- lapply(levels(observed), function(clazz){
		## Grab one-vs-all data for the class
		obs  <- factor(ifelse(observed == clazz, 1, 0))
		prob <- probs[,clazz]
		ROC <- roc(obs, prob) ## , percent=TRUE
		return(ROC) 
	})
	## PRC: precision-recall curve
	prc_stats <- NULL 
	PR <- FALSE 
	if(require(precrec)){ ## only available for R >= 3.2.1
		prc_stats <- lapply(levels(observed), function(clazz){
			## Grab one-vs-all data for the class
			obs  <- factor(ifelse(observed == clazz, 1, 0))
			prob <- probs[,clazz]
			sscurves <- evalmod(scores=prob, labels=obs)
			return(sscurves$prcs[[1]]) 
		})
		PR <- TRUE 
	}
	names(roc_stats) <- levels(observed)
	auc_stats <- median(unlist(lapply(roc_stats, function(x){x$auc+0})))
	multiclass <- ifelse(length(levels(observed)) > 2, T, F) 
	result <- append(list(obs=observed, pred=predicted, auc=auc_stats, roc=roc_stats, prc=prc_stats, mc=multiclass, pr=PR), classification.criteria(observed, predicted))
	return(result)
}

## input: Data = [Y, Xs] where Y is a vector containing the response values
## and Xs is a matrix containing the predictor (regressor) values
regression.cross.validation <- function(Data, nfold=10, model="svm.regression"){
	if(is.null(dim(Data)) || dim(Data)[2] < 1){return(NULL)}
	if(nfold < 2){nfold <- 2}
	
	n <- nrow(Data)
	accuracy <- c()
	modelfunc <- get(model)
	observed <- predicted <- relimp <- c()
	
	## n-fold CV (n>1): divide the dataset into n fold, for each fold(ith), train the n-1 folds (except ith) and then predict the ith fold, go on...
	# divide the dataset into n-fold, randomly
	ni <- trunc(n/nfold)  # number in ith group, where i=2..nfold
	n1 <- n - ni*(nfold-1) # number of 1st group
	index <- c(rep(1, n1), sapply(2:nfold, function(x) rep(x, ni)))  # not random
	groups <- split(sample(1:n), index)
	
	for(run in 1:nfold){
		sample.ids <- groups[[run]]
		testset <- Data[sample.ids, ]
		trainset <- Data[-sample.ids, ]
		traindata <- trainset[,-1]
		if(is.null(dim(traindata))){
			traindata <- as.data.frame(traindata)
			colnames(traindata) <- colnames(Data)[-1]
		}
		myTry <- try(modelfunc(traindata, trainset[,1], evaluation=FALSE), silent=TRUE)
		if (class(myTry) == "try-error") {
			cat("****** regression.cross.validation ******\n")
			cat("**** Errors in running regression model: \"", model, "\" [run =", run, "]\n")
			cat(myTry)
			cat("*****************************************\n")
			accuracy <- c(accuracy, NA)
		}else{
			train.model <- myTry
			if(!is.null(train.model$weight)){
				relimp <- cbind(relimp, train.model$weight)
			}
			predout <- regression.metrics(train.model$model, testset)
			accuracy <- predout
			observed <- c(observed, testset[, 1])
			predicted <- c(predicted, predout$pred)
		}
	}
	relimp <- apply(relimp, 1, mean)
	result <- append(list(relimp=relimp, observed=observed, predicted=predicted), regression.criteria(observed, predicted))
	return(result)
}

## TrainData = [Y, Xs]; TestData = [Y, Xs]
regression.cohort.validation <- function(TrainData, TestData, model="svm.regression"){
	observed <- TestData[, 1]
	modelfunc <- get(model)
	myTry <- try(modelfunc(TrainData[,-1], TrainData[,1], test.x=TestData[, -1], evaluation=FALSE), silent=TRUE)
	if (class(myTry) == "try-error") {
		cat("****** regression.cohort.validation ******\n")
		cat("**** Errors in running regression model: \"", model, "\"\n")
		cat(myTry)
		cat("******************************************\n")
		if(plot){
			plot(1, type="n", xaxt="n", yaxt="n", frame=TRUE, ...)
			text(1, "Error", cex=4, col=2)
		}
		return(NULL)
	}else{
		reg.model <- myTry
		predicted <- reg.model$test.y
		result <- append(list(observed=observed, predicted=predicted), regression.criteria(observed, predicted))
		return(result)
	}
}

## scatter plot of observed and predicted data
regression.plot <- function(result, mu=FALSE, pch=16, col='#08306b', xlab="Observed values", ylab="Predicted values", ...){
	observed <- result$observed
	predicted <- result$predicted
	cols <- colorRampPalette(c('#f0f0f0',col))(length(predicted))
	cols <- cols[rank(predicted)]
	# ylim <- xlim <- range(c(observed, predicted), na.rm=TRUE)
	plot(observed, predicted, col=cols, pch=pch, xlab=xlab, ylab=ylab, ...) # xlim=xlim, ylim=ylim, smoothScatter(...)
	mylm <- lm(predicted ~ observed)
	abline(mylm, lwd=2, col='red')
#	lgds <- vector('expression',2)
#	lgds[1] <- substitute(expression(paste("Pearson's ", italic(r)==PCC, " (", italic(p), "-value < ", Pval, ")", sep="")), list(PCC=format(result$PCC, dig=4), Pval=format(result$p.value, dig=3, scienticif=TRUE)))[2]
#	lgds[2] <- substitute(expression(paste("RMSRE = ", RMSRE, " (", italic(R)^2==R2, ")", sep="")), list(RMSRE=format(result$RMSRE, dig=3, scienticif=TRUE), R2=format(result$R2, dig=4)))[2]
#	legend("topleft", legend=lgds, inset=c(-0.025, 0.01), box.col=NA, bg=NA, pch=NA, title.col="black")
	
	if(mu){
		lgds <- vector('expression',5)
	}else{
		lgds <- vector('expression',4)
	}
	lgds[1] <- substitute(expression(paste("Pearson's ", italic(r)==PCC, sep="")), list(PCC=format(result$PCC, dig=4)))[2]
	lgds[2] <- substitute(expression(paste(italic(p), "-value < ", Pval, sep="")), list(Pval=format(result$p.value, dig=3, scienticif=TRUE)))[2]
	lgds[3] <- substitute(expression(paste("RMSRE = ", RMSRE, sep="")), list(RMSRE=format(result$RMSRE, dig=3, scienticif=TRUE)))[2]
	lgds[4] <- substitute(expression(italic(R)^2==R2), list(R2=format(result$R2, dig=4)))[2]
	if(mu) {lgds[5] <- substitute(expression(mu==MU), list(MU=format(result$mu, dig=3, scienticif=TRUE)))[2]}
	legend("topleft", legend=lgds[1:2], inset=c(-0.025, 0.01), box.col=NA, bg=NA, pch=NA, title.col="black")
	if(mu) {
		legend("bottomright", legend=lgds[3:5], inset=0.01, box.col=NA, bg=NA, pch=NA, title.col="black")
	}else{
		legend("bottomright", legend=lgds[3:4], inset=0.01, box.col=NA, bg=NA, pch=NA, title.col="black")
	}
}

## plot confusion matrix 
confusion.matrix.plot <- classification.plot <- function(result, col='#08306b', xlab="Observed classes", ylab="Predicted classes", ...){
    contab <- result$confusion 
    pltd <- contab * 100 / rowSums(contab)
    cols <- colorRampPalette(c('white',col))(nrow(pltd)*ncol(pltd))
    image(1:nrow(pltd), 1:ncol(pltd), pltd, col=cols, xaxt='n', yaxt='n', frame=F, xlab=xlab, ylab=ylab, ...)
    box() 
    grid(nx=nrow(pltd), ny=ncol(pltd))
    if(nrow(pltd) < 10){
        cf <- 0 
        pos <- which(pltd > cf, arr.ind=T)
        txt <- sprintf("%.1f%%", pltd[pltd > cf])
        cex.cor <- 0.8/strwidth(txt)
        text(pos, txt, cex=cex.cor)
    } 
    axis(1, at=1:nrow(pltd), labels=rownames(pltd), las=2) 
    axis(2, at=1:ncol(pltd), labels=colnames(pltd), las=2) 
}

## a list of regression models as defined above (OLD)
reg.models <- list(
	 MLR="ml.regression"
	,MARS="mars.regression"
	,RF="rf.regression"
	,SVR="svm.regression"
#	,NN="nnet.regression"
#	,GBM="gbm.regression"
#	,MM=NULL
#	,LASSO="lasso.regression"
#	,ENET="elasticnet.regression" 
#	,RIDGE="ridge.regression"
#	,RGLM="rglm.regression"
)

regression.models <- list(
	 `MLR: Multivariate Linear Regression`="lm" 
	,`MARS: Multivariate Adaptive Regression Spline`="earth"    ## earth
	,`RF: Random Forest`="rf"                                   ## randomForest
	,`SVM-Radial: Support Vector Machines with Radial Kernel`="svmRadial"   ## kernlab
	,`SVM-Linear: Support Vector Machines with Linear Kernel`="svmLinear2"  ## e1071
	,`KNN: k-Nearest Neighbors`="kknn"                          ## kknn
	,`GBM: Stochastic Gradient Boosting`="gbm"                  ## gbm,plyr 
	,`GLMNET: Lasso and Elastic-Net Regularized Generalized Linear Models`="glmnet" ## glmnet,Matrix
	,`RIDGE: Ridge Regression`="ridge"                          ## elasticnet
	,`LASSO: Lasso Model`="lasso"                               ## elasticnet
	,`BLASSO: Bayesian Lasso`="blasso"                          ## monomvn
	,`BRNN: Bayesian Regularized Neural Networks`="brnn"        ## brnn
	,`BGLM: Bayesian Generalized Linear Model`="bayesglm"       ## arm
	,`GP-Radial: Gaussian Process with Radial Kernel`="gaussprRadial"   ## kernlab
	,`GP-Poly: Gaussian Process with Polynomial Kernel`="gaussprPoly"   ## kernlab
	,`GLM: Generalized Linear Model`="glm" 
#	,`BART: Bayesian Additive Regression Trees`="bartMachine"           ## bartMachine
#	,`CART: `="rpart"
#	,`RGLM: `="randomGLM"
#	,`NN: `="nnet"
)

## a list of classification models 
classification.models <- list(
	 `NBC: Naive Bayes`="naive_bayes"               ## naivebayes 
	,`NNET: Neural Network`="nnet"                  ## nnet * 
	,`SVM-Radial: Support Vector Machines with Radial Kernel`="svmRadial"   ## kernlab 
	,`SVM-Linear: Support Vector Machines with Linear Kernel`="svmLinear2"  ## e1071 
	,`RF: Random Forest`="rf"                       ## randomForest * 
	,`KNN: k-Nearest Neighbors`="kknn"              ## kknn 
	,`LDA: Linear Discriminant Analysis`="lda"      ## MASS 
	,`RDA: Regularized Discriminant Analysis`="rda" ## klaR 
	,`GBM: Stochastic Gradient Boosting`="gbm"      ## gbm,plyr * 
	,`PLS: Partial Least Squares`="pls"             ## pls 
	,`GLMNET: Lasso and Elastic-Net Regularized Generalized Linear Models`="glmnet" ## glmnet,Matrix 
	,`CART: Classification And Regression Trees`="rpart"          ## rpart * 
	,`MARS: Multivariate Adaptive Regression Spline`="earth"    ## earth * 
	,`LLDA: Localized Linear Discriminant Analysis`="loclda"    ## klaR 
	,`MDA: Mixture Discriminant Analysis`="mda"     ## mda 
	,`PDA: Penalized Discriminant Analysis`="pda"   ## mda 
#	,`RGLM: Ensembles of Generalized Linear Models`="randomGLM" ## randomGLM * 
)
classification.models <- classification.models[order(names(classification.models))]
regression.models <- regression.models[order(names(regression.models))]

########### wrapper code based on the caret package 
# all possible models 
## getModelInfo(), names(getModelInfo())

## input: Data = [Y, Xs] where Y is a vector containing the response values
## and Xs is a matrix containing the predictor (regressor) values
regression.analysis <- function(Data, model="rf", nfold=10, ntime=1, preProc=c("center", "scale")){
    require(caret)
    if(is.null(dim(Data)) || ncol(Data) < 1){return(NULL)}
    if(nfold < 2){nfold <- 2}
    colnames(Data)[1] <- 'y' 
    
    ## n-fold CV (n>1): divide the dataset into n fold, for each fold(ith), train the n-1 folds (except ith) and then predict the ith fold, go on...
    # divide the dataset into n-fold, randomly
    n <- nrow(Data)
    ni <- trunc(n/nfold)  # number in ith group, where i=2..nfold
    n1 <- n - ni*(nfold-1) # number of 1st group
    index <- c(rep(1, n1), sapply(2:nfold, function(x) rep(x, ni)))  # not random
    groups <- split(sample(1:n), index)
    
    ctrl <- trainControl(method="repeatedcv", number=nfold, repeats=ntime)
    accuracy <- c()
    observed <- predicted <- relimp <- c()
    for(run in 1:nfold){
        sample.ids <- groups[[run]]
        testset <- Data[sample.ids, ]
        trainset <- Data[-sample.ids, ]
        
        if(model == "rpart"){
            train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl)
        ## }else if (model == "svmLinear2") {
        ##     train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl) ## NOT USE probability=TRUE in regression
        ## }else if (model == "svmRadial") {
        ##     train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl) ## NOT USE prob.model=TRUE in regression
        }else if (model == "rf") {
            train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl, verbose=FALSE, importance=TRUE)
        }else if (model == "gbm") {
            require(gbm)
            if(nrow(Data) / nfold < 100){
                ctrl <- trainControl(method="none")
            }
            train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl, verbose=FALSE)
        }else if (model == "bartMachine") {
            train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl, verbose=FALSE)
        }else if (model == "randomGLM") {
            train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl, verbose=FALSE, classify=FALSE, nCandidateCovariates=ceiling(ncol(trainset)/2), nBags=50, keepModels=TRUE)
        }else if (model == "nnet" || length(grep("svm", model))>0) { ## model == "earth" || 
            train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl, trace=FALSE)
        }else{
            train.model <- train(y ~ ., data=trainset, metric="RMSE", method=model, preProc=preProc, trControl=ctrl)
        }
        
        imp <- varImp(train.model)
        if("Overall" %in% names(imp$importance)){
            if(is.null(relimp)){
                relimp <- imp$importance[,"Overall"]
                names(relimp) <- rownames(imp$importance)
            }else{
                relimp <- cbind(relimp, imp$importance[rownames(relimp),"Overall"])
            }
        }else{
            
        }
        
        predout <- regression.metrics(train.model, testset)
        observed <- c(observed, testset[, 1])
        predicted <- c(predicted, predout$pred)
    }
    relimp <- apply(relimp, 1, mean)
    result <- append(list(relimp=relimp, observed=observed, predicted=predicted), regression.criteria(observed, predicted))
    return(result)
}

classification.analysis <- function(Data, model="rf", nfold=10, ntime=1, preProc=c("center", "scale")){
    require(caret)
    if(is.null(dim(Data)) || ncol(Data) < 1){return(NULL)}
    colnames(Data)[1] <- 'y' 
    Data[, 'y'] <- factor(Data[, 'y'])
    if(min(table(Data$y))/2 < nfold){
        nfold <- floor(min(table(Data$y))/2)
    }
    if(nfold < 2){nfold <- 2}
    
    for(x in levels(Data$y)){
        rows <- which(Data$y == x)
        n <- length(rows)
        ni <- trunc(n/nfold)
        n1 <- n - ni*(nfold-1)
        index <- c(rep(1, n1), sapply(2:nfold, function(x) rep(x, ni)))
        g <- sample(split(sample(1:n), index))
    }
    
    ## preserve the overall class distribution of the data 
    ## groups <- createDataPartition(Data$y, p=0.1, list=FALSE, times=10)
    gs <- lapply(levels(Data$y), function(x){
        rows <- which(Data$y == x)
        n <- length(rows)
        ni <- trunc(n/nfold)
        n1 <- n - ni*(nfold-1)
        index <- c(rep(1, n1), sapply(2:nfold, function(x) rep(x, ni)))
        g <- sample(split(rows, index))
    })
    groups <- list() 
    for(i in 1:nfold){
        groups[[i]] <- unlist(lapply(gs, function(x){
                            x[i]
                        }), use.names=F)
    }
    
    ctrl <- trainControl(method="repeatedcv", number=nfold, repeats=ntime)
    mauc <- macc <- 0 
    bmod <- NULL; ## mroc <- NULL 
    accuracy <- c()
    observed <- predicted <- relimp <- c(); ## aucs <- c() 
    for(run in 1:nfold){
        sample.ids <- groups[[run]]
        testset <- Data[sample.ids, ]
        trainset <- Data[-sample.ids, ]
        
        if(model == "rpart"){
            train.model <- train(y ~ ., data=trainset, metric="Accuracy", method=model, preProc=preProc, trControl=ctrl)
        }else if (model == "svmLinear2") {
            train.model <- train(y ~ ., data=trainset, metric="Accuracy", method=model, preProc=preProc, trControl=ctrl, probability=TRUE)
        }else if (model == "svmRadial") {
            train.model <- train(y ~ ., data=trainset, metric="Accuracy", method=model, preProc=preProc, trControl=ctrl, prob.model=TRUE)
        }else if (model == "rf") {
            train.model <- train(y ~ ., data=trainset, metric="Accuracy", method=model, preProc=preProc, trControl=ctrl, verbose=FALSE, importance=TRUE)
        }else if (model == "randomGLM") {
            train.model <- train(y ~ ., data=trainset, metric="Accuracy", method=model, preProc=preProc, trControl=ctrl, verbose=FALSE, classify=TRUE, nCandidateCovariates=ceiling(ncol(trainset)/2), nBags=50, keepModels=TRUE)
        }else if (model == "gbm") {
            require(gbm)
            if(nrow(Data) / nfold < 100){
                ctrl <- trainControl(method="none")
            }
            train.model <- train(y ~ ., data=trainset, metric="Accuracy", method=model, preProc=preProc, trControl=ctrl, verbose=FALSE)
        }else if (model == "earth" || model == "nnet" || length(grep("svm", model))>0) {
            train.model <- train(y ~ ., data=trainset, metric="Accuracy", method=model, preProc=preProc, trControl=ctrl, trace=FALSE)
        }else{
            train.model <- train(y ~ ., data=trainset, metric="Accuracy", method=model, preProc=preProc, trControl=ctrl)
        }
        
        imp <- varImp(train.model)
        if("Overall" %in% colnames(imp$importance)){
            if(is.null(relimp)){
                relimp <- imp$importance
            }else{
                relimp <- cbind(relimp, imp$importance[rownames(relimp), ])
            }
        }
        
        predout <- classification.metrics(train.model, testset)
        if(predout$accuracy > macc){
            macc <- predout$accuracy
            mroc <- predout$roc
            bmod <- train.model
        }
        ## aucs <- c(aucs, predout$auc)
    }
    overall <- F
    if(!is.null(relimp)){
        overall <- T 
        relimp <- apply(relimp, 1, mean)
    }else{
        imp <- varImp(bmod)
        relimp <- imp$importance 
    }
    ## prediction by the best model 
    predout <- classification.metrics(bmod, Data)
    observed <- predout$obs 
    predicted <- predout$pred 
    result <- append(list(observed=observed, predicted=predicted, auc=predout$auc, roc=predout$roc, prc=predout$prc, relimp=relimp, mc=predout$mc, pr=predout$pr, overall=overall), classification.criteria(observed, predicted))
    return(result)
}
