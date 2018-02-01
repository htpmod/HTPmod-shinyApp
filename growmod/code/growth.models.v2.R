################################################
# Author: Dijun Chen (chend@ipk-gatersleben.de)
# Update on June 2, 2014
################################################
cat("loading modeling.models.R \n")

## Compute the squared error
se <- function(observed, predicted){
	(observed - predicted)^2
}
## Compute the sum of squared error
sse <- function(observed, predicted){
	sum(se(observed, predicted), na.rm=TRUE)
}
## Compute the mean squared error
mse <- function(observed, predicted){
	mean(se(observed, predicted), na.rm=TRUE)
}
## Compute the root mean squared error
rmse <- function(observed, predicted){
	sqrt(mse(observed, predicted))
}
## Compute the squared relative error
rmsre <- function(observed, predicted){
	sqrt(mean(((observed - predicted)/observed)^2, na.rm=TRUE))
}
## Compute the R squared 
rsq <- function(observed, predicted){
	1 - sse(observed, predicted) / sum((observed - mean(observed, na.rm=TRUE))^2, na.rm=TRUE)
}

## In the Weibull, we suppose y0 = 0 
weibull.model <- function(data.y, data.time, pred.time=NULL, kmax=NULL, optimize.step=0.01, col="#E4007F", lty=6, ...){
	if(is.null(kmax)){
		kmax <- max(data.y, na.rm=TRUE) + 0.01
		##if(kmax<0.4) {kmax=0.4}
		myFunc <- function(data.y, kmax, data.time){
			kmaxn <- kmax
			data.time.ln <- log(data.time)
			mweibull <- lm(log(log(kmax/(kmax-data.y))) ~ data.time.ln)
			rold <- summary(mweibull)$adj.r.squared
			for (kmax1 in seq(kmax, 1.5, by=optimize.step)){
				mweibull <- lm(log(log(kmax1/(kmax1-data.y))) ~ data.time.ln)
				if (summary(mweibull)$adj.r.squared > rold){
					rold <- summary(mweibull)$adj.r.squared
					kmaxn <- kmax1
				}
			}
			kmax <- kmaxn
			mweibull <- lm(log(log(kmax/(kmax-data.y))) ~ data.time.ln)
			## summary(mweibull)
			return(list(kmax=kmax, model=mweibull))
		}
		
		myTry <- try(myFunc(data.y, kmax, data.time), silent=TRUE)
		if (class(myTry) == "try-error") {
			cat(paste("\t>>> Modeling error in [weibull.model]\n"))
			cat(myTry)
			return(NULL)
		}else{
			kmax <- myTry$kmax
			mweibull <- myTry$model
		}
	}
	
	predict.fun <- function(data.time){
		ppred <- exp(exp(predict(mweibull, data.frame(data.time.ln=log(data.time)))))
		result <- mweibull$kmax*(ppred-1) / ppred
		return(result)
	}
	
	agr.fun <- function(data.time){
		K <- mweibull$kmax
		m <- mweibull$m
		y0 <- mweibull$y0
		r <- mweibull$r
		r*m*(K-y0)*data.time^(m-1)*exp(-r*data.time^m)
	}
	
	rgr.fun <- function(data.time){
		K <- mweibull$kmax
		m <- mweibull$m
		y0 <- mweibull$y0
		r <- mweibull$r
		r*m*(K-y0)*data.time^(m-1)/(K*exp(r*data.time^m)-(K-y0))
	}
	
	# if(is.null(kmax)) kmax <- max(data.y, na.rm=TRUE) + 0.01
	if(is.null(pred.time)){pred.time <- data.time}
	
	data.time.ln <- log(data.time)
	
	mweibull <- lm(log(log(kmax/(kmax-data.y))) ~ data.time.ln)

	mweibull$kmax <- kmax
	mweibull$y0 <- y0 <- 0 ##############
	mweibull$r <- r <- exp(summary(mweibull)$coefficients[1])
	mweibull$m <- m <- summary(mweibull)$coefficients[2]
	mweibull$inflection <- inflection <- ((m-1)/r/m)^(1/m)
	
	# inflection.growth <- predict.fun(inflection) ## kmax-(kmax-y0)*exp((1-m)/m))
	
	mweibull$R2 <- R2 <- summary(mweibull)$adj.r.squared
	data.y.pred <- predict.fun(data.time)
	mweibull$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mweibull$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)
	
	prediction <- predict.fun(pred.time)
	legend <- substitute(expression(paste(italic(K-(K-y[0])*e^-rt^m), ", Weibull, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mweibull, para=list(RMSRE=RMSRE, R=R, R2=R2, r=r, kmax=kmax, inflection=inflection), 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Weibull")

}

logistic.model <- function(data.y, data.time, pred.time=NULL, kmax=NULL, optimize.step=0.01, col="#F39800", lty=5, ...){
	if(is.null(kmax)){
		kmax <- max(data.y, na.rm=TRUE) + 0.01
		##if(kmax<0.4) {kmax=0.4}
		myFunc <- function(data.y, kmax, data.time){
			kmaxn <- kmax
			mlogistic <- lm(log(data.y/(kmax-data.y)) ~ data.time)
			rold <- summary(mlogistic)$adj.r.squared
			for (kmax1 in seq(kmax, 1.5, by=optimize.step)){
				mlogistic <- lm(log(data.y/(kmax1-data.y)) ~ data.time)
				if (summary(mlogistic)$adj.r.squared > rold){
					rold <- summary(mlogistic)$adj.r.squared
					kmaxn <- kmax1
				}
			}
			kmax <- kmaxn
			mlogistic <- lm(log(data.y/(kmax-data.y)) ~ data.time)
			## summary(mlogistic)
			return(list(kmax=kmax, model=mlogistic))
		}
		myTry <- try(myFunc(data.y, kmax, data.time), silent=TRUE)
		if (class(myTry) == "try-error") {
			cat(paste("\t>>> Modeling error in [logistic.model]\n"))
			cat(myTry)
			return(NULL)
		}else{
			kmax <- myTry$kmax
			mlogistic <- myTry$model
		}
	}
	
	predict.fun <- function(data.time){
		result <- mlogistic$kmax/(1+exp(-predict(mlogistic, data.frame(data.time=data.time))))
		return(result)
	}
	
	agr.fun <- function(data.time){
		K <- mlogistic$kmax
		y0 <- mlogistic$y0
		r <- mlogistic$r
		r*y0*K*(K-y0)*exp(-r*data.time) / (y0+(K-y0)*exp(-r*data.time))^2
	}
	
	rgr.fun <- function(data.time){
		K <- mlogistic$kmax
		y0 <- mlogistic$y0
		r <- mlogistic$r
		r*K*(K-y0)*exp(-r*data.time) / (y0+(K-y0)*exp(-r*data.time))
	}
	
	if(is.null(pred.time)){pred.time <- data.time}
	
	mlogistic <- lm(log(data.y/(kmax-data.y)) ~ data.time)
	mlogistic$kmax <- kmax
	mlogistic$y0 <- y0 <- kmax * exp(summary(mlogistic)$coefficients[1]) / (1+exp(summary(mlogistic)$coefficients[1]))
	mlogistic$r <- r <- summary(mlogistic)$coefficients[2]
	mlogistic$inflection <- inflection <- abs(summary(mlogistic)$coefficients[1])/r
	
	# inflection.growth <- predict.fun(inflection)
	# inflection.growth.rate <- r*inflection.growth*(1-inflection.growth/kmax)
	# inflection.growth.rate <- agr.fun(mlogistic, inflection)
	# inflection.relative.growth.rate <- inflection.growth.rate / inflection.growth
	
	mlogistic$R2 <- R2 <- summary(mlogistic)$adj.r.squared
	data.y.pred <- predict.fun(data.time)
	mlogistic$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mlogistic$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)
	
	prediction <- predict.fun(pred.time)
	legend <- substitute(expression(paste(italic(K*y[0]/(y[0]+(K-y[0])*e^-rt)), 
				", Logistic, ", italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mlogistic, para=list(RMSRE=RMSRE, R=R, R2=R2, r=r, kmax=kmax, inflection=inflection), 
			# inflection.growth=inflection.growth, inflection.growth.rate=inflection.growth.rate,
			# inflection.relative.growth.rate=inflection.relative.growth.rate, 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Logistic")
}

gompetz.model <- function(data.y, data.time, pred.time=NULL, kmax=NULL, optimize.step=0.01, col="#7E318E", lty=4, ...){
	if(is.null(kmax)){
		kmax <- max(data.y, na.rm=TRUE) + 0.01
		##if(kmax<0.4) {kmax=0.4}
		myFunc <- function(data.y, kmax, data.time){
			kmaxn <- kmax
			mgompetz <- lm(-log(-log(data.y/kmax)) ~ data.time)
			rold <- summary(mgompetz)$adj.r.squared
			for (kmax1 in seq(kmax, 1.5, by=optimize.step)){
				mgompetz <- lm(-log(-log(data.y/kmax1)) ~ data.time)
				if (summary(mgompetz)$adj.r.squared > rold){
					rold <- summary(mgompetz)$adj.r.squared
					kmaxn <- kmax1
				}
			}
			kmax <- kmaxn
			mgompetz <- lm(-log(-log(data.y/kmax)) ~ data.time)
			## summary(mgompetz)
			return(list(kmax=kmax, model=mgompetz))
		}
		myTry <- try(myFunc(data.y, kmax, data.time), silent=TRUE)
		if (class(myTry) == "try-error") {
			cat(paste("\t>>> Modeling error in [gompetz.model]\n"))
			cat(myTry)
			return(NULL)
		}else{
			kmax <- myTry$kmax
			mgompetz <- myTry$model
		}
	}
	
	predict.fun <- function(data.time){
		result <- mgompetz$kmax*exp(-exp(-predict(mgompetz, data.frame(data.time=data.time))))
		return(result)
	}
	
	agr.fun <- function(data.time){
		K <- mgompetz$kmax
		y0 <- mgompetz$y0
		r <- mgompetz$r
		r*K*log(K/y0)*exp(-r*data.time)*(y0/K)^(exp(-r*data.time))
	}
	
	rgr.fun <- function(data.time){
		K <- mgompetz$kmax
		y0 <- mgompetz$y0
		r <- mgompetz$r
		r*log(K/y0)*exp(-r*data.time)
	}
	
	# if(is.null(kmax)) kmax <- max(data.y, na.rm=TRUE) + 0.01
	if(is.null(pred.time)){pred.time <- data.time}
	mgompetz <- lm(-log(-log(data.y/kmax)) ~ data.time)
	mgompetz$kmax <- kmax
	mgompetz$y0 <- y0 <- kmax * exp(-exp(-summary(mgompetz)$coefficients[1]))
	mgompetz$r <- r <- summary(mgompetz)$coefficients[2]
	mgompetz$inflection <- inflection <- log(log(kmax/y0))/r
	
	mgompetz$R2 <- R2 <- summary(mgompetz)$adj.r.squared
	data.y.pred <- predict.fun(data.time)
	mgompetz$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mgompetz$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)

	prediction <- predict.fun(pred.time) 
	legend <- substitute(expression(paste(italic(K*(y[0]/K)^e^-rt), ", Gompetz, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mgompetz, para=list(RMSRE=RMSRE, R=R, R2=R2, r=r, kmax=kmax, inflection=inflection),
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Gompetz")
}

monomolecular.model <- function(data.y, data.time, pred.time=NULL, kmax=NULL, optimize.step=0.01, col="#B28247", lty=3, ...){
	if(is.null(kmax)){
		kmax <- max(data.y, na.rm=TRUE) + 0.01
		##if(kmax<0.4) {kmax=0.4}
		myFunc <- function(data.y, kmax, data.time){
			kmaxn <- kmax
			mmonomolecular <- lm(log(1/(kmax-data.y)) ~ data.time)
			rold <- summary(mmonomolecular)$adj.r.squared
			for (kmax1 in seq(kmax, 1.5, by=optimize.step)){
				mmonomolecular <- lm(log(1/(kmax1-data.y)) ~ data.time)
				if (summary(mmonomolecular)$adj.r.squared > rold){
					rold <- summary(mmonomolecular)$adj.r.squared
					kmaxn <- kmax1
				}
			}
			kmax <- kmaxn
			mmonomolecular <- lm(log(1/(kmax-data.y)) ~ data.time)
			## summary(mmonomolecular)
			return(list(kmax=kmax, model=mmonomolecular))
		}
		myTry <- try(myFunc(data.y, kmax, data.time), silent=TRUE)
		if (class(myTry) == "try-error") {
			cat(paste("\t>>> Modeling error in [monomolecular.model]\n"))
			cat(myTry)
			return(NULL)
		}else{
			kmax <- myTry$kmax
			mmonomolecular <- myTry$model
		}
	}
	
	predict.fun <- function(data.time){
		result <- (mmonomolecular$kmax-exp(-predict(mmonomolecular, data.frame(data.time=data.time))))
		return(result)
	}
	
	agr.fun <- function(data.time){
		K <- mmonomolecular$kmax
		y0 <- mmonomolecular$y0
		r <- mmonomolecular$r
		r*(K-y0)*exp(-r*data.time)
	}
	
	rgr.fun <- function(data.time){
		K <- mmonomolecular$kmax
		y0 <- mmonomolecular$y0
		r <- mmonomolecular$r
		r*(K-y0)/(y0+K*(exp(r*data.time)-1))
	}
	
	## if(is.null(kmax)) kmax <- max(data.y, na.rm=TRUE) + 0.01
	if(is.null(pred.time)){pred.time <- data.time}
	mmonomolecular <- lm(log(1/(kmax-data.y)) ~ data.time)
	mmonomolecular$kmax <- kmax
	mmonomolecular$y0 <- y0 <- kmax-exp(-summary(mmonomolecular)$coefficients[1])
	mmonomolecular$r <- r <- summary(mmonomolecular)$coefficients[2]
	
	mmonomolecular$R2 <- R2 <- summary(mmonomolecular)$adj.r.squared
	data.y.pred <- predict.fun(data.time)
	mmonomolecular$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mmonomolecular$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)

	prediction <- predict.fun(pred.time)
	legend <- substitute(expression(paste(italic(K-(K-y[0])*e^-rt), ", Monomolecular, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mmonomolecular, para=list(RMSRE=RMSRE, R=R, R2=R2, r=r, kmax=kmax), 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Monomolecular")
}

exponential.model <- function(data.y, data.time, pred.time=NULL, optimize.step=0.01, col="#00A29A", lty=2, ...){
	predict.fun <- function(data.time){
		result <- exp(predict(mexponential, data.frame(data.time=data.time)))
		return(result)
	}
	
	agr.fun <- function(data.time){
		y0 <- mexponential$y0
		r <- mexponential$r
		r*y0*exp(r*data.time)
	}
	
	rgr.fun <- function(data.time){
		rep(mexponential$r, times=length(data.time))
	}
	
	if(is.null(pred.time)){pred.time <- data.time}
	mexponential <- lm(log(data.y) ~ data.time)
	mexponential$y0 <- y0 <- exp(summary(mexponential)$coefficients[1])
	mexponential$r <- r <- summary(mexponential)$coefficients[2]
	mexponential$R2 <- R2 <- summary(mexponential)$adj.r.squared
	data.y.pred <- predict.fun(data.time)
	mexponential$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mexponential$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)

	prediction <- predict.fun(pred.time)
	legend <- substitute(expression(paste(italic(y[0]*e^rt), ", Exponential, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mexponential, para=list(RMSRE=RMSRE, R=R, R2=R2), 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Exponential")
}

linear.model <- function(data.y, data.time, pred.time=NULL, col="#E4007F", lty=6, ...){
	predict.fun <- function(data.time){
		result <- predict(mlinear, data.frame(data.time=data.time))
		return(result)
	}
	
	agr.fun <- function(data.time){
		rep(mlinear$r, times=length(data.time))
	}
	
	rgr.fun <- function(data.time){
		y0 <- mlinear$y0
		r <- mlinear$r
		r/(y0+r*data.time)
	}
	
	if(is.null(pred.time)){pred.time <- data.time}
	mlinear <- lm(data.y ~ data.time)
	mlinear$y0 <- y0 <- summary(mlinear)$coefficients[1,1]
	mlinear$r <- r <- summary(mlinear)$coefficients[2,1]
	
	mlinear$R2 <- R2 <- summary(mlinear)$adj.r.squared
	data.y.pred <- predict(mlinear, data.frame(data.time=data.time))
	mlinear$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mlinear$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)
	
	prediction <- predict(mlinear, data.frame(data.time=pred.time))
	legend <- substitute(expression(paste(italic(y[0]+r*t), ", Linear, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mlinear, para=list(RMSRE=RMSRE, R=R, R2=R2, r=r), 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=2, name="Linear")
}

quadratic.model <- function(data.y, data.time, pred.time=NULL, col="#00A29A", lty=2, ...){
	predict.fun <- function(data.time){
		result <- predict(mquadratic, data.frame(data.time=data.time, data.time.sq=-data.time^2))
		return(result)
	}
	
	agr.fun <- function(data.time){
		mquadratic$b-2*mquadratic$a*data.time
	}
	
	rgr.fun <- function(data.time){
		(mquadratic$b-2*mquadratic$a*data.time)/(mquadratic$c+mquadratic$b*data.time-mquadratic$a*data.time^2)
	}
	
	if(is.null(pred.time)){pred.time <- data.time}
	data.time.sq <- -data.time^2
	mquadratic <- lm(data.y ~ data.time+data.time.sq)
	mquadratic$a <- a <- summary(mquadratic)$coefficients[3,1]
	mquadratic$b <- b <- summary(mquadratic)$coefficients[2,1]
	mquadratic$c <- c <- summary(mquadratic)$coefficients[1,1]
	mquadratic$t.max <- t.max <- abs(b/(2*a))
	
	mquadratic$R2 <- R2 <- summary(mquadratic)$adj.r.squared
	data.y.pred <- predict.fun(data.time)
	mquadratic$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mquadratic$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)

	prediction <- predict.fun(pred.time)
	legend <- substitute(expression(paste(italic(a*t+b*t^2), ", Quadratic, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mquadratic, para=list(RMSRE=RMSRE, R=R, R2=R2, t.max=t.max), 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Quadratic")
}

bellshape1.model <- function(data.y, data.time, pred.time=NULL, col="#B28247", lty=3, ...){
	predict.fun <- function(data.time){
		result <- exp(predict(mbellshape1, data.frame(data.time=(data.time-mbellshape1$t.max)^2)))
		return(result)
	}
	
	agr.fun <- function(data.time){
		tmax <- mbellshape1$t.max
		A <- mbellshape1$A
		a <- mbellshape1$a
		2*A*a*(data.time-tmax)*exp(a*(data.time-tmax)^2)
	}
	
	rgr.fun <- function(data.time){
		tmax <- mbellshape1$t.max
		a <- mbellshape1$a
		2*a*(data.time-tmax)
	}
	
	if(is.null(pred.time)){pred.time <- data.time}
	o.data.time <- data.time
	t.max <- data.time[which(data.y==max(data.y, na.rm=TRUE))][1] ###########
	data.time <- (data.time-t.max)^2
	mbellshape1 <- lm(log(data.y) ~ data.time)
	mbellshape1$A <- A <- exp(summary(mbellshape1)$coefficients[1,1])
	mbellshape1$a <- a <- summary(mbellshape1)$coefficients[2,1]
	
	mbellshape1$t.max <- t.max
	mbellshape1$R2 <- R2 <- summary(mbellshape1)$adj.r.squared
	data.y.pred <- predict.fun(o.data.time)
	mbellshape1$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mbellshape1$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)

	prediction <- predict.fun(pred.time)
	legend <- substitute(expression(paste(italic(A*e^{a*(t-{t[max]})^2}), ", Bell-shaped 1, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mbellshape1, para=list(RMSRE=RMSRE, R=R, R2=R2, t.max=t.max), 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Bell-shaped 1")
}

bellshape2.model <- function(data.y, data.time, pred.time=NULL, col="#7E318E", lty=4, ...){
	predict.fun <- function(data.time){
		result <- exp(predict(mbellshape2, data.frame(data.time=data.time, data.time.lg=log(data.time))))
		return(result)
	}
	
	agr.fun <- function(data.time){
		A <- mbellshape2$A
		a <- mbellshape2$a
		b <- mbellshape2$b
		A(b/data.time-a)*data.time^b*exp(-a*data.time)
	}
	
	rgr.fun <- function(data.time){
		A <- mbellshape2$A
		a <- mbellshape2$a
		b <- mbellshape2$b
		b/data.time-a
	}
	
	if(is.null(pred.time)){pred.time <- data.time}
	data.time.lg <- log(data.time)
	mbellshape2 <- lm(log(data.y) ~ data.time.lg+data.time)
	mbellshape2$A <- A <- exp(summary(mbellshape2)$coefficients[1,1])
	mbellshape2$b <- b <- summary(mbellshape2)$coefficients[2,1]
	mbellshape2$a <- a <- -summary(mbellshape2)$coefficients[3,1] ##############
	
	mbellshape2$t.max <- t.max <- abs(b/a)
	mbellshape2$R2 <- R2 <- summary(mbellshape2)$adj.r.squared
	data.y.pred <- predict.fun(data.time)
	mbellshape2$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mbellshape2$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)

	mbellshape2$IP1 <- IP1 <- abs((b-sqrt(b))/a)
	mbellshape2$IP2 <- IP2 <- abs((b+sqrt(b))/a)
	prediction <- predict.fun(pred.time)
	# IPs <- c(t.max, IP1, IP2)
	# pred.infl <- predict.fun(IPs)
	legend <- substitute(expression(paste(italic(A*t^b*e^{-a*t}), ", Bell-shaped 2, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mbellshape2, para=list(RMSRE=RMSRE, R=R, R2=R2, t.max=t.max), 
			# inflection.before=IP1, inflection.after=IP2, 
			# inflection.before.pred=pred.infl[2], inflection.after.pred=pred.infl[3], t.max.pred=pred.infl[1], 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Bell-shaped 2")
}

bellshape3.model <- function(data.y, data.time, pred.time=NULL, col="#F39800", lty=5, ...){
	predict.fun <- function(data.time){
		result <- exp(predict(mbellshape3, data.frame(data.time=data.time, data.time.sq=-data.time^2)))
		return(result)
	}
	
	agr.fun <- function(data.time){
		A <- mbellshape3$A
		a <- mbellshape3$a
		b <- mbellshape3$b
		A*(b-2*a*data.time)*exp(b*data.time-a*data.time^2)
	}
	
	rgr.fun <- function(data.time){
		a <- mbellshape3$a
		b <- mbellshape3$b
		b-2*a*data.time
	}
	
	if(is.null(pred.time)){pred.time <- data.time}
	data.time.sq <- -data.time^2 ##############
	mbellshape3 <- lm(log(data.y) ~ data.time+data.time.sq)
	mbellshape3$A <- A <- exp(summary(mbellshape3)$coefficients[1,1])
	mbellshape3$b <- b <- summary(mbellshape3)$coefficients[2,1]
	mbellshape3$a <- a <- summary(mbellshape3)$coefficients[3,1]
	mbellshape3$t.max <- t.max <- abs(b/(2*a))
	mbellshape3$IP1 <- IP1 <- abs((b-sqrt(2*a))/(2*a))
	mbellshape3$IP2 <- IP2 <- abs((b+sqrt(2*a))/(2*a))
	
	mbellshape3$R2 <- R2 <- summary(mbellshape3)$adj.r.squared
	data.y.pred <- predict.fun(data.time)
	mbellshape3$R <- R <- cor(data.y, data.y.pred, use="pairwise.complete.obs")
	mbellshape3$RMSRE <- RMSRE <- rmsre(data.y, data.y.pred)
	
	prediction <- predict.fun(pred.time)
	# IPs <- c(t.max, IP1, IP2)
	# pred.infl <- predict.fun(IPs)
	legend <- substitute(expression(paste(italic(A*e^{b*t-a*t^2}), ", Bell-shaped 3, ", 
				italic(R)^2 == RR)), list(RR=sprintf("%.4f", R2)))[2]
	list(model=mbellshape3, para=list(RMSRE=RMSRE, R=R, R2=R2, t.max=t.max), 
			# inflection.before=IP1, inflection.after=IP2, 
			# inflection.before.pred=pred.infl[2], inflection.after.pred=pred.infl[3], t.max.pred=pred.infl[1], 
			prediction=prediction, predict.fun=predict.fun, agr.fun=agr.fun, rgr.fun=rgr.fun, 
			legend=legend, color=col, lty=lty, name="Bell-shaped 3")
}

model.list <- list(
    exponential = exponential.model, 
    monomolecular = monomolecular.model, 
    logistic = logistic.model, 
    gompetz = gompetz.model, 
    weibull = weibull.model, 
    linear = linear.model,
    quadratic = quadratic.model,
    bellshape1 = bellshape1.model, 
    bellshape2 = bellshape2.model, 
    bellshape3 = bellshape3.model
)
