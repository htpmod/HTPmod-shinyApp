library(shiny)
library(shinyBS)
library(readr)
library(svglite)
library(doParallel)
library(BiocParallel)
library(gbm)
library(RColorBrewer)
library(pROC)
source('code/models.R')

options(shiny.maxRequestSize = 10 * 1024 ^ 2) # max file size, 10Mb
gc(reset=TRUE)
## options(java.parameters = "-Xmx50g")
if(!exists("workers")){
    workers <- 2
}
param <- MulticoreParam(workers=workers, stop.on.error=F, timeout=432000)
## doMC not available on Windows
if(.Platform$OS.type != "windows") {
    library(doMC)
    registerDoMC(cores = workers)
    ## makeCluster(workers, type='FORK') 
}else{
    # param <- SnowParam(workers=workers, stop.on.error=F, type="SOCK", timeout=432000)
    # cl <- makeCluster(workers, type='PSOCK')
    # registerDoParallel(cl)
    # ## code 
    # stopCluster(cl)
}

my.models <- list(Regression=regression.models, Classification=classification.models)
my.analysis <- list(Regression=regression.analysis, Classification=classification.analysis)
my.plot <- list(Regression=regression.plot, Classification=classification.plot)

### hist plot with error bars
error.bar <- function(x, y, upper, lower=upper, length=0.03, ...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper)){
        stop("vectors must be same length")
    }
    arrows(x, y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

shinyServer(function(input, output, session) {
    ## creat tmp workspace 
    mydir <- paste0(getwd(), "/tmp/", getTmpString())
    dir.create(mydir, recursive=T)
    
    ## cleaning temp data
    onReactiveDomainEnded(session, function(){
        for(d in dev.list()){
            dev.off()
        }
        if(length(list.files(mydir)) == 0){
            print(paste("Cleaning workspace:", mydir))
            unlink(mydir, recursive=T, force=T)
        }
        ## print(names(session$clientData))
        ## print(getwd())
    })
    
    output$uiDS <- renderUI(
        if(!is.nothing(input$example_data)){
            id <- paste(unlist(strsplit(input$example_data, "_"))[1:2], collapse="_")
            div("The example file is from",
                a(
                    href = reference[[id]][1],
                    target = "_blank",
                    reference[[id]][2]
                ),
                "You can download it",
                downloadLink("downloadExample", label = " here")
            )
        }
    )
    
    observe({
        disable("downloadModelpng")
        disable("downloadModelsvg")
        disable("downloadModelpdf")
        disable("downloadModeltab")
        disable("downloadEvalpng")
        disable("downloadEvalsvg")
        disable("downloadEvalpdf")
        disable("downloadEvaltab")
        disable("downloadImppng")
        disable("downloadImpsvg")
        disable("downloadImppdf")
        disable("downloadImptab")
        if (input$fileToUse == "uploadData") {
            shinyjs::show("div_fileupload")
            shinyjs::hide("div_exampleInUse")
        } else {
            shinyjs::hide("div_fileupload")
            shinyjs::show("div_exampleInUse")
        }
    })
    
    output$downloadExample <- downloadHandler(
        paste0("example_modeling_data.csv"),
        content = function(file) {
            file.copy(paste0("data/",input$example_data,".csv"), file)
        },
        contentType = "text/csv"
    )
    
    output$uiModels <- renderUI({
        mymods <- my.models[[input$model_type]]
        selectizeInput(
            "models",
            "Models (empty to select all)",
            choices = mymods,
            multiple = T,
            options = list(placeholder = 'Please select models below'),
            selected = ""
        )
    })
    
    ##
    inputDataAnalysis <- reactive({
        if (input$fileToUse == "demo") {
            if(is.nothing(input$example_data)){
                return(NULL)
            }

            inputData <- NULL
            withProgress(value = 1,
                         message = "Loading example data: ",
                         detail = "reading file",
                         {
                             inputData <-
                                 read_delim(paste0("data/",input$example_data,".csv"),
                                            delim = ",", col_names = T)
                             setProgress(value = 0.5, detail = "checking file format ...")
                             validate(
                                 need(
                                     ncol(inputData) > 1,
                                     "Incorrect input file format. Please ask the author. "
                                 )
                             )
                             setProgress(value = 1, detail = "done!")
                         })
            withProgress(value = 1,
                         message = "Preparing data",
                         detail = "...",
                         {
                             nums <- sapply(inputData, is.numeric)
                             numericols <- colnames(inputData)[nums]
                             nonumeric <- catcols <- colsize <- NULL
                             if(!is.null(nums)){
                                 nonumeric <- colnames(inputData)[!nums]
                                 unicols <- apply(inputData[, nonumeric], 2, function(x){
                                     ## length(unique(x))!=length(x) && sum(table(x)==1) < length(unique(x))/2
                                     min(table(x)) > 1 && length(table(x)) > 1
                                 })
                                 catcols <- names(unicols)[unicols]
                                 colsize <- apply(inputData[, nonumeric], 2, function(x){length(unique(x))})
                             }
                         })
            return(list(data=inputData, numericCol=numericols, nonNumeric=nonumeric, 
                        categoryCol=catcols, colSize=colsize));
        }
        
        inputFileName <- input$inputFile
        if (is.null(inputFileName)) {
            inputData <- numericols <- nonumeric <- catcols <- colsize <- NULL
        } else {
            withProgress(value = 1,
                         message = "Loading user data: ",
                         detail = "reading file",
                         {
                             inputData <-
                                 read_delim(
                                     inputFileName$datapath,
                                     delim = input$separator,
                                     col_names = T
                                 )
                             setProgress(value = 1, detail = "checking file format ...")
                             validate(
                                 need(
                                     ncol(inputData) > 1,
                                     "Incorrect input file format (at two columns). Plase check your input data."
                                 )
                             )
                             setProgress(value = 1, detail = "done!")
                         })
            withProgress(value = 1,
                         message = "Preparing data",
                         detail = "...",
                         {
                             nums <- sapply(inputData, is.numeric)
                             numericols <- colnames(inputData)[nums]
                             nonumeric <- catcols <- colsize <- NULL
                             if(!is.null(nums)){
                                 nonumeric <- colnames(inputData)[!nums]
                                 unicols <- apply(inputData[, nonumeric], 2, function(x){
                                     ## length(unique(x))!=length(x) && sum(table(x)==1) < length(unique(x))/2
                                     min(table(x)) > 1 && length(table(x)) > 1
                                 })
                                 catcols <- names(unicols)[unicols]
                                 colsize <- apply(inputData[, nonumeric], 2, function(x){length(unique(x))})
                             }
                         })
        }
        return(list(data=inputData, numericCol=numericols, nonNumeric=nonumeric, 
                    categoryCol=catcols, colSize=colsize));
    })
    
    getModata <- reactive({
        data <- inputDataAnalysis()$data
        excs <- c(input$Ex)
        incs <- setdiff(colnames(data), excs)
        data <- data[, incs]
        y <- input$y
        return(list(mat=data, y=y, x=setdiff(incs, y)))
    })
    
    observe({
        ## cat(class(inputDataAnalysis()$data))
        if (is.null(inputDataAnalysis()$data)) {
            ## shinyjs::hide("div_step2")
            shinyjs::hide("div_step3")
            shinyjs::hide("div_step4")
        } else {
            ## shinyjs::show("div_step2")
            shinyjs::show("div_step3")
            shinyjs::show("div_step4")
            output$uiY <- renderUI({
                if(input$model_type == "Regression"){
                    choices <- inputDataAnalysis()$numericCol
                    selectizeInput(
                        "y",
                        "The 'y' column for prediction: ",
                        choices = setNames(choices, choices)
                    )
                }else{
                    inputData <- inputDataAnalysis()
                    colsize <- inputData$colSize
                    choices <- clabels <- inputData$categoryCol
                    if(length(choices) > 0){
                        clabels <- paste0(choices, " (class=", colsize[choices], ")")
                    }
                    selectizeInput(
                        "y",
                        "The 'y' column for prediction: ",
                        choices = setNames(choices, clabels)
                    )
                }
            })
            output$uiEx <- renderUI({
                if(input$model_type == "Regression"){
                    inputData <- inputDataAnalysis()
                    nonumeric <- inputData$nonNumeric
                    numericols <- setdiff(inputData$numericCol, input$y)
                    choices <- c(nonumeric, numericols)
                    selectizeInput(
                        "Ex",
                        "Excluding columns from prediction: ",
                        multiple = T,
                        selected = nonumeric, 
                        choices = setNames(choices, choices)
                    )
                }else{
                    inputData <- inputDataAnalysis()
                    nonumeric <- inputData$nonNumeric
                    numericols <- inputData$numericCol
                    nonumeric <- setdiff(nonumeric, input$y)
                    choices <- c(nonumeric, numericols)
                    selectizeInput(
                        "Ex",
                        "Excluding columns from prediction: ",
                        multiple = T,
                        selected = nonumeric, 
                        choices = setNames(choices, choices)
                    )
                }
                
            })
            output$uiX <- renderPrint({
                data <- getModata()
                c(nrow(data$mat), length(data$x))
            })
        }
    })
    
    myValidation <- function() {
        validate(
            need(
                !is.null(input$inputFile) |
                    input$fileToUse == "demo",
                "Upload your file, or Use the example data."
            ),
            need(
                !(is.nothing(getModata()$y) | length(getModata()$y)< 1), 
                "Select the 'y' column for prediction."
            ),
            need(
                !(is.nothing(getModata()$x) | length(getModata()$x) < 2), 
                "Not enough data for modeling."
            ),
            need(
                (input$model_type == "Regression" && all(sapply(getModata()$mat, is.numeric))) || 
                    input$model_type != "Regression" && sum(!sapply(getModata()$mat, is.numeric))==1, 
                "The 'X' contains non-numeric values (for regression), or contains more than one columns with non-numeric values (for classification)."
            )
        )
    }
    
    output$uiModType <- renderUI({
        paste0("Model formula for ", input$model_type, ":")
    })
    
    output$uiFormula <- renderPrint({
        myValidation()
        data <- getModata() 
        print(formula(paste(data$y, "~", paste(getModata()$x, collapse="+"))), showEnv = F)
    })
    
    output$inputData <- DT::renderDataTable({
        myValidation()
        data <- inputDataAnalysis()$data
        if(ncol(data) > 4){
            n <- ncol(data) - 4;
            data <- cbind(data[, 1:4], More=paste0("... skip ", n, " columns"))
        }
        data
    })
    
    getModCols <- function(models){
        if(length(models) < 3){
            mcols <- c("#e41a1c", "#377eb8", "#4daf4a")[1:length(models)]
        }else if(length(models) < 9){
            mcols <- brewer.pal(n=length(models), name="Set1")
        }else{
            mcols <- colorRampPalette(brewer.pal(n=5, name="Set1"))(length(models))
        }
        names(mcols) <- unlist(lapply(names(models), function(x){unlist(strsplit(x,":"))[1]}))
        return(mcols)
    }
    
    getPreProc <- function(){
        preProc <- NULL
        if(input$center || input$scale){
            preProc <- c()
            if(input$center){
                preProc <- "center"
            }
            if(input$scale){
                preProc <- c(preProc, "scale")
            }
        }
        preProc
    }
    
    runModeling <- function(){
        if(is.nothing(input$model_type)){
            msgPlot()
            return(NULL)
        }
        myValidation()
        modat <- getModata()
        validate(
            need(
                all(c(modat$y, modat$x) %in% colnames(modat$mat)),
                "Check input data ... "
            )
        )
        mydata <- data.frame(modat$mat[, c(modat$y, modat$x)], check.names=F)
        mymods <- my.models[[input$model_type]]
        mypips <- my.analysis[[input$model_type]]
        myfigs <- my.plot[[input$model_type]]
        if (is.nothing(input$models) ||
            is.nothing(trimws(input$models))) {
            useModels <- as.character(mymods)
        } else{
            useModels <- input$models
        }
        usedModels <- list()
        for(mid in names(mymods)){
            model <- mymods[[mid]]
            if(model %in% useModels){
                usedModels[[mid]] <- model 
            }
        }
        mcols <- getModCols(usedModels)
        preProc <- getPreProc()
        run <- function(mid) {
            model <- usedModels[[mid]]
            mname <- unlist(strsplit(mid, ":"))[1]
            withProgress(value = 0.5,
                         message = paste("Run", input$model_type, "model:", mname),
                         detail = "...",
                         {
                             mod <- try(mypips(mydata, model=model, nfold=input$nfold, ntime=input$ntimes, preProc=preProc), silent=TRUE)
                             if (class(mod) == "try-error") {
                                 msg <- paste0("**** errors in running model: \"", mname, "\" [", model, "]. Skipping ...\n")
                                 cat(msg)
                                 showNotification(msg, type='error')
                                 setProgress(value = 1, detail = "skip due to errors!")
                                 return(mname)
                             }
                             setProgress(value = 1, detail = "done!")
                             if(!is.null(mod)){
                                 mod$model <- mname
                                 mod$y <- modat$y
                             }
                             mod 
                         })
        }
        result <- bplapply(names(usedModels), run, BPPARAM=param)
        out <- c()
        for(mod in result){
            if("list" %in% class(mod)){
                o <- data.frame(observed=mod$observed, predicted=mod$predicted)
                colnames(o) <- paste(mod$model, c("observed", "predicted"))
                if(is.null(out)){
                    out <- o
                }else{
                    out <- cbind(out, o)
                }
            }
        }
        n <- ceiling(sqrt(length(useModels)))
        pp <- function(){
            op <- par(mfrow=c(n, n), pty="m")
            i <- 1
            for(mod in result){
                if("list" %in% class(mod)){
                    myfigs(mod, col=mcols[mod$model], pch=16, main=paste0(mod$model, ": prediction of ", mod$y))
                }else{
                    plot(1, type="n", xaxt="n", yaxt="n", frame=F, xlab="", ylab="", 
                         main=paste0(mod, ": prediction of ", modat$y), sub=names(usedModels)[i])
                    txt <- "Error in modeling"
                    cex.cor <- 0.8/strwidth(txt)
                    text(1, 1, txt, cex=cex.cor, col=mcols[mod])
                    ## msgPlot(msg=paste("Error in modeling\n", names(usedModels)[i]), col=mcols[mod])
                }
                i <- i + 1
            }
            par(op)
        }
        
        if(input$model_type == "Regression"){
            ## export files for downloading 
            write.table(out, paste0(mydir, "/", "modeling_regression_out.txt"), quote=F, row.names=F, sep="\t")
            pdf(paste0(mydir, "/", "modeling_regression_figures.pdf"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            png(paste0(mydir, "/", "modeling_regression_figures.png"), width = 750, height = 750, pointsize = 10)
            pp()
            dev.off()
            svglite(paste0(mydir, "/", "modeling_regression_figures.svg"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            enable("downloadModelpng")
            enable("downloadModelsvg")
            enable("downloadModelpdf")
            enable("downloadModeltab")
            ## save(result, file = paste0(mydir, "/", "modeling_data.Rdata"))
            pp()
        }else{
            ## export files for downloading
            write.table(out, paste0(mydir, "/", "modeling_classification_out.txt"), quote=F, row.names=F, sep="\t")
            pdf(paste0(mydir, "/", "modeling_classification_figures.pdf"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            png(paste0(mydir, "/", "modeling_classification_figures.png"), width = 750, height = 750, pointsize = 10)
            pp()
            dev.off()
            svglite(paste0(mydir, "/", "modeling_classification_figures.svg"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            enable("downloadModelpng")
            enable("downloadModelsvg")
            enable("downloadModelpdf")
            enable("downloadModeltab")
            pp()
        }
    }
    
    runEvaluation <- function(){
        if(is.nothing(input$model_type)){
            msgPlot()
            return(NULL)
        }
        myValidation()
        modat <- getModata()
        validate(
            need(
                all(c(modat$y, modat$x) %in% colnames(modat$mat)),
                "Check input data ... "
            )
        )
        mydata <- data.frame(modat$mat[, c(modat$y, modat$x)], check.names=F)
        mymods <- my.models[[input$model_type]]
        if (is.nothing(input$models) ||
            is.nothing(trimws(input$models))) {
            useModels <- as.character(mymods)
        } else{
            useModels <- input$models
        }
        usedModels <- list()
        for(mid in names(mymods)){
            model <- mymods[[mid]]
            if(model %in% useModels){
                usedModels[[mid]] <- model 
            }
        }
        mcols <- getModCols(usedModels)
        preProc <- getPreProc()
        times <- input$ntimes 
        
        errorbarplot <- function(data, metric, col="orange", ...){
            avg <- aggregate(data[,metric], list(model=data$model), mean, na.rm=T)
            std <- aggregate(data[,metric], list(model=data$model), function(x) sqrt(var(x, na.rm=TRUE)/length(x)))
            rownames(avg) <- avg[,'model']; rownames(std) <- std[,'model']
            ylim <- range(data[,metric], na.rm=T)
            if(metric == "RMSRE"){
                ylim[1] <- 0
            }else if (metric == "mu"){
                ylim <- range(data[,metric]*1.05, na.rm=T)
            }else if (metric == "Accuracy" || metric == "AUC"){
                ylim[2] <- 1
            }else{
                ylim[1] <- ylim[1]*.95
                round(ylim, dig=2)
            }
            avg <- avg[rownames(std),'x']; names(avg) <- rownames(std)
            std <- std[, 'x']; names(std) <- names(avg)
            if(!is.null(names(col))){
                col <- col[names(avg)]
            }
            barx <- barplot(avg, beside=TRUE, las=2, xpd=FALSE, border=NA, col=col, ylim=ylim, ...)
            box()
            ## text(barx, avg+abs(par('usr')[4]-par('usr')[3])*0.02, xpd=FALSE, labels=format(avg, dig=3))
            error.bar(barx, avg, std, col='black', ...)
            invisible(barx)
        }
        biotrait <- modat$y 
        tit <- paste("Prediction of", biotrait)
        
        if(input$model_type == "Regression"){
            run <- function(mid, n) {
                model <- mymods[[mid]]
                mname <- unlist(strsplit(mid, ":"))[1]
                withProgress(value = 1,
                             message = paste0("Run Regression model: ", mname, ", i=", n),
                             detail = "...",
                             {
                                 mod <- try(regression.analysis(mydata, model=model, nfold=input$nfold, preProc=preProc), silent=TRUE)
                                 if (class(mod) == "try-error") {
                                     msg <- paste0("**** Errors in running model: \"", mname, "\" [", model, "]. Skipping ...\n")
                                     cat(msg)
                                     showNotification(msg, type='error')
                                     setProgress(value = 1, detail = "skip due to errors!")
                                     return(mname)
                                 }
                                 mod$n <- n 
                                 mod$model <- model 
                                 mod 
                             })
            }
            result <- bpmapply(run, rep(names(usedModels), times), rep(1:times, each=length(usedModels)), BPPARAM=param, SIMPLIFY = F)
            mnames <- unlist(lapply(names(result), function(x){unlist(strsplit(x,":"))[1]}))
            out <- data.frame(model=mnames, Run=NA, R2=NA, PCC=NA, RMSRE=NA, mu=NA) 
            for(i in 1:length(result)){
                if("list" %in% class(result[[i]])){
                    out[i,c('Run', 'R2', 'PCC', 'RMSRE', 'mu')] <- c(result[[i]]$n, result[[i]]$R2, result[[i]]$PCC, result[[i]]$RMSRE, result[[i]]$mu)
                }
            }
            
            pp <- function(){
                biotrait <- modat$y 
                op <- par(mfrow=c(2, 2), pty="m")
                errorbarplot(out, 'R2', col=mcols, main=tit, ylab=expression(R^2))
                errorbarplot(out, 'PCC', col=mcols, main=tit, ylab="PCC")
                errorbarplot(out, 'RMSRE', col=mcols, main=tit, ylab="RMSRE")
                errorbarplot(out, 'mu', col=mcols, main=tit, ylab=expression(mu))
                par(op)
            }
            
            ## export files for downloading 
            write.table(out, paste0(mydir, "/", "modeling_regression_evaluation.txt"), quote=F, row.names=F, sep="\t")
            pdf(paste0(mydir, "/", "modeling_regression_evaluation.pdf"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            png(paste0(mydir, "/", "modeling_regression_evaluation.png"), width = 750, height = 750, pointsize = 10)
            pp()
            dev.off()
            svglite(paste0(mydir, "/", "modeling_regression_evaluation.svg"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            enable("downloadEvalpng")
            enable("downloadEvalsvg")
            enable("downloadEvalpdf")
            enable("downloadEvaltab")
            pp()
        }else{
            run <- function(mid, n) {
                model <- mymods[[mid]]
                mname <- unlist(strsplit(mid, ":"))[1]
                withProgress(value = 1,
                             message = paste0("Run Classification model: ", mname, ", i=", n),
                             detail = "...",
                             {
                                 mod <- try(classification.analysis(mydata, nfold=input$nfold, model=model, preProc=preProc), silent=TRUE)
                                 if (class(mod) == "try-error") {
                                     msg <- paste0("**** Errors in running model: \"", mname, "\" [", model, "]. Skipping ...\n")
                                     cat(msg)
                                     showNotification(msg, type='error')
                                     setProgress(value = 1, detail = "skip due to errors!")
                                     return(mname)
                                 }
                                 mod$model <- mname 
                                 mod
                             })
            }
            result <- bpmapply(run, names(usedModels), rep(1:times, each=length(usedModels)), BPPARAM=param, SIMPLIFY = F)
            mnames <- unlist(lapply(names(result), function(x){unlist(strsplit(x,":"))[1]}))
            out <- data.frame(model=mnames, AUC=NA, Accuracy=NA)
            iroc <- rep(0, length(usedModels))
            names(iroc) <- unlist(lapply(names(usedModels), function(x){unlist(strsplit(x,":"))[1]})) 
            macc <- iroc 
            for(i in 1:length(result)){
                if("list" %in% class(result[[i]])){
                    out[i,c('AUC', 'Accuracy')] <- c(result[[i]]$auc, result[[i]]$accuracy)
                    if(result[[i]]$accuracy > macc[result[[i]]$model]){
                        macc[result[[i]]$model] <- result[[i]]$accuracy
                        iroc[result[[i]]$model] <- i
                    }
                }
            }
            pp <- function(){
                if(all(is.na(out[,'AUC']))){
                    msgPlot("Nothing to plot\ndue to modeling errors!")
                }else{
                    op <- par(mfrow=c(2, 2), pty="m")
                    errorbarplot(out, 'AUC', col=mcols, main=tit, ylab="AUC")
                    errorbarplot(out, 'Accuracy', col=mcols, main=tit, ylab="Accuracy")
                    
                    lgds <- pcols <- c()
                    flag <- F 
                    ## ROC 
                    if(length(iroc) == 1){ ## only one model is used 
                        m <- names(iroc)[1]
                        i <- iroc[m] 
                        if(i > 0){
                            rocs <- result[[i]]$roc 
                            mc <- result[[i]]$mc
                            plot(rocs[[1]], col=mcols[m], auc.polygon.col=NA, ## auc.polygon=TRUE, 
                                 mar=par('mar'), mgp=par('mgp'), grid.lty=4,
                                 grid=c(0.2, 0.2), grid.col=c("lightgray", "lightgray"), main=tit)
                            if(mc){ ## multiclass 
                                ccc <- colorRampPalette(c('white', mcols[m]))(length(rocs)+1)
                                for(j in 2:length(rocs)){
                                    plot(rocs[[j]], add=TRUE, col=ccc[j])
                                }
                                lgds <- paste0(m, ": ", names(rocs))
                                pcols <- ccc[-1]
                            }else{
                                lgds <- c(lgds, paste0(m, ": ", names(rocs)[1]))
                                pcols <- c(pcols, mcols[m])
                            }
                        }else{
                            msgPlot("Nothing to plot!")
                        }
                    }else{ ## multiple models 
                        nerr <- 0
                        n <- 0
                        for(m in names(iroc)){
                            i <- iroc[m] 
                            if(i > 0){
                                rocs <- result[[i]]$roc 
                                mc <- result[[i]]$mc
                                flag <- mc 
                                mx <- 1
                                if(mc){ ## multiclass 
                                    mx <- which.max(unlist(lapply(rocs, function(x){x$auc+0})))
                                    lgds <- c(lgds, paste0(m, ": ", names(rocs)[mx]))
                                }else{
                                    lgds <- c(lgds, m)
                                }
                                pcols <- c(pcols, mcols[m])
                                ## for multiclass, only plot the best ROC 
                                if(n > 0){
                                    plot(rocs[[mx]], add=TRUE, col=mcols[m])
                                }else{
                                    plot(rocs[[mx]], col=mcols[m], auc.polygon.col=NA, ## auc.polygon=TRUE, 
                                         mar=par('mar'), mgp=par('mgp'), grid.lty=4, 
                                         grid=c(0.2, 0.2), grid.col=c("lightgray", "lightgray"), main=tit)
                                }
                                n <- n + 1
                            }else{
                                nerr <- nerr + 1
                            }
                        } 
                        if(nerr == length(iroc)){
                            msgPlot("Nothing to plot!")
                        }
                    }
                    if(length(lgds) > 0){
                        ## plot(1, type="n", xaxt="n", yaxt="n", frame=F, xlab="", ylab="", main="Figure legend")
                        if(length(iroc) > 1 && flag){
                            title(sub="Note: for multiclass, only the best ROC is shown")
                        }
                        ncolumn <- min(3, trunc(sqrt(length(lgds))))
                        cex <- 1
                        if(ncolumn > 2){
                            cex <- sqrt(2/ncolumn) 
                        }
                        my.legend("bottomright", xpd=TRUE, lty=1, legend=lgds, cex=cex, 
                                  ncol=ncolumn, col=pcols, text.col=pcols, pch=16)
                    }
                    ## PRC 
                    if(length(iroc) == 1){ ## only one model is used 
                        m <- names(iroc)[1]
                        i <- iroc[m] 
                        if(i > 0 && result[[i]]$pr){
                            prcs <- result[[i]]$prc 
                            mc <- result[[i]]$mc
                            plot(prcs[[1]], type='l', col=mcols[m], xlim=c(0,1), ylim=c(0,1),
                                 panel.first=grid(nx=5,ny=5,lty=4), xlab='Recall', ylab='Precision', main=tit)
                            if(mc){ ## multiclass 
                                ccc <- colorRampPalette(c('white', mcols[m]))(length(prcs)+1)
                                for(j in 2:length(prcs)){
                                    lines(prcs[[j]], col=ccc[j])
                                }
                                lgds <- paste0(m, ": ", names(prcs))
                                pcols <- ccc[-1]
                            }else{
                                lgds <- c(lgds, paste0(m, ": ", names(prcs)[1]))
                                pcols <- c(pcols, mcols[m])
                            }
                        }else{
                            msgPlot("Nothing to plot!")
                        }
                    }else{ ## multiple models 
                        nerr <- 0
                        n <- 0
                        for(m in names(iroc)){
                            i <- iroc[m] 
                            if(i > 0 && result[[i]]$pr){
                                rocs <- result[[i]]$roc 
                                prcs <- result[[i]]$prc
                                mc <- result[[i]]$mc
                                flag <- mc 
                                mx <- 1
                                if(mc){ ## multiclass 
                                    mx <- which.max(unlist(lapply(rocs, function(x){x$auc+0})))
                                    lgds <- c(lgds, paste0(m, ": ", names(rocs)[mx]))
                                }else{
                                    lgds <- c(lgds, m)
                                }
                                pcols <- c(pcols, mcols[m])
                                ## for multiclass, only plot the best ROC 
                                if(n > 0){
                                    lines(prcs[[mx]], col=mcols[m])
                                }else{
                                    plot(prcs[[mx]], type='l', col=mcols[m], xlim=c(0,1), ylim=c(0,1),
                                         panel.first=grid(nx=5,ny=5,lty=4), xlab='Recall', ylab='Precision', main=tit)
                                }
                                n <- n + 1
                            }else{
                                nerr <- nerr + 1
                            }
                        } 
                        if(nerr == length(iroc)){
                            msgPlot("Nothing to plot!")
                        }
                        title(sub="Note: for multiclass, only PRC for the best ROC is shown")
                    }
                    par(op)
                }
            }
            
            ## export files for downloading 
            write.table(out, paste0(mydir, "/", "modeling_classification_evaluation.txt"), quote=F, row.names=F, sep="\t")
            pdf(paste0(mydir, "/", "modeling_classification_evaluation.pdf"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            png(paste0(mydir, "/", "modeling_classification_evaluation.png"), width = 750, height = 750, pointsize = 10)
            pp()
            dev.off()
            svglite(paste0(mydir, "/", "modeling_classification_evaluation.svg"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            enable("downloadEvalpng")
            enable("downloadEvalsvg")
            enable("downloadEvalpdf")
            enable("downloadEvaltab")
            pp()
        }
    }
    
    output$modelingPlot <- output$evaluationPlot <-  output$featureImp <- renderPlot({
        msgPlot()
    })
    
    observeEvent(input$runMod, {
        output$modelingPlot <- renderPlot({
            runModeling() 
        })
    })
    
    observeEvent(input$runEva, {
        output$evaluationPlot <- renderPlot({
            runEvaluation()
        })
    })
    
    output$currentMod <- renderUI({
        mymods <- my.models[[input$model_type]]
        if (is.nothing(input$models) ||
            is.nothing(trimws(input$models))) {
            useModels <- as.character(mymods)
        } else{
            useModels <- input$models
        }
        usedModels <- list()
        for(mid in names(mymods)){
            model <- mymods[[mid]]
            if(model %in% useModels){
                usedModels[[mid]] <- model 
            }
        }
        selectizeInput(
            "cm",
            "Choose a model",
            choices = usedModels
        )
    })
    
    runFeatureImp <- function(){
        validate(
            need(
                !is.nothing(input$cm),
                "Select an available model to calculate. "
            )
        )
        myValidation()
        preProc <- getPreProc()
        ## updateActionButton(session, "runImp", icon=icon("pause"))
        model <- input$cm
        modelab <- ''
        for(mid in names(regression.models)){
            if(model == regression.models[[mid]]){
                modelab <- unlist(strsplit(mid, ":"))[1]
                break; 
            }
        }
        modat <- getModata()
        validate(
            need(
                length(modat$x) > 2,
                "The 'X' matrix should be > 2 columns."
            )
        )
        mydata <- data.frame(modat$mat[, c(modat$y, modat$x)], check.names=F)
        biotrait <- modat$y 
        if(input$model_type == "Regression"){
            run <- function(trait) {
                withProgress(value = 1,
                             message = "Calculating prediction power",
                             detail = trait,
                             {
                                 submdata <- mydata[, c(biotrait, trait)]
                                 mod <- try(regression.analysis(submdata, model=model, nfold=input$nfold, ntime=input$ntimes, preProc=preProc), silent=TRUE)
                                 if (class(mod) == "try-error") {
                                     msg <- paste0("**** Errors in running model: \"", model, "\" for trait ", trait, ". Skipping ...\n")
                                     cat(msg)
                                     showNotification(msg, type='error')
                                     setProgress(value = 1, detail = "skip due to errors!")
                                     return(data.frame(trait=trait, R2=NA))
                                 }
                                 data.frame(trait=trait, R2=mod$R2)
                             })
            }
            R2 <- bplapply(modat$x, run, BPPARAM=param)
            trait.power <- data.frame(stringsAsFactors=FALSE, check.names=F)
            ## print(R2)
            for(i in 1:length(R2)){
                trait.power <- rbind(trait.power, R2[[i]])
            }
            rownames(trait.power) <- trait.power[,1]
            withProgress(value = 1,
                         message = "Calculating feature importance",
                         detail = "...",
                         {
                             mod <- suppressWarnings(regression.analysis(mydata, model=model, nfold=input$nfold, ntime=input$ntimes, preProc=preProc))
                         })
            trait.relimp <- mod$relimp
            
            pp <- function(){
                op <- par(mfrow=c(2, 1), pty="m")
                pltd <- sort(trait.relimp, decreasing = T)
                lbs <- names(pltd)
                cols <- colorRampPalette(brewer.pal(n=7, name="RdBu"))(length(lbs))
                names(cols) <- lbs
                barx <- barplot(pltd, border=NA, col=cols, ylab="Relative importance", las=2)
                m <- median(pltd)
                lines(range(barx), c(m,m), col='red', lty=5)
                title(main=paste0("Prediction of ", biotrait, ", based on ", modelab))
                
                pltd <- trait.power[,2]
                names(pltd) <- trait.power[,1]
                if(all(is.na(pltd))){
                    msgPlot(paste("Predictive power is not available for the model", modelab))
                }else{
                    pltd <- pltd[lbs]
                    barx <- barplot(pltd, border=NA, col=cols, ylab="Predictive power", las=2)
                    m <- median(pltd)
                    lines(range(barx), c(m,m), col='red', lty=5)
                    ## title(main=paste0("Prediction of ", biotrait, ", based on ", modelab))
                }
                par(op)
            }
            
            ## export files for downloading 
            out <- cbind(trait.power, Importance=trait.relimp[rownames(trait.power)])
            write.table(out, paste0(mydir, "/", "feature_importance.txt"), quote=F, row.names=F, sep="\t")
            pdf(paste0(mydir, "/", "feature_importance.pdf"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            png(paste0(mydir, "/", "feature_importanc.png"), width = 750, height = 750, pointsize = 10)
            pp()
            dev.off()
            svglite(paste0(mydir, "/", "feature_importanc.svg"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            enable("downloadImppng")
            enable("downloadImpsvg")
            enable("downloadImppdf")
            enable("downloadImptab")
            pp()
        }else{
            withProgress(value = 1,
                         message = "Calculating feature importance",
                         detail = "...",
                         {
                             mod <- classification.analysis(mydata, model=model, nfold=input$nfold, ntime=input$ntimes, preProc=preProc)
                         })
            trait.relimp <- mod$relimp
            allacc <- mod$accuracy
            overall <- mod$overall
            
            run <- function(subtraits) {
                subtraits <- unlist(subtraits)
                withProgress(value = 1,
                             message = paste("Calculating prediction accuracy based on", length(subtraits), "traits"),
                             detail = paste(paste(head(subtraits, n=3), collapse=", "), "..."),
                             {
                                 submdata <- mydata[, c(biotrait, subtraits)]
                                 mod <- try(classification.analysis(submdata, model=model, nfold=input$nfold, ntime=input$ntimes, preProc=preProc), silent=TRUE)
                                 if (class(mod) == "try-error") {
                                     msg <- paste0("**** Errors in running model: \"", model, "\" for trait ", trait, ". Skipping ...\n")
                                     cat(msg)
                                     showNotification(msg, type='error')
                                     setProgress(value = 1, detail = "skip due to errors!")
                                     return(NA)
                                 }
                                 mod$accuracy
                             })
            }
            ## refer to: 10.1038/msb.2010.25
            traits <- current <- modat$x
            accuracy_list <- list() 
            trait_list <- c()
            numb <- 1
            showNotification("Assess the infuence of features on classification performance", type='message', duration=30)
            while(length(current) > 2){
                accuracy_list[[numb]] <- list()
                showNotification(paste("Greedy feature selection from", length(current), "traits"), type='message')
                sublist <- apply(combn(length(current), length(current)-1), 2, function(x){list(current[x])})
                accuracy <- unlist(bplapply(sublist, run, BPPARAM=param))
                nextone <- unlist(sublist[[which.min(accuracy)]])
                rmtrait <- setdiff(current, nextone)
                current <- nextone
                showNotification(paste("Remove", rmtrait, "from the current list"), type='warning')
                accuracy_list[[numb]][['acc']] <- accuracy
                accuracy_list[[numb]][['trt']] <- current
                trait_list <- c(trait_list, rmtrait)
                numb <- numb + 1
            }
            trait_list <- c(trait_list, current)
            trait.performance <- c(length(traits), allacc) 
            for(i in 1:length(accuracy_list)){
                trait.performance <- rbind(trait.performance, cbind(length(accuracy_list[[i]][['trt']]),accuracy_list[[i]][['acc']])) 
            }
            trait.performance <- rbind(trait.performance, c(1, NA), c(1,NA))
            colnames(trait.performance) <- c('n', 'accuracy')
            trait.performance <- as.matrix(trait.performance)
            
            pp <- function(){
                op <- par(mfrow=c(2, 1), pty="m")
                if(overall){
                    pltd <- sort(trait.relimp, decreasing = T)
                    lbs <- names(pltd)
                    cols <- colorRampPalette(brewer.pal(n=7, name="RdBu"))(length(lbs))
                    names(cols) <- lbs
                    barx <- barplot(pltd, border=NA, col=cols, ylab="Relative importance", las=2)
                    m <- median(pltd)
                    lines(range(barx), c(m,m), col='red', lty=5)
                }else{
                    trait.relimp <- trait.relimp[trait_list, ]
                    pltd <- as.matrix(trait.relimp)
                    cols <- colorRampPalette(brewer.pal(n=7, name="RdBu"))(nrow(pltd)*ncol(pltd))
                    image(1:nrow(pltd), 1:ncol(pltd), pltd, col=cols, xaxt='n', yaxt='n', 
                          frame=F, xlab="Features", ylab="Classes")
                    box() 
                    grid(nx=nrow(pltd), ny=ncol(pltd))
                    axis(1, at=1:nrow(pltd), labels=rownames(pltd), las=2) 
                    axis(2, at=1:ncol(pltd), labels=colnames(pltd), las=2)
                }
                title(main=paste0("Prediction of ", biotrait, ", based on ", modelab))
                
                pltd <- trait.performance
                pltd <- as.data.frame(pltd)
                pltd$n <- factor(pltd$n, levels=sort(unique(pltd$n), decreasing=T))
                cols <- colorRampPalette(brewer.pal(n=7, name="RdBu"))(length(traits))
                bpt <- boxplot(accuracy~n, data=pltd, col=cols, xlab="Number of features", 
                               ylab="CV classification performance")
                text(1:length(traits), bpt$stats[5,], paste0("N=",table(pltd$n)), pos=3)
                title(main=paste0("Prediction of ", biotrait, ", based on ", modelab))
                par(op)
            }
            
            ## export files for downloading 
            trait_rank <- data.frame(Feature=trait_list, "##", Rank=length(traits):1)
            rownames(trait_rank) <- trait_list
            if(overall){
                out <- cbind(Feature=names(trait.relimp), trait.relimp, trait_rank[names(trait.relimp),])
            }else{
                out <- cbind(Feature=rownames(trait.relimp), trait.relimp, trait_rank[rownames(trait.relimp),])
            }
            write.table(out, paste0(mydir, "/", "feature_importance.txt"), quote=F, row.names=F, sep="\t")
            pdf(paste0(mydir, "/", "feature_importance.pdf"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            png(paste0(mydir, "/", "feature_importanc.png"), width = 750, height = 750, pointsize = 10)
            pp()
            dev.off()
            svglite(paste0(mydir, "/", "feature_importanc.svg"), width = 8.27, height = 8.27, pointsize = 10)
            pp()
            dev.off()
            enable("downloadImppng")
            enable("downloadImpsvg")
            enable("downloadImppdf")
            enable("downloadImptab")
            pp()
        }
    }
    
    # featurePlot <- reactive({
    #     clickAct <- input$runImp
    #     if (is.null(clickAct) || clickAct < 1) {
    #         msgPlot()
    #         return()
    #     }
    #     runFeatureImp() 
    # })
    # output$featureImp <- renderPlot({
    #     featurePlot()
    # })
    
    observeEvent(input$runImp, {
        output$featureImp <- renderPlot({
            runFeatureImp()
        })
    })
    
    output$downloadInputData <- downloadHandler(
        "modeling_data_file.txt",
        content = function(file) {
            data <- inputDataAnalysis()$data
            write.table(
                data,
                file = file,
                row.names = F,
                quote = F,
                sep = "\t"
            )
        },
        contentType = "text/tsv"
    )
    
    output$downloadModelsvg <- downloadHandler(
        "modeling_regression_figures.svg",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "modeling_regression_figures.svg"), file)
        },
        contentType = "image/svg"
    )
    
    output$downloadModelpng <- downloadHandler(
        "modeling_regression_figures.png",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "modeling_regression_figures.png"), file)
        },
        contentType = "image/png"
    )
    
    output$downloadModelpdf <- downloadHandler(
        "modeling_regression_figures.pdf", 
        content <- function(file) {
            file.copy(paste0(mydir, "/", "modeling_regression_figures.pdf"), file)
        },
        contentType = "image/pdf"
    )
    
    output$downloadModeltab <- downloadHandler(
        "modeling_regression_out.txt",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "modeling_regression_out.txt"), file)
        },
        contentType = "text/tsv"
    )
    
    output$downloadEvalsvg <- downloadHandler(
        "modeling_regression_evaluation.svg",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "modeling_regression_evaluation.svg"), file)
        },
        contentType = "image/svg"
    )
    
    output$downloadEvalpng <- downloadHandler(
        "modeling_regression_evaluation.png",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "modeling_regression_evaluation.png"), file)
        },
        contentType = "image/png"
    )
    
    output$downloadEvalpdf <- downloadHandler(
        "modeling_regression_evaluation.pdf",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "modeling_regression_evaluation.pdf"), file)
        },
        contentType = "image/pdf"
    )
    
    output$downloadEvaltab <- downloadHandler(
        "modeling_regression_evaluation.txt",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "modeling_regression_evaluation.txt"), file)
        },
        contentType = "text/tsv"
    )
    
    output$downloadImpsvg <- downloadHandler(
        "feature_importance.svg",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "feature_importance.svg"), file)
        },
        contentType = "image/svg"
    )
    
    output$downloadImppng <- downloadHandler(
        "feature_importance.png",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "feature_importance.png"), file)
        },
        contentType = "image/png"
    )
    
    output$downloadImppdf <- downloadHandler(
        "feature_importance.pdf",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "feature_importance.pdf"), file)
        },
        contentType = "image/pdf"
    )
    
    output$downloadImptab <- downloadHandler(
        "feature_importance.txt",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "feature_importance.txt"), file)
        },
        contentType = "text/tsv"
    )
    
    addPopover(session, "uiEx", "Tip", "<p>Exclude these columns from \"X\" for modeling. </p>",
               placement="bottom", trigger="hover", options = NULL)
})
