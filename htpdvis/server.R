library(shiny)
library(shinyBS)
library(readr)
library(svglite)
library(pcaMethods)
library(pheatmap)
library(ade4)
library(kohonen)
library(factoextra)
library(RColorBrewer)
library(ggcorrplot)
library(caret)
library(ggpubr)
library(corrplot)
library(Rtsne)
library(plot3D)
library(ape)
library(preprocessCore)
library(AppliedPredictiveModeling)
source('code/pca.plot.R')

options(shiny.maxRequestSize = 10 * 1024 ^ 2) # max file size, 10Mb

nospecial <- function(x){
    gsub("[^[:alnum:]]", ".", x)
}

# Multiple plot function (Cookbook for R)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
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
        ## if(length(list.files(mydir)) == 0){
        print(paste("Cleaning workspace:", mydir))
        unlink(mydir, recursive=T, force=T)
        ## }
        ## print(names(session$clientData))
        ## print(getwd())
    })
    
    # UI elements activations -------------------
    observe({
        if (input$fileToUse == "uploadData") {
            shinyjs::show("div_fileupload")
            shinyjs::hide("div_exampleInUse")
        } else {
            shinyjs::hide("div_fileupload")
            shinyjs::show("div_exampleInUse")
        }
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
    
    output$downloadExample <- downloadHandler(
        paste0("htpvis_data.csv"),
        content = function(file) {
            file.copy(paste0("data/",input$example_data,".csv"), file)
        },
        contentType = "text/csv"
    )
    
    ## prepare data 
    
    prepareData <- function(data){ 
        withProgress(value = 0.5,
                     message = "Preparing data",
                     detail = "...",
                     {
                         nums <- sapply(data, is.numeric)
                         numericCol <- colnames(data)[nums]
                         nonNumeric <- nonNumericSize <- NULL
                         if(!is.null(nums)){
                             nonNumeric <- colnames(data)[!nums]
                             if(length(nonNumeric) > 0){
                                 unicols <- apply(data[, nonNumeric], 2, function(x){
                                     ## length(unique(x))!=length(x) && sum(table(x)==1) < length(unique(x))/2
                                     min(table(x)) > 1 && length(table(x)) > 1
                                 })
                                 nonNumeric <- names(unicols)[unicols]
                                 nonNumericSize <- apply(data[, nonNumeric], 2, function(x){length(unique(x))})
                             }
                         }
                         mat <- NULL 
                         if(length(numericCol) > 0){
                             mat <- data[,numericCol]
                         }
                         setProgress(value = 1, detail = "done!")
                     })
        ## preprocessing: fill NA values
        if (sum(is.na(mat)) > 0){
            NPC <- max(trunc(sqrt(ncol(mat))), 3) 
            withProgress(value = 0.5,
                         message = "Data preprocessing",
                         detail = "Estimating 'NA' values using Bayesian PCA",
                         {
                             ## Perform Bayesian PCA with * components
                             resBPCA <- suppressWarnings(pca(as.matrix(mat), method="bpca", nPcs=NPC))
                             ## Get the estimated complete observations
                             mat <- completeObs(resBPCA)
                             setProgress(value = 1, detail = "done!")
                         })
        }
        attr(data, 'class') <- 'data.frame'
        save(data, file=paste0(mydir, "/data.RData"))
        save(mat, file=paste0(mydir, "/mat.RData"))
        save(numericCol, file=paste0(mydir, "/numericCol.RData"))
        save(nonNumeric, file=paste0(mydir, "/nonNumeric.RData"))
        save(nonNumericSize, file=paste0(mydir, "/nonNumericSize.RData"))
        out <- list(data=data, mat=mat, numericCol=numericCol, 
                    nonNumeric=nonNumeric, nonNumericSize=nonNumericSize)
        save(out, file=paste0(mydir, "/.RData"))
        return(out) 
    }
    
    ## 
    inputDataAnalysis <- reactive({
        if (input$fileToUse == "demo") {
            if(is.nothing(input$example_data)){
                return(NULL)
            }
            experment <- input$example_data
            if(file.exists(paste0(mydir, "/key.RData"))){
                load(paste0(mydir, "/key.RData"))
                if(experment == key){
                    showNotification("Loading data ...")
                    load(paste0(mydir, "/.RData"))
                    return(out)
                }
            }
            inputData <- NULL
            withProgress(value = 0,
                         message = "Loading example data: ",
                         detail = "reading file",
                         {
                             inputData <-
                                 read_delim(paste0("data/",experment,".csv"),
                                            delim = ",",
                                            col_names = T)
                             setProgress(value = 0.5, detail = "checking file format ...")
                             validate(
                                 need(
                                     ncol(inputData) > 1,
                                     "Incorrect input file format. Please ask the author. "
                                 )
                             )
                             setProgress(value = 1, detail = "done!")
                         })
            data0 <- prepareData(inputData)
            key <- experment
            save(key, file=paste0(mydir, "/key.RData"))
            return(data0); 
        }
        
        inputFileName <- input$inputFile
        if (is.null(inputFileName)) {
            inputData <- mat <- numericols <- nonumeric <- unicols <- NULL 
        } else {
            if(file.exists(paste0(mydir, "/key.RData"))){
                load(paste0(mydir, "/key.RData"))
                if(inputFileName$datapath == key){
                    showNotification("Loading data ...")
                    load(paste0(mydir, "/.RData"))
                    return(out)
                }
            }
            withProgress(value = 0,
                         message = "Loading user data: ",
                         detail = "reading file",
                         {
                             inputData <-
                                 read_delim(
                                     inputFileName$datapath,
                                     delim = input$separator,
                                     col_names = T
                                 )
                             setProgress(value = 0.5, detail = "checking file format ...")
                             validate(
                                 need(
                                     ncol(inputData) > 1,
                                     "Incorrect input file format (at least two columns). Plase check your input data."
                                 )
                             )
                             setProgress(value = 1, detail = "done!")
                         })
            data0 <- prepareData(inputData)
            key <- inputFileName$datapath
            save(key, file=paste0(mydir, "/key.RData"))
            return(data0) 
        }
        return(list())
    })
    
    retrieveData <- function(key){
        if(file.exists(paste0(mydir, "/", key, ".RData"))){
            load(paste0(mydir, "/", key, ".RData"))
            if(is.nothing(key)){
                return(out)
            }else{
                return(get(key))
            }
        }else{
            inputData <- inputDataAnalysis()
            if(is.nothing(key)){
                return(inputData)
            }else{
                return(inputData[[key]])
            }
        }
    }
    
    observe({
        ## cat(class(inputDataAnalysis()$data))
        inputData <- inputDataAnalysis()
        if (is.null(inputData$data)) {
            shinyjs::hide("div_step2")
            shinyjs::hide("div_step3")
        } else {
            shinyjs::show("div_step2")
            shinyjs::show("div_step3")
            withProgress(value = 0.5,
                         message = "Rendering UI. Please wait ...",
                         detail = "...",
                         {
                             output$uiX1 <- renderPrint({
                                 input$fileToUse
                                 input$example_data
                                 data <- inputData$data
                                 dim(data)
                             })
                             output$uiX2 <- renderPrint({
                                 input$fileToUse
                                 input$example_data
                                 data <- inputData$mat
                                 dim(data)
                             })
                             choicesRow <- inputData$nonNumeric
                             output$uicpRow <- renderUI({
                                 input$fileToUse
                                 input$example_data
                                 div(
                                     selectizeInput(
                                         "rowColor",
                                         "Point colors by: ",
                                         multiple = F, 
                                         choices = setNames(choicesRow, choicesRow)
                                     ),
                                     selectizeInput(
                                         "rowPoint",
                                         "Point symbols by: ",
                                         multiple = F, 
                                         choices = setNames(choicesRow, choicesRow)
                                     )
                                 )
                             })
                             setProgress(value = 1, detail = "done!")
                         })
        }
    })
    
    myValidation <- function(inputData) {
        ## showNotification("Checking Data ...");
        withProgress(value = 0.5,
                     message = "Checking Data ",
                     detail = "...",
                     {
                         ## inputData <- inputDataAnalysis()
                         validate(
                             need(
                                 !is.null(input$inputFile) |
                                     input$fileToUse == "demo",
                                 "Upload your file, or Use the example data."
                             ),
                             need(
                                 !is.nothing(inputData),
                                 "No input data detected."
                             ),
                             need(
                                 nrow(inputData$mat) > 0 && ncol(inputData$mat) > 0,
                                 "No numeric data found. Please check your input file."
                             ),
                             need(
                                 all(sapply(inputData$mat, is.numeric)), 
                                 "The matrix 'X' contains non-numeric values."
                             )
                         )
                         setProgress(value = 1, detail = "done!")
                     })
        
    }
    
    uiValidation <- function(){
        need(
            !is.null(input$rowColor) && 
                !is.null(input$rowPoint) && 
                !is.null(input$npcs), 
            "Some UIs not yet initiate. "
        )
    }
    
    output$inputData <- DT::renderDataTable({
        inputData <- inputDataAnalysis()
        myValidation(inputData)
        showNotification("Rendering Table ...");
        data <- inputData$data
        if(ncol(data) > 4){
            n <- ncol(data) - 4;
            data <- cbind(data[, 1:4], More=paste0("... ", n, " columns"))
        }
        data
    })
    
    output$downloadInputData <- downloadHandler(
        "visulization_data_file.txt",
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
    
    getCols <- function(inputData){
        uiValidation()
        ccol <- input$rowColor
        data <- inputData$data
        if(ccol %in% colnames(data)){
            cids <- sort(unique(data[, ccol]), na.last=T)
            if(length(cids) <= 3){
                cols <- c("#ff7f00", "#1f78b4", "#33a02c")[1:length(cids)]
            }else if(length(cids)<9){
                cols <- brewer.pal(n=length(cids), name="Set1")
                if(length(cols) > 5){
                    cols[6] <- '#999999'
                }
            }else{
                cols <- colorRampPalette(brewer.pal(n=8, name="Set1"))(length(cids))
            }
            names(cols) <- cids
            cols
        }else{
            NULL
        }
    }
    
    getPchs <- function(inputData){
        uiValidation()
        cpty <- input$rowPoint
        data <- inputData$data
        if(cpty %in% colnames(data)){
            pids <- sort(unique(data[, cpty]), na.last=T)
            pchs <- (1:length(pids)) %% 26
            names(pchs) <- pids
            pchs
        }else{
            NULL
        }
    }
    
    getFeatureCols <- function(inputData){
        features <- inputData$numericCol
        group <- unlist(lapply(features, function(x){o=unlist(strsplit(x, split="\\|"));o[2]}))
        names(group) <- features
        uniGroup <- sort(unique(group), na.last=T)
        if(length(uniGroup) <= 3){
            gcols <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")[1:length(uniGroup)]
        }else if(length(uniGroup)<10){
            gcols <- brewer.pal(n=length(uniGroup), name="Dark2")
        }else{
            gcols <- colorRampPalette(brewer.pal(n=8, name="Dark2"))(length(uniGroup))
        }
        names(gcols) <- paste0(uniGroup, "") ## in case that ALL are NA
        cols <- gcols[paste0(group,"")]
        names(cols) <- features
        list(col=cols, gcol=gcols, group=group)
    }
    
    output$uipxyzm <- renderUI({
        input$example_data
        input$fileToUse
        inputData <- inputDataAnalysis()
        myValidation(inputData)
        showNotification("Rendering variables ...");
        valx <- inputData$nonNumeric ## setdiff(inputData$nonNumeric, input$rowColor)
        vals <- inputData$numericCol
        div(
            selectizeInput(
                "miscx",
                "X variable: ",
                multiple = F, 
                choices = setNames(valx, valx)
            ), 
            selectizeInput(
                "miscy",
                "Y variable: ",
                multiple = F, 
                choices = setNames(vals, vals)
            ),
            selectizeInput(
                "miscz",
                "Z variable: ",
                multiple = F, 
                choices = setNames(vals, vals)
            ),
            selectizeInput(
                "miscm",
                "M variable: ",
                multiple = T, 
                choices = setNames(vals, vals),
                selected = head(vals)
            )
        )
    })

    runAnalysis <- function(plot=T, type='pca', para=F, both=F){
        inputData <- inputDataAnalysis()
        myValidation(inputData)
        uiValidation()
        if((
            type=="pca" || type=="hca" || type=="kmc" || type=="tsne" || type=="mds" || type=="som" ||
            ((!is.nothing(input$miscx) || !is.nothing(input$miscy) || !is.nothing(input$miscz) || !is.nothing(input$miscm)) && !is.nothing(input$misct) && type=="misc") 
            ) &&
            (input$rowPoint == "" || input$rowPoint %in% colnames(inputData$data)) && 
            (input$rowColor  =="" || input$rowColor %in% colnames(inputData$data))){
            pchs <- getPchs(inputData)
            cols <- getCols(inputData)
            ppchs <- pcols <- NA
            if(input$rowPoint != ""){
                ppchs <- pchs[inputData$data[, input$rowPoint]]
            }else{
                pchs <- c(All=16)
                ppchs <- rep(c(All=16), nrow(inputData$data))
            }
            if(input$rowColor != ""){
                pcols <- cols[inputData$data[, input$rowColor]]    
            }else{
                cols <- c(All='#ff7f00')
                pcols <- rep(c(All='#ff7f00'), nrow(inputData$data))
            }
            hcols <- pcols
            names(pcols) <- names(ppchs) <- rownames(inputData$data)
            tcols <- getFeatureCols(inputData)
            trait.cols <- tcols$col[colnames(inputData$mat)]
            gcols <- tcols$gcol
            group <- tcols$group[colnames(inputData$mat)]
            ## PCA 
            if(type == "pca"){
                nPCs <- input$npcs
                withProgress(value = 0,
                             message = paste0("Performing PCA with N=", nPCs),
                             detail = "...",
                             {
                                 pca.dudi <- dudi.pca(inputData$mat, center=input$center, scale=input$scale, scan=FALSE, nf=nPCs) 
                                 R2 <- pca.dudi$eig/sum(pca.dudi$eig) * 100
                                 loading <- pca.dudi$co
                                 colnames(loading) <- paste0("PC", 1:ncol(loading))
                                 if(plot){
                                     setProgress(value = 0.25, detail = "rendering figure legend ...")
                                     ## showNotification("Rendering figure legend ...");
                                     output$ctype <- DT::renderDataTable({
                                         input$example_data
                                         input$fileToUse
                                         cols <- getCols(inputData)
                                         if(is.null(cols)){
                                             NULL
                                         }else{
                                             out <- data.frame(Color=sprintf('<b style="color:%s">%s</b>',cols,cols), row.names=paste0(names(cols), ""))
                                             colnames(out) <- input$rowColor
                                             out
                                         }
                                     }, escape=FALSE, options = list(dom='t', ordering=FALSE))
                                     
                                     output$ptype <- DT::renderDataTable({
                                         input$example_data
                                         input$fileToUse
                                         pchs <- getPchs(inputData)
                                         if(is.null(pchs)){
                                             NULL
                                         }else{
                                             out <- data.frame(Point=pchs, row.names=paste0(names(pchs),"")) ## in case of NA
                                             colnames(out) <- input$rowPoint
                                             out
                                         }
                                     }, escape=FALSE, options = list(dom='t', ordering=FALSE))
                                     
                                     output$ftype <- DT::renderDataTable({
                                         input$example_data
                                         input$fileToUse
                                         cols <- getFeatureCols(inputData)$gcol
                                         out <- data.frame(Feature=sprintf('<b style="color:%s">%s</b>',cols,cols), row.names=names(cols))
                                     }, escape=FALSE, options = list(dom='t', ordering=FALSE))
                                     
                                     loading.pch <- 1
                                     if(!input$show_component_score && input$show_factor_loading){
                                         pcols <- NA
                                     }
                                     if(input$show_component_score && !input$show_factor_loading){
                                         loading.pch <- trait.cols <- NA
                                     }
                                     if(input$show_component_score && input$show_factor_loading){
                                         loading.pch <- NA
                                     }
                                     ncol <- 1 
                                     if(length(pchs) > 8){
                                         ncol <- 2
                                     }
                                     
                                     setProgress(value = 0.5, detail = "generating plot ...")
                                     if(para){
                                         ## layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
                                         pltd <- t(loading)
                                         annotation_col <- data.frame(Feature=factor(group))
                                         ann_colors <- list(Feature=gcols)
                                         pheatmap(pltd, border_color=NA, show_colnames=FALSE, cluster_rows=FALSE, 
                                                  annotation_col=annotation_col, annotation_colors=ann_colors, 
                                                  ylab="Loading score", sub="Loading score of features", 
                                                  main="\n\n\n\n\n\n\n\n\n\n\n\n\n")
                                         if(input$show_component_score){
                                             op <- par(fig=c(0, 0.4, 0.5, 1), new=TRUE, xpd = TRUE)
                                             msgPlot(msg="")
                                             my.legend(0.3, 0.75, xpd=TRUE, legend=names(pchs), ncol=ncol, pch=pchs, col="grey80", text.col='grey80', title=input$rowPoint)
                                             my.legend(0.3, 1.50, xpd=TRUE, legend=names(cols), ncol=ncol, col=cols, text.col=cols, pch=16, title=input$rowColor)
                                             par(op)
                                         }
                                         if(nPCs > 2){
                                             par(fig=c(0.35, 1, 0.2, 1), new=TRUE)
                                             pltd <- pca.dudi$li 
                                             scatter3D(pltd[,1], pltd[,2], pltd[,3], col=pcols, pch=ppchs, colkey=F, phi=input$phi, theta=input$theta, 
                                                       xlab="PC1", ylab="PC2", zlab="PC3", main="3D PCA")
                                         }
                                     }else{
                                         suppressWarnings(my.pca.plot(pca.dudi, sp=ppchs, sc=pcols, lp=loading.pch, lc=trait.cols, cex=1, lty=1))
                                         ## par(xpd = TRUE)
                                         if(input$show_factor_loading && length(gcols) > 1){
                                             my.legend(x=0, y=0.04, xpd=TRUE, ncol=length(gcols), legend=names(gcols), col=gcols, text.col=gcols, pch=NA, lty=1)
                                         }
                                         ## par(xpd = NA)
                                         op <- par(fig=c(0.5, 1, 0.5, 1), new=TRUE)
                                         col1 <- rep("grey80", length(R2))
                                         col1[1:nPCs] <- 'red'
                                         plot(R2, type='h', lwd=2, col=col1, xlab="PC", ylab="% variance", 
                                              main="% variance explained")
                                         points(1:length(R2), R2, col=col1, type='b')
                                         par(op)
                                     }
                                 }else{
                                     setProgress(value = 0.5, detail = "preparing data ...")
                                     return(list(R2=data.frame(PC=1:length(R2), R2=R2), loading=data.frame(Feature=rownames(loading), loading)))
                                 }
                             })
            }else if(type == "tsne"){ ## t-SNE
                ndims <- input$ndims
                    withProgress(value = 0,
                                 message = paste("Performing t-SNE with N =", ndims),
                                 detail = "...",
                                 {
                                     if(plot){
                                         ## set.seed(123)
                                         perplexity <- 30
                                         if(nrow(inputData$mat) < 100){
                                             perplexity <- ceiling(nrow(inputData$mat)/4)
                                         }
                                         tsne_out <- Rtsne(inputData$mat, dims=ndims, perplexity=perplexity, check_duplicates=F, verbose=F, max_iter=500, 
                                                           pca_center=input$center, pca_scale=input$scale)
                                         colnames(tsne_out$Y) <- paste0("t-SNE", 1:ncol(tsne_out$Y)) 
                                         
                                         setProgress(value = 0.5, detail = "making t-SNE graph ...")
                                         pltd <- tsne_out$Y
                                         if(para){
                                             pp <- function(){
                                                 if(ncol(pltd) > 2){
                                                     scatter3D(pltd[,1], pltd[,2], pltd[,3], phi=input$phi2, theta=input$theta2, col=pcols, pch=ppchs, colkey=F, xlab="t-SNE1", ylab="t-SNE2", zlab="t-SNE3", main="3D t-SNE")
                                                     if(length(pchs) > 1) my.legend("bottomleft", xpd=TRUE, legend=names(pchs), ncol=1, pch=pchs, col="grey80", text.col='grey80', title=input$rowPoint)
                                                     if(length(cols) > 1) my.legend("bottomright", xpd=TRUE, legend=names(cols), ncol=1, col=cols, text.col=cols, pch=16, title=input$rowColor)
                                                 }else{
                                                     msgPlot(msg = "Figure\nLegend")
                                                     if(length(pchs) > 1) my.legend("topleft", xpd=TRUE, legend=names(pchs), ncol=1, pch=pchs, col="grey80", text.col='grey80', title=input$rowPoint)
                                                     if(length(cols) > 1) my.legend("topright", xpd=TRUE, legend=names(cols), ncol=1, col=cols, text.col=cols, pch=16, title=input$rowColor)
                                                 }
                                             }
                                             png(paste0(mydir, "/", "t-SNE-3D.png"), width = 960, height = 960, pointsize = 24)
                                             pp()
                                             dev.off()
                                             pdf(paste0(mydir, "/", "t-SNE-3D.pdf"), width = 8.27, height = 8.27, pointsize = 10)
                                             pp()
                                             dev.off()
                                             svglite(paste0(mydir, "/", "t-SNE-3D.svg"), width = 8.27, height = 8.27, pointsize = 10)
                                             pp()
                                             dev.off()
                                             pp()
                                             
                                         }else{
                                             pp <- function(){
                                                 if(ncol(pltd) > 2){
                                                     pairs(pltd, col=pcols, pch=ppchs, main="t-SNE", upper.panel=NULL)
                                                     if(length(pchs) > 1) my.legend("topright", xpd=TRUE, legend=names(pchs), ncol=1, pch=pchs, col="grey80", text.col='grey80', title=input$rowPoint)
                                                     if(length(cols) > 1) my.legend("right", xpd=TRUE, legend=names(cols), ncol=1, col=cols, text.col=cols, pch=16, title=input$rowColor)
                                                 }else{
                                                     plot(pltd[,1:2], col=pcols, pch=ppchs, main="t-SNE")
                                                 }
                                             }
                                             png(paste0(mydir, "/", "t-SNE-plot.png"), width = 960, height = 960, pointsize = 24)
                                             pp()
                                             dev.off()
                                             pdf(paste0(mydir, "/", "t-SNE-plot.pdf"), width = 8.27, height = 8.27, pointsize = 10)
                                             pp()
                                             dev.off()
                                             svglite(paste0(mydir, "/", "t-SNE-plot.svg"), width = 8.27, height = 8.27, pointsize = 10)
                                             pp()
                                             dev.off()
                                             pp()
                                         }
                                     }else{
                                         return(NULL)
                                     }
                                 })
            }else if(type == "mds"){ ## MDS
                ndims <- input$nmds
                    withProgress(value = 0,
                                 message = paste("Performing MDS with N =", ndims),
                                 detail = "...",
                                 {
                                     if(plot){
                                         mat <- apply(inputData$mat, 2, function(x){scale(x, scale=input$scale, center=input$center)})
                                         ## all_norm <- normalize.quantiles(as.matrix(inputData$mat)) 
                                         ## mat <- t(apply(all_norm, 1, function(x){scale(x, scale=input$scale, center=input$center)}))
                                         dmth <- input$dist_method
                                         if(dmth == ""){
                                             dmth <- "euclidean"
                                         }
                                         pltd <- cmdscale(dist(mat, method=dmth), k=ndims)
                                         colnames(pltd) <- paste0("MDS", 1:ncol(pltd))
                                         
                                         setProgress(value = 0.5, detail = "making MDS graph ...")
                                         if(para){
                                             if(ncol(pltd) > 2){
                                                 scatter3D(pltd[,1], pltd[,2], pltd[,3], col=pcols, pch=ppchs, colkey=F, xlab="MDS1", ylab="MDS2", zlab="MDS3", phi=input$phi3, theta=input$theta3, main="3D MDS")
                                                 if(length(pchs) > 1) my.legend("bottomleft", xpd=TRUE, legend=names(pchs), ncol=1, pch=pchs, col="grey80", text.col='grey80', title=input$rowPoint)
                                                 if(length(cols) > 1) my.legend("bottomright", xpd=TRUE, legend=names(cols), ncol=1, col=cols, text.col=cols, pch=16, title=input$rowColor)
                                             }else{
                                                 msgPlot(msg = "Figure\nLegend")
                                                 if(length(pchs) > 1) my.legend("topleft", xpd=TRUE, legend=names(pchs), ncol=1, pch=pchs, col="grey80", text.col='grey80', title=input$rowPoint)
                                                 if(length(cols) > 1) my.legend("topright", xpd=TRUE, legend=names(cols), ncol=1, col=cols, text.col=cols, pch=16, title=input$rowColor)
                                             }
                                         }else{
                                             if(ncol(pltd) > 2){
                                                 pairs(pltd, col=pcols, pch=ppchs, main="MDS", upper.panel=NULL)
                                                 if(length(pchs) > 1) my.legend("topright", xpd=TRUE, legend=names(pchs), ncol=1, pch=pchs, col="grey80", text.col='grey80', title=input$rowPoint)
                                                 if(length(cols) > 1) my.legend("right", xpd=TRUE, legend=names(cols), ncol=1, col=cols, text.col=cols, pch=16, title=input$rowColor)
                                             }else{
                                                 plot(pltd[,1:2], col=pcols, pch=ppchs, main="MDS")
                                             }
                                         }
                                     }else{
                                         return(NULL)
                                     }
                                 })
            }else if(type == "som"){ ## SOM 
                xDim <- input$xdim
                yDim <- input$ydim 
                withProgress(value = 0,
                             message = paste0("Performing SOM with x-dim=", xDim, ", y-dim=", yDim),
                             detail = "...",
                             {
                                 if(plot){
                                     mycol <- function(n, alpha = 1) {
                                         colorRampPalette(rev(brewer.pal(n=9, name='RdYlBu')))(n)
                                     }
                                     mat <- apply(inputData$mat, 2, function(x){scale(x, scale=input$scale, center=input$center)})
                                     ## all_norm <- normalize.quantiles(as.matrix(inputData$mat)) 
                                     ## mat <- t(apply(all_norm, 1, function(x){scale(x, scale=input$scale, center=input$center)}))
                                     colnames(mat) <- colnames(inputData$mat)
                                     rownames(mat) <- rownames(inputData$mat)
                                     somobj <- list(profile=mat)
                                     set.seed(123456)
                                     somobj.supersom <- supersom(somobj, somgrid(xDim, yDim, input$topo), whatmap=1, 
                                                                 alpha=c(0.05,0.01), keep.data=TRUE) ## n.hood="circular", 
                                     cluster.labs <- somobj.supersom$unit.classif
                                     som.counts <- table(cluster.labs)
                                     som.labels <- paste0("Cluster ", names(som.counts), "\n(", som.counts, ")")
                                     stable <- table(pcols, cluster.labs)
                                     bgcol0 <- rownames(stable)[apply(stable, 2, which.max)] 
                                     bgcol1 <- paste0(bgcol0, "33")
                                     bgcol2 <- paste0(bgcol0, "99")
                                     
                                     setProgress(value = 0.5, detail = "making SOM graph ...")
                                     if(para){ ## SOM + PCA 
                                         op <- par(mfrow=c(2, 2), pty="m")
                                         nc <- length(som.counts)
                                         if(nc > 8){
                                             som.cols <- colorRampPalette(brewer.pal(n=8, name="Dark2"))(nc)
                                         }else{
                                             som.cols <- brewer.pal(n=nc, name="Dark2")
                                         }
                                         plot(somobj.supersom, type="mapping", col=som.cols[cluster.labs], pch=1, keepMargins=FALSE, bgcol=gray(0.95))
                                         text(somobj.supersom$grid$pts, labels=som.labels, cex=1)
                                         pie(som.counts, labels=som.labels, col=som.cols, border="white", main="Cluster colors")
                                         
                                         pcaobj <- dudi.pca(inputData$mat, center=input$center, scale=input$scale, scan=FALSE, nf=3)
                                         pca_score <- pcaobj$li
                                         pca_labs <- paste("PC", 1:pcaobj$nf, " (", round(pcaobj$eig/sum(pcaobj$eig)*100, 2), "%)", sep="")
                                         plot(pca_score[, 1:2], col=som.cols[cluster.labs], pch=1, 
                                              xlab=pca_labs[1], ylab=pca_labs[2], main='PCA with SOM clustering', 
                                              panel.first=abline(v=0, h=0, col='grey', lty=2))
                                         clusters <- sort(unique(cluster.labs), na.last=T)
                                         for(cluster in clusters){
                                             x <- pca_score[cluster.labs==cluster, 1]
                                             y <- pca_score[cluster.labs==cluster, 2]
                                             z <- (cluster.labs==cluster)+0
                                             text(median(x), median(y), labels=cluster, cex=2) #col=som.cols[cluster]
                                             if(length(x) > 10){
                                                 suppressWarnings(ade4:::scatterutil.ellipse(x, y, z, cellipse=1.6, axesell=FALSE, coul=som.cols[cluster]))
                                             }
                                         }
                                         
                                         sz <- inputData$nonNumericSize
                                         if(input$rowColor %in% names(sz)){
                                             stat <- table(cluster.labs, inputData$data[,input$rowColor])
                                             image(1:nrow(stat), 1:ncol(stat), stat, col=mycol(100), axes=FALSE, 
                                                   ylab=input$rowColor, xlab="Cluster",
                                                   main="#individuals in clusters")
                                             Map(function(x,y,z) 
                                                 axis(1, at=x, col.axis=y, labels=z, lwd=0, las=1),
                                                 1:nrow(stat),
                                                 som.cols,
                                                 rownames(stat)
                                             )
                                             Map(function(x,y,z) 
                                                 axis(2, at=x, col.axis=y, labels=z, lwd=0, las=2),
                                                 1:ncol(stat),
                                                 cols,
                                                 colnames(stat)
                                             )
                                             ## axis(1, at=1:nrow(stat), rownames(stat))
                                             ## axis(2, at=1:ncol(stat), colnames(stat), las=2)
                                             if(nrow(stat) < 10 && ncol(stat) < 10){
                                                 text(rep(1:nrow(stat),ncol(stat)), rep(1:ncol(stat), each=nrow(stat)), stat)
                                             }
                                         }
                                         par(op)
                                     }else{
                                         op <- par(mfrow=c(2, 2), pty="m")
                                         plot(somobj.supersom, type="mapping", cex=1.6, col=pcols, pch=ppchs, 
                                              main="SOM", keepMargins=FALSE, bgcol=bgcol1)
                                         plot(1, type="n", xaxt="n", yaxt="n", frame=F, xlab="", ylab="", main="Figure legend")
                                         if(length(pchs) > 1) my.legend("left", xpd=TRUE, legend=names(pchs), ncol=1, pch=pchs, col="grey80", text.col='grey80', title=input$rowPoint)
                                         if(length(cols) > 1) my.legend("right", xpd=TRUE, legend=names(cols), ncol=1, col=cols, text.col=cols, pch=16, title=input$rowColor)
                                         if(length(som.counts) > 1){
                                             plot(somobj.supersom, type="counts", palette.name=mycol, keepMargins=FALSE, bgcol=gray(0.95)) 
                                             text(somobj.supersom$grid$pts, labels=som.labels, cex=1)
                                         }
                                         sz <- inputData$nonNumericSize
                                         if(input$rowColor %in% names(sz)){
                                             plot(somobj.supersom, type="mapping", cex=1.2, 
                                                  main=paste0("Cluster colored by '", input$rowColor, "'"), 
                                                  keepMargins=FALSE, bgcol=bgcol2, palette.name=mycol)
                                             try(add.cluster.boundaries(somobj.supersom, as.integer(as.factor(bgcol1))), silent=TRUE)
                                         }
                                         par(op)
                                     } 
                                 }else{
                                     return(NULL)
                                 }
                             })
            }else if(type == "kmc"){ ## K-MC
                        withProgress(value = 0,
                                     message = "Performing K-means clusterings analysis",
                                     detail = "...",
                                     {
                                         if(plot){
                                             mat <- apply(inputData$mat, 2, function(x){scale(x, scale=input$scale, center=input$center)})
                                             ## all_norm <- normalize.quantiles(as.matrix(inputData$mat)) 
                                             ## mat <- t(apply(all_norm, 1, function(x){scale(x, scale=input$scale, center=input$center)}))
                                             colnames(mat) <- colnames(inputData$mat)
                                             rownames(mat) <- rownames(inputData$mat)
                                             if(para){ ## stat
                                                 setProgress(value = 0.5, detail = "statistics of K-means clustering analysis")
                                                 ## 
                                                 m0 <- NULL
                                                 if(input$rowColor != ""){
                                                     res.km <- eclust(mat, "kmeans", k=input$ncluster, graph=F, verbose=F)
                                                     clst <- paste0("C", res.km$cluster)
                                                     stable <- table(inputData$data[, input$rowColor], clst)
                                                     ## m0 <- ggtexttable(stable, theme = ttheme("lGreenWhite"))
                                                     m0 <- ggtexttable(stable,
                                                                       theme = ttheme(
                                                                           ## colnames.style = colnames_style(fill = "white"),
                                                                           tbody.style = tbody_style(fill=paste0(cols[rownames(stable)], "88"))
                                                                       )
                                                     )
                                                 }
                                                 
                                                 # Gap statistic 
                                                 set.seed(123)
                                                 m1 <- fviz_nbclust(mat, kmeans, method = "gap_stat") +
                                                     labs(subtitle = "Gap statistic method")
                                                 
                                                 # Elbow method
                                                 m2 <- fviz_nbclust(mat, kmeans, method = "wss") +
                                                     geom_vline(xintercept = 4, linetype = 2) +
                                                     labs(subtitle = "Elbow method")
                                                 # Silhouette method
                                                 m3 <- fviz_nbclust(mat, kmeans, method = "silhouette") +
                                                     labs(subtitle = "Silhouette method")
                                                 ## figure <- ggarrange(m0, m1, m2, labels = c("A", "B", "C"), ncol = 2, nrow = 2)
                                                 ## figure
                                                 
                                                 multiplot(m0, m1, m2, m3, cols=2)
                                             }else{
                                                 setProgress(value = 0.5, detail = "making graph ...")
                                                 res.km <- eclust(mat, "kmeans", k=input$ncluster, graph=F, verbose=F) 
                                                 pp <- fviz_cluster(res.km, geom='point', main="K-means clustering", pointsize=2, 
                                                                    ggtheme=theme_pubr(), shape=21, palette="Set1")
                                                 print(pp)
                                             } 
                                         }else{
                                             return(NULL)
                                         }
                                     })
            }else if(type == "hca"){ ## HCA 
                withProgress(value = 0,
                             message = "Performing hierarchical clustering analysis",
                             detail = "...",
                             {
                                 mat <- apply(inputData$mat, 2, function(x){scale(x, scale=input$scale, center=input$center)})
                                 ## all_norm <- normalize.quantiles(as.matrix(inputData$mat)) 
                                 ## mat <- t(apply(all_norm, 1, function(x){scale(x, scale=input$scale, center=input$center)}))
                                 colnames(mat) <- colnames(inputData$mat)
                                 rownames(mat) <- rownames(inputData$mat)
                                 if(para){
                                     setProgress(value = 0.25, detail = "calculating correlation of features ...") 
                                     mth <- input$cor_method2
                                     if(mth == "") {mth <- "spearman"}
                                     cors <- cor(mat, method=mth)
                                     setProgress(value = 0.5, detail = "generating graphs ...") 
                                     if(plot){
                                         if(ncol(mat) > 2500){
                                             showNotification("Number of features > 2500. Stop here. May consider other types of analysis.", type='warning');
                                             return(NULL)
                                         }
                                         gg <- paste0(group, "")
                                         names(gg) <- names(group)
                                         ann_col_row <- data.frame(Feature=factor(gg))
                                         ann_colors <- list(Feature=gcols) 
                                         color_space <- input$color_space2
                                         if(color_space == ""){
                                             color_space <- 'PiYG-rev'
                                         }
                                         if(length(grep("-rev", color_space))){
                                             color_space <- gsub("-rev", "", color_space)
                                             mycol <- colorRampPalette(rev(brewer.pal(n=9, name=color_space)))(100)
                                         }else{
                                             mycol <- colorRampPalette(brewer.pal(n=9, name=color_space))(100)
                                         }
                                         showlbs <- FALSE
                                         if(nrow(cors) < 15){
                                             showlbs <- TRUE
                                         }
                                         if(input$circle1){
                                             setProgress(value = 0.75, detail = "generating dendrogram of features ...") 
                                             phtm <- pheatmap(cors, plot=F)
                                             phylo_data <- as.phylo(phtm$tree_row)
                                             plot(phylo_data, type="fan", tip.color=gcols, 
                                                  main=paste0("Dendrogram of features, n=", nrow(cors)))
                                         }else{
                                             setProgress(value = 0.75, detail = "generating heatmap of features ...") 
                                             pheatmap(cors, col=mycol, border_color=NA, annotation_row=ann_col_row, 
                                                      annotation_col=ann_col_row, annotation_colors=ann_colors, 
                                                      show_colnames=showlbs, show_rownames=showlbs, 
                                                      main=paste0("Heatmap of features (columns), n=", nrow(cors)))
                                         }
                                     }else{
                                         setProgress(value = 0.75, detail = "exporting data ...") 
                                         cors
                                     }
                                 }else{
                                     if(both){
                                         if(plot){
                                             setProgress(value = 0.25, detail = "preparing heatmap data ...") 
                                             gg <- paste0(group, "")
                                             names(gg) <- names(group)
                                             ann_col <- data.frame("Feature.Group"=factor(gg))
                                             
                                             ann_row <- data.frame("Row.Group"=factor(names(hcols)))
                                             rownames(mat) <- rownames(ann_row) <- 1:nrow(mat)
                                             ann_colors <- list("Feature.Group"=gcols, "Row.Group"=cols)
                                             if(input$rowColor != ""){
                                                 colnames(ann_row) <- names(ann_colors)[2] <- input$rowColor
                                             }
                                             cluster_rows <- input$cluster_rows
                                             cluster_cols <- input$cluster_cols
                                             cutree_rows <- cutree_cols <- NA
                                             if(input$cutree_rows > 1){
                                                 cutree_rows <- input$cutree_rows
                                             }
                                             if(input$cutree_cols > 1){
                                                 cutree_cols <- input$cutree_cols
                                             }
                                             setProgress(value = 0.25, detail = "generating heatmap of the whole dataset ...") 
                                             color_space <- input$color_space3
                                             if(color_space == ""){
                                                 color_space <- 'RdYlBu-rev'
                                             }
                                             if(length(grep("-rev", color_space))){
                                                 color_space <- gsub("-rev", "", color_space)
                                                 mycol <- colorRampPalette(rev(brewer.pal(n=9, name=color_space)))(100)
                                             }else{
                                                 mycol <- colorRampPalette(brewer.pal(n=9, name=color_space))(100)
                                             }
                                             showrlbs <- showclbs <- FALSE
                                             if(nrow(mat) < 15){
                                                 showrlbs <- TRUE
                                             }
                                             if(ncol(mat) < 15){
                                                 showclbs <- TRUE
                                             }
                                             pheatmap(mat, col=mycol, border_color=NA, annotation_row=ann_row, 
                                                      annotation_col=ann_col, annotation_colors=ann_colors, 
                                                      show_rownames=showrlbs, show_colnames=showclbs, 
                                                      cutree_rows=cutree_rows, cutree_cols=cutree_cols, 
                                                      cluster_rows=cluster_rows, cluster_cols=cluster_cols, 
                                                      main=paste0("#rows=", nrow(mat), ", #columns=", ncol(mat)))
                                         }else{
                                             setProgress(value = 0.75, detail = "exporting data ...") 
                                             mat
                                         }
                                     }else{
                                         if(nrow(mat) > 1000){
                                             showNotification("Number of individuals > 1000. Stop here. May consider other types of analysis.", type='warning');
                                             return(NULL)
                                         }
                                         setProgress(value = 0.25, detail = "calculating correlation of individuals ...")
                                         mth <- input$cor_method
                                         if(mth == "") {mth <- "spearman"}
                                         cors <- cor(t(mat), method=mth)
                                         setProgress(value = 0.5, detail = "generating graphs ...") 
                                         if(plot){
                                             ann_col_row <- data.frame(Group=factor(names(hcols)))
                                             rownames(ann_col_row) <- rownames(cors) <- colnames(cors) <- 1:nrow(cors)
                                             ann_colors <- list(Group=cols)
                                             if(input$rowColor != ""){
                                                 colnames(ann_col_row) <- names(ann_colors) <- input$rowColor
                                             }
                                             color_space <- input$color_space
                                             if(color_space == ""){
                                                 color_space <- 'RdYlBu-rev'
                                             }
                                             if(length(grep("-rev", color_space))){
                                                 color_space <- gsub("-rev", "", color_space)
                                                 mycol <- colorRampPalette(rev(brewer.pal(n=9, name=color_space)))(100)
                                             }else{
                                                 mycol <- colorRampPalette(brewer.pal(n=9, name=color_space))(100)
                                             }
                                             showlbs <- FALSE
                                             if(nrow(cors) < 15){
                                                 showlbs <- TRUE
                                             }
                                             if(input$circle){
                                                 setProgress(value = 0.75, detail = "generating dendrogram of individuals ...") 
                                                 phtm <- pheatmap(cors, plot=F)
                                                 phylo_data <- as.phylo(phtm$tree_row)
                                                 plot(phylo_data, type="fan", tip.color=pcols, 
                                                      main=paste0("Dendrogram of individuals, n=", nrow(cors)))
                                             }else{
                                                 setProgress(value = 0.75, detail = "generating heatmap of individuals ...") 
                                                 pheatmap(cors, col=mycol, border_color=NA, annotation_row=ann_col_row, 
                                                          annotation_col=ann_col_row, annotation_colors=ann_colors, 
                                                          show_colnames=showlbs, show_rownames=showlbs, 
                                                          main=paste0("Heatmap of individuals (rows), n=", nrow(cors)))
                                             }
                                         }else{
                                             setProgress(value = 0.75, detail = "exporting data ...") 
                                             cors
                                         }    
                                     }
                                 }
                             })
            }else if(type == "misc"){ ## misc 
                data <- inputData$data 
                colnames(data) <- nospecial(colnames(data))
                ps <- unlist(strsplit(input$misct, split="\\|"))
                color <- nospecial(input$rowColor)
                fill <- color
                if(color == ""){
                    fill <- NA
                    color <- 'orange'
                }
                x <- nospecial(input$miscx)
                y <- nospecial(input$miscy)
                z <- nospecial(input$miscz)
                m <- nospecial(input$miscm)
                if(ps[1] == "1"){ ## Plot One Variable: Continuous Y
                    enable("miscy")
                    disable("miscx")
                    disable("miscz")
                    disable("miscm")
                    if(!is.nothing(y)){
                        pltfun <- get(ps[2])
                        p <- pltfun(data, x=y, add="mean", rug=TRUE, color=color, fill=fill, palette=cols)
                        print(p)
                    }else{
                        msgPlot()
                    }
                }else if(ps[1] == "2"){ ## Plot Two Variables: Discrete X & Continuous Y
                    enable("miscx")
                    enable("miscy")
                    disable("miscz")
                    disable("miscm")
                    if(!is.nothing(x) && !is.nothing(y)){
                        pltfun <- get(ps[2])
                        p <- pltfun(data, x, y, color=color, palette=cols, add=c("mean_se", "jitter")) + 
                            labs(y=input$miscy, x=input$miscx)
                        print(p)
                    }else{
                        showNotification("X or Y is missing.", type='error')
                        msgPlot()
                    }
                }else if(ps[1] == "3"){
                    enable("miscy")
                    enable("miscz")
                    disable("miscx")
                    disable("miscm")
                    if(y == z){
                        # showModal(modalDialog(
                        #     title = "Warning",
                        #     "Y and Z are the same column.",
                        #     fade = FALSE
                        # ))
                        # msgPlot()
                        showNotification("Y and Z are the same column.", type='error')
                    }else{
                        if(!is.nothing(y) && !is.nothing(z)){
                            if(ps[2]=="ggscatterhist"){
                                p <- ggscatterhist(data, x=y, y=z, color=color, size=3, alpha=0.6,
                                                   palette=cols, margin.params=list(fill=fill, color="black", size=0.2)
                                )
                            }else if(ps[2]=="stat_cor"){
                                p <- ggscatter(data, x=y, y=z, color=color, palette=cols,
                                               add="reg.line", conf.int = TRUE) + 
                                    stat_cor(aes_string(color=color))
                            }else if(ps[2]=="stat_stars"){
                                p <- ggscatter(data, x=y, y=z, color=color, shape=color, palette=cols, 
                                               mean.point=TRUE, ellipse=TRUE, star.plot = TRUE) 
                                ## + stat_stars(aes_string(color=color))
                            }else {
                                p <- ggscatterhist(data, x=y, y=z, color=color, alpha=0.6,
                                                   margin.params=list(color=fill, size=0.2), 
                                                   palette=cols, margin.plot="boxplot", ggtheme=theme_bw()
                                )
                            }
                            p <-  p + labs(x=input$miscy, y=input$miscz)
                            print(p)
                        }else{
                            showNotification("Y or Z is missing.", type='error')
                            msgPlot()
                        }
                    }
                }else if(ps[1] == "m"){
                    disable("miscy")
                    enable("miscm")
                    disable("miscx")
                    disable("miscz")
                    if(!is.nothing(m)){
                        yy <- factor(rep("All", nrow(data)))
                        if(input$rowColor != ""){
                            yy <- as.factor(data[, input$rowColor])
                        }
                        if(ps[2]=="box"){
                            transparentTheme(trans = .9)
                            featurePlot(x=data[, m], y=yy, plot="box", col=cols,
                                        scales=list(y=list(relation="free"), x=list(rot=90)),
                                        # layout = c(4, 1), 
                                        auto.key=list(columns=4))
                        }else if(ps[2]=="density"){
                            ## transparentTheme(trans = .9)
                            featurePlot(x=data[, m], y=yy, plot="density",
                                        scales=list(x=list(relation="free"), 
                                                    y=list(relation="free")), 
                                        adjust=1.5, pch="|", col=cols,  
                                        ## layout = c(4, 1), 
                                        auto.key=list(columns=4))
                        }else if(ps[2]=="pairs"){
                            transparentTheme(trans = .9)
                            featurePlot(x=data[, m], y=yy, plot="pairs", col=paste0(cols, "aa"),
                                        auto.key=list(columns=4))
                        }else{
                            transparentTheme(trans = .9)
                            featurePlot(x=data[, m], y=yy, plot="ellipse", col=paste0(cols, "aa"),
                                        auto.key=list(columns=4))
                        }
                    }else{
                        showNotification("Y or M is missing.", type='error')
                        msgPlot()
                    }
                }else if(ps[1] == "o"){
                    disable("miscx")
                    disable("miscz")
                    enable("miscy")
                    enable("miscm")
                    if(!is.nothing(y) && !is.nothing(m)){
                        theme1 <- trellis.par.get()
                        theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
                        theme1$plot.symbol$pch = 16
                        theme1$plot.line$col = rgb(1, 0, 0, .7)
                        theme1$plot.line$lwd <- 2
                        trellis.par.set(theme1)
                        featurePlot(x=data[, m], y=data[, y], col=pcols,  
                                    ## layout = c(3, 1), 
                                    plot="scatter",
                                    type=c("p", "smooth"),
                                    span=.5)
                    }
                }else{
                    msgPlot()
                }
            }else{
                showNotification("Unknow plot.", type="error")
                msgPlot()
                return(NULL)
            }
        }else{
            showNotification(paste0("Plot (type=", type, ", para=", para, ") will be generated. Please wait ..."))
            msgPlot()
        }
    }
    
    output$ppca <- renderPlot(
        runAnalysis(para = T)
    )
    
    output$mpca <- renderPlot(
        runAnalysis()
    )
    
    output$msom <- renderPlot(
        runAnalysis(type = 'som')
    )
    
    output$psom <- renderPlot(
        runAnalysis(type = 'som', para = T)
    )
    
    output$mkmc <- renderPlot(
        runAnalysis(type = 'kmc')
    )
    
    output$pkmc <- renderPlot(
        runAnalysis(type = 'kmc', para = T)
    )
    
    output$hca1 <- renderPlot(
        runAnalysis(type = 'hca')
    )
    
    output$hca2 <- renderPlot(
        runAnalysis(type = 'hca', para = T)
    )
    
    output$hca3 <- renderPlot(
        runAnalysis(type = 'hca', both = T)
    )
    
    output$miscplot <- renderPlot(
        runAnalysis(type = 'misc')
    )
    
    output$tsne1 <- renderPlot(
        runAnalysis(type = 'tsne')
    )
    
    output$tsne2 <- renderPlot(
        runAnalysis(type = 'tsne', para = T)
    )
    
    output$mds1 <- renderPlot(
        runAnalysis(type = 'mds')
    )
    
    output$mds2 <- renderPlot(
        runAnalysis(type = 'mds', para = T)
    )
    
    output$downloadMpcasvg <- downloadHandler(
        "PCA_plot.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            runAnalysis()
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadMpcapng <- downloadHandler(
        "PCA_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis()
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadMpcapdf <- downloadHandler(
        "PCA_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis()
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadSOMsvg <- downloadHandler(
        "SOM_plot.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            runAnalysis(type = 'som')
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadSOMpng <- downloadHandler(
        "SOM_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'som')
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadSOMpdf <- downloadHandler(
        "SOM_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'som')
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadsSOMsvg <- downloadHandler(
        "SOM_stat_plot.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            runAnalysis(type = 'som', para = T)
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadsSOMpng <- downloadHandler(
        "SOM_stat_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'som', para = T)
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadsSOMpdf <- downloadHandler(
        "SOM_stat_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'som', para = T)
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadMkmcsvg <- downloadHandler(
        "K-means_plot.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            runAnalysis(type = 'kmc')
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadMkmcpng <- downloadHandler(
        "K-means_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'kmc')
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadMkmcpdf <- downloadHandler(
        "K-means_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'kmc')
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadPkmcsvg <- downloadHandler(
        "K-means_stat_plot.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            runAnalysis(type = 'kmc', para = T)
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadPkmcpng <- downloadHandler(
        "K-means_stat_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'kmc', para = T)
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadPkmcpdf <- downloadHandler(
        "K-means_stat_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'kmc', para = T)
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    # output$downloadPpcasvg <- downloadHandler(
    #     "PCA_para_plot.svg",
    #     content = function(file) {
    #         svglite(file,
    #                 width = 8.27,
    #                 height = 8.27,
    #                 pointsize = 10)
    #         runAnalysis(para = T)
    #         dev.off()
    #     },
    #     contentType = "image/svg"
    # )
    
    output$downloadPpcapng <- downloadHandler(
        "PCA_para_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(para = T)
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadPpcapdf <- downloadHandler(
        "PCA_para_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(para = T)
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadPpcaVar <- downloadHandler(
        "PCA_para_explained_variance.txt",
        content = function(file) {
            data <- runAnalysis(plot = F)$R2
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
    
    output$downloadPpcaLoading <- downloadHandler(
        "PCA_para_loading_score.txt",
        content = function(file) {
            data <- runAnalysis(plot = F)$loading
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
    
    output$downloadHtmp1tab <- downloadHandler(
        "Heatmap_individual_corr.txt",
        content = function(file) {
            data <- runAnalysis(type = 'hca', plot = F)
            write.table(
                data,
                file = file,
                row.names = T,
                quote = F,
                sep = "\t"
            )
        },
        contentType = "text/tsv"
    )
    
    output$downloadHtmp1png <- downloadHandler(
        "Heatmap_individual_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'hca')
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadHtmp1pdf <- downloadHandler(
        "Heatmap_individual_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'hca')
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadHtmp2tab <- downloadHandler(
        "Heatmap_feature_corr.txt",
        content = function(file) {
            data <- runAnalysis(type = 'hca', plot = F, para = T)
            write.table(
                data,
                file = file,
                row.names = T,
                quote = F,
                sep = "\t"
            )
        },
        contentType = "text/tsv"
    )
    
    output$downloadHtmp2png <- downloadHandler(
        "Heatmap_feature_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'hca', para = T)
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadHtmp2pdf <- downloadHandler(
        "Heatmap_feature_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'hca', para = T)
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadHtmp3tab <- downloadHandler(
        "Heatmap_whole_data.txt",
        content = function(file) {
            data <- runAnalysis(type = 'hca', plot = F, both = T)
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
    
    output$downloadHtmp2png <- downloadHandler(
        "Heatmap_whole_data.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'hca', both = T)
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadHtmp3pdf <- downloadHandler(
        "Heatmap_whole_data.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'hca', both = T)
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadmiscsvg <- downloadHandler(
        "Misc_plot.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            runAnalysis(type = 'misc')
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadmiscpng <- downloadHandler(
        "Misc_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'misc')
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadmiscpdf <- downloadHandler(
        "Misc_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'misc')
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadtsne1svg <- downloadHandler(
        "t-SNE_plot.svg",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "t-SNE-plot.svg"), file)
        },
        contentType = "image/svg"
    )
    
    output$downloadtsne1png <- downloadHandler(
        "t-SNE_plot.png",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "t-SNE-plot.png"), file)
        },
        contentType = "image/png"
    )
    
    output$downloadtsne1pdf <- downloadHandler(
        "t-SNE_plot.pdf",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "t-SNE-plot.pdf"), file)
        },
        contentType = "image/pdf"
    )
    
    output$downloadtsne2svg <- downloadHandler(
        "t-SNE_3D.svg",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "t-SNE-3D.svg"), file)
        },
        contentType = "image/svg"
    )
    
    output$downloadtsne2png <- downloadHandler(
        "t-SNE_3D.png",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "t-SNE-3D.png"), file)
        },
        contentType = "image/png"
    )
    
    output$downloadtsne2pdf <- downloadHandler(
        "t-SNE_3D.pdf",
        content <- function(file) {
            file.copy(paste0(mydir, "/", "t-SNE-3D.pdf"), file)
        },
        contentType = "image/pdf"
    )
    
    output$downloadmds1svg <- downloadHandler(
        "MDS_plot.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            runAnalysis(type = 'mds')
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadmds1png <- downloadHandler(
        "MDS_plot.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'mds')
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadmds1pdf <- downloadHandler(
        "MDS_plot.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'mds')
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadmds2svg <- downloadHandler(
        "MDS_plot2.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            runAnalysis(type = 'mds', para = T)
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadmds2png <- downloadHandler(
        "MDS_plot2.png",
        content = function(file) {
            png(file,
                width = 960, 
                height = 960, 
                pointsize = 24)
            runAnalysis(type = 'mds', para = T)
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadmds2pdf <- downloadHandler(
        "MDS_plot2.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            runAnalysis(type = 'mds', para = T)
            dev.off()
        },
        contentType = "image/pdf"
    )
})
