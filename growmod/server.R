library(readr)
library(svglite)
library(cba)
##library(doMC)
##registerDoMC(cores=5)
library(BiocParallel)
source('code/growth.models.v2.R')

options(shiny.maxRequestSize = 10 * 1024 ^ 2) # max file size, 10Mb
if(!exists("workers")){
    workers <- 10
}
# if(Sys.info()["sysname"] == "Windows"){
#     param <- SnowParam(workers = workers, type="SOCK")
# }
param <- MulticoreParam(workers=workers, stop.on.error=F, timeout=432000)
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

requiredFieds <- c("plant", "group", "day")

shinyServer(function(input, output, session) {
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
        paste0("example_growth_data.csv"),
        content = function(file) {
            file.copy(paste0("data/",input$example_data,".csv"), file)
        },
        contentType = "text/csv"
    )
    
    myValidation <- function() {
        validate(
            need(
                !is.null(input$inputFile) |
                    input$fileToUse == "demo",
                "Upload your file, or Use the example data."
            )
        )
    }
    
    output$uiModels <- renderUI({
        selectizeInput(
            "models",
            "3.2. Models (empty to select all)",
            choices = setNames(as.character(models[models$category == input$category, "id"]),
                               as.character(models[models$category == input$category, "name"])),
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
                                 read_delim(paste0("data/", input$example_data, ".csv"),
                                            delim = ",", col_names = T)
                             setProgress(value = 0.5, detail = "checking file format ...")
                             validate(
                                 need(
                                     all(requiredFieds %in% colnames(inputData)) && ncol(inputData) > 3,
                                     "Incorrect input file format. Please ask the administrator. "
                                 )
                             )
                             setProgress(value = 1, detail = "done!")
                         })
            if (!all(requiredFieds %in% colnames(inputData))) {
                inputData <- NULL
            }
            if(!grepl("_stress", input$example_data)){
                pred.time <- plants <- days <- NULL
                if (!is.null(inputData)) {
                    pred.time <-
                        seq(min(inputData$day), ceiling(max(inputData$day) * 1.1))
                    days <-
                        na.omit(unique(round(inputData$day)))
                    plants <- unique(as.character(inputData$plant))
                }
                updateRadioButtons(session, "category", selected = "normal")
                return(list(
                    data = inputData,
                    days = days,
                    plants = plants,
                    pred.time = pred.time
                ))
            }else{
                pred.time <- plants <- days <- NULL
                if (!is.null(inputData)) {
                    pred.time <-
                        seq(min(inputData$day), ceiling(max(inputData$day) * 1.1))
                    days <-
                        na.omit(unique(round(inputData$day)))
                    plants <- unique(as.character(inputData$plant))
                }
                updateRadioButtons(session, "category", selected = "stressed")
                return(list(
                    data = inputData,
                    days = days,
                    from = 27, 
                    to = 45, 
                    plants = plants,
                    pred.time = pred.time
                ))
            }
        }
        
        inputFileName <- input$inputFile
        if (is.null(inputFileName)) {
            inputData <- NULL
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
                                     all(requiredFieds %in% colnames(inputData)) && ncol(inputData) > 3,
                                     "Incorrect input file format. Plase check if the header is correct or you choose a right separator."
                                 )
                             )
                             setProgress(value = 1, detail = "done!")
                         })
        }
        if (!all(requiredFieds %in% colnames(inputData))) {
            inputData <- NULL
        }
        pred.time <- plants <- days <- NULL
        if (!is.null(inputData)) {
            pred.time <-
                seq(min(inputData$day), ceiling(max(inputData$day) * 1.1))
            days <-
                na.omit(unique(round(sort(
                    inputData$day
                ))))
            plants <- unique(as.character(inputData$plant))
        }
        return(list(
            data = inputData,
            days = days,
            plants = plants,
            pred.time = pred.time
        ))
    })
    
    myValidation2 <- function(){
        inputData <- inputDataAnalysis()$data
        validate(
            need(input$measurement %in% colnames(inputData),
                 "Check your input data ..."
            )
        )
    }
    
    output$uiStressedPeriod <- renderUI({
        inputData <- inputDataAnalysis()
        days <- inputData$days
        if (!is.null(days) && input$category == "stressed") {
            shinyjs::show("div_highlight")
            if(!is.null(inputData$from) && !is.null(inputData$to)){
                dayRange <- c(inputData$from, inputData$to)
            }else{
                dayRange <- range(days, na.rm = T)
                dayRange <- round(quantile(dayRange, probs = c(
                    0.25, 0.75
                )))
            }
            sliderInput(
                "stressedPeriod",
                "3.3. Stressed period",
                min = min(days),
                max = max(days),
                value = dayRange
            )
        } else{
            shinyjs::hide("div_highlight")
        }
    })
    
    observe({
        ## cat(class(inputDataAnalysis()$data))
        if (is.null(inputDataAnalysis()$data)) {
            shinyjs::hide("div_step2")
            shinyjs::hide("div_step3")
            shinyjs::hide("div_step4")
        } else {
            shinyjs::show("div_step2")
            shinyjs::show("div_step3")
            shinyjs::show("div_step4")
            output$uiMeasurement <- renderUI({
                ## TODO: determine based on class 
                ## specs <- spec(inputDataAnalysis()$data)
                ## isNum <- unlist(lapply(specs$cols, function(x){"collector_integer" %in% class(x) | "collector_double" %in% class(x)}))
                ## growthMeasures <- setdiff(names(isNum)[isNum], requiredFieds)
                growthMeasures <-
                    setdiff(colnames(inputDataAnalysis()$data),
                            requiredFieds)
                selectizeInput(
                    "measurement",
                    "Which measurement for modeling?",
                    choices = setNames(growthMeasures, growthMeasures)
                )
            })
        }
    })
    
    output$inputData <- DT::renderDataTable({
        myValidation()
        inputDataAnalysis()$data
    })
    
    output$downloadInputData <- downloadHandler(
        "modeling_data_file.txt",
        content = function(file) {
            write.table(
                inputDataAnalysis()$data,
                file = file,
                row.names = F,
                quote = F,
                sep = "\t"
            )
        },
        contentType = "text/tsv"
    )
    
    plantDataSummay <- reactive({
        inputData <- inputDataAnalysis()$data
        if(is.null(inputData)){
            return(NULL)
        }
        if (!is.null(input$measurement) &&
            input$measurement != "") {
            statData <-
                aggregate(inputData[, input$measurement], inputData[, c("plant", "group")], function(x) {
                    paste(length(na.omit(x)), "/", length(x))
                })
            colnames(statData)[3] <- "# points<br/>valid / total"
            return(statData)
        }
    })
    
    growthModeling <-
        function(modata,
                 id,
                 absmax,
                 days = na.omit(unique(round(sort(modata$day)))),
                 pred.time = seq(floor(min(modata$day)), ceiling(max(modata$day) *
                                                                     1.1)),
                 plot = T,
                 show.legend = T) {
            iplants <- unique(as.character(modata$plant))
            datay <- modata$trait
            datat <- modata$day
            if (any(datay < 0, na.rm = T)) {
                if (plot) {
                    msgPlot("Growth values cannot be negative!", add = T)
                }
                return(NULL)
            }
            withProgress(
                value = 1,
                message = "Modeling ...",
                detail = paste(id, " ..."),
                {
                    if (plot) {
                        if (is.null(input$xlabel) ||
                            trimws(input$xlabel) == "") {
                            xlabel <- 'Day'
                        } else{
                            xlabel <- input$xlabel
                        }
                        if (is.null(input$ylabel) ||
                            trimws(input$ylabel) == "") {
                            ylabel <- input$measurement
                        } else{
                            ylabel <- input$ylabel
                        }
                        xrange <-
                            range(0, pred.time, na.rm = T)
                        yrange <-
                            range(0, datay * 1.1, na.rm = T)
                        
                        plot(
                            xrange,
                            yrange,
                            type = "n",
                            xlab = xlabel,
                            ylab = ylabel,
                            main = id
                        )
                        if (input$category == "stressed") {
                            if (input$highlight) {
                                abline(
                                    v = input$stressedPeriod,
                                    lty = 5,
                                    col = 'lightgrey'
                                )
                            }
                        }
                        
                        if (input$showpoints) {
                            cols <- rainbow(length(iplants))
                            if (length(iplants) == 1)
                                cols[1] <- "#005a32"
                            ltys <- 1:length(iplants)
                            pchs <- c(16, 9:15, 17:25, 0:8)
                            pchs <-
                                pchs[{
                                    ind <- 1:length(iplants) %% 26
                                    ind[ind == 0] <- 26
                                    ind
                                }]
                            names(cols) <-
                                names(ltys) <-
                                names(pchs) <- iplants
                            if(length(iplants) < 10){
                                for (p in iplants) {
                                    rid <- which(modata$plant == p)
                                    lines((modata[rid, ])$day,
                                          (modata[rid, ])$trait,
                                          type = "b",
                                          lty = ltys[p],
                                          col = cols[p],
                                          pch = pchs[p]
                                    )
                                }
                            }else{
                                points(modata$day, modata$trait, col='grey80')
                            }
                            if (length(iplants) > 1 && length(iplants) < 10) {
                                my.legend(
                                    "bottomleft",
                                    legend = iplants,
                                    cex = 0.8,
                                    col = cols,
                                    text.col = cols,
                                    pch = pchs,
                                    lty = ltys,
                                    title = paste("Plants of \"", id, "\"", sep = "")
                                )
                            }
                        }
                    }
                    if (input$category == "stressed") {
                        stressedPhase <- input$stressedPeriod
                        phase1 <- which(round(datat) >= stressedPhase[1]-5 
                                        & round(datat) <= stressedPhase[2])
                        phase2 <- which(round(datat) > stressedPhase[2])
                        datat1 <- datat[phase1]
                        datat2 <- datat[phase2]
                        datay1 <- datay[phase1]
                        datay2 <- datay[phase2]
                        pred.time1 <-
                            pred.time[pred.time <= stressedPhase[2]]
                        pred.time2 <-
                            pred.time[pred.time > stressedPhase[2]]
                        tmax.obs <- datat1[which.max(datay1)]
                    } else{
                        datay[datay == 0] <- NA
                        datay <-
                            datay / absmax ## scale the data to [0, 1]
                    }
                    if (sum(!is.na(datay)) < input$minpoints) {
                        if (plot) {
                            msgPlot("No enough data for modeling!", add = T)
                        }
                        setProgress(value = 1, detail = "warning!")
                        return(NULL)
                    }
                    if (input$checkdata) {
                        ## check if the data points cover most of the time scale
                        if (input$category == "stressed") {
                            if (##all(datat1 >= tmax.obs, na.rm = TRUE) ||
                                ##all(datat1 <= tmax.obs, na.rm = TRUE) ||
                                sum(!is.na(datay1)) < 4 ||
                                sum(!is.na(datay2)) < 4) {
                                if (plot) {
                                    msgPlot("Data look incomplete!", add = T)
                                }
                                setProgress(value = 1, detail = "warning!")
                                return(NULL)
                            }
                        } else{
                            days.quant <-
                                quantile(days, probs = c(1 / 4, 3 / 4))
                            first.check.point <- days.quant[1]
                            last.check.point <- days.quant[2]
                            compl.time <- na.omit(modata)$day
                            if (all(compl.time > first.check.point, na.rm =
                                    T) |
                                all(compl.time < last.check.point, na.rm =
                                    T)) {
                                if (plot) {
                                    msgPlot("Data look incomplete!", add = T)
                                }
                                setProgress(value = 1, detail = "warning!")
                                return(NULL)
                            }
                        }
                    }
                    
                    if (is.nothing(input$models) ||
                        is.nothing(trimws(input$models))) {
                        useModels <- models[models$category == input$category, "id"]
                    } else{
                        useModels <- input$models
                    }
                    if (input$category == "stressed") {
                        lgds <-
                            vector('expression', length(useModels))
                        ltys <-
                            cols <-
                            mnames <- rep(NA, length(useModels))
                        names(lgds) <-
                            names(ltys) <-
                            names(cols) <-
                            names(mnames) <- useModels
                    } else{
                        lgds <-
                            vector('expression', length(useModels) + 1)
                        ltys <-
                            cols <-
                            mnames <- rep(NA, length(useModels) + 1)
                        names(lgds) <-
                            names(ltys) <-
                            names(cols) <-
                            names(mnames) <- c(useModels, 'linear2')
                    }
                    
                    modpara <- list()
                    pdata <- data.frame()
                    for (m in useModels) {
                        if (input$category == "stressed") {
                            my <- datay1
                            mx <- datat1
                            px <- pred.time1
                        } else{
                            my <- datay
                            mx <- datat
                            px <- pred.time
                        }
                        modout <- model.list[[m]](my, mx, px)
                        if (plot) {
                            if (input$category == "stressed") {
                                lines(
                                    px,
                                    modout$prediction,
                                    lwd = 1.5,
                                    lty = modout$lty,
                                    col = modout$color
                                )
                            } else{
                                lines(
                                    px,
                                    modout$prediction * absmax,
                                    lwd = 1.5,
                                    lty = modout$lty,
                                    col = modout$color
                                )
                            }
                        }
                        lgds[m] <- modout$legend
                        ltys[m] <- modout$lty
                        cols[m] <- modout$color
                        mnames[m] <- modout$name
                        modpara[[m]] <- modout$para
                        pdata <- rbind(
                            pdata,
                            data.frame(
                                model = modout$name,
                                R = modout$para[['R']],
                                R2 = modout$para[['R2']],
                                RMSRE = modout$para[['RMSRE']],
                                color = modout$color,
                                lty = modout$lty,
                                stringsAsFactors = F
                            )
                        )
                    }
                    ## recovery with linear growth
                    if (input$category == "stressed") {
                        my <- datay2
                        mx <- datat2
                        px <- pred.time2
                        if (sum(!is.na(my)) >= 3) {
                            modout <- linear.model(my, mx, px)
                            if (plot) {
                                lines(
                                    px,
                                    modout$prediction,
                                    lwd = 1.5,
                                    lty = modout$lty,
                                    col = modout$color
                                )
                            }
                            m <- "linear2"
                            lgds[m] <- modout$legend
                            ltys[m] <- modout$lty
                            cols[m] <- modout$color
                            mnames[m] <-
                                paste(modout$name, "(recovery)")
                            modpara[[m]] <- modout$para
                            pdata <- rbind(
                                pdata,
                                data.frame(
                                    model = paste(modout$name, "(recovery)"),
                                    R = modout$para[['R']],
                                    R2 = modout$para[['R2']],
                                    RMSRE = modout$para[['RMSRE']],
                                    color = modout$color,
                                    lty = modout$lty,
                                    stringsAsFactors = F
                                )
                            )
                        }
                    }
                    if (plot && show.legend) {
                        my.legend(
                            "topleft",
                            legend = lgds,
                            cex = 0.8,
                            col = cols,
                            text.col = cols,
                            lty = ltys,
                            title = "Models"
                        )
                    }
                    setProgress(value = 1, detail = "done!")
                    return(
                        list(
                            base = absmax,
                            data = modata,
                            model = useModels,
                            parameter = modpara,
                            name = mnames,
                            color = cols,
                            pdata = pdata ## data for ploting
                        )
                    )
                }
            )
        }
    
    plantGowthModeling <- function(group = F) {
        myValidation2()
        if (group) {
            s <- input$groupGrowthData_rows_selected
        } else{
            s <- input$plantGrowthData_rows_selected
        }
        if (!is.null(input$measurement) &&
            input$measurement != "") {
            dataset <- inputDataAnalysis()
            inputData <- dataset$data
            if(is.null(s) && nrow(inputData) > 0){
                s <- 1 
            }
            if (group) {
                statData <- groupDataSummay()
                id <- statData[s, 'group']
                modata <-
                    inputData[inputData$group == id, c("plant", "day", input$measurement)]
            } else{
                statData <- plantDataSummay()
                id <- statData[s, 'plant']
                modata <-
                    inputData[inputData$plant == id, c("plant", "day", input$measurement)]
            }
            colnames(modata) <- c("plant", "day", "trait")
            absmax <-
                max(abs(inputData[, input$measurement]), na.rm = T)
            growthModeling(modata,
                           id,
                           absmax,
                           dataset$days,
                           dataset$pred.time)
        } else{
            msgPlot()
        }
    }
    
    output$plantGrowthData <- DT::renderDataTable({
        myValidation()
        myValidation2()
        plantDataSummay()
    }, server = T, selection = 'single', escape = F,
    caption = 'Data summary at plant level')
    
    output$plantGrowthPlot = renderPlot({
        ## shinyjs::show("div_step4")
        myValidation()
        plantGowthModeling()
    })
    
    output$downloadPlantpng <- downloadHandler(
        "plant_growth_curve.png",
        content = function(file) {
            png(file,
                width = 960,
                height = 960,
                pointsize = 24)
            plantGowthModeling()
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadPlantpdf <- downloadHandler(
        "plant_growth_curve.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            plantGowthModeling()
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadPlantsvg <- downloadHandler(
        "plant_growth_curve.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            plantGowthModeling()
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadPlantRdata <- downloadHandler(
        "plant_growth_parameter.Rdata",
        content = function(file) {
            result <- plantGowthModeling()
            save(result,
                 file = file)
        },
        contentType = "application/Rdata"
    )
    
    output$downloadPlantGrowthData <- downloadHandler(
        "modeling_data_summary_plant_level.txt",
        content = function(file) {
            write.table(
                plantDataSummay(),
                file = file,
                row.names = F,
                quote = F,
                sep = "\t"
            )
        },
        contentType = "text/tsv"
    )
    
    groupDataSummay <- reactive({
        inputData <- inputDataAnalysis()$data
        if(is.null(inputData)){
            return(NULL)
        }
        statData <-
            aggregate(inputData[, "plant"], inputData[, c("group")], function(x) {
                paste(length(unique(x)), "/", length(x))
            })
        colnames(statData)[2] <- "# plants /<br/>points"
        return(statData)
    })
    
    output$groupGrowthData <- DT::renderDataTable({
        myValidation()
        groupDataSummay()
    }, server = T, selection = 'single', escape = F,
    caption = 'Data summary at group level')
    
    output$groupGrowthPlot = renderPlot({
        myValidation()
        plantGowthModeling(T)
    })
    
    output$downloadGrouppng <- downloadHandler(
        "group_growth_curve.png",
        content = function(file) {
            png(file,
                width = 960,
                height = 960,
                pointsize = 24)
            plantGowthModeling(T)
            dev.off()
        },
        contentType = "image/png"
    )
    
    output$downloadGrouppdf <- downloadHandler(
        "group_growth_curve.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            plantGowthModeling(T)
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadGroupsvg <- downloadHandler(
        "group_growth_curve.svg",
        content = function(file) {
            svglite(file,
                    width = 8.27,
                    height = 8.27,
                    pointsize = 10)
            plantGowthModeling(T)
            dev.off()
        },
        contentType = "image/svg"
    )
    
    output$downloadGroupRdata <- downloadHandler(
        "group_growth_parameter.Rdata",
        content = function(file) {
            result <- plantGowthModeling(T)
            save(result,
                 file = file)
        },
        contentType = "application/Rdata"
    )
    
    output$downloadGroupGrowthData <- downloadHandler(
        "modeling_data_summary_group_level.txt",
        content = function(file) {
            write.table(
                groupDataSummay(),
                file = file,
                row.names = F,
                quote = F,
                sep = "\t"
            )
        },
        contentType = "text/tsv"
    )
    
    modelPerformance <- reactive({
        withProgress(value = 1,
                     message = "Modeling",
                     detail = "...",
                     {
                         dataset <- inputDataAnalysis()
                         inputData <- dataset$data
                         plants <- dataset$plants
                         run <- function(plant) {
                             setProgress(value = 0.5, detail = plant)
                         }
                         system.time({
                             ## foreach(i=1:length(plants)) %dopar% run(plants[i])
                             result <-
                                 bplapply(plants, run, BPPARAM=param)
                         })
                         if (is.null(input$models) ||
                             trimws(input$models) == "") {
                             useModels <- models[models$category == input$category, "id"]
                         } else{
                             useModels <- input$models
                         }
                         plot(1:10)
                         setProgress(value = 1, detail = "done!")
                     })
        
    })
    
    
    output$uiPlantAniPlot <- renderUI({
        myValidation()
        dataset <- inputDataAnalysis()
        inputData <- dataset$data
        plants <- dataset$plants
        sliderInput(
            "plantID",
            "Select a plant for check",
            0,
            length(plants),
            0,
            step = 1,
            animate = animationOptions(interval = 3000, loop = TRUE)
        )
    })
    
    output$aniPlantPlot <- renderPlot({
        myValidation()
        if (!is.null(input$plantID) && input$plantID > 0 &&
            !is.null(input$measurement) &&
            input$measurement != "") {
            dataset <- inputDataAnalysis()
            inputData <- dataset$data
            plants <- dataset$plants
            absmax <-
                max(abs(inputData[, input$measurement]), na.rm = T)
            id <- plants[input$plantID]
            modata <-
                inputData[inputData$plant == id, c("plant", "day", input$measurement)]
            colnames(modata) <- c("plant", "day", "trait")
            op <- par(mfrow = c(2, 2), pty = "m")
            modout <-
                growthModeling(modata,
                               id,
                               absmax,
                               dataset$days,
                               dataset$pred.time,
                               show.legend = F)
            if (!is.null(modout)) {
                pdata <- modout$pdata
                rownames(pdata) <- pdata$model
                bdata <- pdata$R
                names(bdata) <- rownames(pdata)
                barplot(
                    bdata,
                    las = 2,
                    col = pdata$color,
                    ylab = "R"
                )
                bdata <- pdata$R2
                names(bdata) <- rownames(pdata)
                barplot(
                    bdata,
                    las = 2,
                    col = pdata$color,
                    ylab = "R2"
                )
                bdata <- pdata$RMSRE
                names(bdata) <- rownames(pdata)
                barplot(
                    bdata,
                    las = 2,
                    col = pdata$color,
                    ylab = "RMSRE"
                )
            }
            par(op)
        } else{
            msgPlot()
        }
    })
    
    runAllModeling <- function() {
        dataset <- inputDataAnalysis()
        inputData <- dataset$data
        plants <- dataset$plants
        absmax <-
            max(abs(inputData[, input$measurement]), na.rm = T)
        for (id in plants) {
            modata <-
                inputData[inputData$plant == id, c("plant", "day", input$measurement)]
            colnames(modata) <- c("plant", "day", "trait")
            growthModeling(modata,
                           id,
                           absmax,
                           dataset$days,
                           dataset$pred.time)
        }
    }
    
    output$downloadModelspdf <- downloadHandler(
        "growth_models.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            op <- par(mfrow = c(2, 2), pty = "m")
            runAllModeling()
            par(op)
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    modelPerformance <- function() {
        dataset <- inputDataAnalysis()
        inputData <- dataset$data
        plants <- dataset$plants
        absmax <-
            max(abs(inputData[, input$measurement]), na.rm = T)
        run <- function(id) {
            modata <-
                inputData[inputData$plant == id, c("plant", "day", input$measurement)]
            colnames(modata) <- c("plant", "day", "trait")
            modout <-
                growthModeling(modata,
                               id,
                               absmax,
                               dataset$days,
                               dataset$pred.time,
                               plot = F)
            if (!is.null(modout)) {
                return(modout$pdata)
            } else{
                return(NULL)
            }
        }
        result <-
            bplapply(plants, run, BPPARAM=param)
        out <- data.frame()
        for (i in 1:length(result)) {
            out <- rbind(out, result[[i]])
        }
        out
    }
    
    wholePerformance <- reactive({
        clickAct <- input$wholeAction
        if (is.null(clickAct) || clickAct < 1) {
            msgPlot()
            return()
        }
        myValidation()
        plotd <- modelPerformance()
        op <- par(mfrow = c(2, 2), pty = "m")
        msgPlot(msg = "")
        boxplot(
            R ~ model,
            data = plotd,
            ylab = expression(R),
            col = plotd$color,
            las = 2
        )
        boxplot(
            R2 ~ model,
            data = plotd,
            ylab = expression(R ^ 2),
            col = plotd$color,
            las = 2
        )
        boxplot(
            RMSRE ~ model,
            data = plotd,
            ylab = expression(RMSRE),
            col = plotd$color,
            las = 2
        )
        par(op)
    })
    
    output$modelPerformance <- renderPlot({
        myValidation()
        wholePerformance()
    })
    
    output$downloadPerformancepdf <- downloadHandler(
        "model_performance.pdf",
        content = function(file) {
            pdf(file,
                width = 8.27,
                height = 8.27,
                pointsize = 10)
            plotd <- modelPerformance()
            op <- par(mfrow = c(2, 2), pty = "m")
            boxplot(
                R ~ model,
                data = plotd,
                ylab = expression(R),
                col = plotd$color,
                las = 2
            )
            boxplot(
                R2 ~ model,
                data = plotd,
                ylab = expression(R ^ 2),
                col = plotd$color,
                las = 2
            )
            boxplot(
                RMSRE ~ model,
                data = plotd,
                ylab = expression(RMSRE),
                col = plotd$color,
                las = 2
            )
            par(op)
            dev.off()
        },
        contentType = "image/pdf"
    )
    
    output$downloadPerformanceData <- downloadHandler(
        "model_performance.txt",
        content = function(file) {
            write.table(
                modelPerformance(),
                file = file,
                row.names = F,
                quote = F,
                sep = "\t"
            )
        },
        contentType = "text/tsv"
    )
})
