library(shiny)
library(shinyBS)

shinyUI(tagList(
    ## shinythemes::themeSelector(),
    navbarPage(
        strong(a(APP_NAME, href = APP_URL), style = "font-size:14pt"),
        id = "navbar",
        header = tagList(
            useShinyjs()
            ##,extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory"))
        ),
        tabPanel(
            strong(icon(APPs$module2$icon), APPs$module2$short, style = "font-size:14pt"),
            value = "app",
            sidebarLayout(
                sidebarPanel(
                    div(
                        id = "div_step1",
                        h4(icon("caret-right"), "1. Load Data", a(id="tip1", icon("question-circle"))),
                        bsPopover("tip1", "File Formating Instruction", "<p>Upload a table file with the first row as the header line.</p> <p>Columns (features) can be assigned to different groups, with column in the format of \"feature_name|group\", while group information is optional (see example data for details). </p> <p>Input features (i.e., \"X\") should be numeric values. For regression, \"y\" is numeric; for classification, \"y\" is categorical. </p>",
                                  placement="right", options = list(container = "body")),
                        radioButtons(
                            "fileToUse",
                            label = NULL,
                            choices = list("Example data" = "demo",
                                           "Upload your data" = "uploadData")
                        ),
                        div(
                            selectizeInput(
                                "example_data",
                                "Example Data",
                                choices = predmod_example_data,
                                multiple = F,
                                options = list(placeholder = 'Please select one example dataset'),
                                selected = ""
                            ),
                            id = "div_exampleInUse",
                            ## class = "alert alert-info",
                            uiOutput("uiDS")
                        ),
                        div(
                            id = "div_fileupload",
                            fileInput(
                                'inputFile',
                                label = "Choose a File:",
                                accept = c('text/csv',
                                           'text/comma-separated-values,text/plain',
                                           '.txt')
                            ),
                            radioButtons(
                                "separator",
                                "Separator",
                                c(
                                    "Comma" = ',',
                                    "Semicolon" = ';',
                                    "Tab" = '\t'
                                ),
                                selected = '\t',
                                inline = T
                            )
                        )
                    ),
                    div(
                        id = "div_step2",
                        h4(icon("caret-right"), "2. Models"),
                        radioButtons(
                            "model_type",
                            "Which type of modeling?",
                            c("Regression", "Classification"),
                            selected = 'Regression',
                            inline = T
                        ),
                        uiOutput("uiModels")
                    ),
                    div(
                        id = "div_step3",
                        h4(icon("caret-right"), "3. Modeling Data"),
                        uiOutput("uiY"),
                        uiOutput("uiEx"),
                        h5(strong("The size of the 'X' matrix: ")),
                        verbatimTextOutput("uiX")
                    ),
                    div(
                        id = "div_step4",
                        h4(icon("caret-right"), "4. Customization"),
                        # checkboxInput("showpoints",
                        #               strong("Show data points in the plots"),
                        #               TRUE),
                        h5(strong("Pre-processing transformation:"), id="prepro"),
                        fluidRow(
                            column(
                                6,
                                checkboxInput("center",
                                              strong("Center"),
                                              TRUE)        
                            ),
                            column(
                                6,
                                checkboxInput("scale",
                                              strong("Scale"),
                                              TRUE)        
                            )
                        ),
                        bsPopover("prepro", "Tip", "Pre-process the data prior to model fitting.",
                                  placement="bottom", options = list(container = "body")),
                        h5(strong("Model training and parameter tuning:"), id="tuning"),
                        fluidRow(
                            column(
                                6, 
                                sliderInput(
                                    "nfold",
                                    "k-fold cross-validation",
                                    min = 2,
                                    max = 10,
                                    value = 5
                                )
                            ),
                            column(
                                6, 
                                sliderInput(
                                    "ntimes",
                                    "N-time randomizations",
                                    min = 1,
                                    max = 50,
                                    value = 3
                                )
                            )
                        ),
                        bsPopover("tuning", "Tip", "Control parameters for training.",
                                  placement="bottom", options = list(container = "body"))
                    )
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            strong(icon("database"), "My Data"),
                            br(),
                            DT::dataTableOutput("inputData"),
                            br(),
                            downloadButton("downloadInputData", label = "Save as Table .txt")
                        ),
                        tabPanel(strong(icon("futbol-o"), "Modeling"), 
                            br(),
                            fluidRow(
                                column(
                                    12,
                                    actionButton("showformula", uiOutput("uiModType"), class = "btn btn-info"),
                                    bsPopover("showformula", "", "Toggle to show/hide. ",
                                              placement="right", options = list(container = "body")),
                                    conditionalPanel(
                                        condition = "input.showformula % 2 == 1",
                                        verbatimTextOutput("uiFormula")
                                    )
                                )
                            ),
                            br(),
                            fluidRow(
                                column(
                                    6,
                                    h5(strong("Prediction Models")),
                                    div(class = "thumbnail", plotOutput("modelingPlot", height = "500px")),
                                    actionButton(
                                        class = "btn btn-warning",
                                        icon = icon("play"),
                                        "runMod",
                                        HTML("<b>Run</b>")
                                    ),
                                    bsPopover("runMod", "", "Time-consuming!", placement="right", options = list(container = "body"))
                                ),
                                # column(
                                #     3,
                                #     helpText("Note: for classification analysis, some model may fail due to too many classes or not enough samples for training the model")
                                # ),
                                column(
                                    6,
                                    h5(strong("Model Evaluation")),
                                    div(class = "thumbnail", plotOutput("evaluationPlot", height = "500px")),
                                    actionButton(
                                        class = "btn btn-danger",
                                        icon = icon("play"),
                                        "runEva",
                                        HTML("<b>Run</b>")
                                    ),
                                    bsPopover("runEva", "", "Time-consuming!", placement="right", options = list(container = "body"))
                                )
                            ),
                            br(),
                            fluidRow(
                                column(
                                    6,
                                    downloadButton("downloadModelpng", label = "Save as png"),
                                    downloadButton("downloadModelpdf", label = "Save as pdf"),
                                    downloadButton("downloadModelsvg", label = "Save as svg"),
                                    downloadButton("downloadModeltab", label = "Save as Table")
                                ),
                                column(
                                    6,
                                    downloadButton("downloadEvalpng", label = "Save as png"),
                                    downloadButton("downloadEvalpdf", label = "Save as pdf"),
                                    downloadButton("downloadEvalsvg", label = "Save as svg"),
                                    downloadButton("downloadEvaltab", label = "Save as Table")
                                )
                            ),
                            br(),
                            fluidRow(
                                column(
                                    12,
                                    h5(strong("Feature Importance")),
                                    div(class = "thumbnail", plotOutput("featureImp", height = "500px")),
                                    uiOutput("currentMod")
                                )
                            ),
                            fluidRow(
                                column(
                                    4,
                                    actionButton(
                                        class = "btn btn-success",
                                        icon = icon("play"),
                                        "runImp",
                                        HTML("<b>Run</b>")
                                    ),
                                    bsPopover("runImp", "", "Time-consuming!", placement="right", options = list(container = "body"))
                                ),
                                column(
                                    8,
                                    downloadButton("downloadImppng", label = "Save as png"),
                                    downloadButton("downloadImppdf", label = "Save as pdf"),
                                    downloadButton("downloadImpsvg", label = "Save as svg"),
                                    downloadButton("downloadImptab", label = "Save as Table")
                                )
                            )
                        ),
                        tabPanel(
                            strong(icon("cubes"), "Models"),
                            br(),
                            includeMarkdown("www/model_info.md")
                        ),
                        id = "mainPanels",
                        selected = strong(icon("database"), "My Data")
                    ),
                    br()
                )
            )
        ),
        windowTitle = paste0(APP_NAME, ": ", APPs$module2$name) ##, theme="bootstrap.css"
    ),
    ## footer
    shiny::tags$div(class = "container-fluid shiny-code-container well",
                    HTML(HTML_FOOTER))
))
