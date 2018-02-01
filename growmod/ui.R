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
            strong(icon(APPs$module1$icon), APPs$module1$short, style = "font-size:14pt"),
            value = "app",
            sidebarLayout(
                sidebarPanel(
                    div(
                        id = "div_step1",
                        h4(icon("caret-right"), "1. Load Data", a(id="tip1", icon("question-circle"))),
                        bsPopover("tip1", "File Formating Instruction", "<p>Upload a tab/comma/semicolon delimited text file (< 10MB in size) of at least four columns.</p> <p>With a header line: plant, group, day, measurement, (other measurement).</p>",
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
                                choices = growmod_example_data,
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
                        h4(icon("caret-right"), "2. Input Data"),
                        uiOutput("uiMeasurement")
                    ),
                    div(
                        id = "div_step3",
                        h4(icon("caret-right"), "3. Select Models"),
                        selectInput(
                            'category',
                            '3.1. Which type of growth model?',
                            choices = list("Normal" = 'normal', "Stressed" = 'stressed'),
                            selected = 'normal'
                        ),
                        uiOutput("uiModels"),
                        uiOutput("uiStressedPeriod")
                    ),
                    div(
                        id = "div_step4",
                        h4(icon("caret-right"), "4. Customization"),
                        checkboxInput("checkdata",
                                      strong("Check data completeness before modeling"),
                                      TRUE),
                        checkboxInput("showpoints",
                                      strong("Show data points in the plot"),
                                      TRUE),
                        div(id = "div_highlight", checkboxInput(
                            "highlight",
                            strong("Highlight the stressed period"),
                            TRUE
                        )),
                        sliderInput(
                            "minpoints",
                            "Minimum data points for modeling",
                            min = 5,
                            max = 50,
                            value = 6
                        ),
                        textInput("xlabel",
                                  "x label of growth curve",
                                  ""),
                        textInput("ylabel",
                                  "y label of growth curve",
                                  "")
                    )
                    ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            strong(icon("database"), "My Data"),
                            br(),
                            DT::dataTableOutput("inputData"),
                            br(),
                            downloadButton("downloadInputData", label = "Save as tab delimited .txt")
                        ),
                        tabPanel(
                            strong(icon("cogs"), "Plant-based Modeling"),
                            br(),
                            fluidRow(
                                column(
                                    6,
                                    DT::dataTableOutput('plantGrowthData'),
                                    br(),
                                    downloadButton("downloadPlantGrowthData", label = "Save as tab delimited .txt")
                                ),
                                column(
                                    6,
                                    h5(strong("Growth Curves (plant)")),
                                    div(class = "thumbnail", plotOutput("plantGrowthPlot", height = "500px")),
                                    downloadButton("downloadPlantpng", label = "Save as png"),
                                    downloadButton("downloadPlantpdf", label = "Save as pdf"),
                                    downloadButton("downloadPlantsvg", label = "Save as svg"),
                                    downloadButton("downloadPlantRdata", label = "Dowload parameters")
                                )
                            )
                        ),
                        tabPanel(
                            strong(icon("cogs"), "Group-based Modeling"),
                            br(),
                            fluidRow(
                                column(
                                    6,
                                    DT::dataTableOutput('groupGrowthData'),
                                    br(),
                                    downloadButton("downloadGroupGrowthData", label = "Save as tab delimited .txt")
                                ),
                                column(
                                    6,
                                    h5(strong("Growth Curves (group)")),
                                    div(class = "thumbnail", plotOutput("groupGrowthPlot", height = "500px")),
                                    downloadButton("downloadGrouppng", label = "Save as png"),
                                    downloadButton("downloadGrouppdf", label = "Save as pdf"),
                                    downloadButton("downloadGroupsvg", label = "Save as svg"),
                                    downloadButton("downloadGroupRdata", label = "Dowload parameters")
                                )
                            )
                        ),
                        tabPanel(strong(icon("balance-scale"), "Performance"),
                                 br(),
                                 fluidRow(
                                     column(
                                         6,
                                         h5(strong("Model Performance (individual)")),
                                         div(class = "thumbnail", plotOutput("aniPlantPlot", height = "500px")),
                                         uiOutput("uiPlantAniPlot"),
                                         br(),
                                         downloadButton("downloadModelspdf", label = "Save all as pdf")
                                     ),
                                     column(
                                         6,
                                         h5(strong("Model Performance (overall)")),
                                         div(class = "thumbnail", plotOutput("modelPerformance", height = "500px")),
                                         flowLayout(
                                             actionButton(
                                                 class = "btn btn-warning",
                                                 icon = icon("exclamation-triangle"),
                                                 "wholeAction",
                                                 HTML(
                                                     "<b>Start</b> (may take some time)"
                                                 )
                                             )
                                         ),
                                         br(),
                                         downloadButton("downloadPerformancepdf", label = "Save as pdf"),
                                         downloadButton("downloadPerformanceData", label = "Save as tab delimited .txt")
                                     )
                                 )),
                        id = "mainPanels",
                        selected = strong(icon("database"), "My Data")
                    ),
                    br()
                )
        )
        ),
        windowTitle = paste0(APP_NAME, ": ", APPs$module1$name) ##, theme="bootstrap.css"
    ),
    ## footer
    shiny::tags$div(class = "container-fluid shiny-code-container well",
                    HTML(HTML_FOOTER))
))
