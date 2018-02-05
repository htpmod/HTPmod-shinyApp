library(shiny)
library(shinyBS)
color_scheme <- list(
    'Single hue' = c("Blues","Greens","Greys","Oranges","Purples","Reds"),
    'Multi-hue' = c("BuGn","BuPu","GnBu","OrRd","PuBu","PuBuGn","PuRd","RdPu","YlGn","YlGnBu","YlOrBr","YlOrRd"),
    'Diverging' = c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral",
                    paste0(c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral"),'-rev')))

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
            strong(icon(APPs$module3$icon), APPs$module3$short, style = "font-size:14pt"),
            value = "app",
            sidebarLayout(
                sidebarPanel(
                    div(
                        id = "div_step1",
                        h4(icon("caret-right"), "1. Load Data", a(id="tip1", icon("question-circle"))),
                        bsPopover("tip1", "File Formating Instruction", "<p>Upload a table file with the first row as the header line.</p> All columns with numeric values are used for analysis. Categorial columns will be used for plot annotation purpose. </p>",
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
                                choices = htpdvis_example_data,
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
                        h4(icon("caret-right"), "2. Data Summary"),
                        h5(strong("All input data, with size of: ")),
                        verbatimTextOutput("uiX1"),
                        h5(strong("Numeric data matrix for visualization, with size of: ")),
                        verbatimTextOutput("uiX2")
                    ),
                    div(
                        id = "div_step3",
                        h4(icon("caret-right"), "3. Customization"),
                        uiOutput('uicpRow'),
                        h5(strong("Data preprocessing:")),
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
                        )
                        
                        # fileInput(
                        #     'featureMeta',
                        #     label = "Feature annotation:",
                        #     accept = c('text/csv',
                        #                'text/comma-separated-values,text/plain',
                        #                '.txt')
                        # )
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
                        tabPanel(strong(icon("cubes"), "PCA"),
                                 br(), 
                                 fluidRow(
                                     column(
                                         3,
                                         sliderInput(
                                             "npcs",
                                             "Number of components",
                                             min = 1,
                                             max = 10,
                                             value = 3,
                                             step = 1
                                         )
                                     ),
                                     column(
                                         3,
                                         h5(strong("In the PCA plot: ")),
                                         checkboxInput("show_component_score",
                                                       strong("Show component scores"),
                                                       TRUE),
                                         checkboxInput("show_factor_loading",
                                                       strong("Show factor loadings"),
                                                       TRUE) 
                                     ),
                                     column(
                                         3,
                                         sliderInput(
                                             "theta",
                                             "Azimuthal direction",
                                             min = -360,
                                             max = 360,
                                             value = 40,
                                             step = 10
                                         )
                                     ),
                                     column(
                                         3,
                                         sliderInput(
                                             "phi",
                                             "Colatitude direction",
                                             min = -360,
                                             max = 360,
                                             value = 40,
                                             step = 10
                                         )
                                     )
                                 ),
                                 fluidRow(
                                     column(
                                         6,
                                         h5(strong("PCA: Principal Component Analysis")),
                                         div(class = "thumbnail", plotOutput("mpca", height = "500px"))
                                     ),
                                     column(
                                         6,
                                         h5(strong("PCA")),
                                         div(class = "thumbnail", plotOutput("ppca", height = "500px"))
                                     )
                                 ), 
                                 fluidRow(
                                     column(
                                         6,
                                         downloadButton("downloadMpcapng", label = "Save as png"),
                                         downloadButton("downloadMpcapdf", label = "Save as pdf"),
                                         downloadButton("downloadMpcasvg", label = "Save as svg")
                                     ),
                                     column(
                                         6,
                                         downloadButton("downloadPpcapng", label = "Save as png"),
                                         downloadButton("downloadPpcapdf", label = "Save as pdf"),
                                         ## downloadButton("downloadPpcasvg", label = "Save as svg")
                                         downloadButton("downloadPpcaVar", label = "Explained variance .txt"),
                                         downloadButton("downloadPpcaLoading", label = "Loading score .txt")
                                     )
                                 ),
                                 hr(),
                                 fluidRow(
                                     column(
                                         12,
                                         h5(strong("Figure legend preview"))
                                     ),
                                     ## colourInput("col", "Select colour", "red", palette="limited"),
                                     column(
                                         4,
                                         DT::dataTableOutput("ctype")
                                     ),
                                     column(
                                         4,
                                         DT::dataTableOutput("ptype")
                                     ),
                                     column(
                                         4,
                                         DT::dataTableOutput("ftype")
                                     )
                                 )
                        ),
                        tabPanel(strong(icon("snowflake-o"), "t-SNE"),
                                 br(),
                                 fluidRow(
                                     column(
                                         3,
                                         sliderInput(
                                             "ndims",
                                             "Dimension",
                                             min = 2,
                                             max = 5,
                                             value = 3,
                                             step = 1
                                         )
                                     ),
                                     column(
                                         3,
                                         div()
                                     ),
                                     column(
                                         3,
                                         sliderInput(
                                             "theta2",
                                             "Azimuthal direction",
                                             min = -360,
                                             max = 360,
                                             value = 40,
                                             step = 10
                                         )
                                     ),
                                     column(
                                         3,
                                         sliderInput(
                                             "phi2",
                                             "Colatitude direction",
                                             min = -360,
                                             max = 360,
                                             value = 40,
                                             step = 10
                                         )
                                     )
                                 ),
                                 fluidRow(
                                     column(
                                         6,
                                         h5(strong("t-SNE: t-Distributed Stochastic Neighbor Embedding")),
                                         div(class = "thumbnail", plotOutput("tsne1", height = "500px"))
                                     ),
                                     column(
                                         6,
                                         h5(strong("t-SNE")),
                                         div(class = "thumbnail", plotOutput("tsne2", height = "500px"))
                                     )
                                 ),
                                 fluidRow(
                                     column(
                                         6,
                                         downloadButton("downloadtsne1png", label = "Save as png"),
                                         downloadButton("downloadtsne1pdf", label = "Save as pdf"),
                                         downloadButton("downloadtsne1svg", label = "Save as svg")
                                     ),
                                     column(
                                         6,
                                         downloadButton("downloadtsne2png", label = "Save as png"),
                                         downloadButton("downloadtsne2pdf", label = "Save as pdf"),
                                         downloadButton("downloadtsne2svg", label = "Save as svg")
                                     )
                                 )
                        ),
                        tabPanel(
                            strong(icon("diamond"), "MDS"),
                            br(),
                            fluidRow(
                                column(
                                    3,
                                    sliderInput(
                                        "nmds",
                                        "Dimension",
                                        min = 2,
                                        max = 6,
                                        value = 3,
                                        step = 1
                                    )
                                ),
                                column(
                                    3,
                                    selectizeInput(
                                        "dist_method",
                                        "Distance measure",
                                        choices = c(Euclidean="euclidean", Maximum="maximum", Manhattan="manhattan", 
                                                    Canberra="canberra", Minkowski="minkowski"),
                                        multiple = F, 
                                        options = list(placeholder = 'Distance'),
                                        selected = "euclidean"
                                    )
                                ),
                                column(
                                    3,
                                    sliderInput(
                                        "theta3",
                                        "Azimuthal direction",
                                        min = -360,
                                        max = 360,
                                        value = 40,
                                        step = 10
                                    )
                                ),
                                column(
                                    3,
                                    sliderInput(
                                        "phi3",
                                        "Colatitude direction",
                                        min = -360,
                                        max = 360,
                                        value = 40,
                                        step = 10
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    6,
                                    h5(strong("MDS: Multidimensional Scaling")),
                                    div(class = "thumbnail", plotOutput("mds1", height = "500px"))
                                ),
                                column(
                                    6,
                                    h5(strong("MDS")),
                                    div(class = "thumbnail", plotOutput("mds2", height = "500px"))
                                )
                            ), 
                            fluidRow(
                                column(
                                    6,
                                    downloadButton("downloadmds1png", label = "Save as png"),
                                    downloadButton("downloadmds1pdf", label = "Save as pdf"),
                                    downloadButton("downloadmds1svg", label = "Save as svg")
                                ),
                                column(
                                    6,
                                    downloadButton("downloadmds2png", label = "Save as png"),
                                    downloadButton("downloadmds2pdf", label = "Save as pdf"),
                                    downloadButton("downloadmds2svg", label = "Save as svg")
                                )
                            )
                        ),
                        tabPanel(strong(icon("life-ring"), "SOM"),
                                 br(),
                                 fluidRow(
                                     column(
                                         8,
                                         h5(strong("Dimensions of the grid")),
                                         fluidRow(
                                             column(
                                                 6,
                                                 sliderInput(
                                                     "xdim",
                                                     "x-dimension",
                                                     min = 1,
                                                     max = 10,
                                                     value = 3,
                                                     step = 1
                                                 )
                                             ),
                                             column(
                                                 6,
                                                 sliderInput(
                                                     "ydim",
                                                     "y-dimension",
                                                     min = 1,
                                                     max = 10,
                                                     value = 3,
                                                     step = 1
                                                 )
                                             )
                                         )
                                     ),
                                     column(
                                         4,
                                         radioButtons(
                                             "topo",
                                             label = "Topology of the grid",
                                             choices = list("Hexagonal" = "hexagonal",
                                                            "Rectangular" = "rectangular"),
                                             inline = F
                                         )
                                     )
                                 ),
                                 fluidRow(
                                     column(
                                         6,
                                         h5(strong("SOM: Self-Organizing Map")),
                                         div(class = "thumbnail", plotOutput("msom", height = "500px")),
                                         downloadButton("downloadSOMpng", label = "Save as png"),
                                         downloadButton("downloadSOMpdf", label = "Save as pdf"),
                                         downloadButton("downloadSOMsvg", label = "Save as svg")
                                     ),
                                     column(
                                         6,
                                         h5(strong("SOM + PCA")),
                                         div(class = "thumbnail", plotOutput("psom", height = "500px")),
                                         downloadButton("downloadsSOMpng", label = "Save as png"),
                                         downloadButton("downloadsSOMpdf", label = "Save as pdf"),
                                         downloadButton("downloadsSOMsvg", label = "Save as svg")
                                     )
                                 )
                        ),
                        tabPanel(strong(icon("recycle"), "K-MC"),
                                 br(), 
                                 fluidRow(
                                     column(
                                         4,
                                         sliderInput(
                                             "ncluster",
                                             "Number of clusters",
                                             min = 2,
                                             max = 10,
                                             value = 3,
                                             step = 1
                                         )
                                     )
                                 ),
                                 fluidRow(
                                     column(
                                         6,
                                         h5(strong("K-MC: K-Means Clustering")),
                                         div(class = "thumbnail", plotOutput("mkmc", height = "500px"))
                                     ),
                                     column(
                                         6,
                                         h5(strong("Statistics")),
                                         div(class = "thumbnail", plotOutput("pkmc", height = "500px"))
                                     )
                                 ), 
                                 fluidRow(
                                     column(
                                         6,
                                         downloadButton("downloadMkmcpng", label = "Save as png"),
                                         downloadButton("downloadMkmcpdf", label = "Save as pdf"),
                                         downloadButton("downloadMkmcsvg", label = "Save as svg")
                                     ),
                                     column(
                                         6,
                                         downloadButton("downloadPkmcpng", label = "Save as png"),
                                         downloadButton("downloadPkmcpdf", label = "Save as pdf"),
                                         downloadButton("downloadPkmcsvg", label = "Save as svg")
                                     )
                                 )
                        ),    
                        tabPanel(strong(icon("sitemap"), "HCA"),
                                 br(),
                                 fluidRow(
                                     column(
                                         2, 
                                         selectizeInput(
                                             "cor_method",
                                             "Correlation method",
                                             choices = c(Pearson="pearson", Spearman="spearman", Kendall="kendall"),
                                             multiple = F,
                                             options = list(placeholder = 'Correlation'),
                                             selected = "spearman"
                                         )
                                     ),
                                     column(
                                         3, 
                                         selectizeInput(
                                             "color_space",
                                             div("Color scheme for heatmap", a(target="_blank", icon("question-circle"), href="http://colorbrewer2.org/")),
                                             choices = color_scheme,
                                             multiple = F,
                                             options = list(placeholder = 'Color'),
                                             selected = "RdYlBu-rev"
                                         )
                                     ),
                                     column(
                                         1,
                                         h5(strong("Shape")),
                                         checkboxInput("circle", icon("circle-o-notch"), F), 
                                         bsPopover("circle", "Tip", "Visualize the dendrogram tree in a fan style",
                                                   placement="bottom", options = list(container = "body"))
                                     ),
                                     column(
                                         2, 
                                         selectizeInput(
                                             "cor_method2",
                                             "Correlation method",
                                             choices = c(Pearson="pearson", Spearman="spearman", Kendall="kendall"),
                                             multiple = F,
                                             options = list(placeholder = 'Correlation'),
                                             selected = "spearman"
                                         )
                                     ),
                                     column(
                                         3, 
                                         selectizeInput(
                                             "color_space2",
                                             div("Color scheme for heatmap", a(target="_blank", icon("question-circle"), href="http://colorbrewer2.org/")),
                                             choices = color_scheme,
                                             multiple = F,
                                             options = list(placeholder = 'Color'),
                                             selected = "PiYG-rev"
                                         )
                                     ),
                                     column(
                                         1,
                                         h5(strong("Shape")),
                                         checkboxInput("circle1", icon("circle-o-notch"), F),
                                         bsPopover("circle1", "Tip", "Visualize the dendrogram tree in a fan style",
                                                   placement="bottom", options = list(container = "body"))
                                     )
                                 ),
                                 fluidRow(
                                     column(
                                         6,
                                         h5(strong("Correlation of observations")),
                                         div(class = "thumbnail", plotOutput("hca1", height = "500px"))
                                     ),
                                     column(
                                         6,
                                         h5(strong("Correlation of features")),
                                         div(class = "thumbnail", plotOutput("hca2", height = "500px"))
                                     )
                                 ), 
                                 fluidRow(
                                     column(
                                         6,
                                         downloadButton("downloadHtmp1png", label = "Save as png"),
                                         downloadButton("downloadHtmp1pdf", label = "Save as pdf"),
                                         downloadButton("downloadHtmp1tab", label = "Save as Table")
                                     ),
                                     column(
                                         6,
                                         downloadButton("downloadHtmp2png", label = "Save as png"),
                                         downloadButton("downloadHtmp2pdf", label = "Save as pdf"),
                                         downloadButton("downloadHtmp2tab", label = "Save as Table")
                                     )
                                 ),
                                 hr(),
                                 fluidRow(
                                     column(
                                         6,
                                         h5(strong("Heatmap of the whole dataset")),
                                         div(class = "thumbnail", plotOutput("hca3", height = "500px"))
                                     ),
                                     column(
                                         6,
                                         br(),
                                         selectizeInput(
                                             "color_space3",
                                             div("Color scheme for heatmap", a(target="_blank", icon("question-circle"), href="http://colorbrewer2.org/")),
                                             choices = color_scheme,
                                             multiple = F,
                                             options = list(placeholder = 'Color'),
                                             selected = "RdYlBu-rev"
                                         ),
                                         checkboxInput("cluster_rows",
                                                       strong("Cluster rows/observations"),
                                                       TRUE),
                                         sliderInput(
                                             "cutree_rows",
                                             "#clusters of rows",
                                             min = 1,
                                             max = 10,
                                             value = 1,
                                             step = 1
                                         ), 
                                         checkboxInput("cluster_cols",
                                                       strong("Cluster columns/features"),
                                                       TRUE), 
                                         sliderInput(
                                             "cutree_cols",
                                             "#clusters of columns",
                                             min = 1,
                                             max = 10,
                                             value = 1,
                                             step = 1
                                         ), 
                                         downloadButton("downloadHtmp3png", label = "Save as png"),
                                         downloadButton("downloadHtmp3pdf", label = "Save as pdf"),
                                         downloadButton("downloadHtmp3tab", label = "Save as Table")
                                     )
                                 )
                        ),
                        tabPanel(strong(icon("object-group"), "Misc"),
                                 br(),
                                 fluidRow(
                                     column(
                                         6,
                                         h5(strong("Data for drawing")),
                                         ## bsPopover("tip2", "Tip", "Plot will be colored according the color customization set on the left panel",
                                         ##          placement="right", options = list(container = "body")),
                                         fluidRow(
                                             column(
                                                 6, 
                                                 uiOutput('uipxyzm'),
                                                 helpText("Note: graph will be colored according the color customization set on the left panel")
                                             ),
                                             column(
                                                 6,
                                                 selectizeInput(
                                                     "misct",
                                                     "Type of plot",
                                                     choices = list(
                                                         'One-Variable Plot: numeric Y' = c(`Density plot` = '1|ggdensity', 
                                                                                            `ECDF plot` = '1|ggecdf', 
                                                                                            `Histogram plot` = '1|gghistogram', 
                                                                                            `QQ plot` = '1|ggqqplot'),
                                                         'Two-Vriable Plot: categorical X & numeric Y' = c(`Bar plot` = '2|ggbarplot', 
                                                                                                           `Box plot` = '2|ggboxplot',
                                                                                                           `Line plot` ='2|ggline',
                                                                                                           ## `Pie chart` = '2|ggpie',
                                                                                                           `Stripchart` = '2|ggstripchart',
                                                                                                           `Violin Plot` = '2|ggviolin'), 
                                                         'Two-Vriable Plot: numeric Y & Z' = c(`Scatter plot with boxplot` = '3|ggscatter',
                                                                                               `Scatter plot with correlation` = '3|stat_cor',
                                                                                               `Scatter plot with histograms` = '3|ggscatterhist',
                                                                                               `Scatter plot with stars` = '3|stat_stars'), 
                                                         'Multi-Vriable Plot: numeric M' = c(`Multi-Box plot` = 'm|box',
                                                                                             `Overlayed density plot` = 'm|density', 
                                                                                             `Scatterplot matrix` = 'm|pairs', 
                                                                                             `Scatterplot matrix with ellipses` = 'm|ellipse'),
                                                         'Other Plot: numeric Y & M' = c(`Multi-Scatter plot` = 'o|scatter')
                                                     ),
                                                     multiple = F,
                                                     options = list(placeholder = 'Choose a plot'),
                                                     selected = "m|pairs"
                                                 )
                                             )
                                         )
                                     ),
                                     column(
                                         6,
                                         h5(strong("Plot")),
                                         div(class = "thumbnail", plotOutput("miscplot", height = "500px")),
                                         downloadButton("downloadmiscpng", label = "Save as png"),
                                         downloadButton("downloadmiscpdf", label = "Save as pdf"),
                                         downloadButton("downloadmiscsvg", label = "Save as svg")
                                     )
                                 )
                        ),
                        id = "mainPanel",
                        selected = strong(icon("database"), "My Data")
                    ),
                    br()
                )
            )
        ),
        windowTitle = paste0(APP_NAME, ": ", APPs$module3$name) ##, theme="bootstrap.css"
    ),
    ## footer
    shiny::tags$div(class = "container-fluid shiny-code-container well",
                    HTML(HTML_FOOTER))
))
