library(markdown)
shinyUI(tagList(
    includeCSS("www/global.css"),
    navbarPage(
        strong(APP_NAME, style = "font-size:14pt"),
        id = "navbar",
        header = tagList(
            useShinyjs()
            #,extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory"))
        ),
        tabPanel(
            strong(icon("home"), "Home", style = "font-size:14pt"),
            value = "home",
            fluidRow(column(12, div(
                class = "jumbotron",
                div(
                    class = "container",
                    style = "margin-top:-40px; margin-bottom:-40px;",
                    
                    h2(style = "font-weight:800;", align =
                           "center", APP_NAME),
                    h3(
                        style = "font-weight:800;",
                        align = "center",
                        "A Web-based Platform for Modeling and Visualization of Large-Scale Biological Data"
                    ),
                    p(),
                    p(
                        align = "center",
                        "A Shiny Application for Modeling and Visualizing Data from High-Throughput Experiments" ## High-Throughput Phenotyping/Sequencing
                    ),
                    p() 
                    # p(a(
                    #     actionButton("aboutButton", "Learn more", class = "btn-primary"),
                    #     href = "?page=about"
                    # ), align = "center")
                )
            ))),
            fluidRow(column(
                4,
                div(
                    class = "well well-sm",
                    h3(icon(APPs$module1$icon), strong(
                        paste0(APPs$module1$short, ": ", APPs$module1$name)
                    ), align =
                        "center"),
                    p(icon("align-justify"), class = "text-justify", APPs$module1$desc),
                    a(
                        class = "thumbnail",
                        img(src = APPs$module1$image),
                        align =
                            "center"
                    ),
                    # a(
                    #     class = "label label-default",
                    #     href = APPs$module1$href,
                    #     align = "right",
                    #     target = "_blank",
                    #     APPs$module1$ref
                    # ),
                    p(a(
                        actionButton(
                            "growthButton",
                            "Launch",
                            icon = icon("arrow-circle-right"),
                            class = "btn-success",
                            style = "font-size:150%"
                        ),
                        href = APPs$module1$link
                    ), align = "right")
                )
                
            ),
            column(
                4,
                div(
                    class = "well well-sm",
                    h3(icon(APPs$module2$icon), strong(
                        paste0(APPs$module2$short, ": ", APPs$module2$name)
                    ), align =
                           "center"),
                    p(icon("align-justify"), class = "text-justify", APPs$module2$desc),
                    a(
                        class = "thumbnail",
                        img(src = APPs$module2$image),
                        align =
                            "center"
                    ),
                    # a(
                    #     class = "label label-default",
                    #     href = APPs$module2$href,
                    #     target = "_blank",
                    #     APPs$module2$ref
                    # ),
                    p(a(
                        actionButton(
                            "biomassButton",
                            "Launch",
                            icon = icon("arrow-circle-right"),
                            class = "btn-warning",
                            style = "font-size:150%"
                        ),
                        href = APPs$module2$link
                    ), align = "right")
                )
                
            ),
            column(
                4,
                div(
                    class = "well well-sm",
                    h3(icon(APPs$module3$icon), strong(
                        paste0(APPs$module3$short, ": ", APPs$module3$name)
                    ), align =
                           "center"),
                    p(icon("align-justify"), class = "text-justify", APPs$module3$desc),
                    a(
                        class = "thumbnail",
                        img(src = APPs$module3$image),
                        align =
                            "center"
                    ),
                    # a(
                    #     class = "label label-default",
                    #     href = APPs$module3$href,
                    #     target = "_blank",
                    #     APPs$module3$ref
                    # ),
                    p(a(
                        actionButton(
                            "visButton",
                            "Launch",
                            icon = icon("arrow-circle-right"),
                            class = "btn-danger",
                            style = "font-size:150%"
                        ),
                        href = APPs$module3$link
                    ), align = "right")
                )
                
            ))
        ),
        navbarMenu(
            strong(icon("tasks"), "Apps", style = "font-size:14pt"),
            ##value = "apps",
            tabPanel(a(
                h4(icon(APPs$module1$icon), paste0(APPs$module1$short, ": ", APPs$module1$name)), href = APPs$module1$link
            ), value = "module1"),
            "----",
            tabPanel(a(
                h4(icon(APPs$module2$icon), paste0(APPs$module2$short, ": ", APPs$module2$name)), href = APPs$module2$link
            ), value = "module2"),
            "----",
            tabPanel(a(
                h4(icon(APPs$module3$icon), paste0(APPs$module3$short, ": ", APPs$module3$name)), href = APPs$module3$link
            ), value = "module3")
        ),
        tabPanel(
            strong(icon("book"), "About", style = "font-size:14pt"),
            value = "about", 
            # navbarPage(
            #     "App",
            #     tabPanel("Main"),
            #     navbarMenu(
            #         "More",
            #         tabPanel("Menu 1"),
            #         "----",
            #         "Section header",
            #         tabPanel("Github")
            #     )
            # ),
            # shiny::tags$iframe(
            #     src = "https://github.com/htpmod/HTPmod/",
            #     style = "margin-bottom:10px;",
            #     class = "scale",
            #     height = 650,
            #     frameborder = "no"
            # )
            mainPanel(includeMarkdown("www/about.md"))
        ),
        windowTitle = APP_NAME ##, theme="bootstrap.css"
    ),
    ## footer
    shiny::tags$div(class = "container-fluid shiny-code-container well",
                    HTML(HTML_FOOTER))
))
