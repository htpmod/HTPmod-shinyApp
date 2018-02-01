shinyServer(function(input, output, session) {
    # # ----- navigation logic -----
    # values <-
    #     reactiveValues(# variable to keep track of whether or not the tab switching is manual (by the
    #         # user) or automatic (restoring the app's state on initialization or prev/next buttons)
    #         autoNavigating = 0)
    # 
    # # when the app initializes, if there is a history in the URL, navigate to it
    # observeEvent(session$clientData$url_search, {
    #     # if there is a history in the URL, restore the state
    #     if (nchar(session$clientData$url_search) > 1) {
    #         # when the app starts, the input$navbar gets triggered, but we don't
    #         # want to trigger the navigation function because the user didn't actively
    #         # navigate anywhere
    #         values$autoNavigating <- values$autoNavigating + 1
    #         restore(session$clientData$url_search)
    #     }
    # })
    # 
    # # restore the Shiny app's state based on the URL
    # restore <- function(qs) {
    #     data <- parseQueryString(qs)
    #     if (!is.null(data[['page']])) {
    #         # we're about to change tabs programatically, so don't trigger the
    #         # navigation function
    #         values$autoNavigating <- values$autoNavigating + 1
    #         # change to the correct tab
    #         updateTabsetPanel(session, "navbar", data[['page']])
    #     }
    # }
    # 
    # # when the user changes tabs, save the state in the URL
    # observeEvent(input$navbar, {
    #     if (values$autoNavigating > 0) {
    #         values$autoNavigating <- values$autoNavigating - 1
    #         return()
    #     }
    #     shinyjs::js$updateHistory(page = input$navbar)
    # })
})
