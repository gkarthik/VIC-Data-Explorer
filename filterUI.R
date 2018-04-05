library(shiny)
library(ggplot2)

filterUI <- function(id) {
    ns <- NS(id)
    tags$div(
        selectInput(ns("filter"), label="", choices = names(subl), selected="Protection"),
        uiOutput(ns("filterValue")),
        plotlyOutput(ns("filterPlot")),
        tags$hr(style="border-color: #000000;border-width:5px;"),
        id = ns(paste0('filter-wrapper-',id))
    )
}

filter <- function(input, output, session) {
    ns <- session$ns

    filterVal <- reactive({
        list("col" = input$filter, "cond" = input$filterCond)
    })

    output$filterValue <- renderUI({
        if(class(subl[,input$filter]) == "numeric"){
            sliderInput(ns("filterCond"), "Range:",
                        min = min(subl[,input$filter]), max = max(subl[,input$filter]),
                        value = c(min(subl[,input$filter]),max(subl[,input$filter])))
        } else if(class(subl[,input$filter]) == "factor"){
            selectInput(ns('filterCond'), 'Value', choices = levels(subl[,input$filter]), multiple=TRUE, selected=levels(subl[,input$filter])[1])
        } else {
            selectInput(ns('filterCond'), 'Value', choices = unique(subl[,input$filter]), multiple=TRUE, selected = unique(subl[,input$filter])[1])
        }
    })

    output$filterPlot <- renderPlotly({
        if(class(subl[,input$filter]) == "numeric"){
            ggplot(subl, aes_string(input$filter)) + geom_histogram()
        } else {
            ggplot(subl, aes_string(input$filter)) + geom_bar(stat="count")
        }
    })

    return(filterVal);
}
