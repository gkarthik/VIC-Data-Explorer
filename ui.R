library(shiny)
library(plotly)

shinyUI(fluidPage(
    titlePanel("VIC Data Explorer", "VIC Data Explorer"),
    tags$div(class = "header", style="font-size:14px;",
             tags$a(style="position:relative;", href = "https://vhfimmunotherapy.org/", "VIC"),
             tags$a(style="position:relative;margin-left:20px;", href = "https://andersen-lab.com/", "Andersen Lab")
             ),
    sidebarPanel(
        selectInput('x', 'X', choices = names(subl), selected = "Protection"),
        selectInput('y', 'Y', choices = names(subl), selected = "Neut_micro"),
        selectInput('color', 'Color', choices = names(subl), selected = "Neut_micro"),
        actionButton('addFilter', 'Insert Filter'),
        actionButton('removeFilter', 'Remove Filter'),
        h3("R code for Filters"),
        textOutput("dfStatus"),
        tags$div(id = 'filter-wrapper')
    ),
    mainPanel(
        plotlyOutput("trendPlot"),
        htmlOutput("detailsText")
    )
    )
    );
