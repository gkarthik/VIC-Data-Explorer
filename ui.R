library(shiny)
library(plotly)

shinyUI(fluidPage(
    headerPanel("VIC"),
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
    ),
    hr(),
    tags$div(class = "footer", style="text-align:right;", checked = NA,
             tags$a(href = "https://andersen-lab.com/", "Andersen Lab")
             )
    )
    );
