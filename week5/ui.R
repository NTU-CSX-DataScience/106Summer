library(shiny)
library(ggplot2)

library(markdown)

choice.type <-
    c('gender', 'math.hours', 'science.hours', 'parental.education')
choice.value <-
    c(
        'math',
        'math.interest',
        'science.evaluation',
        'math.input',
        'science',
        'science.interest',
        'science.evaluation',
        'science.input'
    )

navbarPage(
    "Shiny Example",
    tabPanel(
        "Introduction",
        tags$h1("This is an example for making a shiny app."),
        tags$p("Let's use the dataset same with week4 task example.")
        #HTML("<img height=600 src=\"https://78.media.tumblr.com/aa70ed84a7cd2e7b83c36118dfb2c0e5/tumblr_p8xzq1U8ik1qhy6c9o1_500.gif\"/>")
    ),
    tabPanel(
        "Raw Data",
        tags$h1("Let's take a look at the dataset."),
        br(),
        fluidRow(column(
            8,
            tabPanel("Table",
                     DT::dataTableOutput("data.raw"))
        ))
    ),
    
    tabPanel(
        "Single Variable",
        tags$h1("Summrizing time!"),
        br(),
        sidebarLayout(
            sidebarPanel(
                selectInput('SV.input', 'type', c(choice.type, choice.value), selectize = TRUE)
            ),
            mainPanel(plotOutput("SV.plot"))
        ),
        
        verbatimTextOutput("summary")
    ),
    
    tabPanel(
        "PartA.",
        tags$h1("Box Plot"),
        sidebarLayout(
            sidebarPanel(
                selectInput('PA.type', 'type', choice.type, selectize = TRUE),
                selectInput('PA.value', 'Value', choice.value, selectize =
                                TRUE)
            ),
            mainPanel(plotOutput("PA.plot"))
        )
    ),
    
    tabPanel("Summary"),
    
    navbarMenu("More",
               plotOutput("plot"))
)