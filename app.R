#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(rsconnect)
library(here) 
library(shinythemes)

raw = read.csv(here("raw.csv") )
clean = read.csv(here("cleaned_data.csv"))
ui <- navbarPage(
  title = "Survey Analysis",
  theme = shinytheme("flatly"),
  tabPanel(
    "Independence Test",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4(icon("th"), "Choose variables"),
        selectInput("ind_cat1", "Row variable",  choices = NULL),
        selectInput("ind_cat2", "Column variable", choices = NULL),
        hr(),
        checkboxInput("ind_expctd", "Show expected counts", FALSE)
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel(icon("table"), "Contingency Table",  tableOutput("ind_table")),
          tabPanel(icon("chart-bar"), "Visualisation",   plotOutput("ind_plot")),
          tabPanel(icon("microscope"), "Test Output",    verbatimTextOutput("ind_test"))
        )
      )
    )
  )
)


server <- function(input, output, session) {}


# Run the application 
shinyApp(ui = ui, server = server)
