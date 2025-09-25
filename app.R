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
library(ggplot2)
library(RColorBrewer)
library(shinythemes)





clean = read.csv(here("cleaned.csv")) 
is_categorical <- function(x) {
  is.character(x) || is.factor(x) || n_distinct(na.omit(x)) <= 20
}
cat_vars <- names(clean)[vapply(clean, is_categorical, logical(1))]
num_vars <- names(clean)[vapply(clean, is.numeric,   logical(1))]


cat_choices <- c("target_grade", "assignment_preference")


ui <- navbarPage(
  title = "Survey Analysis",
  theme = shinytheme("flatly"),
  tabPanel(
    "Independence Test",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Choose variables"),
        selectInput("cat1", "Row variable",  choices = cat_choices),
        selectInput("cat2", "Column variable", choices = cat_choices),
        hr(),
        checkboxInput("show_exp", "Show expected counts", FALSE)
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel(icon("table"), "Contingency Table",  tableOutput("chi_table")),
          tabPanel(icon("chart-bar"), "Visualisation",   plotOutput("chi_plot")),
          tabPanel(icon("microscope"), "Test Statistics",    verbatimTextOutput("chi_out"))
        )
      )
    )
  )
)


server <- function(input, output, session) { 
  # Independence test UI 
  chi_data <- reactive({
    req(input$cat1, input$cat2)
    #validate(need(input$cat1 != input$cat2, "Choose two different variables"))
    table(clean[[input$cat1]], clean[[input$cat2]])
  })
  
  output$chi_table <- renderTable({
    if (input$show_exp) round(chisq.test(chi_data())$expected, 1)
    else                addmargins(chi_data())
  }, rownames = TRUE)
  
  output$chi_plot <- renderPlot({
    tab <- chi_data()
    barplot(tab,
            beside = TRUE,
            col    = brewer.pal(n = nrow(tab), "Set2"),
            legend = TRUE,
            ylab   = "Count")
  })
  
  output$chi_out <- renderPrint({
    chisq.test(chi_data(), correct = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
