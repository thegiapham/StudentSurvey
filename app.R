library(shiny)
library(rsconnect)
library(here) 
library(ggplot2)
library(RColorBrewer)
library(shinythemes)





clean = read.csv(here("cleaned.csv")) 
clean$target_grade
is_categorical <- function(x) {
  is.character(x) || is.factor(x) || n_distinct(na.omit(x)) <= 20
}
cat_vars <- names(clean)[vapply(clean, is_categorical, logical(1))]
num_vars <- names(clean)[vapply(clean, is.numeric,   logical(1))]

cat_choices <- c(
  "target_grade", 
  "assignment_preference",
  "trimester_or_semester",
  "tendency_yes_or_no",
  "pay_rent",
  "stall_choice",
  "living_arrangements",
  "believe_in_aliens",
  "work_status",
  "gender",
  "sleep_schedule",
  "diet_style",
  "drivers_license",
  "relationship_status",
  "computer_os",
  "steak_preference",
  "dominant_hand",
  "enrolled_unit",
  "assignments_on_time",
  "used_r_before",
  "team_role_type",
  "university_year",
  "country_of_birth"
)


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
        checkboxInput("flip_plot", "Flip coordinates", FALSE)
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
    validate(need(input$cat1 != input$cat2, "Please choose two different variables!"))  
    df_sub <- clean[, c(input$cat1, input$cat2)]
    
    df_sub <- na.omit(df_sub)
    table(df_sub[[input$cat1]], df_sub[[input$cat2]])
  })
  
  output$chi_table <- renderTable({
    validate(
      need(input$cat1 != input$cat2, "Please choose two different variables!")
    )
    tab <- table(
      clean[[input$cat1]],
      clean[[input$cat2]],
      useNA = "no"
    )
    
    # Always return as a data.frame-like table
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  
  output$chi_plot <- renderPlot({
    tab <- chi_data()
    df <- as.data.frame(tab)
    colnames(df) <- c("RowVar", "ColVar", "Freq")
    
    p <- ggplot(df, aes(x = RowVar, y = Freq, fill = ColVar)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = input$cat1,
        y = "Count",
        fill = input$cat2,
        title = paste("Distribution of", input$cat1, "by", input$cat2)
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    if (input$flip_plot) p <- p + coord_flip()
    
    p
  })
  
  output$chi_out <- renderPrint({ 
    tab <- chi_data() 
    test <- tryCatch(
      chisq.test(tab, correct = FALSE),
      error = function(e) NULL
    ) 
    if (!is.null(test)) {
      # Check if expected counts too small
      if (any(test$expected < 5)) {
        message("Using Monte Carlo simulation due to small expected frequencies")
        chisq.test(tab, simulate.p.value = TRUE, B = 5000)
      } else {
        test
      }
    } else {
      "Chi-square test could not be performed"
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
