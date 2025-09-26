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

t_choices = c(
  "age",
  "weetbix_count", 
  "weekly_food_spend",
  "weekly_alcohol",
  "height",
  "daily_anxiety_frequency",
  "weekly_study_hours",
  "average_daily_sleep",
  "sleep_schedule",
  "sibling_count",
  "allergy_count",
  "random_number",
  "favourite_number",
  "daily_short_video_time",
  "weekly_exercise_hours",
  "weekly_paid_work_hours",
  "team_role_type",
  "fluent_languages",
  "readable_languages",
  "wam",
  "shoe_size",
  "books_read_highschool",
  "daily_water_intake_l",
  "shoe_size_num")



# Shorten the label to fit the panel 
trim_name <- function(x, max_len = 14) {
  ifelse(nchar(x) > max_len,
         paste0(substr(x, 1, max_len - 3), "..."),
         x)
}

make_choices <- function(vec, max_len = 14) {
  setNames(vec, trim_name(vec, max_len))
}
# ----------------------------

#Helper functions for QQ-plot to keep the plot placed in the center 
symmetric_limits <- function(x, trim = 0.01, extend = 0.1) {
  qs       <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  max_abs  <- max(abs(qs))                 # 1 %–99 % band
  pad      <- max_abs * extend            # slight extra margin
  c( -max_abs - pad, max_abs + pad )
}
# ------------ 

ui <- navbarPage(
  title = "Survey Analysis",
  theme = shinytheme("flatly"),
  header = tags$head(
    # keep the selectize input itself from stretching horizontally
    tags$style(HTML("
      .selectize-input {max-width: 100%;}
    "))
  ),
  tabPanel(
    "Independence Test",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Choose variables"),
        selectInput("cat1", "Row variable",  choices = make_choices(cat_choices)),
        selectInput("cat2", "Column variable", choices = make_choices(cat_choices)),
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
  ), 
  tabPanel(
    "T-Test",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Choose variables"),
        selectInput("tt_num1", "Grouping variable (2 levels only)",
                    choices = make_choices(t_choices)),
        selectInput("tt_num2", "Numeric variable",
                    choices = make_choices(t_choices)),
        selectInput("tt_alt", "Alternative hypothesis",
                    c("Two-sided"       = "two.sided",
                      "Group 1 > Group 2" = "greater",
                      "Group 1 < Group 2" = "less"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel( id = "plot_tabs",
                     
                     tabPanel("Plots",             # parent
                              tabsetPanel( id = "which_plot",
                                           tabPanel("Boxplots", plotOutput("plot_boxes")),
                                           tabPanel("QQ-plots", plotOutput("plot_qq"))
                              )
                     ),
                     
                     tabPanel("Test Output", verbatimTextOutput("tt_out"))
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
  tt_data <- reactive({
    req(input$tt_num1, input$tt_num2)
    na.omit(clean[c(input$tt_num1, input$tt_num2)])
  })
  
  output$tt_box <- renderPlot({
    ggplot(tt_data(),
           aes_string(x = input$tt_num1, y = input$tt_num2)) +
      geom_boxplot(fill = "#3498db", alpha = .6) +
      labs(x = input$tt_num1, y = input$tt_num2) +
      theme_minimal()
  })
  
  output$plot_boxes <- renderPlot({
    df <- tt_data()
    
    p1 <- ggplot(df, aes_string(y = input$tt_num1)) +
      geom_boxplot(alpha = .6, width = .7) +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      labs(title = input$tt_num1, x = NULL, y = NULL) +
      theme_minimal()
    
    p2 <- ggplot(df, aes_string(y = input$tt_num2)) +
      geom_boxplot(alpha = .6, width = .7) +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      labs(title = input$tt_num2, x = NULL, y = NULL) +
      theme_minimal()
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })
  
  # 2. two QQ-plots
  output$plot_qq <- renderPlot({
    df <- tt_data()
    
    qq1 <- ggplot(df, aes(sample = .data[[input$tt_num1]])) +
      stat_qq() + stat_qq_line(color="red ") +
      labs(title = "QQ – Plot for variable 1") +
      theme_minimal()
    
    qq2 <- ggplot(df, aes(sample = .data[[input$tt_num2]])) +
      stat_qq() + stat_qq_line() +
      labs(title = "QQ – Plot for variable 2") +
      theme_minimal()
    
    gridExtra::grid.arrange(qq1, qq2, ncol = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
