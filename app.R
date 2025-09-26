library(shiny)
library(rsconnect)
library(here) 
library(ggplot2)
library(RColorBrewer)
library(shinythemes)
library(DT)




clean = read.csv(here("cleaned.csv")) 

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
        selectInput("cat1", "Row variable",  choices = cat_choices),
        selectInput("cat2", "Column variable", choices = cat_choices),
        hr(),
        checkboxInput("flip_plot", "Flip coordinates", FALSE)
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Contingency Table",  DTOutput("chi_table")),
          tabPanel("Visualisation",   plotOutput("chi_plot")),
          tabPanel("Test Statistics",    uiOutput("chi_out"))
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
                    choices = t_choices),
        selectInput("tt_num2", "Numeric variable",
                    choices = t_choices)
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
                     
                     tabPanel("Test Output", 
                              br(), 
                              selectInput("tt_alt", "Test Tail", 
                              c("Two-sided" = "two.sided", 
                                "Group 1 > Group 2" = "greater", 
                                "Group 1 < Group 2" = "less")),  
                              uiOutput("shapiro_out"), 
                              uiOutput("test_advice"),            
                              
                              uiOutput("tt_out"))
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
  
  output$chi_table <- DT::renderDataTable({
    validate(need(input$cat1 != input$cat2, "Please choose two different variables!"))
    
    tab <- table(
      clean[[input$cat1]],
      clean[[input$cat2]],
      useNA = "no"
    )
    
    df <- as.data.frame.matrix(tab)        # rows = factor level of cat1
    DT::datatable(df,
                  options = list(dom = "t",                       # hide search/controls
                                 ordering = FALSE,
                                 pageLength = nrow(df)),
                  rownames = TRUE) |>
      DT::formatStyle(
        columns = names(df),
        backgroundColor = DT::styleColorBar(range(tab), "tomato"),   # red gradient
        backgroundSize   = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      DT::formatStyle(columns = names(df), color = "black")          # keep text dark
  })
  
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
  
  output$chi_out <- renderUI({
    tab  <- chi_data()
    test <- tryCatch(chisq.test(tab, correct = FALSE),
                     error = function(e) NULL)
    
    if (is.null(test)) {
      return(
        wellPanel(
          tags$p("Chi-square test could not be performed."),
          style = "background:#f8d7da; border-left:5px solid #721c24;"
        )
      )
    }
    
    #  Expected-count rule
    small_exp <- any(test$expected < 5)
    
    if (small_exp) {                       
      perm     <- chisq.test(tab, simulate.p.value = TRUE, B = 5000)
      stat_val <- unname(perm$statistic)
      p_val    <- perm$p.value
      method   <- "Independence test with permutation"
      exp_note <- "<b>Note:</b> expected counts&nbsp;&lt; 5, a permutation test was performed."
    } else {                                # classical Pearson version
      stat_val <- unname(test$statistic)
      p_val    <- test$p.value
      method   <- "Chi-square test"
      exp_note <- "<b>Note:</b> expected counts are sufficient for the standard chi-square test."
    }
    
    # ---- 2. P-value interpretation ----------------------------------------
    interp <- if (p_val < 0.05)
      "We reject the null hypothesis — a significant association exists."
    else
      "We fail to reject the null hypothesis; the two variables are independent."
    
    # Ouutput
    wellPanel(
      tags$h5(strong(method)),
      div(
        sprintf("Observed test statistic: %.3f", stat_val), tags$br(),
        sprintf("P-value: %.4g", p_val)
      ),
      div(HTML(exp_note), style = "margin-top:10px;"),
      div(interp,          style = "margin-top:10px; font-style:italic;"),
      style = "background:#f5faff; border-left:5px solid #17a2b8;"
    )
  })
  
  
  tt_data <- reactive({
    req(input$tt_num1, input$tt_num2)
    na.omit(clean[c(input$tt_num1, input$tt_num2)])
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
      stat_qq() + stat_qq_line(color="red") +
      labs(title = "QQ – Plot for variable 2") +
      theme_minimal()
    
    gridExtra::grid.arrange(qq1, qq2, ncol = 2)
  }) 
  output$tt_out <- renderUI({
    df <- tt_data()
    g  <- df[[input$tt_num1]]
    n  <- df[[input$tt_num2]]
    
    #  choose the right tesst
    non_normal <- (shapiro.test(g)$p.value < 0.05) ||
      (shapiro.test(n)$p.value < 0.05)
    
    if (non_normal) {
      res      <- wilcox.test(g, n, alternative = input$tt_alt)
      method   <- res$method                     # “Wilcoxon rank-sum …”
      stat_val <- unname(res$statistic)
      p_val    <- res$p.value
    } else {
      res      <- t.test(g, n, alternative = input$tt_alt)
      method   <- res$method                     # “Welch Two-Sample t-test”
      stat_val <- unname(res$statistic)
      p_val    <- res$p.value
    }
    
    #interpretation 
    interp <- if (p_val < 0.05)
      "We reject the null hypothesis — a significant difference exists between groups."
    else
      "We fail to reject the null hypothesis — no significant difference detected."
    
    # output card
    wellPanel(
      tags$h5(strong(method)),
      div(
        sprintf("Observed test statistic: %.3f", stat_val), tags$br(),
        sprintf("P-value: %.4g", p_val)
      ),
      div(interp, style = "margin-top:10px; font-style:italic;"),
      style = "background:#f5faff; border-left:5px solid #17a2b8;"
    )
  })
  output$shapiro_out <- renderUI({
    df <- tt_data()
    p1 <- shapiro.test(df[[input$tt_num1]])$p.value
    p2 <- shapiro.test(df[[input$tt_num2]])$p.value
    
    wellPanel(
      tags$h4("Shapiro-Wilk Normality Test"),
      tags$p(sprintf("P-value for %s: %.4f", input$tt_num1, p1)),
      tags$p(sprintf("P-value for %s: %.4f", input$tt_num2, p2)),
      style = "background:#fff3cd; border-left:5px solid #856404;"
    )
  })
  
    # Advisory message: which test will run
  output$test_advice <- renderUI({
    df <- tt_data()
    p1 <- shapiro.test(df[[input$tt_num1]])$p.value
    p2 <- shapiro.test(df[[input$tt_num2]])$p.value
    
    advice_html <- if (p1 < 0.05 || p2 < 0.05)
      "<b>Note:</b> normality failed for at least one group &nbsp;→&nbsp; Wilcoxon rank-sum test will be used."
    else
      "<b>Note:</b> both groups passed normality &nbsp;→&nbsp; Welch’s t-test will be used."
    
    # 20 px bottom margin creates the gap
    div(HTML(advice_html), style = "margin-bottom:20px;")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
