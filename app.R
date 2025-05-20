# Load necessary libraries
library(shiny)
library(readxl)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(DT) # For interactive data tables
library(fitdistrplus)  # For distribution fitting
library(nortest)       # For additional normality tests

# Define UI for the application
ui <- fluidPage(
  titlePanel("Cox Regression and Kaplan-Meier Analysis by Robert Vanagas 0.5"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Excel File", accept = c(".xlsx")),
      uiOutput("sheet_selector"), # UI for selecting the Excel sheet
      uiOutput("select_vars"), # Dynamic UI for selecting variables
      
      # Outlier Detection Section
      hr(),
      h4("Outlier Detection"),
      checkboxInput("exclude_outliers", "Exclude Outliers from Analysis", value = FALSE),
      conditionalPanel(
        condition = "input.exclude_outliers == true",
        selectInput("outlier_method", "Select Outlier Detection Method:",
                    choices = c("IQR Method", "Z-Score Method", "Modified Z-Score Method", "Percentile Method")),
        conditionalPanel(
          condition = "input.outlier_method == 'Percentile Method'",
          sliderInput("outlier_percentile", "Outlier Percentile Threshold:", min = 0, max = 100, value = c(5, 95))
        )
      ),
      
      # Variable Transformation Section
      hr(),
      h4("Variable Transformation"),
      checkboxInput("apply_transformation", "Apply Transformation to Main Variable", value = FALSE),
      conditionalPanel(
        condition = "input.apply_transformation == true",
        selectInput("transformation_method", "Select Transformation:",
                    choices = c("Log Transformation", "Square Root Transformation"))
      ),
      
      # Plot Customization
      hr(),
      h4("Plot Customization"),
      selectInput("plot_theme", "Select Plot Theme:", 
                  choices = c("Minimal" = "theme_minimal",
                              "Classic" = "theme_classic",
                              "Gray" = "theme_gray",
                              "Black & White" = "theme_bw")),
      selectInput("color_palette", "Select Color Palette:", 
                  choices = c("Default" = "default",
                              "Blue-Green" = "blue_green",
                              "Purple-Orange" = "purple_orange",
                              "Red-Blue" = "red_blue")),
      
      # Grouping Options
      checkboxInput("use_grouping", "Divide Patients into Groups Based on Biomarker Expression", value = TRUE),
      
      conditionalPanel(
        condition = "input.use_grouping == true",
        selectInput("grouping_method", "Select Grouping Method:", 
                    choices = c("Median", "Custom Percentage", "Optimized for Lowest P-value", 
                                "Top and Bottom Quartiles")),
        conditionalPanel(
          condition = "input.grouping_method == 'Custom Percentage'",
          sliderInput("custom_percentage", "Custom Percentage for High Group:", min = 0, max = 100, value = 50)
        )
      ),
      
      hr(),
      actionButton("run_analysis", "Run Analysis"),
      
      # Download Buttons for Analysis Plots and Summaries
      hr(),
      h4("Download Analysis Outputs"),
      downloadButton("download_forest_uni", "Download Univariate Forest Plot"),
      downloadButton("download_forest_multi", "Download Multivariate Forest Plot"),
      downloadButton("download_km_plot", "Download Kaplan-Meier Plot"),
      downloadButton("download_schoenfeld_uni", "Download Univariate Schoenfeld Plot"),
      downloadButton("download_schoenfeld_multi", "Download Multivariate Schoenfeld Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Univariate Analysis", 
                 plotOutput("forest_uni"),
                 plotOutput("schoenfeld_uni"),
                 verbatimTextOutput("schoenfeld_test_uni"),
                 textInput("uni_plot_title", "Plot Title:", "Forest Plot for Univariate Analysis"),
                 numericInput("uni_font_size", "Font Size:", value = 1, min = 0, max = 24),
                 downloadButton("download_forest_uni", "Download Univariate Forest Plot"),
                 downloadButton("download_schoenfeld_uni", "Download Univariate Schoenfeld Plot")
        ),
        tabPanel("Multivariate Analysis", 
                 plotOutput("forest_multi"),
                 plotOutput("schoenfeld_multi"),
                 verbatimTextOutput("schoenfeld_test_multi"),
                 textInput("multi_plot_title", "Plot Title:", "Forest Plot for Multivariate Analysis with Grouping"),
                 numericInput("multi_font_size", "Font Size:", value = 1, min = 0, max = 24),
                 textInput("schoenfeld_multi_title", "Plot Title Schoenfeld:", "Schoenfeld Residuals Plot"),
                 downloadButton("download_forest_multi", "Download Multivariate Forest Plot"),
                 downloadButton("download_schoenfeld_multi", "Download Multivariate Schoenfeld Plot")
        ),
        tabPanel("Kaplan-Meier Plot", 
                 plotOutput("km_plot"),
                 textInput("km_plot_title", "Plot Title:", "Kaplan-Meier Plot"),
                 numericInput("km_font_size", "Font Size:", value = 12, min = 0, max = 24),
                 checkboxInput("show_conf_intervals", "Show Confidence Intervals", value = TRUE),  
                 checkboxInput("show_risk_table", "Show Risk Table", value = TRUE),
                 
                 # Inputs for X-axis Customization
                 numericInput("x_max", "Maximum X-axis Value (e.g., 60 for 5 years):", value = 60, min = 1),
                 numericInput("x_breaks", "X-axis Break Interval (e.g., 12 for yearly ticks):", value = 12, min = 1),
                 downloadButton("download_km_plot", "Download Kaplan-Meier Plot")
        ),
        tabPanel("Data Inspection",
                 DT::dataTableOutput("data_table"),
                 downloadButton("download_data", "Download Inspected Data")
        ),
        tabPanel("Grouping Verification",
                 h4("Group Summary"),
                 tableOutput("group_summary"),
                 
                 h4("Biomarker Distribution with Grouping Cutoff"),
                 plotOutput("group_plot"),
                 downloadButton("download_group_plot", "Download Biomarker Distribution Plot"),
                 
                 h4("Biomarker Boxplot by Group"),
                 plotOutput("boxplot_group"),
                 downloadButton("download_boxplot_group", "Download Biomarker Boxplot"),
                 
                 h4("Group Summary Download"),
                 downloadButton("download_group_summary", "Download Group Summary")
        ),
        # Updated Variable Distribution Tab
        tabPanel("Variable Distribution",
                 h4("Distribution of Main Variable"),
                 plotOutput("distribution_plot"),
                 plotOutput("qq_plot"),
                 verbatimTextOutput("distribution_summary"),
                 verbatimTextOutput("shapiro_test"),
                 verbatimTextOutput("distribution_check"),  # Added Output
                 verbatimTextOutput("recommendation")       # Added Output
        )
      )
    )
  )
)
# Define server logic
server <- function(input, output, session) {
  
  ####################################### USER INPUT LOGIC ###############################################################  
  
  # Reactive expression to get sheet names from the uploaded Excel file
  sheet_names <- reactive({
    req(input$file1)
    excel_sheets(input$file1$datapath)
  })
  
  # Dynamic UI for selecting the sheet from the Excel file
  output$sheet_selector <- renderUI({
    req(sheet_names())
    selectInput("sheet", "Select Sheet:", choices = sheet_names())
  })
  
  # Reactive expression to read the data from the selected sheet
  dataset <- reactive({
    req(input$file1, input$sheet)
    df <- read_excel(input$file1$datapath, sheet = input$sheet)
    
    # Store original column names
    original_names <- names(df)
    
    # Ensure column names are valid for R
    sanitized_names <- make.names(original_names, unique = TRUE)
    names(df) <- sanitized_names
    
    # Create a mapping dataframe
    name_mapping <- data.frame(
      sanitized = sanitized_names,
      original = original_names,
      stringsAsFactors = FALSE
    )
    
    # Store the mapping as an attribute
    attr(df, "name_mapping") <- name_mapping
    
    df
  })
  
  # Dynamic UI to select variables for the analysis
  output$select_vars <- renderUI({
    req(dataset())
    var_names <- names(dataset())
    tagList(
      selectizeInput("main_vars", "Select Main Variable(s) (e.g., Biomarkers) to Combine:", choices = var_names, multiple = TRUE),
      selectInput("time_var", "Select Time Variable (Ex: days/months)", choices = var_names),
      selectInput("event_var", "Select binary event Variable, (0=censored, 1=death/event)", choices = var_names),
      selectizeInput("co_vars", "Co-Variables for multivariate analysis (e.g., Age, Gender, Smoking, HPV)", choices = var_names, multiple = TRUE)
    )
  })
  
  ####################################### FUNCTIONS TO HANDLE INPUTS #####################################################
  
  # Reactive expression to combine selected main variables and handle outlier detection
  combined_main_var <- reactive({
    req(input$main_vars)
    df <- dataset()
    
    # Ensure only numeric columns are included
    numeric_vars <- sapply(df[, input$main_vars, drop = FALSE], is.numeric)
    if (all(numeric_vars)) {
      combined_main <- rowSums(df[, input$main_vars, drop = FALSE], na.rm = TRUE)
    } else {
      # Handle non-numeric data in input$main_vars
      combined_main <- rowSums(as.data.frame(lapply(df[, input$main_vars, drop = FALSE], function(x) as.numeric(as.character(x)))), na.rm = TRUE)
    }
    df$combined_main <- combined_main
    
    # Apply transformation if selected
    if (input$apply_transformation) {
      if (input$transformation_method == "Log Transformation") {
        df$combined_main <- log(df$combined_main + 1) # Adding 1 to avoid log(0)
      } else if (input$transformation_method == "Square Root Transformation") {
        df$combined_main <- sqrt(df$combined_main)
      }
    }
    
    # Perform outlier detection on combined_main if exclude_outliers is TRUE
    if (input$exclude_outliers) {
      method <- input$outlier_method
      
      if (method == "IQR Method") {
        Q1 <- quantile(df$combined_main, 0.25, na.rm = TRUE)
        Q3 <- quantile(df$combined_main, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        df <- df %>% dplyr::filter(combined_main >= lower_bound & combined_main <= upper_bound)
        
      } else if (method == "Z-Score Method") {
        mean_val <- mean(df$combined_main, na.rm = TRUE)
        sd_val <- sd(df$combined_main, na.rm = TRUE)
        z_scores <- (df$combined_main - mean_val) / sd_val
        df <- df %>% dplyr::filter(abs(z_scores) <= 3)
        
      } else if (method == "Modified Z-Score Method") {
        median_val <- median(df$combined_main, na.rm = TRUE)
        mad_val <- mad(df$combined_main, na.rm = TRUE)
        modified_z_scores <- 0.6745 * (df$combined_main - median_val) / mad_val
        df <- df %>% dplyr::filter(abs(modified_z_scores) <= 3.5)
        
      } else if (method == "Percentile Method") {
        lower_percentile <- input$outlier_percentile[1] / 100
        upper_percentile <- input$outlier_percentile[2] / 100
        lower_bound <- quantile(df$combined_main, lower_percentile, na.rm = TRUE)
        upper_bound <- quantile(df$combined_main, upper_percentile, na.rm = TRUE)
        df <- df %>% dplyr::filter(combined_main >= lower_bound & combined_main <= upper_bound)
      }
    }
    
    df
  })
  
  # Generate a separate dataframe for Kaplan-Meier analysis
  km_data <- reactive({
    df <- combined_main_var()
    # Ensure the time and event variables are formatted correctly for Kaplan-Meier
    df <- df %>% 
      dplyr::select(input$time_var, input$event_var, combined_main = "combined_main") %>%
      dplyr::mutate(
        time = as.numeric(get(input$time_var)), 
        event = as.numeric(get(input$event_var))
      )
    
    # Check grouping method and divide patients into groups accordingly
    if (input$use_grouping) {
      if (input$grouping_method == "Median") {
        median_value <- median(df$combined_main, na.rm = TRUE)
        df$group <- factor(ifelse(df$combined_main >= median_value, "High", "Low"), levels = c("Low", "High"))
      } else if (input$grouping_method == "Custom Percentage") {
        cutoff <- quantile(df$combined_main, probs = input$custom_percentage / 100, na.rm = TRUE)
        df$group <- factor(ifelse(df$combined_main >= cutoff, "High", "Low"), levels = c("Low", "High"))
      } else if (input$grouping_method == "Optimized for Lowest P-value") {
        best_cutoff <- optimize_cutoff_uni(df, "combined_main")
        df$group <- factor(ifelse(df$combined_main >= best_cutoff, "High", "Low"), levels = c("Low", "High"))
      } else if (input$grouping_method == "Top and Bottom Quartiles") {
        Q1 <- quantile(df$combined_main, 0.25, na.rm = TRUE)
        Q3 <- quantile(df$combined_main, 0.75, na.rm = TRUE)
        df$group <- factor(ifelse(df$combined_main >= Q3, "High",
                                  ifelse(df$combined_main <= Q1, "Low", NA)), levels = c("Low", "High"))
        # Exclude middle 50%
        df <- df %>% dplyr::filter(!is.na(group))
      }
    } else {
      df$group <- df$combined_main 
    }
    
    df
  })
  
  ####################################### KAPLAN-MEIER ###############################################################
  # Reactive expression for generating Kaplan-Meier plot
  output$km_plot <- renderPlot({
    req(km_data())
    df <- km_data()
    
    # Define color palette based on user selection
    palette_colors <- switch(input$color_palette,
                             "default" = c("#E7B800", "#2E9FDF"),
                             "blue_green" = c("#1f78b4", "#33a02c"),
                             "purple_orange" = c("#6a3d9a", "#fb9a99"),
                             "red_blue" = c("#e31a1c", "#1f78b4"),
                             c("#E7B800", "#2E9FDF")) # Default fallback
    
    fit <- survfit(Surv(time, event) ~ group, data = df)
    
    ggsurvplot(fit, data = df, 
               pval = TRUE, 
               title = input$km_plot_title, 
               font.main = input$km_font_size, 
               risk.table = input$show_risk_table,
               pval.method = TRUE,
               xlim = c(0, input$x_max),
               break.x.by = input$x_breaks,
               xlab = "Time",
               conf.int = input$show_conf_intervals,
               palette = palette_colors,
               ggtheme = switch(input$plot_theme,
                                "theme_minimal" = theme_minimal(),
                                "theme_classic" = theme_classic(),
                                "theme_gray" = theme_gray(),
                                "theme_bw" = theme_bw(),
                                theme_minimal())
    )
  })
  
  ####################################### OPTIMIZE P-Value ########################################################### 
  # Function to find the optimal cutoff for univariate analysis
  optimize_cutoff_uni <- function(df, biomarker_col) {
    best_pvalue <- 1
    best_cutoff <- median(df[[biomarker_col]], na.rm = TRUE)
    
    for (perc in seq(10, 90, by = 1)) {
      cutoff <- quantile(df[[biomarker_col]], probs = perc / 100, na.rm = TRUE)
      df$temp_group <- factor(ifelse(df[[biomarker_col]] >= cutoff, "High", "Low"), levels = c("Low", "High"))
      fit <- coxph(Surv(time, event) ~ temp_group, data = df)
      pvalue <- summary(fit)$sctest["pvalue"]
      
      if (!is.na(pvalue) && pvalue < best_pvalue) {
        best_pvalue <- pvalue
        best_cutoff <- cutoff
      }
    }
    
    return(best_cutoff)
  }
  
  # Function to find the optimal cutoff for multivariate analysis
  optimize_cutoff_multi <- function(df, biomarker_col, co_vars) {
    best_pvalue <- 1
    best_cutoff <- median(df[[biomarker_col]], na.rm = TRUE)
    
    for (perc in seq(10, 90, by = 1)) {
      cutoff <- quantile(df[[biomarker_col]], probs = perc / 100, na.rm = TRUE)
      df$temp_group <- factor(ifelse(df[[biomarker_col]] >= cutoff, "High", "Low"), levels = c("Low", "High"))
      formula <- as.formula(paste("Surv(time, event) ~ temp_group + ", paste(co_vars, collapse = " + ")))
      fit <- coxph(formula, data = df)
      pvalue <- summary(fit)$sctest["pvalue"]
      
      if (!is.na(pvalue) && pvalue < best_pvalue) {
        best_pvalue <- pvalue
        best_cutoff <- cutoff
      }
    }
    
    return(best_cutoff)
  }
  
  # Reactive expression to divide patients into two groups for univariate analysis
  patient_groups_uni <- reactive({
    req(input$main_vars)
    df <- combined_main_var()
    
    # Add standardized 'time' and 'event' columns
    df <- df %>%
      dplyr::mutate(
        time = as.numeric(get(input$time_var)),
        event = as.numeric(get(input$event_var))
      )
    
    if (length(input$main_vars) > 1) {
      biomarker_col <- "combined_main"
    } else {
      biomarker_col <- input$main_vars
    }
    
    if (input$use_grouping) {
      if (input$grouping_method == "Median") {
        median_value <- median(df[[biomarker_col]], na.rm = TRUE)
        df$group <- factor(ifelse(df[[biomarker_col]] >= median_value, "High", "Low"), levels = c("Low", "High"))
        
      } else if (input$grouping_method == "Custom Percentage") {
        cutoff <- quantile(df[[biomarker_col]], probs = input$custom_percentage / 100, na.rm = TRUE)
        df$group <- factor(ifelse(df[[biomarker_col]] >= cutoff, "High", "Low"), levels = c("Low", "High"))
        
      } else if (input$grouping_method == "Optimized for Lowest P-value") {
        best_cutoff <- optimize_cutoff_uni(df, biomarker_col)
        df$group <- factor(ifelse(df[[biomarker_col]] >= best_cutoff, "High", "Low"), levels = c("Low", "High"))
      } else if (input$grouping_method == "Top and Bottom Quartiles") {
        Q1 <- quantile(df[[biomarker_col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(df[[biomarker_col]], 0.75, na.rm = TRUE)
        df$group <- factor(ifelse(df[[biomarker_col]] >= Q3, "High",
                                  ifelse(df[[biomarker_col]] <= Q1, "Low", NA)), levels = c("Low", "High"))
        # Exclude middle 50%
        df <- df %>% dplyr::filter(!is.na(group))
      }
    } else {
      df$group <- df[[biomarker_col]] 
    }
    
    df
  })
  
  # Reactive expression to divide patients into two groups for multivariate analysis
  patient_groups_multi <- reactive({
    req(input$main_vars, input$co_vars)
    df <- combined_main_var()
    
    # Add standardized 'time' and 'event' columns
    df <- df %>%
      dplyr::mutate(
        time = as.numeric(get(input$time_var)),
        event = as.numeric(get(input$event_var))
      )
    
    if (length(input$main_vars) > 1) {
      biomarker_col <- "combined_main"
    } else {
      biomarker_col <- input$main_vars
    }
    
    if (input$use_grouping) {
      if (input$grouping_method == "Median") {
        median_value <- median(df[[biomarker_col]], na.rm = TRUE)
        df$group <- factor(ifelse(df[[biomarker_col]] >= median_value, "High", "Low"), levels = c("Low", "High"))
        
      } else if (input$grouping_method == "Custom Percentage") {
        cutoff <- quantile(df[[biomarker_col]], probs = input$custom_percentage / 100, na.rm = TRUE)
        df$group <- factor(ifelse(df[[biomarker_col]] >= cutoff, "High", "Low"), levels = c("Low", "High"))
        
      } else if (input$grouping_method == "Optimized for Lowest P-value") {
        best_cutoff <- optimize_cutoff_multi(df, biomarker_col, input$co_vars)
        df$group <- factor(ifelse(df[[biomarker_col]] >= best_cutoff, "High", "Low"), levels = c("Low", "High"))
      } else if (input$grouping_method == "Top and Bottom Quartiles") {
        Q1 <- quantile(df[[biomarker_col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(df[[biomarker_col]], 0.75, na.rm = TRUE)
        df$group <- factor(ifelse(df[[biomarker_col]] >= Q3, "High",
                                  ifelse(df[[biomarker_col]] <= Q1, "Low", NA)), levels = c("Low", "High"))
        # Exclude middle 50%
        df <- df %>% dplyr::filter(!is.na(group))
      }
    } else {
      df$group <- df[[biomarker_col]] 
    }
    
    df
  })
  
  ####################################### COX ANALYSIS ###############################################################  
  # Reactive expression to perform Univariate Cox Regression
  univariate_cox <- reactive({
    req(input$time_var, input$event_var)
    df <- patient_groups_uni()
    
    # Create the formula
    formula <- as.formula("Surv(time, event) ~ group")
    coxph(formula, data = df)
  })
  
  # Reactive expression to perform Multivariate Cox Regression
  multivariate_cox <- reactive({
    req(input$time_var, input$event_var, input$co_vars)
    df <- patient_groups_multi()
    
    # Create the formula
    covariate_terms <- paste(c("group", input$co_vars), collapse = " + ")
    formula <- as.formula(paste("Surv(time, event) ~ ", covariate_terms))
    coxph(formula, data = df)
  })
  
  ####################################### COX PLOTS ############################################################### 
  # Render forest plot for Univariate Analysis
  output$forest_uni <- renderPlot({
    req(univariate_cox())
    res <- univariate_cox()
    
    ggforest(res, data = model.frame(res), 
             main = input$uni_plot_title, 
             fontsize = input$uni_font_size,
             noDigits = 3) +
      switch(input$plot_theme,
             "theme_minimal" = theme_minimal(),
             "theme_classic" = theme_classic(),
             "theme_gray" = theme_gray(),
             "theme_bw" = theme_bw(),
             theme_minimal())
  })
  
  # Render forest plot for Multivariate Analysis
  output$forest_multi <- renderPlot({
    req(multivariate_cox())
    res <- multivariate_cox()
    
    ggforest(res, data = model.frame(res), 
             main = input$multi_plot_title, 
             fontsize = input$multi_font_size,
             noDigits = 2) +
      switch(input$plot_theme,
             "theme_minimal" = theme_minimal(),
             "theme_classic" = theme_classic(),
             "theme_gray" = theme_gray(),
             "theme_bw" = theme_bw(),
             theme_minimal())
  })
  
  ####################################### SCHOENFELD RESIDUALS #####################################################
  
  # Render Schoenfeld residuals plot for Univariate Analysis
  output$schoenfeld_uni <- renderPlot({
    req(univariate_cox())
    res <- univariate_cox()
    schoenfeld <- cox.zph(res)
    
    # Define color palette based on user selection
    plot_color <- switch(input$color_palette,
                         "default" = "#2E9FDF",
                         "blue_green" = "#1f78b4",
                         "purple_orange" = "#6a3d9a",
                         "red_blue" = "#e31a1c",
                         "#2E9FDF")
    
    plot(schoenfeld, main = "Schoenfeld Residuals for Univariate Analysis", col = plot_color, lwd = 2)
  })
  
  # Render Schoenfeld residuals plot for Multivariate Analysis
  output$schoenfeld_multi <- renderPlot({
    req(multivariate_cox())
    res <- multivariate_cox()
    schoenfeld <- cox.zph(res)
    
    # Define color palette based on user selection
    plot_color <- switch(input$color_palette,
                         "default" = "#E7B800",
                         "blue_green" = "#33a02c",
                         "purple_orange" = "#fb9a99",
                         "red_blue" = "#1f78b4",
                         "#E7B800")
    
    plot(schoenfeld, main = input$schoenfeld_multi_title, col = plot_color, lwd = 2)
  })
  
  # Function to perform Schoenfeld residuals test
  schoenfeld_test <- function(cox_model) {
    cox.zph(cox_model)
  }
  
  # Output for Schoenfeld residuals test for Univariate Analysis
  output$schoenfeld_test_uni <- renderPrint({
    req(univariate_cox())
    test_result <- schoenfeld_test(univariate_cox())
    print(test_result)
  })
  
  # Output for Schoenfeld residuals test for Multivariate Analysis
  output$schoenfeld_test_multi <- renderPrint({
    req(multivariate_cox())
    test_result <- schoenfeld_test(multivariate_cox())
    print(test_result)
  })
  
  ####################################### DATA INSPECTION ###########################################################
  
  # Reactive expression to gather selected columns for inspection
  selected_columns <- reactive({
    req(dataset())  # Ensure dataset is available
    
    # Collect all selected columns: main_vars, time_var, event_var, co_vars
    vars <- c(input$main_vars, input$time_var, input$event_var, input$co_vars)
    
    # Remove any NULLs, NAs, or empty strings
    vars <- vars[!is.null(vars) & vars != "" & !is.na(vars)]
    
    # Remove duplicates
    unique(vars)
  })
  
  # Render the data table in the "Data Inspection" tab
  output$data_table <- DT::renderDataTable({
    req(selected_columns())  # Ensure columns are selected
    
    # Subset the dataset to include only the selected columns
    df <- combined_main_var()[, selected_columns(), drop = FALSE]
    
    # Retrieve the name mapping
    name_mapping <- attr(dataset(), "name_mapping")
    
    # Create a mapping vector: sanitized to original
    mapping <- setNames(name_mapping$original, name_mapping$sanitized)
    
    # Get display names based on sanitized column names
    display_names <- mapping[selected_columns()]
    
    # Handle any missing mappings
    if(any(is.na(display_names))){
      warning_cols <- selected_columns()[is.na(display_names)]
      showNotification(paste("Warning: The following columns could not be matched and will retain their sanitized names:", 
                             paste(warning_cols, collapse = ", ")), type = "warning")
      display_names[is.na(display_names)] <- selected_columns()[is.na(display_names)]
    }
    
    # Rename the columns for display
    colnames(df) <- display_names
    
    # Optionally, limit rows for large datasets
    if(nrow(df) > 100) {
      df <- df[1:100, ]
    }
    
    # Render the data table with interactive features
    DT::datatable(df, 
                  options = list(pageLength = 10, 
                                 scrollX = TRUE, 
                                 autoWidth = TRUE,
                                 columnDefs = list(list(targets = "_all", className = "dt-left")),
                                 fixedHeader = TRUE,
                                 dom = 'lfrtip'),
                  rownames = FALSE)
  })
  
  ####################################### GROUPING VERIFICATION #####################################################
  # Output for Group Summary
  output$group_summary <- renderTable({
    req(km_data())
    df <- km_data()
    if (input$use_grouping) {
      summary_df <- df %>%
        group_by(group) %>%
        summarise(
          Count = n(),
          Median_Biomarker = round(median(combined_main, na.rm = TRUE), 2),
          Mean_Time = round(mean(time, na.rm = TRUE), 2),
          Events = sum(event, na.rm = TRUE)
        )
    } else {
      summary_df <- data.frame(
        Group = "All",
        Count = nrow(df),
        Median_Biomarker = round(median(df$combined_main, na.rm = TRUE), 2),
        Mean_Time = round(mean(df$time, na.rm = TRUE), 2),
        Events = sum(df$event, na.rm = TRUE)
      )
      colnames(summary_df) <- c("Group", "Count", "Median_Biomarker", "Mean_Time", "Events")
    }
    summary_df
  })
  
  # Output for Group Plot
  output$group_plot <- renderPlot({
    req(km_data())
    df <- km_data()
    if (input$use_grouping) {
      if (input$grouping_method == "Top and Bottom Quartiles") {
        Q1 <- quantile(df$combined_main, 0.25, na.rm = TRUE)
        Q3 <- quantile(df$combined_main, 0.75, na.rm = TRUE)
        cutoff_values <- c(Q1, Q3)
      } else if (input$grouping_method == "Median") {
        median_value <- median(df$combined_main, na.rm = TRUE)
        cutoff_values <- median_value
      } else if (input$grouping_method == "Custom Percentage") {
        cutoff <- quantile(df$combined_main, probs = input$custom_percentage / 100, na.rm = TRUE)
        cutoff_values <- cutoff
      } else if (input$grouping_method == "Optimized for Lowest P-value") {
        best_cutoff <- optimize_cutoff_uni(df, "combined_main")
        cutoff_values <- best_cutoff
      }
      
      # Define color palette based on user selection
      palette_colors <- switch(input$color_palette,
                               "default" = c("#E7B800", "#2E9FDF"),
                               "blue_green" = c("#1f78b4", "#33a02c"),
                               "purple_orange" = c("#6a3d9a", "#fb9a99"),
                               "red_blue" = c("#e31a1c", "#1f78b4"),
                               c("#E7B800", "#2E9FDF")) # Default fallback
      
      ggplot(df, aes(x = combined_main, fill = group)) +
        geom_histogram(binwidth = (max(df$combined_main, na.rm = TRUE) - min(df$combined_main, na.rm = TRUE))/30, alpha = 0.6, position = "identity", color = "black") +
        { if (input$grouping_method == "Top and Bottom Quartiles") 
          geom_vline(xintercept = cutoff_values, color = "red", linetype = "dashed", size = 1) 
          else 
            geom_vline(xintercept = cutoff_values, color = "red", linetype = "dashed", size = 1) 
        } +
        labs(title = "Biomarker Distribution with Grouping Cutoff",
             x = "Biomarker Expression",
             y = "Count",
             fill = "Group") +
        scale_fill_manual(values = palette_colors) +
        switch(input$plot_theme,
               "theme_minimal" = theme_minimal(),
               "theme_classic" = theme_classic(),
               "theme_gray" = theme_gray(),
               "theme_bw" = theme_bw(),
               theme_minimal()) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    } else {
      # Define color palette based on user selection
      palette_colors <- switch(input$color_palette,
                               "default" = "#2E9FDF",
                               "blue_green" = "#1f78b4",
                               "purple_orange" = "#6a3d9a",
                               "red_blue" = "#e31a1c",
                               "#2E9FDF") # Default fallback
      
      ggplot(df, aes(x = combined_main)) +
        geom_histogram(binwidth = (max(df$combined_main, na.rm = TRUE) - min(df$combined_main, na.rm = TRUE))/30, fill = palette_colors, alpha = 0.7, color = "black") +
        labs(title = "Biomarker Distribution",
             x = "Biomarker Expression",
             y = "Count") +
        switch(input$plot_theme,
               "theme_minimal" = theme_minimal(),
               "theme_classic" = theme_classic(),
               "theme_gray" = theme_gray(),
               "theme_bw" = theme_bw(),
               theme_minimal()) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14)
        )
    }
  })
  
  # Output for Boxplot by Group
  output$boxplot_group <- renderPlot({
    req(km_data())
    df <- km_data()
    
    # Define color palette based on user selection
    palette_colors <- switch(input$color_palette,
                             "default" = c("#E7B800", "#2E9FDF"),
                             "blue_green" = c("#1f78b4", "#33a02c"),
                             "purple_orange" = c("#6a3d9a", "#fb9a99"),
                             "red_blue" = c("#e31a1c", "#1f78b4"),
                             c("#E7B800", "#2E9FDF")) # Default fallback
    
    if (input$use_grouping) {
      ggplot(df, aes(x = group, y = combined_main, fill = group)) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
        labs(title = "Biomarker Expression by Group",
             x = "Group",
             y = "Biomarker Expression") +
        scale_fill_manual(values = palette_colors) +
        switch(input$plot_theme,
               "theme_minimal" = theme_minimal(),
               "theme_classic" = theme_classic(),
               "theme_gray" = theme_gray(),
               "theme_bw" = theme_bw(),
               theme_minimal()) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          legend.position = "none"
        )
    } else {
      # Define color palette based on user selection
      palette_colors_single <- switch(input$color_palette,
                                      "default" = "#2E9FDF",
                                      "blue_green" = "#1f78b4",
                                      "purple_orange" = "#6a3d9a",
                                      "red_blue" = "#e31a1c",
                                      "#2E9FDF") # Default fallback
      
      ggplot(df, aes(y = combined_main)) +
        geom_boxplot(fill = palette_colors_single, alpha = 0.7, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
        labs(title = "Biomarker Expression",
             y = "Biomarker Expression") +
        switch(input$plot_theme,
               "theme_minimal" = theme_minimal(),
               "theme_classic" = theme_classic(),
               "theme_gray" = theme_gray(),
               "theme_bw" = theme_bw(),
               theme_minimal()) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14)
        )
    }
  })
  
  ####################################### VARIABLE DISTRIBUTION #####################################################
  
  ####################################### VARIABLE DISTRIBUTION #####################################################
  
  # Reactive expression to perform distribution tests
  distribution_tests <- reactive({
    req(combined_main_var())
    df <- combined_main_var()
    
    # Shapiro-Wilk test on original data
    if (nrow(df) >= 3 && nrow(df) <= 5000) {
      shapiro_original <- shapiro.test(df$combined_main)
      p_original <- shapiro_original$p.value
    } else {
      p_original <- NA
    }
    
    # Shapiro-Wilk test on log-transformed data
    if (all(df$combined_main > 0)) {
      log_transformed <- log(df$combined_main)
      if (nrow(df) >= 3 && nrow(df) <= 5000) {
        shapiro_log <- shapiro.test(log_transformed)
        p_log <- shapiro_log$p.value
      } else {
        p_log <- NA
      }
    } else {
      p_log <- NA
    }
    
    list(p_original = p_original, p_log = p_log)
  })
  
  # Output for Distribution Plot
  output$distribution_plot <- renderPlot({
    req(combined_main_var())
    df <- combined_main_var()
    
    ggplot(df, aes(x = combined_main)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#69b3a2", color = "black", alpha = 0.7) +
      geom_density(color = "blue", size = 1) +
      labs(title = "Histogram and Density Plot of Main Variable",
           x = "Main Variable (Combined Biomarker)",
           y = "Density") +
      theme_minimal()
  })
  
  # Output for QQ Plot
  output$qq_plot <- renderPlot({
    req(combined_main_var())
    df <- combined_main_var()
    
    qqnorm(df$combined_main, main = "QQ Plot of Main Variable")
    qqline(df$combined_main, col = "red", lwd = 2)
  })
  
  # Output for Distribution Summary
  output$distribution_summary <- renderPrint({
    req(combined_main_var())
    df <- combined_main_var()
    summary(df$combined_main)
  })
  

  
  ####################################### DOWNLOAD HANDLERS #####################################################################################################################
  
  # Download handler for Univariate Forest Plot
  output$download_forest_uni <- downloadHandler(
    filename = function() {
      paste("univariate_forest_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      res <- univariate_cox()
      p <- ggforest(res, data = model.frame(res), 
                    main = input$uni_plot_title, fontsize = input$uni_font_size,
                    noDigits = 3) +
        switch(input$plot_theme,
               "theme_minimal" = theme_minimal(),
               "theme_classic" = theme_classic(),
               "theme_gray" = theme_gray(),
               "theme_bw" = theme_bw(),
               theme_minimal())
      print(p)
      dev.off()
    }
  )
  
  # Download handler for Multivariate Forest Plot
  output$download_forest_multi <- downloadHandler(
    filename = function() {
      paste("multivariate_forest_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      res <- multivariate_cox()
      p <- ggforest(res, data = model.frame(res), 
                    main = input$multi_plot_title, fontsize = input$multi_font_size,
                    noDigits = 2) +
        switch(input$plot_theme,
               "theme_minimal" = theme_minimal(),
               "theme_classic" = theme_classic(),
               "theme_gray" = theme_gray(),
               "theme_bw" = theme_bw(),
               theme_minimal())
      print(p)
      dev.off()
    }
  )
  
  # Download handler for Kaplan-Meier Plot
  output$download_km_plot <- downloadHandler(
    filename = function() {
      paste("kaplan_meier_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      df <- km_data()
      
      # Define color palette based on user selection
      palette_colors <- switch(input$color_palette,
                               "default" = c("#E7B800", "#2E9FDF"),
                               "blue_green" = c("#1f78b4", "#33a02c"),
                               "purple_orange" = c("#6a3d9a", "#fb9a99"),
                               "red_blue" = c("#e31a1c", "#1f78b4"),
                               c("#E7B800", "#2E9FDF")) # Default fallback
      
      p <- ggsurvplot(survfit(Surv(time, event) ~ group, data = df), data = df, 
                      pval = TRUE, 
                      title = input$km_plot_title, 
                      font.main = input$km_font_size, 
                      risk.table = input$show_risk_table,
                      pval.method = TRUE,
                      xlim = c(0, input$x_max),
                      break.x.by = input$x_breaks,
                      xlab = "Time",
                      conf.int = input$show_conf_intervals,
                      palette = palette_colors,
                      ggtheme = switch(input$plot_theme,
                                       "theme_minimal" = theme_minimal(),
                                       "theme_classic" = theme_classic(),
                                       "theme_gray" = theme_gray(),
                                       "theme_bw" = theme_bw(),
                                       theme_minimal())
      )
      print(p)
      dev.off()
    }
  )
  
  # Download handler for Univariate Schoenfeld Residuals Plot
  output$download_schoenfeld_uni <- downloadHandler(
    filename = function() {
      paste("univariate_schoenfeld_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      res <- univariate_cox()
      schoenfeld <- cox.zph(res)
      
      # Define color palette based on user selection
      plot_color <- switch(input$color_palette,
                           "default" = "#2E9FDF",
                           "blue_green" = "#1f78b4",
                           "purple_orange" = "#6a3d9a",
                           "red_blue" = "#e31a1c",
                           "#2E9FDF")
      
      plot(schoenfeld, main = "Schoenfeld Residuals for Univariate Analysis", col = plot_color, lwd = 2)
      dev.off()
    }
  )
  
  # Download handler for Multivariate Schoenfeld Residuals Plot
  output$download_schoenfeld_multi <- downloadHandler(
    filename = function() {
      paste("multivariate_schoenfeld_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      res <- multivariate_cox()
      schoenfeld <- cox.zph(res)
      
      # Define color palette based on user selection
      plot_color <- switch(input$color_palette,
                           "default" = "#E7B800",
                           "blue_green" = "#33a02c",
                           "purple_orange" = "#fb9a99",
                           "red_blue" = "#1f78b4",
                           "#E7B800")
      
      plot(schoenfeld, main = input$schoenfeld_multi_title, col = plot_color, lwd = 2)
      dev.off()
    }
  )
  
  # Download handler for Group Summary
  output$download_group_summary <- downloadHandler(
    filename = function() {
      paste("group_summary", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- km_data()
      if (input$use_grouping) {
        summary_df <- df %>%
          group_by(group) %>%
          summarise(
            Count = n(),
            Median_Biomarker = round(median(combined_main, na.rm = TRUE), 2),
            Mean_Time = round(mean(time, na.rm = TRUE), 2),
            Events = sum(event, na.rm = TRUE)
          )
      } else {
        summary_df <- data.frame(
          Group = "All",
          Count = nrow(df),
          Median_Biomarker = round(median(df$combined_main, na.rm = TRUE), 2),
          Mean_Time = round(mean(df$time, na.rm = TRUE), 2),
          Events = sum(df$event, na.rm = TRUE)
        )
        colnames(summary_df) <- c("Group", "Count", "Median_Biomarker", "Mean_Time", "Events")
      }
      write.csv(summary_df, file, row.names = FALSE)
    }
  )
  
  # Download handler for Biomarker Distribution Plot
  output$download_group_plot <- downloadHandler(
    filename = function() {
      paste("biomarker_distribution_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      df <- km_data()
      
      # Define color palette based on user selection
      palette_colors <- switch(input$color_palette,
                               "default" = c("#E7B800", "#2E9FDF"),
                               "blue_green" = c("#1f78b4", "#33a02c"),
                               "purple_orange" = c("#6a3d9a", "#fb9a99"),
                               "red_blue" = c("#e31a1c", "#1f78b4"),
                               c("#E7B800", "#2E9FDF")) # Default fallback
      
      if (input$use_grouping) {
        if (input$grouping_method == "Top and Bottom Quartiles") {
          Q1 <- quantile(df$combined_main, 0.25, na.rm = TRUE)
          Q3 <- quantile(df$combined_main, 0.75, na.rm = TRUE)
          cutoff_values <- c(Q1, Q3)
        } else if (input$grouping_method == "Median") {
          median_value <- median(df$combined_main, na.rm = TRUE)
          cutoff_values <- median_value
        } else if (input$grouping_method == "Custom Percentage") {
          cutoff <- quantile(df$combined_main, probs = input$custom_percentage / 100, na.rm = TRUE)
          cutoff_values <- cutoff
        } else if (input$grouping_method == "Optimized for Lowest P-value") {
          best_cutoff <- optimize_cutoff_uni(df, "combined_main")
          cutoff_values <- best_cutoff
        }
        
        p <- ggplot(df, aes(x = combined_main, fill = group)) +
          geom_histogram(binwidth = (max(df$combined_main, na.rm = TRUE) - min(df$combined_main, na.rm = TRUE))/30, alpha = 0.6, position = "identity", color = "black") +
          { if (input$grouping_method == "Top and Bottom Quartiles") 
            geom_vline(xintercept = cutoff_values, color = "red", linetype = "dashed", size = 1) 
            else 
              geom_vline(xintercept = cutoff_values, color = "red", linetype = "dashed", size = 1) 
          } +
          labs(title = "Biomarker Distribution with Grouping Cutoff",
               x = "Biomarker Expression",
               y = "Count",
               fill = "Group") +
          scale_fill_manual(values = palette_colors) +
          switch(input$plot_theme,
                 "theme_minimal" = theme_minimal(),
                 "theme_classic" = theme_classic(),
                 "theme_gray" = theme_gray(),
                 "theme_bw" = theme_bw(),
                 theme_minimal()) +
          theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 16),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14)
          )
        print(p)
      } else {
        # Define color palette based on user selection
        palette_colors_single <- switch(input$color_palette,
                                        "default" = "#2E9FDF",
                                        "blue_green" = "#1f78b4",
                                        "purple_orange" = "#6a3d9a",
                                        "red_blue" = "#e31a1c",
                                        "#2E9FDF") # Default fallback
        
        p <- ggplot(df, aes(x = combined_main)) +
          geom_histogram(binwidth = (max(df$combined_main, na.rm = TRUE) - min(df$combined_main, na.rm = TRUE))/30, fill = palette_colors_single, alpha = 0.7, color = "black") +
          labs(title = "Biomarker Distribution",
               x = "Biomarker Expression",
               y = "Count") +
          switch(input$plot_theme,
                 "theme_minimal" = theme_minimal(),
                 "theme_classic" = theme_classic(),
                 "theme_gray" = theme_gray(),
                 "theme_bw" = theme_bw(),
                 theme_minimal()) +
          theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 16)
          )
        print(p)
      }
      dev.off()
    }
  )
  
  # Download handler for Biomarker Boxplot
  output$download_boxplot_group <- downloadHandler(
    filename = function() {
      paste("biomarker_boxplot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      df <- km_data()
      
      # Define color palette based on user selection
      palette_colors <- switch(input$color_palette,
                               "default" = c("#E7B800", "#2E9FDF"),
                               "blue_green" = c("#1f78b4", "#33a02c"),
                               "purple_orange" = c("#6a3d9a", "#fb9a99"),
                               "red_blue" = c("#e31a1c", "#1f78b4"),
                               c("#E7B800", "#2E9FDF")) # Default fallback
      
      if (input$use_grouping) {
        p <- ggplot(df, aes(x = group, y = combined_main, fill = group)) +
          geom_boxplot(alpha = 0.7, outlier.shape = NA) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
          labs(title = "Biomarker Expression by Group",
               x = "Group",
               y = "Biomarker Expression") +
          scale_fill_manual(values = palette_colors) +
          switch(input$plot_theme,
                 "theme_minimal" = theme_minimal(),
                 "theme_classic" = theme_classic(),
                 "theme_gray" = theme_gray(),
                 "theme_bw" = theme_bw(),
                 theme_minimal()) +
          theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 16),
            legend.position = "none"
          )
        print(p)
      } else {
        # Define color palette based on user selection
        palette_colors_single <- switch(input$color_palette,
                                        "default" = "#2E9FDF",
                                        "blue_green" = "#1f78b4",
                                        "purple_orange" = "#6a3d9a",
                                        "red_blue" = "#e31a1c",
                                        "#2E9FDF") # Default fallback
        
        p <- ggplot(df, aes(y = combined_main)) +
          geom_boxplot(fill = palette_colors_single, alpha = 0.7, outlier.shape = NA) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
          labs(title = "Biomarker Expression",
               y = "Biomarker Expression") +
          switch(input$plot_theme,
                 "theme_minimal" = theme_minimal(),
                 "theme_classic" = theme_classic(),
                 "theme_gray" = theme_gray(),
                 "theme_bw" = theme_bw(),
                 theme_minimal()) +
          theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 16)
          )
        print(p)
      }
      dev.off()
    }
  )
  
  # Download handler for Inspected Data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("inspected_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- combined_main_var()[, selected_columns(), drop = FALSE]
      name_mapping <- attr(dataset(), "name_mapping")
      mapping <- setNames(name_mapping$original, name_mapping$sanitized)
      display_names <- mapping[selected_columns()]
      
      # Handle any missing mappings
      if(any(is.na(display_names))){
        display_names[is.na(display_names)] <- selected_columns()[is.na(display_names)]
      }
      
      # Rename the columns for display
      colnames(df) <- display_names
      
      # Write to CSV
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  ####################################### VARIABLE DISTRIBUTION #####################################################
  
  # Reactive expression to check distribution
  distribution_check <- reactive({
    req(combined_main_var())
    df <- combined_main_var()
    data <- df$combined_main
    
    # Remove NA and non-positive values for log transformation
    data_positive <- data[data > 0 & !is.na(data)]
    
    # Initialize result
    result <- list()
    
    # Normality test on original data
    normal_test <- shapiro.test(data)
    is_normal <- ifelse(normal_test$p.value > 0.05, TRUE, FALSE)
    
    # Normality test on log-transformed data
    if (length(data_positive) >= 3) {
      log_data <- log(data_positive)
      log_normal_test <- shapiro.test(log_data)
      is_log_normal <- ifelse(log_normal_test$p.value > 0.05, TRUE, FALSE)
    } else {
      is_log_normal <- FALSE
    }
    
    # Decide distribution
    if (is_normal) {
      distribution <- "The data appears to be normally distributed."
    } else if (is_log_normal) {
      distribution <- "The data appears to be log-normally distributed."
    } else {
      distribution <- "The data does not appear to be normally or log-normally distributed."
    }
    
    # Store results
    result$distribution <- distribution
    result$is_normal <- is_normal
    result$is_log_normal <- is_log_normal
    result$normal_p_value <- normal_test$p.value
    result$log_normal_p_value <- ifelse(exists("log_normal_test"), log_normal_test$p.value, NA)
    
    # Check suitability for Cox and KM analysis
    if (any(is.na(data) | is.infinite(data))) {
      suitability <- "Data contains NA or infinite values, which may not be suitable for Cox or KM analysis."
    } else if (any(data <= 0)) {
      suitability <- "Data contains non-positive values, which may not be suitable for log transformations."
    } else {
      suitability <- "Data is suitable for Cox and KM analysis."
    }
    result$suitability <- suitability
    
    return(result)
  })
  
  # Output for Distribution Result
  output$distribution_result <- renderPrint({
    result <- distribution_check()
    cat(result$distribution, "\n")
    cat("Normality test p-value:", round(result$normal_p_value, 5), "\n")
    if (!is.na(result$log_normal_p_value)) {
      cat("Log-normality test p-value:", round(result$log_normal_p_value, 5), "\n")
    }
    cat("Suitability Check:", result$suitability, "\n")
  })
  
  # Output for Distribution Plot
  output$distribution_plot <- renderPlot({
    req(combined_main_var())
    df <- combined_main_var()
    
    ggplot(df, aes(x = combined_main)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#69b3a2", color = "black", alpha = 0.7) +
      geom_density(color = "blue", size = 1) +
      labs(title = "Histogram and Density Plot of Main Variable",
           x = "Main Variable (Combined Biomarker)",
           y = "Density") +
      theme_minimal()
  })
  
  # Output for QQ Plot
  output$qq_plot <- renderPlot({
    req(combined_main_var())
    df <- combined_main_var()
    
    qqnorm(df$combined_main, main = "QQ Plot of Main Variable")
    qqline(df$combined_main, col = "red", lwd = 2)
  })
  
  # Output for Distribution Summary
  output$distribution_summary <- renderPrint({
    req(combined_main_var())
    df <- combined_main_var()
    summary(df$combined_main)
  })
  
  # Output for Shapiro-Wilk Test
  output$shapiro_test <- renderPrint({
    req(combined_main_var())
    df <- combined_main_var()
    data <- df$combined_main
    if (nrow(df) >= 3 && nrow(df) <= 5000) {
      test_result <- shapiro.test(data)
      print(test_result)
    } else {
      cat("Shapiro-Wilk test requires sample size between 3 and 5000.")
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



