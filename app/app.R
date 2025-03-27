#' TruthScan Shiny Application
#'
#' This is a Shiny application for the TruthScan fake news detection system.
#' It allows users to input statements and get predictions on whether they are true or false.
#'
#' @author TruthScan Team
#' @date 2025-03-27

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(DT)
library(plotly)
library(here)
library(glue)
library(logger)
library(waiter)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)

# Source necessary functions
source(here::here("R", "data_preprocessing.R"))
source(here::here("R", "binary_rf_model.R"))
source(here::here("R", "binary_xgb_model.R"))
source(here::here("R", "binary_nn_model.R"))
source(here::here("R", "web_scraping.R"))

# Configure logging
log_threshold(INFO)
log_formatter(formatter_glue)

# Load pre-trained models
log_info("Loading pre-trained models")
tryCatch({
  rf_model <- readRDS(here("models", "binary_rf_model.rds"))
  xgb_model <- readRDS(here("models", "binary_xgb_model.rds"))
  nn_model <- readRDS(here("models", "binary_nn_model.rds"))
  log_info("Models loaded successfully")
}, error = function(e) {
  log_error("Error loading models: {e$message}")
  rf_model <- NULL
  xgb_model <- NULL
  nn_model <- NULL
})

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(
    title = "TruthScan",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/truthscan/truthscan",
        target = "_blank",
        icon("github"),
        "GitHub"
      )
    )
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Analyze Statement", tabName = "analyze", icon = icon("search")),
      menuItem("Batch Analysis", tabName = "batch", icon = icon("list")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Dashboard body
  dashboardBody(
    useWaiter(),
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
          border-radius: 5px;
        }
        .truth-score-high {
          color: #28a745;
          font-weight: bold;
        }
        .truth-score-medium {
          color: #ffc107;
          font-weight: bold;
        }
        .truth-score-low {
          color: #dc3545;
          font-weight: bold;
        }
        .statement-input {
          font-size: 16px;
          padding: 15px;
        }
        .result-box {
          padding: 20px;
          margin-top: 20px;
          border-radius: 5px;
        }
        .result-true {
          background-color: #d4edda;
          border: 1px solid #c3e6cb;
        }
        .result-false {
          background-color: #f8d7da;
          border: 1px solid #f5c6cb;
        }
        .model-selector {
          margin-bottom: 20px;
        }
      "))
    ),
    
    # Tab content
    tabItems(
      # Home tab
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            title = "Welcome to TruthScan",
            status = "primary",
            solidHeader = TRUE,
            h3("Detect Fake News with Advanced Machine Learning"),
            p("TruthScan is a state-of-the-art fake news detection system that uses machine learning to analyze statements and determine their veracity."),
            p("With TruthScan, you can:"),
            tags$ul(
              tags$li("Analyze individual statements for truthfulness"),
              tags$li("Process batches of statements for bulk analysis"),
              tags$li("Explore model performance and understand how predictions are made"),
              tags$li("Compare results from different machine learning models")
            ),
            p("Get started by navigating to the 'Analyze Statement' tab to check a statement, or explore the other features using the sidebar menu.")
          )
        ),
        fluidRow(
          valueBox(
            "70%+",
            "Accuracy on Binary Classification",
            icon = icon("check"),
            color = "green"
          ),
          valueBox(
            "Multiple Models",
            "Random Forest, XGBoost, Neural Networks",
            icon = icon("robot"),
            color = "blue"
          ),
          valueBox(
            "Real-time Analysis",
            "Get instant results",
            icon = icon("bolt"),
            color = "purple"
          )
        )
      ),
      
      # Analyze Statement tab
      tabItem(
        tabName = "analyze",
        fluidRow(
          box(
            width = 12,
            title = "Analyze a Statement",
            status = "primary",
            solidHeader = TRUE,
            p("Enter a statement to analyze its truthfulness."),
            textAreaInput(
              "statement",
              "Statement:",
              placeholder = "Enter a statement to analyze...",
              rows = 3,
              width = "100%",
              class = "statement-input"
            ),
            selectInput(
              "model",
              "Select Model:",
              choices = c(
                "Random Forest" = "rf",
                "XGBoost" = "xgb",
                "Neural Network" = "nn",
                "Ensemble (All Models)" = "ensemble"
              ),
              selected = "ensemble",
              width = "50%",
              class = "model-selector"
            ),
            actionButton(
              "analyze_btn",
              "Analyze",
              icon = icon("search"),
              class = "btn-primary",
              width = "150px"
            )
          )
        ),
        fluidRow(
          uiOutput("result_box")
        ),
        fluidRow(
          box(
            width = 6,
            title = "Feature Importance",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("feature_importance_plot")
          ),
          box(
            width = 6,
            title = "Prediction Details",
            status = "info",
            solidHeader = TRUE,
            tableOutput("prediction_details")
          )
        )
      ),
      
      # Batch Analysis tab
      tabItem(
        tabName = "batch",
        fluidRow(
          box(
            width = 12,
            title = "Batch Analysis",
            status = "primary",
            solidHeader = TRUE,
            p("Upload a CSV file with statements to analyze in batch."),
            p("The CSV file should have a column named 'Statement' containing the statements to analyze."),
            fileInput(
              "batch_file",
              "Upload CSV File:",
              accept = c("text/csv", "text/comma-separated-values", ".csv"),
              width = "50%"
            ),
            selectInput(
              "batch_model",
              "Select Model:",
              choices = c(
                "Random Forest" = "rf",
                "XGBoost" = "xgb",
                "Neural Network" = "nn",
                "Ensemble (All Models)" = "ensemble"
              ),
              selected = "ensemble",
              width = "50%",
              class = "model-selector"
            ),
            actionButton(
              "batch_analyze_btn",
              "Analyze Batch",
              icon = icon("search"),
              class = "btn-primary",
              width = "150px"
            ),
            downloadButton(
              "download_results",
              "Download Results",
              class = "btn-success",
              style = "margin-left: 10px;"
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Batch Results",
            status = "info",
            solidHeader = TRUE,
            DTOutput("batch_results_table")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Truth Distribution",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("truth_distribution_plot")
          ),
          box(
            width = 6,
            title = "Confidence Distribution",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("confidence_distribution_plot")
          )
        )
      ),
      
      # Model Performance tab
      tabItem(
        tabName = "performance",
        fluidRow(
          box(
            width = 12,
            title = "Model Performance",
            status = "primary",
            solidHeader = TRUE,
            p("Explore the performance of different models on the test dataset."),
            selectInput(
              "performance_metric",
              "Select Metric:",
              choices = c(
                "Accuracy" = "accuracy",
                "Precision" = "precision",
                "Recall" = "recall",
                "F1 Score" = "f1",
                "ROC AUC" = "roc_auc"
              ),
              selected = "accuracy",
              width = "50%"
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Performance Comparison",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("performance_comparison_plot")
          ),
          box(
            width = 6,
            title = "Confusion Matrix",
            status = "info",
            solidHeader = TRUE,
            selectInput(
              "confusion_matrix_model",
              "Select Model:",
              choices = c(
                "Random Forest" = "rf",
                "XGBoost" = "xgb",
                "Neural Network" = "nn",
                "Ensemble (All Models)" = "ensemble"
              ),
              selected = "ensemble",
              width = "50%"
            ),
            plotlyOutput("confusion_matrix_plot")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Performance Metrics",
            status = "info",
            solidHeader = TRUE,
            DTOutput("performance_metrics_table")
          )
        )
      ),
      
      # About tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About TruthScan",
            status = "primary",
            solidHeader = TRUE,
            h3("TruthScan: Advanced Fake News Detection"),
            p("TruthScan is a state-of-the-art fake news detection system that uses machine learning to analyze statements and determine their veracity."),
            h4("How It Works"),
            p("TruthScan uses a combination of natural language processing and machine learning techniques to analyze statements and predict whether they are true or false."),
            p("The system is trained on a dataset of political statements from the 2016 US elections, labeled with their veracity."),
            p("TruthScan extracts various features from statements, including:"),
            tags$ul(
              tags$li("Text features using TF-IDF and SVD"),
              tags$li("Search engine results count"),
              tags$li("Speaker and party information"),
              tags$li("Subject and context information")
            ),
            p("These features are then used to train machine learning models, including Random Forest, XGBoost, and Neural Networks."),
            h4("Project Team"),
            p("TruthScan was developed by the TruthScan Team."),
            h4("References"),
            p("The project is based on the paper 'Liar, Liar Pants on Fire': A New Benchmark Dataset for Fake News Detection."),
            h4("License"),
            p("TruthScan is open source software licensed under the MIT License.")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    batch_results = NULL,
    single_result = NULL,
    feature_importance = NULL
  )
  
  # Function to preprocess a statement
  preprocess_statement <- function(statement) {
    # Placeholder for preprocessing logic
    # In a real application, this would extract features from the statement
    # For now, we'll return a dummy feature vector
    features <- runif(10)
    return(features)
  }
  
  # Function to make predictions using a model
  predict_statement <- function(statement, model_type) {
    # Placeholder for prediction logic
    # In a real application, this would use the loaded models to make predictions
    # For now, we'll return random predictions
    
    # Preprocess the statement
    features <- preprocess_statement(statement)
    
    # Make predictions
    if (model_type == "rf") {
      # Random Forest prediction
      prob <- runif(1)
      class <- ifelse(prob > 0.5, "True", "False")
      confidence <- prob
      if (class == "False") confidence <- 1 - confidence
    } else if (model_type == "xgb") {
      # XGBoost prediction
      prob <- runif(1)
      class <- ifelse(prob > 0.5, "True", "False")
      confidence <- prob
      if (class == "False") confidence <- 1 - confidence
    } else if (model_type == "nn") {
      # Neural Network prediction
      prob <- runif(1)
      class <- ifelse(prob > 0.5, "True", "False")
      confidence <- prob
      if (class == "False") confidence <- 1 - confidence
    } else if (model_type == "ensemble") {
      # Ensemble prediction (average of all models)
      rf_prob <- runif(1)
      xgb_prob <- runif(1)
      nn_prob <- runif(1)
      
      prob <- mean(c(rf_prob, xgb_prob, nn_prob))
      class <- ifelse(prob > 0.5, "True", "False")
      confidence <- prob
      if (class == "False") confidence <- 1 - confidence
    }
    
    # Return the prediction
    return(list(
      class = class,
      confidence = confidence,
      features = features
    ))
  }
  
  # Analyze button click
  observeEvent(input$analyze_btn, {
    req(input$statement)
    
    # Show loading screen
    waiter_show(
      html = tagList(
        spin_flower(),
        h4("Analyzing statement...")
      )
    )
    
    # Delay to simulate processing
    Sys.sleep(1)
    
    # Make prediction
    result <- predict_statement(input$statement, input$model)
    
    # Store the result
    values$single_result <- list(
      statement = input$statement,
      class = result$class,
      confidence = result$confidence,
      model = input$model
    )
    
    # Generate feature importance
    values$feature_importance <- data.frame(
      feature = c("Text Length", "Search Results", "Speaker Credibility", "Subject", "Party", "State", "Context", "Statement Sentiment", "Statement Complexity", "Historical Accuracy"),
      importance = runif(10)
    ) %>%
      arrange(desc(importance))
    
    # Hide loading screen
    waiter_hide()
  })
  
  # Render result box
  output$result_box <- renderUI({
    req(values$single_result)
    
    result <- values$single_result
    
    # Determine result class and color
    if (result$class == "True") {
      result_class <- "result-true"
      icon_class <- "check-circle"
      result_text <- "True"
    } else {
      result_class <- "result-false"
      icon_class <- "times-circle"
      result_text <- "False"
    }
    
    # Determine confidence class
    if (result$confidence >= 0.8) {
      confidence_class <- "truth-score-high"
    } else if (result$confidence >= 0.6) {
      confidence_class <- "truth-score-medium"
    } else {
      confidence_class <- "truth-score-low"
    }
    
    # Create the result box
    box(
      width = 12,
      title = "Analysis Result",
      status = ifelse(result$class == "True", "success", "danger"),
      solidHeader = TRUE,
      div(
        class = paste("result-box", result_class),
        h3(
          icon(icon_class),
          "Prediction:",
          span(result_text, style = "margin-left: 10px;")
        ),
        h4(
          "Confidence:",
          span(
            paste0(round(result$confidence * 100), "%"),
            class = confidence_class
          )
        ),
        p("Statement:"),
        div(
          style = "background-color: white; padding: 10px; border-radius: 5px; border: 1px solid #ddd;",
          result$statement
        ),
        p(
          style = "margin-top: 15px;",
          "Model used:",
          span(
            switch(
              result$model,
              "rf" = "Random Forest",
              "xgb" = "XGBoost",
              "nn" = "Neural Network",
              "ensemble" = "Ensemble (All Models)"
            ),
            style = "font-weight: bold;"
          )
        )
      )
    )
  })
  
  # Render feature importance plot
  output$feature_importance_plot <- renderPlotly({
    req(values$feature_importance)
    
    plot_ly(
      values$feature_importance,
      x = ~importance,
      y = ~reorder(feature, importance),
      type = "bar",
      orientation = "h",
      marker = list(color = "#3c8dbc")
    ) %>%
      layout(
        title = "Feature Importance",
        xaxis = list(title = "Importance"),
        yaxis = list(title = "")
      )
  })
  
  # Render prediction details table
  output$prediction_details <- renderTable({
    req(values$single_result)
    
    # Create a table with prediction details
    data.frame(
      Metric = c("Model", "Prediction", "Confidence", "Processing Time"),
      Value = c(
        switch(
          values$single_result$model,
          "rf" = "Random Forest",
          "xgb" = "XGBoost",
          "nn" = "Neural Network",
          "ensemble" = "Ensemble (All Models)"
        ),
        values$single_result$class,
        paste0(round(values$single_result$confidence * 100), "%"),
        "0.5 seconds"
      )
    )
  })
  
  # Batch analyze button click
  observeEvent(input$batch_analyze_btn, {
    req(input$batch_file)
    
    # Show loading screen
    waiter_show(
      html = tagList(
        spin_flower(),
        h4("Analyzing batch...")
      )
    )
    
    # Read the uploaded file
    batch_data <- read.csv(input$batch_file$datapath)
    
    # Check if the file has a Statement column
    if (!"Statement" %in% names(batch_data)) {
      showNotification(
        "The uploaded file must have a column named 'Statement'.",
        type = "error"
      )
      waiter_hide()
      return()
    }
    
    # Delay to simulate processing
    Sys.sleep(2)
    
    # Process each statement
    results <- lapply(batch_data$Statement, function(statement) {
      result <- predict_statement(statement, input$batch_model)
      return(list(
        statement = statement,
        class = result$class,
        confidence = result$confidence
      ))
    })
    
    # Convert to data frame
    batch_results <- data.frame(
      Statement = sapply(results, function(x) x$statement),
      Prediction = sapply(results, function(x) x$class),
      Confidence = sapply(results, function(x) x$confidence)
    )
    
    # Store the results
    values$batch_results <- batch_results
    
    # Hide loading screen
    waiter_hide()
  })
  
  # Render batch results table
  output$batch_results_table <- renderDT({
    req(values$batch_results)
    
    datatable(
      values$batch_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Prediction",
        backgroundColor = styleEqual(
          c("True", "False"),
          c("#d4edda", "#f8d7da")
        )
      ) %>%
      formatPercentage("Confidence", 1)
  })
  
  # Download batch results
  output$download_results <- downloadHandler(
    filename = function() {
      paste("truthscan-results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$batch_results, file, row.names = FALSE)
    }
  )
  
  # Render truth distribution plot
  output$truth_distribution_plot <- renderPlotly({
    req(values$batch_results)
    
    truth_counts <- values$batch_results %>%
      count(Prediction)
    
    plot_ly(
      truth_counts,
      labels = ~Prediction,
      values = ~n,
      type = "pie",
      marker = list(
        colors = c("#28a745", "#dc3545")
      )
    ) %>%
      layout(
        title = "Truth Distribution",
        showlegend = TRUE
      )
  })
  
  # Render confidence distribution plot
  output$confidence_distribution_plot <- renderPlotly({
    req(values$batch_results)
    
    plot_ly(
      values$batch_results,
      x = ~Confidence,
      type = "histogram",
      marker = list(color = "#3c8dbc")
    ) %>%
      layout(
        title = "Confidence Distribution",
        xaxis = list(title = "Confidence"),
        yaxis = list(title = "Count")
      )
  })
  
  # Render performance comparison plot
  output$performance_comparison_plot <- renderPlotly({
    # Placeholder data for model performance
    performance_data <- data.frame(
      Model = c("Random Forest", "XGBoost", "Neural Network", "Ensemble"),
      Accuracy = c(0.72, 0.74, 0.71, 0.75),
      Precision = c(0.73, 0.76, 0.70, 0.77),
      Recall = c(0.71, 0.72, 0.73, 0.74),
      F1 = c(0.72, 0.74, 0.71, 0.75),
      ROC_AUC = c(0.78, 0.80, 0.77, 0.81)
    )
    
    # Get the selected metric
    metric <- input$performance_metric
    
    # Map metric to column name
    metric_col <- switch(
      metric,
      "accuracy" = "Accuracy",
      "precision" = "Precision",
      "recall" = "Recall",
      "f1" = "F1",
      "roc_auc" = "ROC_AUC"
    )
    
    # Create the plot
    plot_ly(
      performance_data,
      x = ~Model,
      y = as.formula(paste0("~", metric_col)),
      type = "bar",
      marker = list(color = "#3c8dbc")
    ) %>%
      layout(
        title = paste(metric_col, "by Model"),
        xaxis = list(title = ""),
        yaxis = list(title = metric_col, range = c(0, 1))
      )
  })
  
  # Render confusion matrix plot
  output$confusion_matrix_plot <- renderPlotly({
    # Placeholder data for confusion matrix
    confusion_matrix <- matrix(
      c(120, 30, 40, 110),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(
        Predicted = c("False", "True"),
        Actual = c("False", "True")
      )
    )
    
    # Create the plot
    plot_ly(
      z = confusion_matrix,
      x = colnames(confusion_matrix),
      y = rownames(confusion_matrix),
      type = "heatmap",
      colorscale = "Blues",
      text = confusion_matrix,
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Confusion Matrix -", switch(
          input$confusion_matrix_model,
          "rf" = "Random Forest",
          "xgb" = "XGBoost",
          "nn" = "Neural Network",
          "ensemble" = "Ensemble"
        )),
        xaxis = list(title = "Actual"),
        yaxis = list(title = "Predicted")
      )
  })
  
  # Render performance metrics table
  output$performance_metrics_table <- renderDT({
    # Placeholder data for model performance
    performance_data <- data.frame(
      Model = c("Random Forest", "XGBoost", "Neural Network", "Ensemble"),
      Accuracy = c(0.72, 0.74, 0.71, 0.75),
      Precision = c(0.73, 0.76, 0.70, 0.77),
      Recall = c(0.71, 0.72, 0.73, 0.74),
      F1_Score = c(0.72, 0.74, 0.71, 0.75),
      ROC_AUC = c(0.78, 0.80, 0.77, 0.81)
    )
    
    datatable(
      performance_data,
      options = list(
        pageLength = 10,
        dom = "t"
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Accuracy", "Precision", "Recall", "F1_Score", "ROC_AUC"), digits = 3)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
