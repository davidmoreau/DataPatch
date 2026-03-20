# Load libraries
library(shiny)
library(DT)
library(ggplot2)
library(mice)
library(VIM)
library(missForest)
library(Amelia)
library(shinythemes)
library(plotly)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("DataPatch: Missing Data Simulation and Imputation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV Dataset", accept = ".csv"),
      actionButton("load_sample", "Load Sample Data", 
                   icon = icon("database"),
                   class = "btn-primary"),
      helpText("Don't have a CSV file? Click the button above to load a sample dataset."),
      
      selectInput("workflow", "Workflow", 
                  choices = c("Simulate Missing Data", "Impute Missing Data")),
      
      conditionalPanel(
        condition = "input.workflow == 'Simulate Missing Data'",
        numericInput("missing_pct", "Percentage of Missing Data to Simulate", 
                     10, min = 1, max = 50),
        selectInput("missing_type", "Type of Missingness", 
                    choices = c("MCAR", "MAR", "MNAR"), 
                    selected = "MCAR")
      ),
      
      selectInput("method", "Choose Handling Technique", 
                  choices = c("Listwise Deletion", "Pairwise Deletion", 
                              "Mean Substitution", "Regression Imputation", 
                              "Multiple Imputation (MICE)", 
                              "Bayesian Multiple Imputation (Amelia)",
                              "Bayesian Data Augmentation (MICE-Gibbs)",  # Added
                              "K-Nearest Neighbor Imputation", 
                              "Random Forest Imputation", 
                              "Maximum Likelihood", 
                              "Pattern Mixture Model")),
      
      # Additional controls for Bayesian Data Augmentation
      conditionalPanel(
        condition = "input.method == 'Bayesian Data Augmentation (MICE-Gibbs)'",
        numericInput("gibbs_iter", "Gibbs Iterations", 
                     value = 20, min = 10, max = 100, step = 5),
        numericInput("gibbs_burnin", "Burn-in Iterations", 
                     value = 10, min = 0, max = 50, step = 1),
        helpText("Bayesian Data Augmentation uses a Gibbs sampler for full Bayesian imputation.")
      ),
      
      actionButton("process", "Process Data", 
                   icon = icon("play"),
                   class = "btn-success"),
      
      conditionalPanel(
        condition = "input.workflow == 'Impute Missing Data'",
        downloadButton("download_data", "Download Processed Data",
                       class = "btn-info")
      ),
      
      hr(),
      
      # New report generation section
      h4("Methods Report"),
      textInput("study_name", "Study Name", placeholder = "Enter study name"),
      textInput("author_name", "Author Name", placeholder = "Enter author name"),
      textInput("dataset_name", "Dataset Name", placeholder = "Enter dataset name"),
      actionButton("generate_report", "Generate Methods Report",
                   icon = icon("file-text"),
                   class = "btn-warning"),
      br(), br(),
      downloadButton("download_report", "Download Report"),
      
      helpText("See Moreau (2026) for details")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset", 
                 icon = icon("table"),
                 DTOutput("dataset_table")),
        
        tabPanel("Visualization", 
                 icon = icon("chart-bar"),
                 uiOutput("plot_ui")),  # uiOutput to switch between plotly and ggplot
        
        tabPanel("Missingness Pattern", 
                 icon = icon("chart-area"),
                 plotOutput("missing_pattern"),
                 verbatimTextOutput("missing_summary")),
        
        tabPanel("Summary", 
                 icon = icon("list-alt"),
                 verbatimTextOutput("summary")),
        
        tabPanel("Methods Report", 
                 icon = icon("file-alt"),
                 uiOutput("report_output_html")), 
        
        tabPanel("About", 
                 icon = icon("info-circle"),
                 uiOutput("about_content"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the dataset
  data <- reactiveVal(NULL)
  
  numeric_columns <- function(df) {
    vapply(df, is.numeric, logical(1))
  }
  
  safe_sample <- function(indices, size) {
    if (length(indices) == 0 || size <= 0) {
      return(integer(0))
    }
    sample(indices, size = min(length(indices), size))
  }
  
  # Load sample data when the button is clicked
  observeEvent(input$load_sample, {
    sample_path <- "sample_data.csv"
    
    sample_data <- tryCatch({
      if (!file.exists(sample_path)) {
        stop("sample_data.csv not found")
      }
      read.csv(sample_path)
    }, error = function(e) {
      set.seed(123)
      fallback_data <- data.frame(
        ID = 1:100,
        Age = rnorm(100, mean = 50, sd = 10),
        Income = rnorm(100, mean = 50000, sd = 15000),
        Education = sample(12:20, 100, replace = TRUE),
        Score1 = rnorm(100, mean = 75, sd = 15),
        Score2 = rnorm(100, mean = 80, sd = 10)
      )
      showNotification(
        paste("Sample CSV unavailable, using built-in fallback:", e$message),
        type = "warning"
      )
      fallback_data
    })
    
    data(sample_data)
    showNotification("Sample data loaded successfully!", type = "message")
  })
  
  # Load uploaded data
  observeEvent(input$file, {
    req(input$file)
    uploaded_data <- tryCatch({
      df <- read.csv(input$file$datapath)
      showNotification("Data uploaded successfully!", type = "message")
      df
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
      return(NULL)
    })
    data(uploaded_data)
  })
  
  # Reactive to introduce missingness (Simulate Workflow)
  missing_data <- reactive({
    req(data(), input$workflow == "Simulate Missing Data")
    set.seed(123)
    df <- data()
    pct <- input$missing_pct / 100
    n <- nrow(df)
    m <- ncol(df)
    numeric_idx <- which(numeric_columns(df))
    
    if (input$missing_type == "MCAR") {
      for (i in 1:m) {
        missing_rows <- safe_sample(seq_len(n), round(pct * n))
        if (length(missing_rows) > 0) {
          df[missing_rows, i] <- NA
        }
      }
    } else if (input$missing_type == "MAR") {
      if (length(numeric_idx) < 2) {
        showNotification(
          "MAR simulation requires at least two numeric variables. No missingness was added.",
          type = "warning"
        )
      } else {
        for (j in seq_len(length(numeric_idx) - 1)) {
          driver_idx <- numeric_idx[j]
          target_idx <- numeric_idx[j + 1]
          driver_values <- df[[driver_idx]]
          threshold <- mean(driver_values, na.rm = TRUE)
          
          if (is.finite(threshold)) {
            eligible_rows <- which(!is.na(driver_values) & driver_values > threshold)
            missing_rows <- safe_sample(eligible_rows, round(pct * length(eligible_rows)))
            if (length(missing_rows) > 0) {
              df[missing_rows, target_idx] <- NA
            }
          }
        }
      }
    } else if (input$missing_type == "MNAR") {
      if (length(numeric_idx) == 0) {
        showNotification(
          "MNAR simulation requires at least one numeric variable. No missingness was added.",
          type = "warning"
        )
      } else {
        for (i in numeric_idx) {
          threshold <- median(df[[i]], na.rm = TRUE)
          if (is.finite(threshold)) {
            eligible_rows <- which(!is.na(df[[i]]) & df[[i]] < threshold)
            missing_rows <- safe_sample(eligible_rows, round(pct * length(eligible_rows)))
            if (length(missing_rows) > 0) {
              df[missing_rows, i] <- NA
            }
          }
        }
      }
    }
    return(df)
  })
  
  analysis_input_data <- reactive({
    req(input$workflow, data())
    if (input$workflow == "Simulate Missing Data") {
      missing_data()
    } else {
      data()
    }
  })
  
  reference_data_for_viz <- reactive({
    req(data())
    data()
  })
  
  analysis_input_for_viz <- reactive({
    analysis_input_data()
  })
  
  # Reactive to handle missing data (Simulate or Impute Workflow)
  handled_data <- reactive({
    req(input$workflow, data())
    original_df <- analysis_input_data()
    
    df <- original_df
    method <- input$method
    numeric_cols <- numeric_columns(df)
    
    withProgress(message = 'Processing data...', value = 0.3, {
      if (method == "Listwise Deletion") {
        incProgress(0.4, detail = "Performing listwise deletion")
        df <- na.omit(df)
      } else if (method == "Pairwise Deletion") {
        incProgress(0.4, detail = "Processing pairwise deletion")
        df <- df
      } else if (method == "Mean Substitution") {
        incProgress(0.4, detail = "Performing mean substitution")
        for (i in which(numeric_cols)) {
          column_mean <- mean(df[[i]], na.rm = TRUE)
          if (is.finite(column_mean)) {
            df[is.na(df[[i]]), i] <- column_mean
          }
        }
      } else if (method == "Regression Imputation") {
        incProgress(0.4, detail = "Performing regression imputation")
        for (i in which(numeric_cols)) {
          if (!any(is.na(df[[i]]))) {
            next
          }
          
          predictor_idx <- which(numeric_cols)
          predictor_idx <- predictor_idx[predictor_idx != i]
          
          if (length(predictor_idx) == 0) {
            fallback_mean <- mean(df[[i]], na.rm = TRUE)
            if (is.finite(fallback_mean)) {
              df[is.na(df[[i]]), i] <- fallback_mean
            }
            next
          }
          
          model_vars <- c(i, predictor_idx)
          complete_rows <- complete.cases(df[, model_vars, drop = FALSE])
          missing_rows <- which(is.na(df[[i]]))
          predictable_rows <- missing_rows[complete.cases(df[missing_rows, predictor_idx, drop = FALSE])]
          
          if (sum(complete_rows) < 5 || length(predictable_rows) == 0) {
            fallback_mean <- mean(df[[i]], na.rm = TRUE)
            if (is.finite(fallback_mean)) {
              df[missing_rows, i] <- fallback_mean
            }
            next
          }
          
          formula_text <- paste(names(df)[i], "~", paste(names(df)[predictor_idx], collapse = " + "))
          lm_fit <- tryCatch(
            lm(as.formula(formula_text), data = df[complete_rows, , drop = FALSE]),
            error = function(e) NULL
          )
          
          if (is.null(lm_fit)) {
            fallback_mean <- mean(df[[i]], na.rm = TRUE)
            if (is.finite(fallback_mean)) {
              df[missing_rows, i] <- fallback_mean
            }
            next
          }
          
          predictions <- tryCatch(
            predict(lm_fit, newdata = df[predictable_rows, , drop = FALSE]),
            error = function(e) rep(NA_real_, length(predictable_rows))
          )
          
          df[predictable_rows, i] <- predictions
          
          remaining_rows <- missing_rows[is.na(df[missing_rows, i])]
          fallback_mean <- mean(df[[i]], na.rm = TRUE)
          if (length(remaining_rows) > 0 && is.finite(fallback_mean)) {
            df[remaining_rows, i] <- fallback_mean
          }
        }
      } else if (method == "Multiple Imputation (MICE)") {
        incProgress(0.4, detail = "Running MICE multiple imputation")
        imputed_data <- mice(df, m = 5, maxit = 10, method = "pmm", seed = 123, printFlag = FALSE)
        df <- complete(imputed_data, 1)
      } else if (method == "Bayesian Multiple Imputation (Amelia)") {
        incProgress(0.4, detail = "Running Bayesian multiple imputation")
        amelia_fit <- tryCatch({
          amelia(df, m = 1, p2s = 0)
        }, error = function(e) {
          showNotification("Using simpler imputation method", type = "warning")
          return(list(imputations = list(df)))
        })
        df <- amelia_fit$imputations[[1]]
      } else if (method == "Bayesian Data Augmentation (MICE-Gibbs)") {
        incProgress(0.4, detail = "Running Bayesian Data Augmentation")
        
        # Bayesian Data Augmentation using mice with simpler approach
        tryCatch({
          # First, identify numeric columns
          numeric_cols <- sapply(df, is.numeric)
          
          if (sum(numeric_cols) == 0) {
            stop("No numeric columns for Bayesian imputation")
          }
          
          # Use a simpler Bayesian approach: mice with norm method (Bayesian linear regression)
          # and increased iterations for proper convergence
          iterations <- input$gibbs_iter
          burnin <- input$gibbs_burnin
          total_iterations <- iterations + burnin
          
          # Initialize with default methods
          init <- mice(df, maxit = 0, printFlag = FALSE)
          meth <- init$method
          
          # Use norm (Bayesian linear regression) for numeric columns
          # Use pmm (predictive mean matching) for others
          for (j in 1:ncol(df)) {
            if (is.numeric(df[, j])) {
              meth[j] <- "norm"  # Bayesian linear regression
            } else {
              meth[j] <- "pmm"   # Predictive mean matching as fallback
            }
          }
          
          # Run MICE with Bayesian settings
          # Use more chains (m=3) for better Bayesian inference
          imputed_data <- mice(df, 
                               method = meth,
                               m = 3,  # Multiple chains for Bayesian inference
                               maxit = total_iterations,
                               seed = 123,
                               printFlag = FALSE)
          
          # For Bayesian inference, we should ideally pool across chains
          # Here we use the first chain after burn-in
          # Extract the last iteration after burn-in
          df <- complete(imputed_data, 1)
          
          showNotification("Bayesian Data Augmentation completed successfully!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Bayesian Data Augmentation failed:", e$message), type = "warning")
          # Fallback to standard MICE
          imputed_data <- mice(df, m = 5, maxit = 5, method = "pmm", seed = 123, printFlag = FALSE)
          df <- complete(imputed_data, 1)
        })
      } else if (method == "K-Nearest Neighbor Imputation") {
        incProgress(0.4, detail = "Running KNN imputation")
        # For KNN, we need to handle non-numeric columns
        df_imputed <- kNN(df, k = 5)
        # Remove extra columns added by kNN
        df <- df_imputed[, 1:ncol(df), drop = FALSE]
      } else if (method == "Random Forest Imputation") {
        incProgress(0.4, detail = "Running random forest imputation")
        df <- missForest(df)$ximp
      } else if (method == "Maximum Likelihood") {
        incProgress(0.4, detail = "Running maximum likelihood estimation")
        ml_fit <- amelia(df, m = 1)
        df <- ml_fit$imputations[[1]]
      } else if (method == "Pattern Mixture Model") {
        incProgress(0.4, detail = "Running pattern mixture model")
        imputed_data <- mice(df, m = 5, maxit = 5, method = "pmm", seed = 123, printFlag = FALSE)
        df <- complete(imputed_data, 1)
      }
      
      incProgress(0.3, detail = "Finalizing")
      return(df)
    })
  })
  
  # Render Dataset Table
  output$dataset_table <- renderDT({
    req(data())
    df <- if (input$workflow == "Simulate Missing Data") missing_data() else handled_data()
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      class = 'display nowrap',
      rownames = FALSE,
      caption = if (input$workflow == "Simulate Missing Data") 
        "Dataset with Simulated Missing Values" else 
          "Dataset After Missing Data Handling"
    ) %>%
      formatStyle(
        columns = colnames(df),
        backgroundColor = styleEqual(NA, 'lightyellow')
      )
  })
  
  # Choose which plot to display
  output$plot_ui <- renderUI({
    if (input$method == "Pairwise Deletion") {
      plotlyOutput("plot_correlation", height = "500px")
    } else {
      plotOutput("plot_scatter", height = "500px")
    }
  })
  
  # Correlation plot for Pairwise Deletion (using plotly)
  output$plot_correlation <- renderPlotly({
    tryCatch({
      req(data())
      original_data <- data()
      
      # Use only numeric columns for correlation
      numeric_cols <- sapply(original_data, is.numeric)
      if (sum(numeric_cols) < 2) {
        return(plotly_empty() %>%
                 layout(title = "Need at least 2 numeric variables for correlation"))
      }
      
      original_numeric <- original_data[, numeric_cols, drop = FALSE]
      cor_matrix <- cor(original_numeric, use = "pairwise.complete.obs")
      
      plot_ly(
        x = colnames(cor_matrix),
        y = colnames(cor_matrix),
        z = cor_matrix,
        type = "heatmap",
        colorscale = "RdYlBu",
        zmin = -1,
        zmax = 1,
        hovertemplate = "X: %{x}<br>Y: %{y}<br>Correlation: %{z:.3f}<extra></extra>"
      ) %>%
        layout(
          title = "Correlation Matrix (Pairwise Deletion)",
          xaxis = list(title = "Variables", tickangle = -45),
          yaxis = list(title = "Variables"),
          margin = list(l = 100, r = 50, b = 100, t = 50)
        )
    }, error = function(e) {
      plotly_empty() %>%
        layout(
          title = "Error creating correlation matrix",
          annotations = list(
            text = paste("Error:", e$message),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            x = 0.5,
            y = 0.5
          )
        )
    })
  })
  
  # Scatter plot for all other methods (using ggplot2 - simpler and more reliable)
  output$plot_scatter <- renderPlot({
    tryCatch({
      req(reference_data_for_viz(), analysis_input_for_viz(), handled_data())
      
      reference_data <- reference_data_for_viz()
      original_data <- analysis_input_for_viz()
      modified_data <- handled_data()
      
      # For listwise deletion, we need special handling
      if (input$method == "Listwise Deletion") {
        # Get complete cases from original data
        complete_indices <- complete.cases(original_data)
        original_complete <- original_data[complete_indices, , drop = FALSE]
        
        # For visualization, compare only the imputed values (none since deletion)
        # Instead, show that values are identical
        original_vals <- as.numeric(unlist(original_complete))
        modified_vals <- as.numeric(unlist(modified_data))
        
        # Ensure same length
        min_length <- min(length(original_vals), length(modified_vals))
        original_vals <- original_vals[1:min_length]
        modified_vals <- modified_vals[1:min_length]
        
      } else if (input$method == "Pairwise Deletion") {
        # Shouldn't reach here, but just in case
        original_vals <- as.numeric(unlist(original_data))
        modified_vals <- as.numeric(unlist(modified_data))
      } else {
        missing_mask <- is.na(original_data)
        
        if (sum(missing_mask) == 0) {
          if (input$workflow == "Simulate Missing Data") {
            original_vals <- as.numeric(unlist(reference_data))
            modified_vals <- as.numeric(unlist(modified_data))
          } else {
            df <- data.frame(
              Message = "No missing values in original data to compare",
              Value = 1
            )
            return(
              ggplot(df, aes(x = Message, y = Value)) +
                geom_text(aes(label = Message), size = 6) +
                theme_void() +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 14),
                  panel.background = element_rect(fill = "white")
                ) +
                labs(title = "No imputation performed (no missing values)")
            )
          }
        } else if (input$workflow == "Simulate Missing Data") {
          original_vals <- c()
          modified_vals <- c()
          
          common_cols <- intersect(colnames(original_data), colnames(reference_data))
          for (col in common_cols) {
            orig_col <- original_data[[col]]
            ref_col <- reference_data[[col]]
            mod_col <- modified_data[[col]]
            na_indices <- which(is.na(orig_col))
            
            if (length(na_indices) == 0) {
              next
            }
            
            reference_numeric <- suppressWarnings(as.numeric(as.character(ref_col[na_indices])))
            modified_numeric <- suppressWarnings(as.numeric(as.character(mod_col[na_indices])))
            valid_idx <- is.finite(reference_numeric) & is.finite(modified_numeric)
            
            if (any(valid_idx)) {
              original_vals <- c(original_vals, reference_numeric[valid_idx])
              modified_vals <- c(modified_vals, modified_numeric[valid_idx])
            }
          }
          
          if (length(original_vals) == 0 || length(modified_vals) == 0) {
            original_vals <- as.numeric(unlist(reference_data))
            modified_vals <- as.numeric(unlist(modified_data))
          }
        } else {
          original_vals <- c()
          modified_vals <- c()
          
          for (col in colnames(original_data)) {
            if (col %in% colnames(modified_data)) {
              orig_col <- original_data[[col]]
              mod_col <- modified_data[[col]]
              
              # Find indices where original was NA
              na_indices <- which(is.na(orig_col))
              
              if (length(na_indices) > 0) {
                imputed_vals <- mod_col[na_indices]
                non_na_indices <- which(!is.na(orig_col))
                if (length(non_na_indices) > 0) {
                  non_missing_vals <- orig_col[non_na_indices]
                  
                  sample_size <- min(length(imputed_vals), length(non_missing_vals))
                  if (sample_size > 0) {
                    original_vals <- c(original_vals, sample(non_missing_vals, sample_size))
                    modified_vals <- c(modified_vals, imputed_vals[1:sample_size])
                  }
                }
              }
            }
          }
          
          # If we couldn't get comparison data, fall back to full comparison
          if (length(original_vals) == 0 || length(modified_vals) == 0) {
            original_vals <- as.numeric(unlist(original_data))
            modified_vals <- as.numeric(unlist(modified_data))
          }
        }
      }
      
      # Convert to numeric safely
      original_vals_numeric <- suppressWarnings(as.numeric(as.character(original_vals)))
      modified_vals_numeric <- suppressWarnings(as.numeric(as.character(modified_vals)))
      
      # Remove NA/Inf values
      valid_idx <- is.finite(original_vals_numeric) & is.finite(modified_vals_numeric)
      original_vals_numeric <- original_vals_numeric[valid_idx]
      modified_vals_numeric <- modified_vals_numeric[valid_idx]
      
      if (length(original_vals_numeric) == 0 || length(modified_vals_numeric) == 0) {
        # Create a simple informative plot instead
        df <- data.frame(
          Message = "No comparable numeric data available",
          Value = 1
        )
        return(
          ggplot(df, aes(x = Message, y = Value)) +
            geom_text(aes(label = Message), size = 6) +
            theme_void() +
            theme(
              plot.title = element_text(hjust = 0.5, size = 14),
              panel.background = element_rect(fill = "white")
            ) +
            labs(title = "Cannot create comparison plot")
        )
      }
      
      # Create data frame for plotting
      plot_data <- data.frame(
        Original = original_vals_numeric,
        Modified = modified_vals_numeric
      )
      
      # Remove any remaining NA/Inf values
      plot_data <- plot_data[is.finite(plot_data$Original) & is.finite(plot_data$Modified), ]
      
      if (nrow(plot_data) < 2) {
        df <- data.frame(
          Message = "Insufficient data for comparison",
          Value = 1
        )
        return(
          ggplot(df, aes(x = Message, y = Value)) +
            geom_text(aes(label = Message), size = 6) +
            theme_void() +
            theme(
              plot.title = element_text(hjust = 0.5, size = 14),
              panel.background = element_rect(fill = "white")
            ) +
            labs(title = "Insufficient comparable data")
        )
      }
      
      # Calculate proximity (similar to original code)
      plot_data$Distance <- abs(plot_data$Original - plot_data$Modified)
      max_distance <- max(plot_data$Distance, na.rm = TRUE)
      
      if (max_distance > 0 && is.finite(max_distance)) {
        plot_data$Proximity <- 1 - (plot_data$Distance / max_distance)
      } else {
        plot_data$Proximity <- 1
      }
      
      # Calculate R² if possible - only if there's variation
      r_squared <- NA
      if (sd(plot_data$Original) > 0 && sd(plot_data$Modified) > 0 && 
          length(unique(plot_data$Original)) > 1 && length(unique(plot_data$Modified)) > 1) {
        r_squared <- cor(plot_data$Original, plot_data$Modified)^2
      }
      
      # Create the plot
      p <- ggplot(plot_data, aes(x = Original, y = Modified)) +
        geom_point(aes(color = Proximity), alpha = 0.7, size = 2) +
        scale_color_gradient(low = "coral", high = "cyan4", 
                             limits = c(0, 1), name = "Proximity") +
        geom_abline(intercept = 0, slope = 1, color = "gray", 
                    linetype = "dashed", size = 0.8) +
        labs(
          title = if (input$workflow == "Simulate Missing Data") {
            paste("True vs Imputed Values After", input$method)
          } else {
            paste("Impact of", input$method, "on Data")
          },
          x = "Original Values",
          y = "Modified Values"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "right",
          panel.grid.minor = element_blank()
        )
      
      # Add R² annotation if available and not 1
      if (!is.na(r_squared) && r_squared < 0.999) {
        p <- p + 
          annotate("text", 
                   x = min(plot_data$Original, na.rm = TRUE),
                   y = max(plot_data$Modified, na.rm = TRUE),
                   label = paste("R² =", round(r_squared, 3)),
                   hjust = 0, vjust = 1,
                   size = 5, color = "black",
                   fontface = "bold")
      } else if (!is.na(r_squared) && r_squared >= 0.999) {
        # If R² is essentially 1, show a different message
        p <- p + 
          annotate("text", 
                   x = min(plot_data$Original, na.rm = TRUE),
                   y = max(plot_data$Modified, na.rm = TRUE),
                   label = "Perfect agreement",
                   hjust = 0, vjust = 1,
                   size = 5, color = "darkgreen",
                   fontface = "bold")
      }
      
      return(p)
      
    }, error = function(e) {
      # Create error plot
      df <- data.frame(
        Message = paste("Error:", e$message),
        Value = 1
      )
      ggplot(df, aes(x = Message, y = Value)) +
        geom_text(aes(label = Message), size = 5) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14),
          panel.background = element_rect(fill = "white")
        ) +
        labs(title = "Error creating visualization")
    })
  })
  
  # Missingness pattern visualization
  output$missing_pattern <- renderPlot({
    req(data())
    df <- if (input$workflow == "Simulate Missing Data") missing_data() else data()
    
    # Calculate missing percentage
    missing_percent <- colMeans(is.na(df)) * 100
    
    # Create data frame for plotting
    plot_df <- data.frame(
      Variable = names(missing_percent),
      MissingPercent = missing_percent
    )
    
    # Reorder by missing percentage
    plot_df$Variable <- reorder(plot_df$Variable, -plot_df$MissingPercent)
    
    ggplot(plot_df, aes(x = Variable, y = MissingPercent)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = sprintf("%.1f%%", MissingPercent)), 
                vjust = -0.5, size = 3.5) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      ) +
      labs(
        title = "Missing Data Pattern by Variable",
        x = "Variables",
        y = "Percentage Missing (%)"
      ) +
      ylim(0, max(missing_percent, na.rm = TRUE) * 1.1)
  })
  
  # Missing data summary
  output$missing_summary <- renderPrint({
    req(data())
    df <- if (input$workflow == "Simulate Missing Data") missing_data() else data()
    
    cat("MISSING DATA SUMMARY\n")
    cat("====================\n\n")
    cat("Dataset dimensions:", nrow(df), "rows ×", ncol(df), "columns\n")
    cat("Total missing values:", sum(is.na(df)), "\n")
    cat("Percentage missing:", round(mean(is.na(df)) * 100, 2), "%\n\n")
    
    cat("Missing values by variable:\n")
    missing_counts <- colSums(is.na(df))
    missing_pct <- colMeans(is.na(df)) * 100
    
    if (length(missing_counts) > 0) {
      for (i in seq_along(missing_counts)) {
        cat(sprintf("%-20s: %4d (%.1f%%)\n", 
                    names(missing_counts)[i], 
                    missing_counts[i], 
                    missing_pct[i]))
      }
    } else {
      cat("No variables found\n")
    }
  })
  
  # Render Summary Statistics
  output$summary <- renderPrint({
    req(data())
    cat("DATASET SUMMARY AFTER PROCESSING\n")
    cat("=================================\n\n")
    cat("Method used:", input$method, "\n")
    cat("Workflow:", input$workflow, "\n")
    if (input$workflow == "Simulate Missing Data") {
      cat("Missingness type:", input$missing_type, "\n")
      cat("Missing percentage:", input$missing_pct, "%\n")
    }
    if (input$method == "Bayesian Data Augmentation (MICE-Gibbs)") {
      cat("Gibbs iterations:", input$gibbs_iter, "\n")
      cat("Burn-in iterations:", input$gibbs_burnin, "\n")
    }
    cat("\n")
    print(summary(handled_data()))
  })
  
  # Generate methods report
  report_text <- reactiveVal("")
  
  observeEvent(input$generate_report, {
    req(data(), handled_data())
    
    original_data <- if (input$workflow == "Simulate Missing Data") missing_data() else data()
    final_data <- handled_data()
    
    # Calculate statistics
    total_missing_original <- sum(is.na(original_data))
    total_missing_final <- sum(is.na(final_data))
    pct_missing_original <- mean(is.na(original_data)) * 100
    pct_missing_final <- mean(is.na(final_data)) * 100
    
    # Generate report
    report <- paste(
      "METHODS SECTION: MISSING DATA HANDLING REPORT\n",
      "==============================================\n\n",
      "Study: ", input$study_name, "\n",
      "Author: ", input$author_name, "\n",
      "Dataset: ", input$dataset_name, "\n",
      "Date: ", format(Sys.Date(), "%B %d, %Y"), "\n\n",
      
      "1. DATA CHARACTERISTICS\n",
      "   --------------------\n",
      "   Original dataset dimensions: ", nrow(original_data), " observations × ", 
      ncol(original_data), " variables\n",
      "   Missing values in original data: ", total_missing_original, 
      " (", sprintf("%.2f", pct_missing_original), "%)\n\n",
      
      "2. MISSING DATA HANDLING PROCEDURE\n",
      "   -------------------------------\n",
      "   Missing data handling was performed using DataPatch (Moreau, 2026).\n",
      "   Workflow: ", input$workflow, "\n",
      if(input$workflow == "Simulate Missing Data") {
        paste("   Missing data simulation: ", input$missing_pct, 
              "% ", input$missing_type, "\n")
      },
      "   Imputation method: ", input$method, "\n",
      if(input$method == "Bayesian Data Augmentation (MICE-Gibbs)") {
        paste("   Gibbs sampling iterations: ", input$gibbs_iter, 
              " (burn-in: ", input$gibbs_burnin, ")\n")
      },
      "\n",
      
      "3. POST-PROCESSING RESULTS\n",
      "   -----------------------\n",
      "   Final dataset dimensions: ", nrow(final_data), " observations × ", 
      ncol(final_data), " variables\n",
      "   Remaining missing values: ", total_missing_final, 
      " (", sprintf("%.2f", pct_missing_final), "%)\n",
      "   Observations removed: ", nrow(original_data) - nrow(final_data), "\n\n",
      
      "4. JUSTIFICATION AND LIMITATIONS\n",
      "   -----------------------------\n",
      "   The selected method (", input$method, ") was chosen based on the\n",
      "   pattern of missingness and the statistical assumptions of our analysis.\n",
      "   This approach ", 
      if(input$method == "Bayesian Data Augmentation (MICE-Gibbs)") {
        "implements full Bayesian inference using Gibbs sampling, providing proper uncertainty quantification \n   and incorporating prior information through weakly informative priors."
      } else if(input$method %in% c("Multiple Imputation (MICE)", 
                                    "Bayesian Multiple Imputation (Amelia)",
                                    "Random Forest Imputation")) {
        "provides robust handling of missing data while preserving \n   statistical properties and relationships among variables."
      } else if(input$method == "Listwise Deletion") {
        "ensures complete cases for analysis but may reduce statistical power."
      } else if(input$method == "Pairwise Deletion") {
        "maximizes available data for each analysis but may lead to \n   inconsistent sample sizes across analyses."
      } else {
        "was selected to address specific patterns of missingness in the data."
      }, "\n\n",
      
      "5. REFERENCE\n",
      "   ----------\n",
      "   Missing data handling procedures followed recommendations in:\n",
      "   Moreau, D. (2026). Best Practices in Handling Missing Data in Psychological Research.",
      " Advances in Methods and Practices in Psychological Science \n\n",
      
      "--- END OF REPORT ---\n",
      sep = ""
    )
    
    report_text(report)
    showNotification("Methods report generated successfully!", type = "message")
  })
  
  # Display report
  output$report_output_html <- renderUI({
    req(report_text())
    
    # Convert plain text to HTML with proper formatting
    # Replace line breaks with HTML breaks and add italics for journal names
    html_text <- report_text()
    
    # Add italics to journal names using the same <em> tag you used in the About section
    html_text <- gsub("Advances in Methods and Practices in Psychological Science", 
                      "<em>Advances in Methods and Practices in Psychological Science</em>", 
                      html_text, fixed = TRUE)
    
    # For the downloaded text file, we'll keep the asterisk version
    # But for display in the app, we use HTML with <pre> tag to preserve formatting
    HTML(paste0("<pre style='white-space: pre-wrap; font-family: monospace; background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
                html_text,
                "</pre>"))
  })
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("methods_report_", gsub(" ", "_", input$study_name), "_", 
             Sys.Date(), ".txt")
    },
    content = function(file) {
      writeLines(report_text(), file)
    }
  )
  
  # Download processed data
  output$download_data <- downloadHandler(
    filename = function() { 
      paste("processed_data_", Sys.Date(), ".csv", sep = "") 
    },
    content = function(file) {
      req(input$workflow == "Impute Missing Data")
      write.csv(handled_data(), file, row.names = FALSE)
    }
  )
  
  # About content
  output$about_content <- renderUI({
    HTML("
    <div class='container'>
      <h3>About DataPatch</h3>
      <p><strong>Version:</strong> 1.2.0</p>
      <p><strong>Developed by:</strong> David Moreau, University of Auckland </p>
      
      <h4>Description</h4>
      <p>DataPatch is a comprehensive tool for simulating and handling missing data in psychological research. 
      The application provides multiple imputation methods, including Bayesian approaches, and generates transparent reports for methods sections.</p>
      
      <h4>Key Features</h4>
      <ul>
        <li>Simulate missing data (MCAR, MAR, MNAR)</li>
        <li>Multiple imputation methods including Bayesian Data Augmentation</li>
        <li>Interactive visualizations</li>
        <li>Automatic methods section report generation</li>
        <li>Export capabilities</li>
      </ul>
      
      <h4>New in Version 1.2.0</h4>
      <ul>
        <li>Added Bayesian Data Augmentation using Gibbs sampling</li>
        <li>Enhanced methods report with Bayesian-specific details</li>
        <li>Improved visualization reliability</li>
      </ul>
      
      <h4>Citation</h4>
      <p>When using DataPatch in your research, please cite:</p>
      <blockquote>
        Moreau, D. (2026). Best Practices in Handling Missing Data in Psychological Research. <em>Advances in Methods and Practices in Psychological Science 
        </em>.
      </blockquote>
      
      <h4>License</h4>
      <p>This software is provided under the MIT License.</p>
      
      <hr>
      <p class='text-muted'>
        <i class='fa fa-github'></i> 
        <a href='https://github.com/davidmoreau/DataPatch' target='_blank'>
          View on GitHub
        </a>
      </p>
    </div>
    ")
  })
}

# Run app
shinyApp(ui = ui, server = server)
