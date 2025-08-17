# app.R — Yield Predictor per Crop (Shiny)
# Filters by Crop, then trains a regression model for that crop only.

# ---- Packages (load only; install once from console if needed) ----
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(readxl); library(dplyr)
  library(ggplot2); library(DT); library(shinycssloaders); library(plotly); library(stringr)
})

options(shiny.fullstacktrace = TRUE, shiny.sanitize.errors = FALSE)

# ---- Helpers ----
safe_read <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.null(x$datapath)) return(as.data.frame(readxl::read_excel(x$datapath)))
  if (is.character(x) && file.exists(x))  return(as.data.frame(readxl::read_excel(x)))
  NULL
}

metric_card <- function(title, value) {
  tags$div(
    class = "shadow-sm rounded-3 p-3 text-center bg-white",
    tags$div(class="fw-semibold text-secondary", as.character(title)[1]),
    tags$div(class="fs-3 fw-bold", as.character(value)[1])
  )
}

one_string <- function(...) paste(..., collapse = "")  # ensure length-1 strings

# ---- UI ----
ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      .app-header { background: linear-gradient(90deg, #0ea5e9, #14b8a6);
        color: white; padding: 18px 22px; border-radius: 14px; margin-bottom:14px; }
      .predict-box { background:#f8fafc; border:1px solid #e5e7eb; border-radius:14px; padding:16px; }
      .big-number { font-size: 2.2rem; font-weight: 800; }
      .pill { background: rgba(255,255,255,0.18); padding:4px 10px; border-radius:999px; }
    "))
  ),
  
  tags$div(
    class="app-header d-flex align-items-center justify-content-between",
    tags$div(
      tags$div(class="pill", "Yield Predictor"),
      tags$h3(class="m-0 mt-1", "Multiple Regression • Per-Crop Training")
    ),
    tags$div(class="text-end small",
             div(one_string("Built with Shiny")),
             div(one_string("Flow: choose crop → choose predictors → train → predict.")))
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      fileInput("file", "Upload data (Excel .xlsx)", accept = c(".xlsx")),
      helpText(one_string(
        "If you don't upload a file, the app will try to load ",
        "`crop_recommendation_shiny.xlsx` from this folder."
      )),
      hr(),
      selectInput("crop_filter", "Select Crop", choices = NULL),
      selectInput("target", "Target (yield column)", choices = NULL),
      checkboxGroupInput("predictors", "Predictor variables", choices = NULL),
      sliderInput("split", "Train/Test split (train %)", min = 10, max = 90, value = 75, step = 5),
      actionButton("train", "Train / Refit Model", class = "btn btn-primary w-100"),
      br(), uiOutput("model_status")
    ),
    card(
      card_header("Workspace"),
      tabsetPanel(
        id = "tabs",
        tabPanel("Predict",
                 br(),
                 uiOutput("predict_inputs"),
                 actionButton("do_predict", "Predict Yield", class = "btn btn-success"),
                 br(), br(),
                 div(class="predict-box",
                     h5("Prediction"),
                     uiOutput("pred_value") %>% withSpinner())
        ),
        tabPanel("Diagnostics",
                 br(),
                 fluidRow(
                   column(4, uiOutput("rmse_card")),
                   column(4, uiOutput("mae_card")),
                   column(4, uiOutput("r2_card"))
                 ),
                 br(),
                 h5("Actual vs Predicted (Test Set)"),
                 plotlyOutput("avp_plot") %>% withSpinner(),
                 br(),
                 h5("Model Summary"),
                 verbatimTextOutput("model_summary") %>% withSpinner()
        ),
        tabPanel("Data",
                 br(),
                 DTOutput("data_table") %>% withSpinner()
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Load data: prefer upload; else local default file; else NULL
  data_raw <- reactive({
    up <- safe_read(input$file)
    if (!is.null(up)) return(up)
    safe_read("crop_recommendation_shiny.xlsx")
  })
  
  # Populate crop choices + guess target/predictors when data loads
  observeEvent(data_raw(), {
    dat <- data_raw()
    if (is.null(dat)) {
      updateSelectInput(session, "crop_filter", choices = character(), selected = character())
      updateSelectInput(session, "target", choices = character(), selected = character())
      updateCheckboxGroupInput(session, "predictors", choices = character(), selected = character())
      return()
    }
    
    cols <- names(dat)
    validate(need("Crop" %in% cols, one_string(
      "Required column 'Crop' not found. Please include a 'Crop' column.")))
    
    # Crop choices
    crops <- sort(unique(as.character(dat$Crop)))
    updateSelectInput(session, "crop_filter", choices = crops, selected = crops[1])
    
    # Target guess
    num_cols <- cols[vapply(dat, is.numeric, logical(1))]
    guess_target <- if ("yield_kg_ha" %in% cols) "yield_kg_ha" else {
      ycol <- cols[grepl("yield", tolower(cols), fixed = TRUE)]
      if (length(ycol)) ycol[1] else if (length(num_cols)) num_cols[1] else cols[1]
    }
    updateSelectInput(session, "target", choices = cols, selected = guess_target)
    
    # Default predictors: exclude target and 'Crop'
    predictors <- setdiff(cols, c(guess_target, "Crop"))
    updateCheckboxGroupInput(session, "predictors",
                             choices = predictors,
                             selected = predictors
    )
  }, priority = 50)
  
  # Prepare modeling dataset: filter by crop; characters -> factors; target must be numeric
  model_prep <- reactive({
    dat <- data_raw(); req(dat)
    req("Crop" %in% names(dat), input$crop_filter)
    
    dat <- dat[dat$Crop == input$crop_filter, , drop = FALSE]
    validate(need(nrow(dat) >= 2, one_string("No (or too few) rows for crop '", input$crop_filter, "'.")))
    
    req(input$target, length(input$target) == 1)
    preds <- input$predictors; req(length(preds) >= 1)
    
    # Ensure 'Crop' is NOT in predictors
    preds <- setdiff(preds, "Crop")
    validate(need(length(preds) >= 1, "Select at least one predictor (excluding 'Crop')."))
    
    keep <- c(input$target, preds)
    dd <- dat[, keep, drop = FALSE]
    
    # Make all non-target character columns factors (stable modeling)
    for (nm in names(dd)) {
      if (nm != input$target && is.character(dd[[nm]])) dd[[nm]] <- factor(dd[[nm]])
    }
    
    validate(need(is.numeric(dd[[input$target]]),
                  one_string("Target '", input$target, "' must be numeric for regression.")
    ))
    
    dd <- dd[stats::complete.cases(dd), , drop = FALSE]
    validate(need(nrow(dd) >= 5, one_string(
      "Not enough complete rows (≥5) for crop '", input$crop_filter, "'."
    )))
    dd
  })
  
  # Train/test split + model
  model_obj <- eventReactive(input$train, {
    dd <- model_prep()
    set.seed(123)
    n <- nrow(dd)
    idx <- sample.int(n, size = max(1, floor(input$split/100 * n)))
    train <- dd[idx, , drop = FALSE]
    test  <- dd[-idx, , drop = FALSE]
    validate(need(nrow(test) > 0, "Train/Test split left no test rows. Reduce the train % or add more data."))
    
    form <- as.formula(paste(input$target, "~", paste(setdiff(names(dd), input$target), collapse = " + ")))
    fit  <- lm(form, data = train)
    
    y_test <- test[[input$target]]
    p_test <- as.numeric(predict(fit, newdata = test))
    
    rmse <- sqrt(mean((y_test - p_test)^2))
    mae  <- mean(abs(y_test - p_test))
    r2   <- if (length(unique(y_test)) > 1) suppressWarnings(cor(y_test, p_test)^2) else NA_real_
    
    list(
      fit=fit, formula=deparse(form),
      crop = input$crop_filter,
      train=train, test=test,
      y_test=y_test, p_test=p_test,
      rmse=rmse, mae=mae, r2=r2
    )
  }, ignoreInit = TRUE)
  
  # Status
  output$model_status <- renderUI({
    if (is.null(data_raw())) {
      div(class="text-danger small",
          one_string("No data loaded. Upload an .xlsx file or place ",
                     "crop_recommendation_shiny.xlsx in the app folder."))
    } else if (is.null(model_obj())) {
      div(class="text-muted small",
          "Model not trained yet. Click 'Train / Refit Model' to build it.")
    } else {
      tags$div(class="small",
               tags$b("Crop: "), as.character(model_obj()$crop)[1], " • ",
               tags$b("Model: "), tags$code(as.character(model_obj()$formula)[1]))
    }
  })
  
  # Metrics
  output$rmse_card <- renderUI({ req(model_obj()); metric_card("RMSE", sprintf("%.3f", model_obj()$rmse)) })
  output$mae_card  <- renderUI({ req(model_obj()); metric_card("MAE",  sprintf("%.3f", model_obj()$mae))  })
  output$r2_card   <- renderUI({ req(model_obj()); metric_card("R²",   ifelse(is.na(model_obj()$r2), "—", sprintf("%.3f", model_obj()$r2))) })
  
  # Actual vs Predicted
  output$avp_plot <- renderPlotly({
    req(model_obj())
    df <- data.frame(Actual = model_obj()$y_test, Predicted = model_obj()$p_test)
    g <- ggplot(df, aes(Actual, Predicted)) +
      geom_point(alpha = 0.75) +
      geom_abline(linetype = "dashed") +
      labs(title = "Actual vs Predicted (Test Set)", x = "Actual", y = "Predicted") +
      theme_minimal(base_size = 13)
    ggplotly(g)
  })
  
  # Model summary
  output$model_summary <- renderPrint({ req(model_obj()); summary(model_obj()$fit) })
  
  # Data table (filtered to selected crop for clarity)
  output$data_table <- renderDT({
    dat <- data_raw()
    validate(need(!is.null(dat), "No data to show yet."))
    if (!is.null(input$crop_filter) && "Crop" %in% names(dat)) {
      dat <- dat[dat$Crop == input$crop_filter, , drop = FALSE]
    }
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Dynamic prediction inputs (respect training types), built from training frame
  output$predict_inputs <- renderUI({
    req(model_obj())
    preds <- setdiff(names(model_obj()$train), input$target)
    trn   <- model_obj()$train
    
    items <- lapply(preds, function(p) {
      v <- trn[[p]]
      if (is.numeric(v)) {
        rng <- range(v, na.rm = TRUE)
        numericInput(paste0("pred_", p), p,
                     value = round(mean(v, na.rm = TRUE), 3),
                     min = floor(rng[1]), max = ceiling(rng[2]), step = 0.1)
      } else {
        levs <- levels(as.factor(v))
        selectInput(paste0("pred_", p), p, choices = levs, selected = levs[1])
      }
    })
    div(class="row g-3", lapply(items, function(ctrl) div(class="col-md-4", ctrl)))
  })
  
  # Predict (coerce to training types/levels)
  new_pred <- eventReactive(input$do_predict, {
    req(model_obj())
    preds <- setdiff(names(model_obj()$train), input$target)
    trn   <- model_obj()$train
    
    newx <- setNames(vector("list", length(preds)), preds)
    for (p in preds) newx[[p]] <- input[[paste0("pred_", p)]]
    newx <- as.data.frame(newx, stringsAsFactors = FALSE)
    
    for (p in preds) {
      tr_col <- trn[[p]]
      if (is.numeric(tr_col)) {
        newx[[p]] <- suppressWarnings(as.numeric(newx[[p]]))
      } else {
        lv <- levels(as.factor(tr_col))
        newx[[p]] <- factor(newx[[p]], levels = lv)
      }
    }
    as.numeric(predict(model_obj()$fit, newdata = newx))
  }, ignoreInit = TRUE)
  
  output$pred_value <- renderUI({
    req(new_pred())
    tags$div(
      tags$div("Estimated yield", class="text-secondary"),
      tags$div(class="big-number", sprintf("%.3f", new_pred())),
      tags$div(class="small text-muted",
               one_string("Units match your target column (", input$target, ")."))
    )
  })
}

shinyApp(ui, server)

