
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(janitor)
library(randomForest)
library(DT)
library(scales)

find_col <- function(data, candidates) {
  found <- intersect(candidates, names(data))
  if (length(found) == 0) return(NULL)
  found[1]
}

get_col <- function(data, candidates) {
  nm <- find_col(data, candidates)
  if (is.null(nm)) return(rep(NA_character_, nrow(data)))
  as.character(data[[nm]])
}

# Remove literal escape sequences (e.g. \\xc2\\xa0, \\n) found in the raw Kaggle file
strip_escapes <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\+x[0-9a-fA-F]{2}", " ", x)
  x <- gsub("\\\\+n", " ", x)
  str_squish(x)
}

clean_cat <- function(x) {
  x <- strip_escapes(x)
  x[is.na(x) | x == "" | tolower(x) %in% c("na", "n/a", "null", "nan")] <- "Unknown"
  x
}

# Merge spelling variants of the same label
standardise_city <- function(x) {
  k <- tolower(x)
  x[k %in% c("bengaluru", "bangalore")] <- "Bangalore"
  x[k %in% c("gurugram", "gurgaon")] <- "Gurgaon"
  x[k %in% c("delhi", "new delhi")] <- "New Delhi"
  x
}
standardise_industry <- function(x) {
  k <- gsub("[^a-z]", "", tolower(x))
  x[k == "ecommerce"] <- "eCommerce"
  x[k == "edtech"] <- "EdTech"
  x[k == "fintech"] <- "FinTech"
  x
}
standardise_type <- function(x) {
  k <- tolower(x)
  x[grepl("^(seed|angel)", k)] <- "Seed/Angel Funding"
  x[grepl("^private equity", k)] <- "Private Equity"
  x[grepl("^pre-?series a", k)] <- "Pre-Series A"
  x
}

clean_amount <- function(x) {
  x <- strip_escapes(x)
  x <- gsub(",", "", x)
  x <- gsub("[$₹+]", "", x)
  x <- trimws(x)
  x[tolower(x) %in% c("", "na", "n/a", "null", "nan", "undisclosed", "unknown", "-")] <- NA
  suppressWarnings(as.numeric(x))
}

parse_funding_date <- function(x) {
  x <- as.character(x)
  out <- suppressWarnings(dmy(x, quiet = TRUE))
  idx <- is.na(out) & !is.na(x)
  if (any(idx)) out[idx] <- suppressWarnings(mdy(x[idx], quiet = TRUE))
  idx <- is.na(out) & !is.na(x)
  if (any(idx)) out[idx] <- suppressWarnings(ymd(x[idx], quiet = TRUE))
  out
}

prepare_data <- function(raw) {
  names(raw) <- make_clean_names(names(raw))
  amount_col <- find_col(raw, c("amount_in_usd", "amountinusd", "amount_usd", "usd_amount", "amount"))
  if (is.null(amount_col)) stop(paste("Could not find amount column. CSV columns:", paste(names(raw), collapse = ", ")))

  date_text <- get_col(raw, c("date", "date_dd_mm_yyyy", "funding_date"))
  d <- tibble(
    startup = clean_cat(get_col(raw, c("startup_name", "startupname", "company_brand", "startup"))),
    industry = standardise_industry(clean_cat(get_col(raw, c("industry_vertical", "industryvertical", "industry", "sector")))),
    city = standardise_city(clean_cat(get_col(raw, c("city_location", "citylocation", "city", "headquarter")))),
    investor = clean_cat(get_col(raw, c("investors_name", "investorsname", "investors", "investor_name"))),
    investment_type = standardise_type(clean_cat(get_col(raw, c("investment_type", "investmenttype", "investmentntype", "investmentn_type", "stage", "funding_stage")))),
    funding_date = parse_funding_date(date_text),
    amount_usd = clean_amount(raw[[amount_col]])
  ) %>%
    distinct() %>%
    mutate(year = year(funding_date), month = month(funding_date))
  d
}

group_rare <- function(x, n = 12) {
  x <- clean_cat(x)
  freq <- sort(table(x[x != "Unknown"]), decreasing = TRUE)
  keep <- names(freq)[seq_len(min(n, length(freq)))]
  ifelse(x == "Unknown", "Unknown", ifelse(x %in% keep, x, "Other"))
}

ui <- dashboardPage(
  dashboardHeader(title = "Startup Funding Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Funding Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Investor Patterns", tabName = "investors", icon = icon("users")),
      menuItem("Prediction Model", tabName = "prediction", icon = icon("brain")),
      menuItem("Dataset & Quality", tabName = "dataset", icon = icon("table"))
    ),
    hr(),
    fileInput("file", "Upload Kaggle CSV", accept = ".csv"),
    helpText("Upload the extracted CSV file downloaded from Kaggle."),
    hr(),
    uiOutput("filters")
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper,.right-side{background:#f5f7fb}.box{border-radius:8px}"))),
    tabItems(
      tabItem(tabName = "overview",
        h2("Startup Funding Overview"),
        fluidRow(
          valueBoxOutput("records_box", width = 3),
          valueBoxOutput("funding_box", width = 3),
          valueBoxOutput("startups_box", width = 3),
          valueBoxOutput("investors_box", width = 3)
        ),
        fluidRow(
          box(title = "Funding Records by Year", status = "primary", solidHeader = TRUE, width = 7, plotOutput("year_plot", height = 300)),
          box(title = "Top Industries", status = "primary", solidHeader = TRUE, width = 5, plotOutput("industry_plot", height = 300))
        ),
        fluidRow(
          box(title = "Top Cities", status = "primary", solidHeader = TRUE, width = 6, plotOutput("city_plot", height = 300)),
          box(title = "Funding Amount Distribution", status = "primary", solidHeader = TRUE, width = 6, plotOutput("amount_plot", height = 300))
        ),
        box(title = "Important limitation", status = "warning", solidHeader = TRUE, width = 12,
            "The dataset records funding transactions. It may not include startups rejected for funding. The model predicts higher/lower recorded funding amounts, not actual funding success.")
      ),
      tabItem(tabName = "analysis",
        h2("Funding Analysis"),
        fluidRow(
          box(title = "Investment Types", status = "primary", solidHeader = TRUE, width = 6, plotOutput("type_plot", height = 330)),
          box(title = "Average Funding by Industry", status = "primary", solidHeader = TRUE, width = 6, plotOutput("average_plot", height = 330))
        ),
        box(title = "Industry Summary", status = "primary", solidHeader = TRUE, width = 12, DTOutput("industry_table"))
      ),
      tabItem(tabName = "investors",
        h2("Investor Pattern Analysis"),
        fluidRow(
          box(title = "Frequently Recorded Investors", status = "primary", solidHeader = TRUE, width = 7, plotOutput("investor_plot", height = 400)),
          box(title = "Investment Type Frequency", status = "primary", solidHeader = TRUE, width = 5, plotOutput("stage_plot", height = 400))
        ),
        box(title = "Investor Summary", status = "primary", solidHeader = TRUE, width = 12, DTOutput("investor_table"))
      ),
      tabItem(tabName = "prediction",
        h2("Funding Amount Category Prediction"),
        p("Random Forest predicts whether a record falls below or at/above the median recorded funding amount."),
        p(strong("This is not a genuine funding-success probability.")),
        fluidRow(
          box(title = "Model Evaluation", status = "primary", solidHeader = TRUE, width = 6, verbatimTextOutput("metrics")),
          box(title = "Feature Importance", status = "primary", solidHeader = TRUE, width = 6, plotOutput("importance_plot", height = 300))
        ),
        box(title = "Try a Prediction", status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(
            column(4, uiOutput("pred_industry")),
            column(4, uiOutput("pred_city")),
            column(4, uiOutput("pred_type"))
          ),
          actionButton("predict", "Predict Funding Amount Category", icon = icon("calculator"), class = "btn-primary"),
          br(), br(), verbatimTextOutput("prediction_result")
        )
      ),
      tabItem(tabName = "dataset",
        h2("Dataset and Data Quality"),
        box(title = "Data Quality Summary", status = "primary", solidHeader = TRUE, width = 12, DTOutput("quality_table")),
        box(title = "Filtered Records", status = "primary", solidHeader = TRUE, width = 12,
            downloadButton("download", "Download Filtered CSV"), br(), br(), DTOutput("data_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$file)
    readr::read_csv(input$file$datapath, show_col_types = FALSE, name_repair = "minimal")
  })

  data_clean <- reactive({
    req(raw_data())
    tryCatch(prepare_data(raw_data()), error = function(e) {
      showNotification(e$message, type = "error", duration = 10)
      NULL
    })
  })

  output$filters <- renderUI({
    req(data_clean())
    d <- data_clean()
    tagList(
      selectInput("industry_filter", "Industry", c("All", sort(unique(d$industry))), "All"),
      selectInput("city_filter", "City", c("All", sort(unique(d$city))), "All"),
      selectInput("type_filter", "Investment Type", c("All", sort(unique(d$investment_type))), "All"),
      selectInput("year_filter", "Year", c("All", sort(unique(na.omit(d$year)))), "All")
    )
  })

  filtered <- reactive({
    req(data_clean())
    d <- data_clean()
    if (!is.null(input$industry_filter) && input$industry_filter != "All") d <- filter(d, industry == input$industry_filter)
    if (!is.null(input$city_filter) && input$city_filter != "All") d <- filter(d, city == input$city_filter)
    if (!is.null(input$type_filter) && input$type_filter != "All") d <- filter(d, investment_type == input$type_filter)
    if (!is.null(input$year_filter) && input$year_filter != "All") d <- filter(d, year == as.integer(input$year_filter))
    d
  })

  output$records_box <- renderValueBox(valueBox(comma(nrow(filtered())), "Funding Records", icon = icon("database"), color = "blue"))
  output$funding_box <- renderValueBox({
    total <- sum(filtered()$amount_usd, na.rm = TRUE)
    valueBox(if (total > 0) dollar(total, scale = 1e-6, suffix = "M", accuracy = 0.1) else "N/A",
             "Recorded Funding (USD)", icon = icon("dollar-sign"), color = "green")
  })
  output$startups_box <- renderValueBox(valueBox(comma(n_distinct(filtered()$startup[filtered()$startup != "Unknown"])), "Distinct Startups", icon = icon("building"), color = "purple"))
  output$investors_box <- renderValueBox(valueBox(comma(n_distinct(filtered()$investor[filtered()$investor != "Unknown"])), "Investor Names", icon = icon("users"), color = "yellow"))

  output$year_plot <- renderPlot({
    d <- filtered() %>% filter(!is.na(year)) %>% count(year)
    validate(need(nrow(d) > 0, "No valid date records available."))
    ggplot(d, aes(year, n)) + geom_col(fill = "#2471A3") +
      labs(title = "Funding Records by Year", x = "Year", y = "Number of Records") + theme_minimal()
  })
  output$industry_plot <- renderPlot({
    d <- filtered() %>% filter(industry != "Unknown") %>% count(industry, sort = TRUE) %>% slice_head(n = 10)
    validate(need(nrow(d) > 0, "No industry data available."))
    ggplot(d, aes(reorder(industry, n), n)) + geom_col(fill = "#17A589") + coord_flip() +
      labs(title = "Top Industries by Record Count", x = "Industry", y = "Records") + theme_minimal()
  })
  output$city_plot <- renderPlot({
    d <- filtered() %>% filter(city != "Unknown") %>% count(city, sort = TRUE) %>% slice_head(n = 10)
    validate(need(nrow(d) > 0, "No city data available."))
    ggplot(d, aes(reorder(city, n), n)) + geom_col(fill = "#8E44AD") + coord_flip() +
      labs(title = "Top Cities by Record Count", x = "City", y = "Records") + theme_minimal()
  })
  output$amount_plot <- renderPlot({
    d <- filtered() %>% filter(is.finite(amount_usd), amount_usd > 0)
    validate(need(nrow(d) > 0, "No valid funding amounts."))
    ggplot(d, aes(amount_usd)) + geom_histogram(bins = 30, fill = "#E67E22", color = "white") +
      scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(title = "Recorded Funding Amount Distribution", x = "Funding Amount (USD)", y = "Records") + theme_minimal()
  })
  output$type_plot <- renderPlot({
    d <- filtered() %>% count(investment_type, sort = TRUE) %>% slice_head(n = 12)
    ggplot(d, aes(reorder(investment_type, n), n)) + geom_col(fill = "#2874A6") + coord_flip() +
      labs(title = "Investment Type Frequency", x = "Investment Type", y = "Records") + theme_minimal()
  })
  output$average_plot <- renderPlot({
    d <- filtered() %>% filter(is.finite(amount_usd), amount_usd > 0, industry != "Unknown") %>%
      group_by(industry) %>% summarise(avg_amount = mean(amount_usd), records = n(), .groups = "drop") %>%
      filter(records >= 3) %>% slice_max(avg_amount, n = 10)
    validate(need(nrow(d) > 0, "Not enough valid funding amounts for this view."))
    ggplot(d, aes(reorder(industry, avg_amount), avg_amount)) + geom_col(fill = "#CA6F1E") + coord_flip() +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(title = "Average Funding by Industry", x = "Industry", y = "Average USD") + theme_minimal()
  })
  output$industry_table <- renderDT({
    d <- filtered() %>% filter(is.finite(amount_usd), amount_usd > 0) %>% group_by(industry) %>%
      summarise(records = n(), total_usd = sum(amount_usd), average_usd = mean(amount_usd), median_usd = median(amount_usd), .groups = "drop") %>%
      arrange(desc(total_usd))
    datatable(d, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(c("total_usd", "average_usd", "median_usd"), currency = "$", digits = 0)
  })

  investor_records <- reactive({
    filtered() %>% filter(investor != "Unknown") %>%
      mutate(investor = str_split(investor, "\\s*,\\s*")) %>% unnest(investor) %>%
      mutate(investor = str_squish(investor)) %>% filter(investor != "")
  })
  output$investor_plot <- renderPlot({
    d <- investor_records() %>% count(investor, sort = TRUE) %>% slice_head(n = 15)
    validate(need(nrow(d) > 0, "No investor information available."))
    ggplot(d, aes(reorder(investor, n), n)) + geom_col(fill = "#117864") + coord_flip() +
      labs(title = "Frequently Recorded Investors", x = "Investor", y = "Associated Records") + theme_minimal()
  })
  output$stage_plot <- renderPlot({
    d <- filtered() %>% count(investment_type, sort = TRUE) %>% slice_head(n = 10)
    ggplot(d, aes(reorder(investment_type, n), n)) + geom_col(fill = "#AF7AC5") + coord_flip() +
      labs(title = "Investment Type Distribution", x = "Investment Type", y = "Records") + theme_minimal()
  })
  output$investor_table <- renderDT({
    d <- investor_records() %>% count(investor, sort = TRUE, name = "associated_records")
    datatable(d, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  model_data <- reactive({
    d <- data_clean() %>% filter(is.finite(amount_usd), amount_usd > 0, !is.na(year))
    validate(need(nrow(d) >= 50, "Need at least 50 valid records with amount and date for modelling."))
    d <- d %>% mutate(
      industry_model = factor(group_rare(industry, 15)),
      city_model = factor(group_rare(city, 12)),
      type_model = factor(group_rare(investment_type, 10))
    )
    cutoff <- median(d$amount_usd, na.rm = TRUE)
    d %>% mutate(amount_category = factor(if_else(amount_usd >= cutoff, "Higher", "Lower"), levels = c("Lower", "Higher")))
  })

  fit <- reactive({
    d <- model_data()
    set.seed(123)
    ids <- sample(seq_len(nrow(d)), floor(0.8 * nrow(d)))
    train <- d[ids, ]; test <- d[-ids, ]
    # Align factor levels and route unseen test categories to Other
    for (v in c("industry_model", "city_model", "type_model")) {
      train_levels <- levels(train[[v]])
      test_values <- as.character(test[[v]])
      test_values[!test_values %in% train_levels] <- "Other"
      if (!"Other" %in% train_levels) {
        train[[v]] <- factor(as.character(train[[v]]), levels = c(train_levels, "Other"))
      }
      test[[v]] <- factor(test_values, levels = levels(train[[v]]))
    }
    set.seed(123)
    model <- randomForest(amount_category ~ industry_model + city_model + type_model + year,
                          data = train, ntree = 300, importance = TRUE)
    pred <- predict(model, test)
    cm <- table(Actual = factor(test$amount_category, levels = c("Lower", "Higher")),
                Predicted = factor(pred, levels = c("Lower", "Higher")))
    acc <- sum(diag(cm)) / sum(cm)
    precision <- if (sum(cm[, "Higher"]) > 0) cm["Higher", "Higher"] / sum(cm[, "Higher"]) else NA_real_
    recall <- if (sum(cm["Higher", ]) > 0) cm["Higher", "Higher"] / sum(cm["Higher", ]) else NA_real_
    f1 <- if (is.finite(precision) && is.finite(recall) && precision + recall > 0) 2 * precision * recall / (precision + recall) else NA_real_
    list(model = model, train = train, test = test, cm = cm, accuracy = acc, precision = precision, recall = recall, f1 = f1, cutoff = median(d$amount_usd))
  })

  output$metrics <- renderPrint({
    m <- fit()
    cat("Model: Random Forest classifier\n")
    cat("Target: Higher / Lower than median funding amount\n")
    cat("Median threshold (USD):", format(round(m$cutoff, 2), big.mark = ","), "\n")
    cat("Training rows:", nrow(m$train), "| Test rows:", nrow(m$test), "\n\n")
    cat("Accuracy:", round(m$accuracy, 3), "\n")
    cat("Precision (Higher):", round(m$precision, 3), "\n")
    cat("Recall (Higher):", round(m$recall, 3), "\n")
    cat("F1 (Higher):", round(m$f1, 3), "\n\n")
    print(m$cm)
  })
  output$importance_plot <- renderPlot({
    m <- fit()
    imp <- as.data.frame(randomForest::importance(m$model))
    imp$feature <- rownames(imp)
    ggplot(imp, aes(reorder(feature, MeanDecreaseGini), MeanDecreaseGini)) +
      geom_col(fill = "#2E86C1") + coord_flip() +
      labs(title = "Random Forest Feature Importance", x = "Feature", y = "Mean Decrease in Gini") + theme_minimal()
  })
  output$pred_industry <- renderUI({
    d <- model_data(); selectInput("pi", "Industry", choices = levels(d$industry_model))
  })
  output$pred_city <- renderUI({
    d <- model_data(); selectInput("pc", "City", choices = levels(d$city_model))
  })
  output$pred_type <- renderUI({
    d <- model_data(); selectInput("pt", "Investment Type", choices = levels(d$type_model))
  })
  observeEvent(input$predict, {
    m <- fit()
    newdata <- data.frame(
      industry_model = factor(input$pi, levels = levels(m$train$industry_model)),
      city_model = factor(input$pc, levels = levels(m$train$city_model)),
      type_model = factor(input$pt, levels = levels(m$train$type_model)),
      year = as.integer(median(m$train$year, na.rm = TRUE))
    )
    cls <- predict(m$model, newdata)
    probs <- predict(m$model, newdata, type = "prob")
    output$prediction_result <- renderPrint({
      cat("Predicted category:", as.character(cls), "\n")
      cat("Model class probabilities:\n")
      print(round(probs, 4))
      cat("\nThis estimates Higher/Lower funding amount category, not actual funding success.")
    })
  })

  output$quality_table <- renderDT({
    req(raw_data(), data_clean())
    raw <- raw_data(); d <- data_clean()
    q <- tibble(
      Measure = c("Original rows", "Original columns", "Rows after duplicate removal",
                  "Duplicate rows removed", "Missing amounts", "Missing dates",
                  "Unknown industries", "Unknown cities", "Unknown investors"),
      Value = c(nrow(raw), ncol(raw), nrow(d), nrow(raw) - nrow(d),
                sum(is.na(d$amount_usd)), sum(is.na(d$funding_date)),
                sum(d$industry == "Unknown"), sum(d$city == "Unknown"), sum(d$investor == "Unknown"))
    )
    datatable(q, options = list(dom = "t"), rownames = FALSE)
  })
  output$data_table <- renderDT(datatable(filtered(), options = list(pageLength = 10, scrollX = TRUE), filter = "top", rownames = FALSE))
  output$download <- downloadHandler(
    filename = function() paste0("startup_funding_filtered_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
