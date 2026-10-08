library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

data_path <- file.path("data", "air_quality.csv")
if (!file.exists(data_path)) {
  stop("Dataset not found at ", data_path, ". Run generate_data.R first.")
}

air_quality <- read.csv(data_path, stringsAsFactors = FALSE)
air_quality$Date <- as.Date(air_quality$Date)
air_quality$AQI_Category <- factor(
  air_quality$AQI_Category,
  levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy")
)

city_choices <- sort(unique(air_quality$City))
category_levels <- levels(air_quality$AQI_Category)
category_colors <- c(
  "Good" = "#2f855a",
  "Moderate" = "#c58b16",
  "Unhealthy for Sensitive Groups" = "#dd7834",
  "Unhealthy" = "#c84c4c",
  "Very Unhealthy" = "#704477"
)
chart_colors <- c("#1f7a72", "#3f8f8a", "#78aaa0", "#d29a39", "#c86b51", "#78628d", "#597b9a", "#93a464")
chart_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e8efed", linewidth = 0.35),
    axis.title = element_text(color = "#526662", face = "bold"),
    axis.text = element_text(color = "#657772"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", color = "#203a36")
  )

latest_record <- air_quality[which.max(air_quality$Date), , drop = FALSE]
latest_aqi <- latest_record$AQI[[1]]
latest_category <- as.character(latest_record$AQI_Category[[1]])

section_heading <- function(title, subtitle = NULL) {
  div(
    class = "section-heading",
    h2(title),
    if (!is.null(subtitle)) p(subtitle)
  )
}

chart_card <- function(title, subtitle, plot_output, height = "340px") {
  div(
    class = "chart-card",
    div(class = "chart-card-heading", h3(title), p(subtitle)),
    plot_output
  )
}

empty_plot <- function(message) {
  plot_ly() %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(text = message, x = 0.5, y = 0.5, showarrow = FALSE, font = list(color = "#71837e", size = 14)))
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

nice_plot <- function(plot, y_title = NULL) {
  plot %>%
    layout(
      font = list(family = "Inter, system-ui, sans-serif", color = "#4f625d"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 55, r = 20, t = 15, b = 55),
      xaxis = list(title = "", gridcolor = "#edf2f0", zerolinecolor = "#dfe8e5"),
      yaxis = list(title = y_title %||% "", gridcolor = "#edf2f0", zerolinecolor = "#dfe8e5"),
      legend = list(orientation = "h", x = 0, y = -0.22)
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

correlation_insight <- function(data, x, label) {
  complete <- data[complete.cases(data[, c(x, "AQI")]), , drop = FALSE]
  if (nrow(complete) < 3 || stats::sd(complete[[x]]) == 0 || stats::sd(complete$AQI) == 0) {
    return(paste(label, "relationship with AQI cannot be estimated from the selected records."))
  }
  value <- stats::cor(complete[[x]], complete$AQI)
  strength <- if (abs(value) >= 0.7) {
    "strong"
  } else if (abs(value) >= 0.4) {
    "moderate"
  } else {
    "weak"
  }
  direction <- if (value >= 0) "positive" else "negative"
  paste0(label, " has a ", strength, " ", direction, " association with AQI in the selected records (r = ", sprintf("%.2f", value), ").")
}

ui <- page_sidebar(
  title = NULL,
  fillable = TRUE,
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#1f7a72",
    secondary = "#617570",
    success = "#2f855a",
    base_font = font_google("Inter")
  ),
  sidebar = sidebar(
    width = 270,
    open = "always",
    div(
      class = "brand-block",
      div(class = "brand-mark", icon("wind")),
      div(
        class = "brand-copy",
        div(class = "brand-title", "AERIS"),
        div(class = "brand-subtitle", "AIR QUALITY INTELLIGENCE")
      )
    ),
    div(class = "sidebar-label", "WORKSPACE"),
    radioButtons(
      "page",
      label = NULL,
      choiceNames = list(
        tagList(icon("gauge-high"), span("Overview")),
        tagList(icon("chart-line"), span("Air Quality")),
        tagList(icon("scale-balanced"), span("City Comparison")),
        tagList(icon("arrow-trend-up"), span("Trends")),
        tagList(icon("sliders"), span("AQI Checker")),
        tagList(icon("circle-info"), span("About"))
      ),
      choiceValues = c("overview", "air_quality", "comparison", "trends", "checker", "about"),
      selected = "overview"
    ),
    div(
      class = "sidebar-foot",
      div(class = "live-dot"),
      div(
        div(class = "sidebar-foot-title", "Dataset status"),
        div(class = "sidebar-foot-copy", paste(nrow(air_quality), "observations loaded"))
      )
    )
  ),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      :root { --ink: #203a36; --muted: #71837e; --teal: #1f7a72; --canvas: #f4f8f7; }
      body { background: var(--canvas); color: var(--ink); font-family: Inter, system-ui, sans-serif; }
      .bslib-page-sidebar { --bslib-sidebar-bg: #173b38; --bslib-sidebar-fg: #e7f1ee; }
      .bslib-sidebar-layout > .sidebar { border: 0; box-shadow: 5px 0 22px rgba(20,52,47,.08); }
      .brand-block { display:flex; align-items:center; gap:12px; padding: 8px 6px 30px; }
      .brand-mark { width:42px; height:42px; display:grid; place-items:center; border-radius:14px; color:#173b38; background:#a9d9ca; font-size:19px; }
      .brand-title { color:#fff; font-weight:800; letter-spacing:.16em; font-size:15px; }
      .brand-subtitle { color:#a9c4bc; font-size:9px; letter-spacing:.12em; margin-top:3px; }
      .sidebar-label { color:#86a79e; font-size:10px; font-weight:700; letter-spacing:.16em; margin:0 0 10px 10px; }
      .sidebar .shiny-options-group { display:flex; flex-direction:column; gap:5px; }
      .sidebar .radio label { display:flex; align-items:center; gap:13px; width:100%; padding:11px 12px; border-radius:11px; color:#c7d8d2; font-size:13px; cursor:pointer; }
      .sidebar .radio label:hover { background:rgba(255,255,255,.07); color:white; }
      .sidebar .radio input { position:absolute; opacity:0; }
      .sidebar .radio input:checked + span { color:#fff; }
      .sidebar .radio:has(input:checked) label { color:#fff; background:#2b5751; box-shadow:inset 3px 0 #9dd1c2; }
      .sidebar .radio label i { width:18px; text-align:center; color:#a8c7be; }
      .sidebar-foot { display:flex; align-items:center; gap:11px; border-top:1px solid rgba(255,255,255,.12); margin-top:25px; padding:20px 6px 4px; }
      .live-dot { width:9px; height:9px; border-radius:50%; background:#67c599; box-shadow:0 0 0 5px rgba(103,197,153,.12); }
      .sidebar-foot-title { color:#e9f4ef; font-size:12px; font-weight:600; }
      .sidebar-foot-copy { color:#9cbab1; font-size:11px; margin-top:3px; }
      .dashboard-content { max-width:1500px; margin:0 auto; padding:34px 38px 48px; }
      .page-hero { display:flex; justify-content:space-between; align-items:flex-start; gap:20px; margin-bottom:28px; }
      .eyebrow { color:var(--teal); font-size:10px; font-weight:800; letter-spacing:.16em; text-transform:uppercase; margin-bottom:9px; }
      .page-hero h1 { color:var(--ink); font-weight:750; font-size:clamp(25px,3vw,35px); letter-spacing:-.035em; margin:0 0 7px; }
      .page-hero p { color:var(--muted); margin:0; font-size:14px; }
      .data-stamp { padding:10px 14px; border:1px solid #dce8e4; background:#fff; color:#586c66; border-radius:12px; font-size:11px; white-space:nowrap; }
      .data-stamp i { color:var(--teal); margin-right:7px; }
      .kpi-grid { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:16px; margin-bottom:22px; }
      .kpi-card, .chart-card, .content-card { background:#fff; border:1px solid #e5eeeb; border-radius:17px; box-shadow:0 5px 18px rgba(24,66,57,.035); }
      .kpi-card { padding:19px 20px 17px; min-height:132px; position:relative; overflow:hidden; }
      .kpi-card:after { content:''; position:absolute; right:-24px; top:-28px; width:90px; height:90px; border-radius:50%; background:#edf7f3; }
      .kpi-label { color:#71837e; font-size:11px; font-weight:700; letter-spacing:.07em; text-transform:uppercase; position:relative; z-index:1; }
      .kpi-value { color:#203a36; font-size:30px; line-height:1.2; font-weight:750; letter-spacing:-.04em; margin-top:14px; position:relative; z-index:1; }
      .kpi-unit { color:#83918d; font-size:12px; font-weight:500; letter-spacing:0; }
      .kpi-note { color:#91a09b; font-size:11px; margin-top:7px; position:relative; z-index:1; }
      .status-pill { display:inline-flex; border-radius:30px; padding:6px 10px; font-size:11px; font-weight:700; margin-top:15px; position:relative; z-index:1; }
      .section-heading { margin:30px 0 15px; }
      .section-heading h2 { font-size:18px; color:var(--ink); font-weight:720; letter-spacing:-.02em; margin:0 0 4px; }
      .section-heading p { color:var(--muted); font-size:12px; margin:0; }
      .chart-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:17px; }
      .chart-card { min-width:0; padding:18px 18px 10px; margin-bottom:17px; }
      .chart-card-heading { margin:0 2px 8px; }
      .chart-card-heading h3 { margin:0 0 4px; font-size:14px; font-weight:700; color:#29433e; }
      .chart-card-heading p { margin:0; font-size:11px; color:#879590; }
      .chart-card .plotly, .chart-card .plot-container { width:100% !important; }
      .filter-card { display:flex; flex-wrap:wrap; align-items:end; gap:14px; background:#fff; border:1px solid #e5eeeb; border-radius:15px; padding:16px 18px; margin-bottom:17px; }
      .filter-card .form-group { margin:0; min-width:190px; flex:1; }
      .filter-card label { color:#617570; font-size:11px; font-weight:700; }
      .form-control, .selectize-input { border-color:#dfe9e5 !important; border-radius:9px !important; box-shadow:none !important; }
      .insight-list { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:12px; margin-top:2px; }
      .insight { display:flex; gap:10px; align-items:flex-start; border-radius:12px; padding:14px; background:#f2f8f5; color:#506760; font-size:12px; line-height:1.55; }
      .insight i { color:var(--teal); margin-top:2px; }
      .about-card { padding:26px; margin-bottom:17px; }
      .feature-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:11px; margin-top:18px; }
      .feature-chip { padding:12px 13px; border-radius:11px; background:#f3f8f6; color:#48615a; font-size:12px; }
      .feature-chip i { color:var(--teal); width:22px; }
      .technology-tags { display:flex; gap:8px; flex-wrap:wrap; margin-top:12px; }
      .technology-tags span { background:#eaf4f0; color:#34675d; border-radius:20px; padding:7px 11px; font-size:11px; font-weight:650; }
      .recommendation { background:#eef7f3; border-left:4px solid #4c9d76; border-radius:9px; padding:15px 17px; color:#47625a; font-size:13px; line-height:1.6; }
      .estimator-note { color:#9b651e; background:#fff8e9; border:1px solid #f1e0b8; padding:12px 14px; border-radius:10px; font-size:12px; margin:14px 0 19px; }
      .dataTables_wrapper { color:#63736e; font-size:12px; }
      table.dataTable thead th { color:#49615a; font-size:11px; }
      @media(max-width:900px) { .dashboard-content { padding:25px 20px 38px; } .kpi-grid { grid-template-columns:repeat(2,minmax(0,1fr)); } }
      @media(max-width:620px) { .dashboard-content { padding:20px 14px 32px; } .kpi-grid, .chart-grid, .insight-list, .feature-grid { grid-template-columns:1fr; } .page-hero { display:block; } .data-stamp { display:inline-block; margin-top:14px; } }
    "))
  ),
  div(class = "dashboard-content", uiOutput("page_content"))
)

server <- function(input, output, session) {
  overview_header <- function() {
    tagList(
      div(
        class = "page-hero",
        div(
          div(class = "eyebrow", "Environmental intelligence"),
          h1("Air Quality Monitoring Dashboard"),
          p("Real-Time Pollution Insights & Environmental Analytics")
        ),
        div(class = "data-stamp", icon("calendar-days"), paste("Latest reading ·", format(latest_record$Date[[1]], "%d %b %Y")))
      )
    )
  }

  output$page_content <- renderUI({
    switch(
      input$page,
      overview = tagList(
        overview_header(),
        div(
          class = "kpi-grid",
          div(class = "kpi-card", div(class = "kpi-label", "Current AQI"), div(class = "kpi-value", latest_aqi), div(class = "kpi-note", paste("Latest ·", latest_record$City[[1]]))),
          div(class = "kpi-card", div(class = "kpi-label", "Average AQI"), div(class = "kpi-value", round(mean(air_quality$AQI), 1)), div(class = "kpi-note", "Across all records")),
          div(class = "kpi-card", div(class = "kpi-label", "PM2.5"), div(class = "kpi-value", paste0(latest_record$PM2_5[[1]], " ", span(class = "kpi-unit", "µg/m³"))), div(class = "kpi-note", paste("Latest ·", latest_record$City[[1]]))),
          div(class = "kpi-card", div(class = "kpi-label", "Air Quality Status"), div(class = "status-pill", style = paste0("background:", category_colors[[latest_category]], "1c;color:", category_colors[[latest_category]], ";"), latest_category), div(class = "kpi-note", "Based on the latest available record"))
        ),
        section_heading("At a glance", "The latest reading and distribution of recorded air-quality categories."),
        div(
          class = "chart-grid",
          chart_card("AQI at latest reading", "Latest record · gauge shown on a 0–300 scale", plotlyOutput("aqi_gauge", height = "300px")),
          chart_card("AQI category distribution", "Share of observations in each category", plotlyOutput("category_donut", height = "300px"))
        ),
        section_heading("Pollution over time", "Recorded measurements across cities; hover for exact values."),
        div(
          class = "chart-grid",
          chart_card("PM2.5 trend", "Fine particulate matter · µg/m³", plotlyOutput("overview_pm25", height = "310px")),
          chart_card("PM10 trend", "Coarse particulate matter · µg/m³", plotlyOutput("overview_pm10", height = "310px"))
        ),
        chart_card("Pollutant comparison", "Average concentration as a percentage of each pollutant's own observed maximum; values use different units.", plotlyOutput("pollutant_comparison", height = "340px")),
        section_heading("Observation records", "Filter and browse the synthetic monitoring records used by this dashboard."),
        div(
          class = "filter-card",
          uiOutput("table_city_filter_ui"),
          uiOutput("table_category_filter_ui"),
          uiOutput("table_dates_filter_ui")
        ),
        div(class = "chart-card", DTOutput("records_table"))
      ),
      air_quality = tagList(
        div(class = "page-hero", div(div(class = "eyebrow", "Pollutant analysis"), h1("Air Quality"), p("Explore how environmental conditions and pollutants vary with AQI."))),
        section_heading("Relationships in the data", "Hover over observations for city, date, pollutant values, and AQI."),
        div(
          class = "chart-grid",
          chart_card("AQI vs PM2.5", "Fine particulate matter · µg/m³", plotlyOutput("scatter_pm25", height = "320px")),
          chart_card("AQI vs PM10", "Particulate matter · µg/m³", plotlyOutput("scatter_pm10", height = "320px")),
          chart_card("Temperature vs AQI", "Temperature · °C", plotlyOutput("scatter_temperature", height = "320px")),
          chart_card("Humidity vs AQI", "Relative humidity · %", plotlyOutput("scatter_humidity", height = "320px"))
        ),
        section_heading("Data-led insights", "Correlation describes association in these records; it does not establish causation."),
        uiOutput("correlation_insights")
      ),
      comparison = tagList(
        div(class = "page-hero", div(div(class = "eyebrow", "Regional snapshot"), h1("City Comparison"), p("Compare average AQI and inspect a selected city's pollutant profile."))),
        section_heading("City Air Quality Comparison", "Average AQI by city, sorted from lower to higher (better to worse)."),
        chart_card("Average AQI by city", "Lower average AQI indicates cleaner air in this dataset.", plotlyOutput("city_aqi_chart", height = "380px")),
        section_heading("Selected city profile", "Select a city to update its summary and pollutant concentration chart."),
        div(
          class = "filter-card",
          selectInput("comparison_city", "Selected city", choices = city_choices, selected = city_choices[[1]])
        ),
        div(class = "kpi-grid", uiOutput("city_kpis")),
        chart_card("Pollution by selected city", "Average PM2.5, PM10, and NO2 concentrations · µg/m³", plotlyOutput("city_pollution_chart", height = "330px"))
      ),
      trends = tagList(
        div(class = "page-hero", div(div(class = "eyebrow", "Historical monitoring"), h1("Trends"), p("Follow daily AQI and monthly pollution patterns."))),
        div(
          class = "filter-card",
          selectInput("trends_city", "City", choices = c("All Cities", city_choices), selected = "All Cities")
        ),
        section_heading("Time-series overview", "Daily readings are averaged by date when multiple records are available."),
        chart_card("AQI trend", "Daily average AQI · 2025", plotlyOutput("aqi_trend", height = "350px")),
        div(
          class = "chart-grid",
          chart_card("Monthly average AQI", "Monthly average across the selected city or cities.", plotlyOutput("monthly_aqi", height = "320px")),
          chart_card("Monthly PM2.5", "Monthly average fine particulate matter · µg/m³.", plotlyOutput("monthly_pm25", height = "320px"))
        )
      ),
      checker = tagList(
        div(class = "page-hero", div(div(class = "eyebrow", "Quick estimate"), h1("AQI Checker"), p("Explore an educational estimate from three pollutant inputs."))),
        section_heading("Air Quality Risk Checker", "Enter measured pollutant concentrations to estimate an AQI category."),
        div(
          class = "content-card about-card",
          div(class = "estimator-note", icon("triangle-exclamation"), "Educational AQI Estimator — this simplified estimate is not an official government AQI calculation."),
          div(
            class = "filter-card",
            numericInput("checker_pm25", "PM2.5 (µg/m³)", value = 18, min = 0, max = 500, step = 1),
            numericInput("checker_pm10", "PM10 (µg/m³)", value = 35, min = 0, max = 600, step = 1),
            numericInput("checker_no2", "NO2 (µg/m³)", value = 20, min = 0, max = 400, step = 1)
          ),
          uiOutput("checker_result")
        )
      ),
      about = tagList(
        div(class = "page-hero", div(div(class = "eyebrow", "About this project"), h1("Air Quality Monitoring Dashboard"), p("An accessible view of synthetic air-quality observations and indicators."))),
        div(
          class = "content-card about-card",
          h3("Project Objective"),
          p("An interactive environmental analytics dashboard designed to monitor air-quality indicators, compare pollution levels across cities, and visualize AQI trends."),
          h3("Features"),
          div(
            class = "feature-grid",
            div(class = "feature-chip", icon("gauge-high"), "AQI monitoring"),
            div(class = "feature-chip", icon("chart-line"), "Pollution analysis"),
            div(class = "feature-chip", icon("city"), "City comparison"),
            div(class = "feature-chip", icon("calendar-days"), "Historical trends"),
            div(class = "feature-chip", icon("chart-simple"), "Interactive charts"),
            div(class = "feature-chip", icon("sliders"), "AQI estimator & data filtering")
          ),
          h3(style = "margin-top:25px;", "Technologies"),
          div(class = "technology-tags", lapply(c("R", "Shiny", "bslib", "Plotly", "ggplot2", "dplyr", "DT"), tags$span)),
          div(class = "estimator-note", style = "margin-top:25px;margin-bottom:0;", "Academic Data Analytics Project")
        ),
        div(
          class = "content-card about-card",
          h3("Data note"),
          p("The included dataset contains reproducible synthetic observations for eight cities over 2025. It is for interface demonstration and educational analysis only, not live monitoring or regulatory reporting.")
        )
      )
    )
  })

  output$aqi_gauge <- renderPlotly({
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = latest_aqi,
      number = list(font = list(size = 40, color = "#203a36")),
      gauge = list(
        axis = list(range = list(0, 300), tickwidth = 1, tickcolor = "#a8b9b3", tickfont = list(size = 10)),
        bar = list(color = category_colors[[latest_category]], thickness = 0.22),
        bgcolor = "white",
        borderwidth = 0,
        steps = list(
          list(range = c(0, 50), color = "#e7f3eb"),
          list(range = c(50, 100), color = "#fbf2d8"),
          list(range = c(100, 150), color = "#fae8d9"),
          list(range = c(150, 200), color = "#f7dfdc"),
          list(range = c(200, 300), color = "#eee5f0")
        ),
        threshold = list(line = list(color = "#29433e", width = 3), thickness = 0.8, value = latest_aqi)
      ),
      title = list(text = paste(latest_category, "<br><span style='font-size:11px;color:#82918c'>", latest_record$City[[1]], "</span>"), font = list(size = 14, color = "#49615a"))
    ) %>%
      layout(margin = list(l = 25, r = 25, t = 50, b = 15), paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$category_donut <- renderPlotly({
    distribution <- air_quality %>%
      count(AQI_Category, .drop = FALSE) %>%
      filter(n > 0)
    plot_ly(
      distribution,
      labels = ~AQI_Category,
      values = ~n,
      type = "pie",
      hole = 0.66,
      sort = FALSE,
      textinfo = "label+percent",
      textposition = "outside",
      marker = list(colors = unname(category_colors[as.character(distribution$AQI_Category)]), line = list(color = "#ffffff", width = 3)),
      hovertemplate = "%{label}<br>%{value} records (%{percent})<extra></extra>"
    ) %>%
      layout(showlegend = FALSE, margin = list(l = 20, r = 20, t = 15, b = 15), paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$overview_pm25 <- renderPlotly({
    p <- ggplot(air_quality, aes(Date, PM2_5, color = City, text = paste0(
      "City: ", City, "<br>Date: ", format(Date, "%d %b %Y"),
      "<br>PM2.5: ", PM2_5, " µg/m³<br>AQI: ", AQI
    ))) +
      geom_line(aes(group = City), alpha = 0.32, linewidth = 0.45) +
      geom_point(alpha = 0.65, size = 1.2) +
      scale_color_manual(values = rep(chart_colors, length.out = length(city_choices))) +
      labs(x = NULL, y = "PM2.5 (µg/m³)") + chart_theme
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = -0.3), margin = list(b = 75)) %>% config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$overview_pm10 <- renderPlotly({
    p <- ggplot(air_quality, aes(Date, PM10, color = City, text = paste0(
      "City: ", City, "<br>Date: ", format(Date, "%d %b %Y"),
      "<br>PM10: ", PM10, " µg/m³<br>AQI: ", AQI
    ))) +
      geom_line(aes(group = City), alpha = 0.32, linewidth = 0.45) +
      geom_point(alpha = 0.65, size = 1.2) +
      scale_color_manual(values = rep(chart_colors, length.out = length(city_choices))) +
      labs(x = NULL, y = "PM10 (µg/m³)") + chart_theme
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = -0.3), margin = list(b = 75)) %>% config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$pollutant_comparison <- renderPlotly({
    pollutant_columns <- c("PM2_5", "PM10", "NO2", "SO2", "CO", "O3")
    pollutant_labels <- c("PM2.5", "PM10", "NO2", "SO2", "CO", "O3")
    means <- vapply(air_quality[pollutant_columns], mean, numeric(1))
    maxima <- vapply(air_quality[pollutant_columns], max, numeric(1))
    comparison <- data.frame(Pollutant = pollutant_labels, Relative_Mean = round(100 * means / maxima, 1))
    plot_ly(
      comparison,
      x = ~Pollutant,
      y = ~Relative_Mean,
      type = "bar",
      marker = list(color = c("#1f7a72", "#3f8f8a", "#78aaa0", "#d29a39", "#c86b51", "#78628d")),
      text = ~paste0(Relative_Mean, "%"),
      textposition = "outside",
      hovertemplate = "%{x}<br>Mean as % of its observed maximum: %{y:.1f}%<extra></extra>"
    ) %>%
      nice_plot("Mean / observed maximum (%)") %>%
      layout(yaxis = list(range = c(0, 115), title = "Mean / observed maximum (%)"))
  })

  scatter_plot <- function(x, x_label) {
    p <- ggplot(air_quality, aes(x = .data[[x]], y = AQI, color = City, text = paste0(
      "City: ", City, "<br>Date: ", format(Date, "%d %b %Y"),
      "<br>", x_label, ": ", .data[[x]], "<br>AQI: ", AQI
    ))) +
      geom_point(alpha = 0.7, size = 2) +
      scale_color_manual(values = rep(chart_colors, length.out = length(city_choices))) +
      labs(x = x_label, y = "AQI") + chart_theme
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.3), margin = list(b = 75)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  output$scatter_pm25 <- renderPlotly(scatter_plot("PM2_5", "PM2.5 (µg/m³)"))
  output$scatter_pm10 <- renderPlotly(scatter_plot("PM10", "PM10 (µg/m³)"))
  output$scatter_temperature <- renderPlotly(scatter_plot("Temperature", "Temperature (°C)"))
  output$scatter_humidity <- renderPlotly(scatter_plot("Humidity", "Humidity (%)"))

  output$correlation_insights <- renderUI({
    div(
      class = "insight-list",
      div(class = "insight", icon("chart-line"), correlation_insight(air_quality, "PM2_5", "PM2.5")),
      div(class = "insight", icon("chart-line"), correlation_insight(air_quality, "PM10", "PM10")),
      div(class = "insight", icon("temperature-half"), correlation_insight(air_quality, "Temperature", "Temperature")),
      div(class = "insight", icon("droplet"), correlation_insight(air_quality, "Humidity", "Humidity"))
    )
  })

  city_averages <- reactive({
    air_quality %>%
      group_by(City) %>%
      summarise(Avg_AQI = mean(AQI), .groups = "drop") %>%
      arrange(Avg_AQI)
  })

  output$city_aqi_chart <- renderPlotly({
    plot_ly(
      city_averages(),
      x = ~Avg_AQI,
      y = ~City,
      type = "bar",
      orientation = "h",
      marker = list(color = ~Avg_AQI, colorscale = list(c(0, "#78aaa0"), c(0.5, "#d8b45c"), c(1, "#c86b51")), showscale = FALSE),
      text = ~round(Avg_AQI, 1),
      textposition = "outside",
      hovertemplate = "%{y}<br>Average AQI: %{x:.1f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Average AQI", gridcolor = "#edf2f0", range = c(0, max(city_averages()$Avg_AQI) * 1.18)),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(city_averages()$City)),
        margin = list(l = 100, r = 35, t = 10, b = 45),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, system-ui, sans-serif", color = "#4f625d")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  selected_city_data <- reactive({
    req(input$comparison_city)
    air_quality %>% filter(City == input$comparison_city)
  })

  output$city_kpis <- renderUI({
    data <- selected_city_data()
    average_aqi <- mean(data$AQI)
    average_category <- category_levels[which(average_aqi <= c(50, 100, 150, 200, Inf))[1]]
    tagList(
      div(class = "kpi-card", div(class = "kpi-label", "Selected City"), div(class = "kpi-value", input$comparison_city), div(class = "kpi-note", paste(nrow(data), "records"))),
      div(class = "kpi-card", div(class = "kpi-label", "Average AQI"), div(class = "kpi-value", round(average_aqi, 1)), div(class = "kpi-note", "Selected city")),
      div(class = "kpi-card", div(class = "kpi-label", "Average PM2.5"), div(class = "kpi-value", paste0(round(mean(data$PM2_5), 1), " ", span(class = "kpi-unit", "µg/m³"))), div(class = "kpi-note", "Selected city")),
      div(class = "kpi-card", div(class = "kpi-label", "Average PM10"), div(class = "kpi-value", paste0(round(mean(data$PM10), 1), " ", span(class = "kpi-unit", "µg/m³"))), div(class = "kpi-note", "Selected city")),
      div(class = "kpi-card", div(class = "kpi-label", "Air Quality Category"), div(class = "status-pill", style = paste0("background:", category_colors[[average_category]], "1c;color:", category_colors[[average_category]], ";"), average_category), div(class = "kpi-note", "Category of average AQI"))
    )
  })

  output$city_pollution_chart <- renderPlotly({
    data <- selected_city_data()
    values <- data.frame(
      Pollutant = c("PM2.5", "PM10", "NO2"),
      Mean = c(mean(data$PM2_5), mean(data$PM10), mean(data$NO2))
    )
    plot_ly(
      values,
      x = ~Pollutant,
      y = ~Mean,
      type = "bar",
      marker = list(color = c("#1f7a72", "#4b9c8d", "#87b9aa")),
      text = ~round(Mean, 1),
      textposition = "outside",
      hovertemplate = "%{x}<br>Average: %{y:.1f} µg/m³<extra></extra>"
    ) %>%
      nice_plot("Average concentration (µg/m³)") %>%
      layout(title = list(text = input$comparison_city, x = 0.02, font = list(size = 13)))
  })

  trends_data <- reactive({
    data <- air_quality
    if (!is.null(input$trends_city) && input$trends_city != "All Cities") {
      data <- data %>% filter(City == input$trends_city)
    }
    data %>%
      group_by(Date) %>%
      summarise(AQI = mean(AQI), PM2_5 = mean(PM2_5), .groups = "drop") %>%
      arrange(Date)
  })

  monthly_data <- reactive({
    trends_data() %>%
      mutate(Month = as.Date(format(Date, "%Y-%m-01"))) %>%
      group_by(Month) %>%
      summarise(AQI = mean(AQI), PM2_5 = mean(PM2_5), .groups = "drop") %>%
      arrange(Month) %>%
      mutate(Month_Label = format(Month, "%b %Y"))
  })

  output$aqi_trend <- renderPlotly({
    data <- trends_data()
    if (nrow(data) == 0) return(empty_plot("No records available for this selection."))
    plot_ly(data, x = ~Date, y = ~AQI, type = "scatter", mode = "lines+markers", line = list(color = "#1f7a72", width = 2), marker = list(color = "#1f7a72", size = 5), text = ~paste0(format(Date, "%d %b %Y"), "<br>Average AQI: ", round(AQI, 1)), hoverinfo = "text") %>%
      nice_plot("Average AQI") %>%
      layout(xaxis = list(title = "", gridcolor = "#edf2f0", rangeselector = list(buttons = list(list(count = 3, label = "3m", step = "month", stepmode = "backward"), list(count = 6, label = "6m", step = "month", stepmode = "backward"), list(step = "all", label = "All"))), rangeslider = list(visible = FALSE)))
  })

  output$monthly_aqi <- renderPlotly({
    data <- monthly_data()
    plot_ly(data, x = ~Month, y = ~AQI, type = "bar", marker = list(color = "#1f7a72"), text = ~round(AQI, 1), textposition = "outside", hovertemplate = "%{x|%b %Y}<br>Average AQI: %{y:.1f}<extra></extra>") %>%
      nice_plot("Average AQI") %>%
      layout(xaxis = list(title = "", tickformat = "%b %Y"), yaxis = list(title = "Average AQI"))
  })

  output$monthly_pm25 <- renderPlotly({
    data <- monthly_data()
    plot_ly(data, x = ~Month, y = ~PM2_5, type = "scatter", mode = "lines+markers", line = list(color = "#d29a39", width = 3), marker = list(color = "#d29a39", size = 7), text = ~paste0(format(Month, "%b %Y"), "<br>Average PM2.5: ", round(PM2_5, 1), " µg/m³"), hoverinfo = "text") %>%
      nice_plot("PM2.5 (µg/m³)") %>%
      layout(xaxis = list(title = "", tickformat = "%b %Y"), yaxis = list(title = "PM2.5 (µg/m³)"))
  })

  checker_result <- reactive({
    pm25 <- input$checker_pm25
    pm10 <- input$checker_pm10
    no2 <- input$checker_no2
    validate(
      need(!is.null(pm25) && !is.null(pm10) && !is.null(no2), "Enter values for all pollutants."),
      need(all(is.finite(c(pm25, pm10, no2))) && all(c(pm25, pm10, no2) >= 0), "Enter non-negative numeric values.")
    )
    estimate <- round(min(300, pm25 * 1.45 + pm10 * 0.45 + no2 * 0.35))
    category <- category_levels[which(estimate <= c(50, 100, 150, 200, Inf))[1]]
    recommendation <- switch(
      category,
      "Good" = "Air quality is generally suitable for outdoor activities.",
      "Moderate" = "Sensitive individuals may consider reducing prolonged outdoor exposure.",
      "Unhealthy for Sensitive Groups" = "Sensitive individuals should consider reducing prolonged outdoor exposure.",
      "Unhealthy" = "Consider limiting prolonged outdoor activity.",
      "Very Unhealthy" = "Consider avoiding prolonged outdoor exposure."
    )
    list(estimate = estimate, category = category, recommendation = recommendation)
  })

  output$checker_result <- renderUI({
    result <- checker_result()
    div(
      class = "kpi-grid",
      div(class = "kpi-card", div(class = "kpi-label", "Estimated AQI"), div(class = "kpi-value", result$estimate), div(class = "kpi-note", "Simplified educational estimate")),
      div(class = "kpi-card", div(class = "kpi-label", "Air Quality Status"), div(class = "status-pill", style = paste0("background:", category_colors[[result$category]], "1c;color:", category_colors[[result$category]], ";"), result$category), div(class = "kpi-note", "Not an official AQI calculation")),
      div(class = "recommendation", style = "grid-column:span 2;align-self:center;", icon("leaf"), " ", result$recommendation)
    )
  })

  output$table_city_filter_ui <- renderUI({
    selectInput("table_city_filter", "City", choices = c("All Cities", city_choices), selected = "All Cities")
  })

  output$table_category_filter_ui <- renderUI({
    selectInput("table_category_filter", "AQI Category", choices = c("All Categories", category_levels), selected = "All Categories")
  })

  output$table_dates_filter_ui <- renderUI({
    dateRangeInput(
      "table_dates_filter",
      "Date range",
      start = min(air_quality$Date),
      end = max(air_quality$Date),
      min = min(air_quality$Date),
      max = max(air_quality$Date),
      format = "yyyy-mm-dd"
    )
  })

  output$records_table <- renderDT({
    data <- air_quality
    if (!is.null(input$table_city_filter) && input$table_city_filter != "All Cities") data <- data %>% filter(City == input$table_city_filter)
    if (!is.null(input$table_category_filter) && input$table_category_filter != "All Categories") data <- data %>% filter(AQI_Category == input$table_category_filter)
    if (!is.null(input$table_dates_filter) && length(input$table_dates_filter) == 2) {
      data <- data %>% filter(Date >= as.Date(input$table_dates_filter[1]), Date <= as.Date(input$table_dates_filter[2]))
    }
    names(data) <- c("Record ID", "Date", "City", "Temperature", "Humidity", "PM2.5", "PM10", "NO2", "SO2", "CO", "O3", "AQI", "AQI Category")
    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE, dom = "tip", order = list(list(1, "desc"))),
      class = "stripe hover compact"
    ) %>%
      formatRound(c("Temperature", "Humidity", "PM2.5", "PM10", "NO2", "SO2", "O3"), 1) %>%
      formatRound("CO", 2)
  }, server = TRUE)
}

shinyApp(ui, server)
