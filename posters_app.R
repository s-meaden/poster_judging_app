library(shiny)
library(bslib)
library(dplyr)
library(DT)

# UI Definition
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "flatly", primary = "#2C3E50", success = "#18BC9C"),
  title = "RAD2026 Poster Scoring Dashboard",
  
  sidebar = sidebar(
    title = "Controls & Data Upload",
    fileInput("file1", "Upload Google Form CSV",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    hr(),
    uiOutput("career_stage_ui"),
    checkboxInput("show_flagged_only", "Show only posters with < 2 judges", FALSE),
    hr(),
    downloadButton("download_winners", "Download Winners (CSV)", class = "btn-success w-100 mb-2"),
    downloadButton("download_summary", "Download Full Summary (CSV)", class = "btn-outline-primary w-100")
  ),
  
  # Top KPI Summary Cards
  layout_column_wrap(
    width = 1/4,
    value_box(
      title = "Total Submissions",
      value = textOutput("kpi_total_submissions"),
      showcase = bsicons::bs_icon("file-earmark-text")
    ),
    value_box(
      title = "Unique Posters",
      value = textOutput("kpi_total_posters"),
      showcase = bsicons::bs_icon("easel")
    ),
    value_box(
      title = "Active Judges",
      value = textOutput("kpi_total_judges"),
      showcase = bsicons::bs_icon("people")
    ),
    value_box(
      title = "Needs More Judges",
      value = textOutput("kpi_flagged_posters"),
      showcase = bsicons::bs_icon("exclamation-triangle"),
      theme = "danger"
    )
  ),
  
  # Tabbed Views
  navset_card_tab(
    nav_panel(
      title = "🏆 Winners Leaderboard",
      DTOutput("winners_table")
    ),
    nav_panel(
      title = "📊 Full Poster Summary & Flags",
      DTOutput("summary_table")
    ),
    nav_panel(
      title = "📋 Raw Data",
      DTOutput("raw_table")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Process raw data dynamically upon upload
  raw_data <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    
    # Rename columns by index as defined in original script
    colnames(df)[c(2, 4, 5, 6, 7, 8, 9)] <- c(
      "email", "overall_qual", "aim", "approach", "data_analysis", "discussion", "career_stage"
    )
    
    # Identify Poster Number column (Column 3)
    poster_col_name <- colnames(df)[3]
    df <- df %>% rename(Poster.Number = !!sym(poster_col_name))
    
    # Convert score columns to numeric safely
    score_cols <- c("overall_qual", "aim", "approach", "data_analysis", "discussion")
    df[score_cols] <- lapply(df[score_cols], function(x) suppressWarnings(as.numeric(as.character(x))))
    
    # Calculate total scores
    df2 <- df %>%
      mutate(total_score = overall_qual + aim + approach + data_analysis + discussion)
    
    # Remove Timestamp and column 10 if present
    cols_to_remove <- c()
    if ("Timestamp" %in% colnames(df2)) cols_to_remove <- c(cols_to_remove, "Timestamp")
    if (ncol(df) >= 10 && colnames(df)[10] %in% colnames(df2)) cols_to_remove <- c(cols_to_remove, colnames(df)[10])
    
    if (length(cols_to_remove) > 0) {
      df2 <- df2 %>% select(-any_of(cols_to_remove))
    }
    
    df2
  })
  
  # Dynamic Career Stage Filter Dropdown
  output$career_stage_ui <- renderUI({
    req(raw_data())
    stages <- unique(raw_data()$career_stage)
    stages <- stages[!is.na(stages) & stages != ""]
    selectInput("selected_stage", "Filter Career Stage:", choices = c("All Stages", stages), selected = "All Stages")
  })
  
  # Poster Summary calculation
  poster_summary <- reactive({
    req(raw_data())
    
    res <- raw_data() %>%
      group_by(Poster.Number, career_stage) %>%
      summarise(
        n_judges = n_distinct(email[!is.na(email) & email != ""]),
        mean_score = round(mean(total_score, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(
        flag = if_else(n_judges < 2, "CHECK - fewer than 2 judges", "OK")
      )
    
    if (!is.null(input$selected_stage) && input$selected_stage != "All Stages") {
      res <- res %>% filter(career_stage == input$selected_stage)
    }
    
    if (isTRUE(input$show_flagged_only)) {
      res <- res %>% filter(n_judges < 2)
    }
    
    res
  })
  
  # Winner Rankings calculation (ordered by career stage, then highest mean score)
  winners <- reactive({
    req(poster_summary())
    poster_summary() %>%
      arrange(career_stage, desc(mean_score))
  })
  
  # Key Metrics Display
  output$kpi_total_submissions <- renderText({
    if (is.null(input$file1)) return("0")
    nrow(raw_data())
  })
  
  output$kpi_total_posters <- renderText({
    if (is.null(input$file1)) return("0")
    n_distinct(raw_data()$Poster.Number)
  })
  
  output$kpi_total_judges <- renderText({
    if (is.null(input$file1)) return("0")
    n_distinct(raw_data()$email[!is.na(raw_data()$email) & raw_data()$email != ""])
  })
  
  output$kpi_flagged_posters <- renderText({
    if (is.null(input$file1)) return("0")
    sum(poster_summary()$n_judges < 2, na.rm = TRUE)
  })
  
  # Interactive Winners Table (In-App Leaderboard)
  output$winners_table <- renderDT({
    req(winners())
    datatable(
      winners(),
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        order = list(list(1, 'asc'), list(3, 'desc')), # Sort by Career Stage asc, Mean Score desc
        searchHighlight = TRUE
      ),
      colnames = c("Poster #", "Career Stage", "# Judges", "Mean Score", "Status"),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatStyle('mean_score', fontWeight = 'bold', backgroundColor = '#e8f8f5')
  })
  
  # Summary Table sorted by flag priority
  output$summary_table <- renderDT({
    req(poster_summary())
    datatable(
      poster_summary() %>% arrange(n_judges, Poster.Number),
      options = list(pageLength = 15, autoWidth = TRUE),
      colnames = c("Poster #", "Career Stage", "# Judges", "Mean Score", "Status"),
      rownames = FALSE
    ) %>%
      formatStyle(
        'flag',
        target = 'row',
        backgroundColor = styleEqual("CHECK - fewer than 2 judges", '#f8d7da')
      )
  })
  
  # Raw Data View
  output$raw_table <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # Download Handlers
  output$download_winners <- downloadHandler(
    filename = function() { paste0("RAD2026_Poster_Winners_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(winners(), file, row.names = FALSE) }
  )
  
  output$download_summary <- downloadHandler(
    filename = function() { paste0("RAD2026_Poster_Summary_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(poster_summary(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)
