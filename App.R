#############################
## app.R – FDA Approvals Dashboard (No Email)
#############################

library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(DT)
library(stringr)
library(tidyr)      # for expand_grid
library(openxlsx)   # Excel export
library(rmarkdown)  # PDF export
library(knitr)      # tables for PDF/HTML

# ==============
# 0. Global helpers & config
# ==============

OPENFDA_BASE <- "https://api.fda.gov/drug/drugsfda.json"

# OPTIONAL: if you get throttled, request an API key from openFDA and set it here
OPENFDA_API_KEY <- Sys.getenv("OPENFDA_API_KEY", unset = NA_character_)

# Helper to call openFDA drugsfda endpoint safely
fetch_drugsfda <- function(search = "", sort = NULL, limit = 100, skip = 0) {
  query <- list(
    search = search,
    limit  = limit,
    skip   = skip
  )
  if (!is.null(sort)) query$sort <- sort
  if (!is.na(OPENFDA_API_KEY)) query$api_key <- OPENFDA_API_KEY
  
  resp <- httr::GET(OPENFDA_BASE, query = query)
  if (httr::status_code(resp) != 200) {
    warning("openFDA request failed: ", httr::content(resp, "text"))
    return(NULL)
  }
  
  json <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  parsed
}

# Flatten drugsfda JSON into a tibble of (application, product, submission) rows
flatten_drugsfda <- function(parsed) {
  results <- parsed$results
  if (is.null(results)) return(tibble())
  
  map_dfr(results, function(app) {
    app_num      <- app$application_number %||% NA_character_
    sponsor_name <- app$sponsor_name %||% NA_character_
    
    products    <- app$products    %||% list()
    submissions <- app$submissions %||% list()
    
    if (length(products) == 0)    products    <- list(list())
    if (length(submissions) == 0) submissions <- list(list())
    
    # All combinations product x submission per application
    expand_grid(
      product    = seq_along(products),
      submission = seq_along(submissions)
    ) |>
      mutate(
        application_number = app_num,
        sponsor_name       = sponsor_name,
        brand_name   = map_chr(product, ~ products[[.x]]$brand_name   %||% NA_character_),
        generic_name = map_chr(product, ~ products[[.x]]$generic_name %||% NA_character_),
        dosage_form  = map_chr(product, ~ products[[.x]]$dosage_form  %||% NA_character_),
        route        = map_chr(product, ~ paste(products[[.x]]$route  %||% NA_character_, collapse = "; ")),
        marketing_status = map_chr(product, ~ products[[.x]]$marketing_status %||% NA_character_),
        application_type  = substr(application_number, 1, 4),
        submission_type   = map_chr(submission, ~ submissions[[.x]]$submission_type %||% NA_character_),
        submission_status = map_chr(submission, ~ submissions[[.x]]$submission_status %||% NA_character_),
        submission_status_date_raw = map_chr(submission, ~ submissions[[.x]]$submission_status_date %||% NA_character_),
        submission_class_code = map_chr(submission, ~ submissions[[.x]]$submission_class_code %||% NA_character_),
        submission_class_desc = map_chr(submission, ~ submissions[[.x]]$submission_class_code_description %||% NA_character_),
        review_priority = map_chr(submission, ~ submissions[[.x]]$review_priority %||% NA_character_)
      ) |>
      mutate(
        submission_status_date = suppressWarnings(
          lubridate::ymd(submission_status_date_raw)
        )
      )
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Utility: filter by date range safely
filter_by_date_range <- function(df, date_col, from, to) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.null(from) && is.null(to)) return(df)
  
  if (!is.null(from)) df <- df |> dplyr::filter(.data[[date_col]] >= from)
  if (!is.null(to))   df <- df |> dplyr::filter(.data[[date_col]] <= to)
  df
}

# Helper to create a simple PDF report for a data.frame
create_pdf_report <- function(df, title, file) {
  if (nrow(df) == 0) {
    tmp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(c(
      "---",
      "title: \"FDA Report\"",
      "output: pdf_document",
      "---",
      "",
      paste("##", title),
      "",
      "No data available for the selected filters."
    ), tmp_rmd)
    rmarkdown::render(tmp_rmd, output_file = file, quiet = TRUE)
    return(invisible(NULL))
  }
  
  tmp_rmd <- tempfile(fileext = ".Rmd")
  df_print <- head(df, 500)
  
  rmd_content <- c(
    "---",
    paste0("title: \"", title, "\""),
    "output: pdf_document",
    "---",
    "",
    paste("Report generated on", Sys.time()),
    "",
    "```{r echo=FALSE}",
    "knitr::kable(df_print, format = 'latex', booktabs = TRUE)",
    "```"
  )
  writeLines(rmd_content, tmp_rmd)
  
  env <- new.env(parent = globalenv())
  env$df_print <- df_print
  
  rmarkdown::render(
    tmp_rmd,
    output_file = file,
    envir = env,
    quiet = TRUE
  )
}

# ==============
# 1. UI
# ==============

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Gassem FDA Approvals Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("New Drug Approvals",      tabName = "new_drugs",  icon = icon("capsules")),
      menuItem("New Generic Approvals",   tabName = "generics",   icon = icon("copy")),
      menuItem("Recent Submissions",      tabName = "submissions",icon = icon("file-signature")),
      hr(),
      menuItem("About / Help",            tabName = "about",      icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # ---- Tab 1: New Drug Approvals (NDA/BLA) ----
      tabItem(
        tabName = "new_drugs",
        fluidRow(
          box(
            title = "Filters", width = 12, solidHeader = TRUE, status = "primary",
            dateRangeInput(
              "nd_date_range",
              "Approval status date range",
              start = Sys.Date() - 30,
              end   = Sys.Date()
            ),
            textInput("nd_search_term", "Search by brand or generic name (optional):", ""),
            actionButton("nd_refresh", "Refresh", icon = icon("sync"))
          )
        ),
        fluidRow(
          box(
            title = "Recently Approved New Drugs (NDA/BLA)",
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              style = "margin-bottom: 10px;",
              downloadButton("download_nd_excel", "Download Excel"),
              downloadButton("download_nd_pdf",   "Download PDF")
            ),
            DTOutput("nd_table")
          )
        )
      ),
      
      # ---- Tab 2: New Generic Approvals (ANDA) ----
      tabItem(
        tabName = "generics",
        fluidRow(
          box(
            title = "Filters", width = 12, solidHeader = TRUE, status = "primary",
            dateRangeInput(
              "gen_date_range",
              "Approval status date range",
              start = Sys.Date() - 30,
              end   = Sys.Date()
            ),
            textInput("gen_brand_ref", "Reference brand (optional):", ""),
            actionButton("gen_refresh", "Refresh", icon = icon("sync"))
          )
        ),
        fluidRow(
          box(
            title = "Recently Approved Generics (ANDA)",
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              style = "margin-bottom: 10px;",
              downloadButton("download_gen_excel", "Download Excel"),
              downloadButton("download_gen_pdf",   "Download PDF")
            ),
            DTOutput("gen_table")
          )
        )
      ),
      
      # ---- Tab 3: Recent Submissions (Applications Filed) ----
      tabItem(
        tabName = "submissions",
        fluidRow(
          box(
            title = "Filters", width = 12, solidHeader = TRUE, status = "primary",
            dateRangeInput(
              "sub_date_range",
              "Submission status date range",
              start = Sys.Date() - 30,
              end   = Sys.Date()
            ),
            selectInput(
              "sub_app_type",
              "Application type prefix (from application_number):",
              choices = c("All", "NDA", "ANDA", "BLA", "NDAX", "ANDAx"),
              selected = "All"
            ),
            selectInput(
              "sub_status",
              "Submission status (raw codes, may be empty in some records):",
              choices = c("All", "AP", "SU", "PND", "NA"),
              selected = "All"
            ),
            actionButton("sub_refresh", "Refresh", icon = icon("sync"))
          )
        ),
        fluidRow(
          box(
            title = "Recent Submissions / Applications",
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              style = "margin-bottom: 10px;",
              downloadButton("download_sub_excel", "Download Excel"),
              downloadButton("download_sub_pdf",   "Download PDF")
            ),
            DTOutput("sub_table")
          )
        )
      ),
      
      # ---- About tab ----
      tabItem(
        tabName = "about",
        box(
          width = 12, title = "About this dashboard", status = "info", solidHeader = TRUE,
          p("This dashboard uses :) ",
            "to retrieve information on drug applications, products, and submissions."),
          tags$ul(
            tags$li("New Drug Approvals tab shows only submissions with status = 'AP' ",
                    "for NDA/BLA-like application numbers."),
            tags$li("New Generic Approvals tab shows only submissions with status = 'AP' ",
                    "for ANDA (generic) applications."),
            tags$li("Recent Submissions tab shows all recent submissions, ",
                    "filterable by date, application prefix and status.")
          ),
          p("Tables are de-duplicated using dplyr::distinct() after selecting key columns.")
        )
      )
    )
  )
)

# ==============
# 2. Server
# ==============

server <- function(input, output, session) {
  
  # 2.1 New Drug Approvals (NDA/BLA) – only approved (submission_status == "AP")
  nd_data <- eventReactive(input$nd_refresh, {
    parsed <- fetch_drugsfda(
      search = 'submissions.submission_status:"AP"',
      sort   = "submissions.submission_status_date:desc",
      limit  = 200
    )
    df <- flatten_drugsfda(parsed)
    
    df <- df |>
      dplyr::filter(
        submission_status == "AP",   # ensure it's truly approved
        str_starts(application_number, "NDA") | 
          str_starts(application_number, "BLA")
      )
    
    dr <- input$nd_date_range
    df <- filter_by_date_range(df, "submission_status_date", dr[1], dr[2])
    
    term <- trimws(input$nd_search_term)
    if (nzchar(term)) {
      term_low <- tolower(term)
      df <- df |>
        dplyr::filter(
          str_detect(tolower(brand_name), term_low) |
            str_detect(tolower(generic_name), term_low)
        )
    }
    
    df |>
      arrange(desc(submission_status_date)) |>
      select(
        Approval_Date   = submission_status_date,
        Application     = application_number,
        Brand           = brand_name,
        Generic         = generic_name,
        Dosage_Form     = dosage_form,
        Route           = route,
        Sponsor         = sponsor_name,
        Submission_Type = submission_type,
        Submission_Status = submission_status,
        Class_Code      = submission_class_code,
        Class_Desc      = submission_class_desc
      ) |>
      distinct()   # remove exact duplicate rows
  }, ignoreNULL = FALSE)
  
  output$nd_table <- renderDT({
    datatable(
      nd_data(),
      options = list(pageLength = 25, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  
  output$download_nd_excel <- downloadHandler(
    filename = function() paste0("new_drug_approvals_", Sys.Date(), ".xlsx"),
    content = function(file) {
      openxlsx::write.xlsx(nd_data(), file)
    }
  )
  
  output$download_nd_pdf <- downloadHandler(
    filename = function() paste0("new_drug_approvals_", Sys.Date(), ".pdf"),
    content = function(file) {
      create_pdf_report(nd_data(), "New Drug Approvals (NDA/BLA)", file)
    }
  )
  
  # 2.2 New Generic Approvals (ANDA) – only approved (submission_status == "AP")
  gen_data <- eventReactive(input$gen_refresh, {
    parsed <- fetch_drugsfda(
      search = 'submissions.submission_status:"AP"',
      sort   = "submissions.submission_status_date:desc",
      limit  = 200
    )
    df <- flatten_drugsfda(parsed)
    
    df <- df |>
      dplyr::filter(
        submission_status == "AP",
        str_starts(application_number, "ANDA")
      )
    
    dr <- input$gen_date_range
    df <- filter_by_date_range(df, "submission_status_date", dr[1], dr[2])
    
    term <- trimws(input$gen_brand_ref)
    if (nzchar(term)) {
      term_low <- tolower(term)
      df <- df |>
        dplyr::filter(
          str_detect(tolower(brand_name), term_low) |
            str_detect(tolower(submission_class_desc), term_low)
        )
    }
    
    df |>
      arrange(desc(submission_status_date)) |>
      select(
        Approval_Date     = submission_status_date,
        Application       = application_number,
        Generic           = generic_name,
        Brand             = brand_name,
        Dosage_Form       = dosage_form,
        Route             = route,
        Sponsor           = sponsor_name,
        Marketing_Status  = marketing_status,
        Submission_Type   = submission_type,
        Submission_Status = submission_status
      ) |>
      distinct()
  }, ignoreNULL = FALSE)
  
  output$gen_table <- renderDT({
    datatable(
      gen_data(),
      options = list(pageLength = 25, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  
  output$download_gen_excel <- downloadHandler(
    filename = function() paste0("generic_approvals_", Sys.Date(), ".xlsx"),
    content = function(file) {
      openxlsx::write.xlsx(gen_data(), file)
    }
  )
  
  output$download_gen_pdf <- downloadHandler(
    filename = function() paste0("generic_approvals_", Sys.Date(), ".pdf"),
    content = function(file) {
      create_pdf_report(gen_data(), "Generic Drug Approvals (ANDA)", file)
    }
  )
  
  # 2.3 Recent Submissions / Applications Filed – keep all statuses
  sub_data <- eventReactive(input$sub_refresh, {
    parsed <- fetch_drugsfda(
      search = "",
      sort   = "submissions.submission_status_date:desc",
      limit  = 200
    )
    df <- flatten_drugsfda(parsed)
    
    dr <- input$sub_date_range
    df <- filter_by_date_range(df, "submission_status_date", dr[1], dr[2])
    
    if (input$sub_app_type != "All") {
      df <- df |>
        dplyr::filter(str_starts(application_number, input$sub_app_type))
    }
    
    if (input$sub_status != "All") {
      df <- df |>
        mutate(submission_status_clean = if_else(
          is.na(submission_status) | submission_status == "",
          "NA",
          submission_status
        )) |>
        dplyr::filter(submission_status_clean == input$sub_status)
    }
    
    df |>
      arrange(desc(submission_status_date)) |>
      select(
        Status_Date      = submission_status_date,
        Application      = application_number,
        Brand            = brand_name,
        Generic          = generic_name,
        Sponsor          = sponsor_name,
        Submission_Type  = submission_type,
        Submission_Status= submission_status,
        Review_Priority  = review_priority,
        Class_Code       = submission_class_code,
        Class_Desc       = submission_class_desc
      ) |>
      distinct()
  }, ignoreNULL = FALSE)
  
  output$sub_table <- renderDT({
    datatable(
      sub_data(),
      options = list(pageLength = 25, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  
  output$download_sub_excel <- downloadHandler(
    filename = function() paste0("fda_submissions_", Sys.Date(), ".xlsx"),
    content = function(file) {
      openxlsx::write.xlsx(sub_data(), file)
    }
  )
  
  output$download_sub_pdf <- downloadHandler(
    filename = function() paste0("fda_submissions_", Sys.Date(), ".pdf"),
    content = function(file) {
      create_pdf_report(sub_data(), "FDA Recent Submissions / Applications", file)
    }
  )
}

shinyApp(ui, server)
