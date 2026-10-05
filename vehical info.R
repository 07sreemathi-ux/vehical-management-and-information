#  ==========================================================
# Vehicle Information System - Shiny App
# ==========================================================
# Improvements over the plain script version:
# - Data frame backend (idiomatic R, not list-of-lists)
# - Full CRUD: Add, Update, Delete, Search/Filter
# - Input validation (duplicate vehicle numbers, empty fields, numeric checks)
# - Interactive UI with live-updating table
# - Filter by service status and fuel type
# - Downloadable CSV of current records
# ==========================================================

library(shiny)
library(DT)

# ---------------------------------------------------------
# Seed data (data frame instead of list-of-lists)
# ---------------------------------------------------------
initial_vehicles <- data.frame(
  Number = c("KA05MN1234", "MH12AB5678", "DL8CAF9012"),
  Brand = c("Toyota", "Honda", "Tata"),
  Model = c("Innova", "City", "Nexon"),
  Fuel = c("Diesel", "Petrol", "Electric"),
  Mileage = c(14.5, 17.8, 0.0),
  Service = c("Due", "Completed", "Completed"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------
ui <- fluidPage(
  titlePanel("Vehicle Information Management System"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Add / Update Vehicle"),
      textInput("number", "Vehicle Number", placeholder = "e.g. KA05MN1234"),
      textInput("brand", "Brand"),
      textInput("model", "Model"),
      selectInput("fuel", "Fuel Type",
                  choices = c("Petrol", "Diesel", "Electric", "CNG", "Hybrid")),
      numericInput("mileage", "Mileage (km/l, 0 for EV)", value = 0, min = 0, step = 0.1),
      selectInput("service", "Service Status",
                  choices = c("Due", "In Progress", "Completed")),
      br(),
      actionButton("add_btn", "Add Vehicle", class = "btn-success"),
      actionButton("update_btn", "Update Selected", class = "btn-primary"),
      actionButton("delete_btn", "Delete Selected", class = "btn-danger"),
      br(), br(),
      helpText("Select a row in the table first to Update or Delete it."),
      
      hr(),
      h4("Filter"),
      selectInput("filter_service", "Service Status",
                  choices = c("All", "Due", "In Progress", "Completed")),
      selectInput("filter_fuel", "Fuel Type",
                  choices = c("All", "Petrol", "Diesel", "Electric", "CNG", "Hybrid")),
      
      hr(),
      downloadButton("download_csv", "Download Records (CSV)")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Records",
                 br(),
                 DTOutput("vehicle_table"),
                 br(),
                 verbatimTextOutput("status_msg")
        ),
        tabPanel("Chat Assistant",
                 br(),
                 helpText("Ask about the fleet, e.g.: \"show due\", \"how many electric\",",
                          "\"find KA05MN1234\", \"average mileage\", \"list toyota\""),
                 fluidRow(
                   column(9, textInput("chat_input", NULL, placeholder = "Type a question...", width = "100%")),
                   column(3, actionButton("chat_send", "Send", class = "btn-primary", width = "100%"))
                 ),
                 br(),
                 uiOutput("chat_history")
        )
      )
    )
  )
)

# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------
server <- function(input, output, session) {
  
  vehicles <- reactiveVal(initial_vehicles)
  status <- reactiveVal("")
  
  # ---- Filtered view for the table ----
  filtered_vehicles <- reactive({
    df <- vehicles()
    if (input$filter_service != "All") {
      df <- df[df$Service == input$filter_service, ]
    }
    if (input$filter_fuel != "All") {
      df <- df[df$Fuel == input$filter_fuel, ]
    }
    df
  })
  
  output$vehicle_table <- renderDT({
    datatable(filtered_vehicles(), selection = "single", rownames = FALSE)
  })
  
  output$status_msg <- renderText({ status() })
  
  # ---- ADD ----
  observeEvent(input$add_btn, {
    df <- vehicles()
    
    # Validation
    if (trimws(input$number) == "" || trimws(input$brand) == "" || trimws(input$model) == "") {
      status("Error: Vehicle number, brand, and model are required.")
      return()
    }
    if (input$number %in% df$Number) {
      status(sprintf("Error: Vehicle number '%s' already exists.", input$number))
      return()
    }
    
    new_row <- data.frame(
      Number = input$number, Brand = input$brand, Model = input$model,
      Fuel = input$fuel, Mileage = input$mileage, Service = input$service,
      stringsAsFactors = FALSE
    )
    vehicles(rbind(df, new_row))
    status(sprintf("Added vehicle '%s'.", input$number))
  })
  
  # ---- UPDATE (selected row) ----
  observeEvent(input$update_btn, {
    sel <- input$vehicle_table_rows_selected
    if (is.null(sel)) {
      status("Select a row first to update it.")
      return()
    }
    df <- vehicles()
    target_number <- filtered_vehicles()[sel, "Number"]
    idx <- which(df$Number == target_number)
    
    df[idx, c("Brand", "Model", "Fuel", "Mileage", "Service")] <-
      list(input$brand, input$model, input$fuel, input$mileage, input$service)
    
    vehicles(df)
    status(sprintf("Updated vehicle '%s'.", target_number))
  })
  
  # ---- DELETE (selected row) ----
  observeEvent(input$delete_btn, {
    sel <- input$vehicle_table_rows_selected
    if (is.null(sel)) {
      status("Select a row first to delete it.")
      return()
    }
    df <- vehicles()
    target_number <- filtered_vehicles()[sel, "Number"]
    df <- df[df$Number != target_number, ]
    
    vehicles(df)
    status(sprintf("Deleted vehicle '%s'.", target_number))
  })
  
  # ---- Auto-fill the form when a row is clicked (for easy updates) ----
  observeEvent(input$vehicle_table_rows_selected, {
    sel <- input$vehicle_table_rows_selected
    if (!is.null(sel)) {
      row <- filtered_vehicles()[sel, ]
      updateTextInput(session, "number", value = row$Number)
      updateTextInput(session, "brand", value = row$Brand)
      updateTextInput(session, "model", value = row$Model)
      updateSelectInput(session, "fuel", selected = row$Fuel)
      updateNumericInput(session, "mileage", value = row$Mileage)
      updateSelectInput(session, "service", selected = row$Service)
    }
  })
  
  # ---- CSV download ----
  output$download_csv <- downloadHandler(
    filename = function() "vehicle_records.csv",
    content = function(file) {
      write.csv(vehicles(), file, row.names = FALSE)
    }
  )
  
  # ===========================================================
  # CHAT ASSISTANT (rule-based, queries the live data frame)
  # ===========================================================
  
  chat_log <- reactiveVal(list()) # list of list(sender="user"/"bot", text=...)
  
  # Core logic: turn a free-text question into an answer using the vehicle data
  answer_query <- function(q) {
    df <- vehicles()
    q_lc <- tolower(trimws(q))
    
    if (q_lc == "") {
      return("Please type a question.")
    }
    
    # Greeting
    if (grepl("^(hi|hello|hey)\\b", q_lc)) {
      return("Hello! Ask me about vehicles, e.g. 'show due', 'how many electric', 'find KA05MN1234'.")
    }
    
    # Help
    if (grepl("help|what can you do", q_lc)) {
      return(paste(
        "I can answer things like:\n",
        "- show all / list vehicles\n",
        "- show due / show completed / show in progress\n",
        "- how many vehicles / how many electric / how many diesel\n",
        "- find <vehicle number>\n",
        "- list <brand>\n",
        "- average mileage"
      ))
    }
    
    # Count vehicles, optionally by fuel type
    if (grepl("how many", q_lc)) {
      fuel_match <- c("petrol", "diesel", "electric", "cng", "hybrid")
      hit <- fuel_match[sapply(fuel_match, function(f) grepl(f, q_lc))]
      if (length(hit) > 0) {
        n <- sum(tolower(df$Fuel) == hit[1])
        return(sprintf("There are %d %s vehicle(s).", n, hit[1]))
      }
      return(sprintf("There are %d vehicle(s) in total.", nrow(df)))
    }
    
    # Average mileage
    if (grepl("average mileage|mileage average", q_lc)) {
      avg <- round(mean(df$Mileage[df$Mileage > 0]), 2)
      return(sprintf("Average mileage (excluding EVs) is %.2f km/l.", avg))
    }
    
    # Find a specific vehicle by number
    if (grepl("find|lookup|search", q_lc)) {
      match_no <- df$Number[sapply(df$Number, function(n) grepl(tolower(n), q_lc, fixed = TRUE))]
      if (length(match_no) > 0) {
        v <- df[df$Number == match_no[1], ]
        return(sprintf("%s: %s %s | Fuel: %s | Mileage: %.1f | Service: %s",
                       v$Number, v$Brand, v$Model, v$Fuel, v$Mileage, v$Service))
      }
      return("I couldn't find a vehicle matching that number.")
    }
    
    # Service status queries
    if (grepl("due", q_lc)) {
      res <- df[df$Service == "Due", ]
      if (nrow(res) == 0) return("No vehicles are currently due for service.")
      return(paste(sprintf("%s (%s %s)", res$Number, res$Brand, res$Model), collapse = "\n"))
    }
    if (grepl("completed", q_lc)) {
      res <- df[df$Service == "Completed", ]
      if (nrow(res) == 0) return("No vehicles have completed service.")
      return(paste(sprintf("%s (%s %s)", res$Number, res$Brand, res$Model), collapse = "\n"))
    }
    if (grepl("in progress", q_lc)) {
      res <- df[df$Service == "In Progress", ]
      if (nrow(res) == 0) return("No vehicles are currently in service.")
      return(paste(sprintf("%s (%s %s)", res$Number, res$Brand, res$Model), collapse = "\n"))
    }
    
    # List by brand
    if (grepl("^list |show ", q_lc)) {
      brands <- unique(tolower(df$Brand))
      hit <- brands[sapply(brands, function(b) grepl(b, q_lc))]
      if (length(hit) > 0) {
        res <- df[tolower(df$Brand) == hit[1], ]
        return(paste(sprintf("%s: %s %s (%s)", res$Number, res$Brand, res$Model, res$Service),
                     collapse = "\n"))
      }
      if (grepl("all", q_lc)) {
        return(paste(sprintf("%s: %s %s (%s)", df$Number, df$Brand, df$Model, df$Service),
                     collapse = "\n"))
      }
    }
    
    "Sorry, I didn't understand that. Type 'help' to see what I can answer."
  }
  
  observeEvent(input$chat_send, {
    req(trimws(input$chat_input) != "")
    user_msg <- input$chat_input
    bot_msg <- answer_query(user_msg)
    
    log <- chat_log()
    log[[length(log) + 1]] <- list(sender = "user", text = user_msg)
    log[[length(log) + 1]] <- list(sender = "bot", text = bot_msg)
    chat_log(log)
    
    updateTextInput(session, "chat_input", value = "")
  })
  
  output$chat_history <- renderUI({
    log <- chat_log()
    if (length(log) == 0) {
      return(helpText("No messages yet. Try asking 'how many vehicles' or 'show due'."))
    }
    tagList(lapply(log, function(m) {
      if (m$sender == "user") {
        div(style = "text-align:right; margin:6px;",
            span(style = "background:#daf1da; padding:6px 10px; border-radius:10px; display:inline-block;",
                 strong("You: "), m$text))
      } else {
        div(style = "text-align:left; margin:6px;",
            span(style = "background:#eef2f7; padding:6px 10px; border-radius:10px; display:inline-block; white-space:pre-wrap;",
                 strong("Bot: "), m$text))
      }
    }))
  })
}

# ---------------------------------------------------------
shinyApp(ui = ui, server = server)

