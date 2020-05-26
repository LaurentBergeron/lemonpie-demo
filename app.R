# Copyright 2020, Laurent Bergeron, All rights reserved.




library(shiny)
library(shinydashboard)
library(DT)
source("tools.R")
source("plot.R")

# Define UI
ui <- dashboardPage(skin="green",

  # Application title
  dashboardHeader(title = TITLE),

  # Filters sidebar
  dashboardSidebar(tags$head(tags$style(type = "text/css", ".shiny-input-container {padding-top: 0px !important;}")),
    HTML("<h5><b>&nbsp;&nbsp;Date Range</b></h5>"),
    dateRangeInput("filterDateRange", NULL, start="2019-01-01", end="2019-12-31"),
    HTML("<h5><b>&nbsp;&nbsp;Amount Range ($)</b></h5>"),
    sliderInput("filterAmount", NULL, -MAX_AMOUNT, +MAX_AMOUNT, c(-MAX_AMOUNT,+MAX_AMOUNT)),
    HTML("<h5><b>&nbsp;&nbsp;Account</b></h5>"),
    checkboxInput("filterAccountSelectAll", "Select all", value=TRUE),
    checkboxGroupInput("filterAccount", NULL, choices=get_all_accounts(), select=get_all_accounts()),
    HTML("<h5><b>&nbsp;&nbsp;Heading</b></h5>"),
    checkboxInput("filterCategSelectAll", "Select all", value=TRUE),
    checkboxGroupInput("filterCateg", NULL, choices=get_valid_headings(), select=get_valid_headings())
  ),    
  # Main panel with tabs
  dashboardBody(
    tags$style('.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #d73925;
               }"'),
    tabBox(width = 12,
      # --- Analysis tab --- #
      tabPanel("Analysis",
        fluidRow(
          tabBox(width = 12,
            tabPanel("Total",
                     fluidRow(box(width = 12,
                       plotOutput("plotOut1", click="plotOut1_hover", height=PLOT_HEIGHT)
                     )),
                     "Click coordinates",
                     verbatimTextOutput("plotOut1_info")
            ),
            tabPanel("Total-by-month",
                     fluidRow(box(width = 12,
                     plotOutput("plotOut5", click="plotOut5_hover", height=PLOT_HEIGHT)
                     )),
                     "Click coordinates",
                     verbatimTextOutput("plotOut5_info")
            ),
            tabPanel("Headings",
                     fluidRow(box(width = 12,
                     plotOutput("plotOut2", click="plotOut2_hover", height=PLOT_HEIGHT)
                     )),
                     "Click coordinates",
                     verbatimTextOutput("plotOut2_info")
            ),
            tabPanel("Headings-by-month",
                     fluidRow(box(width = 12,
                     plotOutput("plotOut6", click="plotOut6_hover", height=PLOT_HEIGHT)
                     )),
                     "Click coordinates",
                     verbatimTextOutput("plotOut6_info")
            ),
            tabPanel("Accounts",
                     fluidRow(box(width = 12,
                     plotOutput("plotOut7", click="plotOut7_hover", height=PLOT_HEIGHT)
                     )),
                     "Click coordinates",
                     verbatimTextOutput("plotOut7_info")
            ),
            tabPanel("Accounts-by-month",
                     fluidRow(box(width = 12,
                     plotOutput("plotOut8", click="plotOut8_hover", height=PLOT_HEIGHT)
                     )),
                     "Click coordinates",
                     verbatimTextOutput("plotOut8_info")
            ),
            tabPanel("Categories",
                     fluidRow(box(width = 12,
                     plotOutput("plotOut3", width="100%", height = "800px")
                     ))
            ),
            tabPanel("Distribution",
                     fluidRow(box(width = 12,
                     plotOutput("plotOut4")
                     ))
            )
          )
        )
      ),
      # --- Database tab --- #
      tabPanel("Database", 
        DTOutput('databaseTable')
      ),
      # --- Import tab --- #
      tabPanel("Import", 
        wellPanel(
            fluidRow(
              column(4, fileInput("uploadedFile", "Choose CSV File",
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv"))),
              column(4, selectInput("importCSVFormat", "CSV format", possib_csvformats), style="vertical-align:bottom;"),
              column(4, uiOutput("resetableCSVButton", style="padding=30px;"))
            )
        ),
        textOutput("importNotificationText"),
        conditionalPanel("input.readCSVButton > 0", wellPanel(
          fluidRow(
            column(6, uiOutput("resetableOwnershipSelector")), 
            column(6, uiOutput("resetableAccountSelector"))
            ),
          fluidRow(
            column(6, uiOutput("resetableHeadingSelector")),
            column(6, uiOutput("resetableCategSelector"))
          ),
          fluidRow(
            column(4, actionButton("prevButton", "prev."),
                      actionButton("nextButton", "next"),
                      actionButton("finishImportButton", "Finish import"))
          ),
          DTOutput("statementTable")
        ))
      ),
      tabPanel("Help", htmlOutput("helpText")
      )
    )
  )
  
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Define reactive values (global variables that change from events)
  rv <- reactiveValues(current_import_row=5, 
                       database_df_full=data.frame(),
                       database_df_filter=data.frame(),
                       statement_df=data.frame(),
                       statement_df_show=data.frame(),
                       counter_csvbutton_id=0,
                       counter_ownership_id=0,
                       counter_account_id=0,
                       counter_heading_id=0,
                       counter_categ_id=0,
                       trigger_next=0
                       )
  
  
  # ----------------- Resetable UIs ----------------- 
  output$resetableCSVButton <- renderUI ({
    div(id=rv$counter_csvbutton_id,
        actionButton("readCSVButton", "Read csv"))
  })
  possib_owners <- get_valid_owners()
  output$resetableOwnershipSelector <- renderUI({
    div(id=rv$counter_ownership_id,
        selectInput("importOwnership", "Choose ownership...", c("..." = "", possib_owners))
    )
  })
  account_choice <- c("Choose ownership first..." = "")
  output$resetableAccountSelector <- renderUI({
    print(account_choice)
    div(id=rv$counter_account_id,
        selectInput("importAccount", "Choose account...", account_choice)
    )
  }) 
  possib_heading <- get_valid_headings()
  output$resetableHeadingSelector <- renderUI({
    div(id=rv$counter_heading_id,
        selectInput("importHeadingSelector", "Choose heading...", c("..." = "", possib_heading))
    )
  })
  categ_choice <- c("Choose heading first..." = "")
  output$resetableCategSelector <- renderUI({
    div(id=rv$counter_categ_id,
        selectInput("importCategorySelector", "Choose category...", categ_choice)
    )
  })
  # ----------------------------------------------------------------- 
  
  # --------------------------- Select all--------------------------- 
  observe({
    if (input$filterAccountSelectAll) {
      updateCheckboxGroupInput(session, "filterAccount", choices=get_all_accounts(), selected=get_all_accounts())
    } else {
      updateCheckboxGroupInput(session, "filterAccount", choices=get_all_accounts(), selected=c())
    }
  })
  observe({
    if (input$filterCategSelectAll) {
      updateCheckboxGroupInput(session, "filterCateg", choices=get_valid_headings(), selected=get_valid_headings())
    } else {
      updateCheckboxGroupInput(session, "filterCateg", choices=get_valid_headings(), selected=c())
    }
  })
  
  # ----------------------------------------------------------------- 
  
  
  # Initiate database
  reload_database(rv)
  output$statementTable <- renderDT({rv$statement_df_show})
  
  
  # --- Database tab --- #
  database_df_filter <- reactive({ apply_filters(input, rv$database_df_full) })
  output$databaseTable <- renderDT({ database_df_filter() })
  
  
  
  # --- Analysis tab --- #
  
  output$plotOut1 <- renderPlot({plot_time_series(input, database_df_filter())})
  output$plotOut1_info <- renderText({xy_click_str(input$plotOut1_hover)})
  
  output$plotOut5 <- renderPlot({plot_time_series_by_month(input, database_df_filter())})
  output$plotOut5_info <- renderText({xy_click_str(input$plotOut5_hover)})
  
  output$plotOut2 <- renderPlot({plot_time_series_by_COL(input, database_df_filter(), COL="Heading")})
  output$plotOut2_info <- renderText({xy_click_str(input$plotOut2_hover)})
  
  output$plotOut6 <- renderPlot({plot_time_series_by_COL_by_month(input, database_df_filter(), COL="Heading")})
  output$plotOut6_info <- renderText({xy_click_str(input$plotOut6_hover)})
  
  output$plotOut7 <- renderPlot({plot_time_series_by_COL(input, database_df_filter(), COL="OwnerAccount")})
  output$plotOut7_info <- renderText({xy_click_str(input$plotOut7_hover)})
  
  output$plotOut8 <- renderPlot({plot_time_series_by_COL_by_month(input, database_df_filter(), COL="OwnerAccount")})
  output$plotOut8_info <- renderText({xy_click_str(input$plotOut8_hover)})
  
  output$plotOut3 <- renderPlot({plot_categ_bars(input, database_df_filter())})
  
  output$plotOut4 <- renderPlot({plot_total_distrib(input, database_df_filter())})
  
  # --- Import tab --- #
  output$importNotificationText <- renderText({"Click 'Read csv' to begin import."})
  
  # READ CSV button
  observeEvent(input$readCSVButton, {
      # Import input file into a temporary dataframe
      rv$statement_df <- import_csv_statement(input, output)
      output$importNotificationText <- renderText({"Click 'Finish import' when you're done."})
      rv$current_import_row <- 1
      reload_statement_df(output, rv)
  })
  # PREV button
  observeEvent(input$prevButton, {
    decrement_current_row(rv)
    reload_statement_df(output, rv)
  })
  # NEXT button
  observeEvent(input$nextButton, {
    rv$trigger_next <- rv$trigger_next + 1
  })
  observeEvent(rv$trigger_next, {
    increment_current_row(rv)
    reload_statement_df(output, rv)
  })
  # PREV OR NEXT buttons
  observeEvent(input$prevButton + rv$trigger_next, {
    # Reset Heading selector
    rv$counter_heading_id <- rv$counter_heading_id + 1
    # Reset Categ selector
    categ_choice <<- c("Choose heading first..." = "")
    rv$counter_categ_id <- rv$counter_categ_id + 1
  })
  # FINISH IMPORT button
  observeEvent(input$finishImportButton, {
    success <- finish_import(output, data.frame(rv$statement_df))
    if (success) {
      # Reset csv button and make import panel invisible
      rv$counter_csvbutton_id <- rv$counter_csvbutton_id + 1
      # Reload database table
      reload_database(rv)
    }
    
  })
  # change OWNERSHIP
  observeEvent(input$importOwnership, {
    if (nrow(rv$statement_df) > 0 && input$importOwnership != ""){
      rv$statement_df['Ownership'] <- input$importOwnership
      rv$counter_account_id <- rv$counter_account_id + 1 
      account_choice <<- c("..." = "", get_valid_accounts(input$importOwnership))
      reload_statement_df(output, rv)
    }
  })
  # change ACCOUNT
  observeEvent(input$importAccount, {
    if (nrow(rv$statement_df) > 0 && input$importAccount != ""){
      rv$statement_df['Account'] <- input$importAccount
      reload_statement_df(output, rv)
    }
  })
  # change HEADING
  observeEvent(input$importHeadingSelector, {
    if (nrow(rv$statement_df) > 0 && input$importHeadingSelector != ""){
      rv$statement_df[rv$current_import_row, 'Heading'] <- input$importHeadingSelector
      reload_statement_df(output, rv)
      
      # Load possible categories into Category selector UI
      categ_choice <<- c("..." = "", get_valid_categories(input$importHeadingSelector))
      rv$counter_categ_id <- rv$counter_categ_id + 1
    }
  })
  # change CATEGORY
  observeEvent(input$importCategorySelector, {
    if (nrow(rv$statement_df) > 0 && input$importCategorySelector != ""){
      rv$statement_df[rv$current_import_row, 'Category'] <- input$importCategorySelector
      reload_statement_df(output, rv)
      rv$trigger_next <- rv$trigger_next + 1
    }
  })
  
  
  
  output$helpText <- renderUI({HTML("
  <b> *** DISCLAIMER *** </b><br/>
  The database used in this demo app has been randomly generated and is not meant to represent any real life situation. 
  <br/><br/>
  <b>Filter sidebar</b><br/>
  - Use the filters to limit the data processed for visualization. 
  For the \"Amount\" filter, note that Amounts beyond extrema values are included if the filter sits at the extrema value.
  <br/><br/>
  <b>Analysis</b><br/>
  - Use the Analysis tab to visualize budget over time or per category. 
  Click on the figure to display coordinates below.
  Data shown reacts to the filter sidebar.
  <br/><br/>
  <b>Database</b><br/>
  - Use the Database tab for a comprehensive table of all imported statements. 
  Data shown reacts to the filter sidebar.
  <br/><br/>
  <b>Import</b><br/>
  - To import a bank statement, select a csv file and choose appropriate CSV format.
  If your no CSV format matches your bank, a new function needs to be added to the program.
  Select Owner and Account. Select Heading and Category for each line of the statement.
  Use \"prev\" and \"next\" buttons to navigate the statement. 
  When you are finished, click \"Finish import\". You will receive a notifiation if the import was succesful. 
  ")})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

