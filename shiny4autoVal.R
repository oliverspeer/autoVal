# libraries and db connection ----------------------------------------------------
source("StartUp.R")
StartUpRoutine()

# setting ggplot theme------------------------------------------------------
theme_set(
  theme_grey() +
    theme( text = element_text(size = 14),
           axis.title = element_text(size = 16),
           axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
           axis.text.y = element_text(size = 14))
)

# shiny app ----------------------------------------------------------------
# shinyOptions(cache = cache_mem(max_size = 5000e6))
# Define UI------------------------------------------------------------------
ui <- fluidPage(
  
  navbarPage(
    title = div(img(src="logo_pos.png",  
                    height = 28, 
                    width = 130, 
                    style = "margin:1px 3px", "  Klinische Chemie ")
    ), 
    # theme = shinytheme("paper"), 
    collapsible = TRUE,
    fluid = TRUE,
    

    
    # tabPanel-----------------------------------------------------------------------
    tabPanel("Methodenvalidation",
             # fluidRow(
             #   column(12, DTOutput("yearlyDevice"))
             # ),
             selectInput("method", "Wähle die Methode", choices = NULL),
             # fluidRow(
             #   column(12, DTOutput("nSamples"))
             # ),
             actionButton("generate.report", "Erstelle Validationsbericht"))
  )
)

# Define server logic---------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive value to store the method corresponding to the selected test name
  selectedMethod <- reactiveVal()
  
  # Update method choices based on the database
  updateSelectInput(session, "method",
                    choices = dbGetQuery(con, "SELECT DISTINCT TestName FROM DxIvalData 
                                         WHERE SampleID IS NOT NULL 
                                         ORDER BY TestName ASC"))
  
  # Observe changes in the selected TestName and update the Methode accordingly
  observe({
    testName <- input$method
    if (!is.null(testName)) {
      # Query to get the corresponding Method
      methodQuery <- sprintf("SELECT Methode FROM TranslationData WHERE TestName = '%s'", testName)
      methodResult <- dbGetQuery(con, methodQuery)
      # Assume methodResult returns one row with one column named 'Method'
      if (nrow(methodResult) > 0) {
        selectedMethod(methodResult$Method[1])
      } else {
        selectedMethod(NULL)  # No method found
      }
    }
  })
  
  
  # Generate report
  observeEvent(input$generate.report, {
    # Ensure method is not NULL before rendering
    if (!is.null(selectedMethod())) {
      rmarkdown::render("DxI_autoValOffcDwnWrd.Rmd", 
                            output_format = "all", 
                            params = list(method = selectedMethod()) 
                            )
    } else {
      # Handle case where no method is found
      showNotification("No method found for the selected test name.", type = "error")
    }
  })
}


# Run the application--------------------------------------------------------------
shinyApp(ui = ui, server = server)
