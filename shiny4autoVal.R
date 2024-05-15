# Load required libraries and establish database connection ---------------------
source("StartUp.R")
StartUpRoutine()


# Define UI----------------------------------------------------------------------
ui <- fluidPage(
  navbarPage(
    title = div(img(src="logo_pos.png", height = 28, width = 130, style = "margin:1px 3px"), " Klinische Chemie"), 
    collapsible = TRUE,
    fluid = TRUE,
    
    tabPanel("Methodenvalidation",
             selectInput("method", "Wähle die Methode", choices = NULL),
             fluidRow(
               column(3, DTOutput("nSamples"))
             ),
             actionButton("generate.report", "Erstelle Validationsbericht")
    )
  )
)

# Define server logic -----------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value to store the data corresponding to the selected test name
  validation.data <- reactiveVal()
  
  # Update method choices based on the database
  updateSelectInput(session, "method",
                    choices = dbGetQuery(con, "SELECT DISTINCT TestName FROM DxIvalData 
                                         WHERE SampleID IS NOT NULL 
                                         ORDER BY TestName ASC"))
  
  # Observe changes in the selected TestName and update the SQL query accordingly
  observeEvent(input$method, { 
    req(input$method)
    query.dxi.val <- "SELECT DISTINCT
                      a.Werte AS DxI800,
                      d.DoseResult AS DxI9000 --,
                      -- a.Bezeichnung,
                      -- a.Methode,
                      -- m.EINHEIT,
                      -- d.DoseUnit
                      FROM MeasurementData a
                      JOIN MethodData m ON a.Methode = m.Methode
                      JOIN TranslationData t ON a.Methode = t.Methode
                      JOIN DxIvalData d ON t.TestOrderCode = d.TestOrderCode
                      WHERE d.TestName = '%s' AND a.Probennummer = d.Probennummer;"
    
    # Fetch data from the database
    data <- dbGetQuery(con, sprintf(query.dxi.val, #input$
                                      method))
    # convert data to numeric
    data[,1] <- as.numeric(data[,1])
    data[,2] <- as.numeric(data[,2])
    
    # omit rows with NA values
    data <- na.omit(data)
    
    # Store the data in the reactive value
    validation.data(data)
    
    # Write data to an Excel file
    filepath <- "vdata _DxI.xlsx"
    wb <- loadWorkbook(filepath)
    sheet_name <- "DATA"
    
    # Clear data from the sheet
    if (sheet_name %in% names(wb)) {
      removeWorksheet(wb, sheet_name)
    }
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = validation.data(), startRow = 1, startCol = 1)
    saveWorkbook(wb, filepath, overwrite = TRUE)
  })
  
  # Generate table with number of samples
  output$nSamples <- renderDT({
    req(validation.data())
    data <- validation.data()
    sample.count <- data.frame(
      device = names(data[,1:2]),
      n = c(length(data[,1]),
            length(data[,2]))
    )
    datatable(sample.count)
  })
  
  
  
  # Generate report
  observeEvent(input$generate.report, {
    req(validation.data())  # Ensure that validation.data is not NULL before rendering
    rmarkdown::render("DxI_autoValOffcDwnWrd.Rmd", 
                      output_format = "all")
  })
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)
