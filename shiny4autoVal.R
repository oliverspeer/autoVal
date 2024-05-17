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
             fluidRow(
               column(9, DTOutput("summary"))
               ),
             selectInput("method", "Wähle die Methode", choices = NULL),
             # fluidRow(
             #   column(3, DTOutput("nSamples"))
             #   ),
             actionButton("generate.report", "Erstelle Validationsbericht")
    )
  )
)

# Define server logic -----------------------------------------------------------
server <- function(input, output, session) {
  
  sum.query <- "SELECT
                  TestName AS DxI9000,
                  COUNT(*) AS n_Doppelmessungen,
                  (SELECT DISTINCT DoseUnit
                   FROM DxIvalData AS sub
                   WHERE sub.TestName = main.TestName AND sub.DoseUnit IS NOT NULL
                   LIMIT 1) AS Einheit_9000,
                  (SELECT DISTINCT m.EINHEIT
                      FROM MeasurementData AS md
                      JOIN MethodData AS m ON md.Methode = m.Methode
                      JOIN TranslationData AS t ON md.Methode = t.Methode
                      JOIN DxIvalData AS d ON t.TestOrderCode = d.TestOrderCode
                      WHERE d.TestName = main.TestName AND main.DoseUnit IS NOT NULL
                      LIMIT 1) AS Einheit_800
                FROM 
                  DxIvalData AS main
                WHERE
                  Probennummer IS NOT NULL
                GROUP BY
                  TestName;"
  sum.dat <- dbGetQuery(con, sum.query)
  
  
  prc.query <- "SELECT 
                  TestName AS DxI9000,
                  COUNT(*) AS n_QC_Messungen
                FROM 
                  DxIvalData
                WHERE
                  Probennummer IS NULL 
                  AND SampleID LIKE '%QC%' 
                  AND DoseResult <> 'No result'
                GROUP BY
                  TestName;"
  prc.dat <- dbGetQuery(con, prc.query)
  
  unit.query <- "SELECT
                  TestName AS Analyt,
                  -- COUNT(*) AS n_Doppelmessungen,
                      (SELECT DISTINCT m.EINHEIT
                      FROM MeasurementData AS md
                      JOIN MethodData AS m ON md.Methode = m.Methode
                      JOIN TranslationData AS t ON md.Methode = t.Methode
                      JOIN DxIvalData AS d ON t.TestOrderCode = d.TestOrderCode
                      WHERE d.TestName = main.TestName AND m.EINHEIT IS NOT NULL
                      LIMIT 1) AS Einheit_800
                FROM 
                  DxIvalData AS main
                WHERE
                  Probennummer IS NOT NULL
                GROUP BY
                  TestName;"
  unit.dat <- dbGetQuery(con, unit.query)
  
  output$summary <- renderDT({
    sum.dat <- merge(sum.dat, prc.dat, by = "DxI9000", all = TRUE)
    sum.dat[is.na(sum.dat)] <- 0
    datatable(sum.dat, options = list(pageLength = 50))
  })
  
  # Reactive value to store the method corresponding to the selected test name
  selectedMethod <- reactiveVal()
  
  # Update method choices based on the database
  updateSelectInput(session, "method",
                    choices = dbGetQuery(con, "SELECT DISTINCT TestName FROM DxIvalData 
                                         WHERE SampleID IS NOT NULL 
                                         ORDER BY TestName ASC"))
  
  
  
  # Observe changes in the selected TestName and update the Methode accordingly
  observe({
    con <- dbConnect(SQLite(), dbname = "C:/R_local/labStat/ClinicalChemistry_2.db")
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
    req(selectedMethod())  # Ensure that selectedMethod is not NULL before rendering
    rmarkdown::render("DxI_autoValOffcDwnWrd.Rmd", 
                      output_format = "all",
                      params = list(method = selectedMethod())
                      )
  })
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)
