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
             fluidRow(
               column(12, DTOutput("nSamples"))
             ),
             actionButton("generate.report", "Erstelle Validationsbericht"))
  )
)

# Define server logic---------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Update method choices based on the database
  updateSelectInput(session, "method",
                    choices = dbGetQuery(con, "SELECT DISTINCT TestName FROM DxIvalData 
                                         WHERE SampleID IS NOT NULL 
                                         ORDER BY TestName ASC"))
  

}


# Run the application--------------------------------------------------------------
shinyApp(ui = ui, server = server)
