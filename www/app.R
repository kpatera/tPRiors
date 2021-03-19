## App 1: Sample usage
  shinyApp(
    ui = fluidPage(
      column(4,
             numericInput("x", "Value", 5),
             br(),
             actionButton("button", "Show")
      ),
      column(8, tableOutput("table"))
    ),
    server = function(input, output) {
      # Take an action every time button is pressed;
      # here, we just print a message to the console
      observeEvent(input$button, {
        cat("Showing", input$x, "rows\n")
      })
      # Take a reactive dependency on input$button, but
      # not on any of the stuff inside the function
      df <- eventReactive(input$button, {
        head(cars, input$x)
      })
      output$table <- renderTable({
        df()
      })
    }
  )
  
  
  
  
  
  library(shiny)
  
  ui <- fluidPage(
    tags$head(tags$script(src = "message-handler.js")),
    actionButton("do", "Click Me")
  )
  
  server <- function(input, output, session) {
    observeEvent(input$do, {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Thank you for clicking')
    })
  }
  
  shinyApp(ui, server)
  
  