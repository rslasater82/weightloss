library(shiny)

# Define UI
shinyUI(navbarPage("Test ODE output and plotting using Lorenz",
    
    # Application title
  
    
    # Sidebar with controls
    
    tabPanel("Plot", fluid = TRUE,
        sidebarPanel(    
            numericInput("tmax", "T:", 100),
            numericInput("alpha", "alpha:", 0.3),
            numericInput("A", "A:", -8/3),
            numericInput("B", "B:", -10),
            numericInput("C", "C:", 28),
            actionButton("button", "click")
        ),
        mainPanel(plotOutput("guessPlot"))
    ),
    tabPanel("Table", fluid = TRUE,
         sidebarPanel(
             numericInput("week", "Week: ", 0),
             numericInput("actual", "Actual Weight: ", 200),
             actionButton("input", "click")
         ),
         mainPanel(tableOutput("guessTable"))
        )
    )
)

