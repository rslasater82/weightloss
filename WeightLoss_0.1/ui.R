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
            numericInput("weight", "Starting Weight:", 200),
            actionButton("execute", "Calculate")
        ),
        mainPanel(plotOutput("guessPlot"),
                  tableOutput("fullTable"))
    ),
    tabPanel("Table", fluid = TRUE,
         sidebarPanel(
             numericInput("week", "Week: ", 0),
             numericInput("actual", "Actual Weight: ", 200),
             fileInput("load", "Load csv", accept = ".csv"),
             actionButton("userfile", "Use File"),
             actionButton("addrow", "Add Entry"),
             actionButton("merge", "Add to Plot"),
             downloadButton("download", lable = "Export Table")
         ),
         mainPanel(tableOutput("guessTable"))
        )
    )
)

