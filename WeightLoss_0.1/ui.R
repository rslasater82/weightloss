library(shiny)
#library(shinycssloaders)

# Define UI
shinyUI(navbarPage("Weight Loss DE Plotter",
    
    # Application title
  
    
    # Sidebar with controls
    
    tabPanel("Plot", fluid = TRUE,
        sidebarPanel(    
            radioButtons("sex", 
                               label = "Sex: ",
                               inline = TRUE,
                               choiceNames = c("Male", "Female"),
                               choiceValues = c("M", "F"),
                               selected = "F"),
            numericInput("weight", 
                         label = "Starting Weight (Kg): ", 
                         value = 80),
            numericInput("age", 
                         label = "Age:",
                         value = 44),
            numericInput("height",
                         label = "Height (cm): ",
                         value = 150.2),
            numericInput("Tf", 
                         label = "Number of days to diet: ",
                         value = 336),
            sliderInput("CalRed",
                         label = "Percent Calories to reduce from Total Energy Expenditure: ",
                         value = .25,
                         min = 0,
                         max = 1
                         ),
            actionButton("execute", "Calculate")
        ),
        mainPanel(plotlyOutput("guessPlot"),#%>% withSpinner(color="#0dc5c1"),
                  tableOutput("fullTable") 
                  )
    ),
    tabPanel("Table", fluid = TRUE,
         sidebarPanel(
             numericInput("week", "Week: ", 0),
             numericInput("actual", "Actual Weight: ", 200),
             actionButton("addrow", "Add Entry"),
             actionButton("merge", "Add to Plot"),
             downloadButton("download", lable = "Export Table"),
             fileInput("load", "Load csv", accept = ".csv"),
             actionButton("userfile", "Use File")
         ),
         mainPanel(tableOutput("guessTable"))
        ),
    tabPanel("TestValues", fluid = TRUE,
             mainPanel(textOutput("test")))
    )
)


