library(shiny)
library(plotly)
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
            radioButtons("unit",
                         label = "Units: ",
                         inline=FALSE,
                         choiceNames =  c("Imperial (lb/in)", "Metric (kg/cm)"),
                         choiceValues = c("Imperial", "Metric"),
                         selected = "Metric"),
            numericInput("weight", 
                         label = "Starting Weight: ", 
                         value = 0),
            numericInput("age", 
                         label = "Age:",
                         value = 0),
            numericInput("height",
                         label = "Height: ",
                         value = 0),
            numericInput("Tf", 
                         label = "Duration of weight change (days): ",
                         value = 336),
            numericInput("CalRed",
                         label = "Change in daily calorie intake: ",
                         value = 0),
            actionButton("execute", "Calculate")
        ),
        mainPanel(plotlyOutput("guessPlot"),
                  textOutput("attribution")
                  #,#%>% withSpinner(color="#0dc5c1"),
                  #tableOutput("fullTable") 
                  )
    )#,
    #tabPanel("Table", fluid = TRUE,
    #     sidebarPanel(
    #         numericInput("week", "Week: ", 0),
    #         numericInput("actual", "Actual Weight: ", 200),
    #         actionButton("addrow", "Add Entry"),
    #         actionButton("merge", "Add to Plot"),
    #         downloadButton("download", lable = "Export Table"),
    #         fileInput("load", "Load csv", accept = ".csv"),
    #         actionButton("userfile", "Use File")
    #     ),
    #     mainPanel(tableOutput("guessTable"))
    #    ),
    #tabPanel("TestValues", fluid = TRUE,
    #         mainPanel(textOutput("test")))
    )
)


