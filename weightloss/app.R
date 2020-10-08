library(shiny)
library(tidyverse)

# Define UI for application that generates a trajectory for weight loss based on user inputs
ui <- fluidPage(
    
    # Application title
    titlePanel("Weight Loss Trajectory"),
    
    # Sidebar with a slider input for height and weight, radio button for sex 
    sidebarLayout(
        sidebarPanel(
            sliderInput("age",
                        "Age:",
                        min = 0,
                        max = 100,
                        value = 30),
            sliderInput("weight",
                        "Enter your weight in kg",
                        min = 0,
                        max = 400,
                        value = 150),
            sliderInput("height",
                        "Enter your height in cms",
                        min = 0,
                        max = 200,
                        value = 65),
            radioButtons("sex", 
                         "Select your sex:", 
                         choices = c("Male", 
                                     "Female")
            ),
            
        ),
        
        # Displays the desired plot
        mainPanel(
            textOutput("test"),
            plotOutput("trajectoryPlot")
        )
    )
)

# Define server logic required to draw the plot
server <- function(input, output) {
    
    #Create reactive variables to be able to be accesssed by all objects
    
    #TEE0:=-.0971*W0^2+40.853*W0+323.59 for males; 
    #TEE0:=.0278*W0^2+9.2893*W0+1528.90 for females
    TEE0 <- reactive(if_else(input$sex == "Male",
                             -.0971*(as.numeric(input$weight)^2)+40.853*(as.numeric(input$weight))+323.59,
                             .0278*(as.numeric(input$weight)^2) + 9.2893*(as.numeric(input$weight))+1528.9)
    )
    kCalTarget <- reactive(TEE0()-200)
    
    RMR0 <- reactive(if_else(input$sex == "Male",
                             293*(as.numeric(input$weight)^0.4330)-5.92*as.numeric(input$age),
                             248*(as.numeric(input$weight)^0.4356)-5.09*as.numeric(input$age)))
    
    
    #Text to test variable definitions
    output$test <- renderText(paste("TEE0 =",TEE0(), 
                                    "KCalTarget = ", kCalTarget(),
                                    "RMR0 = ", RMR0()
    ))
    #output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    #    x    <- faithful[, 2]
    #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
