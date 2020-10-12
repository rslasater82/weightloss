library(shiny)
library(deSolve) 
library(ggplot2)

# Define server logic required to plot various variables
shinyServer(function(input, output) {
    
    myPlot = reactiveVal()
    myData = reactiveVal()
    
    solveLorenz <- function(pars, times=tout) {
        derivs <- function(t, state, pars) { # returns rate of change
            with(as.list(c(state, pars)), {
                dX <- A*X + Y*Z
                dY <- B * (Y-Z)
                dZ <- -X*Y +C*Y -Z
                return(list(c(dX, dY, dZ)))
            }
            )
        }
        state <- c(X = 34.848292, Y = 14.331027983, Z = 14.327178391) #  originally X = 1, Y = 1, Z = 1
        ## ode solves the model by integration...
        return(ode(y = state, times = times, func = derivs, parms = pars))
    }
    
    observeEvent(input$button, { #Starts the calculations and generates the plots and table
        tout<-seq(0, input$tmax, by = .01)
        guess_pars<-c(A = input$A, B = input$B, C = input$C)
        #     alpha<-input$alpha
        guess <- as.data.frame(solveLorenz(guess_pars, tout))
        myData(guess)
        myPlot(ggplot(as.data.frame(guess)) + geom_path(aes(time, Y), alpha=input$alpha, lwd=.3))
    })
    
    uitable <- reactiveVal() #To set the uitable
    
    observeEvent(input$input, { #This will generate the user input values, need action button to overlay the plot on the ODE plot by matching by time
        table1 = uitable()
        table2 = data.frame("time" = input$week, "Actual Wt" = input$actual)
        inputtable = rbind(table1,table2)
        uitable(inputtable)
        #table3 = myData()
        #guess = merge(table3, table2, all.x = T)
        #myData(guess)
    })
    output$guessPlot <- renderPlot({
        myPlot()
        
    })
    output$guessTable = renderTable({
        uitable()
    })
})