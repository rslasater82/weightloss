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
    
    observeEvent(input$execute, { #Starts the calculations and generates the plots and table
        tout<-seq(0, input$tmax, by = .01)
        guess_pars<-c(A = input$A, B = input$B, C = input$C)
        #     alpha<-input$alpha
        guess <- as.data.frame(solveLorenz(guess_pars, tout))
        myData(guess)
        myPlot(ggplot(as.data.frame(guess)) + geom_path(aes(time, Y), alpha=input$alpha, lwd=.3))
    })
    
    uitable <- reactiveVal() #To set the uitable
    
    observeEvent(input$userfile, { #Allows for user to upload a file, must be in the correct format
        file <- input$load
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        loadedfile <- read_csv(file$datapath)
        uitable(loadedfile)
    })
    
    observeEvent(input$addrow, { #This will addrows to generate a data table to then plot
        table1 = uitable()
        table2 = data.frame("time" = input$week, 
                            "Actual Wt" = input$actual, 
                            "Pct Lost" =  (input$weight-input$actual)/input$weight)
        inputtable = rbind(table1,table2)
        uitable(inputtable)
    })
    observeEvent(input$merge, { #Button to merge the user input with ODE output
        uitable <- uitable()
        ODEtable <- myData()
        oldplot <- myPlot()
        newplot <- oldplot + geom_line(data = uitable, aes(x=time, y=`Actual.Wt`))
        myPlot(newplot)
        fulltable <- merge(ODEtable, uitable, all.x=T)
        myData(fulltable)
    })   
    output$download <- downloadHandler( #download datafile, not currently working right
            filename = function() {
            paste("data", Sys.Date(), ".csv", sep="")
            },
        content= function(file) {
            write_csv(uitable(), file)
        }
    )
    output$guessPlot <- renderPlot({
        myPlot()
        
    })
    output$guessTable = renderTable({
        uitable()
    })
    output$fullTable = renderTable({
        myData()
    })
})