library(shiny)
library(deSolve) 
library(ggplot2)


# Define server logic required to plot various variables
shinyServer(function(input, output) {
    
    myPlot = reactiveVal()
    myData = reactiveVal()
    
    a1 <- reactive(if_else(input$sex == "M",293,248))
    y1 <- reactive(if_else(input$sex == "M",5.92,5.09))
    p <- reactive(if_else(input$sex == "M",.4330,.4356))
    TEE0 <- reactive(if_else(input$sex == "M",
                             -.0971*(as.numeric(input$weight)^2)+40.853*(as.numeric(input$weight))+323.59,
                             .0278*(as.numeric(input$weight)^2) + 9.2893*(as.numeric(input$weight))+1528.9))
    
    RMR0 <- reactive(a1()*(input$weight^(p()))-y1()*input$age)
    
    a <- 0.02       # adaption param: a-"percent that RMR has lowered than expected"
    Cr <- reactive(input$CalRed*TEE0())   #Calories required with user input for deficit, other model was reduced by 25%
    
    NEAT0 <- reactive(0.326*TEE0())
    #omega <- 0.075      # percent of total caloric input contribution to DIT
    DIT0 <- reactive(.075*TEE0())
    
    PA0 <- reactive(if_else(TEE0()-DIT0()-RMR0()-NEAT0() > 0, 
                            TEE0()-DIT0()-RMR0()-NEAT0(),
                            0))
    
    
    polyc <- reactive(case_when(input$sex == "M" ~ 
                                    c(-input$weight -71.73349 - 0.38273e-1*input$age + 0.6555023*input$height,    # 1's terms
                                      1.0 + 3.5907722 - 0.2296e-2*input$age - 0.13308e-1*input$height, # x terms 
                                      0.332e-4*input$age - 0.7195e-1 + 0.2721e-3*input$height,         # x^2 terms 
                                      0.6841e-3 - 0.187e-5*input$height,                       # x^3 terms
                                      - 0.162e-5 ), 
                       TRUE ~ c(-input$weight - 72.055453 + 0.6555023*input$height - 0.38273e-1*input$age,   # 1's terms
                                1.0 + 2.4837412 - 0.2296e-2*input$age - 0.13308e-1*input$height,   # x terms 
                                -0.390627e-1 +0.332e-4*input$age +0.2721e-3*input$height,          # x^2 terms
                                0.2291e-3 -0.187e-5*input$height,                          # x^3 terms
                                3.5*10^(-7) )))
        
    res <- reactive(polyroot(polyc()))
    F0 <-  reactive(Re(res()[1]))

    FFM0 <- reactive(input$weight - F0())
    
    C = reactive(F0() * 1/exp(FFM0()/10.4))
    
    CC <- reactive(NEAT0() - 2*(RMR0()+PA0()+DIT0()))
    ############## ODE SOLVER ###############
    
    NI1 <- reactive(TEE0() - Cr())
    DIT1 <- reactive(.075 * NI1())
    cl <- 1020                     # energy in 1kg of lean muscle
    cf <- 9500                  # energy in 1kg of fat
       # initial fat mass   
 SolveFF <- function(pars, times=tout) { 
    derivs <- function(t, state, parameters){
        with(as.list(c(state, parameters)), {
            mff <- 10.4 * log(f / C)
            RMR <- a1 * (mff + f)^p - y1*(A + t/365)
            PA <- PA0 / W0 * (mff + f)
            G1 <- 2*((1-a)*RMR + DIT1 + PA) + CC
            
            # rate of change
            df <- if_else(G1 <= 0,
                (NI1 - RMR - DIT1 - PA) / (cl * 10.4 / f + cf),
                (NI1 - RMR - DIT1 - PA - G1) / (cl * 10.4 / f + cf)) 
            
            
            #return the rate of change
            return(list(c(df)))
        }) 
    }
 state <- c(f=isolate(F0())) 
 return(ode(y = state,
            times = times,
            func = derivs,
            parms = pars))
 }
    
    
    observeEvent(input$execute, { #Starts the calculations and generates the plots and table
        #a1 <- isolate(a1())
        #y1 <- isolate(y1())
        #p <- isolate(p())
        out_pars <- c(C = isolate(C()),
                      CC = isolate(CC()),
                      TEE0 = isolate(TEE0()),
                      p = isolate(p()),
                      PA0 = isolate(PA0()),
                      W0 = isolate(input$weight),
                      DIT1 = isolate(DIT1()),
                      NI1 = isolate(NI1()),
                      A = isolate(input$age),
                      a1 = isolate(a1()),
                      y1 = isolate(y1())
                      )
        
        tout <- (seq(0,
                  input$Tf,
                  by = 0.01))
        out <- as.data.frame(SolveFF(out_pars, tout))
        out$weight <- out$f + FFM0()
        
        myData(out)
        myPlot(ggplot(out, aes(x=time, y=weight)) + 
                   geom_point() +
                   #ylim(70,80) +
                   xlab("Day of Weight Loss") + 
                   ylab("kg"))
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
    output$test <- renderText(paste("TEE0 =",TEE0(),
                                    "a1 = ", a1(),
                                    "y1 = ", y1(),
                                    "p = ", p(),
                                    "Cr = ", Cr(),
                                    "NEAT0 = ", NEAT0(),
                                    "DIT0 = ", DIT0(),
                                    "PA0 = ", PA0(),
                                    "RMR0 = ", RMR0(),
                                    "F0 = ", F0(),
                                    "FFM0 =", FFM0(),
                                    "C =", C(),
                                    "CC =", CC(),
                                    "NI1 =", NI1(),
                                    "DIT1 =", DIT1()
    ))
    
})