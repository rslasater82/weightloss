library(deSolve)
library(rootSolve)
library(ggplot2)

Gender = "F"    # gender
W0 = 80       # baseline weight

A = 44        # age
H = 150.2     # height (?)
Tf = 2*168      # duration of diet

if(Gender == "M"){
  a1 = 293      # L-K parameter a1
  y1 = 5.92     # L-K parameter y1
  p = 0.4330     # L-K parameter p
  T0 = -0.0971*W0^2 + 40.853*W0 + 323.59    # TEE = ?????
} else{
  a1 = 248      # L-K parameter a1
  y1 = 5.09     # L-K parameter y1
  p = 0.4356     # L-K parameter p
  T0 = 0.0278*W0^2 + 9.2893*W0 + 1528.9    # TEE = ?????
}

a = 0.02       # adaption param: a-"percent that RMR has lowered than expected"
r = 0.67       # adaption param: r-"percent that NEAT has changed rel. to change in TEE"

RMR0 <- a1*(W0^(p))-y1*A           # RMR = ?????

kcalTarget <- T0 - 0.25*T0 
Cr <- T0-kcalTarget  # cal reduced
NEAT0 <- 0.326*T0
omega <- 0.075      # percent of total caloric input contribution to DIT
DIT0 <- omega*T0               # DIT = ?????

PA0 <- T0-DIT0-RMR0-NEAT0
PA0 <- ifelse(PA0 > 0, PA0, 0)

cl <- 1020                     # energy in 1kg of lean muscle
cf <- 9500                     # energy in 1kg of fat

if (Gender == "M"){
  polyc <- c(-W0-71.73349 - 0.38273e-1*A + 0.6555023*H,    # 1's terms
             1.0 + 3.5907722 - 0.2296e-2*A - 0.13308e-1*H, # x terms 
             0.332e-4*A - 0.7195e-1 + 0.2721e-3*H,         # x^2 terms 
             0.6841e-3 - 0.187e-5*H,                       # x^3 terms
             - 0.162e-5 )                                  # x^4 terms
  res <- polyroot(polyc)
  y =  Re(res[1])   # get smallest real root
}else{
  polyc <- c(-W0 - 72.055453 + 0.6555023*H - 0.38273e-1*A,   # 1's terms
             1.0 + 2.4837412 - 0.2296e-2*A - 0.13308e-1*H,   # x terms 
             -0.390627e-1 +0.332e-4*A +0.2721e-3*H,          # x^2 terms
             0.2291e-3 -0.187e-5*H,                          # x^3 terms
             3.5*10^(-7) )                                   # x^4 terms
  
  res <- polyroot(polyc)
  y <-  Re(res[1])  # get smallest real root
}

F0 <- y
FFM0 <- W0 - F0

F1 <- function(z) (- FFM0 + 10.4*log(F0/z))

C <- uniroot(F1,c(0,100))$root

CC <- NEAT0 - 2*(RMR0+PA0+DIT0)   # "Const" in Maple code


############## ODE SOLVER ###############

NI1 <- T0 - Cr
DIT1 <- omega * NI1

parameters <- c(cl,
                cf,
                C,
                CC,
                T0,
                p,
                PA0,
                W0,
                DIT1,
                NI1)

state <- c(f = F0)   # initial fat mass

FF <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    mff <- 10.4 * log(f / C)
    RMR <- a1 * (mff + f)^p - y1*(A + t/365)
    PA <- PA0 / W0 * (mff + f)
    G1 <- 2*((1-a)*RMR + DIT1 + PA) + CC
    
    # rate of change
    if (G1 <= 0){
      df <- (NI1 - RMR - DIT1 - PA) / (cl * 10.4 / f + cf)
    } else {
      df <- (NI1 - RMR - DIT1 - PA - G1) / (cl * 10.4 / f + cf) 
    }
    
    #return the rate of change
    list(c(df))
  }) 
}


times <- seq(0,Tf, by = 0.01)

out <- ode(y = state,
           times = times,
           func = FF,
           parms = parameters)
           # method = "ode45")
head(out)

out_df <- as.data.frame(out)
out_df$weight <- out_df$f + FFM0

# plot(out_df$time, FFM0 +out_df$f, pch='.')


ggplot(out_df, aes(x=time, y=weight)) + 
  geom_point() +
  ylim(70,80) +
  xlab("Day of Weight Loss") + 
  ylab("kg")

