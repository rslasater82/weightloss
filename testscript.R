library(deSolve)
#Script attempting to replicate Di's code before I dive back into the shiny app

Gender = F
W0 = 90

A = 44
H = 150.2
Tf = 168
a1 = 248
y1 = 5.09
p = .4356
a = .02
r = .67
RMR0 = a1*(W0^(p))-y1*A
#RMR = a1*(W^(p))-y1*(A+t/365)
TEE0 = .0278*W0^2+9.2893*W0+1528.9
kcalTarget = TEE0-200
CalRed = TEE0-kcalTarget
NEAT0 = .326*TEE0
omega = .075
DIT0 = omega*TEE0
beta = 1*omega
DIT = beta*NI
PA0 = TEE0-DIT0-RMR0-NEAT0
m = PA0/W0
PA = m*W
cl = 1020
cf = 9500
y = 44.78232545
F0 = 38.06018075
FFM0 = 41.93981925
C = 0.5792391878
FFM = 10.4*ln(F(t)/C)
NI = TEE0 - CalRed

Const = NEAT0 - 2*(RMR0+PA0+DIT0)
W = FFM+F(t)


parameters <- c(cl,
                cf,
                C)

state <- c(RMR = RMR0,
           TEE = TEE0,
           NEAT = NEAT0,
           DIT = DIT0,
           PA = PA0)

NEAT <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    #rate of change
    dX <- a*X + Y*Z
    dY <- b * (Y-Z)
    dZ <- -X*Y + c*Y - Z
    
    #return the rate of change
    list(c(dX, dY, dZ))
  }) # end with (as.list...)
}
