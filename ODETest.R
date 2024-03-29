library(deSolve)

parameters <- c(a = -8/3,
                b = -10,
                c = 28)

state <- c(X = 1,
           Y = 1,
           Z = 1)

Lorenz <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    #rate of change
    dX <- a*X + Y*Z
    dY <- b * (Y-Z)
    dZ <- -X*Y + c*Y - Z
    
    #return the rate of change
    list(c(dX, dY, dZ))
  }) # end with (as.list...)
}

times <- seq(0,100, by = 0.01)

out <- ode(y = state,
           times = times,
           func = Lorenz,
           parms = parameters)
head(out)

par(oma = c(0,0,3,0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[,"Z"], pch = ".")
mtext(outer=TRUE, side = 3, "Lorenz model", cex = 1.5)

#solve(FFM0 = 10.4*log(F0/x))
C <- function(z){
  10.4*log(F0/z)-FFM0
}
z = NULL

f <- function(x) x^2 - 4
uniroot(C, c(0,100))
