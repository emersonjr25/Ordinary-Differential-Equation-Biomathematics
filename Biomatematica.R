library(deSolve)
library(ggplot2)
library(reshape2)


#ESTADOS & PARAMETROS
parameters <- c(an = 0.3, bn = 0.3, dn = 0.1,
                ai = 0.05, bi = 0.05, di = 0.1,
                alphaI = 0.2, beta = 0.02,
                br = 60, dr = 0.1)

state <- c(N = 50, I = 1, R = 50)

time <- seq(0, 100, by = 0.1)

pig <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    dN  = an * bn * R * N - alphaI * N * I - dn * N
    dI = ai * bi * R * I + beta * alphaI * N * I - di * I
    dR = br - R * dr - ai * I * R - an * R * N
    return(list(c(dN, dI, dR)))
  })
}

  
out <- ode(y = state, times = time, func = pig, parms = parameters)


out.df = as.data.frame(out) # exigido pelo ggplot

out.m = melt(out.df, id.vars='time') 

ggplot(out.m, aes(time, value, color = variable)) + geom_point()
ggplot(out.m, aes(time, value)) + geom_point() + facet_wrap(out.m$variable)


#ggplot(data = out.df, aes(x = N, I, color = time)) + geom_point()

