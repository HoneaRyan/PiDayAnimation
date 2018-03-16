library(plotrix)

circ_dist <- function(x, y) {
  dist <- sqrt(x^2 + y^2)
  if (dist <= 1) return(1)
  else return(0)
}

rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',x,'pi.png',sep=''))
  }
  if (x < 100 && x >= 10) {
    return(name <- paste('00',x,'pi.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', x,'pi.png', sep=''))
  }
}

x <- c()
y <- c()
circ <- 0
props <- c()
for (i in 1:500) {
  x[i] <- runif(1,-1,1)
  y[i] <- runif(1,-1,1)
  circ <- circ + circ_dist(x[i], y[i])
  props[i] <- format(round(4*circ/i, 4), nsmall = 4)
  text <- paste("Pi Approximation =", props[i])
  png(filename = rename(i))
  plot(x,y, pch = 20, asp = 1, axes = F, ylab = "", xlab = "",
       xlim = c(-1, 1), ylim = c(-1, 1),
       main = text)
  draw.circle(0,0,1, border = "black", lwd = 3)
  dev.off()
}

my_command <- 'convert *.png -delay 3 -loop 0 animation.gif'
system(my_command)