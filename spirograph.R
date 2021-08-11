library(ggplot2)
library(tibble)

spiro <- function(n1,n2,n3) {
  t <- seq(0,1,length.out=1000)
  z <- exp(1i*2*pi*n1*t) + exp(1i*2*pi*n2*t) + exp(1i*2*pi*n3*t)
  result <- tibble(x=Re(z),y=Im(z))
  return (result)
}

n1 <- floor(runif(1, min=-10, max=20))
n2 <- floor(runif(1, min=-10, max=20))
n3 <- floor(runif(1, min=-10, max=20))

result <- spiro(n1, n2, n3)

ggplot(data=result,aes(x=x,y=y)) +
    geom_path() +
    xlab("Real(z)") +
    ylab("Imag(z)")

ggsave("spiro.png")

