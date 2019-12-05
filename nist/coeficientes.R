data <- read.csv("~/aplicada2/nist/datos.txt", sep="")
attach(data)
plot(x, y)

## Regresión con polinomios 
m1 <- lm(y ~ x + I(x^2) + I(x^3)+ I(x^4)+ I(x^5)+ I(x^6)+ I(x^7)+ I(x^8)+ I(x^9)+ I(x^10))
y_hat <- predict(object = m, newdata = data)
plotPolinomio(x, y_hat, 'red')
summary(m1)

## Regresión con polinomios ortogonales
m2 <- lm(y ~ poly(x,10))
y_hat2 <- predict(object = m, newdata = data)
plotPolinomio(x, y_hat2)
summary(m2)
plotPolinomio(x, y_hat2, 'green')
## Función auxiliar para graficar el polinomio resultante
plotPolinomio <- function(x,y, color){
  p <- data.frame(x, y)
  p <- p[order(x), ]
  lines(p, col=color)
}

b <- coef(m)
b <- as.vector(b)
pol <- function(b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10){
  function(x){
    b0 + b1*x + b2*x^2 + b3*x^3 + b4*x^4 + b5*x^5 + b6*x^6 + b7*x^7 + b8*x^8 + b9*x^9## + b10*x^10
  }
}
polin <- pol(b[1],b[2],b[3],b[4],b[5],b[6],b[7],b[8],b[9],b[10],b[11])
plotPolinomio(x, polin(x))
