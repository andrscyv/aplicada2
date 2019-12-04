library(dplyr)
data <- read.csv('./aplicada2/datos_ex2')
acum <- 0
for (y_i in data$y) {
  acum <- y_i^2 + acum
}
t(data$y)%*%data$y
m <- lm( y ~ ., data=data)
beta <- as.matrix(coefficients(m))

X <- data %>% select( -y ) %>% mutate(x0 = 1)
X <- as.matrix(X[,c(5,1,2,3,4)])
Y <- as.matrix(data %>% select( y ))
n_y_barra <- (sum(Y)^2)/length(Y)
ssr <- t(beta)%*%t(X)%*%Y - n_y_barra
ssres <- t(Y)%*%Y - t(beta)%*%t(X)%*%Y
sst <- ssr + ssres

tano <- anova(m)
ssr_an <- sum(tano$`Sum Sq`)

mx1 <- lm( y ~ x1 , data = data)
y_hat <- predict( mx1, data=data)
sr <- y_hat -mean(Y)
st <- Y -mean(Y)
st <- t(st) %*% st 
st
t(Y)%*%Y - n_y_barra
sr <- t(sr) %*% sr
sr
t <- anova(mx1)
t$`Sum Sq`[1]
