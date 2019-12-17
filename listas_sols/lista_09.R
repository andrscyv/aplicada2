datos <- read.csv("~/aplicada2/listas_sols/lista09.dat", sep="")
attach(datos)
m <- lm( y ~ x1 + x2 + x3   , data = datos)
plot(m, which = 1)
res <- as.vector(resid(m))
res
plot(region, res)
x_barra <- seq(1, max(unique(region)))
for (i in unique(region)) {
  x_barra[i] = mean(datos[ region==i , ]$region)
}
