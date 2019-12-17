library(ggplot2)
library(reshape2)
library(dplyr)

datosFit <- read.csv("~/aplicada2/control4/datosFit.csv")
attach(datosFit)
summary(datosFit)
datos <-datosFit[,-c(1,3,9) ]
datos$msrp <- datos$msrp/1000
#Cambiar mpg a km por litro
datos$kmpl <- datos$mpg*0.42514
#Cambiar mpge a km por litro equivalente
datos$kmple <- datos$mpgmpge*0.42514
datos <- datos[, -c(4,5)]

#Analisis exploratorio
summary(datos)
cormat <- round(cor(datos[,c(2,3,5,6)]),2)
melted_cormat <- melt(cormat)
plot(datosFit$year, datosFit$msrp)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) + geom_tile()
pairs(datos[,c(2,3,5,6)])
# El precio de un vehiculo es proporcional a su tasa de aceleración e inversamente a kmpl y kmple
boxplot(datos$msrp, datos$carclass)
#Classes : two-seaters (TS), compact (C), midsize (M), large (L),
#sport utility vehicle (SUV), minivan (MV), and pickup truck(PT)
#Los vehiculos mas caros son los L, los suv tienen la mayor varianza, 
#los midzise tienen muchos outliers
# Los two-seaters sólo contienen dos modelos (Insight y Cr-z) para varios años
 p <- ggplot( data = datos, aes(x=carclass, y = msrp) ) + geom_boxplot()
ggplot( data = datos, aes(x=vehicle, y = msrp) ) + geom_boxplot()
m <- lm(msrp ~ ., data = datos)
m2 <- lm(msrp ~ accelrate + carclass + kmple + kmpl, data = datos)
summary(m2)
m3 <- lm(msrp ~ accelrate + carclass + kmpl, data = datos)
summary(m3)

#Comparando la nueva base de datos notamos que hay modelos que no se han considerado en los 
#datos para el ajuste
