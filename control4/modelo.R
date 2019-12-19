library(ggplot2)
library(reshape2)
library(dplyr)
library(glmnet)
library(MASS)
library(knitr)

preprocesamiento <- function(datos){
  #Precio en miles de dlls
  datos$msrp <- datos$msrp/1000
  #Cambiar mpg a km por litro
  datos$kmpl <- datos$mpg*0.42514
  #Cambiar mpge a km por litro equivalente
  datos$kmple <- datos$mpgmpge*0.42514
  datos<- datos %>% mutate(carclass_modif= ifelse(as.character(datos$carclass) == 'L', as.character(datos$carclass), 'O'))
  datos$carclass_modif <- as.factor(datos$carclass_modif)
  datos
}
datosFit <- read.csv("~/aplicada2/control4/datosFit.csv")
summary(datosFit)
datos <- preprocesamiento(datosFit)
modelos <- list()

#Analisis exploratorio
summary(datos)
vars_numericas <- c("msrp", "year", "accelrate", "kmpl","kmple")
cormat <- round(cor(datos[,vars_numericas]),2)
melted_cormat <- melt(cormat)
plot(datosFit$year, datosFit$msrp)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) + geom_tile() + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank())


# El precio de un vehiculo es proporcional a su tasa de aceleración e inversamente a kmpl y kmple
pairs(datos[,vars_numericas])

# Parece que solo la categoria L tiene valores distintos de msrp, las demas pueden quedar en uno solo
plot(datos$carclass, datos$msrp)

#Classes : two-seaters (TS), compact (C), midsize (M), large (L),
#sport utility vehicle (SUV), minivan (MV), and pickup truck(PT)
#Los vehiculos mas caros son los L, los suv tienen la mayor varianza, 
#los midzise tienen muchos outliers
# Los two-seaters sólo contienen dos modelos (Insight y Cr-z) para varios años

analisis_modelo <- function(m){
  print(summary(m))
  print(anova(m))
  plot(m, which=1)
}

#Modelo con todos los regresores preprocesados ( + año )
m_todos_year <- lm(msrp ~ year + accelrate + carclass + kmpl + kmple, data = datos)
modelos[[1]] <-  m_todos_year
analisis_modelo(m_todos_year)


#Modelo con todos los regresores preprocesados ( sin año)
m_todos <- lm(msrp ~accelrate + carclass + kmpl + kmple, data = datos)
modelos[[2]] <- m_todos
analisis_modelo(m_todos)

#Modelo con todos los regresores preprocesados kmple antes( sin año)
# m_todos <- lm(msrp ~accelrate + carclass + kmple + kmpl, data = datos)
# modelos[[2]] <- m_todos
# analisis_modelo(m_todos)

#Modelo con carclass_modif ('L', 'O') y solo variables significativas
m_carclass_modif <- lm(msrp ~ accelrate + carclass_modif + kmpl, data=datos)
modelos[[3]] <- m_carclass_modif
analisis_modelo(m_carclass_modif)
boxcox(m_carclass_modif) #Contiene lambda=0 en el intervalo de confianza

#Transf log
m_log <- lm(log(msrp) ~ accelrate + carclass_modif + kmpl, data = datos)
modelos[[4]] <- m_log
analisis_modelo(m_log)

#Interacciones
m_inter <- lm(msrp ~ accelrate*carclass_modif + accelrate*kmpl + carclass_modif*kmpl, data = datos)
modelos[[5]] <- m_inter
analisis_modelo(m_inter)
boxcox.list <- boxcox(m_inter, objective.name='Log-Likelihood') 
boxcox.list 

#Interacciones + año
m_inter_year <- lm(msrp ~ accelrate*carclass_modif + accelrate*kmpl + carclass_modif*kmpl +accelrate*year + carclass_modif*year + kmpl*year, data = datos)
modelos[[6]] <- m_inter_year
analisis_modelo(m_inter_year)
boxcox.list <- boxcox(m_inter, objective.name='Log-Likelihood') 
boxcox.list 

#Ridge regression NO FUNCIONA
# m_ridge <- cv.glmnet(datos_modif[,c("accelrate", "carclass_modif", "kmpl", "kmple")], datos_modif$msrp, alpha = 0, type.measure = 'mse')
# m_ridge <- cv.glmnet(msrp ~ accelrate, data=datos[,c("accelrate")], family="gaussian")




#Comparando la nueva base de datos notamos que hay modelos que no se han considerado en los 
#datos para el ajuste

#ANALISIS DE RESIDULES ESTUDENTIZADOS Y PUNTOS DE INFLUENCIA
#

#Predicción
datosFin <- read.csv("~/aplicada2/control4/datosFin.csv")
datos_pred <- preprocesamiento(datosFin)
y <- datos_pred$msrp
error_promedio <- function(y_hat){
  sum(abs(y_hat - y))/length(y)
}
calc_error <- function(m){
  pred <- predict(m, newdata = datos_pred)
  error_promedio(pred)
}

pred_log <- predict(m_log, newdata = datos_pred)
err_log <- error_promedio(exp(pred_log))
pred_inter <- predict(m_inter, newdata = datos_pred)
err_inter <- error_promedio(pred_inter)
err_log
sapply(modelos, calc_error)

#Graficas
#Modelo con todas las variables (sin año) antes de colapsar carclass
s1 <- summary(m_todos)
#Coeficientes 
kable(s1$coefficients, caption = "Coeficientes modelo 1")
#Anova
kable(anova(m_todos), caption="Tabla ANOVA modelo 1")

#Modelo 2
s2 <- summary(m_carclass_modif)
kable(s2$coefficients, caption="Coeficientes modelo 2")
plot(m_carclass_modif)
kable(anova(m_todos), caption="Tabla ANOVA modelo 2")

errores <- data.frame("Modelo con transformacion logaritmo" = err_log, "Modelo con interacciones" = err_inter)
kable(errores, caption = "Diferencia en valor absoluto promedio")

#Modelo 3
s3 <- summary(m_log)
kable(s3$coefficients, caption="Coeficientes modelo 3")
plot(m_log)
kable(anova(m_log), caption="Tabla ANOVA modelo 3")

#Modelo 4
s4 <- summary(m_inter)
kable(s4$coefficients, caption="Coeficientes modelo 4")
plot(m_inter)
kable(anova(m_inter), caption="Tabla ANOVA modelo 4")


