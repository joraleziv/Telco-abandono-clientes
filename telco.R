

#Crear modelo de clasificación que permita predecir el abandono del servicio por parte del cliente

#creamos una copia de los datos --> "datos"
datos <- datos_teleco_1_

#análisis preliminar de los datos
str(datos)
summary(datos)

#observaciones:
#tenemos variables categóricas o ficticias, ya que toman un valor de 0 y 1
#NO existen celdas vacías
#la primera columna "...1" tiene un max de 7043 cuando las observaciones son 7032 !!!!!!!!!
#CREO que debemos ELIMINAR la primera columna o ignorarla



# 1) PARTICIONAR CONJUNTO DATOS EN ENTRENAMIENTO Y TEST PROPORCION 80/20 (ENTRENAMIENTO Y TEST)

dim(datos)
#tenemos 7032 filas (registros de clientes) y 23 columnas

#procedemos a seleccionar solo el 80% de las observaciones para realizar el "ENTRENAMIENTO"
#dejando el 20% de las observaciones (las más recientes) para la realización del "TEST" posteriormente

0.80 * 7032
#el 80% de las observaciones son 5625.6 --> 5626 observaciones utilizaremos para el "ENTRENAMIENTO"
#por lo cual el resto de observaciones (1406) se reservan para el "TEST"

#eliminamos las últimas 1406 filas de "datos" y creamos "datos80"


# utilizaremos la Librería --> DPLYR  
# usaremos el comando FILTER para extraer o eliminar filas que nos interesan

install.packages("dplyr")
library(dplyr)

datos80 <- filter(datos,...1<5627) # ERROR porque coge la primera columna NO las observaciones

#NO SE HACERLO, PUES PILLO TODOS LOS DATOS



#CREAR 2 MODELOS QUE PERMITAN PREDECIR EL ABANDONO DE LOS CLIENTES

# UN MODELO LOGIT (regresión logística) Y UN MODELO DE ARBOL DE DECISION
#usando las  variables: Contrato, Factura digital, Servicio Internet, 
#Soporte técnico, Copia de Seguridad Online, Televisión, Meses de alta en el servicio

### MODELO LOGIT  o REGRESION LOGISTICA ### --> comando GLM

#variable DEPENDIENTE --> "ABANDONO" (cumple la condición de ser una variable categórica: "Yes" y "No")

#primero variable dependiente y después la variable independiente
#IMPORTANTE --> la variable dependiente "abandono" siempre tiene que tener valores 0 y 1 
#debemos advertir a R que la variable es categórica o re nombrar los valores
#¿como? --> usando el comando --> as.factor --------------------> AS.FACTOR 
#lo metemos en la  regresión logística antes de "Abandono"

modelologit_teleco <- glm(as.factor(datos$Abandono)~datos$Contrato+
                     datos$Factura_digital+
                     datos$Servicio_Internet+
                     datos$Soporte_tecnico+
                     datos$CopiaSeguridad_Online+
                     datos$Television_carta+
                     datos$Meses_alta, 
                     family = "binomial")

summary(modelologit_teleco)

#INTERPRETACION MODELO LOGIT

#en primer lugar, analizamos como siempre la SIGNIFICANCIA de los parámetros
#para ello nos fijamos en el pvalue o Pr(>|z|)
#PERO una regresión logística --> NO se puede interpretar directamente la magnitud del coeficiente
#SOLO se puede INTERPRETAR el SIGNO (negativo o positivo)
#para interpretar la magnitud es necesario --> aplicar la formula --> 1-exp(coeficiente)

#una vez tengamos el modelo hay que VALIDAR, es decir, ver si funciona o acierta
#¿cómo? --> analizando sus aciertos --> mediante la MATRIZ DE CONFUSION

### MATRIZ DE CONFUSION DEL MODELO LOGIT ###

# la MATRIZ DE CONFUSION nos dice si acertamos o no
#compara los datos reales con la predicción

#datos reales
table(datos$Abandono)

#datos predicción
prediccion_teleco <- predict(modelologit_teleco, type = 'response')
View(prediccion_teleco)

#recordemos que nuestra variable dependiente (la variable real) Abandono solo toma 2 valores
#pero nuestra predicción solo toma valores ENTRE 0 y 1
#por lo cual tenemos que cambiar los valores de la predicción (entre 0 y 1) a valores de 0 o 1
#aquellos valores que se acerquen más valores a 0 serán 0 y los que se acerquen más a 1 serán 1
#es decir, tenemos que codificar nuestra predicción a 0 y 1

#¿como lo hacemos?

prediccion_teleco_cod <- ifelse(prediccion_teleco>0.5,1,0)
View(prediccion_teleco_cod)

#observamos que ya solo tenemos valores 0 y 1 en los resultados de la predicción (codificada)

#ahora SI podemos comparar los datos reales y los datos predichos (predicción codificada)
#ya podemos calcular la MATRIZ DE CONFUSION


####### MATRIZ DE CONFUSION ##########

table(datos$Abandono,prediccion_teleco_cod)

#ACIERTOS = diagonal matriz = 4636 + 935 = 5571
#FALLOS = 537 + 934 = 1471
#TOTAL = 5571 + 1471 = 7042  -----> OJO Recodar el problema con las observaciones 7032 y 7043

#métrica de exactitud --> porcentajes de aciertos en total = aciertos / total
# 5571 / 7042 = 0.7911105 --> el 79,12% de las veces que el modelo logit acierta


#### MODELOS DE ARBOL DE DECISION ####

#el árbol de decisión es una metodología alternativa
#para calcular el árbol de decisión necesitamos instalar 2 librerías
#RPART y RPART.PLOR

install.packages("rpart")
install.packages("rpart.plot")

#llamamos librerías
library(rpart)
library(rpart.plot)

modeloarbol_teleco <- rpart(as.factor(Abandono)~Contrato+
                                                Factura_digital+
                                                Servicio_Internet+
                                                Soporte_tecnico+
                                                CopiaSeguridad_Online+
                                                Television_carta+
                                                Meses_alta,
                                                data = datos,
                                                method = "class")

#IMPORTANTE --> para analizar los resultados del árbol es mejor dibujarlo
#para ellos utilizamos rpart.plot

rpart.plot(modeloarbol_teleco)

#podemos ver el árbol en el cuadro-abajo de R en ventana "PLOT"

summary(modeloarbol_teleco)

#interpretación TOP variables más relevantes en el modelo "árbol de decisión"

#Variable importance:
#1)contrato
#2)meses_alta
#3)soporte_tecnico

#ahora procedemos a validar el modelo --> con la matriz de confusión

### MATRIZ DE CONFUSION DEL ARBOL DE DECISION ###

# la MATRIZ DE CONFUSION nos dice si acertamos o no
#compara los datos reales con la predicción

#datos reales
table(datos$Abandono)

#datos predicción (predicción 2 --> para ARBOL DECISION) y en type ="class"

prediccion_teleco2 <- predict(modeloarbol_teleco, type = "class")

table(datos$Abandono,prediccion_teleco2) 

#ya tenemos la matriz de confusión
#OTRA FORMA DE CALCULAR LA MATRIZ DE CONFUSION:

#instalamos libreria caret para calcular matriz de confusion 

install.packages("caret")
library(caret)

#matriz de confusion
confusionMatrix(as.factor(datos$Abandono),as.factor(prediccion_teleco2))

#Accuracy = 0.7908 =metrica exactitud = 79,08% de las veces acierta el modelo de Arbol de Decision


################################################################ 2)

# MODELO LOGIT 79,12% > MODELO ARBOL DECISION 79,08%


################################################################ 3)
# Cómo se podría obtener una clasificación de la importancia de 
#las variables en nuestro modelo?

#MODELO LOGIT
summary(modelologit_teleco)

#datos$ContratoOne year                         3.91e-14 ***
#datos$ContratoTwo year                          < 2e-16 ***
#datos$Factura_digitalYes                       8.73e-09 ***
#datos$Servicio_InternetFiber optic              < 2e-16 ***
#datos$Servicio_InternetNo                      9.86e-16 ***
 

#MODELO ARBOL DE DECISION
summary(modeloarbol_teleco)

#Variable importance

#Contrato            Meses_alta       Soporte_tecnico 
# 29                    22                    14 

#Servicio_Internet CopiaSeguridad_Online      Television_carta 
# 13                    11                     9 

#Factura_digital 
# 2


