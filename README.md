# Codigos_Pronostico_Semana_2

####################################################

# LECTURA DE DATOS DE PRECIPITACION Y PRONOSTICO

####################################################

rm(list=ls()) # Por si quiero limpiar el environment.
graphics.off() # Elimina configuracion de graficos previos.
setwd("/home/ezequiel.amor") # Cambiar segun donde esten los datos.

####################################################

# LECTURA DE DATOS DE PRECIPITACION

####################################################

# Cargo el archivo con los datos en una variable.

datos <- read.csv("RESISTENCIAAERO.txt",sep=";",dec=",",stringsAsFactors = FALSE)

# Aquellos espacios en blanco, caracteres sin espacio o "S/D" los reescribo como NA. 
# A los datos "S/P" y "<0.1" los reescribo como "0" (cero).
# "S/D" implica "Sin Dato", lo que quiere decir que no se sabe si hubo precipitacion 
# o no porque no se midio, mientras que "S/P" implica "Sin Precipitacion", por lo 
# que en este caso si hubo medicion pero no se registro precipitacion.

datos[datos ==" " | datos == "S/D"]     <- NA
datos[datos == "S/P" | datos == "<0.1"] <- 0

# Genero una lista para guardar los datos de cada anio y un vector con los anios del archivo.

datos_por_anio <- list()
anios <- c(1964:2022)

# Guardo en cada elemento de la lista un vector con los datos de precipitacion de cada anio.

for(i in 1:length(anios)) {
  datos_por_anio[[i]] <- c(datos[seq(19+434*(i-1),409+434*(i-1),13),1],
                           datos[seq(20+434*(i-1),410+434*(i-1),13),1],
                           datos[seq(21+434*(i-1),411+434*(i-1),13),1],
                           datos[seq(22+434*(i-1),412+434*(i-1),13),1],
                           datos[seq(23+434*(i-1),413+434*(i-1),13),1],
                           datos[seq(24+434*(i-1),414+434*(i-1),13),1],
                           datos[seq(25+434*(i-1),415+434*(i-1),13),1],
                           datos[seq(26+434*(i-1),416+434*(i-1),13),1],
                           datos[seq(27+434*(i-1),417+434*(i-1),13),1],
                           datos[seq(28+434*(i-1),418+434*(i-1),13),1],
                           datos[seq(29+434*(i-1),419+434*(i-1),13),1],
                           datos[seq(30+434*(i-1),420+434*(i-1),13),1])
}

# Elimino aquellos dias que ocupan lugar en los datos pero no tiene informacion. 
# Estos son los 29/2 cuando el anio no es bisiesto, los 30/2 que no existen y los 
# 31 de aquellos meses que no tienen.
# Ademas se reemplazan las comas de los decimales por puntos y se transforman los 
# datos a numeros para luego poder hacer calculos.

for(i in 1:length(anios)) {
  datos_por_anio[[i]] <- datos_por_anio[[i]][-which(datos_por_anio[[i]] == "")]
  datos_por_anio[[i]] <- sub(",", ".",datos_por_anio[[i]], fixed = TRUE)
  datos_por_anio[[i]] <- as.numeric(datos_por_anio[[i]])
}

# A la lista anterior se le pueden sumar mas datos que provengan de otros archivos, entonces genero
# una nueva variable para leer los datos y poder aniadirlos.

datos1 <- read.csv("RESISTENCIAAERO_2023.txt",sep=";",dec=",",stringsAsFactors = FALSE)

# Reescribo los datos como en el caso anterior.

datos1[datos1 == " " | datos1 == "S/D"]    <- NA
datos1[datos1 == "S/P" | datos1 == "<0.1"] <- 0

# Reescribo el vector de anios con aquel al que pertenecen los datos nuevos.

anios <- c(1964:2023)

# Como el archivo nuevo puede no contar con los datos de todos los meses, genero un condicional
# que me permita saber cuantos meses de datos tengo y asi extraerlos correctamente.

if(datos1[7,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(8,68,2),1])
} else if(datos1[8,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(9,99,3),1],
                                       datos1[seq(10,100,3),1])
} else if(datos1[9,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(10,130,4),1],
                                       datos1[seq(11,131,4),1],
                                       datos1[seq(12,132,4),1])
} else if(datos1[10,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(11,161,5),1],
                                       datos1[seq(12,162,5),1],
                                       datos1[seq(13,163,5),1],
                                       datos1[seq(14,164,5),1])
} else if(datos1[11,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(12,192,6),1],
                                       datos1[seq(13,193,6),1],
                                       datos1[seq(14,194,6),1],
                                       datos1[seq(15,195,6),1],
                                       datos1[seq(16,196,6),1])
} else if(datos1[12,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(13,222,7),1],
                                       datos1[seq(14,223,7),1],
                                       datos1[seq(15,224,7),1],
                                       datos1[seq(16,225,7),1],
                                       datos1[seq(17,226,7),1],
                                       datos1[seq(18,227,7),1])
} else if(datos1[13,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(14,254,8),1],
                                       datos1[seq(15,255,8),1],
                                       datos1[seq(16,256,8),1],
                                       datos1[seq(17,257,8),1],
                                       datos1[seq(18,258,8),1],
                                       datos1[seq(19,259,8),1],
                                       datos1[seq(20,260,8),1])
} else if(datos1[14,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(15,285,9),1],
                                       datos1[seq(16,410,9),1],
                                       datos1[seq(17,411,9),1],
                                       datos1[seq(18,412,9),1],
                                       datos1[seq(19,413,9),1],
                                       datos1[seq(20,414,9),1],
                                       datos1[seq(21,415,9),1],
                                       datos1[seq(22,416,9),1])
} else if(datos1[15,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(16,316,10),1],
                                       datos1[seq(17,317,10),1],
                                       datos1[seq(18,318,10),1],
                                       datos1[seq(19,319,10),1],
                                       datos1[seq(20,320,10),1],
                                       datos1[seq(21,321,10),1],
                                       datos1[seq(22,322,10),1],
                                       datos1[seq(23,323,10),1],
                                       datos1[seq(24,324,10),1])
} else if(datos1[16,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(17,347,11),1],
                                       datos1[seq(18,348,11),1],
                                       datos1[seq(19,349,11),1],
                                       datos1[seq(20,350,11),1],
                                       datos1[seq(21,351,11),1],
                                       datos1[seq(22,352,11),1],
                                       datos1[seq(23,353,11),1],
                                       datos1[seq(24,354,11),1],
                                       datos1[seq(25,355,11),1],
                                       datos1[seq(26,356,11),1])
} else if(datos1[17,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(18,378,12),1],
                                       datos1[seq(19,379,12),1],
                                       datos1[seq(20,380,12),1],
                                       datos1[seq(21,381,12),1],
                                       datos1[seq(22,382,12),1],
                                       datos1[seq(23,383,12),1],
                                       datos1[seq(24,384,12),1],
                                       datos1[seq(25,385,12),1],
                                       datos1[seq(26,386,12),1],
                                       datos1[seq(27,387,12),1],
                                       datos1[seq(28,388,12),1])
} else { 
  datos_por_anio[[length(anios)]] <- c(datos1[seq(19,409,13),1],
                                       datos1[seq(20,410,13),1],
                                       datos1[seq(21,411,13),1],
                                       datos1[seq(22,412,13),1],
                                       datos1[seq(23,413,13),1],
                                       datos1[seq(24,414,13),1],
                                       datos1[seq(25,415,13),1],
                                       datos1[seq(26,416,13),1],
                                       datos1[seq(27,417,13),1],
                                       datos1[seq(28,418,13),1],
                                       datos1[seq(29,419,13),1],
                                       datos1[seq(30,420,13),1])
}

# Nuevamente elimino los datos que no me dan informacion, cambio el tipo de decimal 
# y transformo los datos a numeros.

datos_por_anio[[length(anios)]] <- datos_por_anio[[length(anios)]][-which(datos_por_anio[[length(anios)]] == "")]
datos_por_anio[[length(anios)]] <- sub(",", ".",datos_por_anio[[length(anios)]], fixed = TRUE)
datos_por_anio[[length(anios)]] <- as.numeric(datos_por_anio[[length(anios)]])

######## LECTURA PARA ARCHIVOS DE TEXTO EN COLUMNA ########

# En caso de tener archivo de texto con ancho fijo, puedo usar otra funcion para
# leer los datos.

datos_smn <- read.table("197654-2.txt",skip=1,head=F,sep=",")
colnames(datos_smn) <- c("Estacion","Fechas","Tmax","Tmin","Precipitacion")

datos_Formosa    <- data.frame("fecha"=datos_smn$Fechas[which(datos_smn$Estacion=="87162" & datos_smn$Fechas=="2020-11-20"):
                                                                 which(datos_smn$Estacion=="87162" & datos_smn$Fechas=="2023-04-27")],
                               "PP"=as.numeric(datos_smn$Precipitacion[which(datos_smn$Estacion=="87162" & datos_smn$Fechas=="2020-11-20"):
                                                              which(datos_smn$Estacion=="87162" & datos_smn$Fechas=="2023-04-27")]))

datos_Corrientes <- data.frame("fecha"=datos_smn$Fechas[which(datos_smn$Estacion=="87166" & datos_smn$Fechas=="2020-11-20"):
                                                          which(datos_smn$Estacion=="87166" & datos_smn$Fechas=="2023-04-27")],
                               "PP"=as.numeric(datos_smn$Precipitacion[which(datos_smn$Estacion=="87166" & datos_smn$Fechas=="2020-11-20"):
                                                              which(datos_smn$Estacion=="87166" & datos_smn$Fechas=="2023-04-27")]))

####################################################

# LECTURA DE DATOS DE PRONOSTICO Y CALCULO DE 
# OCURRENCIA DE CATEGORIA DE ACUMULADO

####################################################

#Libreria a utilizar.

library(readxl) # Lee archivos excel.

# Cargo el archivo con los datos en una variable.

prono <- as.data.frame(read_xlsx("App_Pron_S2.xlsx"))

# Tengo que calcular la probabilidad de la ocurrencia de cada categoria de acumulado,
# para eso primero guardo los datos de precipitacion acumulada pronosticada por fecha 
# para analizar que pasa en los 16 miembros de cada dia en una lista.
# Tambien guardo las fechas de referencia para los pronosticos.

PP <- list()
prono$FechaRef <- as.character(prono$FechaRef)

fechas_prono <- prono$FechaRef[seq(1,which.max(prono$FechaRef=="2023-04-13")+15,16)]

# Guardo en cada elemento de la lista los 16 miembros de pronostico para cada dia.

for(i in 1:length(fechas_prono)) {
  PP[[i]] <- prono$PreS2_C[(1+16*(i-1)):(16+16*(i-1))]
}

# Ahora que cuento con los datos para cada fecha, calculo la probalidad de ocurrencia de cada categoria.
# Para ello, cuento la cantidad de miembros donde el pronostico cumple que se acumula igual o mas 
# precipitacion de la que indica cada categoria y lo divido por los miembros totales.

Probabilidad_1   <- c()
Probabilidad_20  <- c()
Probabilidad_50  <- c()
Probabilidad_100 <- c()

for(i in 1:length(fechas_prono)) {
  Probabilidad_1[i]   <- (length(PP[[i]][which(PP[[i]] >= 1)])/16)
  Probabilidad_20[i]  <- (length(PP[[i]][which(PP[[i]] >= 20)])/16)
  Probabilidad_50[i]  <- (length(PP[[i]][which(PP[[i]] >= 50)])/16)
  Probabilidad_100[i] <- (length(PP[[i]][which(PP[[i]] >= 100)])/16)
} 

# Guardo las fechas y los vectores de probabilidad de ocurrencia de las categorias en un dataframe.

Probabilidad <- data.frame("FechaRef"=fechas_prono,"Prob_1mm"=Probabilidad_1,"Prob_20mm"=Probabilidad_20,
                           "Prob_50mm"=Probabilidad_50,"Prob_100mm"=Probabilidad_100)

####################################################

# CALCULO DE ACUMULADOS DE LA SEMANA 2

####################################################

# Los pronosticos son desde el 12/11/2020, asi que la primera "semana 2" es la 
# del 20/11/2020 al 26/11/2020. Los acumulados se calculan entonces desde esa semana. 
# La ultima semana a trabajar es la del 21/4/2023 al 27/4/2023 asociado al 
# pronostico del 13/4/2023, ya que solo se cuenta con datos de PP hasta el 30/4/2023.

# Armo un dataframe que combine las fechas de todo el periodo de datos y la 
# precipitacion registrada. A los datos de precipitacion le saco el formato de 
# lista para que queden todos en un unico vector.
# Modificar las fechas en caso de que sean diferentes ante el uso de nuevos datos.

fechas <- as.character(seq(as.Date("1964-01-01"),as.Date("2023-04-30"),1))

Precipitacion <- unlist(datos_por_anio)

datos_PP <- data.frame("Fechas"=fechas, "PP"=Precipitacion, stringsAsFactors = FALSE)

# Ahora extraigo de todos los datos de precipitacion aquellos que permiten calcular 
# los acumulados de cada semana 2 (o sea, desde el 20/11/2020 al 27/04/2023). 
# Tambien genero un vector con todas las fechas que existen entre la primera y 
# la ultima de referencia.

datos_PP_prono <- datos_PP$PP[which(datos_PP$Fechas == fechas_prono[9]):
                                (which(datos_PP$Fechas == fechas_prono[length(fechas_prono)])+14)]

fechas_totales <- as.character(seq(as.Date("2020-11-12"),as.Date("2023-04-13"),1))

# Realizo los calculos de los acumulados de la semana 2. 
# Como toma 7 dias de datos, los ultimos 6 acumulados son NA dado que no tengo 
# informacion mas alla del 27/04/2023 (ejemplo: si me paro en el 22/04/2023, 
# necesito el dato del 28/04/2023, pero no lo tengo). Esos datos luego los elimino.

acumulados <- c()

for(i in 1:length(datos_Corrientes$PP)) {
  acumulados[i] <- sum(datos_Corrientes$PP[(1+(i-1)):(7+(i-1))]) # Agregar na.rm=T si hay NAs en los datos.
}

acumulados <- acumulados[-which(is.na(acumulados))] # Correr si no tengo NAs en los datos (mas alla de los ultimos 6).

acumulados <- acumulados[-c(884:889)] # Correr si tengo NAs en los datos (mas alla de los ultimos 6).

# Guardo las fechas y los acumulados en un dataframe.

datos_totales <- data.frame("A"=fechas_totales,"B"=acumulados)

# Necesito solo los datos de precipitacion acumulada para las fechas de referencia que hay
# en el excel. Hasta ahora tengo datos para todo el periodo 12/11/2020-13/04/2023 con sus
# respectivas semana 2, pero en los datos reales no todas las fechas presentan pronosticos.
# Selecciono entonces aquellos datos de acumulados donde las fechas totales del 
# periodo coincidan con aquellas donde hubo pronostico.

acumulados1 <- datos_totales$B[which(fechas_totales %in% fechas_prono)]

# IMPORTANTE: Si estoy trabajando con los datos de Resistencia Aero, continuo
# para generar el dataframe con toda la informacion. Pero si estoy trabajando con
# otra estacion, saltearse hasta la linea 329 (no correr ninguna linea hasta pasar
# la de "PP_acum") porque el dataframe original ya fue construido y sino se 
# perderia toda la informacion.

# Genero vectores con las fechas de inicio y final de las semana 2.

S2_Desde <- c()
S2_Hasta <- c()

for(i in 1:length(fechas_prono)) {
  S2_Desde[i] <- datos_PP$Fechas[which(datos_PP$Fechas == fechas_prono[i])+8]
  S2_Hasta[i] <- datos_PP$Fechas[which(datos_PP$Fechas == fechas_prono[i])+14]
}

# Genero un dataframe con las fechas de referencia del pronostico, de inicio y fin de la semana 2
# y los acumulados.

PP_acum <- data.frame("FechaRef"=fechas_prono,"S2_Desde"=S2_Desde,"S2_Hasta"=S2_Hasta,
                      "Acum_S2"=acumulados1)

# Hasta aca se obtuvo toda la informacion de Resistencia Aero, pero si tengo alguna
# estacion mas (como en este caso Corrientes o Formosa), entonces agrego otra 
# columna de acumulado en el dataframe anterior. Con la siguiente linea se
# agregaran todos los datos de precipitacion acumulada para cualquier estacion
# nueva con la que se trabaje, por lo que debe cambiarse el nombre segun la 
# estacion a agregar.

# IMPORTANTE: los acumulados de las estaciones de la RCB se guardan en el dataframe
# "PP_acum" desde el archivo "datos_estaciones". Aca solo se guardan datos de 
# estaciones del SMN.

PP_acum$Acum_S2_Corrientes <- acumulados1

# Agrego tambien en "Probabilidad" las fechas de inicio y final de cada semana 2.

Probabilidad$S2_Desde <- S2_Desde
Probabilidad$S2_Hasta <- S2_Hasta

#####################################################

# CALCULO DE OCURRENCIA DE PRECIPITACION ACUMULADA

#####################################################

# Ya calculados los acumulados de la semana 2, debe verificarse la ocurrencia de 
# precipitacion acumulada dentro cada categoría del pronostico (es decir, si para 
# cada acumulado se registro >=1mm, >=20mm, >=50mm y >=100mm).
# Se guarda la informacion en vectores, donde con "1" se determina que se registro 
# un acumulado dentro de la categoria estudiada y con "0" que el acumulado fue menor.

Ocurrencia_PP_1mm   <- c()
Ocurrencia_PP_20mm  <- c()
Ocurrencia_PP_50mm  <- c()
Ocurrencia_PP_100mm <- c()

for(i in 1:length(PP_acum$Acum_S2)){
  if(PP_acum$Acum_S2[i]>=1) {
    Ocurrencia_PP_1mm[i] <- 1
  } else {
    Ocurrencia_PP_1mm[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2)){
  if(PP_acum$Acum_S2[i]>=20) {
    Ocurrencia_PP_20mm[i] <- 1
  } else {
    Ocurrencia_PP_20mm[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2)){
  if(PP_acum$Acum_S2[i]>=50) {
    Ocurrencia_PP_50mm[i] <- 1
  } else {
    Ocurrencia_PP_50mm[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2)){
  if(PP_acum$Acum_S2[i]>=100) {
    Ocurrencia_PP_100mm[i] <- 1
  } else {
    Ocurrencia_PP_100mm[i] <- 0}
}

#####################################################

# CALCULO DE OCURRENCIA DE CATEGORIA DE ACUMULADO
# CLIMATOLOGICA

#####################################################

# Al momento de trabajar con la ocurrencia de la precipitacion acumulada,
# es importante tener la base climatologica de la ocurrencia de cada categoria
# de acumulado. Para ello, se deberan calcular los acumulados de cada semana 2
# durante un periodo de 30 anios.
# El periodo de 30 anios a utlizar es el de 1989-2018, de esta forma puedo tomar
# los primeros dias de 2019 para poder realizar los calculos de la semana 2 que
# incluye dias de diciembre y enero del anio siguiente y no estar parado en 
# algun anio que se use dentro del pronostico.

# Primero genero un vector de fechas para luego poder obtener los datos de PP
# asociado a todas esas fechas. Tomo todo el mes de enero y no solo los primeros
# dias para no tener problemas con las variaciones de las posiciones por los 
# anios bisciestos.
# Extraigo también los datos de PP asociados a dichas fechas y guardo todo en un
# dataframe para organizar la informacion.

fechas_clima <- fechas[which(fechas=="1989-01-01"):which(fechas=="2019-01-31")]

datos_PP_clima <- datos_PP$PP[which(datos_PP$Fechas == fechas_clima[1]):
                                which(datos_PP$Fechas == fechas_clima[length(fechas_clima)])]

datos_clima <- data.frame("Fechas"=fechas_clima,"PP"=datos_PP_clima)

# Ahora genero una lista donde voy a guardar en cada elemento los 30 datos de
# precipitacion acumulada en cada anio del periodo de referencia. Los acumulados
# se calculan segun las fechas de cada semana 2 guardadas en el dataframe 
# "PP_acum".

acum_clima    <- list()
acum_S2_clima <- c()

for(i in 1:length(PP_acum$FechaRef)) {
    for(j in 1:30) {
      acum_S2_clima[j] <- sum(datos_clima$PP[which(datos_clima$Fechas == paste(substr(datos_clima$Fechas[1+(366*(j-1))],1,4),substr(PP_acum$S2_Desde[i],6,10),sep="-")):
                                               which(datos_clima$Fechas == paste(substr(datos_clima$Fechas[1+(366*(j-1))],1,4),substr(PP_acum$S2_Hasta[i],6,10),sep="-"))],na.rm = T)
      acum_clima[[i]] <- acum_S2_clima
  }  
}

# Al momento de hacer lo acumulados, las semanas 2 que arrancan entre los dias
# 26 y 31 de diciembre tienen asociados dias de enero del anio siguiente. El 
# ciclo anterior no reconoce ese salto de anio (sino que sigue parado en el 
# mismo) entonces calcula un acumulado incorrecto. Ante esto, miro cuales son las 
# posiciones correspondientes a las fechas entre el 26 y 31 de diciembre para 
# que luego reemplace esos acumulados por los correctos, donde el anio de los 
# ultimos dias de la semana sea el siguiente y no el mismo.

dias_diciembre <- c(which(substr(PP_acum$S2_Desde,6,10) == "12-26"),which(substr(PP_acum$S2_Desde,6,10) == "12-27"),
                    which(substr(PP_acum$S2_Desde,6,10) == "12-28"),which(substr(PP_acum$S2_Desde,6,10) == "12-29"),
                    which(substr(PP_acum$S2_Desde,6,10) == "12-30"),which(substr(PP_acum$S2_Desde,6,10) == "12-31"))
dias_diciembre <- dias_diciembre[order(dias_diciembre)]

for(i in dias_diciembre) {
  for(j in 1:30) {
    acum_S2_clima[j] <- sum(datos_clima$PP[which(datos_clima$Fechas == paste(substr(datos_clima$Fechas[1+(366*(j-1))],1,4),substr(PP_acum$S2_Desde[i],6,10),sep="-")):
                                             which(datos_clima$Fechas == paste(substr(datos_clima$Fechas[1+(366*j)],1,4),substr(PP_acum$S2_Hasta[i],6,10),sep="-"))],na.rm = T)
    acum_clima[[i]]  <- acum_S2_clima
  }
}

# Ahora calculo la probabilidad de que ocurra cada categoria de acumulado a 
# nivel climatologico.

Probabilidad_1_clima   <- c()
Probabilidad_20_clima  <- c()
Probabilidad_50_clima  <- c()
Probabilidad_100_clima <- c()

for(i in 1:length(PP_acum$FechaRef)) {
  Probabilidad_1_clima[i]   <- (length(acum_clima[[i]][which(acum_clima[[i]] >= 1)])/30)
  Probabilidad_20_clima[i]  <- (length(acum_clima[[i]][which(acum_clima[[i]] >= 20)])/30)
  Probabilidad_50_clima[i]  <- (length(acum_clima[[i]][which(acum_clima[[i]] >= 50)])/30)
  Probabilidad_100_clima[i] <- (length(acum_clima[[i]][which(acum_clima[[i]] >= 100)])/30)
}

# Agrego las probabilidades al dataframe "Probabilidad" para tener tanto las 
# probabilidades de cada semana 2 segun los miembros de cada fecha de referencia
# como tambien las probabilidades climatologicas.

Probabilidad$Prob_1mm_clima   <- Probabilidad_1_clima
Probabilidad$Prob_20mm_clima  <- Probabilidad_20_clima
Probabilidad$Prob_50mm_clima  <- Probabilidad_50_clima
Probabilidad$Prob_100mm_clima <- Probabilidad_100_clima

#####################################################

# Lectura de datos para periodos calido y frio

#####################################################

# Voy a extraer los datos separados en periodos calidos y frios del anio. El
# periodo calido va de octubre a marzo y el frio de abril a septiembre.

################## PERIODO CALIDO ###################

# Solo tomo los datos que corresponden a los meses de octubre y marzo, pero 
# siempre y cuando el ultimo dia de la semana 2 no sea de abril (sin importar si
# el dia de inicio de la semana 2 es en marzo).

PP_acum_calido <- c()
PP_acum_calido <- PP_acum$Acum_S2[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido <- data.frame("Acum"=PP_acum_calido)

# Agrego las probabilidades de cada categoría y las climatologicas asociadas al
# periodo calido.

datos_calido$Prob_1mm <- Probabilidad$Prob_1mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_20mm <- Probabilidad$Prob_20mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_50mm <- Probabilidad$Prob_50mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_100mm <- Probabilidad$Prob_100mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_1mm_clima <- Probabilidad$Prob_1mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_20mm_clima <- Probabilidad$Prob_20mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido$Prob_50mm_clima <- Probabilidad$Prob_50mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_100mm_clima <- Probabilidad$Prob_100mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "12")]

# Agrego el numero de la semana 2 a la que corresponden las probabilidades y 
# los acumulados.

datos_calido$Semana_2 <- which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "12")

# Agrego las ocurrencias de cada categoria para las fechas del periodo calido.

datos_calido$Ocurrencia_1mm <- Ocurrencia_PP_1mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido$Ocurrencia_20mm <- Ocurrencia_PP_20mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido$Ocurrencia_50mm <- Ocurrencia_PP_50mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido$Ocurrencia_100mm <- Ocurrencia_PP_100mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "12")]

################### PERIODO FRIO ####################

# Solo tomo los datos que corresponden a los meses de abril y septiembre, pero 
# siempre y cuando el ultimo dia de la semana 2 no sea de octubre (sin importar 
# si el dia de inicio de la semana 2 es en septiembre).

PP_acum_frio <- c()
PP_acum_frio <- PP_acum$Acum_S2[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "09")]


datos_frio <- data.frame("Acum"=PP_acum_frio)

# Agrego las probabilidades de cada categoría y las climatologicas asociadas al
# periodo frio.

datos_frio$Prob_1mm <- Probabilidad$Prob_1mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_20mm <- Probabilidad$Prob_20mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_50mm <- Probabilidad$Prob_50mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_100mm <- Probabilidad$Prob_100mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_1mm_clima <- Probabilidad$Prob_1mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_20mm_clima <- Probabilidad$Prob_20mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "09")]

datos_frio$Prob_50mm_clima <- Probabilidad$Prob_50mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_100mm_clima <- Probabilidad$Prob_100mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "09")] 

# Agrego el numero de la semana 2 a la que corresponden las probabilidades y 
# los acumulados.

datos_frio$Semana_2 <- which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                               substr(PP_acum$S2_Hasta,6,7) == "05" | 
                               substr(PP_acum$S2_Hasta,6,7) == "06" | 
                               substr(PP_acum$S2_Hasta,6,7) == "07" | 
                               substr(PP_acum$S2_Hasta,6,7) == "08" | 
                               substr(PP_acum$S2_Hasta,6,7) == "09")

# Agrego las ocurrencias de cada categoria para las fechas del periodo frio.

datos_frio$Ocurrencia_1mm <- Ocurrencia_PP_1mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "09")]

datos_frio$Ocurrencia_20mm <- Ocurrencia_PP_20mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "09")]

datos_frio$Ocurrencia_50mm <- Ocurrencia_PP_50mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "09")]

datos_frio$Ocurrencia_100mm <- Ocurrencia_PP_100mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "09")]

####################################################

# CASOS EXTREMOS.

####################################################

# Primero busco aquellos casos donde habia pronostico nulo de precipitacion pero
# se registró acumulado.

# Como no existen probabilidades de acumulado nulo, tomo los casos con la proba-
# bilidad de lluvia más bajas.

print(which(Probabilidad$Prob_1mm < "0.1"))
print(Probabilidad$Prob_1mm[which(Probabilidad$Prob_1mm < "0.1")])
print(PP_acum$Acum_S2[which(Probabilidad$Prob_1mm < "0.1")])
print(PP_acum$S2_Desde[which(Probabilidad$Prob_1mm < "0.1")])
print(PP_acum$S2_Hasta[which(Probabilidad$Prob_1mm < "0.1")])

# El resto de las categorias si presentan probabilidad nula, por lo que miro
# en que semana donde se tenía dicha probabilidad si llovio.

# Categorias 20 mm.

prob_nulo_20 <- which(Probabilidad$Prob_20mm == "0")
print(PP_acum$Acum_S2[prob_nulo_20[which(PP_acum$Acum_S2[which(Probabilidad$Prob_20mm == "0")]>=20)]])
print(PP_acum$S2_Desde[prob_nulo_20[which(PP_acum$Acum_S2[which(Probabilidad$Prob_20mm == "0")]>=20)]])
print(PP_acum$S2_Hasta[prob_nulo_20[which(PP_acum$Acum_S2[which(Probabilidad$Prob_20mm == "0")]>=20)]])

# Categorias 50 mm.

prob_nulo_50 <- which(Probabilidad$Prob_50mm == "0")
print(PP_acum$Acum_S2[prob_nulo_50[which(PP_acum$Acum_S2[which(Probabilidad$Prob_50mm == "0")]>=50)]])
print(PP_acum$S2_Desde[prob_nulo_50[which(PP_acum$Acum_S2[which(Probabilidad$Prob_50mm == "0")]>=50)]])
print(PP_acum$S2_Hasta[prob_nulo_50[which(PP_acum$Acum_S2[which(Probabilidad$Prob_50mm == "0")]>=50)]])

# Categorias 100 mm.

prob_nulo_100 <- which(Probabilidad$Prob_100mm == "0")
print(PP_acum$Acum_S2[prob_nulo_100[which(PP_acum$Acum_S2[which(Probabilidad$Prob_100mm == "0")]>=100)]])
print(PP_acum$S2_Desde[prob_nulo_100[which(PP_acum$Acum_S2[which(Probabilidad$Prob_100mm == "0")]>=100)]])
print(PP_acum$S2_Hasta[prob_nulo_100[which(PP_acum$Acum_S2[which(Probabilidad$Prob_100mm == "0")]>=100)]])


# Ahora miro los casos donde la probabilidad de acumular 50mm o 100mm eran igua-
# les o superiores al 50% pero no se registro precipitacion.

# Categoria 50mm.

extremo_50 <- which(Probabilidad$Prob_50mm >= "0.5")

extremos_50mm <- data.frame("Prob_50mm"=Probabilidad$Prob_50mm[extremo_50[
  which(PP_acum$Acum_S2[which(Probabilidad$Prob_50mm >= "0.5")]<50)]])

extremos_50mm$Inicio_S2 <- PP_acum$S2_Desde[extremo_50[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_50mm >= "0.5")]<50)]]
  
extremos_50mm$Fin_S2 <- PP_acum$S2_Hasta[extremo_50[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_50mm >= "0.5")]<50)]]

extremos_50mm$Acum <- PP_acum$Acum_S2[extremo_50[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_50mm >= "0.5")]<50)]]

# Categoria 100mm.

extremo_100 <- which(Probabilidad$Prob_100mm >= "0.5")

extremos_100mm <- data.frame("Prob_100mm"=Probabilidad$Prob_100mm[extremo_100[
  which(PP_acum$Acum_S2[which(Probabilidad$Prob_100mm >= "0.5")]<100)]])

extremos_100mm$Inicio_S2 <- PP_acum$S2_Desde[extremo_100[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_100mm >= "0.5")]<100)]]

extremos_100mm$Fin_S2 <- PP_acum$S2_Hasta[extremo_100[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_100mm >= "0.5")]<100)]]

extremos_100mm$Acum <- PP_acum$Acum_S2[extremo_100[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_100mm >= "0.5")]<100)]]

################################################################################

####################################################

# LECTURA DE DATOS DE PRECIPITACION DE ESTACIONES
# DE LA RED COMUNITARIA BERMEJO.

####################################################

graphics.off() # Elimina congifuracion de graficos previos.
setwd("/home/ezequiel.amor")

# Libreria a usar en este programa.

library(openxlsx) # Permite leer los archivos excel.

####################################################

# LECTURA DE DATOS DE PRECIPITACION MENSUAL.

####################################################

# En primer lugar tengo que seleccionar los datos de precipitacion mensual en el
# mismo periodo que las fechas de pronostico (NOV 2020 - ABR 2023). Esos datos 
# se extraen tanto de los datos de Resistencia como de las estaciones de la RCB.

# ESTACION RESISTENCIA.

PP_mensual_RESISTENCIA <- c()

PP_mensual_RESISTENCIA[1]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2020-11")])
PP_mensual_RESISTENCIA[2]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2020-12")])
PP_mensual_RESISTENCIA[3]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-01")])
PP_mensual_RESISTENCIA[4]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-02")])
PP_mensual_RESISTENCIA[5]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-03")])
PP_mensual_RESISTENCIA[6]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-04")])
PP_mensual_RESISTENCIA[7]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-05")])
PP_mensual_RESISTENCIA[8]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-06")])
PP_mensual_RESISTENCIA[9]  <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-07")])
PP_mensual_RESISTENCIA[10] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-08")])
PP_mensual_RESISTENCIA[11] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-09")])
PP_mensual_RESISTENCIA[12] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-10")])
PP_mensual_RESISTENCIA[13] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-11")])
PP_mensual_RESISTENCIA[14] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2021-12")])
PP_mensual_RESISTENCIA[15] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-01")])
PP_mensual_RESISTENCIA[16] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-02")])
PP_mensual_RESISTENCIA[17] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-03")])
PP_mensual_RESISTENCIA[18] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-04")])
PP_mensual_RESISTENCIA[19] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-05")])
PP_mensual_RESISTENCIA[20] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-06")])
PP_mensual_RESISTENCIA[21] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-07")])
PP_mensual_RESISTENCIA[22] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-08")])
PP_mensual_RESISTENCIA[23] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-09")])
PP_mensual_RESISTENCIA[24] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-10")])
PP_mensual_RESISTENCIA[25] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-11")])
PP_mensual_RESISTENCIA[26] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2022-12")])
PP_mensual_RESISTENCIA[27] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2023-01")])
PP_mensual_RESISTENCIA[28] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2023-02")])
PP_mensual_RESISTENCIA[29] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2023-03")])
PP_mensual_RESISTENCIA[30] <- sum(datos_PP$PP[which(substr(datos_PP$Fechas,1,7) == "2023-04")])

# Genero un dataframe donde voy a guardar toda la información. Guardo en primer 
# lugar los datos mensuales de Resistencia.

PP_mensual <- data.frame("Resistencia"=PP_mensual_RESISTENCIA)

# Ahora trabajo con los datos de las estaciones RCB. Los datos vienen en archivos
# excel, entonces con "sheet" indico el nombre de la hoja a leer, con "cols" las
# columnas a leer y con "rows" las filas a leer.

# ESTACION MONCHOLO.

PP_mensual_MONCHOLO_2020 <- read.xlsx("5_Moncholo.xlsx",sheet="2020",cols=12:13,rows=37,colNames=F)
PP_mensual_MONCHOLO_2021 <- read.xlsx("5_Moncholo.xlsx",sheet="2021",cols=2:13,rows=37,colNames=F)
PP_mensual_MONCHOLO_2022 <- read.xlsx("5_Moncholo.xlsx",sheet="2022",cols=2:13,rows=37,colNames=F)
PP_mensual_MONCHOLO_2023 <- read.xlsx("5_Moncholo.xlsx",sheet="2023",cols=2:5,rows=37,colNames=F)


PP_mensual_MONCHOLO <- as.vector(c(PP_mensual_MONCHOLO_2020$X1,PP_mensual_MONCHOLO_2020$X2,
                                   PP_mensual_MONCHOLO_2021$X1,PP_mensual_MONCHOLO_2021$X2,
                                   PP_mensual_MONCHOLO_2021$X3,PP_mensual_MONCHOLO_2021$X4,
                                   PP_mensual_MONCHOLO_2021$X5,PP_mensual_MONCHOLO_2021$X6,
                                   PP_mensual_MONCHOLO_2021$X7,PP_mensual_MONCHOLO_2021$X8,
                                   PP_mensual_MONCHOLO_2021$X9,PP_mensual_MONCHOLO_2021$X10,
                                   PP_mensual_MONCHOLO_2021$X11,PP_mensual_MONCHOLO_2021$X12,
                                   PP_mensual_MONCHOLO_2022$X1,PP_mensual_MONCHOLO_2022$X2,
                                   PP_mensual_MONCHOLO_2022$X3,PP_mensual_MONCHOLO_2022$X4,
                                   PP_mensual_MONCHOLO_2022$X5,PP_mensual_MONCHOLO_2022$X6,
                                   PP_mensual_MONCHOLO_2022$X7,PP_mensual_MONCHOLO_2022$X8,
                                   PP_mensual_MONCHOLO_2022$X9,PP_mensual_MONCHOLO_2022$X10,
                                   PP_mensual_MONCHOLO_2022$X11,PP_mensual_MONCHOLO_2022$X12,
                                   PP_mensual_MONCHOLO_2023$X1,PP_mensual_MONCHOLO_2023$X2,
                                   PP_mensual_MONCHOLO_2023$X3,PP_mensual_MONCHOLO_2023$X4))

# Agrego los datos al dataframe

PP_mensual$Moncholo <- PP_mensual_MONCHOLO

# ESTACION GENERAL VEDIA.

PP_mensual_GRALVEDIA_2020 <- read.xlsx("11_General_Vedia.xlsx",sheet="2020",cols=12:13,rows=37,colNames=F)
PP_mensual_GRALVEDIA_2021 <- read.xlsx("11_General_Vedia.xlsx",sheet="2021",cols=2:13,rows=37,colNames=F)
PP_mensual_GRALVEDIA_2022 <- read.xlsx("11_General_Vedia.xlsx",sheet="2022",cols=2:13,rows=37,colNames=F)
PP_mensual_GRALVEDIA_2023 <- read.xlsx("11_General_Vedia.xlsx",sheet="2023",cols=2:5,rows=37,colNames=F)

PP_mensual_GRALVEDIA <- as.vector(c(PP_mensual_GRALVEDIA_2020$X1,PP_mensual_GRALVEDIA_2020$X2,
                                    PP_mensual_GRALVEDIA_2021$X1,PP_mensual_GRALVEDIA_2021$X2,
                                    PP_mensual_GRALVEDIA_2021$X3,PP_mensual_GRALVEDIA_2021$X4,
                                    PP_mensual_GRALVEDIA_2021$X5,PP_mensual_GRALVEDIA_2021$X6,
                                    PP_mensual_GRALVEDIA_2021$X7,PP_mensual_GRALVEDIA_2021$X8,
                                    PP_mensual_GRALVEDIA_2021$X9,PP_mensual_GRALVEDIA_2021$X10,
                                    PP_mensual_GRALVEDIA_2021$X11,PP_mensual_GRALVEDIA_2021$X12,
                                    PP_mensual_GRALVEDIA_2022$X1,PP_mensual_GRALVEDIA_2022$X2,
                                    PP_mensual_GRALVEDIA_2022$X3,PP_mensual_GRALVEDIA_2022$X4,
                                    PP_mensual_GRALVEDIA_2022$X5,PP_mensual_GRALVEDIA_2022$X6,
                                    PP_mensual_GRALVEDIA_2022$X7,PP_mensual_GRALVEDIA_2022$X8,
                                    PP_mensual_GRALVEDIA_2022$X9,PP_mensual_GRALVEDIA_2022$X10,
                                    PP_mensual_GRALVEDIA_2022$X11,PP_mensual_GRALVEDIA_2022$X12,
                                    PP_mensual_GRALVEDIA_2023$X1,PP_mensual_GRALVEDIA_2023$X2,
                                    PP_mensual_GRALVEDIA_2023$X3,PP_mensual_GRALVEDIA_2023$X4))

# Agrego los datos al dataframe.

PP_mensual$GralVedia <- PP_mensual_GRALVEDIA

# ESTACION LOTE 16.

PP_mensual_LOTE16_2020 <- read.xlsx("12_Lote16.xlsx",sheet="2020",cols=12:13,rows=37,colNames=F)
PP_mensual_LOTE16_2021 <- read.xlsx("12_Lote16.xlsx",sheet="2021",cols=2:13,rows=37,colNames=F)
PP_mensual_LOTE16_2022 <- read.xlsx("12_Lote16.xlsx",sheet="2022",cols=2:13,rows=37,colNames=F)
PP_mensual_LOTE16_2023 <- read.xlsx("12_Lote16.xlsx",sheet="2023",cols=2:5,rows=37,colNames=F)

PP_mensual_LOTE16 <- as.vector(c(PP_mensual_LOTE16_2020$X1,PP_mensual_LOTE16_2020$X2,
                                 PP_mensual_LOTE16_2021$X1,PP_mensual_LOTE16_2021$X2,
                                 PP_mensual_LOTE16_2021$X3,PP_mensual_LOTE16_2021$X4,
                                 PP_mensual_LOTE16_2021$X5,PP_mensual_LOTE16_2021$X6,
                                 PP_mensual_LOTE16_2021$X7,PP_mensual_LOTE16_2021$X8,
                                 PP_mensual_LOTE16_2021$X9,PP_mensual_LOTE16_2021$X10,
                                 PP_mensual_LOTE16_2021$X11,PP_mensual_LOTE16_2021$X12,
                                 PP_mensual_LOTE16_2022$X1,PP_mensual_LOTE16_2022$X2,
                                 PP_mensual_LOTE16_2022$X3,PP_mensual_LOTE16_2022$X4,
                                 PP_mensual_LOTE16_2022$X5,PP_mensual_LOTE16_2022$X6,
                                 PP_mensual_LOTE16_2022$X7,PP_mensual_LOTE16_2022$X8,
                                 PP_mensual_LOTE16_2022$X9,PP_mensual_LOTE16_2022$X10,
                                 PP_mensual_LOTE16_2022$X11,PP_mensual_LOTE16_2022$X12,
                                 PP_mensual_LOTE16_2023$X1,PP_mensual_LOTE16_2023$X2,
                                 PP_mensual_LOTE16_2023$X3,PP_mensual_LOTE16_2023$X4))

# Agrego los datos al dataframe.

PP_mensual$Lote16 <- PP_mensual_LOTE16

# ESTACION PUERTO BERMEJO KM 90.

PP_mensual_PTOBJOKM90_2020 <- read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2020",cols=12:13,rows=37,colNames=F)
PP_mensual_PTOBJOKM90_2021 <- read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=2:13,rows=37,colNames=F)
PP_mensual_PTOBJOKM90_2022 <- read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=2:13,rows=37,colNames=F)
PP_mensual_PTOBJOKM90_2023 <- read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2023",cols=2:5,rows=37,colNames=F)

PP_mensual_PTOBJOKM90 <- as.vector(c(PP_mensual_PTOBJOKM90_2020$X1,PP_mensual_PTOBJOKM90_2020$X2,
                                     PP_mensual_PTOBJOKM90_2021$X1,PP_mensual_PTOBJOKM90_2021$X2,
                                     PP_mensual_PTOBJOKM90_2021$X3,PP_mensual_PTOBJOKM90_2021$X4,
                                     PP_mensual_PTOBJOKM90_2021$X5,PP_mensual_PTOBJOKM90_2021$X6,
                                     PP_mensual_PTOBJOKM90_2021$X7,PP_mensual_PTOBJOKM90_2021$X8,
                                     PP_mensual_PTOBJOKM90_2021$X9,PP_mensual_PTOBJOKM90_2021$X10,
                                     PP_mensual_PTOBJOKM90_2021$X11,PP_mensual_PTOBJOKM90_2021$X12,
                                     PP_mensual_PTOBJOKM90_2022$X1,PP_mensual_PTOBJOKM90_2022$X2,
                                     PP_mensual_PTOBJOKM90_2022$X3,PP_mensual_PTOBJOKM90_2022$X4,
                                     PP_mensual_PTOBJOKM90_2022$X5,PP_mensual_PTOBJOKM90_2022$X6,
                                     PP_mensual_PTOBJOKM90_2022$X7,PP_mensual_PTOBJOKM90_2022$X8,
                                     PP_mensual_PTOBJOKM90_2022$X9,PP_mensual_PTOBJOKM90_2022$X10,
                                     PP_mensual_PTOBJOKM90_2022$X11,PP_mensual_PTOBJOKM90_2022$X12,
                                     PP_mensual_PTOBJOKM90_2023$X1,PP_mensual_PTOBJOKM90_2023$X2,
                                     PP_mensual_PTOBJOKM90_2023$X3,PP_mensual_PTOBJOKM90_2023$X4))

# Agrego los datos al dataframe.

PP_mensual$PtoBermejokm90 <- PP_mensual_PTOBJOKM90

# ESTACION TRES HORQUETAS ROLON.

PP_mensual_TRESHR_2020 <- read.xlsx("26_Tres_H.ROLON.xlsx",sheet="2020",cols=12:13,rows=37,colNames=F)
PP_mensual_TRESHR_2021 <- read.xlsx("26_Tres_H.ROLON.xlsx",sheet="2021",cols=2:13,rows=37,colNames=F)
PP_mensual_TRESHR_2022 <- read.xlsx("26_Tres_H.ROLON.xlsx",sheet="2022",cols=2:13,rows=37,colNames=F)
PP_mensual_TRESHR_2023 <- read.xlsx("26_Tres_H.ROLON.xlsx",sheet="2023",cols=2:5,rows=37,colNames=F)

PP_mensual_TRESHR <- as.vector(c(PP_mensual_TRESHR_2020$X1,PP_mensual_TRESHR_2020$X2,
                                 PP_mensual_TRESHR_2021$X1,PP_mensual_TRESHR_2021$X2,
                                 PP_mensual_TRESHR_2021$X3,PP_mensual_TRESHR_2021$X4,
                                 PP_mensual_TRESHR_2021$X5,PP_mensual_TRESHR_2021$X6,
                                 PP_mensual_TRESHR_2021$X7,PP_mensual_TRESHR_2021$X8,
                                 PP_mensual_TRESHR_2021$X9,PP_mensual_TRESHR_2021$X10,
                                 PP_mensual_TRESHR_2021$X11,PP_mensual_TRESHR_2021$X12,
                                 PP_mensual_TRESHR_2022$X1,PP_mensual_TRESHR_2022$X2,
                                 PP_mensual_TRESHR_2022$X3,PP_mensual_TRESHR_2022$X4,
                                 PP_mensual_TRESHR_2022$X5,PP_mensual_TRESHR_2022$X6,
                                 PP_mensual_TRESHR_2022$X7,PP_mensual_TRESHR_2022$X8,
                                 PP_mensual_TRESHR_2022$X9,PP_mensual_TRESHR_2022$X10,
                                 PP_mensual_TRESHR_2022$X11,PP_mensual_TRESHR_2022$X12,
                                 PP_mensual_TRESHR_2023$X1,PP_mensual_TRESHR_2023$X2,
                                 PP_mensual_TRESHR_2023$X3,PP_mensual_TRESHR_2023$X4))

# Agrego los datos al dataframe.

PP_mensual$TresHorquetasRolon <- PP_mensual_TRESHR

# Agrego también una columna con el numero de los meses.

PP_mensual$mes <- c(1:30)

# Estos datos se usaran en el programa "graficos" para plotear los valores 
# mensuales de precipitacion y asi poder comparar lo que sucede en Resistencia con
# las estaciones de la RCB.

####################################################

# CALCULO DE ACUMULADO PARA LA SEMANA 2.

####################################################

# Extraigo del archivo de precipitación de cada estacion para calcular los 
# acumulados de la semana 2 correspondientes a las mismas fechas de referencia 
# que para Resistencia Aero.

# IMPORTANTE: Para no repetir el programa, se debera modificar el nombre de la 
# estacion que se este trabajando en todas las variables que se usen a continuacion 
# y del archivo excel que se lea en "datos_PP_...".

# Necesito los datos de precipitacion entre el 20/11/2020 y el 27/4/2023 para
# calcular los acumulados. 
# Si hay algun dato que falta (en algunos archivos hay datos de precipitacion que 
# estan vacios) agregar un cero por cada dato faltante para que el largo de datos
# sea el correcto para aplicar los calculos. Al poner ceros en el mes donde falten datos, 
# no se modificaran las cuentas. (Por ejemplo, en los datos de Puerto Bermejo km 90 
# falta el dato del 31 de marzo, entonces luego de los datos de marzo 2023 se 
# agrega un cero para que se lean 31 datos totales del mes).

datos_PP_PtoBjokm90 <- rbind(read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2020",cols=12,rows=23:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2020",cols=13,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=2,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=3,rows=4:31,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=4,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=5,rows=4:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=6,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=7,rows=4:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=8,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=9,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=10,rows=4:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=11,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=12,rows=4:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2021",cols=13,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=2,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=3,rows=4:31,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=4,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=5,rows=4:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=6,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=7,rows=4:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=8,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=9,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=10,rows=4:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=11,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=12,rows=4:33,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2022",cols=13,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2023",cols=2,rows=4:34,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2023",cols=3,rows=4:31,colNames=F),
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2023",cols=4,rows=4:34,colNames=F),0,
                        read.xlsx("24_Puerto_bermejo_km90.xlsx",sheet="2023",cols=5,rows=4:30,colNames=F))

colnames(datos_PP_PtoBjokm90) <- ("PP")

# Si hay datos faltantes los asigno como NA.

datos_PP_PtoBjokm90[datos_PP_PtoBjokm90 == "DF"] <- NA

# Calculo los acumulados correspondientes.

# Si los datos se guardan como "character", los paso a "numeric". Sino saltear
# la siguiente linea.

datos_PP_Lote16 <- as.numeric(datos_PP_Lote16[,1])

acumulados_PtoBjokm90 <- c()

# Si hay dato faltante, agregar na.rm=T a la suma, sino lo dejo como esta.
# Si no tengo un vector de acumulados, recordar agregar en el for la dimension de 
# la columna para leer el largo y los datos del dataframe.

for(i in 1:length(datos_PP_PtoBjokm90[,1])) {
  acumulados_PtoBjokm90[i] <- sum(datos_PP_PtoBjokm90[(1+(i-1)):(7+(i-1)),1])
}

# Si no tengo datos faltantes, borro los ultimos 6 datos que se guardan como NAs.
# Si no es el caso, no correr la siguiente linea.

acumulados_PtoBjokm90 <- acumulados_PtoBjokm90[-which(is.na(acumulados_PtoBjokm90))]

# Si tengo dato faltante, borro lo ultimos 6 datos que se que son incorrectos
# pero que ahora no se reconocen como NAs.
# Si no es el caso, no correr la siguiente linea.

acumulados_Lote16 <- acumulados_Lote16[-c(884:889)]

# Guardo la información en un dataframe nuevo.

datos_totales_PtoBjokm90 <- data.frame("A"=fechas_totales,"B"=acumulados_PtoBjokm90)

# Me quedo solo con las semanas en que tengo pronotico.

acumulados_PtoBjokm90 <- datos_totales_PtoBjokm90$B[which(fechas_totales %in% fechas_prono)]

# Guardo la informacion en el mismo dataframe donde ya tenia los datos de 
# Resistencia Aero y alguna otra estacion, generado en el programa "datos_PP_Prono".

PP_acum$Acum_S2_PtoBermejokm90 <- acumulados_PtoBjokm90

#####################################################

# CALCULO DE OCURRENCIA DE PRECIPITACION ACUMULADA

#####################################################

PP_1mm_Promedio_RCB   <- c()
PP_20mm_Promedio_RCB <- c()
PP_50mm_Promedio_RCB  <- c()
PP_100mm_Promedio_RCB <- c()

for(i in 1:length(PP_acum$Acum_S2_Promedio_RCB)){
  if(PP_acum$Acum_S2_Promedio_RCB[i]>=1) {
    PP_1mm_Promedio_RCB[i] <- 1
  } else {
    PP_1mm_Promedio_RCB[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2_Promedio_RCB)){
  if(PP_acum$Acum_S2_Promedio_RCB[i]>=20) {
    PP_20mm_Promedio_RCB[i] <- 1
  } else {
    PP_20mm_Promedio_RCB[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2_Promedio_RCB)){
  if(PP_acum$Acum_S2_Promedio_RCB[i]>=50) {
    PP_50mm_Promedio_RCB[i] <- 1
  } else {
    PP_50mm_Promedio_RCB[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2_Promedio_RCB)){
  if(PP_acum$Acum_S2_Promedio_RCB[i]>=100) {
    PP_100mm_Promedio_RCB[i] <- 1
  } else {
    PP_100mm_Promedio_RCB[i] <- 0}
}

Ocurrencias_estaciones$Ocu_1mm_Promedio_RCB   <- PP_1mm_Promedio_RCB
Ocurrencias_estaciones$Ocu_20mm_Promedio_RCB  <- PP_20mm_Promedio_RCB
Ocurrencias_estaciones$Ocu_50mm_Promedio_RCB  <- PP_50mm_Promedio_RCB
Ocurrencias_estaciones$Ocu_100mm_Promedio_RCB <- PP_100mm_Promedio_RCB

#####################################################

# CALCULO DE PRECIPITACION ACUMULADA PROMEDIO PARA
# LAS 5 ESTACIONES DE LA RCB.

#####################################################

acumulados_prom <- c()

for(i in 1:length(PP_acum[,1])) {
  acumulados_prom[i] <- mean(c(PP_acum$Acum_S2_TresHoquetas2[i],
                             PP_acum$Acum_S2_Moncholo[i],
                             PP_acum$Acum_S2_GralVedia[i],
                             PP_acum$Acum_S2_Lote16[i],
                             PP_acum$Acum_S2_PtoBermejokm90[i]))
}

PP_acum$Acum_S2_Promedio_RCB <- acumulados_prom

################################################################################

####################################################

# INDICES PARA VERIFICACION DE PRONOSTICOS:
# METODOS PARA PRONOSTICOS PROBABILISTICOS.

####################################################

graphics.off() # Elimina confifuracion de graficos previos.
setwd("/home/ezequiel.amor")

# Liberia a usar en este programa.

library(verification)

# En "attach" cambiar el nombre del dataframe segun cual se necesite usar. Esto 
# permite que no haya que indicar a que dataframe pertencen los datos y que luego 
# solo se ponga el nombre de las columnas con la informacion necesaria para trabajar.
# (Ejemplo: en vez de usar "Probabilidad$Prob_1mm" se usa solo "Prob_1mm").

attach(Ocurrencias_estaciones)
attach(Probabilidad)

####################################################

# DIAGRAMA DE CONFIABILIDAD: RELIABILITY PLOT.

####################################################

# Primero con la funcion "verify" obtengo los datos de y.i, obar.i y de prob.y
# que voy a usar en el diagrama de confiabilidad.

A <- verify(Ocurrencia_PP_1mm,Prob_1mm,baseline=Prob_1mm_clima,frcst.type="prob",obs.type="binary")
B <- verify(Ocurrencia_PP_20mm,Prob_20mm,baseline=Prob_20mm_clima,frcst.type="prob",obs.type="binary")
C <- verify(Ocurrencia_PP_50mm,Prob_50mm,baseline=Prob_50mm_clima,frcst.type="prob",obs.type="binary")
D <- verify(Ocurrencia_PP_100mm,Prob_100mm,baseline=Prob_100mm_clima,frcst.type="prob",obs.type="binary")

# Veo los graficos como prueba.

reliability.plot(A$y.i,A$obar.i,A$prob.y,titl="Diagrama de confiabilidad")
reliability.plot(B$y.i,B$obar.i,B$prob.y,titl="Diagrama de confiabilidad")
reliability.plot(C$y.i,C$obar.i,C$prob.y,titl="Diagrama de confiabilidad")
reliability.plot(D$y.i,D$obar.i,D$prob.y,titl="Diagrama de confiabilidad")

# Luego en el programa "graficos" hago todos los plots juntos y con mas detalles.

####################################################

# BRIER SCORE Y OTROS PUNTAJES.

####################################################

# Primero hago el calculo de todos los scores y otras caracteristicas para cada 
# categoria de acumulado.

BS_1mm   <- brier(Ocu_1mm_Lote16,Prob_1mm,Prob_1mm_clima)
BS_20mm  <- brier(Ocu_20mm_Lote16,Prob_20mm,Prob_20mm_clima)
BS_50mm  <- brier(Ocu_50mm_Lote16,Prob_50mm,Prob_50mm_clima)
BS_100mm <- brier(Ocu_100mm_Lote16,Prob_100mm,Prob_100mm_clima)

# Ahora leo los scores que necesito:

# Brier Score y Baseline

print(BS_1mm$bs)
print(BS_1mm$bs.baseline)

print(BS_20mm$bs)
print(BS_20mm$bs.baseline)

print(BS_50mm$bs)
print(BS_50mm$bs.baseline)

print(BS_100mm$bs)
print(BS_100mm$bs.baseline)

# Brier Skill Score.

print(BS_1mm$ss)
print(BS_20mm$ss)
print(BS_50mm$ss)
print(BS_100mm$ss)

# Ahora calculo las diferentes componenetes de los scores:

# Componente Confiabilidad.

print(BS_1mm$bs.reliability)
print(BS_20mm$bs.reliability)
print(BS_50mm$bs.reliability)
print(BS_100mm$bs.reliability)

# Componenete Resolucion.

print(BS_1mm$bs.resol)
print(BS_20mm$bs.resol)
print(BS_50mm$bs.resol)
print(BS_100mm$bs.resol)

# Componenete Incertidumbre.

print(BS_1mm$bs - BS_1mm$bs.reliability + BS_1mm$bs.resol)
print(BS_20mm$bs - BS_20mm$bs.reliability + BS_20mm$bs.resol)
print(BS_50mm$bs - BS_50mm$bs.reliability + BS_50mm$bs.resol)
print(BS_100mm$bs - BS_100mm$bs.reliability + BS_100mm$bs.resol)

####################################################

# CARACTERISTICA OPERATIVA RELATIVA: CURVA ROC. 

####################################################

# Guardo la informacion de las curvas en variables, luego voy a plotearlas todas
# juntas y con mas detalles en el programa "graficos".

Z1 <- roc.plot(Ocurrencia_1mm,Prob_1mm,main="Curva ROC 1 mm")
Z2 <- roc.plot(Ocurrencia_20mm,Prob_20mm,main="Curva ROC 20 mm")
Z3 <- roc.plot(Ocurrencia_50mm,Prob_50mm,main="Curva ROC 50 mm")
Z4 <- roc.plot(Ocurrencia_100mm,Prob_100mm,main="Curva ROC 100 mm")

# Calculo tambien el area bajo la curva.

roc.area(Ocurrencia_PP_1mm,Prob_1mm)
roc.area(Ocurrencia_PP_20mm,Prob_20mm)
roc.area(Ocurrencia_PP_50mm,Prob_50mm)
roc.area(Ocurrencia_PP_100mm,Prob_100mm)

################################################################################

####################################################

# GRAFICOS.

####################################################

graphics.off() # Elimina configuracion de graficos previos 
setwd("/home/ezequiel.amor")

# Librerias a usar en este programa.

library(ggplot2) # Para graficar
library(zoo) # Para promedios moviles
library(patchwork) # Para unir graficos de ggplot2

####################################################

# DIAGRAMA DE CONFIABILIDAD: RELIABILITY PLOT.

####################################################

# Para poder graficar los diagramas necesito usar en el eje x los datos de 
# "y.i", mientras que en el eje y se usan los datos de "obar.i" guardados en A,
# B, C y D con verify en el programa "Indices_Verif_Prono".
# El eje x es el mismo para todos los graficos, solo varian los obar.i.

# Primero guardo toda la informacion en un dataframe para luego poder usar 
# ggplot.

datos_reliability <- data.frame("y.i"=A$y.i,"obar.i_1mm"=A$obar.i,
                                "obar.i_20mm"=B$obar.i,"obar.i_50mm"=C$obar.i,
                                "obar.i_100mm"=D$obar.i)

# Solo habria que modificar el titulo en funcion de la estacion, epoca del anio,
# etc. con la que se este trabajando.

gr <- ggplot(datos_reliability,aes(x=y.i)) + 
     geom_line(aes(x=y.i,y=obar.i_1mm,color="1mm"),linetype=1,size=0.5) + 
     geom_point(aes(y=obar.i_1mm),size=2,color="darkgreen") + 
     geom_line(aes(y=obar.i_20mm,color="20mm"),linetype=1,size=0.5) + 
     geom_point(aes(y=obar.i_20mm),size=2,color="blue") + 
     geom_line(aes(y=obar.i_50mm,color="50mm"),linetype=1,size=0.5) + 
     geom_point(aes(y=obar.i_50mm),size=2,color="darkviolet") + 
     geom_line(aes(y=obar.i_100mm,color="100mm"),linetype=1,size=0.5) + 
     geom_point(aes(y=obar.i_100mm),size=2,color="red") + 
     scale_x_continuous(breaks=seq(0,1,0.1)) + 
     scale_y_continuous(breaks=seq(0,1,0.1)) + 
     geom_abline(intercept=0, slope=1) + 
     labs(title="Reliability Plot para estación Tres Horquetas ROLON durante período frío (abr-sep)",
          x="Probabilidad de Pronóstico (y.i)",
          y="Frecuencia Relativa Observada (obar.i)",colour="Acumulado") + 
    theme_bw() + 
    coord_cartesian(ylim=c(0,1)) 
gr

####################################################

# GRAFICO DE BARRAS DEL RELIABILITY PLOT.

####################################################

# En el eje x van los valores de "y.i". Pero en el eje y en vez de poner los 
# datos de "prob.y", calculo la valores absolutos, es decir, cuantos datos 
# caen dentro de cada caja de probabilidad (cuantos valores de pronostico caen 
# en la caja de 0-10%, cuantos en 10-20%, etc).

val_abs_1mm   <- c()
val_abs_20mm  <- c()
val_abs_50mm  <- c()
val_abs_100mm <- c()

for(i in 1:10) {
  if(i == 1) {
    val_abs_1mm[i] <- length(which(Probabilidad$Prob_1mm >= 0 & 
                                     Probabilidad$Prob_1mm <= 0.1)) 
  } else {
    val_abs_1mm[i] <- length(which(Probabilidad$Prob_1mm > (0.1*(i-1)) & 
                                     Probabilidad$Prob_1mm <= (0.1+(0.1*(i-1)))))
  }
}

for(i in 1:10) {
  if(i == 1) {
    val_abs_20mm[i] <- length(which(Probabilidad$Prob_20mm >= 0 & 
                                      Probabilidad$Prob_20mm <= 0.1)) 
  } else {
    val_abs_20mm[i] <- length(which(Probabilidad$Prob_20mm > (0.1*(i-1)) & 
                                      Probabilidad$Prob_20mm <= (0.1+(0.1*(i-1)))))
  }
}

for(i in 1:10) {
  if(i == 1) {
    val_abs_50mm[i] <- length(which(Probabilidad$Prob_50mm >= 0 & 
                                      Probabilidad$Prob_50mm <= 0.1)) 
  } else {
    val_abs_50mm[i] <- length(which(Probabilidad$Prob_50mm > (0.1*(i-1)) & 
                                      Probabilidad$Prob_50mm <= (0.1+(0.1*(i-1)))))
  }
}

for(i in 1:10) {
  if(i == 1) {
    val_abs_100mm[i] <- length(which(Probabilidad$Prob_100mm >= 0 & 
                                       Probabilidad$Prob_100mm <= 0.1)) 
  } else {
    val_abs_100mm[i] <- length(which(Probabilidad$Prob_100mm > (0.1*(i-1)) & 
                                       Probabilidad$Prob_100mm <= (0.1+(0.1*(i-1)))))
  }
}

# Ahora armo los gráficos de barras, donde en el eje x va "y.i" y en el eje y 
# van los "val_abs". Primero armo el dataframe que va a leer ggplot.

datos_barras <- data.frame("y.i"=A$y.i,"val_abs_1mm"=val_abs_1mm,
                           "val_abs_20mm"=val_abs_20mm, 
                           "val_abs_50mm"=val_abs_50mm,
                           "val_abs_100mm"=val_abs_100mm)

g <- ggplot(datos_barras,aes(x=y.i,y=val_abs_1mm)) + 
  geom_bar(stat = "identity",fill="green",color="darkgreen") +
  scale_x_continuous(breaks=seq(0,1,0.1)) + 
  scale_y_continuous(breaks=seq(0,250,10)) + 
  labs(title="Frecuencia de Probabilidades de Pronósticos para 1 mm",
       x="Probabilidad de Pronóstico (y.i)",
       y="Cantidad de Valores de Probabilidad") +
  theme_bw()
g

g1 <- ggplot(datos_barras,aes(x=y.i,y=val_abs_20mm)) + 
  geom_bar(stat = "identity",fill="blue",color="darkblue") +
  scale_x_continuous(breaks=seq(0,1,0.1)) + 
  scale_y_continuous(breaks=seq(0,115,5)) + 
  labs(title="Frecuencia de Probabilidades de Pronósticos para 20 mm",
       x="Probabilidad de Pronóstico (y.i)",
       y="Cantidad de Valores de Probabilidad") +
  theme_bw()
g1

g2 <- ggplot(datos_barras,aes(x=y.i,y=val_abs_50mm)) + 
  geom_bar(stat = "identity",fill="violet",color="darkviolet") +
  scale_x_continuous(breaks=seq(0,1,0.1)) + 
  scale_y_continuous(breaks=seq(0,205,10)) + 
  labs(title="Frecuencia de Probabilidades de Pronósticos para 50 mm",
       x="Probabilidad de Pronóstico (y.i)",
       y="Cantidad de Valores de Probabilidad") +
  theme_bw()
g2

g3 <- ggplot(datos_barras,aes(x=y.i,y=val_abs_100mm)) + 
  geom_bar(stat = "identity",fill="red",color="darkred") +
  scale_x_continuous(breaks=seq(0,1,0.1)) + 
  scale_y_continuous(breaks=seq(0,430,15)) + 
  labs(title="Frecuencia de Probabilidades de Pronósticos para 100 mm",
       x="Probabilidad de Pronóstico (y.i)",
       y="Cantidad de Valores de Probabilidad") +
  theme_bw()
g3

# Uno los graficos

g + g1 + g2 + g3

####################################################

# GRAFICOS DE CLIMATOLOGIA.

####################################################

# Primero calculo la climatología completa, no solo para las semanas 2 que esta-
# mos estudiando. Los datos climatologicos de PP ya están en "datos_clima", 
# calculados en el programa "datos_PP_Prono".

# Elimino los dias 29 de febrero.

datos_clima <- datos_clima[-which(substr(datos_clima$Fechas,6,10) == "02-29"),]

# Calculo las probabilidades climatológicas.

acum_clima_total    <- list()
acum_S2_clima_total <- c()

for(i in 1:365) {
  for(j in 1:30) {
    if(j>=1 | j<=4) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))):((7+(i-1))+(365*(j-1)))],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total 
    } else if(j>=5 | j<=8) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))+1):((7+(i-1))+(365*(j-1))+1)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total 
    } else if(j>=9 | j<=12) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))+2):((7+(i-1))+(365*(j-1))+2)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total 
    } else if(j>=13 | j<=16) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))+3):((7+(i-1))+(365*(j-1))+3)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    } else if(j>=17 | j<=20) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))+4):((7+(i-1))+(365*(j-1))+4)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    } else if(j>=21 | j<=24) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(375*(j-1))+5):((7+(i-1))+(375*(j-1))+5)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    } else if(j>=25 | j<=28) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(375*(j-1))+6):((7+(i-1))+(375*(j-1))+6)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    } else {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(375*(j-1))+7):((7+(i-1))+(375*(j-1))+7)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    }
  }  
}

Probabilidad_1_clima_total   <- c()
Probabilidad_20_clima_total  <- c()
Probabilidad_50_clima_total  <- c()
Probabilidad_100_clima_total <- c()

for(i in 1:365) {
  Probabilidad_1_clima_total[i]   <- (length(acum_clima_total[[i]][which(acum_clima_total[[i]] >= 1)])/30)
  Probabilidad_20_clima_total[i]  <- (length(acum_clima_total[[i]][which(acum_clima_total[[i]] >= 20)])/30)
  Probabilidad_50_clima_total[i]  <- (length(acum_clima_total[[i]][which(acum_clima_total[[i]] >= 50)])/30)
  Probabilidad_100_clima_total[i] <- (length(acum_clima_total[[i]][which(acum_clima_total[[i]] >= 100)])/30)
}

# Guardo todo en un dataframe.

clima_total <- data.frame("Semana"=c(1:365),"Proba_1mm"=Probabilidad_1_clima_total,
                          "Proba_20mm"=Probabilidad_20_clima_total,
                          "Proba_50mm"=Probabilidad_50_clima_total,
                          "Proba_100mm"=Probabilidad_100_clima_total)

# Realizo el grafico.

g_clima <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_1mm,color="1mm"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=Proba_20mm,color="20mm"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=Proba_50mm,color="50mm"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=Proba_100mm,color="100mm"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018)",x="Semana 2",
       y="Probabilidad",colour="Acumulado") + 
  theme_bw()

g_clima

################ PROMEDIOS MOVILES #################

# Hago el mismo grafico pero aplicando promedios moviles. Para los promedios uso
# ventana de 5, 7 y 10 dias.

# Calculo los promedios moviles.

prom_5d_1mm   <- rollmean(clima_total$Proba_1mm,5,fill=NA)
prom_5d_20mm  <- rollmean(clima_total$Proba_20mm,5,fill=NA)
prom_5d_50mm  <- rollmean(clima_total$Proba_50mm,5,fill=NA)
prom_5d_100mm <- rollmean(clima_total$Proba_100mm,5,fill=NA)

prom_7d_1mm   <- rollmean(clima_total$Proba_1mm,7,fill=NA)
prom_7d_20mm  <- rollmean(clima_total$Proba_20mm,7,fill=NA)
prom_7d_50mm  <- rollmean(clima_total$Proba_50mm,7,fill=NA)
prom_7d_100mm <- rollmean(clima_total$Proba_100mm,7,fill=NA)

prom_10d_1mm   <- rollmean(clima_total$Proba_1mm,10,fill=NA)
prom_10d_20mm  <- rollmean(clima_total$Proba_20mm,10,fill=NA)
prom_10d_50mm  <- rollmean(clima_total$Proba_50mm,10,fill=NA)
prom_10d_100mm <- rollmean(clima_total$Proba_100mm,10,fill=NA)

# Los agrego al dataframe "clima_total" para poder graficar.

clima_total$prom_5d_1mm   <- prom_5d_1mm
clima_total$prom_5d_20mm  <- prom_5d_20mm
clima_total$prom_5d_50mm  <- prom_5d_50mm
clima_total$prom_5d_100mm <- prom_5d_100mm

clima_total$prom_7d_1mm   <- prom_7d_1mm
clima_total$prom_7d_20mm  <- prom_7d_20mm
clima_total$prom_7d_50mm  <- prom_7d_50mm
clima_total$prom_7d_100mm <- prom_7d_100mm

clima_total$prom_10d_1mm   <- prom_10d_1mm
clima_total$prom_10d_20mm  <- prom_10d_20mm
clima_total$prom_10d_50mm  <- prom_10d_50mm
clima_total$prom_10d_100mm <- prom_10d_100mm

# Graficos categoria 1 mm.

g_clima1_5d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_1mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_5d_1mm,color="PM 5 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 1 mm y Promedio Móvil (PM) de 5 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("green","black")) +
  theme_bw()

g_clima1_5d

g_clima1_7d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_1mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_7d_1mm,color="PM 7 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 1 mm y Promedio Móvil (PM) de 7 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") +
  scale_color_manual(values = c("green","black")) +
  theme_bw()

g_clima1_7d

g_clima1_10d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_1mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_10d_1mm,color="PM 10 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 1 mm y Promedio Móvil (PM) de 10 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") +
  scale_color_manual(values = c("green","black")) +
  theme_bw()
  
g_clima1_10d

# Graficos categoria 20 mm.

g_clima20_5d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_20mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_5d_20mm,color="PM 5 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 20 mm y Promedios Móviles (PM) de 5 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("turquoise","black")) +
    theme_bw()

g_clima20_5d

g_clima20_7d<- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_20mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_7d_20mm,color="PM 7 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 20 mm y Promedios Móviles (PM) de 7 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("turquoise","black")) +
  theme_bw()

g_clima20_7d

g_clima20_10d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_20mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_10d_20mm,color="PM 10 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 20 mm y Promedios Móviles (PM) de 10 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("turquoise","black")) +
  theme_bw()

g_clima20_10d

# Graficos categoria 50 mm.

g_clima50_5d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_50mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_5d_50mm,color="PM 5 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 50 mm y Promedios Móviles (PM) de 5 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("violet","black")) +
  theme_bw()

g_clima50_5d

g_clima50_7d<- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_50mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_7d_50mm,color="PM 7 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 50 mm y Promedios Móviles (PM) de 7 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("violet","black")) +
  theme_bw()

g_clima50_7d

g_clima50_10d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_50mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_10d_50mm,color="PM 10 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 50 mm y Promedios Móviles (PM) de 10 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("violet","black")) +
  theme_bw()

g_clima50_10d

# Graficos categoria 100 mm.

g_clima100_5d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_100mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_5d_100mm,color="PM 5 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 100 mm y Promedios Móviles (PM) de 5 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("red","black")) +
  theme_bw()

g_clima100_5d

g_clima100_7d<- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_100mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_7d_100mm,color="PM 7 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 100 mm y Promedios Móviles (PM) de 7 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("red","black")) +
  theme_bw()

g_clima100_7d

g_clima100_10d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_100mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_10d_100mm,color="PM 10 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 100 mm y Promedios Móviles (PM) de 10 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("red","black")) +
  theme_bw()

g_clima100_10d

#######################################################

# GRAFICOS DE DATOS MENSUALES DE DISTINTAS ESTACIONES.

#######################################################

# Grafico las precipitaciones mensuales de las estaciones de la RCB y de Resistencia
# Aero para ver como es el comportamiento espacial de la precipitacion y si es
# posible comparar lo que pasa en Resistencia Aero con las otras estaciones.
# Los datos mensuales se calcularon en el programa "datos_estaciones".

# Solo habria que modificar en el titulo el rango de meses en el que se tiene datos.

g_est <- ggplot(PP_mensual,aes(x=mes)) + 
  geom_line(aes(x=mes,y=Resistencia,color="Resistencia"),linetype=1,size=1.2) + 
  geom_line(aes(y=Moncholo,color="Moncholo"),linetype=1,size=0.5) + 
  geom_line(aes(y=GralVedia,color="Gral Vedia"),linetype=1,size=0.5) + 
  geom_line(aes(y=Lote16,color="Lote 16"),linetype=1,size=0.5) + 
  geom_line(aes(y=PtoBermejokm90,color="Puerto Bermejo KM 90"),linetype=1,size=0.5) + 
  geom_line(aes(y=TresHorquetasRolon,color="Tres Horquetas ROLON"),linetype=1,size=0.5) + 
  scale_x_continuous(breaks=seq(1,30,1)) + 
  scale_y_continuous(breaks=seq(0,350,10)) + 
  labs(title="Precipitaciones mensuales para estaciones de la RCB y Resistencia entre Noviembre 2020 y Abril 2023",
       x="Datos (Mes)",
       y="Precipitación Mensual (mm)",colour="Estaciones") + 
  theme_bw() +
  scale_color_manual(values=c("red1", "darkorange","green3","turquoise","black","darkviolet"))

g_est

####################################################

# CURVAS ROC.

####################################################

# Grafico todas las curvas ROC en una unica figura. Uso los datos guardados en 
# el programa "Indices_Verif_Prono" en Z1, Z2, Z3 y Z4.

# No habría que modificar nada más que el titulo del grafico. Cambiar "main" segun
# cual sea la estacion con la que se este trabajando, el periodo, etc.

plot(x=Z1$plot.data[,3,1],y=Z1$plot.data[,2,1], 
     main="Curvas ROC para el período frío (abr-sep) de la estación Tres Horquetas ROLON",
     type="b",col="green3",lwd=1.5,xlab="False Alarm Rate",ylab="Hit Rate")
lines(Z2$plot.data[,3,1],Z2$plot.data[,2,1],type="b",col="blue",lwd=1.5)
lines(Z3$plot.data[,3,1],Z3$plot.data[,2,1],type="b",col="darkviolet",lwd=1.5)
lines(Z4$plot.data[,3,1],Z4$plot.data[,2,1],type="b",col="red",lwd=1.5)
abline(a=0,b=1,lwd=1.5)
legend("bottomright", legend = c("1 mm","20 mm","50 mm","100 mm"),lwd = 2, 
       col = c("green3", "blue","darkviolet","red"),title="Categoría de Acumulado")

# Recordar que el area bajo la curva se calculo en "Indices_Verif_Prono".

################################################################################

####################################################

# MAPAS DE PRECIPITACION

####################################################

graphics.off() # Elimina configuracion de graficos previos.

setwd("/home/ezequiel.amor/Caso_100mm/Caso3") # Cambiar segun donde esten los archivos.

# Librerias a usar en este programa.

library(metR)
library(ggplot2)
library(maps)
library(mapdata)
library(ggrepel)
library(sf)
library(scales)

####################################################

# Leo los datos para cada dia de la semana en estudio en la region de Chaco.
# Si se queire usar otra region, modificar lat y lon.

pp1 <- ReadNetCDF("2022095.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp2 <- ReadNetCDF("2022096.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp3 <- ReadNetCDF("2022097.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp4 <- ReadNetCDF("2022098.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp5 <- ReadNetCDF("2022099.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp6 <- ReadNetCDF("2022100.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp7 <- ReadNetCDF("2022101.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 

# Calculo el acumulado de precipitacion durante esa semana para despues poder
# graficarlo. Luego lo guardo en un dataframe con sus correpondientes coordenadas.

suma_pp <- c()

for(i in 1:length(pp1$precipitation)) {
  suma_pp[i] <- pp1$precipitation[i] + pp2$precipitation[i] + pp3$precipitation[i] +
    pp4$precipitation[i] + pp5$precipitation[i] + pp6$precipitation[i] + 
    pp7$precipitation[i]
}

pp_total <- data.frame("lon"=pp1$lon,"lat"=pp1$lat,"precipitation"=suma_pp)

# Elementos para el mapa.
# Para los limites se debe cambiar el setwd segun donde esten los archivos con
# dichos datos. Una vez cargados, no hace falta volver a correr las lineas.
# Luego se guardan las coordenadas de las diferentes estaciones para luego 
# localizarlas en el grafico.

mapa_pais          <- read_sf("paises/linea_de_limite_FA004.shp")
mapa_provincias    <- read_sf("provincias/linea_de_limite_070111.shp")
mapa_departamentos <- read_sf("departamentos/linea_de_limite_070110.shp")

coord_Resistencia <- data.frame(longR=c(-59.04583),latR=c(-27.43861),
                                stringsAsFactors = F)
coord_Corrientes  <- data.frame(longC=c(-58.75974),latC=c(-27.44979),
                                stringsAsFactors = F)
coord_Formosa     <- data.frame(longF=c(-58.22929),latF=c(-26.21242),
                                stringsAsFactors = F)
coord_TresHROLON  <- data.frame(longT=c(-58.5863892276972),latT=c(-26.9696770400602),
                                stringsAsFactors = F)
coord_Moncholo    <- data.frame(longM=c(-58.682443),latM=c(-27.043854),
                                stringsAsFactors = F)
coord_GralVedia   <- data.frame(longG=c(-58.661198137723),latG=c(-26.930967580464),
                                stringsAsFactors = F)
coord_Lote16      <- data.frame(longL=c(-58.8067559429548),latL=c(-26.8659190692775),
                                stringsAsFactors = F)
coord_PtoBjo90    <- data.frame(longP=c(-58.5838380967161),latP=c(-26.8785437446458),
                               stringsAsFactors = F)

# Agrego el valor de precipitacion para poner luego en los circulos donde estan
# posicionadas las estaciones meteorologicas en el grafico.

coord_Resistencia$acumulado <- PP_acum$Acum_S2_Resistencia[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_Corrientes$acumulado  <- PP_acum$Acum_S2_Corrientes[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_Formosa$acumulado     <- PP_acum$Acum_S2_Formosa[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_TresHROLON$acumulado  <- PP_acum$Acum_S2_TresHoquetas2[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_Moncholo$acumulado    <- PP_acum$Acum_S2_Moncholo[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_GralVedia$acumulado   <- PP_acum$Acum_S2_GralVedia[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_Lote16$acumulado      <- PP_acum$Acum_S2_Lote16[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_PtoBjo90$acumulado    <- PP_acum$Acum_S2_PtoBermejokm90[PP_acum$S2_Desde==as.character(pp1$time[1])]

# Guardo las fechas del dia inicial y final de cada semana para tener un 
# formato del tipo DD/MM/AAAA. 

dia_inicio <- paste(substr(as.character(pp1$time[1]),9,10),"/",substr(as.character(pp1$time[1]),6,7),"/",
                    substr(as.character(pp1$time[1]),1,4),sep="")
dia_final  <- paste(substr(as.character(pp7$time[1]),9,10),"/",substr(as.character(pp7$time[1]),6,7),"/",
                    substr(as.character(pp7$time[1]),1,4),sep="")

# Realizo el gráfico.
# No haria falta modificar nada, salvo que se quiera cambiar la region de la 
# figura. En ese caso cambiar los limites en "coord_sf".

if(max(pp_total$precipitation)<=30) {
  ggplot(pp_total,aes(lon,lat)) +
    geom_contour_fill(aes(z=precipitation)) +
    geom_contour(aes(z=precipitation),color="grey90",size=0.15) + 
    geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
    geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
    geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
    coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
    geom_point(data=coord_Resistencia,aes(longR,latR,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Resistencia,aes(longR,latR,label=paste(acumulado,"mm")),
              color="black",size=2.28) + 
    geom_text(data=coord_Resistencia,aes(longR,latR,label="Est. 1"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.001,fontface="bold") +
    geom_point(data=coord_TresHROLON,aes(longT,latT,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_TresHROLON,aes(longT,latT,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_TresHROLON,aes(longT,latT,label="Est. 2"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.002,fontface="bold") +
    geom_point(data=coord_Moncholo,aes(longM,latM,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Moncholo,aes(longM,latM,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Moncholo,aes(longM,latM,label="Est. 3"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.004,fontface="bold") +
    geom_point(data=coord_GralVedia,aes(longG,latG,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_GralVedia,aes(longG,latG,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_GralVedia,aes(longG,latG,label="Est. 4"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.009,fontface="bold") +
    geom_point(data=coord_Lote16,aes(longL,latL,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Lote16,aes(longL,latL,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Lote16,aes(longL,latL,label="Est. 5"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.002,fontface="bold") +
    geom_point(data=coord_PtoBjo90,aes(longP,latP,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_PtoBjo90,aes(longP,latP,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_PtoBjo90,aes(longP,latP,label="Est. 6"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.002,fontface="bold") +
    geom_point(data=coord_Corrientes,aes(longC,latC,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Corrientes,aes(longC,latC,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Corrientes,aes(longC,latC,label="Est. 7"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.001,fontface="bold") +
    geom_point(data=coord_Formosa,aes(longF,latF,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Formosa,aes(longF,latF,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Formosa,aes(longF,latF,label="Est. 8"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.003,fontface="bold") +
    scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,30),
                           breaks=seq(0,30,3),oob=squish,guide=NULL) +
    scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,30),
                         breaks=seq(0,30,3),oob=squish) +
    labs(fill="Acumulado (mm)",x="Longitud",y="Latitud",
         title=paste("Precipitación acumulada entre el",dia_inicio,"y el",dia_final),
         subtitle=paste("Probabilidad de acumulado: 1 mm =",
                        Probabilidad$Prob_1mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "20 mm =",Probabilidad$Prob_20mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "50 mm =",Probabilidad$Prob_50mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "100 mm =",Probabilidad$Prob_100mm[Probabilidad$S2_Desde==as.character(pp1$time[1])])) +
    theme(legend.key.size=unit(1.5,'cm')) +
    geom_label(aes(x=-57.671,y=-27.855,label="Estaciones (Est.):
1 - Resistencia Aero
2 - Tres Horquetas ROLON
3 - Moncholo
4 - General Vedia
5 - Lote 16
6 - Puerto Bermejo Km 90
7 - Corrientes Aero
8 - Formosa Aero"),stat="unique",size=3,color="black",fill="white") 
} else {
  ggplot(pp_total,aes(lon,lat)) +
    geom_contour_fill(aes(z=precipitation)) +
    geom_contour(aes(z=precipitation),color="grey90",size=0.15) + 
    geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
    geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
    geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
    coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
    geom_point(data=coord_Resistencia,aes(longR,latR,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Resistencia,aes(longR,latR,label=paste(acumulado,"mm")),
              color="black",size=2.28) + 
    geom_text(data=coord_Resistencia,aes(longR,latR,label="Est. 1"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.001,fontface="bold") +
    geom_point(data=coord_TresHROLON,aes(longT,latT,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_TresHROLON,aes(longT,latT,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_TresHROLON,aes(longT,latT,label="Est. 2"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.002,fontface="bold") +
    geom_point(data=coord_Moncholo,aes(longM,latM,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Moncholo,aes(longM,latM,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Moncholo,aes(longM,latM,label="Est. 3"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.004,fontface="bold") +
    geom_point(data=coord_GralVedia,aes(longG,latG,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_GralVedia,aes(longG,latG,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_GralVedia,aes(longG,latG,label="Est. 4"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.009,fontface="bold") +
    geom_point(data=coord_Lote16,aes(longL,latL,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Lote16,aes(longL,latL,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Lote16,aes(longL,latL,label="Est. 5"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.002,fontface="bold") +
    geom_point(data=coord_PtoBjo90,aes(longP,latP,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_PtoBjo90,aes(longP,latP,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_PtoBjo90,aes(longP,latP,label="Est. 6"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.002,fontface="bold") +
    geom_point(data=coord_Corrientes,aes(longC,latC,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Corrientes,aes(longC,latC,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Corrientes,aes(longC,latC,label="Est. 7"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.001,fontface="bold") +
    geom_point(data=coord_Formosa,aes(longF,latF,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Formosa,aes(longF,latF,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Formosa,aes(longF,latF,label="Est. 8"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.003,fontface="bold") +
    scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                           breaks=seq(0,100,10),oob=squish,guide=NULL) +
    scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish) +
    labs(fill="Acumulado (mm)",x="Longitud",y="Latitud",
         title=paste("Precipitación acumulada entre el",dia_inicio,"y el",dia_final),
         subtitle=paste("Probabilidad de acumulado: 1 mm =",
                        Probabilidad$Prob_1mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "20 mm =",Probabilidad$Prob_20mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "50 mm =",Probabilidad$Prob_50mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "100 mm =",Probabilidad$Prob_100mm[Probabilidad$S2_Desde==as.character(pp1$time[1])])) +
    theme(legend.key.size=unit(1.5,'cm')) +
    geom_label(aes(x=-57.671,y=-27.855,label="Estaciones (Est.):
1 - Resistencia Aero
2 - Tres Horquetas ROLON
3 - Moncholo
4 - General Vedia
5 - Lote 16
6 - Puerto Bermejo Km 90
7 - Corrientes Aero
8 - Formosa Aero"),stat="unique",size=3,color="black",fill="white")
}

################################################################################

#######################################################

# DATOS DE PRONOSTICO ORIGINALES PARA EVALUAR ERRORES

#######################################################

graphics.off() # Elimina configuracion previa de otros graficos.

# Librerias a usar en este programa.

library(metR)
library(ggplot2)
library(maps)
library(mapdata)
library(ggrepel)
library(sf)
library(scales)
library(ncdf4)
library(patchwork)
library(rnaturalearth)

# Agregar los limites de los paises limitrofes (mapa_paises).

mapa_paises <- geom_sf(data=rnaturalearth::ne_countries(country = c("chile","uruguay","paraguay","brazil","bolivia"),returnclass="sf"),
                       inherit.aes=FALSE,fill=NA,color="black")

# Por si no estan cargados el resto de los limites, agregarlos con las siguientes 
# lineas. Recordar que si no estoy parado en las carpetas donde estan los archivos, 
# hay que cambiar el setwd para cargarlos.

mapa_pais          <- read_sf("paises/linea_de_limite_FA004.shp")
mapa_provincias    <- read_sf("provincias/linea_de_limite_070111.shp")
mapa_departamentos <- read_sf("departamentos/linea_de_limite_070110.shp")

####################################################

setwd("/fiona/Prono_Semanal/pre/Data/D20230328") #Cambiar segun la fecha del archivo.

# PRECIPITACION SEGUN LA MEDIA DEL ENSAMBLE.

data_media <- "prate.20230328.week2mean.nc"

pp_media   <- ReadNetCDF(data_media,vars="PRATE_surface", 
                         subset = list(latitude = c(-31,-22.5),longitude = c(296,308.4371))) 

# En caso de querer otra region de datos, modificar las latitudes y longitudes 
# de pp_media.

# Paso las longitudes de 0;360 a -180;180

pp_media$lon <- ConvertLongitude(pp_media$longitude, from = 360)

# Los datos de los archivos son "precipitation rate", con unidades de kg/m**2/s.
# Tengo que convertirlos a milimetros, por lo que tengo que multiplicar a cada 
# dato por 86400 para pasarlos a mm/d y luego por 7, dado que tenemos datos semanales.

PRATE_a_mm <- c()

for(i in 1:length(pp_media$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_media$PRATE_surface[i]*86400*7
}

pp_media$acumulado <- PRATE_a_mm

# Guardo el grafico en una variable.
# "coord_sf" son los limites de la figura. Cambiar en caso de que el area a graficar
# sea mayor o menor. 
# "annotate" es el rectágulo para marcar la región de interés.
# "guide=NULL" no muestra la escala. Si se quiere ver, eliminar este termino.

g_media <- ggplot(pp_media,aes(lon,latitude)) +
    geom_contour_fill(aes(z=acumulado)) +
    geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
    geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
    geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
    geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
    mapa_paises +
    coord_sf(ylim=c(-30.3,-23),xlim=c(-63.23,-52.5)) +
    annotate("rect",xmin =-59.45,xmax=-57.55,ymin=-27.95,ymax=-26.05,fill="NA",color="red2",size=0.8) +
    labs(fill="mm",x="Longitud",y="Latitud",
         title= "Media del ensamble") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                           breaks=seq(0,100,10),oob=squish,guide=NULL) +
    scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL)

# Ahora miro los datos observados en la misma region. Cambiar el setwd para hacer
# esto segun donde esten guardado los archivos de precipitacion observada!.
# Si se quiere otra area de datos, cambiar lat y lon.

setwd("/home/ezequiel.amor/Menos_de_100")

pp1 <- ReadNetCDF("2023095.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp2 <- ReadNetCDF("2023096.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp3 <- ReadNetCDF("2023097.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp4 <- ReadNetCDF("2023098.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp5 <- ReadNetCDF("2023099.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp6 <- ReadNetCDF("2023100.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95))) 
pp7 <- ReadNetCDF("2023101.nc", vars = "precipitation", 
                  subset = list(lat = c(-31,-22.5),lon = c(-64,-51.95)))

suma_pp <- c()

for(i in 1:length(pp1$precipitation)) {
  suma_pp[i] <- pp1$precipitation[i] + pp2$precipitation[i] + pp3$precipitation[i] +
    pp4$precipitation[i] + pp5$precipitation[i] + pp6$precipitation[i] + 
    pp7$precipitation[i]
}

pp_total <- data.frame("lon"=pp1$lon,"lat"=pp1$lat,"precipitation"=suma_pp)

# Guardo el mapa en una variable.

g_obs <- ggplot(pp_total,aes(lon,lat)) +
    geom_contour_fill(aes(z=precipitation)) +
    geom_contour(aes(z=precipitation),color="grey90",size=0.15) + 
    geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
    geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
    geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
    mapa_paises +
    coord_sf(ylim=c(-30.3,-23),xlim=c(-63.23,-52.5)) +
    annotate("rect",xmin =-59.45,xmax=-57.55,ymin=-27.95,ymax=-26.05,fill="NA",color="red2",size=0.8) +
    labs(fill="mm",x="Longitud",y="Latitud",
         title= "Precipitación observada") +
    theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

####################################################

# PRECIPITACION SEGUN CADA MIEMBRO DEL ENSAMBLE.

setwd("/fiona/Prono_Semanal/pre/Data/D20230328") # Volver a cambiar a donde estan 
                                                 # los datos de pronostico.

# Como la fecha de cada miembro de pronostico es la misma de la media del
# ensamble, se la extrae del nombre de "data_media" para no escribirla a mano.
# Como hay 16 miembros, se repite el programa para cada uno cambiando el 
# numero del miembro.

# Además se guarda en una variable el maximo de cada miembro. Aquellos que no 
# tengan informacion, tendran un maximo igual a cero. Es una buena forma de que
# si hay algun warning al graficar, se pueda identificar facilmente cual es el 
# miembro que no posee datos.

# No hace falta modificar nada, salvo que quiere usarse otra region para la
# figura y allí deberían cambiarse la latitud y la longitud de los datos a leer 
# en pp_miembro y en los graficos.

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m01.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g1 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m01 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m02.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g2 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m02 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m03.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g3 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m03 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m04.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g4 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m04 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m05.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g5 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m05 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m06.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g6 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m06 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m07.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g7 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m07 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m08.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g8 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m08 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m09.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g9 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,28,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m09 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m10.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g10 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m10 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m11.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g11 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m11 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m12.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g12 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m12 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m13.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g13 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m13 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m14.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g14 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m14 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m15.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g15 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m15 <- max(pp_miembro$acumulado)

########

miembro <- paste("prate.",substr(data_media,7,14),".week2mean_m16.nc",sep="")

pp_miembro <- ReadNetCDF(miembro,vars="PRATE_surface",
                         subset = list(latitude = c(-28.5,-25.5),longitude = c(299.9996,302.8121)))

pp_miembro$lon <- ConvertLongitude(pp_miembro$longitude, from = 360)

PRATE_a_mm <- c()

for(i in 1:length(pp_miembro$PRATE_surface)) {
  PRATE_a_mm[i] <- pp_miembro$PRATE_surface[i]*86400*7
}

pp_miembro$acumulado <- PRATE_a_mm

# Realizo el grafico.

g16 <- ggplot(pp_miembro,aes(lon,latitude)) +
  geom_contour_fill(aes(z=acumulado)) +
  geom_contour(aes(z=acumulado),color="grey90",size=0.15) + 
  geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
  geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
  geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
  coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
  scale_x_continuous(breaks = seq(-59.45,-57.55,0.8))+
  labs(fill="mm",x="Longitud",y="Latitud",
       title= paste("Miembro",substr(miembro,27,28))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size=unit(0.65,'cm')) +
  scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish,guide=NULL) +
  scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                       breaks=seq(0,100,10),oob=squish,guide=NULL)

max_m16 <- max(pp_miembro$acumulado)

######################################################

# Combino todos los graficos.
# El grafico de precipitacion observada y la media del ensamble quedan uno arriba
# del otro, y los 16 miembros se grafican en paneles más pequeños hacia la derecha
# de los primeros dos graficos mencionados.

(g_obs/g_media)|(g1+g2+g3+g4+g5+g6+g7+g8+g9+g10+g11+g12+g13+g14+g15+g16)
