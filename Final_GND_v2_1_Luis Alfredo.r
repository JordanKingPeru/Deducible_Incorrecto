rm(list = ls())

#################### GND - RIESGO A NIVEL DE FACTURA ####################
#-------1. Cargar datos de DETALLE de CPE  y limpieza de Descripción --------

# Librerias necesarias
library(data.table)
library(tm)
library(stringr)
library(stringi)

# Aumento de memoria
memory.limit(size = 12000)

# Topes para la carga de datos por partes
topes <- c(1,seq(1000001,70000001, 1000000))
nombres <- c("COD_TIPOCOMP", "NUM_RUC", "NUM_SERIECPE", "NUM_CPE", "DES_DETITM")

cpes_limpio <- NULL

for (i in 1:length(topes)) {
  # i=1
  cpes <- fread("cpes.csv", nrows = 1000000, skip = topes[i],header =  F)
  names(cpes) <-  nombres
  # cpes$DES_DETITM <- chartr('áéíóúàèìòù','aeiounaeioun',cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub("s/"," ",cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('S/'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub(' / '," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('@'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('#'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('&'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('%'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub(':'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('Ç'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('¥'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('!'," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('/c '," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub('/C '," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub(' x '," ", cpes$DES_DETITM)
  # cpes$DES_DETITM <- gsub(' X '," ", cpes$DES_DETITM)
  cpes$DES_DETITM <- stri_trim(cpes$DES_DETITM)
  cpes$DES_DETITM <- stri_trans_tolower(cpes$DES_DETITM)
  cpes$DES_DETITM <- removeWords(cpes$DES_DETITM, stopwords(kind = "spanish"))
  cpes$DES_DETITM <- removePunctuation(cpes$DES_DETITM, preserve_intra_word_contractions = FALSE, preserve_intra_word_dashes = FALSE)
  cpes$DES_DETITM <- removeNumbers(cpes$DES_DETITM)
  cpes$DES_DETITM <- stripWhitespace(cpes$DES_DETITM)
  cpes_limpio <- rbind(cpes_limpio, cpes)
  
  print(paste("vamos en ", i, " de ", length(topes)))
  
}

#setwd("D:/RETOS/GastosND/Data/Grupal_completo")
write.csv(cpes_limpio, file = "cpes_limpio.csv", row.names = F)



rm(list = ls())

#-------2. Selección de RUC de datos emisor (preestablecidos) y Matriz de datos CPE--------
rm(list = ls())

library(tidyverse)
library(data.table)
library(stringi)
library(dummies)
library(randomForest)
library(caret)
library(nnet)
library(devtools)
library(rpart)
library(e1071)
library(ROCR)
library(xgboost)
library(Matrix)

memory.limit(size = 12000) #Mejora el rendimiento de la memoria RAM

# Carga de archivos base
Ficha_Nacional <- fread("ficha_nacional.csv")
bdCIIU_nc        <- fread("MaestroCIIU V4.0.csv",select = c("cod_act_princ","CATEGO_OBJETIVO","Actividad_Economica"),colClasses = list(character=1:3))
RUC_Seleccionado <- fread("RUC_GOV_CONSOLIDADO.csv",select = c("ruc","CATEGO_OBJETIVO","Actividad_Economica"),colClasses = list(character=1:3))

# Seleccion de RUC por CIUU de riesgo
P1 <- inner_join(Ficha_Nacional,filter(bdCIIU_nc,CATEGO_OBJETIVO==1),by=c("cod_act_princ"))

# Por RUC seleccionado como emisor de riesgo
P2 <- inner_join(Ficha_Nacional,RUC_Seleccionado,by=c("ruc"))
rm(Ficha_Nacional)

# Tabla FichaEmisor_CIIU
FichaEmisor_CIIU <- unique(bind_rows(P1,P2))

# Limpieza de memoria
rm(P1,P2,bdCIIU_nc,RUC_Seleccionado)
write.csv(FichaEmisor_CIIU, "FichaEmisor_CIIU.csv", row.names = F)

# Matriz de datos de CPE
FichaEmisor_CIIU <- fread("FichaEmisor_CIIU.csv")

# OCTUBRE
# cabecera <- fread("FACT_2017_CAB.csv",
#                  select = c("COD_TIPOCOMP","NUM_RUC","NUM_SERIECPE","NUM_CPE","NUM_DOCIDENTI",
#                             "MTO_IMPCPE","MTO_IGVCPE","FEC_EMICPE"),
#                  colClasses=list(character=1:6))

cabecera <- fread("cabecera_mayo2.unl", 
                  sep = "|", 
                  select = c("V2", "V1", "V3", "V4", "V7", "V9", "V11", "V10"), 
                  colClasses=list(character=1:6))

names(cabecera) <- c("COD_TIPOCOMP","NUM_RUC","NUM_SERIECPE","NUM_CPE","NUM_DOCIDENTI", "MTO_IMPCPE","MTO_IGVCPE","FEC_EMICPE")

# Tabla FichaEmisor_CIIU con informacion de CPE-factura
MD_Factura <- inner_join(cabecera,FichaEmisor_CIIU,by = c("NUM_RUC"="ruc"))
rm(cabecera,FichaEmisor_CIIU)

MD_Factura$llave <- paste(MD_Factura$COD_TIPOCOMP, MD_Factura$NUM_RUC, MD_Factura$NUM_SERIECPE, MD_Factura$NUM_CPE, sep = "_")

# Limpieza de espacios en nro de documentos (RUCs)
MD_Factura$NUM_RUC <- stri_trim(MD_Factura$NUM_RUC)
MD_Factura$NUM_DOCIDENTI <- stri_trim(MD_Factura$NUM_DOCIDENTI)

# Filtros de control a cabecera (RUC: inicien con 1 o 2, con 11 caracteres)
MD_Factura <- MD_Factura %>% filter(nchar(NUM_DOCIDENTI)==11 & (stri_sub(NUM_DOCIDENTI,1,1)==1 | stri_sub(NUM_DOCIDENTI,1,1)==2))

# Creacion y transformacion de varaibles
# Creacion: Fechas - Dia Semana, Dia Mes
MD_Factura$FEC_EMICPE <- strptime(MD_Factura$FEC_EMICPE, "%Y-%m-%d %H:%M:%S")
MD_Factura$dia_semana <- format(MD_Factura$FEC_EMICPE,"%A")
MD_Factura$dia_mes    <- format(MD_Factura$FEC_EMICPE,"%e")
MD_Factura$FEC_EMICPE <- as.character(MD_Factura$FEC_EMICPE)

# Transformacion de montos a variables numericas
MD_Factura$MTO_IMPCPE <- as.numeric(MD_Factura$MTO_IMPCPE)
MD_Factura$MTO_IGVCPE <- as.numeric(MD_Factura$MTO_IGVCPE)

# Creacion de variable "Segmento del mes" 
MD_Factura$dia_mes <- as.numeric(MD_Factura$dia_mes)
MD_Factura$dia_mes_cat <- ifelse(MD_Factura$dia_mes<=3, "FEC_EMICPE.Inicio_mes[1-3]" , #En la ultima semana tratan de igualar los ingresos para disminuir su IGV
                                 ifelse(MD_Factura$dia_mes>=25, "FEC_EMICPE.Fin_mes[25-31]", #En la ultima semana tratan de igualar los ingresos para disminuir su IGV
                                        "FEC_EMICPE.Otros[4-24]"))

# Creacion de categoria del monto
MD_Factura$cut3_Monto <- cut(MD_Factura$MTO_IMPCPE,
                             breaks = c(0,50,100,150,200,400,600,700,800,900,1000,Inf))

# Listado de facturas para creacion de Target
RIESGO_TARGET <- fread("Riesgo_123_v3.csv",colClasses = list(character=1:4))
MD_Factura    <- left_join(MD_Factura,RIESGO_TARGET,by=c("NUM_RUC","NUM_SERIECPE","NUM_CPE"))
aux <- ifelse(is.na(MD_Factura$NewTarget),0,MD_Factura$NewTarget)
MD_Factura$NewTarget[is.na(MD_Factura$NewTarget)] <- 0
MD_Factura$NewTarget <-factor(MD_Factura$NewTarget) 

write.csv(MD_Factura,"MD_Factura_mayo.csv", row.names = F)

#-------3. Cargar Matriz de Datos (nivel Factura) - Unir facturas con detalle ---------
rm(list = ls())
memory.limit(size = 12000)
MatrizD <- fread("MD_Factura_mayo.csv")
MatrizD$COD_TIPOCOMP <- as.character(as.numeric(MatrizD$COD_TIPOCOMP))

cpes <- fread("cpes_limpio_mayo_18.csv", colClasses = list(character=1:5))

CPEjoin <- inner_join(cpes,MatrizD, by = c("COD_TIPOCOMP","NUM_RUC","NUM_SERIECPE","NUM_CPE"))  ### JOINN
rm(cpes)
CPEjoin <- subset(CPEjoin, DES_DETITM != "\\N")

# Agrupar DETALLE de factoras por CIIU 
CPEap <-  CPEjoin %>% group_by(act_principal) %>% summarise(Nitems = n(), 
                                                            Descripcion = paste(DES_DETITM, collapse = " "))

#-------4. Palabras comunes x CIIU (Rubro) -----
ActPrinY <- as.data.frame(table(unique(CPEjoin$act_principal)))[,1]

library(tidytext)
library(tidyverse)
datosT <- NULL
takenT <- NULL
Freqs  <- list()

# Frecuencia de palabras
for (i in 1:length(ActPrinY)) {
  # i=1
  datosT     <- data_frame(txt=CPEap$Descripcion[i])
  takenT     <- datosT %>% unnest_tokens(word, txt)
  Freqs[[i]]     <- as.data.frame(takenT %>% count(word,sort = T) )
  Freqs[[i]]$nch <- nchar(Freqs[[i]]$word)
  Freqs[[i]]$APY <- ActPrinY[i]
  Freqs[[i]]     <- Freqs[[i]][Freqs[[i]]$nch > 3,]
  Freqs[[i]]     <- cbind(Freqs[[i]], Relativa= Freqs[[i]]$n/sum(Freqs[[i]]$n), Acumulada= cumsum(Freqs[[i]]$n/sum(Freqs[[i]]$n)))
}

# Selección del 60% de palabras más frecuentes x CIIU 
Words <- NULL
for (i in 1:length(ActPrinY)) {
  # i=1
  aux <- Freqs[[i]]
  aux <- aux %>% filter(Acumulada <= 0.75)
  aux <- aux[,c("word", "APY")]
  names(aux) <- c("cwords", "APY")
  # Words[[i]] <- aux
  Words <- bind_rows(Words, aux)
}

write.csv(Words, "Words_mayo_2018.csv", row.names = F)


#-------5. Palabras x RECEPTOR (Adquiriente) -----------
auxiliar <- CPEjoin %>% filter(substr(NUM_DOCIDENTI,1,1)=="1" | substr(NUM_DOCIDENTI,1,1)=="2")
auxiliar$NUM_DOCIDENTI <- gsub("[[:punct:]]+","", auxiliar$NUM_DOCIDENTI)
auxiliar <- auxiliar %>% filter(nchar(NUM_DOCIDENTI)==11)

Receptor <- auxiliar %>% 
  group_by(NUM_DOCIDENTI, razon_social,act_principal) %>%
  summarise(Nitems = n(), Descripcion = paste(DES_DETITM, collapse = " "))

Receptor$Descripcion <- stri_trim(Receptor$Descripcion)
Receptor <- Receptor[Receptor$Descripcion != "\\N",]

datosR     <-  NULL
tokenR     <-  NULL
palabrasR  <-  list()

for (i in 1:length(Receptor$Descripcion)) {
  # i <- 2
  datosR         <- data_frame(txt=Receptor$Descripcion[i])
  tokenR         <- datosR %>% unnest_tokens(word, txt)
  palabrasR[[i]] <- as.data.frame(tokenR %>% count(word,sort = T))
  palabrasR[[i]]$ruc <- Receptor$NUM_DOCIDENTI[i]
  palabrasR[[i]]$apy <- Receptor$act_principal[i]
  palabrasR[[i]]$rs  <- Receptor$razon_social[i]
  # print(i)
}


#---------- 8.1 Palabras x Factura (Adquiriente) -----------
Llaves <-
  CPEjoin %>%
  group_by(llave,act_principal) %>%
  summarise(Nitems = n(), Descripcion = paste(DES_DETITM, collapse = " "))

Llaves$Descripcion <- trimws(Llaves$Descripcion)
Llaves$Descripcion <- trimws(Llaves$Descripcion, "l")
Llaves$Descripcion <- trimws(Llaves$Descripcion, "r")
Llaves <- Llaves[Llaves$Descripcion != "",]

length(Llaves$Descripcion)

datosR     <-  NULL
tokenR     <-  NULL
palabras_llave_R  <-  list()

for (i in 1:length(Llaves$Descripcion)) {
  # i <- 12478
  datosR         <- data_frame(txt=Llaves$Descripcion[i])
  tokenR         <- datosR %>% unnest_tokens(word, txt)
  palabras_llave_R[[i]] <- as.data.frame(tokenR %>% count(word,sort = T))
  palabras_llave_R[[i]]$llave <- Llaves$llave[i]
  palabras_llave_R[[i]]$apy <- Llaves$act_principal[i]
  print(i)
}

#--------- 9. Proporción de DIFERENCIAS (palabrasRECEPTOR vs palabrasCIIU) --------
# library(miceadds)

#load.Rdata(filename = "palabrasRapF4.RData", objname =  "palabrasRF")
Pr <- do.call(rbind.data.frame, palabrasR)
Wr <- do.call(rbind.data.frame, Words)
Wr$cwords <- as.character(Wr$cwords)
Wr$APY <- as.character(Wr$APY)
Wr$control <- 1

PrWr <- left_join(Pr,Wr,by=c("word"="cwords","apy"="APY"))
PrWr$control[is.na(PrWr$control)] <- 0

ReceptorR <- PrWr %>% group_by(ruc) %>% summarise(N=n(), Suma = sum(control), PropCom = Suma/n(), PropDif = (n()-Suma)/n() )
ReceptorR <- as.data.frame(ReceptorR)
# ggplot(ReceptorR, aes(x=PropDif)) + geom_histogram()
head(ReceptorR)

write.csv(ReceptorR, file = "ReceptorR.csv", row.names = F)


#--------- 9.1 Proporción de DIFERENCIAS (palabrasRECEPTOR vs palabrasCIIU) --------
# library(miceadds)

#load.Rdata(filename = "palabrasRapF4.RData", objname =  "palabrasRF")
P_ll_r <- do.call(rbind.data.frame, palabras_llave_R)
Wr <- do.call(rbind.data.frame, Words)
Wr$cwords <- as.character(Wr$cwords)
Wr$APY <- as.character(Wr$APY)
Wr$control <- 1

PrWr <- left_join(P_ll_r,Wr,by=c("word"="cwords","apy"="APY"))
PrWr$control[is.na(PrWr$control)] <- 0

LlavesR <- PrWr %>% group_by(llave) %>% summarise(N=n(), Suma = sum(control), PropCom = Suma/n(), PropDif = (n()-Suma)/n() )
LlavesR <- as.data.frame(LlavesR)
# ggplot(ReceptorR, aes(x=PropDif)) + geom_histogram()

write.csv(LlavesR, file = "LlavesR.csv", row.names = F)

######################################################################################################
# Matriz de receptores
######################################################################################################
#Adicion de variables principales a nivel de contribuyente
rm(list = ls())
MD_Factura <- fread("MD_Factura.csv")

MD_Receptores <- MD_Factura %>% group_by(NUM_DOCIDENTI) %>% summarise()
MD_Receptores$NUM_DOCIDENTI <- trimws(MD_Receptores$NUM_DOCIDENTI)
MD_Receptores$NUM_DOCIDENTI <- gsub("[[:cntrl:]]", "", MD_Receptores$NUM_DOCIDENTI)
MD_Receptores$NUM_DOCIDENTI <- gsub("[[:punct:]]+","", MD_Receptores$NUM_DOCIDENTI)
MD_Receptores$NUM_DOCIDENTI <- gsub(" ","", MD_Receptores$NUM_DOCIDENTI)
MD_Receptores <- MD_Receptores %>% filter(nchar(MD_Receptores$NUM_DOCIDENTI)==11)

Ficha_Nacional <- fread("ficha_nacional.csv")
MD_Receptores <- left_join(MD_Receptores,Ficha_Nacional,by = c("NUM_DOCIDENTI"="ruc"))
rm(Ficha_Nacional)

#Creacion de variables resumidas a nivel de receptores
###########################################################################
# c01 <- dcast(MD_Factura,NUM_DOCIDENTI~ruc_segmentos.nom_segfisca,fun=mean,fill=0,value.var ="MTO_IMPCPE")#Segmento RUC
c02 <- dcast(MD_Factura,NUM_DOCIDENTI~cod_act_princ,fun=mean,fill=0,value.var ="MTO_IMPCPE")#Actividad Economica Emisor
c03 <- dcast(MD_Factura,NUM_DOCIDENTI~dia_semana, fun=mean,fill=0,value.var ="MTO_IMPCPE")#D????a de semana 
c04 <- dcast(MD_Factura,NUM_DOCIDENTI~dia_mes_cat, fun=mean,fill=0,value.var ="MTO_IMPCPE")#Categor????a d????a de mes
# c05 <- dcast(MD_Factura,NUM_DOCIDENTI~Departamento_similar, fun=mean,fill=0,value.var ="MTO_IMPCPE")#Departamento similar
c06 <- dcast(MD_Factura,NUM_DOCIDENTI~cut3_Monto, fun=length,fill=0,value.var ="MTO_IMPCPE")#Categor????a monto
c07 <- data.table(MD_Factura %>% group_by(NUM_DOCIDENTI) %>% summarise(Fac_compra = n()))
c08 <- data.table(MD_Factura %>% group_by(NUM_DOCIDENTI) %>% summarise(Precio_Venta = sum(MTO_IMPCPE),IGV_Registrado = sum(MTO_IGVCPE)))
c09 <- MD_Factura %>% group_by(NUM_DOCIDENTI,NewTarget) %>% summarise(n = n()) %>% spread(NewTarget,n) 
c10 <- dcast(MD_Factura,NUM_DOCIDENTI~Actividad_Economica, fun=mean,fill=0,value.var ="MTO_IMPCPE")#Categor????a monto

#Creacion de variable c09 que contiene el target (el contribuyente recibio facturas con productos o servicios relacionados a gastos personales o familiares)
###########################################################################
c09 <- data.frame(c09 %>% replace_na(list('0' = 0, '3' = 0)))
c09$NewTarget <- (c09$X3)/((c09$X0) + (c09$X3))
c09$NewTarget_Cat[c09$NewTarget>0] <- "si"
c09$NewTarget_Cat[c09$NewTarget==0] <- "no"
c09$NewTarget_Cat <- as.factor(c09$NewTarget_Cat)

MD_Receptores$X0 <- NULL
MD_Receptores$X3 <- NULL
MD_Receptores$NewTarget_Cat <- NULL
#Adicion de variables calculadas c01-c10 a matriz principal de receptores
###########################################################################
# MD_Receptores<-left_join(MD_Receptores, c01, by=c("NUM_DOCIDENTI"))
MD_Receptores<-left_join(MD_Receptores, c02, by=c("NUM_DOCIDENTI"))
MD_Receptores<-left_join(MD_Receptores, c03, by=c("NUM_DOCIDENTI"))
MD_Receptores<-left_join(MD_Receptores, c04, by=c("NUM_DOCIDENTI"))
# MD_Receptores<-left_join(MD_Receptores, c05, by=c("NUM_DOCIDENTI"))
MD_Receptores<-left_join(MD_Receptores, c06, by=c("NUM_DOCIDENTI"))
MD_Receptores<-left_join(MD_Receptores, c07, by=c("NUM_DOCIDENTI"))
MD_Receptores<-left_join(MD_Receptores, c08, by=c("NUM_DOCIDENTI"))
MD_Receptores<-left_join(MD_Receptores, c09, by=c("NUM_DOCIDENTI"))
MD_Receptores<-left_join(MD_Receptores, c10, by=c("NUM_DOCIDENTI"))
# rm(c01,c02,c03,c04,c05,c06,c07,c08,c09,c10)
rm(c02,c03,c04,c06,c07,c08,c09,c10)


######################################################################################################
#Creacion de Matriz de datos FINAL
######################################################################################################
MD_FINAL <- MD_Receptores %>% select(NUM_DOCIDENTI,cod_act_princ,comercio_ext,facturacion,cond_domicilio,ubigeo,`55104`,`55205`,`63040`,`85111`,`85124`,`85193`,`92123`,`92149`,`92192`,`92413`,`92495`,domingo,jueves,lunes,martes,`miércoles`,`sábado`,viernes,`FEC_EMICPE.Fin_mes[25-31]`,`FEC_EMICPE.Inicio_mes[1-3]`,`FEC_EMICPE.Otros[4-24]`,`(0,50]`,`(50,100]`,`(100,150]`,`(150,200]`,`(200,400]`,`(400,600]`,`(600,700]`,`(700,800]`,`(800,900]`,`(900,1e+03]`,Fac_compra,Precio_Venta,IGV_Registrado,NewTarget_Cat,MODA,ENTRETENIMIENTO)

#Modificacion de nombres a variables
###########################################################################
nombres <- c("RUC", "C_ACT_PRINC", "C_COMERCIO_EXT", "C_TIPO_FACTURACION", "C_CONDICION_DOMICILIO", 
             "C_UBIGEO", "M_AE_HOTELES", "M_AE_VIAJES", "M_AE_RESTAURANTES", "M_AE_HOSPITALES", 
             "M_AE_MEDICOS", "M_AE_SALUDHUMANA", "M_AE_FILMESVIDEOS", "M_AE_TEATRO", 
             "M_AE_ENTRETENIMIENTO", "M_AE_DEPORTIVO", "M_AE_ESPARCIMIENTO", "M_DSE_DOMINGO", 
             "M_DSE_JUEVES", "M_DSE_LUNES", "M_DSE_MARTES", "M_DSE_MIERCOLES", "M_DSE_SABADO", 
             "M_DSE_VIERNES", "M_DME_2531", "M_DME_0103", "M_DME_0424",  
             "F_000_050", "F_050_100", "F_100_150", "F_150_200", "F_200_400", 
             "F_400_600", "F_600_700", "F_700_800", "F_800_900", "F_900_1000", "F_FACTURAS", 
             "M_PRECIOVENTA", "M_IGVREGISTRADO", "NewTarget_Cat", "MODA", "ENTRETENIMIENTO")
names(MD_FINAL) <- nombres
write.csv(MD_FINAL, "MD_FINAL_MAYO.csv", row.names = F)

#Transformacion de variables UBIGEO y CODIGO DE ACTIVIDAD PRINCIPAL
###########################################################################
MD_FINAL$C_UBIGEO <- stri_sub(MD_FINAL$C_UBIGEO,1,2)
MD_FINAL$C_UBIGEO <- as.factor(MD_FINAL$C_UBIGEO)

MD_FINAL$C_ACT_PRINC <- stri_sub(MD_FINAL$C_ACT_PRINC,1,2)
MD_FINAL$C_ACT_PRINC[is.na(MD_FINAL$C_ACT_PRINC)] <- "00"
MD_FINAL$C_ACT_PRINC <- as.factor(MD_FINAL$C_ACT_PRINC)

#Creacion de variable de Riesgo Combinado 
###########################################################################
MD_FINAL$A1[MD_FINAL$M_AE_HOTELES>0] <- 1
MD_FINAL$A2[MD_FINAL$M_AE_VIAJES>0] <- 1
MD_FINAL$A3[MD_FINAL$M_AE_RESTAURANTES>0] <- 1
MD_FINAL$A4[MD_FINAL$M_AE_HOSPITALES>0] <- 1
MD_FINAL$A5[MD_FINAL$M_AE_MEDICOS>0] <- 1
MD_FINAL$A6[MD_FINAL$M_AE_SALUDHUMANA>0] <- 1
MD_FINAL$A7[MD_FINAL$M_AE_FILMESVIDEOS>0] <- 1
MD_FINAL$A8[MD_FINAL$M_AE_TEATRO>0] <- 1
MD_FINAL$A9[MD_FINAL$M_AE_ENTRETENIMIENTO>0] <- 1
MD_FINAL$A10[MD_FINAL$M_AE_DEPORTIVO>0] <- 1
MD_FINAL$A11[MD_FINAL$M_AE_ESPARCIMIENTO>0] <- 1
MD_FINAL$A12[MD_FINAL$MODA>0] <- 1
MD_FINAL$A13[MD_FINAL$ENTRETENIMIENTO>0] <- 1

MD_FINAL$A1[is.na(MD_FINAL$A1)] <- 0
MD_FINAL$A2[is.na(MD_FINAL$A2)] <- 0
MD_FINAL$A3[is.na(MD_FINAL$A3)] <- 0
MD_FINAL$A4[is.na(MD_FINAL$A4)] <- 0
MD_FINAL$A5[is.na(MD_FINAL$A5)] <- 0
MD_FINAL$A6[is.na(MD_FINAL$A6)] <- 0
MD_FINAL$A7[is.na(MD_FINAL$A7)] <- 0
MD_FINAL$A8[is.na(MD_FINAL$A8)] <- 0
MD_FINAL$A9[is.na(MD_FINAL$A9)] <- 0
MD_FINAL$A10[is.na(MD_FINAL$A10)] <- 0
MD_FINAL$A11[is.na(MD_FINAL$A11)] <- 0
MD_FINAL$A12[is.na(MD_FINAL$A12)] <- 0
MD_FINAL$A13[is.na(MD_FINAL$A13)] <- 0
MD_FINAL$R_COMBINADO_AE <- apply(MD_FINAL[,44:56],1,sum)

######################################################################################################
#Adicion de indicadores a la Matriz de Datos Final
######################################################################################################

#Adicion de variables indicadores
###########################################################################
Palabras_raras <- fread("ReceptorR.csv",colClasses = list(character=1))
Palabras_raras <- Palabras_raras %>% select(ruc,PropDif)

Frecuencia_IGV <- fread("PDT_FrecIGV.csv",colClasses = list(character=1:2))
Frecuencia_IGV <- Frecuencia_IGV %>% select(n_doc_declado,Frec_IGV)

MD_FINAL <- left_join(MD_FINAL,Palabras_raras,by=c("RUC"="ruc"))
MD_FINAL <- left_join(MD_FINAL,Frecuencia_IGV,by=c("RUC"="n_doc_declado"))

MD_FINAL$PropDif[is.na(MD_FINAL$PropDif)] <- 0
MD_FINAL$Frec_IGV[is.na(MD_FINAL$Frec_IGV)] <- 0
# MD_FINAL$Rest[is.na(MD_FINAL$Rest)] <- 0
# MD_FINAL$Prom_Trabajadores[is.na(MD_FINAL$Prom_Trabajadores)] <- 0
# MD_FINAL$Prom_Prestad_Servicio[is.na(MD_FINAL$Prom_Prestad_Servicio)] <- 0

rm(Palabras_raras,Frecuencia_IGV)

#Correccion de inconsistencias en Matriz de Datos Final
###########################################################################
MD_FINAL$C_COMERCIO_EXT[-is.na(MD_FINAL$C_COMERCIO_EXT) | MD_FINAL$C_COMERCIO_EXT==""] <- "BLANCO"
MD_FINAL$C_TIPO_FACTURACION[is.na(MD_FINAL$C_TIPO_FACTURACION) | MD_FINAL$C_TIPO_FACTURACION==""] <- "BLANCO"
MD_FINAL$C_CONDICION_DOMICILIO[is.na(MD_FINAL$C_CONDICION_DOMICILIO) | MD_FINAL$C_CONDICION_DOMICILIO==" "] <- "BLANCO"
# MD_FINAL$C_SEGMENTO_RUC[is.na(MD_FINAL$C_SEGMENTO_RUC)] <- "BLANCO"
# MD_FINAL$F_ACT_ECON_REGISTRADO[is.na(MD_FINAL$F_ACT_ECON_REGISTRADO)] <- 0
levels(MD_FINAL$C_UBIGEO)[26] <- "00"
MD_FINAL$C_UBIGEO[is.na(MD_FINAL$C_UBIGEO)] <- "00"
MD_FINAL$C_COMERCIO_EXT <- factor(MD_FINAL$C_COMERCIO_EXT)
MD_FINAL$C_TIPO_FACTURACION <- factor(MD_FINAL$C_TIPO_FACTURACION)
MD_FINAL$C_CONDICION_DOMICILIO <- factor(MD_FINAL$C_CONDICION_DOMICILIO)
# MD_FINAL$C_SEGMENTO_RUC <- factor(MD_FINAL$C_SEGMENTO_RUC)

write.csv(MD_FINAL,"MD_FINAL_MAYO.csv",row.names = F)


# I. Filtros del negocio:
######################
# 1. Retirar PN_S/N
#Facturas_1<-subset(Facturas_1, tipo_emp.y!="PERSONA NATURAL siN NEGOCIO")

#3. Trabajar Receptor y: HABIDO / Emisor: HABIDO y ACTIVO
#Facturas_1<-subset(Facturas_1,condicion.y=="HABIDO" & condicion.x=="HABIDO" & estado.x=="ACTIVO")


######################################################################################################
#Creacion de datos de entrenamiento y prueba
######################################################################################################
Tn <- length(MD_FINAL$NewTarget_Cat[MD_FINAL$NewTarget_Cat=="no"]) #155709
Ts <- length(MD_FINAL$NewTarget_Cat[MD_FINAL$NewTarget_Cat=="si"]) #3669

set.seed(216)
indexes <-  sample(1:Tn, size=Ts) #14

Test.MD_FINAL  <- filter(MD_FINAL,NewTarget_Cat=="no")[-indexes,] #152040
Train.MD_FINAL <- filter(MD_FINAL,NewTarget_Cat=="no")[indexes,]  #3669
Train.MD_FINAL <- bind_rows(Train.MD_FINAL,data.frame(filter(MD_FINAL,NewTarget_Cat=="si"))) #7338

Varibles_modelo <- c("C_COMERCIO_EXT","C_TIPO_FACTURACION","C_CONDICION_DOMICILIO","C_UBIGEO",
                     "M_AE_HOTELES","M_AE_VIAJES","M_AE_RESTAURANTES","M_AE_HOSPITALES",
                     "M_AE_MEDICOS", "M_AE_SALUDHUMANA","M_AE_FILMESVIDEOS","M_AE_TEATRO",
                     "M_AE_ENTRETENIMIENTO","M_AE_DEPORTIVO","M_AE_ESPARCIMIENTO","MODA",
                     "ENTRETENIMIENTO","M_DSE_DOMINGO","M_DSE_JUEVES","M_DSE_LUNES","M_DSE_MARTES",
                     "M_DSE_MIERCOLES","M_DSE_SABADO","M_DSE_VIERNES","M_DME_2531","M_DME_0103",
                     "M_DME_0424","F_000_050","F_050_100","F_100_150",
                     "F_150_200","F_200_400","F_400_600","F_600_700","F_700_800","F_800_900","F_900_1000",
                     "F_FACTURAS","M_PRECIOVENTA","M_IGVREGISTRADO",
                     "R_COMBINADO_AE","NewTarget_Cat","PropDif","Frec_IGV")

Train.MD_FINALF <- Train.MD_FINAL[,c("RUC",Varibles_modelo)]
Test.MD_FINALF  <- Test.MD_FINAL[ ,c("RUC",Varibles_modelo)]

write.csv(Train.MD_FINALF,"Train_MD_FINALF.csv", row.names = F)
write.csv(Test.MD_FINALF,"Test_MD_FINALF_MAYO.csv", row.names = F)


####### MODELO #####
rm(list=ls())

library(data.table)
library(Matrix)
library(randomForest)
library(rpart)


######################################################################################################
### Construcci?n (con data de 1 a?o)
######################################################################################################
######################################################################################################
Train.MD_FINAL <- fread("Train_MD_FINALF.csv", header = T, stringsAsFactors = T)
Train.MD_FINAL <- as.data.frame(Train.MD_FINAL)

# variables <- c("NewTarget_Cat", "R_COMBINADO_AE", "M_AE_FILMESVIDEOS", "M_AE_DEPORTIVO", 
#                "Frec_IGV", "MODA", "M_DSE_DOMINGO", "M_IGVREGISTRADO", "M_AE_VIAJES", 
#                "M_AE_ENTRETENIMIENTO", "M_AE_HOTELES")
# Train.MD_FINAL <- Train.MD_FINAL[,variables]

variables <- c("M_AE_FILMESVIDEOS", "R_COMBINADO_AE", "M_IGVREGISTRADO", "M_PRECIOVENTA", "MODA", "Frec_IGV", 
               "F_FACTURAS", "M_DSE_DOMINGO", "PropDif", "M_DME_0424", "C_UBIGEO", "M_AE_VIAJES", "M_DSE_SABADO", 
               "M_DME_2531", "M_AE_DEPORTIVO", "NewTarget_Cat")

Train.MD_FINAL <- Train.MD_FINAL[,variables]

bosque <- randomForest(NewTarget_Cat ~ ., data = Train.MD_FINAL, ntree=10, mtry= 3)

save(bosque, file = "gnd_scoring2.rda")

######################################################################################################
### Predicci?n (con data nueva de 1 mes o 1 semana)
######################################################################################################
######################################################################################################
Test.MD_FINAL  <- fread("Test_MD_FINALF_MAYO.csv",  header = T, stringsAsFactors = T)

pred               <- predict(bosque,Test.MD_FINAL, type = "prob")
Test.MD_FINAL$pred <- pred[,2]

library(dplyr)
Test.MD_FINAL      <- Test.MD_FINAL %>% arrange(desc(pred))
OUTPUT             <- as.data.frame(Test.MD_FINAL[,c("RUC","pred")])
write.csv(OUTPUT,"OUTPUT_GPF_MAYO.csv")



aux <- ifelse(pred[,2]>=0.7,1,0)
table(aux)



### PRUEBA DE LOS MODELOS ###
rm(list = ls())
library(randomForest)
library(rpart)
library(caret)
library(dplyr)
library(data.table)
library(nnet)

datos <- fread("Train_MD_FINALF.csv", header = T, stringsAsFactors = T)
datos <- as.data.frame(datos)

str(datos)

table(datos$NewTarget_Cat)

arbol <- rpart(NewTarget_Cat ~ ., data=datos[,-1], method = "class")
variables1 <- row.names(as.data.frame(arbol$variable.importance))[1:15]

bosque <- randomForest(NewTarget_Cat ~ ., data=datos[,-c(1,3)])
variables <- as.data.frame(importance(bosque))
variables$variable <- row.names(variables)
variables <- arrange(variables, -MeanDecreaseGini)
variables <- variables[1:15,2]


datos_final <- datos[,c("RUC", variables, "NewTarget_Cat")]

# Creamos los grupos(K-Folds), separa a la data en 10 grupos diferentes
set.seed(1) # Prefijamos una semilla para tener resultados reproducibles
folds <- 10 # 10 Grupos
datos_final$kfold <- sample(1:folds, nrow(datos_final), replace = T)
sensibilidad <- data.frame(NULL)
gini <- data.frame(NULL)
for (i in 1:folds){
  #i=1
  test <- subset(datos_final, kfold == i)
  train <- subset(datos_final, !kfold == i)
  # REGREsiON LOGISTICA
  logistica <- glm(NewTarget_Cat ~ .,data = train[-which(names(train) %in% c("RUC", "kfold"))],family=binomial(link="logit"))
  logist_pred <- predict(logistica, test, type = "response") # Predicción sobre test
  
  logist_pred_clase <- factor(ifelse(logist_pred>0.4,1,0))
  levels(logist_pred_clase) <- c("no","si")
  # Matriz de confusión
  table(logist_pred_clase,test$NewTarget_Cat)
  confusionMatrix(logist_pred_clase,test$NewTarget_Cat,positive="si")
  Sens_logis <- confusionMatrix(logist_pred_clase,test$NewTarget_Cat,positive="si")$byClass[1]
  auc_logist <- confusionMatrix(logist_pred_clase,test$NewTarget_Cat,positive="si")$overall[1]
  gini_logist <- 2*auc_logist-1
  
  
  # BOSQUE ALEATORIO
  bosque <- randomForest(NewTarget_Cat ~ ., data = train[-which(names(train) %in% c("RUC", "kfold"))], ntree=10, mtry= 3)
  bosque_pred <- predict(bosque, test, type = "prob") # Predicción sobre test
  bosque_pred_class <- factor(ifelse(bosque_pred[,2] >=0.4,"si", "no"))
  # Matriz de confusión
  table(bosque_pred_class,test$NewTarget_Cat)
  confusionMatrix(bosque_pred_class,test$NewTarget_Cat,positive="si")
  Sens_BA <- confusionMatrix(bosque_pred_class,test$NewTarget_Cat,positive="si")$byClass[1]
  auc_BA <- confusionMatrix(bosque_pred_class,test$NewTarget_Cat,positive="si")$overall[1]
  gini_BA <- 2*auc_BA-1
  
  
  # ARBOL DE CLAsiFICACION
  arbol <- rpart(NewTarget_Cat ~ ., data = train[-which(names(train) %in% c("RUC", "kfold"))], method = "class")
  arb_pred <- predict(arbol,test, type = "class")
  # Matriz de confusión
  table(arb_pred,test$NewTarget_Cat)
  confusionMatrix(arb_pred,test$NewTarget_Cat,positive="si")
  Sens_AC <- confusionMatrix(arb_pred,test$NewTarget_Cat,positive="si")$byClass[1]
  auc_AC <- confusionMatrix(arb_pred,test$NewTarget_Cat,positive="si")$overall[1]
  gini_AC <- 2*auc_AC-1
  
  # REDES NEURONALES
  red_neu <- nnet(NewTarget_Cat~., data = train[-which(names(train) %in% c("RUC", "kfold"))],size = 5, rang = 0.1,decay = 5e-4, maxit = 1000)
  red_pred <- predict(red_neu,test)
  red_pred_clase <- factor(ifelse(red_pred>=0.5,1,0))
  levels(red_pred_clase) <- c("no","si")
  # Matriz de confusión
  table(red_pred_clase,test$NewTarget_Cat)
  confusionMatrix(red_pred_clase,test$NewTarget_Cat,positive="si")
  Sens_RN <- confusionMatrix(red_pred_clase,test$NewTarget_Cat,positive="si")$byClass[1]
  auc_RN <- confusionMatrix(red_pred_clase,test$NewTarget_Cat,positive="si")$overall[1]
  gini_RN <- 2*auc_RN-1
  sensibilidad <- rbind(sensibilidad, data.frame(iteracion = i, Logistica = Sens_logis,
                                                 BosqueAleatorio = Sens_BA,ArbolClasificacion = Sens_AC, RedNeuronal = Sens_RN))
  gini <- rbind(gini, data.frame(iteracion = i, Logistica = auc_logist, BosqueAleatorio =
                                   auc_BA,ArbolClasificacion = auc_AC, RedNeuronal = auc_RN))
  print(paste("Fin de Iteración",i))
}

sensibilidad <- round(sensibilidad,2)
row.names(sensibilidad) <- NULL
gini <- round(gini,2)
row.names(gini) <- NULL
# Medida de precisión Sensibilidad
sensibilidad_final <- apply(sensibilidad[,-1],2,mean)
windows()
plot(sensibilidad[,c(1,2)], type = "l", ylim = c(0,1), main = "Evolución de la sensibilidad para
     todos los modelos",
     cex.axis = .7,cex.lab = .7,cex.main = .8,
     xlab ="No. de Iteraciones", ylab="Sensibilidad")
lines(sensibilidad$BosqueAleatorio, col="green",pch=22)
lines(sensibilidad$ArbolClasificacion, col="red")
lines(sensibilidad$RedNeuronal, col="blue")
abline(h = sensibilidad_final[1], col = "black", lty = 2)
abline(h = sensibilidad_final[2], col = "green", lty = 2)
abline(h = sensibilidad_final[3], col = "red", lty = 2)
abline(h = sensibilidad_final[4], col = "blue", lty = 2)
legend("bottomright", ncol=4,c("Logistica","BosqueAleatorio","Arbol de clasificacion", "Red
                               Neuronal"),
       cex=0.6,bty="n",fill=c("black","green","red","blue"))
# Medida de precisión Gini
gini_final <- apply(gini[,-1],2,mean)
windows()
plot(gini[,c(1,2)], type = "l", ylim = c(0,1), main = "Evolución de Gini para todos los modelos",
     cex.axis = .7,cex.lab = .7,cex.main = .8,
     xlab ="No. de Iteraciones", ylab="Indicador Gini")
lines(gini$BosqueAleatorio, col="green",pch=22)
lines(gini$ArbolClasificacion, col="red")
lines(gini$RedNeuronal, col="blue")
abline(h = gini_final[1], col = "black", lty = 2)
abline(h = gini_final[2], col = "green", lty = 2)
abline(h = gini_final[3], col = "red", lty = 2)
abline(h = gini_final[4], col = "blue", lty = 2)
legend("bottomright", ncol=4,c("Logistica","BosqueAleatorio","Arbol de clasificacion", "Red
                               Neuronal"), cex=0.6,bty="n",fill=c("black","green","red","blue"))
