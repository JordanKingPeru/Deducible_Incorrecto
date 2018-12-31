######################################################################################################
#Cargar librerias
######################################################################################################
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
library(DiagrammeR)

memory.limit(size = 10000) #Mejora el rendimiento de la memoria RAM
######################################################################################################
# 01. Selección de RUC o Actividad Económica
######################################################################################################
Ficha_Nacional <- fread("01.BD/1FN/ficha_nacional.csv",select = c("nom_comercial","cod_act_princ", "act_principal","ruc","razon_social","comercio_ext","facturacion","condicion","estado","tipo_emp","tamano","cond_domicilio","ubigeo","act_secun1","act_secun2"),colClasses=list(character=1:6))
bdCIIU_nc <- fread("01.BD/1FN/MaestroCIIU V4.0.csv",select = c("cod_act_princ","CATEGO_OBJETIVO","Actividad_Economica"),colClasses = list(character=1:3))
RUC_Seleccionado <- fread("01.BD/1FN/RUC_GOV_CONSOLIDADO.csv",select = c("ruc","CATEGO_OBJETIVO","Actividad_Economica"),colClasses = list(character=1:3))


    # Creación de variable cantidad de actividades económicas registradas en SUNAT
    ###########################################################################
    Ficha_Nacional$cant_act_1[Ficha_Nacional$act_secun1==""] <- 0
    Ficha_Nacional$cant_act_1[is.na(Ficha_Nacional$cant_act_1)] <- 1
    
    Ficha_Nacional$cant_act_2[Ficha_Nacional$act_secun2==""] <- 0
    Ficha_Nacional$cant_act_2[is.na(Ficha_Nacional$cant_act_2)] <- 1
    
    Ficha_Nacional$cant_act <- 1 + as.numeric(Ficha_Nacional$cant_act_1) + as.numeric(Ficha_Nacional$cant_act_2)

    # Limpieza de caracteres de programación
    ###########################################################################
    Ficha_Nacional$act_principal <- stri_replace_all_regex(Ficha_Nacional$act_principal,"\r","")
    
    # Creación de tabla FichaEmisor_CIIU
    ###########################################################################
    P1 <- inner_join(Ficha_Nacional,filter(bdCIIU_nc,CATEGO_OBJETIVO==1),by=c("cod_act_princ"))
    P2 <- inner_join(Ficha_Nacional,RUC_Seleccionado,by=c("ruc"))

    FichaEmisor_CIIU <- unique(bind_rows(P1,P2))
    
    # Eliminación de objetos temporales
    ###########################################################################
    rm(P1,P2,bdCIIU_nc,RUC_Seleccionado)


######################################################################################################
# 02.Matriz de datos de CPE
######################################################################################################
CPE_RUC <- fread("01.BD/2CPE/FACT_2017_CAB.csv",select = c("COD_TIPOCOMP","NUM_RUC","NUM_SERIECPE","NUM_CPE","NUM_DOCIDENTI","MTO_IMPCPE","MTO_IGVCPE","FEC_EMICPE"),colClasses=list(character=1:6))

    #Limpieza de espacios
    ###########################################################################
    CPE_RUC$NUM_RUC <- stri_trim(CPE_RUC$NUM_RUC)
    CPE_RUC$NUM_RUC <- stri_replace_all_regex(CPE_RUC$NUM_RUC,"\n","")
    CPE_RUC$NUM_DOCIDENTI <- stri_trim(CPE_RUC$NUM_DOCIDENTI)
    #Eliminación de caracteres de programación \n \r
    CPE_RUC$NUM_DOCIDENTI <- stri_replace_all_regex(CPE_RUC$NUM_DOCIDENTI,"\n","")
    CPE_RUC$NUM_DOCIDENTI <- as.numeric(CPE_RUC$NUM_DOCIDENTI)
    CPE_RUC$NUM_DOCIDENTI <- as.character(CPE_RUC$NUM_DOCIDENTI)

    #Aplicación de filtros de control a CPE_RUC 
    ###########################################################################
    CPE_RUC <- CPE_RUC %>% filter(nchar(NUM_DOCIDENTI)==11 & (stri_sub(NUM_DOCIDENTI,1,1)==1 |stri_sub(NUM_DOCIDENTI,1,1)==2))

    # Creación de tabla FichaEmisor_CIIU
    ###########################################################################
    MD_Factura <- inner_join(CPE_RUC,FichaEmisor_CIIU,by = c("NUM_RUC"="ruc"))

    # Eliminación de objetos temporales
    ###########################################################################
    rm(CPE_RUC,FichaEmisor_CIIU)


######################################################################################################
#Creación de varaibles
######################################################################################################
    #Elaboración de fechas
    ###########################################################################
    MD_Factura$FEC_EMICPE <- strptime(MD_Factura$FEC_EMICPE, "%Y-%m-%d %H:%M:%S")
    MD_Factura$dia_semana <- format(MD_Factura$FEC_EMICPE,"%A")
    MD_Factura$dia_mes <- format(MD_Factura$FEC_EMICPE,"%e")
    MD_Factura$FEC_EMICPE <- as.character(MD_Factura$FEC_EMICPE)

    #Transformar los montos a variables numéricas
    ###########################################################################
    MD_Factura$MTO_IMPCPE <- as.numeric(MD_Factura$MTO_IMPCPE)
    MD_Factura$MTO_IGVCPE <- as.numeric(MD_Factura$MTO_IGVCPE)

    #Creación de variables ubigeo
    ###########################################################################
    MD_Factura <- left_join(MD_Factura,select(Ficha_Nacional,c(ruc,ubigeo)),by=c("NUM_DOCIDENTI"="ruc"))
    MD_Factura$Departamento_similar[stri_sub(as.character(MD_Factura$ubigeo.x),1,2)==stri_sub(as.character(MD_Factura$ubigeo.y),1,2)] <- "Igual Departamento"
    MD_Factura$Departamento_similar[is.na(MD_Factura$Departamento_similar)] <- "Diferente Departamento"

    #Cargar datos de RUC SEGMENTOS
    ###########################################################################
    RUC_SEGMENTO<-fread("01.BD/3RUCSEGMENTO/ruc_segmentos.csv",colClasses = list(character=1:2))
    MD_Factura <- left_join(MD_Factura,RUC_SEGMENTO,by = c("NUM_RUC"="ruc_segmentos.ruc"))

    #Crear la variable tipo de empresa y disminuir sus categorias
    ###########################################################################
    MD_Factura$tipo_emp.x_new <- NA
    MD_Factura$tipo_emp.x_new[MD_Factura$tipo_emp=="SOC.COM.RESPONS. LTDA"] <- "EMI_S/E.RLTDA"
    MD_Factura$tipo_emp.x_new[MD_Factura$tipo_emp=="EMPRESA INDIVIDUAL DE RESP. LTDA"] <- "EMI_S/E.RLTDA"
    MD_Factura$tipo_emp.x_new[MD_Factura$tipo_emp=="SOCIEDAD ANONIMA"] <- "EMI_SOCIEDAD ANONIMA"
    MD_Factura$tipo_emp.x_new[MD_Factura$tipo_emp=="SOCIEDAD ANONIMA CERRADA"] <- "EMI_SOCIEDAD ANONIMA CERRADA"
    MD_Factura$tipo_emp.x_new[MD_Factura$tipo_emp=="PERSONA NATURAL CON NEGOCIO"] <- "EMI_PERSONA NATURAL CON NEGOCIO"
    MD_Factura$tipo_emp.x_new[is.na(MD_Factura$tipo_emp.x_new)] <- "EMI_OTROS TIPO EMPRESA"
    MD_Factura$tipo_emp.x_new[MD_Factura$tipo_emp.x_new==" "] <- "EMI_OTROS TIPO EMPRESA"

    #Crear la variable de días del mes 
    ###########################################################################
    MD_Factura$dia_mes <- as.numeric(MD_Factura$dia_mes)
    MD_Factura$dia_mes_cat[MD_Factura$dia_mes<=3] <- "FEC_EMICPE.Inicio_mes[1-3]" #Al momento de emitir una factura, tienes hasta 3 días con anterioridad para su emisión
    MD_Factura$dia_mes_cat[MD_Factura$dia_mes>=25] <- "FEC_EMICPE.Fin_mes[25-31]" #En la última semana tratan de igual los ingresos para disminuir su IGV
    MD_Factura$dia_mes_cat[is.na(MD_Factura$dia_mes_cat)] <- "FEC_EMICPE.Otros[4-24]"

    #Crear la categorizada del monto
    ###########################################################################
    MD_Factura$cut3_Monto<-cut(MD_Factura$MTO_IMPCPE,breaks = c(0,50,100,150,200,400,600,700,800,900,1000,max(MD_Factura$MTO_IMPCPE)))


    #Cargar la base de datos con el listado de facturas 
    ###########################################################################
    RIESGO_TARGET <- fread("01.BD/2CPE/Riesgo_123_v3.csv",colClasses = list(character=1:5))
    MD_Factura <- left_join(MD_Factura,RIESGO_TARGET,by=c("NUM_RUC","NUM_SERIECPE","NUM_CPE"))
    MD_Factura$NewTarget[is.na(MD_Factura$NewTarget)] <- 0
    MD_Factura$NewTarget <-factor(MD_Factura$NewTarget) 
    levels(MD_Factura$NewTarget) <- c("RT0","RT1","RT2","RT3")


######################################################################################################
# Matriz de receptores
######################################################################################################
    #Agregar las variables principales a nivel de contribuyente
    MD_Receptores <- MD_Factura %>% group_by(NUM_DOCIDENTI) %>% summarise()
    MD_Receptores <- left_join(MD_Receptores,Ficha_Nacional,by = c("NUM_DOCIDENTI"="ruc"))
    MD_Receptores <- left_join(MD_Receptores,RUC_SEGMENTO,by = c("NUM_DOCIDENTI"="ruc_segmentos.ruc"))

    rm(Ficha_Nacional,RUC_SEGMENTO)
    
    #Crear variables resumidas de a nivel de receptores
    ###########################################################################
    c01 <- dcast(MD_Factura,NUM_DOCIDENTI~ruc_segmentos.nom_segfisca,fun=mean,fill=0,value.var ="MTO_IMPCPE")#Segmento RUC
    c02 <- dcast(MD_Factura,NUM_DOCIDENTI~cod_act_princ,fun=mean,fill=0,value.var ="MTO_IMPCPE")#Actividad Económica Emisor
    c03 <- dcast(MD_Factura,NUM_DOCIDENTI~dia_semana, fun=mean,fill=0,value.var ="MTO_IMPCPE")#Día de semana 
    c04 <- dcast(MD_Factura,NUM_DOCIDENTI~dia_mes_cat, fun=mean,fill=0,value.var ="MTO_IMPCPE")#Categoría día de mes
    c05 <- dcast(MD_Factura,NUM_DOCIDENTI~Departamento_similar, fun=mean,fill=0,value.var ="MTO_IMPCPE")#Departamento similar
    c06 <- dcast(MD_Factura,NUM_DOCIDENTI~cut3_Monto, fun=length,fill=0,value.var ="MTO_IMPCPE")#Categoría monto
    c07 <- data.table(MD_Factura %>% group_by(NUM_DOCIDENTI) %>% summarise(Fac_compra = n()))
    c08 <- data.table(MD_Factura %>% group_by(NUM_DOCIDENTI) %>% summarise(Precio_Venta = sum(MTO_IMPCPE),IGV_Registrado = sum(MTO_IGVCPE)))
    c09 <- MD_Factura %>% group_by(NUM_DOCIDENTI,NewTarget) %>% summarise(n = n()) %>% spread(NewTarget,n) 
    c10 <- dcast(MD_Factura,NUM_DOCIDENTI~Actividad_Economica, fun=mean,fill=0,value.var ="MTO_IMPCPE")#Categoría monto

    #Calcular la variable c09 que contiene el target (el contribuyente recibió facturas con productos o servicios relacionados a gastos personales o familiares)
    ###########################################################################
    c09 <- data.frame(c09 %>% replace_na(list(RT0 = 0,RT1 = 0, RT2 = 0, RT3 = 0)))
    #c09$NewTarget <- (0 + c09$RT1 + 2*(c09$RT2) + 3*(c09$RT3))/((c09$RT1) + (c09$RT2) + (c09$RT3))
    #c09$NewTarget_Cat[c09$NewTarget==3] <- "si"
    c09$NewTarget_Cat[c09$RT1>0] <- "si"
    c09$NewTarget_Cat[is.na(c09$NewTarget_Cat)] <- "no"
    c09$NewTarget_Cat <- as.factor(c09$NewTarget_Cat)

    MD_Receptores$RT0 <- NULL
    MD_Receptores$RT1 <- NULL
    MD_Receptores$NewTarget_Cat <- NULL
    #Agregar las variables calculadas c01-c10 a matriz principal de receptores
    ###########################################################################
    MD_Receptores<-left_join(MD_Receptores, c01, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c02, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c03, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c04, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c05, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c06, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c07, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c08, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c09, by=c("NUM_DOCIDENTI"))
    MD_Receptores<-left_join(MD_Receptores, c10, by=c("NUM_DOCIDENTI"))

    rm(c01,c02,c03,c04,c05,c06,c07,c08,c09,c10)

######################################################################################################
#Creación de la matriz de datos FINAL
######################################################################################################
    MD_FINAL <- MD_Receptores %>% select(NUM_DOCIDENTI,cod_act_princ,comercio_ext,facturacion,cond_domicilio,ubigeo,ruc_segmentos.nom_segfisca,`GRAN MEDIANO`,GRANDE,MEDIANO,MICRO,`PEQUEÃ‘O`,`55104`,`55205`,`63040`,`85111`,`85124`,`85193`,`92123`,`92149`,`92192`,`92413`,`92495`,domingo,jueves,lunes,martes,`miércoles`,`sábado`,viernes,`FEC_EMICPE.Fin_mes[25-31]`,`FEC_EMICPE.Inicio_mes[1-3]`,`FEC_EMICPE.Otros[4-24]`,`Diferente Departamento`,`Igual Departamento`,`(0,50]`,`(50,100]`,`(100,150]`,`(150,200]`,`(200,400]`,`(400,600]`,`(600,700]`,`(700,800]`,`(800,900]`,`(900,1e+03]`,`(1e+03,1.85e+07]`,Fac_compra,Precio_Venta,IGV_Registrado,cant_act,NewTarget_Cat,MODA,ENTRETENIMIENTO)

    #Modificación de nombres a variables
    ###########################################################################
    names(MD_FINAL) <- c("RUC","C_ACT_PRINC","C_COMERCIO_EXT","C_TIPO_FACTURACION","C_CONDICION_DOMICILIO","C_UBIGEO","C_SEGMENTO_RUC","M_SE_GRANMEDIANO","M_SE_GRANDE","M_SE_MEDIANO","M_SE_MICRO","M_SE_PEQUENO","M_AE_HOTELES","M_AE_VIAJES","M_AE_RESTAURANTES","M_AE_HOSPITALES","M_AE_MEDICOS","M_AE_SALUDHUMANA","M_AE_FILMESVIDEOS","M_AE_TEATRO","M_AE_ENTRETENIMIENTO","M_AE_DEPORTIVO","M_AE_ESPARCIMIENTO","M_DSE_DOMINGO","M_DSE_JUEVES","M_DSE_LUNES","M_DSE_MARTES","M_DSE_MIERCOLES","M_DSE_SABADO","M_DSE_VIERNES","M_DME_2531","M_DME_0103","M_DME_0424","M_DEP_DIFERENTES","M_DEP_IGUALES","F_000_050","F_050_100","F_100_150","F_150_200","F_200_400","F_400_600","F_600_700","F_700_800","F_800_900","F_900_1000","F_1000_INF","F_FACTURAS","M_PRECIOVENTA","M_IGVREGISTRADO","F_ACT_ECON_REGISTRADO","NewTarget_Cat","MODA","ENTRETENIMIENTO")

    #Transformación de variables UBIGEO y CÓDIGO DE ACTIVIDAD PRINCIPAL
    ###########################################################################
    MD_FINAL$C_UBIGEO <- stri_sub(MD_FINAL$C_UBIGEO,1,2)
    MD_FINAL$C_UBIGEO <- as.factor(MD_FINAL$C_UBIGEO)
    
    MD_FINAL$C_ACT_PRINC <- stri_sub(MD_FINAL$C_ACT_PRINC,1,2)
    MD_FINAL$C_ACT_PRINC <- as.factor(MD_FINAL$C_ACT_PRINC)

    #Crear variable de Riesgo Combinado 
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
    MD_FINAL$R_COMBINADO_AE <- apply(MD_FINAL[,54:66],1,sum)

######################################################################################################
#Agregar indicadores a la MD Final
######################################################################################################
    #Cargar y adjuntar las variables indicadores
    ###########################################################################
    Trabajadores <- fread("01.BD/4TRABAJADORESAUTO/Trabajadores2017RUC.csv",select = c("f601_2017.n_doc_declado","f601_2017.periodo","f601_2017.cas728","f601_2017.cas733"),colClasses = c(f601_2017.n_doc_declado="character"))
    Trabajadores <- Trabajadores %>% group_by(f601_2017.n_doc_declado) %>% summarise(Prom_Trabajadores = round(mean(f601_2017.cas728),0),Prom_Prestad_Servicio = round(mean(f601_2017.cas733),0))
    
    Palabras_raras <- fread("01.BD/6INDICADORES/PropPalabrasRarasStef.csv",colClasses = list(character=1))
    Palabras_raras <- Palabras_raras %>% select(ruc,PropDif)
    
    Frecuencia_IGV <- fread("01.BD/6INDICADORES/PDT_FrecIGV.csv",colClasses = list(character=1:2))
    Frecuencia_IGV <- Frecuencia_IGV %>% select(n_doc_declado,Frec_IGV)
    
    IGV_Octubre <- fread("01.BD/6INDICADORES/PDT_140_145_IGV_2.csv",colClasses = list(character=1:2))
    IGV_Octubre <- IGV_Octubre %>% select(n_doc_declado,Rest=`140-145`) 
    IGV_Octubre$Rest <- abs(IGV_Octubre$Rest)
    
    MD_FINAL <- left_join(MD_FINAL,Palabras_raras,by=c("RUC"="ruc"))
    MD_FINAL <- left_join(MD_FINAL,Frecuencia_IGV,by=c("RUC"="n_doc_declado"))
    MD_FINAL <- left_join(MD_FINAL,IGV_Octubre,by=c("RUC"="n_doc_declado"))
    MD_FINAL <- left_join(MD_FINAL,Trabajadores,by=c("RUC"="f601_2017.n_doc_declado"))

    MD_FINAL$PropDif[is.na(MD_FINAL$PropDif)] <- 0
    MD_FINAL$Frec_IGV[is.na(MD_FINAL$Frec_IGV)] <- 0
    MD_FINAL$Rest[is.na(MD_FINAL$Rest)] <- 0
    MD_FINAL$Prom_Trabajadores[is.na(MD_FINAL$Prom_Trabajadores)] <- 0
    MD_FINAL$Prom_Prestad_Servicio[is.na(MD_FINAL$Prom_Prestad_Servicio)] <- 0
    
    rm(Trabajadores,Palabras_raras,Frecuencia_IGV,IGV_Octubre)

    #Corregir inconsistencias en la data para el modelado
    ###########################################################################
    MD_FINAL$C_COMERCIO_EXT[-is.na(MD_FINAL$C_COMERCIO_EXT) | MD_FINAL$C_COMERCIO_EXT==""] <- "BLANCO"
    MD_FINAL$C_TIPO_FACTURACION[is.na(MD_FINAL$C_TIPO_FACTURACION) | MD_FINAL$C_TIPO_FACTURACION==""] <- "BLANCO"
    MD_FINAL$C_CONDICION_DOMICILIO[is.na(MD_FINAL$C_CONDICION_DOMICILIO) | MD_FINAL$C_CONDICION_DOMICILIO==" "] <- "BLANCO"
    MD_FINAL$C_SEGMENTO_RUC[is.na(MD_FINAL$C_SEGMENTO_RUC)] <- "BLANCO"
    MD_FINAL$F_ACT_ECON_REGISTRADO[is.na(MD_FINAL$F_ACT_ECON_REGISTRADO)] <- 0
    levels(MD_FINAL$C_UBIGEO)[26] <- "00"
    MD_FINAL$C_UBIGEO[is.na(MD_FINAL$C_UBIGEO)] <- "00"
    MD_FINAL$C_COMERCIO_EXT <- factor(MD_FINAL$C_COMERCIO_EXT)
    MD_FINAL$C_TIPO_FACTURACION <- factor(MD_FINAL$C_TIPO_FACTURACION)
    MD_FINAL$C_CONDICION_DOMICILIO <- factor(MD_FINAL$C_CONDICION_DOMICILIO)
    MD_FINAL$C_SEGMENTO_RUC <- factor(MD_FINAL$C_SEGMENTO_RUC)

    

# I. Filtros del negocio:
######################
# 1. Retirar PN_S/N
#Facturas_1<-subset(Facturas_1, tipo_emp.y!="PERSONA NATURAL SIN NEGOCIO")

#3. Trabajar Receptor y: HABIDO / Emisor: HABIDO y ACTIVO
#Facturas_1<-subset(Facturas_1,condicion.y=="HABIDO" & condicion.x=="HABIDO" & estado.x=="ACTIVO")

######################################################################################################
#Creación de datos de entrenamiento y prueba
######################################################################################################
    Tn <- sum(MD_FINAL$NewTarget_Cat=="no")#100
    Ts <- sum(MD_FINAL$NewTarget_Cat=="si")#6
    
    set.seed(216)
    indexes <-  sample(1:Tn, size=(Ts*0.7/0.3))#14
    
    Test.MD_FINAL <- filter(MD_FINAL,NewTarget_Cat=="no")[-indexes,]#80
    Train.MD_FINAL <- filter(MD_FINAL,NewTarget_Cat=="no")[indexes,]#14
    
    Train.MD_FINAL <- bind_rows(Train.MD_FINAL,data.frame(filter(MD_FINAL,NewTarget_Cat=="si")))#20

    Varibles_modelo <- c("C_COMERCIO_EXT","C_TIPO_FACTURACION","C_CONDICION_DOMICILIO","C_UBIGEO","C_SEGMENTO_RUC","M_SE_GRANMEDIANO","M_SE_GRANDE","M_SE_MEDIANO","M_SE_MICRO" ,"M_SE_PEQUENO","M_AE_HOTELES","M_AE_VIAJES","M_AE_RESTAURANTES","M_AE_HOSPITALES","M_AE_MEDICOS", "M_AE_SALUDHUMANA","M_AE_FILMESVIDEOS","M_AE_TEATRO","M_AE_ENTRETENIMIENTO","M_AE_DEPORTIVO","M_AE_ESPARCIMIENTO","MODA","ENTRETENIMIENTO","M_DSE_DOMINGO","M_DSE_JUEVES","M_DSE_LUNES","M_DSE_MARTES","M_DSE_MIERCOLES","M_DSE_SABADO","M_DSE_VIERNES","M_DME_2531","M_DME_0103","M_DME_0424","M_DEP_DIFERENTES","M_DEP_IGUALES","F_000_050","F_050_100","F_100_150","F_150_200","F_200_400","F_400_600","F_600_700","F_700_800","F_800_900","F_900_1000","F_1000_INF","F_FACTURAS","M_PRECIOVENTA","M_IGVREGISTRADO","F_ACT_ECON_REGISTRADO","R_COMBINADO_AE","NewTarget_Cat","PropDif","Frec_IGV","Rest","Prom_Trabajadores","Prom_Prestad_Servicio")
    
    Train.MD_FINAL <- Train.MD_FINAL[,Varibles_modelo]
    Test.MD_FINAL <- Test.MD_FINAL[,c("RUC",Varibles_modelo)]

######################################################################################################
#Aplicación del Modelo Random Forest
######################################################################################################
    rf_model <- randomForest(NewTarget_Cat ~ ., data = Train.MD_FINAL[,1:57])

    #Gráfico de errores
    ###########################################################################
    plot(rf_model,ylim=c(0,1))
    legend('topright',colnames(rf_model$err.rate),col = 1:3,fill = 1:3)

    
    #Gráfico de importancia de variables en el modelo rf
    ###########################################################################
    importancia <- importance(rf_model)
    varImportancia <- data.frame(Variables = row.names(importancia), Importancia = round(importancia[,'MeanDecreaseGini'],2))
    
    rankImportancia <- varImportancia %>% mutate(Rank = paste0('#',dense_rank(desc(Importancia))))
    
    ggplot(rankImportancia,aes(x=reorder(Variables,Importancia),y=Importancia, fill=Importancia))+
            geom_bar(stat = "identity")+
            geom_text(aes(x=Variables,y=0.4,label=Rank),hjust=0,vjust=0.55,size=3,colour = "white")+
            labs(x="Variables")+
            coord_flip()

    #Estimación de la prediccióm
    ###########################################################################
    pred1 <- predict(rf_model,Test.MD_FINAL)
    pred1p <- predict(rf_model,Test.MD_FINAL,type = "prob")[,2]
    
    Test.MD_FINAL$pred1 <- pred1
    Test.MD_FINAL$pred1p <- pred1p


######################################################################################################
#Modelo ensamblado
######################################################################################################
    df <- data.table(Train.MD_FINAL[,2:57],keep.rownames = F)
    sparse_matrix_model <- sparse.model.matrix(NewTarget_Cat ~ . , data = df)
  
    df_Test <- data.table(Test.MD_FINAL[,3:57],keep.rownames = F)
    sparse_matrix_model_test <- sparse.model.matrix(NewTarget_Cat ~ . , data = df_Test)

    output_Vector <- df[,NewTarget_Cat=="si"]

    bst <- xgboost(data = sparse_matrix_model, label = output_Vector, max_depth = 4,
                   eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")

    importance <- xgb.importance(feature_names = colnames(sparse_matrix_model), model = bst)
    
    importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix_model), model = bst, data = sparse_matrix_model, label = output_Vector)
    
    importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
    head(importanceClean)

    xgb.plot.importance(importance_matrix = importance)
    
    pred2 <- predict(bst,sparse_matrix_model_test)
    prediction <- as.numeric(pred2 > 0.5)

    
    library(xgboost)
    xgb.plot.tree(feature_names = colnames(sparse_matrix_model),model = bst)
