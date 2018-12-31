######################################################################################################
#Detalle de factura
######################################################################################################
CPEd1 <- fread("01.BD/5DETALLE/det_oct_2017_1.csv",select = c("COD_TIPOCOMP","NUM_RUC","NUM_SERIECPE","NUM_CPE","NUM_CNTITM","DES_DETITM"),colClasses=list(character=1:4,numeric=5))
CPEd2 <- fread("01.BD/5DETALLE/det_oct_2017_2.csv",select = c("COD_TIPOCOMP","NUM_RUC","NUM_SERIECPE","NUM_CPE","NUM_CNTITM","DES_DETITM"),colClasses=list(character=1:4,numeric=5))
CPEd3 <- fread("01.BD/5DETALLE/det_oct_2017_3.csv",select = c("COD_TIPOCOMP","NUM_RUC","NUM_SERIECPE","NUM_CPE","NUM_CNTITM","DES_DETITM"),colClasses=list(character=1:4,numeric=5))

memory.size()
memory.limit(12000)
CPEd1 <- bind_rows(CPEd1,CPEd2)
TablaFinal <- bind_rows(CPEd1,CPEd2,CPEd3)
rm(CPEd1,CPEd2)


######################################################################################################
#Matriz de Datos de receptores
######################################################################################################
Facturas <- left_join(Facturas,TablaFinal,by = c("COD_TIPOCOMP","NUM_RUC","NUM_SERIECPE","NUM_CPE"))

MD_Receptores <- Facturas %>% group_by(NUM_DOCIDENTI) %>% summarise(mean_items = mean(Items))

FichaNacional <- fread("01.BD/ficha_nacional.csv",select = c("ruc","dependencia","condicion","estado","tipo_emp","tamano","cod_act_princ","act_secun1","act_secun2","comercio_ext","facturacion","cond_domicilio","nro_docide","ubigeo"),colClasses=list(character=1:14))

MD_Receptores <- left_join(MD_Receptores,FichaNacional,by =c("NUM_DOCIDENTI"="ruc") )

