#Variables_modelo_factura <- c("COD_TIPOCOMP.x","NUM_RUC","NUM_SERIECPE","NUM_DOCIDENTI","MTO_IMPCPE","MTO_IGVCPE","act_principal","comercio_ext","facturacion","condicion","estado","tipo_emp","tamano","cond_domicilio","ubigeo.x","cant_act","Actividad_Economica","dia_semana","dia_mes","Departamento_similar","ruc_segmentos.nom_segfisca","tipo_emp.x_new","dia_mes_cat","NewTarget" ,"AE")
Variables_modelo_factura <- c("COD_TIPOCOMP.x","NUM_RUC","NUM_SERIECPE","NUM_CPE","NUM_DOCIDENTI","MTO_IMPCPE","MTO_IGVCPE","act_principal","comercio_ext","facturacion","condicion","estado","tipo_emp","tamano","cond_domicilio","ubigeo.x","cant_act","Actividad_Economica","dia_semana","dia_mes","Departamento_similar","ruc_segmentos.nom_segfisca","tipo_emp.x_new","dia_mes_cat","Target" )

names(MD_Factura)
Modelo_MD_Factura <- MD_Factura[,Variables_modelo_factura]

############################################################################
RIESGO_TARGET <- fread("01.BD/2CPE/Riesgo_123_v2.csv",colClasses = list(character=1:5))

Modelo_MD_Factura <- left_join(Modelo_MD_Factura,RIESGO_TARGET,by=c("NUM_RUC","NUM_SERIECPE","NUM_CPE"))


Modelo_MD_Factura$NewTarget[is.na(Modelo_MD_Factura$NewTarget)] <- 0
Modelo_MD_Factura$NewTarget <-factor(Modelo_MD_Factura$NewTarget) 
levels(Modelo_MD_Factura$NewTarget) <- c("RT0","RT1","RT2","RT3")

############################################################################
#Repartición de factura
Modelo_MD_Factura %>% group_by(NewTarget) %>% summarise(n())
#1 RT0       1380841
#2 RT1          4591
#3 RT2          1481
#4 RT3          2486

Modelo_MD_Factura$Target[Modelo_MD_Factura$NewTarget=="RT0"] <- "no"
Modelo_MD_Factura$Target[is.na(Modelo_MD_Factura$Target)] <- "si"

Modelo_MD_Factura %>% group_by(Target) %>% summarise(n())
#1 no     1380841
#2 si        8558

######################################################################################################
#Creación d edatos de entrenamiento y 
######################################################################################################
TFn <- sum(Modelo_MD_Factura$Target=="no")
TFs <- sum(Modelo_MD_Factura$Target=="si")

set.seed(216)
indexes <-  sample(1:TFn, size=(TFs*0.7/0.3))
#19968
Test.Modelo_MD_Factura <- filter(Modelo_MD_Factura,Target=="no")[-indexes,]
Train.Modelo_MD_Factura <- filter(Modelo_MD_Factura,Target=="no")[indexes,]

Train.Modelo_MD_Factura %>% group_by(NewTarget_Cat) %>% summarise(n())

Train.Modelo_MD_Factura <- bind_rows(Train.Modelo_MD_Factura,data.frame(filter(Modelo_MD_Factura,Target=="si")))

Train.Modelo_MD_Factura$act_principal <- factor(Train.Modelo_MD_Factura$act_principal)
Train.Modelo_MD_Factura$comercio_ext <- factor(Train.Modelo_MD_Factura$comercio_ext)
Train.Modelo_MD_Factura$facturacion <- factor(Train.Modelo_MD_Factura$facturacion)
Train.Modelo_MD_Factura$condicion <- factor(Train.Modelo_MD_Factura$condicion)
Train.Modelo_MD_Factura$estado <- factor(Train.Modelo_MD_Factura$estado)
Train.Modelo_MD_Factura$tipo_emp <- factor(Train.Modelo_MD_Factura$tipo_emp)
Train.Modelo_MD_Factura$tamano <- factor(Train.Modelo_MD_Factura$tamano)
Train.Modelo_MD_Factura$cond_domicilio <- factor(Train.Modelo_MD_Factura$cond_domicilio)
Train.Modelo_MD_Factura$ubigeo.x <- factor(stri_sub(Train.Modelo_MD_Factura$ubigeo.x,1,2))
Train.Modelo_MD_Factura$Actividad_Economica <- factor(Train.Modelo_MD_Factura$Actividad_Economica)
Train.Modelo_MD_Factura$dia_semana <- factor(Train.Modelo_MD_Factura$dia_semana)
Train.Modelo_MD_Factura$ruc_segmentos.nom_segfisca <- factor(Train.Modelo_MD_Factura$ruc_segmentos.nom_segfisca)
Train.Modelo_MD_Factura$tipo_emp.x_new <- factor(Train.Modelo_MD_Factura$tipo_emp.x_new)
Train.Modelo_MD_Factura$dia_mes_cat <- factor(Train.Modelo_MD_Factura$dia_mes_cat)
Train.Modelo_MD_Factura$Target <- factor(Train.Modelo_MD_Factura$Target)
Train.Modelo_MD_Factura$Departamento_similar <- factor(Train.Modelo_MD_Factura$Departamento_similar)

Train.Modelo_MD_Factura <- Train.Modelo_MD_Factura[,Variables_modelo_factura]
Test.Modelo_MD_Factura <- Test.Modelo_MD_Factura[,Variables_modelo_factura]
names(Test.Modelo_MD_Factura)

######################################################################################################
# 
######################################################################################################

rf_model <- randomForest(Target ~ ., data = Train.Modelo_MD_Factura[,c(5:23,25)])
plot(rf_model,ylim=c(0,1))
legend('topright',colnames(rf_model$err.rate),col = 1:3,fill = 1:3)

importancia <- importance(rf_model)
varImportancia <- data.frame(Variables = row.names(importancia), Importancia = round(importancia[,'MeanDecreaseGini'],2))

rankImportancia <- varImportancia %>% mutate(Rank = paste0('#',dense_rank(desc(Importancia))))

ggplot(rankImportancia,aes(x=reorder(Variables,Importancia),y=Importancia, fill=Importancia))+
  geom_bar(stat = "identity")+
  geom_text(aes(x=Variables,y=0.5,label=Rank),hjust=0,vjust=0.55,size=4,colour = "white")+
  labs(x="Variables")+
  coord_flip()

pred1 <- predict(rf_model,Test.MD_FINAL)
pred1p <- predict(rf_model,Test.MD_FINAL,type = "prob")[,2]

Test.MD_FINAL$pred1 <- pred1
Test.MD_FINAL$pred1p <- pred1p

Test.MD_FINAL %>% group_by(pred1) %>% summarise(n())
Resultados_Test<- filter(Test.MD_FINAL, pred1=="si")

######################################################################################################
#Modelo ensamblado
######################################################################################################

library(xgboost)
library(Matrix)
library(data.table)

df <- data.table(Train.Modelo_MD_Factura[,c(6:9,11:25)],keep.rownames = F)
sparse_matrix_model <- sparse.model.matrix(Target ~ . , data = df)

df_Test <- data.table(Test.Modelo_MD_Factura[,c(6:9,11:25)],keep.rownames = F)
sparse_matrix_model_test <- sparse.model.matrix(Target ~ . , data = df_Test)
head(sparse_matrix_model)

output_Vector <- df[,Target=="si"]
output_Vector <- df$NewTarget_Cat
table(output_Vector)

bst <- xgboost(data = sparse_matrix_model, label = output_Vector, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")

data(agaricus.train, package='xgboost')

importance <- xgb.importance(feature_names = colnames(sparse_matrix_model), model = bst)
head(importance)

importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix_model), model = bst, data = sparse_matrix_model, label = output_Vector)

importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceClean)

xgb.plot.importance(importance_matrix = importance)

pred2 <- predict(bst,sparse_matrix_model_test)
Test.Modelo_MD_Factura$Pred1 <- pred2

Test.Modelo_MD_Factura$prediction <- as.numeric(pred2 > 0.5)
table(prediction)

library(xgboost)
library(DiagrammeR)
xgb.plot.tree(model = bst)
xgb.plot.tree(feature_names = colnames(sparse_matrix_model),model = bst)
