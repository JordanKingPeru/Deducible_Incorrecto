Test.Modelo_MD_Factura %>% group_by(prediction) %>% summarise(n())

Test.Modelo_MD_Factura %>% filter(prediction=="1") %>% summarise(sum(MTO_IGVCPE),n())
Train.Modelo_MD_Factura %>% filter(Target=="si") %>% summarise(sum(MTO_IGVCPE),n())

Modelo_MD_Factura %>% group_by(Actividad_Economica,Target) %>% summarise(n=n()) %>% 
  spread(Target,n)

Modelo_MD_Factura %>% filter(MTO_IMPCPE<40 & Actividad_Economica=="RESTAURANTES, BARES Y CANTINAS.") %>% summarise(n())
Modelo_MD_Factura$Target[Modelo_MD_Factura$MTO_IMPCPE<100 & Modelo_MD_Factura$Actividad_Economica=="RESTAURANTES, BARES Y CANTINAS."] <- "si"
Test.Modelo_MD_Factura$prediction[Test.Modelo_MD_Factura$MTO_IMPCPE<100 & Test.Modelo_MD_Factura$Actividad_Economica=="RESTAURANTES, BARES Y CANTINAS."] <- 1
Train.Modelo_MD_Factura$Target[Train.Modelo_MD_Factura$MTO_IMPCPE<100 & Train.Modelo_MD_Factura$Actividad_Economica=="RESTAURANTES, BARES Y CANTINAS."] <- "si"

456801.8+5924459
1119746+6000866
171187+79461
