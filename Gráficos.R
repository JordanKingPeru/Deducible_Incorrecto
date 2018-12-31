MD_Factura$dia_semana[MD_Factura$dia_semana=="lunes"] <- "1.Lunes"
MD_Factura$dia_semana[MD_Factura$dia_semana=="martes"] <- "2.martes"
MD_Factura$dia_semana[MD_Factura$dia_semana=="miércoles"] <- "3.miércoles"
MD_Factura$dia_semana[MD_Factura$dia_semana=="jueves"] <- "4.jueves"
MD_Factura$dia_semana[MD_Factura$dia_semana=="viernes"] <- "5.viernes"
MD_Factura$dia_semana[MD_Factura$dia_semana=="sábado"] <- "6.sábado"
MD_Factura$dia_semana[MD_Factura$dia_semana=="domingo"] <- "7.domingo"

MD_Factura$Comportamiento <- NA
MD_Factura$Comportamiento[MD_Factura$dia_semana=="6.sábado"] <- "1.Fin de Semana"
MD_Factura$Comportamiento[MD_Factura$dia_semana=="7.domingo"] <- "1.Fin de Semana"
MD_Factura$Comportamiento[is.na(MD_Factura$Comportamiento)] <- "2.Días laborables"

#Gráfico 1: Facturas emitidas por categoría de Precio de venta
###########################################################################
T1 <- MD_Factura %>%  filter(MTO_IMPCPE > 0) %>% 
  group_by(cut3_Monto) %>% summarise(PVenta=sum(MTO_IMPCPE),n=n()) %>% mutate(porc=round(100*n/sum(n),1)) %>% mutate(sa=cumsum(porc))

ggplot(data = T1, aes(x = cut3_Monto, y=n))+
  geom_bar(stat = "identity")+
  labs(title='CPE Facturas emitidas',x="Categoría de Precio de venta (S/.)",y="N°Facturas")+
  scale_y_continuous(label=function(x){return(paste(x/1000, "k"))})+
  theme(axis.text.x=element_text(angle=90, size=9, vjust=0.5),
        plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5)) +
  geom_text( aes(label = paste(porc,"%",sep=""),
                  y = n + 4), position = position_dodge(1),  vjust = 0.5,size =3,angle=90)+
  annotate("text", label = "83% de las facturas registran precios de venta menor a S/400", x =7,y=350000, size = 4, colour = "blue")

#Gráfico 2: Facturas emitidas por Fines de semana
###########################################################################
T2 <- MD_Factura %>%  filter(MTO_IMPCPE <1000) %>% 
  group_by(cut3_Monto,dia_semana,Comportamiento) %>% summarise(PVenta=sum(MTO_IMPCPE),n=n()) %>% mutate(porc=round(100*n/sum(n),1)) %>% mutate(sa=cumsum(porc))

ggplot(T2,aes(x=cut3_Monto,y=n,fill=Comportamiento))+
  geom_bar(stat = "identity")+
  geom_violin(alpha=0.5, color="gray")+
  labs(title='Precio Venta de CPE Factura emitidas',x="Días de semana",y="Precio de venta (S/.)")+
  scale_y_continuous(label=function(x){return(paste(x/1000000,"M"))})+
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5),
        plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5))

#Gráfico 3: Diferente departamento
###########################################################################
T3 <- as.data.frame(MD_Factura %>%
  group_by(Departamento_similar,Comportamiento) %>% 
  summarise(PVenta=sum(MTO_IMPCPE),n=n()) )
T3 <- T3%>% 
  mutate(porc=round(100*n/sum(n),1)) %>% 
  mutate(sa=cumsum(porc))

ggplot(T3,aes(x=Departamento_similar,y=n,fill=Comportamiento))+
  geom_bar(stat = "identity")+
  labs(x="Comparacion departamento de emisor y receptor",y="N° Factura")+
  scale_y_continuous(label=function(x){return(paste(x/1000,"k"))})+
  theme(axis.text.x=element_text(angle=0, size=8, vjust=0.5),
        plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5))+
  geom_label( aes(label = paste(porc,"%",sep="")),size =3,position = position_stack(vjust = 0.5))
  
#Gráfico 4: Diferente departamento
###########################################################################
library(tidyverse)
"%w/o%" <- function(x, y) x[!x %in% y]
(1:10) %w/o% c(3,7,12)
incluir <- c("GRAN MEDIANO","GRANDE","MEDIANO","MEGA","MICRO","PEQUEÃ‘O","TOP")
  
T4 <- as.data.frame(MD_Factura %>% filter(ruc_segmentos.nom_segfisca %in% incluir) %>% 
                      group_by(ruc_segmentos.nom_segfisca,Comportamiento) %>% 
                      summarise(PVenta=sum(MTO_IMPCPE),n=n()) )
T4 <- T4%>% 
  mutate(porc=round(100*n/sum(n),1)) %>% 
  mutate(sa=cumsum(porc))

ggplot(T4,aes(x=reorder(ruc_segmentos.nom_segfisca,-n),y=n,fill=Comportamiento))+
  geom_bar(stat = "identity")+
  labs(x="Segmento del emisor de CPE Factura",y="N° Factura")+
  scale_y_continuous(label=function(x){return(paste(x/1000,"k"))})+
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5),
        plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5))+
  geom_text( aes(label = paste(porc,"%",sep="")),size =3,position = position_stack(vjust = 0.5))

#Gráfico 5: 
###########################################################################
T5 <- as.data.frame(MD_Factura %>%
                      group_by(dia_semana,Comportamiento) %>% 
                      summarise(PVenta=sum(MTO_IMPCPE),n=n()) )
T5 <- T5%>% 
  mutate(porc=round(100*n/sum(n),1)) %>% 
  mutate(sa=cumsum(porc))

ggplot(T5,aes(x=dia_semana,y=n))+
  geom_bar(stat = "identity")+
  labs(x="Día de semana de emisión de CPE Factura",y="N° Factura")+
  scale_y_continuous(label=function(x){return(paste(x/1000,"k"))})+
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5),
        plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5))+
  geom_label( aes(label = paste(porc,"%",sep="")),size =3,position = position_stack(vjust = 0.5))

#Gráfico 6: 
###########################################################################
T6 <- as.data.frame(MD_Factura %>%
                      group_by(comercio_ext) %>% 
                      summarise(PVenta=sum(MTO_IMPCPE),n=n()) )
T6 <- T6%>% 
  mutate(porc=round(100*n/sum(n),1)) %>% 
  mutate(sa=cumsum(porc))

ggplot(T6,aes(x=reorder(comercio_ext,-n),y=n))+
  geom_bar(stat = "identity")+
  labs(x="Comercio exterior",y="N° Factura")+
  scale_y_continuous(label=function(x){return(paste(x/1000,"k"))})+
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5),
        plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5))+
  geom_label( aes(label = paste(porc,"%",sep="")),size =3,position = position_stack(vjust = 0.5))


#Gráfico 7: 
###########################################################################
T7 <- as.data.frame(MD_Factura %>%
                      group_by(facturacion) %>% 
                      summarise(PVenta=sum(MTO_IMPCPE),n=n()) )
T7 <- T7%>% 
  mutate(porc=round(100*n/sum(n),1)) %>% 
  mutate(sa=cumsum(porc))

ggplot(T7,aes(x=reorder(facturacion,-n),y=n))+
  geom_bar(stat = "identity")+
  labs(x="Comercio exterior",y="N° Factura")+
  scale_y_continuous(label=function(x){return(paste(x/1000,"k"))})+
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5),
        plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5))+
  geom_label( aes(label = paste(porc,"%",sep="")),size =3,position = position_stack(vjust = 0.5))

