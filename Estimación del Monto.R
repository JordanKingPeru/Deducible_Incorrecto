a <- Test.MD_FINAL %>% group_by(stri_sub(pred1p,1,3)) %>% summarise(n())
b <- Test.MD_FINAL %>% filter(pred1p>0.39)

dim(b)[1]
indexes <-  sample(1:dim(b)[1], size=380)

b <- b[indexes,]

ggplot(b,aes(x=pred1p)) + geom_density()

c <- MD_Factura %>% filter(NUM_DOCIDENTI %in% b$RUC)

b %>% group_by(stri_sub(pred1p,1,3))%>% summarise(sum(M_PRECIOVENTA))
b %>%  summarise(sum(M_PRECIOVENTA))
