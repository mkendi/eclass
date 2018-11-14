##DIFERENÇA ENTRE O QUE ELE ACHA E O QUE ELE ACHA QUE O PESQUISADOR ACHA (A-B)
ec.dif<- select(ec.p1, A011,A021,A031,A041,A051,A061,A071,A081,A091,A101,A111,A121,A131,A141,A151,A161,A171,A181,A191,A201,A211,A221,A231,A241,A251,A201,A211,A221,A231,A241,A251,A261,A271,A281,A291,A301)-select(ec.p1, B011,B021,B031,B041,B051,B061,B071,B081,B091,B101,B111,B121,B131,B141,B151,B161,B171,B181,B191,B201,B211,B221,B231,B241,B251,B201,B211,B221,B231,B241,B251,B261,B271,B281,B291,B301)

# Criação do sumário
ec.dif.desc <- ec.dif %>%
  gather(key=quest, value=resp) %>%
  group_by(quest) %>%
  summarise(med=mean(resp, na.rm = TRUE), dp=sd(resp, na.rm = TRUE),
            errop=dp/sqrt(n()),
            total=n()) %>%
  mutate(type=ifelse(grepl("A", quest), "P", "F")) %>%
  arrange(desc(quest))



# Gráfico descritivo dif

gd.dif <- ggplot(ec.dif.desc, aes(x=factor(quest), y=med)) +
  geom_point(aes(fill=type), shape=21, size=2) +
  geom_errorbar(aes(ymin=med-errop, ymax=med+errop),
                size=.3, width=.2, position=position_dodge(.9)) +
  theme_minimal() +
  scale_fill_wsj(name="Resposta", 
                 labels="Físico experimental" +
  coord_flip()


gd.dif