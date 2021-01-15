library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(haven)
library(tidyr)
library(reshape2)
library(geobr)
library(xlsx)
library(psych)
library(readr)
library(devtools)
library(sidrar)
library(reshape2)

#Todo produto tem 7 variaveis e 43 anos para nivel municipal
#fonte:https://sidra.ibge.gov.br/tabela/1612#
#Não tem como usar o get_sidra pelo numero de valores ser muito elevado


#usando csv (us) - area plantada, percentual area plantada, area colhida, percentual area colhida 
#o Sidra só permite pegar de dez em dez anos para nao 
#CULTURAS PERMANENTES

#AREA DEDICADA A COLHEITA

################################
#QUINZE PRIMEIROS ANOS - 2019 A 2004
a_dedcolheita1 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/dedcol/dedcol1.csv", header=FALSE, comment.char="#")
#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
a_dedcolheita1<- a_dedcolheita1%>%
  filter(!V2 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007"))


#PERIODO SEGUINTE - 2006 A 1994 - RESTRICOES DO SIDRA
a_dedcolheita2 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/dedcol/dedcol2.csv", header=FALSE, comment.char="#")

#tirando os valores que não são observações
a_dedcolheita2<- a_dedcolheita2%>%
  filter(!V2 != c("2006",  "2005",  "2004","2003", "2002", "2001", "2000", "1999", "1998",
                  "1997","1996","1995","1994"))

#PERIODO SEGUINTE - 
a_dedcolheita3 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/dedcol/dedcol3.csv", header=FALSE, comment.char="#")

a_dedcolheita3<- a_dedcolheita3%>%
  filter(!V2 != c("1993","1992","1991","1990","1989", "1988","1987","1986","1985", "1984", "1983", "1982", "1981" ))

#PERIODO FINAL
a_dedcolheita4 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/dedcol/dedcol4.csv", header=FALSE, comment.char="#")

a_dedcolheita4<-a_dedcolheita4%>%
  filter(!V2 != c("1980","1979", "1978", "1977","1976", "1975", "1974" ))

a_dedcolheitaf <- a_dedcolheita1%>%
  add_row(a_dedcolheita2)%>%
  add_row(a_dedcolheita3)%>%
  add_row(a_dedcolheita4)


#renomeando com o valor de area plantada de cada produto
colnames(a_dedcolheitaf)<- c("id_municipio","ano","Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                               "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                               "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                               "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                               "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                               "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                               "Maçã", "Mamão", "Mangua", "Maracujá",
                               "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                               "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                "Uva")

#criando uma variavel que identifique qual produto é qual
a_dedcolheitaf<- melt(a_dedcolheitaf, id.vars = c("id_municipio","ano"), 
                        measure.vars = c("Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                         "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                         "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                         "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                         "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                         "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                         "Maçã", "Mamão", "Mangua", "Maracujá",
                                         "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                         "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                         "Uva"))%>%
  rename(area_ded_colheita = value, produto = variable)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#PERCENTUAL DA AREA DEDICADA A COLHEITA

#QUINZE PRIMEIROS ANOS - 2019 A 2004
perca_dedcolheita1 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percdedcol/percdedcol1.csv", header=FALSE, comment.char="#")
#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
perca_dedcolheita1<- perca_dedcolheita1%>%
  filter(!V2 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007"))


#PERIODO SEGUINTE - 2006 A 1992
perca_dedcolheita2 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percdedcol/percdedcol2.csv", header=FALSE, comment.char="#")

#tirando os valores que não são observações
perca_dedcolheita2<-perca_dedcolheita2%>%
  filter(!V2 != c("2006",  "2005",  "2004","2003", "2002", "2001", "2000", "1999", "1998",
                  "1997","1996","1995","1994"))

#PERIODO SEGUINTE - 1991 A 1977
perca_dedcolheita3 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percdedcol/percdedcol3.csv", header=FALSE, comment.char="#")

perca_dedcolheita3<- perca_dedcolheita3%>%
  filter(!V2 != c("1993","1992", "1991","1990","1989", "1988","1987","1986","1985", "1984", "1983", "1982", "1981"))

#PERÍODO FINAL - 1976 A 1974
perca_dedcolheita4 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percdedcol/percdedcol4.csv", header=FALSE, comment.char="#")

perca_dedcolheita4<- perca_dedcolheita4%>%
  filter(!V2 != c( "1980", "1979", 
                   "1978", "1977","1976", "1975", "1974" ))

perca_dedcolheita_final <- perca_dedcolheita1%>%
  add_row(perca_dedcolheita2)%>%
  add_row(perca_dedcolheita3)%>%
  add_row(perca_dedcolheita4)

#renomeando com o valor de area plantada de cada produto
colnames(perca_dedcolheita_final)<- c("id_municipio","ano","Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                      "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                      "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                      "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                      "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                      "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                      "Maçã", "Mamão", "Mangua", "Maracujá",
                                      "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                      "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                      "Uva")


#criando uma variavel que identifique qual produto é qual
perca_dedcolheita_final<- melt(perca_dedcolheita_final, id.vars = c("ano","id_municipio"), 
                            measure.vars = c("Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                             "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                             "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                             "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                             "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                             "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                             "Maçã", "Mamão", "Mangua", "Maracujá",
                                             "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                             "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                             "Uva"))%>%
  rename(perc_a_dedcolheita = value, produto = variable)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#AREA COLHIDA

#QUINZE PRIMEIROS ANOS - 2019 A 2007
a_colhida1 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/acolhida/c1.csv", header=FALSE, comment.char="#")
#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
a_colhida1<- a_colhida1%>%
  filter(!V2 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007" ))


#PERIODO SEGUINTE - 2006 A 1992
a_colhida2 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/acolhida/c2.csv", header=FALSE, comment.char="#")

#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
a_colhida2<-a_colhida2%>%
  filter(!V2 != c( "2006",  "2005",  "2004","2003", "2002", "2001", "2000", "1999", "1998",
                   "1997","1996","1995","1994"))

#PERIODO SEGUINTE - 1991 A 1977
a_colhida3 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/acolhida/c3.csv", header=FALSE, comment.char="#")

a_colhida3<- a_colhida3%>%
  filter(!V2 != c("1993","1992","1991","1990","1989", "1988","1987","1986","1985", "1984", "1983", "1982", "1981"))

a_colhida4 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/acolhida/c4.csv", header=FALSE, comment.char="#")

a_colhida4<- a_colhida4%>%
  filter(!V2 != c("1980", "1979", "1978", "1977","1976", "1975", "1974" ))


a_colhida_final <- a_colhida1%>%
  add_row(a_colhida2)%>%
  add_row(a_colhida3)%>%
  add_row(a_colhida4)

#renomeando com o valor de area plantada de cada produto
colnames(a_colhida_final) <- c("id_municipio","ano","Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                               "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                               "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                               "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                               "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                               "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                               "Maçã", "Mamão", "Mangua", "Maracujá",
                               "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                               "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                               "Uva")

#criando uma variavel que identifique qual produto é qual
a_colhida_final<- melt(a_colhida_final, id.vars = c("ano","id_municipio"), 
                       measure.vars = c("Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                        "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                        "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                        "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                        "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                        "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                        "Maçã", "Mamão", "Mangua", "Maracujá",
                                        "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                        "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                        "Uva"))%>%
  rename(a_colhida = value, produto = variable)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# PERC AREA COLHIDA


#QUINZE PRIMEIROS ANOS - 2019 A 2004
perca_colhida1 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percacolhida/q1.csv", header=FALSE, comment.char="#")
#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
perca_colhida1<- perca_colhida1%>%
  filter(!V2 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007"))


#PERIODO SEGUINTE - 2006 A 1992
perca_colhida2 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percacolhida/q2.csv", header=FALSE, comment.char="#")

#tirando os valores que não são observações
perca_colhida2<-perca_colhida2%>%
  filter(!V2 != c("2006",  "2005",  "2004","2003", "2002", "2001", "2000", "1999", "1998",
                    "1997","1996","1995","1994",))

#PERIODO SEGUINTE - 1991 A 1977
perca_colhida3 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percacolhida/q3.csv", header=FALSE, comment.char="#")

perca_colhida3<- perca_colhida3%>%
  filter(!V2 != c("1993","1992","1991","1990","1989", "1988","1987","1986","1985", "1984", 
                  "1983", "1982", "1981"))

#PERIODO FINAL - 1976 A 1974
perca_colhida4 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percacolhida/q4.csv", header=FALSE, comment.char="#")

perca_colhida4<- perca_colhida4%>%
  filter(!V2 != c("1980", "1979", "1978", "1977", "1976", "1975", "1974" ))



perca_colhida_final <- perca_colhida1%>%
  add_row(perca_colhida2)%>%
  add_row(perca_colhida3)%>%
  add_row(perca_colhida4)

#renomeando com o valor de area plantada de cada produto
colnames(perca_colhida_final)<- c("id_municipio","ano","Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                  "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                  "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                  "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                  "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                  "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                  "Maçã", "Mamão", "Mangua", "Maracujá",
                                  "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                  "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                  "Uva")

#criando uma variavel que identifique qual produto é qual
perca_colhida_final<- melt(a_colhida_final, id.vars = c("ano","id_municipio"), 
                           measure.vars = c("Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                            "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                            "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                            "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                            "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                            "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                            "Maçã", "Mamão", "Mangua", "Maracujá",
                                            "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                            "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                            "Uva"))%>%
  rename(perca_colhida = value, produto = variable)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#QUANTIDADE PRODUZIDA


#QUINZE PRIMEIROS ANOS - 2019 A 2007
qtd_produzida1 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/qtd/t1.csv", header=FALSE, comment.char="#")
#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
qtd_produzida1<- qtd_produzida1%>%
  filter(!V2 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007"))


#PERIODO SEGUINTE - 2006 A 1992
qtd_produzida2 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/qtd/t2.csv", header=FALSE, comment.char="#")

#tirando os valores que não são observações
qtd_produzida2<-qtd_produzida2%>%
  filter(!V2 != c("2006",  "2005",  "2004","2003", "2002", "2001", "2000", "1999", "1998",
                  "1997","1996","1995","1994"))

#PERIODO SEGUINTE - 1991 A 1977
qtd_produzida3 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/qtd/t3.csv", header=FALSE, comment.char="#")

qtd_produzida3<- qtd_produzida3%>%
  filter(!V2 != c("1993","1992", "1991","1990","1989", "1988","1987","1986","1985", "1984", 
                  "1983", "1982", "1981" ))

#PERIODO FINAL - 1976 A 1974
qtd_produzida4 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/qtd/t4.csv", header=FALSE, comment.char="#")

qtd_produzida4<- qtd_produzida4%>%
  filter(!V2 != c("1980", "1979", "1978", "1977", "1976", "1975", "1974" ))


qtd_produzida_final <- qtd_produzida1%>%
  add_row(qtd_produzida2)%>%
  add_row(qtd_produzida3)%>%
  add_row(qtd_produzida4)

#renomeando com o valor de area plantada de cada produto
colnames(qtd_produzida_final)<- c("id_municipio","ano","Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                  "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                  "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                  "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                  "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                  "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                  "Maçã", "Mamão", "Mangua", "Maracujá",
                                  "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                  "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                  "Uva")

#criando uma variavel que identifique qual produto é qual
qtd_produzida_final<- melt(a_colhida_final, id.vars = c("ano","id_municipio"), 
                           measure.vars = c("Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                            "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                            "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                            "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                            "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                            "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                            "Maçã", "Mamão", "Mangua", "Maracujá",
                                            "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                            "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                            "Uva"))%>%
  rename(qtd_produzida = value, produto = variable)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#RENDIMENTO MÉDIO DA PRODUÇÃO

#QUINZE PRIMEIROS ANOS - 2019 A 2007
rend_medio1 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/rend/r1.csv", header=FALSE, comment.char="#")
#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
rend_medio1<- rend_medio1%>%
  filter(!V2 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007"))


#PERIODO SEGUINTE - 2006 a 1992
rend_medio2 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/rend/r2.csv", header=FALSE, comment.char="#")

#tirando os valores que não são observações
rend_medio2<-rend_medio2%>%
  filter(!V2 != c("2006", "2005", "2004", "2003", "2002", "2001", "2000", "1999", "1998",
                  "1997","1996","1995","1994"))

#PERIODO SEGUINTE - 1991 a 1977
rend_medio3 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/rend/r3.csv", header=FALSE, comment.char="#")

rend_medio3<-rend_medio3%>%
  filter(!V2 != c("1993","1992", "1991","1990","1989", "1988","1987","1986","1985", "1984", "1983", "1982", 
                  "1981" ))

#PERIODO FINAL - 1976 A 1974
rend_medio4 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/rend/r4.csv", header=FALSE, comment.char="#")

rend_medio4<- rend_medio4%>%
  filter(!V2 != c("1980", "1979", "1978", "1977","1976", "1975", "1974" ))

rend_medio_final <- rend_medio1%>%
  add_row(rend_medio2)%>%
  add_row(rend_medio3)%>%
  add_row(rend_medio4)

#renomeando com o valor de area plantada de cada produto
colnames(rend_medio_final)<- c("id_municipio","ano","Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                               "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                               "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                               "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                               "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                               "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                               "Maçã", "Mamão", "Mangua", "Maracujá",
                               "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                               "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                               "Uva")

#criando uma variavel que identifique qual produto é qual
rend_medio_final<- melt(a_colhida_final, id.vars = c("ano","id_municipio"), 
                        measure.vars = c("Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                         "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                         "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                         "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                         "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                         "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                         "Maçã", "Mamão", "Mangua", "Maracujá",
                                         "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                         "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                         "Uva"))%>%
  rename(rend_medio = value, produto = variable)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$
#VALOR DA PRODUÇÃO

#QUINZE PRIMEIROS ANOS - 2019 A 2004
valor_prod1 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/valorprod/v1.csv", header=FALSE, comment.char="#")
#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
valor_prod1 <- valor_prod1%>%
  filter(!V2 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007"))


#PERIODO SEGUINTE - 2003 A 1988
valor_prod2 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/valorprod/v2.csv", header=FALSE, comment.char="#")

#tirando os valores que não são observações
valor_prod2<-valor_prod2%>%
  filter(!V2 != c( "2006",  "2005",  "2004", "2003", "2002", "2001", "2000", "1999", "1998",
                   "1997","1996","1995","1994"))

#PERIODO SEGUINTE - 1987 A 1974
valor_prod3 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/valorprod/v3.csv", header=FALSE, comment.char="#")

valor_prod3<-valor_prod3%>%
  filter(!V2 != c("1993","1992", "1991","1990","1989", "1988","1987","1986","1985", 
                  "1984", "1983", "1982", "1981"))

#PERIODO FINAL - 1976 A 1974
valor_prod4 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/valorprod/v4.csv", header=FALSE, comment.char="#")

valor_prod4<- valor_prod4%>%
  filter(!V2 != c("1980", "1979", "1978", "1977", "1976", "1975", "1974"))


valor_prod_final <- valor_prod1%>%
  add_row(valor_prod2)%>%
  add_row(valor_prod3)%>%
  add_row(valor_prod4)

#renomeando com o valor de area plantada de cada produto
colnames(valor_prod_final)<- c("id_municipio","ano","Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                               "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                               "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                               "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                               "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                               "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                               "Maçã", "Mamão", "Mangua", "Maracujá",
                               "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                               "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                               "Uva")

#criando uma variavel que identifique qual produto é qual
valor_prod_final<- melt(a_colhida_final, id.vars = c("ano","id_municipio"), 
                        measure.vars = c("Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                         "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                         "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                         "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                         "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                         "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                         "Maçã", "Mamão", "Mangua", "Maracujá",
                                         "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                         "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                         "Uva"))%>%
  rename(valor_prod = value, produto = variable)

#$$$$$$$$$$$$$$$$$
#PORCENTAGEM DO VALOR DA PRODUÇÃO

#QUINZE PRIMEIROS ANOS - 2019 A 2004
perc_valor_prod1 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percvalorprod/w1.csv", header=FALSE, comment.char="#")
#TIRANDO VALORES QUE NÃO SÃO OBSERVAÇÕES DO CSV
perc_valor_prod1 <- perc_valor_prod1%>%
  filter(!V2 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007"))


#PERIODO SEGUINTE - 2003 A 1988
perc_valor_prod2 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percvalorprod/w2.csv", header=FALSE, comment.char="#")

#tirando os valores que não são observações
perc_valor_prod2<-perc_valor_prod2%>%
  filter(!V2 != c( "2006",  "2005",  "2004","2003", "2002", "2001", "2000", "1999", "1998",
                   "1997","1996","1995","1994"))

#PERIODO SEGUINTE - 1987 A 1974
perc_valor_prod3 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percvalorprod/w3.csv", header=FALSE, comment.char="#")

perc_valor_prod3<-perc_valor_prod3%>%
  filter(!V2 != c("1993","1992","1991","1990","1989", "1988","1987","1986","1985", "1984", "1983", "1982", "1981"))

#PERIODO FINAL - 1976 A 1974
perc_valor_prod4 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro2/percvalorprod/w4.csv", header=FALSE, comment.char="#")

perc_valor_prod4<- perc_valor_prod4%>%
  filter(!V2 != c("1980", "1979", "1978", "1977","1976", "1975", "1974" ))




perc_valor_prod_final <- perc_valor_prod1%>%
  add_row(perc_valor_prod2)%>%
  add_row(perc_valor_prod3)%>%
  add_row(perc_valor_prod4)

#renomeando com o valor de area plantada de cada produto
colnames(perc_valor_prod_final)<- c("id_municipio","ano","Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                    "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                    "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                    "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                    "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                    "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                    "Maçã", "Mamão", "Mangua", "Maracujá",
                                    "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                    "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                    "Uva")

#criando uma variavel que identifique qual produto é qual
perc_valor_prod_final<- melt(a_colhida_final, id.vars = c("ano","id_municipio"), 
                             measure.vars = c("Abacate", "Algodão arbóreo (em caroço)", "Açaí","Azeitona",
                                              "Banana (cacho)", "Borracha (látex coagulado)", "Borracha (látex líquido)", "Cacau (em amêndoa)",
                                              "Café (em grão) Total", "Café (em grão) Arábica", "Café (em grão) Canephora",
                                              "Caju", "Caqui", "Castanha de caju", "Chá-da-índia (folha verde)",
                                              "Coco-da-baía", "Dendê (cacho de coco)", "Erva-mate (folha verde)", "Figo",
                                              "Goiaba", "Guaraná (semente)", "Laranja", "Limão",
                                              "Maçã", "Mamão", "Mangua", "Maracujá",
                                              "Marmelo", "Noz (fruto seco)", "Palmito", "Pera", "Pêssego", "Pimenta-do-reino",
                                              "Sisal ou agave (fibra)", "Tangerina", "Tungue (fruto seco)", "Urucum (semente)",
                                              "Uva"))%>%
  rename(perc_valor_prod = value, produto = variable)








#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#AGLOMERANDO TODAS AS BASES
agricola_final<-a_plantada_final%>%
  inner_join(perca_plantada_final, by = c('ano', 'id_municipio', 'produto'))%>%
  inner_join(a_colhida_final, by = c('ano', 'id_municipio', 'produto'))%>%
  inner_join(perca_colhida_final, by = c('ano', 'id_municipio', 'produto'))%>%
  inner_join(qtd_produzida_final, by = c('ano', 'id_municipio', 'produto'))%>%
  inner_join(rend_medio_final, by = c('ano', 'id_municipio', 'produto'))%>%
  inner_join(valor_prod_final, by = c('ano', 'id_municipio', 'produto'))%>%
  inner_join(perc_valor_prod_final, by = c('ano', 'id_municipio', 'produto'))%>%
  mutate(tipo_de_lavoura = "temporária")


#CHECAGENS NA BASE FINAL















