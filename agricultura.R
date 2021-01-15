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

#AREA PLANTADA

################################
#quinze primeiros anos
a_plantada_19_09 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

#tirando os valores que não são observações
a_plantada_19_04<- a_plantada_19_09%>%
  filter(!V1 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009",
                  "2010", "2009", "2008", "2007",  "2006",  "2005",  "2004"))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_19_09, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
  "Cana para forragem", "Cebola", "Centeio (em grão)",
  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
  "Sorgo (em grão)",
  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))

#criando uma variavel que identifique qual produto é qual
a_plantada_19_09<- melt(a_plantada_19_09, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))%>%
  rename(area_plantada = value, produto = variable)%>%
  mutate(ano = '2019')%>%
  rename(id_municipio = "Cód.",
         nome_municipio = "Município",
  )
  


##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#dez ano seguintes - 2008 a 1998
a_plantada_18_08 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

#tirando os valores que não são observações
a_plantada_18_08<- a_plantada_19_09%>%
  filter(!V1 != c("2008","2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000", "1999", "1998" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_18_08, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))

#criando uma variavel que identifique qual produto é qual
a_plantada_18_08<- melt(a_plantada_18_08, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))%>%
  rename(area_plantada = value, produto = variable)%>%
  mutate(ano = '2019')%>%
  rename(id_municipio = "Cód.",
         nome_municipio = "Município")


##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#dez ano seguintes - 1997 a 1987

a_plantada_97_87 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

a_plantada_97_87<- a_plantada_97_87%>%
  filter(!V1 != c("1997","1996", "1995", "1994", "1993", "1992", "1991", "1990", "1989", "1988", "1987" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_18_08, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


a_plantada_18_08<- melt(a_plantada_18_08, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))%>%
  rename(area_plantada = value, produto = variable)%>%
  mutate(ano = '2019')%>%
  rename(id_municipio = "Cód.",
         nome_municipio = "Município")




#dez ano seguintes - 1986 a 1976

a_plantada_86_76 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

a_plantada_86_76<- a_plantada_86_76%>%
  filter(!V1 != c("1986","1985", "1984", "1983", "1982", "1981", "1980", "1979", "1978", "1977", "1976" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_86_76, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


a_plantada_86_76<- melt(a_plantada_86_76, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


#anos iniciais

a_plantada_75_74 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

a_plantada_75_74<- a_plantada_75_74%>%
  filter(!V1 != c("1975", "1974" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_75_74, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


a_plantada_75_74<- melt(a_plantada_75_74, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))

###############

#PERCENTUAL DA AREA PLANTADA 

#############################
#dez primeiros anos
a_plantada_19_09 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/percaplantada/", header=FALSE, comment.char="#")

#tirando os valores que não são observações
a_plantada_19_09<- a_plantada_19_09%>%
  filter(!V1 != c("2019","2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_19_09, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))

#criando uma variavel que identifique qual produto é qual
a_plantada_19_09<- melt(a_plantada_19_09, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))




#dez ano seguintes - 2008 a 1998
a_plantada_18_08 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

#tirando os valores que não são observações
a_plantada_18_08<- a_plantada_19_09%>%
  filter(!V1 != c("2008","2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000", "1999", "1998" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_18_08, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))

#criando uma variavel que identifique qual produto é qual
a_plantada_18_08<- melt(a_plantada_18_08, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


#dez ano seguintes - 1997 a 1987

a_plantada_97_87 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

a_plantada_97_87<- a_plantada_97_87%>%
  filter(!V1 != c("1997","1996", "1995", "1994", "1993", "1992", "1991", "1990", "1989", "1988", "1987" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_18_08, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


a_plantada_18_08<- melt(a_plantada_18_08, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))




#dez ano seguintes - 1986 a 1976

a_plantada_86_76 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

a_plantada_86_76<- a_plantada_86_76%>%
  filter(!V1 != c("1986","1985", "1984", "1983", "1982", "1981", "1980", "1979", "1978", "1977", "1976" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_86_76, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


a_plantada_86_76<- melt(a_plantada_86_76, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


#anos iniciais

a_plantada_75_74 <- read.csv("C:/Users/Matheus/Desktop/bdmais/agro/aplantada/", header=FALSE, comment.char="#")

a_plantada_75_74<- a_plantada_75_74%>%
  filter(!V1 != c("1975", "1974" ))

#renomeando com o valor de area plantada de cada produto
`colnames<-`( a_plantada_75_74, c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                  "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                  "Cana para forragem", "Cebola", "Centeio (em grão)",
                                  "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                  "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                  "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                  "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                  "Sorgo (em grão)",
                                  "Tomate", "Trigo (em grão)", "Triticale (em grão)"))


a_plantada_75_74<- melt(a_plantada_75_74, id.vars = c("Cód.", "Município"), 
                        measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                         "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                         "Cana para forragem", "Cebola", "Centeio (em grão)",
                                         "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                         "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                         "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                         "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                         "Sorgo (em grão)",
                                         "Tomate", "Trigo (em grão)", "Triticale (em grão)"))













#estrategia antiquada
################


#deram errado
#######################



#variavel(1/7) = area plantada

#2019

areaplantada19 <- read_excel("C:/Users/Matheus/Desktop/bdmais/agro/area_plantada/areaplantada19.xlsx")

areaplantada19 <- melt(areaplantada19, id.vars = c("Cód.", "Município"), 
                       measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                        "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                        "Cana para forragem", "Cebola", "Centeio (em grão)",
                                        "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                        "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                        "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                        "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                        "Sorgo (em grão)",
                                        "Tomate", "Trigo (em grão)", "Triticale (em grão)"))

areaplantada19 <- areaplantada19%>%
  rename(area_plantada = value, produto = variable)%>%
  mutate(ano = '2019')%>%
  rename(id_municipio = "Cód.",
         nome_municipio = "Município",
         )


#2018

areaplantada18 <- read_excel("bdmais/agro/area_plantada/areaplantada18.xlsx")

areaplantada18 <- melt(areaplantada18, id.vars = c("Cód.", "Município"), 
                       measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                        "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                        "Cana para forragem", "Cebola", "Centeio (em grão)",
                                        "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                        "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                        "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                        "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                        "Sorgo (em grão)",
                                        "Tomate", "Trigo (em grão)", "Triticale (em grão)"))

areaplantada18 <- areaplantada18%>%
  rename(area_plantada = value, produto = variable)%>%
  mutate(ano = '2018')


#2017
areaplantada17 <- read_excel("bdmais/agro/area_plantada/areaplantada18.xlsx")

areaplantada17 <- melt(areaplantada17, id.vars = c("Cód.", "Município"), 
                       measure.vars = c("Abacaxi", "Alho", "Alfafa fenada","Amendoim (em casca)",
                                        "Arroz (em casca)", "Aveia (em grão)", "Batata-doce", "Cana-de-açúcar",
                                        "Cana para forragem", "Cebola", "Centeio (em grão)",
                                        "Cevada (em grão)", "Ervilha (em grão)", "Fava (em grão)", "Feijão (em grão)",
                                        "Fumo (em folha)", "Girassol (em grão)", "Juta (fibra)", "Linho (semente)",
                                        "Malva (fibra)", "Mamona (baga)", "Mandioca", "Melancia",
                                        "Melão", "Milho (em grão)", "Rami (fibra)", "Soja (em grão)",
                                        "Sorgo (em grão)",
                                        "Tomate", "Trigo (em grão)", "Triticale (em grão)"))

areaplantada17 <- areaplantada17%>%
  rename(area_plantada = value, produto = variable)%>%
  mutate(ano = '2017')


#variavel(2/7) = area 









#############