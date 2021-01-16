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


#primeiro abrindo a populacao residente em minas gerais - estimativas do IBGE para 2020
pop_estimada <- read.csv("C:/Users/Matheus/Desktop/pop_estimada.csv", header=FALSE)%>%
  filter( str_starts(V1, pattern = "31"))%>%
  rename(id_municipio = V1, municipio = V2, pop2020 = V3)


#abrindo numero de leitos publicos e privados
leitos_municipio <- read_excel("leitos_pub_priv_municipio.xlsx")

  colnames(leitos_municipio)<- c("municipio", "adm_federal", "adm_estadual", "adm_municipal", "adm_pub_outras",
"sociedade_mista", "empresarial", "s_fim_lucrativo", "total")

#quero substituir os - por 0
str_replace_all(string = c(leitos_municipio$adm_federal,leitos_municipio$adm_estadual, 
                           leitos_municipio$adm_municipal, leitos_municipio$adm_pub_outras, 
                           leitos_municipio$empresarial,
                           leitos_municipio$s_fim_lucrativo, 
                           leitos_municipio$sociedade_mista) , pattern =  "-", replacement = NA_character_)
 



 mutate(pub = adm_federal +adm_estadual +adm_municipal +adm_pub_outras,
         priv = empresarial + s_fim_lucrativo + sociedade_mista)
  
  c(adm_federal,adm_estadual, adm_municipal, adm_pub_outras, empresarial,
    s_fim_lucrativo, sociedade_mista)


























#mortalidade por CID juiz de fora
jf_por_cid_ano <- read_excel("C:/Users/Matheus/Desktop/mort_por_cid_jf.xlsx")

jf_por_cid_ano2 <- jf_por_cid_ano%>%
  rename(causa = `Categoria CID-10`)%>%
  mutate(cid = str_sub(causa,1,3))




jf_por_cid_ano2<- melt(jf_por_cid_ano2, id.vars = c("cid"), 
                        measure.vars = c("2012", "2013", "2014", "2015", "2016", "2017",
                                         "2018", "2019", "Total"))


dengue <- jf_por_cid_ano2%>%
  filter(cid == 'A90')