
#URL: http://www.portaldatransparencia.gov.br/downloads/mensal.asp?c=SeguroDefeso

## Diretorio do PC OMT
#setwd('C:\\Users\\omtmaranhao\\Documents\\pesca\\seguro_defeso')
#getwd()

## Diretorio do Notebook
setwd('C:/Users/jadso.FRANCISCOJADSON/Documents/OMT/pesca/seguro_defeso')
getwd()

library(dplyr)
library(tidyr)
library(stringr)
library(geobr)
library(cartography)
library(sf)
library(maps)
library(ggplot2)
library(ggthemes)
library(kableExtra)
library(formattable)

#verifocando todos os objetos
ls()

#limpando memória
rm(list = ls())

#diretorio <- 'C:\\Users\\omtmaranhao\\Documents\\pesca\\seguro_defeso\\'
#diretorio

diretorio <- 'C:/Users/jadso.FRANCISCOJADSON/Documents/OMT/pesca/seguro_defeso/'
diretorio

# Verificando todos os arquivos csv do diretório
arquivos <- list.files(path = diretorio,
                       pattern = '*.csv')
glimpse(arquivos)

for (i in 1:length(arquivos)){
    if(substr(arquivos[i], 1, 4) == '2014') {
        assign(
            paste(substr(arquivos[i], 8, 19), substr(arquivos[i], 1, 6), sep = ''), 
            read.delim2(paste(diretorio, arquivos[i], sep = '\\'), sep = ";",  dec = ",")
    )} 
}

SeguroDefeso201401_ma <- SeguroDefeso201401 %>% 
                        filter(UF == "MA")
SeguroDefeso201402_ma <- SeguroDefeso201402 %>% 
                        filter(UF == "MA")
SeguroDefeso201403_ma <- SeguroDefeso201403 %>% 
                        filter(UF == "MA")
SeguroDefeso201404_ma <- SeguroDefeso201404 %>% 
                        filter(UF == "MA")
SeguroDefeso201405_ma <- SeguroDefeso201405 %>% 
                        filter(UF == "MA")
SeguroDefeso201406_ma <- SeguroDefeso201406 %>% 
                        filter(UF == "MA")
SeguroDefeso201407_ma <- SeguroDefeso201407 %>% 
                        filter(UF == "MA")
SeguroDefeso201408_ma <- SeguroDefeso201408 %>% 
                        filter(UF == "MA")
SeguroDefeso201409_ma <- SeguroDefeso201409 %>% 
                        filter(UF == "MA")
SeguroDefeso201410_ma <- SeguroDefeso201410 %>% 
                        filter(UF == "MA")
SeguroDefeso201411_ma <- SeguroDefeso201411 %>% 
                        filter(UF == "MA")
SeguroDefeso201412_ma <- SeguroDefeso201412 %>% 
                        filter(UF == "MA")


SeguroDefeso_total_2014_ma <-rbind (
                                SeguroDefeso201401_ma,
                                SeguroDefeso201402_ma,
                                SeguroDefeso201403_ma,
                                SeguroDefeso201404_ma,
                                SeguroDefeso201405_ma,
                                SeguroDefeso201406_ma,
                                SeguroDefeso201407_ma,
                                SeguroDefeso201408_ma,
                                SeguroDefeso201409_ma,
                                SeguroDefeso201410_ma,
                                SeguroDefeso201411_ma,
                                SeguroDefeso201412_ma
                                )

rm(
    SeguroDefeso201401,
    SeguroDefeso201402,
    SeguroDefeso201403,
    SeguroDefeso201404,
    SeguroDefeso201405,
    SeguroDefeso201406,
    SeguroDefeso201407,
    SeguroDefeso201408,
    SeguroDefeso201409,
    SeguroDefeso201410,
    SeguroDefeso201411,
    SeguroDefeso201412
    )

rm(
    SeguroDefeso201401_ma,
    SeguroDefeso201402_ma,
    SeguroDefeso201403_ma,
    SeguroDefeso201404_ma,
    SeguroDefeso201405_ma,
    SeguroDefeso201406_ma,
    SeguroDefeso201407_ma,
    SeguroDefeso201408_ma,
    SeguroDefeso201409_ma,
    SeguroDefeso201410_ma,
    SeguroDefeso201411_ma,
    SeguroDefeso201412_ma                               
    )

# visualizando
glimpse(SeguroDefeso_total_2014_ma)

n_distinct(SeguroDefeso_total_2014_ma$NIS.FAVORECIDO)

# Verificando quantidadde e proporção por município
top10_2014_distintos <- SeguroDefeso_total_2014_ma %>%
                        group_by(NOME.MUNICÍPIO) %>%
                        summarize(n = n_distinct(NIS.FAVORECIDO)) %>%
                        mutate(prop = n / sum(n), prop = scales::percent(prop)) %>%
                        arrange(desc(n))
                        
paste("Qauntidade de pescadores do estado que receberam Seguro Defeso em 2014", sum(top10_2014_distintos$n))

#  Os 10 municípios que mais tiveram pescadores benefíciados pelo Seguro Defeso no MA em 2014
top10_2014_distintos %>%
    top_n(10, n) %>%
    rename(Município = NOME.MUNICÍPIO) %>%
    rename("Quantidade de Pescadores" = n) %>%
    rename("Proporção" = prop)

# Quantidade de parcelas recebidas pelos pescadores maranhenses por município
parcelas2014 <- SeguroDefeso_total_2014_ma %>%
                count(NOME.MUNICÍPIO, sort = TRUE) %>%
                mutate(prop = n / sum(n), prop = scales::percent(prop))
                
parcelas2014 %>%
    top_n(10, n) %>%
    rename(Município = NOME.MUNICÍPIO) %>%
    rename("Quantidade de Parcelas" = n) %>%
    rename("Proporção" = prop)

total_pescadores_2014 <- SeguroDefeso_total_2014_ma %>%
                            n_distinct("NIS.FAVORECIDO")
total_pescadores_2014

# filtrando dados para São Luís
SeguroDefeso_2014_slz <- SeguroDefeso_total_2014_ma %>% 
                        filter(NOME.MUNICÍPIO == "SAO LUIS")
glimpse(SeguroDefeso_2014_slz)

# Quantidade de Pescadores que receberam Seguro Defeso em São Luís
total_pescadores_2014_slz <- SeguroDefeso_2014_slz %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")
total_pescadores_2014_slz

# Somando os valores mensais
SeguroDefeso_total_2014_slz <- SeguroDefeso_2014_slz %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2014_slz)

SeguroDefeso_total_2014_ma <- SeguroDefeso_total_2014_ma %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2014_ma)

SeguroDefeso_total_2014_ma_mes <- SeguroDefeso_total_2014_ma %>%
                                    group_by(MÊS.REFERÊNCIA) %>%
                                    summarise(TOTAL = sum(TOTAL))
SeguroDefeso_total_2014_ma_mes

total_pescadores_2014

SeguroDefeso_total_2014_ma_municipio <- SeguroDefeso_total_2014_ma %>% 
                        group_by(NOME.MUNICÍPIO) %>%
                        summarise(TOTAL = sum(TOTAL)) %>%
                        arrange(desc(TOTAL))
glimpse(SeguroDefeso_total_2014_ma_municipio)

# shape municipios maranhenses do pacote GEOBR
shape_mun_pc_2014 <- read_municipality(code_muni = 'MA', year = 2018) # lendo shape
glimpse(shape_mun_pc_2014)

shape_mun_pc_2014$name_muni <- as.character(str_to_upper(shape_mun_pc_2014$name_muni))

shape_mun_pc_2014$name_muni <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", shape_mun_pc_2014$name_muni)

SeguroDefeso_total_2014_ma_municipio$NOME.MUNICÍPIO <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", SeguroDefeso_total_2014_ma_municipio$NOME.MUNICÍPIO)
glimpse(SeguroDefeso_total_2014_ma_municipio)

# Adicionando municipios marenhenses com valores ausentes `a shape

nova_linha <- c(0, 0)


for (i in 1:length(shape_mun_pc_2014$name_muni)) {
    if (shape_mun_pc_2014$name_muni[i]  %in% SeguroDefeso_total_2014_ma_municipio$NOME.MUNICÍPIO) {
        
    } else {
        nova_linha[1]= shape_mun_pc_2014$name_muni[i]
        nova_linha[2] = NA
        SeguroDefeso_total_2014_ma_municipio = rbind(SeguroDefeso_total_2014_ma_municipio, nova_linha) 
    }
}
glimpse(SeguroDefeso_total_2014_ma_municipio)

shape_mun_pc_2014 <- right_join(shape_mun_pc_2014, SeguroDefeso_total_2014_ma_municipio, by = c('name_muni' = 'NOME.MUNICÍPIO')) # juntando os dados
glimpse(shape_mun_pc_2014)

shape_mun_pc_2014$TOTAL <- as.double(shape_mun_pc_2014$TOTAL)
glimpse(shape_mun_pc_2014)

shape_mun_pc_2014 <- shape_mun_pc_2014 %>% 
    mutate(TOTAL_milhoes = round(TOTAL/1000000, 2))
glimpse(shape_mun_pc_2014)

max(shape_mun_pc_2014$TOTAL_milhoes, na.rm = T)
min(shape_mun_pc_2014$TOTAL_milhoes, na.rm = T)

# shape
plot(st_geometry(shape_mun_pc_2014))

# caixa 
box()

# mapas coropléticos
choroLayer(
    x = shape_mun_pc_2014, 
    var = "TOTAL_milhoes",  
    method = "arith", 
    nclass = 5,
    col = carto.pal(pal1 = "red.pal", n1 = 5),
    breaks = c(0, 4, 8, 12, 16, 20),
    legend.title.txt = "Valor  (em milhões R$)",
    legend.values.rnd = 2,
    legend.pos = "topleft",
    legend.nodata = "sem dados",
    add = TRUE)


# layout
layoutLayer(title = "Seguro Defeso - Maranhão (2014)", 
            sources = "",
            author = "Elaboração: OMT-MA\nFonte: Portal da Transparência", 
            frame = FALSE, 
            north = FALSE, 
            tabtitle = FALSE,
            col = "black",
            coltitle = "white",
            postitle = "center",
           scale = FALSE) 

# norte
north(pos = "topright")

# escala
map.scale(x = -42.79000, y = -9.84000, relwidth=0.15, ratio=FALSE)

for (i in 1:length(arquivos)){
    if(substr(arquivos[i], 1, 4) == '2015') {
        assign(
            paste(substr(arquivos[i], 8, 19), substr(arquivos[i], 1, 6), sep = ''), 
            read.delim2(paste(diretorio, arquivos[i], sep = '\\'), sep = ";",  dec = ",")
        )} 
}

SeguroDefeso201501_ma <- SeguroDefeso201501 %>% 
                        filter(UF == "MA")
SeguroDefeso201502_ma <- SeguroDefeso201502 %>% 
                        filter(UF == "MA")
SeguroDefeso201503_ma <- SeguroDefeso201503 %>% 
                        filter(UF == "MA")
SeguroDefeso201504_ma <- SeguroDefeso201504 %>% 
                        filter(UF == "MA")
SeguroDefeso201505_ma <- SeguroDefeso201505 %>% 
                        filter(UF == "MA")
SeguroDefeso201506_ma <- SeguroDefeso201506 %>% 
                        filter(UF == "MA")
SeguroDefeso201507_ma <- SeguroDefeso201507 %>% 
                        filter(UF == "MA")
SeguroDefeso201508_ma <- SeguroDefeso201508 %>% 
                        filter(UF == "MA")
SeguroDefeso201509_ma <- SeguroDefeso201509 %>% 
                        filter(UF == "MA")
SeguroDefeso201510_ma <- SeguroDefeso201510 %>% 
                        filter(UF == "MA")
SeguroDefeso201511_ma <- SeguroDefeso201511 %>% 
                        filter(UF == "MA")
SeguroDefeso201512_ma <- SeguroDefeso201512 %>% 
                        filter(UF == "MA")


SeguroDefeso_total_2015_ma <-rbind (
                                SeguroDefeso201501_ma,
                                SeguroDefeso201502_ma,
                                SeguroDefeso201503_ma,
                                SeguroDefeso201504_ma,
                                SeguroDefeso201505_ma,
                                SeguroDefeso201506_ma,
                                SeguroDefeso201507_ma,
                                SeguroDefeso201508_ma,
                                SeguroDefeso201509_ma,
                                SeguroDefeso201510_ma,
                                SeguroDefeso201511_ma,
                                SeguroDefeso201512_ma
                                )

rm(
    SeguroDefeso201501,
    SeguroDefeso201502,
    SeguroDefeso201503,
    SeguroDefeso201504,
    SeguroDefeso201505,
    SeguroDefeso201506,
    SeguroDefeso201507,
    SeguroDefeso201508,
    SeguroDefeso201509,
    SeguroDefeso201510,
    SeguroDefeso201511,
    SeguroDefeso201512
    )

rm(
    SeguroDefeso201501_ma,
    SeguroDefeso201502_ma,
    SeguroDefeso201503_ma,
    SeguroDefeso201504_ma,
    SeguroDefeso201505_ma,
    SeguroDefeso201506_ma,
    SeguroDefeso201507_ma,
    SeguroDefeso201508_ma,
    SeguroDefeso201509_ma,
    SeguroDefeso201510_ma,
    SeguroDefeso201511_ma,
    SeguroDefeso201512_ma
    )

glimpse(SeguroDefeso_total_2015_ma)

n_distinct(SeguroDefeso_total_2015_ma$NIS.FAVORECIDO)

# Verificando quantidadde e proporção por município
top10_2015_distintos <- SeguroDefeso_total_2015_ma %>%
                        group_by(NOME.MUNICÍPIO) %>%
                        summarize(n = n_distinct(NIS.FAVORECIDO)) %>%
                        mutate(prop = n / sum(n), prop = scales::percent(prop)) %>%
                        arrange(desc(n))
                        
paste("Qauntidade de pescadores do estado que receberam Seguro Defeso em 2015", sum(top10_2015_distintos$n))

#  Os 10 municípios que mais tiveram pescadores benefíciados pelo Seguro Defeso no MA em 2015
top10_2015_distintos %>%
    top_n(10, n) %>%
    rename(Município = NOME.MUNICÍPIO) %>%
    rename("Quantidade de Pescadores" = n) %>%
    rename("Proporção" = prop)

# Quantidade de parcelas recebidas pelos pescadores maranhenses por município
parcelas2015 <- SeguroDefeso_total_2015_ma %>%
                count(NOME.MUNICÍPIO, sort = TRUE) %>%
                mutate(prop = n / sum(n), prop = scales::percent(prop))
                
parcelas2015 %>%
    top_n(10, n) %>%
    rename(Município = NOME.MUNICÍPIO) %>%
    rename("Quantidade de Parcelas" = n) %>%
    rename("Proporção" = prop)

total_pescadores_2015 <- SeguroDefeso_total_2015_ma %>%
                            n_distinct("NIS.FAVORECIDO")
total_pescadores_2015

total_pescadores_2015

SeguroDefeso_2015_slz <- SeguroDefeso_total_2015_ma %>% 
                        filter(NOME.MUNICÍPIO == "SAO LUIS")
glimpse(SeguroDefeso_2015_slz)

total_pescadores_2015_slz <- SeguroDefeso_2015_slz %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")
total_pescadores_2015_slz

SeguroDefeso_total_2015_slz <- SeguroDefeso_2015_slz %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2015_slz)

SeguroDefeso_total_2015_ma <- SeguroDefeso_total_2015_ma %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2015_ma)

SeguroDefeso_total_2015_ma_mes <- SeguroDefeso_total_2015_ma %>%
                                    group_by(MÊS.REFERÊNCIA) %>%
                                    summarise(TOTAL = sum(TOTAL))
SeguroDefeso_total_2015_ma_mes

SeguroDefeso_total_2015_ma_municipio <- SeguroDefeso_total_2015_ma %>% 
                        group_by(NOME.MUNICÍPIO) %>%
                        summarise(TOTAL = sum(TOTAL)) %>%
                        arrange(desc(TOTAL))
glimpse(SeguroDefeso_total_2015_ma_municipio)

# shape municipios maranhenses do pacote GEOBR
shape_mun_pc_2015 <- read_municipality(code_muni = 'MA', year = 2018) # lendo shape
glimpse(shape_mun_pc_2015)

shape_mun_pc_2015$name_muni <- as.character(str_to_upper(shape_mun_pc_2015$name_muni))

shape_mun_pc_2015$name_muni <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", shape_mun_pc_2015$name_muni)
glimpse(shape_mun_pc_2015)

SeguroDefeso_total_2015_ma_municipio$NOME.MUNICÍPIO <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", SeguroDefeso_total_2015_ma_municipio$NOME.MUNICÍPIO)
glimpse(SeguroDefeso_total_2015_ma_municipio)

# Adicionando municipios marenhenses com valores ausentes `a shape

nova_linha <- c(0, 0)


for (i in 1:length(shape_mun_pc_2015$name_muni)) {
    if (shape_mun_pc_2015$name_muni[i]  %in% SeguroDefeso_total_2015_ma_municipio$NOME.MUNICÍPIO) {
        
    } else {
        nova_linha[1]= shape_mun_pc_2015$name_muni[i]
        nova_linha[2] = NA
        SeguroDefeso_total_2015_ma_municipio = rbind(SeguroDefeso_total_2015_ma_municipio, nova_linha) 
    }
}
glimpse(SeguroDefeso_total_2015_ma_municipio)

shape_mun_pc_2015 <- right_join(shape_mun_pc_2015, SeguroDefeso_total_2015_ma_municipio, by = c('name_muni' = 'NOME.MUNICÍPIO')) # juntando os dados
glimpse(shape_mun_pc_2015)

shape_mun_pc_2015$TOTAL <- as.double(shape_mun_pc_2015$TOTAL)
glimpse(shape_mun_pc_2015)

shape_mun_pc_2015 <- shape_mun_pc_2015 %>% 
    mutate(TOTAL_milhoes = round(TOTAL/1000000, 2))
glimpse(shape_mun_pc_2015)

max(shape_mun_pc_2015$TOTAL_milhoes, na.rm = T)
min(shape_mun_pc_2015$TOTAL_milhoes, na.rm = T)

# shape
plot(st_geometry(shape_mun_pc_2015))

# caixa 
box()

# mapas coropléticos
choroLayer(
    x = shape_mun_pc_2015, 
    var = "TOTAL_milhoes",  
    method = "arith", 
    nclass = 5,
    col = carto.pal(pal1 = "red.pal", n1 = 5),
    breaks = c(0, 4, 8, 12, 16, 20),
    legend.title.txt = "Valor  (em milhões R$)",
    legend.values.rnd = 2,
    legend.pos = "topleft",
    legend.nodata = "sem dados",
    add = TRUE)


# layout
layoutLayer(title = "Seguro Defeso - Maranhão (2015)", 
            sources = "",
            author = "Elaboração: OMT-MA\nFonte: Portal da Transparência", 
            frame = FALSE, 
            north = FALSE, 
            tabtitle = FALSE,
            col = "black",
            coltitle = "white",
            postitle = "center",
           scale = FALSE) 

# norte
north(pos = "topright")

# escala
map.scale(x = -42.79000, y = -9.84000, relwidth=0.15, ratio=FALSE)

for (i in 1:length(arquivos)){
    if(substr(arquivos[i], 1, 4) == '2016') {
        assign(
            paste(substr(arquivos[i], 8, 19), substr(arquivos[i], 1, 6), sep = ''), 
            read.delim2(paste(diretorio, arquivos[i], sep = '\\'), sep = ";",  dec = ",")
        )} 
}

SeguroDefeso201601_ma <- SeguroDefeso201601 %>% 
                        filter(UF == "MA")
SeguroDefeso201602_ma <- SeguroDefeso201602 %>% 
                        filter(UF == "MA")
SeguroDefeso201603_ma <- SeguroDefeso201603 %>% 
                        filter(UF == "MA")
SeguroDefeso201604_ma <- SeguroDefeso201604 %>% 
                        filter(UF == "MA")
SeguroDefeso201605_ma <- SeguroDefeso201605 %>% 
                        filter(UF == "MA")
SeguroDefeso201606_ma <- SeguroDefeso201606 %>% 
                        filter(UF == "MA")
SeguroDefeso201607_ma <- SeguroDefeso201607 %>% 
                        filter(UF == "MA")
SeguroDefeso201608_ma <- SeguroDefeso201608 %>% 
                        filter(UF == "MA")
SeguroDefeso201609_ma <- SeguroDefeso201609 %>% 
                        filter(UF == "MA")
SeguroDefeso201610_ma <- SeguroDefeso201610 %>% 
                        filter(UF == "MA")
SeguroDefeso201611_ma <- SeguroDefeso201611 %>% 
                        filter(UF == "MA")
SeguroDefeso201612_ma <- SeguroDefeso201612 %>% 
                        filter(UF == "MA")


SeguroDefeso_total_2016_ma <-rbind (
                                SeguroDefeso201601_ma,
                                SeguroDefeso201602_ma,
                                SeguroDefeso201603_ma,
                                SeguroDefeso201604_ma,
                                SeguroDefeso201605_ma,
                                SeguroDefeso201606_ma,
                                SeguroDefeso201607_ma,
                                SeguroDefeso201608_ma,
                                SeguroDefeso201609_ma,
                                SeguroDefeso201610_ma,
                                SeguroDefeso201611_ma,
                                SeguroDefeso201612_ma
                                )

rm(
    SeguroDefeso201601,
    SeguroDefeso201602,
    SeguroDefeso201603,
    SeguroDefeso201604,
    SeguroDefeso201605,
    SeguroDefeso201606,
    SeguroDefeso201607,
    SeguroDefeso201608,
    SeguroDefeso201609,
    SeguroDefeso201610,
    SeguroDefeso201611,
    SeguroDefeso201612
    )

rm(
    SeguroDefeso201601_ma,
    SeguroDefeso201602_ma,
    SeguroDefeso201603_ma,
    SeguroDefeso201604_ma,
    SeguroDefeso201605_ma,
    SeguroDefeso201606_ma,
    SeguroDefeso201607_ma,
    SeguroDefeso201608_ma,
    SeguroDefeso201609_ma,
    SeguroDefeso201610_ma,
    SeguroDefeso201611_ma,
    SeguroDefeso201612_ma
    )

glimpse(SeguroDefeso_total_2016_ma)

SeguroDefeso_2016_slz <- SeguroDefeso_total_2016_ma %>% 
                        filter(NOME.MUNICÍPIO == "SAO LUIS")
glimpse(SeguroDefeso_2016_slz)

total_pescadores_2016_slz <- SeguroDefeso_2016_slz %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")
total_pescadores_2014_slz

SeguroDefeso_total_2016_slz <- SeguroDefeso_2016_slz %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2016_slz)

total_pescadores_2016 <- SeguroDefeso_total_2016_ma %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")

total_pescadores_2016

SeguroDefeso_total_2016_ma <- SeguroDefeso_total_2016_ma %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2016_ma)

SeguroDefeso_total_2016_ma_mes <- SeguroDefeso_total_2016_ma %>%
                                    group_by(MÊS.REFERÊNCIA) %>%
                                    summarise(TOTAL = sum(TOTAL))
SeguroDefeso_total_2016_ma_mes

SeguroDefeso_total_2016_ma_municipio <- SeguroDefeso_total_2016_ma %>% 
                        group_by(NOME.MUNICÍPIO) %>%
                        summarise(TOTAL = sum(TOTAL)) %>%
                        arrange(desc(TOTAL))
glimpse(SeguroDefeso_total_2016_ma_municipio)

# shape municipios maranhenses do pacote GEOBR
shape_mun_pc_2016 <- read_municipality(code_muni = 'MA', year = 2018) # lendo shape
glimpse(shape_mun_pc_2016)

shape_mun_pc_2016$name_muni <- as.character(str_to_upper(shape_mun_pc_2016$name_muni))

shape_mun_pc_2016$name_muni <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", shape_mun_pc_2016$name_muni)
glimpse(shape_mun_pc_2016)

SeguroDefeso_total_2016_ma_municipio$NOME.MUNICÍPIO <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", SeguroDefeso_total_2016_ma_municipio$NOME.MUNICÍPIO)
glimpse(SeguroDefeso_total_2016_ma_municipio)

# Adicionando municipios marenhenses com valores ausentes `a shape

nova_linha <- c(0, 0)


for (i in 1:length(shape_mun_pc_2016$name_muni)) {
    if (shape_mun_pc_2016$name_muni[i]  %in% SeguroDefeso_total_2016_ma_municipio$NOME.MUNICÍPIO) {
        
    } else {
        nova_linha[1]= shape_mun_pc_2016$name_muni[i]
        nova_linha[2] = NA
        SeguroDefeso_total_2016_ma_municipio = rbind(SeguroDefeso_total_2016_ma_municipio, nova_linha) 
    }
}
glimpse(SeguroDefeso_total_2016_ma_municipio)

shape_mun_pc_2016 <- right_join(shape_mun_pc_2016, SeguroDefeso_total_2016_ma_municipio, by = c('name_muni' = 'NOME.MUNICÍPIO')) # juntando os dados
glimpse(shape_mun_pc_2016)

shape_mun_pc_2016$TOTAL <- as.double(shape_mun_pc_2016$TOTAL)
glimpse(shape_mun_pc_2016)

shape_mun_pc_2016 <- shape_mun_pc_2016 %>% 
    mutate(TOTAL_milhoes = round(TOTAL/1000000, 2))
glimpse(shape_mun_pc_2016)

max(shape_mun_pc_2016$TOTAL_milhoes, na.rm = T)
min(shape_mun_pc_2016$TOTAL_milhoes, na.rm = T)

# shape
plot(st_geometry(shape_mun_pc_2016))

# caixa 
box()

# mapas coropléticos
choroLayer(
    x = shape_mun_pc_2016, 
    var = "TOTAL_milhoes",  
    method = "arith", 
    nclass = 5,
    col = carto.pal(pal1 = "red.pal", n1 = 5),
    breaks = c(0, 4, 8, 12, 16, 20),
    legend.title.txt = "Valor  (em milhões R$)",
    legend.values.rnd = 2,
    legend.pos = "topleft",
    legend.nodata = "sem dados",
    add = TRUE)


# layout
layoutLayer(title = "Seguro Defeso - Maranhão (2016)", 
            sources = "",
            author = "Elaboração: OMT-MA\nFonte: Portal da Transparência", 
            frame = FALSE, 
            north = FALSE, 
            tabtitle = FALSE,
            col = "black",
            coltitle = "white",
            postitle = "center",
           scale = FALSE) 

# norte
north(pos = "topright")

# escala
map.scale(x = -42.79000, y = -9.84000, relwidth=0.15, ratio=FALSE)

for (i in 1:length(arquivos)){
    if(substr(arquivos[i], 1, 4) == '2017') {
        assign(
            paste(substr(arquivos[i], 8, 19), substr(arquivos[i], 1, 6), sep = ''), 
            read.delim2(paste(diretorio, arquivos[i], sep = '\\'), sep = ";",  dec = ",")
        )} 
}

SeguroDefeso201701_ma <- SeguroDefeso201701 %>% 
                        filter(UF == "MA")
SeguroDefeso201702_ma <- SeguroDefeso201702 %>% 
                        filter(UF == "MA")
SeguroDefeso201703_ma <- SeguroDefeso201703 %>% 
                        filter(UF == "MA")
SeguroDefeso201704_ma <- SeguroDefeso201704 %>% 
                        filter(UF == "MA")
SeguroDefeso201705_ma <- SeguroDefeso201705 %>% 
                        filter(UF == "MA")
SeguroDefeso201706_ma <- SeguroDefeso201706 %>% 
                        filter(UF == "MA")
SeguroDefeso201707_ma <- SeguroDefeso201707 %>% 
                        filter(UF == "MA")
SeguroDefeso201708_ma <- SeguroDefeso201708 %>% 
                        filter(UF == "MA")
SeguroDefeso201709_ma <- SeguroDefeso201709 %>% 
                        filter(UF == "MA")
SeguroDefeso201710_ma <- SeguroDefeso201710 %>% 
                        filter(UF == "MA")
SeguroDefeso201711_ma <- SeguroDefeso201711 %>% 
                        filter(UF == "MA")
SeguroDefeso201712_ma <- SeguroDefeso201712 %>% 
                        filter(UF == "MA")


SeguroDefeso_total_2017_ma <-rbind (
                                SeguroDefeso201701_ma,
                                SeguroDefeso201702_ma,
                                SeguroDefeso201703_ma,
                                SeguroDefeso201704_ma,
                                SeguroDefeso201705_ma,
                                SeguroDefeso201706_ma,
                                SeguroDefeso201707_ma,
                                SeguroDefeso201708_ma,
                                SeguroDefeso201709_ma,
                                SeguroDefeso201710_ma,
                                SeguroDefeso201711_ma,
                                SeguroDefeso201712_ma
                                )

rm(
    SeguroDefeso201701,
    SeguroDefeso201702,
    SeguroDefeso201703,
    SeguroDefeso201704,
    SeguroDefeso201705,
    SeguroDefeso201706,
    SeguroDefeso201707,
    SeguroDefeso201708,
    SeguroDefeso201709,
    SeguroDefeso201710,
    SeguroDefeso201711,
    SeguroDefeso201712
    )

rm(
    SeguroDefeso201701_ma,
    SeguroDefeso201702_ma,
    SeguroDefeso201703_ma,
    SeguroDefeso201704_ma,
    SeguroDefeso201705_ma,
    SeguroDefeso201706_ma,
    SeguroDefeso201707_ma,
    SeguroDefeso201708_ma,
    SeguroDefeso201709_ma,
    SeguroDefeso201710_ma,
    SeguroDefeso201711_ma,
    SeguroDefeso201712_ma                               
    )

glimpse(SeguroDefeso_total_2017_ma)

SeguroDefeso_2017_slz <- SeguroDefeso_total_2017_ma %>% 
                        filter(NOME.MUNICÍPIO == "SAO LUIS")
glimpse(SeguroDefeso_2017_slz)

total_pescadores_2017_slz <- SeguroDefeso_2017_slz %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")
total_pescadores_2017_slz

SeguroDefeso_total_2017_slz <- SeguroDefeso_2017_slz %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2017_slz)

total_pescadores_2017 <- SeguroDefeso_total_2017_ma %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")

total_pescadores_2017

SeguroDefeso_total_2017_ma <- SeguroDefeso_total_2017_ma %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2017_ma)

SeguroDefeso_total_2017_ma_mes <- SeguroDefeso_total_2017_ma %>%
                                    group_by(MÊS.REFERÊNCIA) %>%
                                    summarise(TOTAL = sum(TOTAL))
SeguroDefeso_total_2017_ma_mes

SeguroDefeso_total_2017_ma_municipio <- SeguroDefeso_total_2017_ma %>% 
                        group_by(NOME.MUNICÍPIO) %>%
                        summarise(TOTAL = sum(TOTAL)) %>%
                        arrange(desc(TOTAL))
glimpse(SeguroDefeso_total_2017_ma_municipio)

# shape municipios maranhenses do pacote GEOBR
shape_mun_pc_2017 <- read_municipality(code_muni = 'MA', year = 2018) # lendo shape
glimpse(shape_mun_pc_2017)

shape_mun_pc_2017$name_muni <- as.character(str_to_upper(shape_mun_pc_2017$name_muni))

shape_mun_pc_2017$name_muni <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", shape_mun_pc_2017$name_muni)

SeguroDefeso_total_2017_ma_municipio$NOME.MUNICÍPIO <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", SeguroDefeso_total_2017_ma_municipio$NOME.MUNICÍPIO)
glimpse(SeguroDefeso_total_2017_ma_municipio)

# Adicionando municipios marenhenses com valores ausentes `a shape

nova_linha <- c(0, 0)


for (i in 1:length(shape_mun_pc_2017$name_muni)) {
    if (shape_mun_pc_2017$name_muni[i]  %in% SeguroDefeso_total_2017_ma_municipio$NOME.MUNICÍPIO) {
        
    } else {
        nova_linha[1]= shape_mun_pc_2017$name_muni[i]
        nova_linha[2] = NA
        SeguroDefeso_total_2017_ma_municipio = rbind(SeguroDefeso_total_2017_ma_municipio, nova_linha) 
    }
}
glimpse(SeguroDefeso_total_2017_ma_municipio)

shape_mun_pc_2017 <- right_join(shape_mun_pc_2017, SeguroDefeso_total_2017_ma_municipio, by = c('name_muni' = 'NOME.MUNICÍPIO')) # juntando os dados
glimpse(shape_mun_pc_2017)

shape_mun_pc_2017$TOTAL <- as.double(shape_mun_pc_2017$TOTAL)
glimpse(shape_mun_pc_2017)

shape_mun_pc_2017 <- shape_mun_pc_2017 %>% 
    mutate(TOTAL_milhoes = round(TOTAL/1000000, 2))
glimpse(shape_mun_pc_2017)

max(shape_mun_pc_2017$TOTAL_milhoes, na.rm = T)
min(shape_mun_pc_2017$TOTAL_milhoes, na.rm = T)

# shape
plot(st_geometry(shape_mun_pc_2017))

# caixa 
box()

# mapas coropléticos
choroLayer(
    x = shape_mun_pc_2017, 
    var = "TOTAL_milhoes",  
    method = "arith", 
    nclass = 5,
    col = carto.pal(pal1 = "red.pal", n1 = 5),
    breaks = c(0, 4, 8, 12, 16, 20),
    legend.title.txt = "Valor  (em milhões R$)",
    legend.values.rnd = 2,
    legend.pos = "topleft",
    legend.nodata = "sem dados",
    add = TRUE)


# layout
layoutLayer(title = "Seguro Defeso - Maranhão (2017)", 
            sources = "",
            author = "Elaboração: OMT-MA\nFonte: Portal da Transparência", 
            frame = FALSE, 
            north = FALSE, 
            tabtitle = FALSE,
            col = "black",
            coltitle = "white",
            postitle = "center",
           scale = FALSE) 

# norte
north(pos = "topright")

# escala
map.scale(x = -42.79000, y = -9.84000, relwidth=0.15, ratio=FALSE)

for (i in 1:length(arquivos)){
    if(substr(arquivos[i], 1, 4) == '2018') {
        assign(
            paste(substr(arquivos[i], 8, 19), substr(arquivos[i], 1, 6), sep = ''), 
            read.delim2(paste(diretorio, arquivos[i], sep = '\\'), sep = ";",  dec = ",")
        )} 
}

SeguroDefeso201801_ma <- SeguroDefeso201801 %>% 
                        filter(UF == "MA")
SeguroDefeso201802_ma <- SeguroDefeso201802 %>% 
                        filter(UF == "MA")
SeguroDefeso201803_ma <- SeguroDefeso201803 %>% 
                        filter(UF == "MA")
SeguroDefeso201804_ma <- SeguroDefeso201804 %>% 
                        filter(UF == "MA")
SeguroDefeso201805_ma <- SeguroDefeso201805 %>% 
                        filter(UF == "MA")
SeguroDefeso201806_ma <- SeguroDefeso201806 %>% 
                        filter(UF == "MA")
SeguroDefeso201807_ma <- SeguroDefeso201807 %>% 
                        filter(UF == "MA")
SeguroDefeso201808_ma <- SeguroDefeso201808 %>% 
                        filter(UF == "MA")
SeguroDefeso201809_ma <- SeguroDefeso201809 %>% 
                        filter(UF == "MA")
SeguroDefeso201810_ma <- SeguroDefeso201810 %>% 
                        filter(UF == "MA")
SeguroDefeso201811_ma <- SeguroDefeso201811 %>% 
                        filter(UF == "MA")
SeguroDefeso201812_ma <- SeguroDefeso201812 %>% 
                        filter(UF == "MA")


SeguroDefeso_total_2018_ma <-rbind (
                                SeguroDefeso201801_ma,
                                SeguroDefeso201802_ma,
                                SeguroDefeso201803_ma,
                                SeguroDefeso201804_ma,
                                SeguroDefeso201805_ma,
                                SeguroDefeso201806_ma,
                                SeguroDefeso201807_ma,
                                SeguroDefeso201808_ma,
                                SeguroDefeso201809_ma,
                                SeguroDefeso201810_ma,
                                SeguroDefeso201811_ma,
                                SeguroDefeso201812_ma
                                )

rm(
    SeguroDefeso201801,
    SeguroDefeso201802,
    SeguroDefeso201803,
    SeguroDefeso201804,
    SeguroDefeso201805,
    SeguroDefeso201806,
    SeguroDefeso201807,
    SeguroDefeso201808,
    SeguroDefeso201809,
    SeguroDefeso201810,
    SeguroDefeso201811,
    SeguroDefeso201812
    )

rm(
    SeguroDefeso201801_ma,
    SeguroDefeso201802_ma,
    SeguroDefeso201803_ma,
    SeguroDefeso201804_ma,
    SeguroDefeso201805_ma,
    SeguroDefeso201806_ma,
    SeguroDefeso201807_ma,
    SeguroDefeso201808_ma,
    SeguroDefeso201809_ma,
    SeguroDefeso201810_ma,
    SeguroDefeso201811_ma,
    SeguroDefeso201812_ma                               
    )

glimpse(SeguroDefeso_total_2018_ma)

SeguroDefeso_2018_slz <- SeguroDefeso_total_2018_ma %>% 
                        filter(NOME.MUNICÍPIO == "SAO LUIS")
glimpse(SeguroDefeso_2018_slz)

total_pescadores_2018_slz <- SeguroDefeso_2018_slz %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")
total_pescadores_2018_slz

SeguroDefeso_total_2018_slz <- SeguroDefeso_2018_slz %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2018_slz)

total_pescadores_2018 <- SeguroDefeso_total_2018_ma %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")

total_pescadores_2018

SeguroDefeso_total_2018_ma <- SeguroDefeso_total_2018_ma %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2018_ma)

SeguroDefeso_total_2018_ma_mes <- SeguroDefeso_total_2018_ma %>%
                                    group_by(MÊS.REFERÊNCIA) %>%
                                    summarise(TOTAL = sum(TOTAL))
SeguroDefeso_total_2018_ma_mes

SeguroDefeso_total_2018_ma_municipio <- SeguroDefeso_total_2018_ma %>% 
                        group_by(NOME.MUNICÍPIO) %>%
                        summarise(TOTAL = sum(TOTAL)) %>%
                        arrange(desc(TOTAL))
glimpse(SeguroDefeso_total_2018_ma_municipio)

SeguroDefeso_total_2018_ma_municipio

# shape municipios maranhenses do pacote GEOBR
shape_mun_pc_2018 <- read_municipality(code_muni = 'MA', year = 2018) # lendo shape
glimpse(shape_mun_pc_2018)

shape_mun_pc_2018$name_muni <- as.character(str_to_upper(shape_mun_pc_2018$name_muni))

shape_mun_pc_2018$name_muni <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", shape_mun_pc_2018$name_muni)

SeguroDefeso_total_2018_ma_municipio$NOME.MUNICÍPIO <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", SeguroDefeso_total_2018_ma_municipio$NOME.MUNICÍPIO)
glimpse(SeguroDefeso_total_2018_ma_municipio)

# Adicionando municipios marenhenses com valores ausentes `a shape

nova_linha <- c(0, 0)


for (i in 1:length(shape_mun_pc_2018$name_muni)) {
    if (shape_mun_pc_2018$name_muni[i]  %in% SeguroDefeso_total_2018_ma_municipio$NOME.MUNICÍPIO) {
        
    } else {
        nova_linha[1]= shape_mun_pc_2018$name_muni[i]
        nova_linha[2] = NA
        SeguroDefeso_total_2018_ma_municipio = rbind(SeguroDefeso_total_2018_ma_municipio, nova_linha) 
    }
}
glimpse(SeguroDefeso_total_2018_ma_municipio)

shape_mun_pc_2018 <- right_join(shape_mun_pc_2018, SeguroDefeso_total_2018_ma_municipio, by = c('name_muni' = 'NOME.MUNICÍPIO')) # juntando os dados
glimpse(shape_mun_pc_2018)

shape_mun_pc_2018$TOTAL <- as.double(shape_mun_pc_2018$TOTAL)
glimpse(shape_mun_pc_2018)

shape_mun_pc_2018 <- shape_mun_pc_2018 %>% 
    mutate(TOTAL_milhoes = round(TOTAL/1000000, 2))
glimpse(shape_mun_pc_2018)

max(shape_mun_pc_2018$TOTAL_milhoes, na.rm = T)
min(shape_mun_pc_2018$TOTAL_milhoes, na.rm = T)



## PNG export
# get figure dimension
sizes <- getFigDim(x = shape_mun_pc_2018, width = 1080, height = 1080, mar = c(0,0,1.2,0))
# export the map

par(mar = c(0,0,1.2,0))

png(filename = "seguro.defeso.ma.2018.png", width = sizes[1], height = sizes[2])

# shape
plot(st_geometry(shape_mun_pc_2018), col = "white", border = "white", bg = "white")

title(main = list("Valor acumulado recebido pelos pescadores\ndo Seguro Defeso - Maranhão (2018)"
                  , cex = 2.5))

# caixa 
box()

# mapas coropléticos
choroLayer(
    x = shape_mun_pc_2018, 
    var = "TOTAL_milhoes",  
    method = "arith", 
    nclass = 5,
    col = carto.pal(pal1 = "red.pal", n1 = 5),
    breaks = c(0, 4, 8, 12, 16, 20),
    legend.title.txt = "Valor (em milhões (R$))",
    legend.values.rnd = 2.5,
    legend.title.cex = 1.8, 
    legend.values.cex = 2,
    legend.pos = "topleft",
    legend.nodata = 'sem dados',
    add = TRUE)

text(x = -48.75515, y=  -10.26176, cex = 1.8, adj = 0, font = 3,  labels = 
"Fonte: Portal da Transparência Brasil\nElaboração: OMT-MA")


# norte
north(pos = "topright")

# escala
map.scale(x = -43.1000, y = -9.84000, relwidth=0.15, ratio=FALSE)

dev.off()

for (i in 1:length(arquivos)){
    if(substr(arquivos[i], 1, 4) == '2019') {
        assign(
            paste(substr(arquivos[i], 8, 19), substr(arquivos[i], 1, 6), sep = ''), 
            read.delim2(paste(diretorio, arquivos[i], sep = '\\'), sep = ";",  dec = ",")
        )} 
}

SeguroDefeso201901_ma <- SeguroDefeso201901 %>% 
                        filter(UF == "MA")
SeguroDefeso201902_ma <- SeguroDefeso201902 %>% 
                        filter(UF == "MA")
SeguroDefeso201903_ma <- SeguroDefeso201903 %>% 
                        filter(UF == "MA")
SeguroDefeso201904_ma <- SeguroDefeso201904 %>% 
                        filter(UF == "MA")
SeguroDefeso201905_ma <- SeguroDefeso201905 %>% 
                        filter(UF == "MA")


SeguroDefeso_total_2019_ma <-rbind (
                                SeguroDefeso201901_ma,
                                SeguroDefeso201902_ma,
                                SeguroDefeso201903_ma,
                                SeguroDefeso201904_ma,
                                SeguroDefeso201905_ma
                                )

rm(
    SeguroDefeso201901,
    SeguroDefeso201902,
    SeguroDefeso201903,
    SeguroDefeso201904,
    SeguroDefeso201905
    )

rm(
    SeguroDefeso201901_ma,
    SeguroDefeso201902_ma,
    SeguroDefeso201903_ma,
    SeguroDefeso201904_ma,
    SeguroDefeso201905_ma                               
    )

glimpse(SeguroDefeso_total_2019_ma)

# Verificando quantidadde e proporção por município
top10_2019_distintos <- SeguroDefeso_total_2019_ma %>%
                        group_by(NOME.MUNICÍPIO) %>%
                        summarize(n = n_distinct(NIS.FAVORECIDO)) %>%
                        mutate(prop = n / sum(n), prop = scales::percent(prop)) %>%
                        arrange(desc(n))
                        
paste("Qauntidade de pescadores do estado que receberam Seguro Defeso em 2019", sum(top10_2019_distintos$n))

#  Os 10 municípios que mais tiveram pescadores benefíciados pelo Seguro Defeso no MA em 2019
top10_2019_distintos %>%
    top_n(10, n) %>%
    rename(Município = NOME.MUNICÍPIO) %>%
    rename("Quantidade de Pescadores" = n) %>%
    rename("Proporção" = prop)

# Quantidade de parcelas recebidas pelos pescadores maranhenses por município
parcelas2019 <- SeguroDefeso_total_2019_ma %>%
                count(NOME.MUNICÍPIO, sort = TRUE) %>%
                mutate(prop = n / sum(n), prop = scales::percent(prop))
                
parcelas2019 %>%
    top_n(10, n) %>%
    rename(Município = NOME.MUNICÍPIO) %>%
    rename("Quantidade de Parcelas" = n) %>%
    rename("Proporção" = prop)

SeguroDefeso_2019_slz <- SeguroDefeso_total_2019_ma %>% 
                        filter(NOME.MUNICÍPIO == "SAO LUIS")
glimpse(SeguroDefeso_2019_slz)

total_pescadores_2019_slz <- SeguroDefeso_2019_slz %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")
total_pescadores_2019_slz

SeguroDefeso_total_2019_slz <- SeguroDefeso_2019_slz %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2019_slz)

total_pescadores_2019 <- SeguroDefeso_total_2019_ma %>%
                            dplyr::n_distinct("NIS.FAVORECIDO")

total_pescadores_2019

SeguroDefeso_total_2019_ma <- SeguroDefeso_total_2019_ma %>% 
                                select(MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO, VALOR.PARCELA) %>%
                                group_by(NOME.MUNICÍPIO, MÊS.REFERÊNCIA, UF, NOME.MUNICÍPIO) %>%
                                summarise(TOTAL = sum(VALOR.PARCELA)) %>%
                                arrange(desc(TOTAL))
                        
glimpse(SeguroDefeso_total_2019_ma)

SeguroDefeso_total_2019_ma_mes <- SeguroDefeso_total_2019_ma %>%
                                    group_by(MÊS.REFERÊNCIA) %>%
                                    summarise(TOTAL = sum(TOTAL))
SeguroDefeso_total_2019_ma_mes

SeguroDefeso_total_2019_ma_municipio <- SeguroDefeso_total_2019_ma %>% 
                        group_by(NOME.MUNICÍPIO) %>%
                        summarise(TOTAL = sum(TOTAL)) %>%
                        arrange(desc(TOTAL))
glimpse(SeguroDefeso_total_2019_ma_municipio)

SeguroDefeso_total_2019_ma_municipio

# shape municipios maranhenses do pacote GEOBR
shape_mun_pc_2019 <- read_municipality(code_muni = 'MA', year = 2018) # lendo shape
glimpse(shape_mun_pc_2019)

shape_mun_pc_2019$name_muni <- as.character(str_to_upper(shape_mun_pc_2019$name_muni))

shape_mun_pc_2019$name_muni <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", shape_mun_pc_2019$name_muni)

SeguroDefeso_total_2019_ma_municipio$NOME.MUNICÍPIO <- chartr("ÁÉÍÓÚÇÃÕÂÊÎÔÛ", "AEIOUCAOAEIOU", SeguroDefeso_total_2019_ma_municipio$NOME.MUNICÍPIO)
glimpse(SeguroDefeso_total_2019_ma_municipio)

# Adicionando municipios marenhenses com valores ausentes `a shape

nova_linha <- c(0, 0)


for (i in 1:length(shape_mun_pc_2019$name_muni)) {
    if (shape_mun_pc_2019$name_muni[i]  %in% SeguroDefeso_total_2019_ma_municipio$NOME.MUNICÍPIO) {
        
    } else {
        nova_linha[1]= shape_mun_pc_2019$name_muni[i]
        nova_linha[2] = NA
        SeguroDefeso_total_2019_ma_municipio = rbind(SeguroDefeso_total_2019_ma_municipio, nova_linha) 
    }
}
glimpse(SeguroDefeso_total_2019_ma_municipio)

shape_mun_pc_2019 <- right_join(shape_mun_pc_2019, SeguroDefeso_total_2019_ma_municipio, by = c('name_muni' = 'NOME.MUNICÍPIO')) # juntando os dados
glimpse(shape_mun_pc_2019)

shape_mun_pc_2019$TOTAL <- as.double(shape_mun_pc_2019$TOTAL)
glimpse(shape_mun_pc_2019)

shape_mun_pc_2019 <- shape_mun_pc_2019 %>% 
    mutate(TOTAL_milhoes = round(TOTAL/1000000, 2))
glimpse(shape_mun_pc_2019)

shape_mun_pc_2019

max(shape_mun_pc_2019$TOTAL_milhoes, na.rm = T)
min(shape_mun_pc_2019$TOTAL_milhoes, na.rm = T)

## PNG export
# get figure dimension
sizes <- getFigDim(x = shape_mun_pc_2019, width = 1080, height = 1080, mar = c(0,0,1.2,0))
# export the map

par(mar = c(0,0,1.2,0))

png(filename = "seguro.defeso.ma.2019.png", width = sizes[1], height = sizes[2])

# shape
plot(st_geometry(shape_mun_pc_2019), col = "white", border = "white", bg = "white")

title(main = list("Valor acumulado recebido pelos pescadores\ndo Seguro Defeso - Maranhão (2019)"
                  , cex = 2.5))

# caixa 
box()

# mapas coropléticos
choroLayer(
    x = shape_mun_pc_2019, 
    var = "TOTAL_milhoes",  
    method = "arith", 
    nclass = 5,
    col = carto.pal(pal1 = "red.pal", n1 = 5),
    breaks = c(0, 4, 8, 12, 16, 20),
    legend.title.txt = "Valor (em milhões (R$))",
    legend.values.rnd = 2.5,
    legend.title.cex = 1.8, 
    legend.values.cex = 2,
    legend.pos = "topleft",
    legend.nodata = 'sem dados',
    add = TRUE)

text(x = -48.75515, y=  -10.26176, cex = 1.8, adj = 0, font = 3,  labels = 
"Fonte: Portal da Transparência Brasil\nElaboração: OMT-MA")


# norte
north(pos = "topright")

# escala
map.scale(x = -43.1000, y = -9.84000, relwidth=0.15, ratio=FALSE)

dev.off()


Seguro_total_mes <- rbind(
                            SeguroDefeso_total_2014_ma_mes,
                            SeguroDefeso_total_2015_ma_mes,
                            SeguroDefeso_total_2016_ma_mes,
                            SeguroDefeso_total_2017_ma_mes,
                            SeguroDefeso_total_2018_ma_mes,
                            SeguroDefeso_total_2019_ma_mes
                        )

glimpse(Seguro_total_mes)

ano = vector("character", length = length(Seguro_total_mes$MÊS.REFERÊNCIA))
mes = vector("character", length = length(Seguro_total_mes$MÊS.REFERÊNCIA))

for (i in 1:length(Seguro_total_mes$MÊS.REFERÊNCIA)){
    ano[i] <- substr(Seguro_total_mes$MÊS.REFERÊNCIA[i], 1, 4)
    mes[i] <- substr(Seguro_total_mes$MÊS.REFERÊNCIA[i], 5, 6)
    }

Seguro_total_mes = cbind(Seguro_total_mes, ano, mes)
glimpse(Seguro_total_mes)

Seguro_total_mes <- Seguro_total_mes %>% 
    mutate(TOTAL_milhoes = round(TOTAL/1000000, 2)) %>%
    arrange(desc(ano))
glimpse(Seguro_total_mes)

Seguro_total_mes$mes <- factor(Seguro_total_mes$mes)

Seguro_total_mes$mes <- Seguro_total_mes$mes %>% forcats::fct_recode(jan = "01", fev = "02", mar = "03", abr = "04", maio = "05", jun = "06", jul = "07", ago = "08", set = "09", out = "10", nov = "11", dez = "12")

glimpse(Seguro_total_mes)

g <- Seguro_total_mes %>%
    mutate(ano = reorder(ano, desc(ano))) %>%
    ggplot(aes(x = mes, y = TOTAL_milhoes)) +
    geom_line(aes(group = ano, color = ano), size = 0.5) +
    geom_point(aes(color = ano), size = 3.5) +
    xlab("Mês") + ylab("Total (em milhões de reais)")  +
    labs(title = "Valor acurmulado recebido do Seguro Defeso pelos pescadores\nmaranheses (2014-2019*)", 
         caption =  "Elaboração: OMT-MA\nFonte: Portal da Transparência", 
         subtitle = "*Para 2019, há dados disponíveis apenas para os cinco primeiros meses do ano") +
    theme_minimal()

g + scale_colour_hue(l = 40)
ggsave("seguro_defesso_ma1.png", width = 16, height = 12, units = "cm")


total_pescadores_ano <- c(total_pescadores_2014,
                         total_pescadores_2015,
                         total_pescadores_2016,
                         total_pescadores_2017,
                         total_pescadores_2018,
                         total_pescadores_2019
                         )
ano <- c(2014, 2015, 2016, 2017, 2018, 2019)

total_pescadores <- as.data.frame(cbind(ano, total_pescadores_ano))


glimpse(total_pescadores)

total_pescadores <- total_pescadores %>% 
    mutate(TOTAL_milhares = round(total_pescadores_ano /1000, 2))
glimpse(total_pescadores)


g <- ggplot(total_pescadores, aes(x = factor(ano), y = TOTAL_milhares)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    #geom_line(color = "grey50", size = 2) +
    #geom_point(color = "red", size = 3) +
    xlab("Ano") + ylab("Total (em Mil)") +
    geom_text(aes(label = TOTAL_milhares), vjust = -.3, color = "black")+
    labs(title = "Quantidade de pescadores maranhenses que receberam alguma\nparcela do Seguro Defeso (2014-2019*)", 
         caption =  "Elaboração: OMT-MA\nFonte: Portal da Transparência", 
         subtitle = "*Para 2019, há dados disponíveis apenas para os cinco primeiros meses do ano") +
   theme_classic()
   #annotate("text", x = factor(2019), y = 200)
g + scale_colour_hue(l = 45)

ggsave("seguro_defesso_ma2.png", width = 16, height = 12, units = "cm")

glimpse(total_pescadores)


ano <- c(2014, 2015, 2016, 2017, 2018, 2019)


sao_luis <- c(total_pescadores_2014_slz,
                       total_pescadores_2015_slz,
                       total_pescadores_2016_slz,
                       total_pescadores_2017_slz,
                       total_pescadores_2018_slz,
                       total_pescadores_2019_slz)
total_sao_luis <- as.data.frame(cbind(ano, sao_luis))
total_sao_luis

g <- ggplot(total_sao_luis, aes(x = factor(ano), y = sao_luis)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.9) +
    #geom_line(color = "grey50", size = 0.5) +
    #geom_point(color = "red", size = 3) +
    xlab("Ano") + ylab("Total") +
    geom_text(aes(label = sao_luis), vjust = -0.3, color = "black")+
    labs(title = "Quantidade de pecadores de São Luís que receberam\nalguma parcela do Seguro Defeso (2014-2019*)", 
         caption =  "Elaboração: OMT-MA\nFonte: Portal da Transparência", 
         subtitle = "*Para 2019, há dados disponíveis apenas para os cinco primeiros meses do ano") +
    theme_classic()
g + scale_colour_hue(l = 45)

ggsave("seguro_defesso_ma3.png", width = 16, height = 12, units = "cm")

############# Aglomeracao urbana de São Luis ###############################################################
SeguroDefeso_t <- rbind(SeguroDefeso_total_2014_ma,
                           SeguroDefeso_total_2015_ma,
                           SeguroDefeso_total_2016_ma,
                           SeguroDefeso_total_2017_ma,
                           SeguroDefeso_total_2018_ma,
                           SeguroDefeso_total_2019_ma)

SeguroDefeso_aglo <-  SeguroDefeso_t %>% 
    filter(NOME.MUNICÍPIO == "SAO LUIS" | 
                                 NOME.MUNICÍPIO == "SAO JOSE DE RIBAMAR" |
                                 NOME.MUNICÍPIO == "RAPOSA" |
                                 NOME.MUNICÍPIO == "PACO DO LUMIAR")

SeguroDefeso_aglo <- SeguroDefeso_aglo %>%
    mutate(ano = as.integer(stringr::str_sub(MÊS.REFERÊNCIA, 1, 4))) %>%
    mutate(mes = as.integer(stringr::str_sub(MÊS.REFERÊNCIA, 5, 6)))
    
SeguroDefeso_aglo <- SeguroDefeso_aglo %>%
    group_by(NOME.MUNICÍPIO, ano) %>%
    summarise(nis_distintos =  n_distinct(NIS.FAVORECIDO))

glimpse(SeguroDefeso_aglo)

SeguroDefeso_aglo <- as.data.frame(SeguroDefeso_aglo)

SeguroDefeso_aglo$NOME.MUNICÍPIO <- factor(SeguroDefeso_aglo$NOME.MUNICÍPIO)

SeguroDefeso_aglo %>% forcats::fct_recode(NOME.MUNICÍPIO, "São Luís" = "SAO LUIS",
                                           "São José de Ribamar" = "SAO JOSE DE RIBAMAR",
                                           "Raposa" = "RAPOSA",
                                           "Paço do Lumiar" = "PACO DO LUMIAR")

g <- SeguroDefeso_aglo %>%
    #arrange(NOME.MUNICÍPIO,desc(nis_distintos)) %>%
   ggplot(aes(x = factor(ano), 
               y = nis_distintos, 
               fill =  forcats::fct_reorder(NOME.MUNICÍPIO, -nis_distintos))) +
    geom_col(position = "dodge") +
    xlab("Ano") + ylab("Total") +
    #geom_text_repel(aes(label = nis_distintos)) +
    labs(title = "Quantidade de pecadores dos municipios da Aglomeração Urbana\nde São Luís que receberam alguma parcela do Seguro Defeso\n(2014-2019*)", 
         caption =  "Elaboração: OMT-MA\nFonte: Portal da Transparência", 
         subtitle = "*Para 2019, há dados disponíveis apenas para os cinco primeiros meses do ano",
         fill = "MUNICÍPIO") +
    theme_classic() + 
    theme(legend.position = "bottom") + 
    guides(
        color = guide_legend(
            nrow = 1,
            override.aes = list(size = 4)))
g

g + scale_colour_hue(l = 45)
