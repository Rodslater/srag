library(rio)
library(rvest)
library(tidyverse)
library(sf)

site21 <- read_html("https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023/resource/dd91a114-47a6-4f21-bcd5-86737d4fc734")
link21 <- site21 %>% html_nodes(xpath="//a[contains(text(), '.csv')]") %>% html_attr("href")
destino21 <- "SRAG21.csv"
Map(function(u, d) download.file(u, d, mode="wb"), link21, destino21)

site22 <- read_html("https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023/resource/62803c57-0b2d-4bcf-b114-380c392fe825")
link22 <- site22 %>% html_nodes(xpath="//a[contains(text(), '.csv')]") %>% html_attr("href")
destino22<- "SRAG22.csv"
Map(function(u, d) download.file(u, d, mode="wb"), link22, destino22)

site23 <- read_html("https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023/resource/0d78ff63-d6ca-4311-8dc8-6123cf1ca127")
link23 <- site23 %>% html_nodes(xpath="//a[contains(text(), '.csv')]") %>% html_attr("href")
destino23<- "SRAG23.csv"
Map(function(u, d) download.file(u, d, mode="wb"), link23, destino23)

SRAG <- import_list(dir(pattern = ".csv"), rbind = TRUE, encoding = "UTF-8")

arquivos_csv <- dir(pattern = ".csv")
file.remove(arquivos_csv)


SRAG <- SRAG |> 
  filter(SG_UF_NOT == "SE") |>  
  mutate(data = dmy(DT_NOTIFIC),
         data_evolucao = dmy(DT_EVOLUCA),
         idoso = ifelse(NU_IDADE_N >= 60, 'Sim','Não'),
         grupo_idade = case_when(NU_IDADE_N < 5 ~ "0 a 4 anos",
                        NU_IDADE_N > 4 & NU_IDADE_N < 10 ~ "05 a 9 anos",
                        NU_IDADE_N > 9 & NU_IDADE_N < 15 ~ "10 a 14 anos",
                        NU_IDADE_N > 14 & NU_IDADE_N < 20 ~ "15 a 19 anos",
                        NU_IDADE_N > 19 & NU_IDADE_N < 25 ~ "20 a 24 anos",
                        NU_IDADE_N > 24 & NU_IDADE_N < 30 ~ "25 a 29 anos",
                        NU_IDADE_N > 29 & NU_IDADE_N < 35 ~ "30 a 34 anos",
                        NU_IDADE_N > 34 & NU_IDADE_N < 40 ~ "35 a 39 anos",
                        NU_IDADE_N > 39 & NU_IDADE_N < 45 ~ "40 a 44 anos",
                        NU_IDADE_N > 44 & NU_IDADE_N < 50 ~ "45 a 49 anos",
                        NU_IDADE_N > 49 & NU_IDADE_N < 55 ~ "50 a 54 anos",
                        NU_IDADE_N > 54 & NU_IDADE_N < 60 ~ "55 a 59 anos",
                        NU_IDADE_N > 59 & NU_IDADE_N < 65 ~ "60 a 64 anos",
                        NU_IDADE_N > 64 & NU_IDADE_N < 70 ~ "65 a 69 anos",
                        NU_IDADE_N > 69 & NU_IDADE_N < 75 ~ "70 a 74 anos",
                        NU_IDADE_N > 74 & NU_IDADE_N < 80 ~ "75 a 79 anos",
                        NU_IDADE_N > 79 ~ "80 anos ou mais")) |> 
  select(data, semana=SEM_NOT,codigo=CO_MUN_NOT, sexo=CS_SEXO, raca=CS_RACA, idade=NU_IDADE_N, grupo_idade, idoso,
         vacina_covid=VACINA_COV, classificacao=CLASSI_FIN, evolucao=EVOLUCAO, data_evolucao) |> 
    arrange(data)

municipios <- readRDS('data/codigos_municipios.rds')
SRAG <- left_join(SRAG, municipios, by='codigo')

SRAG <- SRAG |>
  mutate(sexo = case_when(sexo  == 'M' ~ 'Homem',
                          sexo  == 'F' ~ 'Mulher',
                          TRUE ~ sexo),
         raca = case_when(raca  == 1 ~ 'Branca',
                          raca  == 2 ~ 'Preta',
                          raca  == 3 ~ 'Amarela',
                          raca  == 4 ~ 'Parda',
                          raca  == 5 ~ 'Indígena',
                          raca  == 9 ~ 'Ignorado',
                          TRUE ~ as.character(raca)),
         vacina_covid = case_when(vacina_covid  == 1 ~ 'Sim',
                                  vacina_covid  == 2 ~ 'Não',
                                  vacina_covid  == 9 ~ 'Ignorado',
                                  TRUE ~ as.character(vacina_covid )),
         classificacao = case_when(classificacao  == 1 ~ 'SRAG por influenza',
                                   classificacao  == 2 ~ 'SRAG por outro vírus respiratório',
                                   classificacao  == 3 ~ 'SRAG por outro agente etiológico,',
                                   classificacao  == 4 ~ 'SRAG não especificado',
                                   classificacao  == 5 ~ 'SRAG por covid-19',
                                   TRUE ~ as.character(classificacao)),
         evolucao = case_when(evolucao  == 1 ~ 'Cura',
                              evolucao  == 2 ~ 'Óbito',
                              evolucao  == 3 ~ 'Óbito por outras causas',
                              evolucao  == 9 ~ 'Ignorado',
                              TRUE ~ as.character(evolucao)))
    
    
    
shp <- st_read("data/mapa/shp.shp")
shp <- shp %>% 
  select(City, geometry)
shp$City <- as.character(shp$City)
shp$City <- substr(shp$City, 1, nchar(shp$City)-1)
shp <- st_as_sf(shp)%>%
  st_transform(4326)

srag_municipios <- SRAG |> 
  group_by(municipio) |> 
  mutate(casos= n(),
         codigo=as.character(codigo)) %>% 
  distinct(casos, .keep_all=TRUE) |> 
  select(municipio, codigo, casos)


shp_srag <- left_join(shp, srag_municipios, by = c("City" = "codigo"))
shp_srag <- shp_srag  |> 
  select(-municipio) 


municipios <- readRDS('data/codigos_municipios.rds')
municipios$codigo <-  as.character(municipios$codigo)
shp_srag <- left_join(shp_srag, municipios, by = c("City" = "codigo"))


saveRDS(SRAG, 'data/SRAG.rds')
saveRDS(shp_srag, 'data/shp_srag.rds')
