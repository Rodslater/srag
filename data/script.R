library(rio)
library(rvest)
library(tidyverse)

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
         idoso = ifelse(NU_IDADE_N >= 60, 'Sim','Não')) |> 
  select(data, semana=SEM_NOT,codigo=CO_MUN_NOT, sexo=CS_SEXO, raca=CS_RACA, idade=NU_IDADE_N, idoso,
         vacina_covid=VACINA_COV, classificacao=CLASSI_FIN, evolucao=EVOLUCAO, data_evolucao)

municipios <- readRDS('data/codigos_municipios.rds')
SRAG <- left_join(SRAG, municipios, by='codigo')

SRAG <- SRAG |> select(-codigo) |> 
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


saveRDS(SRAG, 'data/SRAG.rds')
