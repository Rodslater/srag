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

site23 <- read_html("https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023/resource/62803c57-0b2d-4bcf-b114-380c392fe825")

link23 <- site23 %>% html_nodes(xpath="//a[contains(text(), '.csv')]") %>% html_attr("href")

destino23<- "SRAG23.csv"

Map(function(u, d) download.file(u, d, mode="wb"), link23, destino23)

SRAG <- import_list(dir(pattern = ".csv"), rbind = TRUE, encoding = "UTF-8")

arquivos_csv <- dir(pattern = ".csv")
file.remove(arquivos_csv)

saveRDS(SRAG, 'data/SRAG.rds')
