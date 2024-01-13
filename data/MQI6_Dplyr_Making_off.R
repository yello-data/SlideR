# LOAD DATA
library(dplyr)
library(tidyr)
library(readr)
library(janitor)

# FONTS PRINCIPALS
#https://www.idescat.cat/estad/?geo=mun
#https://opendata-ajuntament.barcelona.cat/data/ca/dataset


# 1. MUNICIPI

## DADES GENERALS MUNICIPI
#https://www.idescat.cat/indicadors/?id=aec&n=15903&t=201900
id <- read_csv2("data/t15903201900.csv", skip = 5) |> 
  clean_names()

id |> 
  mutate(densitat = poblacio / superficie_km2) |> 
  group_by(comarca) |>
  filter(densitat < 9000) |> 
  summarize(densitat = mean(densitat),
            altitud_m = max(altitud_m)) |> 
  arrange(densitat) |> 
  ggplot(aes(x = densitat, y = altitud_m)) +
  geom_text(aes(label = comarca))

write_rds(id, "data/municipi.rds")



# 3. BARRI-ANY

# LLOGUER M2: https://opendata-ajuntament.barcelona.cat/data/ca/dataset/est-mercat-immobiliari-lloguer-mitja-mensual/resource/cfc45f2b-62eb-4621-8486-1b90e36b4bfe
lloguer14 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/5855ba6c-f554-4a99-837a-04ea69bc71f4/download/2014_lloguer_preu_trim.csv")
lloguer15 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/fcdbfa43-d97a-4da3-b78b-6f255dbcf4cc/download/2015_lloguer_preu_trim.csv")
lloguer16 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/b45e8b56-1988-4474-bf61-0a76f8ab28c2/download/2016_lloguer_preu_trim.csv")
lloguer17 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/0a71a12d-55fa-4a76-b816-4ee55f84d327/download/2017_lloguer_preu_trim.csv")
lloguer18 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/3dc45b16-42a9-4f57-9863-e6d1a4f5869f/download/2018_lloguer_preu_trim.csv")
lloguer19 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/004c76b1-6269-4136-89b2-89fd47046930/download/2019_lloguer_preu_trim.csv") |> 
  mutate(Preu = as.numeric(Preu))
lloguer20 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/47c9d64d-317a-45d0-8c45-45488df8601c/download/2020_lloguer_preu_trim.csv")|> 
  mutate(Preu = as.numeric(Preu))
lloguer21 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/cfc45f2b-62eb-4621-8486-1b90e36b4bfe/download/2021_lloguer_preu_trim.csv") |> 
  mutate(Preu = as.numeric(Preu))
lloguer22 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/e96bf614-467b-40ab-91b9-e48a616ea775/download/2022_lloguer_preu_trim.csv")|> 
  mutate(Preu = as.numeric(Preu))
lloguer23 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/69c3250b-100c-4d5b-a08e-0a191d9950e3/resource/8148fe53-6bb4-42ca-98df-93e6b723dca9/download/2023_lloguer_preu_trim.csv")|> 
  mutate(Preu = as.numeric(Preu))

lloguer <- bind_rows(lloguer14, lloguer15, lloguer16, lloguer17, lloguer18, lloguer19, lloguer20, lloguer21, lloguer22, lloguer23) |> 
  pivot_wider(names_from = "Lloguer_mitja", values_from = "Preu") |>
  rename("preu" = "Lloguer mitjà mensual (Euros/mes)", "preu_m2" = "Lloguer mitjà per superfície (Euros/m2 mes)") |> 
  clean_names()

lloguer2 <- bind_rows(lloguer14, lloguer15, lloguer16, lloguer17, lloguer18, lloguer19, lloguer20, lloguer21, lloguer22, lloguer23) |>
  filter(Lloguer_mitja == "Lloguer mitjà mensual (Euros/mes)") |> 
  select(-Lloguer_mitja) |> 
  clean_names()

write_rds(lloguer, "data/lloguer_any.rds")



# 4. SECCIÓ CENSAL

## RENDA NETA MITJANA
#https://opendata-ajuntament.barcelona.cat/data/ca/dataset/renda-tributaria-unitat-consum-atlas-distribucio
rendacap20 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/c9b9b5e5-b02f-4b47-892b-79acf0191802/resource/cb629032-8de5-4a52-ba0a-c5e2410d0911/download/2020_renda_neta_mitjana_per_llar.csv") 

## INDEX GINI (CS): https://opendata-ajuntament.barcelona.cat/data/ca/dataset/atles-renda-index-gini/resource/44dd0cb5-41ef-4359-a1c5-9b3359347ebb
gini <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/3a225bff-a2b3-40f1-9ba1-d7dc77971351/resource/44dd0cb5-41ef-4359-a1c5-9b3359347ebb/download/2020_atles_renda_index_gini.csv") |> 
  select(-Nom_Districte, -Nom_Barri)

rendacs <- rendacap20 |> 
  left_join(gini, by = c("Any", "Codi_Districte", "Codi_Barri", "Seccio_Censal")) |> 
  clean_names()


write_rds(rendacs, "data/rendacs.rds")


#4. ESDEVENIMENTS

## ACCIDENTS GU
#https://opendata-ajuntament.barcelona.cat/data/dataset/87aa433e-fef8-4ff4-8f9a-d66e9beefff6/resource/3df166fe-ceec-4b20-811d-4b58c29d7cb1/download/2020_accidents_persones_gu_bcn.csv
accidents20 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/87aa433e-fef8-4ff4-8f9a-d66e9beefff6/resource/3df166fe-ceec-4b20-811d-4b58c29d7cb1/download/2020_accidents_persones_gu_bcn.csv",
                        locale = locale(encoding = "UTF-8")) |> 
  clean_names()

write_rds(accidents20, "data/accidents.rds")

#CONTRACTES MENORS 
# GENCAT: https://analisi.transparenciacatalunya.cat/Sector-P-blic/Contractaci-de-Catalunya/hb6v-jcbf (> 600Mb!!!)
# AJ BCN: https://opendata-ajuntament.barcelona.cat/data/ca/dataset/contractes-menors
cm18 <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/1121f3e2-bfb1-4dc4-9f39-1c5d1d72cba1/resource/69ae574f-adfc-4660-8f81-73103de169ff/download/2018_menors.csv") |> 
  clean_names() |> 
  select(-trimestre, -nif, -durada) |> 
  mutate(data_adjudicacio = as.Date(data_adjudicacio, format = "%d/%m/%Y"))




write_rds(cm18, "data/contractes_menors.rds")

# FESTIVALS: https://opendata-ajuntament.barcelona.cat/data/ca/dataset/dades-festivals
download.file("https://opendata-ajuntament.barcelona.cat/data/dataset/d35087c8-e863-4b07-bb65-f08ee67d0af7/resource/57ed9976-8291-418d-a0fc-ecf572d8cb74/download",
              "festivals21.csv")
festivals21 <- read_csv("festivals21.csv") |> 
  clean_names() |> 
  mutate(data_inici = as.Date(data_inici, format = "%d/%m/%Y"),
         data_fi = as.Date(data_fi, format = "%d/%m/%Y"))


write_rds(festivals21, "data/festivals.rds")


# CENS PERSONES DESAPAREGUDES DURANT GUERRA CIVIL

#https://analisi.transparenciacatalunya.cat/Legislaci-just-cia/Cens-de-persones-desaparegudes-durant-la-Guerra-Ci/u2ix-2jr6
cens_gc <- read_csv("data/Cens_de_persones_desaparegudes_durant_la_Guerra_Civil.csv") |> 
  clean_names()
write_rds(cens_gc, "data/cens_gc.rds")



# PADRO NACIONALITAT SEXE (DISTRICTE, BARRI, SEXE, NACIONALITAT)
#https://opendata-ajuntament.barcelona.cat/data/ca/dataset/est-padro-nacionalitat-sexe/resource/4663d1aa-ab18-4e4e-8569-80d9fe7d9b75
padro <- read_csv("https://opendata-ajuntament.barcelona.cat/data/dataset/3a0934e4-cdba-4056-950a-331429cb8908/resource/4663d1aa-ab18-4e4e-8569-80d9fe7d9b75/download/2022_padro_nacionalitat_per_sexe.csv")


# FUNCIONARIS
#https://opendata-ajuntament.barcelona.cat/data/ca/dataset/plantilla-municipal/resource/3dc34978-3b82-4a8a-8e48-1a0e24cf07c2


