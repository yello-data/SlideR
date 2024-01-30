# 1. CREAR PROJECTE!!!!!

# 2. ACTIVEM PAQUETS
library(readxl)
library(dplyr)
library(janitor) # <- aquest paquet és nou!! instal·lar primer

# 3. DOWNLOAD DATA

#-- Triar 3.1 OR 3.2

## 3.1. DESCÀRREGA MANUAL
#Go to: https://infoelectoral.interior.gob.es/es/elecciones-celebradas/area-de-descargas/
# Otras Descargas -> Datos de Municipios -> Congreso 2023 23 de Julio -> Descargar

## 3.2. DESCÀRREGA AUTOMÀTICA (no funciona en tots els ordinadors)
download.file("https://infoelectoral.interior.gob.es/estaticos/docxl/02_202307_1.zip",
              "02_202307_1.zip")
unzip("02_202307_1.zip")

## 3.3. READ THE FILE (it should be in your working directory)
elecc23 <- read_xlsx("02_202307_1.xlsx", skip = 5) |> 
  clean_names()

# 4. GLIMPSE DATA
elecc23
glimpse(elecc23)
unique(elecc23$nombre_de_comunidad)
unique(elecc23$nombre_de_provincia)


# 5. EXERCICI
# Estudiant:
# Pregunta1: 
# Codi1 (mínim 3 funcions dplyr entre filter-arrange-count-select-mutate):
elecc23 |> 
  select(nombre_de_comunidad) |> 
  count(nombre_de_comunidad) |> 
  head(6)
# Pregunta2: 
# Codi2 (mínim 2 funcions dplyr: group_by i summarize):
elecc23 |> 
  group_by(nombre_de_comunidad) |> 
  summarize(n = n()) |> 
  head(6)
