#####################################################################
######################## DATA MANAGEMENT ############################
#####################################################################



#####################################################################
# PART 0. LEARNING CURVE
#####################################################################

# R learning curve
library(dplyr)
library(ggplot2)
rlc <- tibble(class = 1:100,
              knowledge = (1:100)^3)
lesson <- 10

rlc |> 
  ggplot(aes(x = class, y = knowledge)) +
  geom_line() +
  geom_point(data = rlc |> 
               filter(class == lesson), col = "red") +
  annotate("text", x = 36, y = 260000, label = "You are here!") +
  geom_curve(aes(x = 25, xend = 9.8, 
                 y = 250000, yend = 15000),
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             color = "gray40", curvature = 0.3) +
  theme_minimal() +
  labs(x = "Class number", y = "Knowledge", title = "The R Knowledge curve")

#Load packages
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(readxl)
library(countrycode)
library(wbstats)
library(naniar)
library(haven)
library(Hmisc)



#####################################################################
# PART 1. DADES DESORDENADES
#####################################################################

#- pivot_longer(df, cols, names_to, names_to)
#- pivot_longer(df, cols, names_to, names_to)
#- separate(df, col, into, sep)
#- unite(df, col, ..., sep)

# 1.0 PER QUÈ NECESSITEM TIDY DATA??
# A la província d'Almeria, quina és la distribució del percentatge de vots al pp, cs i vox?
elecc19


## 1.1. UNTIDY DATA

### 1.1.1. Pivot longer
#- pivot_longer(df, cols, names_to, values_to)
table4a #cases
table4b #population

### 1.1.2. Pivot wider
#- pivot_wider(df, names_from, values_from)
table2


## 1.1.3. Separate
#- separate(df, col, into, sep)
table3_sep <- separate(table3, rate, into = c("cases", "population"), sep = "/")


## 1.1.4. Unite
#- unite(df, col, ..., sep)
table3_sep |> 
  unite("numbers", cases:population, sep = "-")


## 1.1.5. Exercici!!

# IDESCAT: Passa a "tidy data" la següent taula:
#https://www.idescat.cat/indicadors/?id=aec&n=15225


# WORLD BANK
#Combo! Algunes bases de dades poden estar doblement brutes:
#- Alguns valors (anys) són títols de columna
#- Alguns noms de variable (SP.URB, SP.POP...) són valors 
?world_bank_pop
tidyr::world_bank_pop |> 
  pivot_longer(`2000`:`2017`, names_to = "year", values_to = "vals") |> 
  pivot_wider(names_from = indicator, values_from = vals)

# Un altre exemple per practicar
tidyr::relig_income




#####################################################################
# PART 2. DADES PERDUDES (MISSING DATA)
#####################################################################


## 2.1. QUÈ FER AMB LES DADES PERDUDES?

### 2.1.1. Si estan categoritzats com a NA, els trobarem fent un sumari...
# summary()
summary(municipi)
summary(lloguer_any)


### 2.1.2. La funció is.na() ens troba (TRUE) les dades perdudes d'un vector. 
is.na(lloguer_any$preu)

#... per tant, a dins de filter podem excloure les dades perdudes ...
lloguer_any |> 
  filter(!is.na(preu))


### 2.1.3. Ja sabem que l'argument na.rm = T exclou els NA de les operacions de sumari
# mean(vector, na.rm = T)
# sum(vector, na.rm = T)


### 2.1.4. I amb replace NA assignem un valor concret als NA
replace_na(lloguer_any$preu, 150)

### 2.1.5. Per visualitzar NA [TOP CHOICE]
naniar::vis_miss()






#####################################################################
# PART 3. DADES SEPARADES
#####################################################################

# 2.1. FUNCIONS JOIN

# xxxx_join(df1, df2, by = c("join_variable_df1" = "join_variable_df2"))

#full_join
#left_join
#right_join
#inner_join

## 2.1. SITUACIÓ 1: ELS VALORS DE LA COLUMNA D'UNIÓ COINCIDEIXEN

### 2.1.1. Dataframe 1
df1 <- tibble(country = c("France", "Germany", "Czechia", "Austria"),
              code = c("FR", "DE", "CZ", "AT"),
              gdpcap = c(43518, 50801, 26378, NA),
              pop = c(67, 83, 11, 9))

### 2.1.2. Dataframe 2
df2 <- tibble(country = c("French Republic", 
                          "Federal Republic of Germany",
                          "Czech Republic", "United States of America"),
              cowc = c("FR", "DE", "CZ", "US"),
              lang = c("French", "German", "Czech", "English"))
df1
df2

### 2.1.3. Join
full_join(df1, df2, by = c("code" = "cowc"))
left_join(df1, df2, by = c("code" = "cowc"))
right_join(df1, df2, by = c("code" = "cowc"))
inner_join(df1, df2, by = c("code" = "cowc"))




### EXERCICI: IDESCAT

# Volem veure la relació entre renda familiar disponible i ...
# ... la regeneració de residus per càpita dels municipis catalans

# Renda familiar disponible:
# https://www.idescat.cat/pub/?id=rfdbc&n=13301&by=mun

# Generació de residus per càpita
# https://www.idescat.cat/pub/?id=resmc&n=6997&by=mun





## 2.2. SITUACIÓ 2: ELS VALORS DE LA COLUMNA D'UNIÓ NO COINCIDEIXEN

### 2.2.1. Dataframe 1
db1 <- tibble(country = c("France", "Germany", "Czechia", "Austria"),
              code = c("FR", "DE", "CZ", "AT"),
              gdpcap = c(43518, 50801, 26378, NA),
              pop = c(67, 83, 11, 9))

### 2.2.2. Dataframe 2
db2 <- tibble(country = c("French Republic", 
                          "Federal Republic of Germany",
                          "Czech Republic", "United States of America"),
              cowc = c("FRN", "GMY", "CZR", "USA"),
              lang = c("French", "German", "Czech", "English"))
db1
db2

### 2.2.3. Countrycode package

## What is it?
library(countrycode)
countrycode::cldr_examples
countrycode::codelist
countrycode::codelist_panel

# How it works?
# countrycode(variable_with_code, "code_origin", "code_destination")

#Option A: Pipe system
db1 |> 
  mutate(cowc = countrycode(code, "iso2c", "cowc")) |> 
  left_join(db2, by = c("cowc"))

#Option B: Without pipe
df1$cowc <- countrycode(db1$code, "iso2c", "cowc")
df_join <- left_join(df1, df2, by = c("cowc"))


#Language
db2 |> 
  mutate(code = countrycode(cowc, "cowc", "iso2c")) |> 
  full_join(db1, by = c("code")) |> 
  mutate(pais = countrycode(code, "iso2c", "cldr.short.ca")) |> 
  select(pais, gdpcap, pop, lang)



## ARE DEMOCRACIES LESS WARY THAN AUTOCRACIES?
#- Liberals would say yes
#- Realists would say no

# 1. BOIX-MILLER-ROSATO DATASET
#https://sites.google.com/site/mkmtwo/data

dcd <- read_dta("democracy-v4.0.dta") |> 
  select(country, ccode, year, democracy)

# 2. INTERSTATE WAR DATA
# https://correlatesofwar.org
interstate <- read_csv("https://correlatesofwar.org/wp-content/uploads/Inter-StateWarData_v4.0.csv") |> 
  select(ccode, StateName, WarName, StartYear1, EndYear1, BatDeath, Initiator, Outcome) |> 
  pivot_longer(StartYear1:EndYear1, names_to = "value", values_to = "year") |> 
  group_by(ccode, WarName, StateName) |> 
  complete(year = first(year):last(year)) |>
  fill(Initiator, BatDeath, Outcome, .direction = "downup") |> 
  select(-value) |> 
  distinct(ccode, WarName, StateName, year, BatDeath, Initiator) |> 
  mutate(War = 1,
         n = n(),
         BatDeathYear = BatDeath / n,
         Initiator = if_else(Initiator == 1, 1, 0)) |> 
  select(-n) |> 
  ungroup()

waryear <- interstate |> 
  select(-WarName) |> 
  group_by(ccode, year) |> 
  summarise(War = sum(War)) |>
  ungroup()

dcd |>
  filter(year > 1816, year < 2007) |> 
  left_join(waryear) |> 
  mutate(War = replace_na(War, 0)) |> 
  ggplot(aes(x = as.character(democracy), y = War)) +
  stat_summary(fun.data = "mean_cl_normal") +
  scale_y_continuous(labels = scales::label_percent())


#####################################################################
# PART 4. CODEBOOKS
#####################################################################

# 4.1. CHapel Hill Expert Survey
ches <- read_csv("https://www.chesdata.eu/s/CHES2019V3.csv")
glimpse(ches)

# 4.2. UCDP
download.file("https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-221-csv.zip",
              "data/ucdp.zip")
unzip("data/ucdp.zip")

ucdp <- read_csv("data/ucdp-prio-acd-221.csv")

# 4.3. WORLD BANK
library(wbstats)
wbstats::wb_search("pollution")
wbstats::wb_search("air.*pollution")

wbstats::wb_data("SH.STA.AIRP.FE.P5")


