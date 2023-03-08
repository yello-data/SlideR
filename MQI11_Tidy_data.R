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
# PART 1. TIDY DATA
#####################################################################

#- pivot_longer(df, cols, names_to, names_to)
#- pivot_longer(df, cols, names_to, names_to)
#- separate(df, col, into, sep)
#- unite(df, col, ..., sep)

# 1.0 WHY DO WE NEED TIDY DATA??
# A la província d'Almeria, distribució del percentatge de vots al pp, cs i vox
elecc19


## 1.1. UNTIDY DATA

### 1.1.1. Pivot longer
#- pivot_longer(df, cols, names_to, values_to)
pivot_longer(table4a) #cases
pivot_longer(table4b) #population

### 1.1.2. Pivot wider
#- pivot_wider(df, names_from, values_from)
pivot_wider(table2)


## 1.1.3. Separate
#- separate(df, col, into, sep)
separate(table3, rate, into = c("cases", "population"), sep = "/")


## 1.1.4. Unite
#- unite(df, col, ..., sep)
table3 |> 
  unite("country_year", country:year, sep = "-")


## 1.1.5. Exercises!!

# IDESCAT: Geom_tile amb habitants, 
#https://www.idescat.cat/indicadors/?id=aec&n=15225



# WORLD BANK: combo
?world_bank_pop
tidyr::world_bank_pop |> 
  pivot_longer(`2000`:`2017`, names_to = "year", values_to = "vals") |> 
  pivot_wider(names_from = indicator, values_from = vals)

# Another example
tidyr::relig_income




#####################################################################
# PART 2. MISSING DATA
#####################################################################


## 2.1. HOW TO DEAL WITH MISSING DATA

### 2.1.1. EXPLORATORY
# summary()
summary(municipi)
summary(lloguer_any)


### 2.1.2. RETURNS 'TRUE' THE NA VALUES 
is.na(lloguer_any$preu)

#... therefore ...
lloguer_any |> 
  filter(!is.na(preu))


### 2.1.3. REMOVES NA FROM SUMMARY OPERATIONS
mean(vector, na.rm = T)


### 2.1.4. ASSIGNS A VALUE TO NA
replace_na(lloguer_any$preu, 150)

### 2.1.5. VISUALIZES NA [TOP CHOICE]
naniar::vis_miss()






#####################################################################
# PART 3. JOIN DATA
#####################################################################

# 2.1. JOIN FUNCTIONS

# xxxx_join(df1, df2, by = c("join_variable_df1" = "join_variable_df2"))

#full_join
#left_join
#right_join
#inner_join

## 2.1. SITUATION 1: SAME JOINING COLUMN

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

# Renda familiar disponible:
# https://www.idescat.cat/pub/?id=rfdbc&n=13301&by=mun

# Generació de residus per càpita
# https://www.idescat.cat/pub/?id=resmc&n=6997&by=mun&t=201900





## 2.2. SITUATION 2: DIFFERENT JOINING COLUMN

### 2.2.1. Dataframe 1
df1 <- tibble(country = c("France", "Germany", "Czechia", "Austria"),
              code = c("FR", "DE", "CZ", "AT"),
              gdpcap = c(43518, 50801, 26378, NA),
              pop = c(67, 83, 11, 9))

### 2.2.2. Dataframe 2
df2 <- tibble(country = c("French Republic", 
                          "Federal Republic of Germany",
                          "Czech Republic", "United States of America"),
              cowc = c("FRN", "GMY", "CZR", "USA"),
              lang = c("French", "German", "Czech", "English"))


### 2.2.3. Countrycode package

## What is it?
library(countrycode)
countrycode::cldr_examples
countrycode::codelist
countrycode::codelist_panel

# How it works?
# countrycode(variable_with_code, "code_origin", "code_destination")

#Option A: Pipe system
df1 |> 
  mutate(cowc = countrycode(code, "iso2c", "cowc")) |> 
  left_join(df2, by = c("cowc"))

#Option B: Without pipe
df1$cowc <- countrycode(df1$code, "iso2c", "cowc")
df_join <- full_join(df1, df2, by = c("cowc"))


#Language
df_join |> 
  mutate(pais = countrycode(code, "iso2c", "cldr.short.ca"))



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
  scale_y_continuous()


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


