#####################################################################
############################## DPLYR ################################
#####################################################################



#####################################################################
# PART 0. LEARNING CURVE
#####################################################################

# R learning curve
library(dplyr)
rlc <- tibble(class = 1:8,
             knowledge = (1:8)^3)
class <- 4
plot(rlc$class, rlc$knowledge)
points(rlc$class[class], rlc$knowledge[class], col = "red")



#####################################################################
# PART 1. SUMMARY
#####################################################################

# The dplyr package is THE R PACKAGE
# So let's load it
library(dplyr)
library(stringr)
library(ggplot2)

# At the end of this module, you will (almost) learn to do things like this 
lloguer_any |> 
  group_by(any, nom_districte) |> 
  summarize(preu = mean(preu, na.rm = T)) |> 
  ggplot(aes(x = any, y = preu, col = nom_districte)) +
  geom_line() +
  theme_minimal()


# Let's load the datasets

accidents #UO: Accident
contractes_menors #UO: contracte
festivals #UO: festival
municipi #UO: municipi
lloguer_any #UO: ??
rendacs #UO: ??
cens_gc #UO: individu



#####################################################################
# PART 2. VARIABLES
#####################################################################

# Very important to remember what we can do and what we can't do with a type of variable.

#     - Categorical nominal: No order
#     - Categorical ordinal: Order, distance not known
#     - Numeric interval: Order, distance is known, zero no meaning
#     - Numeric ratio: Order, distance is known, zero has meaning


## CATEGORICAL NOMINAL: No order
#    - Store as: Character vector, Factor
#    - Wrangle: Stringr package
#    - Equality: ==
#    - Equality: %in%
#    - Not equality: !=


## CATEGORICAL ORDINAL: Order, distance not known
#    - Store as: Ordered factor
#    - Wrangle: Forcats package
#    - Equality: ==
#    - Equality: %in%
#    - Not equality: !=
#    - More than: >
#    - More or equal than: >=
#    - Less than: <
#    - Less or equal than: <=


## NUMERIC INTERVAL: Order, distance is known, zero no meaning
#    - Store as: Numeric, Integer, Date
#    - Equality: ==
#    - Equality: %in%
#    - Not equality: !=
#    - More than: >
#    - More or equal than: >=
#    - Less than: <
#    - Less or equal than: <=
#    - Sometimes, sums and differences: +, -
#    - Also max, min, etc.



## NUMERIC RATIO: Order, distance is known, zero has meaning
#    - Store as: Numeric
#    - Equality: ==
#    - Equality: %in%
#    - Not equality: !=
#    - More than: >
#    - More or equal than: >=
#    - Less than: <
#    - Less or equal than: <=
#    - Sums: +
#    - Differences: -
#    - Multiplication: *
#    - Division: /
#    - Other: sqrt(), log(), exp(), max(), min(), mean()...



#####################################################################
# PART 3. DPLYR
#####################################################################

## 3.1. THE PIPE

# For the dplyr package (and other packages), not $ form anymore

#f(d$v)
# f(d, v)
# d |> f(v)

# then:
# f1(f1(d, v1), v2)
# d |> f1(v1) |> f2(v2)


## 3.2. ROW FUNCTIONS ##########################

### 3.2.1. FILTER
# Reduces the number of rows according to a certain criteria
lloguer_any
glimpse(lloguer_any)
unique(lloguer_any$nom_districte)
unique(lloguer_any$any)

lloguer_any |> 
  filter(nom_districte == "XXXXXXXXXX")

lloguer_any |> 
  filter(nom_districte == "XXXXXXXXXX" & any >= XXXXXX)

lloguer_any |> 
  filter(preu == max(preu, na.rm = T)) # <- na.rm!!! mean, median, max, min...

festivals |> 
  filter(str_detect(nom_del_festival, "Jazz"))



#### ExErSiSe!!!!

# Quins accidents s'han produït al carrer Agustina Saragossa?
accidents

# Quins accidents s'han produït en Dimecres (descripcio_dia_setmana) durant la segona quinzena del mes (dia_mes) ...
# ... en què el tipus de vehicle implicat era un taxi (desc_tipus_vehicle_implicat)
accidents







### 3.2.2. ARRANGE
# Arranges from MORE TO LESS or for LESS TO MORE

rendacs
glimpse(rendacs)

rendacs |> 
  arrange(seccio_censal)

rendacs |> 
  arrange(desc(XXXXXXX))

rendacs |> 
  arrange(desc(XXXXXXX), desc(XXXXXXX))


#### ExErSiSe!!!!

# Barri amb menys renda per càpita? (data problem?)
rendacs

# Filtra pel districte de Ciutat Vella i ordena les dades en ordre descendent 
rendacs |> 
  filter() |> 
  arrange()

# Festivals de música amb més assistents a Barcelona
festivals |> 
  filter(ambit == "Música") |> 
  arrange(desc(assistents_a_barcelona))



### 3.2.3. COUNT
#counts the number of categories in a vector

count(accidents, nom_carrer)

#Rànking de proveidors que han aconseguit més contractes menors a l'Ajuntament de Barcelona
contractes_menors


#Qui s'ha emportat més pasta 'menor'? To be continued...






## 3.3. COLUMN FUNCTIONS ##########################


### 3.3.1. SELECT
# Select columns
glimpse(accidents)

accidents |> 
  select(descripcio_tipus_dia, hora_dia, descripcio_causa_vianant, desc_tipus_vehicle_implicat, descripcio_sexe, edat)

accidents |> 
  select(-numero_expedient)

accidents |> 
  select(nom_districte, nom_barri, descripcio_torn:descripcio_victimitzacio)


#### ExErSiSe!!!!

# Filtra per ultim any, primer trimestre i Ciutat Vella...
# Selecciona nom_barri, preu i preu_m2
# Ordena per preu_m2 en ordre descendent
lloguer_any



  
### 3.3.2. MUTATE
# Modifies the values of a column or creates a new one

municipi |> 
  mutate(densitat = poblacio / superficie_km2)


#Municipis amb altitud superior a 900m, crea densitat i ordena de major a menor
municipi


#### ExErSiSe!!!!

#Filtra per festivals de titularitat privada...
#...crea la variable durada amb la durada del festival...
#...elimina la columna any (no és una variable), data_inici, data_fi...
#... ordena les dades per durada, ordre descendent
festivals




### 3.3.3. SUMMARIZE / SUMMARISE
# Summarizes data

contractes_menors |>
  summarize(import_total = sum(import_adjudicat))

accidents |> #min, max...
  summarize(edat_mitjana = mean(edat),
            n = n())

cens_gc |> 
  summarize(es_afusellat = mean(es_afusellat)) #percentatges, en variables 1-0

cens_gc |> 
  summarize(tarragona = mean(provincia_desaparicio == "Tarragona", na.rm = T)) #percentatges, si posem vector lògic


### 3.4.3. GROUP_BY
# Groups

#Mitjana assistència per àmbit
festivals |> 
  group_by(ambit) |> 
  summarize(assistencia = mean(assistents_a_barcelona, na.rm = T))

#nombre d'accidents per barri
accidents |>
  group_by(nom_districte, descripcio_torn) |> 
  summarize(m_edat = mean(edat))

#Preu lloguer i preu lloguer m2 per any i districte
lloguer_any |> 
  group_by(any, nom_districte) |> 
  summarize(preu = mean(preu, na.rm = T),
            preu_m2 = mean(preu_m2, na.rm = T),
            n = n())


#### ExErSiSe!!!!

# Quin proveidor s'ha emportat més pasta 'menor'? (group_by -> summarize -> sum)
contractes_menors 

# Quim proveidor s'ha emportat més pasta 'menor' als contractes de l'Ajuntament de Barcelona?
contractes_menors



lloguer_any #2
accidents #3
contractes_menors #2
festivals #3
municipi #1
lloguer_any #1
rendacs #1
cens_gc #1











#####################################################################
# PART 3. RECODING
#####################################################################


## 3.1. BOOLEAN OPERATORS

#    - AND: & (all conditions must be present)
#    - OR: | (any condition can be present)
#    - NOT: ! (returns the contrary of the condition)

ctr_pov <- tibble(country = c("Armenia", "Austria", "Benin", "Bolivia",
                              "Brazil", "Colombia", "El Salvador",
                              "Ethiopia", "Honduras", "Indonesia"),
                  continent = c("ASI", "EUR", "AFR", "AME", "AME", "AME", 
                                "AME", "AFR", "AME", "ASI"),
                  poverty = c(1.90, 0.7, 49.6, 6.4, 3.4, 4.5, 
                              1.9, 26.7, 16.2, 7.2))

# African countries AND less than 30% below poverty line
ctr_pov$continent == "AFR" & ctr_pov$poverty > 30

# Not from Africa AND less than 10% below poverty line  
ctr_pov$continent != "AFR" & ctr_pov$poverty < 10

# American countries OR less than 30% below poverty line
ctr_pov$continent == "AME" | ctr_pov$poverty > 30

# NOT from Africa / less than 30% below poverty line
!ctr_pov$continent %in% c("AFR", "AME") | ctr_pov$poverty < 10



## 3.2. IF_ELSE (dplyr)
#    - if_else(condition, if TRUE, if FALSE)

if_else(strings$continent == "Americas", 1, 0)

if_else(strings$continent %in% c("Europe", "Asia"), "Eurasia", strings$continent)

if_else(polity$polity2 > 5, "Democracy", "Autocracy")

if_else(ratio$milex > 800000 & ratio$milper > 900, "Superpower", "Great Power")



## 3.3. CASE_WHEN (dplyr)
#    - case_when(condition1 ~ if TRUE,
#                condition2 ~ if TRUE,
#                condition3 ~ if TRUE,
#                ..., 
#                TRUE ~ if TRUE)

case_when(polity$polity2 > 5 ~  "Democracy", 
          polity$polity2 > -5 ~  "Anocracy",
          TRUE ~ "Autocracy")

polity$century <- case_when(polity$year < 1800 ~  "18c", 
                            polity$year < 1900 ~  "19c",
                            polity$year < 2000 ~  "20c",
                            TRUE ~ "21c")

## 3.4. FACTOR
#    - factor(vector)
#    - factor(vector)

factor(polity$century,
       ordered = TRUE,
       c("18c", "19c", "20c", "21c"))


## 3.5. RECODE
#    - recode(vector, old_value, "new_value")

recode(ords$donor, `Turkey-TIKA` = "Turkey-TIK")
recode(ratio$country, UKG = "GBR", FRN = "FRA", GMY = "DEU")

## 3.6. AS.XXXXXX
#    - as.numeric(vector)
#    - as.factor(vector)
#    - as.character(vector)
#    - as.integer(vector)
#    - as.Date(vector)

polity <- polity |>
  mutate(across(country:polity2, ~ as.character(.)))


polity$year <- as.numeric(polity$year)
polity$polity2 <- as.numeric(polity$polity2)
