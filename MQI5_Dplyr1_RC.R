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
class <- 3
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


# Let's load the datasets

accidents #UO: Accident
contractes_menors #UO: contracte
festivals #UO: festival
municipi #UO: municipi
lloguer_any #UO: ??
rendacs #UO: ??
cens_gc #UO: individu


# In few sessions, you will (almost) learn to do things like this 
lloguer_any |> 
  group_by(any, nom_districte) |> 
  summarize(preu = mean(preu, na.rm = T)) |> 
  ggplot(aes(x = any, y = preu, col = nom_districte)) +
  geom_line() +
  theme_minimal()




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
  filter(preu == max(preu, na.rm = T)) # <- na.rm!!! in functions mean, median, max, min...

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

# Filtra pel districte de Ciutat Vella i ordena les dades per preu en ordre descendent 
rendacs |> 
  filter() |> 
  arrange()

# Festivals de música amb més assistents a Barcelona
festivals |> 
  filter(ambit == "XXXXXXX") |> 
  arrange(desc(assistents_a_barcelona))



### 3.2.3. COUNT
#counts the number of categories in a vector

accidents |> 
  count(nom_carrer)

#Rànking de proveidors que han aconseguit més contractes menors a l'Ajuntament de Barcelona
contractes_menors


#Qui s'ha emportat més pasta 'menor'? To be continued...






## 3.3. COLUMN FUNCTIONS ##########################


### 3.3.1. SELECT
# Select vectors
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
# Modifies the values of a vector or creates a new one

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


