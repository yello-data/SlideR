#####################################################################
########################### DATASETS ################################
#####################################################################



#####################################################################
# PART 0. LEARNING CURVE
#####################################################################

# Usual learning curve
library(dplyr)
ulc <- tibble(class = 1:8,
             knowledge = 1:8)
plot(ulc$class, ulc$knowledge)

# R learning curve
rlc <- tibble(class = 1:8,
             knowledge = (1:8)^3)
class <- 3
plot(rlc$class, rlc$knowledge)
points(rlc$class[class], rlc$knowledge[class], col = "red")



#####################################################################
# PART 1. SUMMARY
#####################################################################

#Key idea. When we quantify the world, we need two things: 
# - Observations: What we study.
#     - Normally we put the observations in the of a dataset.
# - Variables: Characteristics of what we study.
#     - Normally we put the variables in the columns of a dataset.

# Concepts
"Table" > "Dataset"
"Table" > "Dataframe"
"Dataframe" < "Dataset"

"Variable" == "Column" #*not always
"Observation" == "Row" #*not always


#####################################################################
# PART 1. OBSERVATIONS
#####################################################################

# - Observations: What we study.
#     - Unit of analysis
#     - Unit of observation


# 1.1. OBSERVATIONS
# Check the slides


# 1.2. ECOLOGICAL INFERENCE PROBLEM

# More info: https://en.wikipedia.org/wiki/Simpson%27s_paradox

#Packages
library(dplyr)
library(tidyr)
library(readr)

# 1.2.1. UC BERKELEY
# Tibble with admissions to different undergraduate programmes
uni <- tibble(faculty = rep(c("Psysics", "Politics", "Journalism"), times = 1, each = 2),
              sex = rep(c("Men", "Women"), times = 3),
              applicants = c(825,108,325,593,181,392),
              admitted = c(520, 76, 119, 203, 52, 115))

# What is aggregate data (uni level) telling us?
uni |> 
  group_by(sex) |> 
  summarize(applicants = sum(applicants),
            admitted = sum(admitted)) |> 
  mutate(perc_admissions = admitted / applicants * 100)

#But when we disaggregate the data faculty level ...
uni |> 
  mutate(perc_admissions = admitted / applicants * 100) |>
  arrange(desc(perc_admissions))


## 1.2.2. DURKHEIM SUICIDE
suicide <- tibble(region = c("Isère"),
                  municipality = rep(c("Grenoble", "Le Bourg-d'Oisans", "Saint-Jean-de-Maurienne"), times = 1, each = 2),
                  religion = rep(c("Protestant", "Catholic"), times = 3),
                  population = c(8250,1080,325,593,181,392),
                  suicide = c(520, 72, 12, 20, 5, 11))

#What aggregate data (regional level) is telling us?
suicide |> 
  group_by(religion) |> 
  summarize(population = sum(population),
            suicide = sum(suicide)) |> 
  mutate(perc = suicide / population * 100)

#But when we disaggregate the data ...
suicide |> 
  mutate(perc = suicide / population * 100) |>
  arrange(desc(perc))





#####################################################################
# PART 2. VARIABLES
#####################################################################

# - Variables: Characteristics of what we study.
#     - Categorical nominal: No order
#     - Categorical ordinal: Order, distance not known
#     - Numeric interval: Order, distance is known, zero no meaning
#     - Numeric ratio: Order, distance is known, zero has meaning


## 2.1. NOMINAL

#     - Categorical nominal: No order


### 2.1.1. DATAFRAME
strings <- tribble(~iso3c, ~country,           ~currency, ~continent, ~region,
                   "CMR", "Cameroon",          "CFA Franc BEAC",           "Africa",   "Sub-Saharan Africa",
                   "COL", "Colombia",          "Colombian Peso",           "Americas", "Latin America & Caribbean",
                   "CUB", "Cuba",              "Cuban Peso",               "Americas", "Latin America & Caribbean",
                   "FRA", "France",            "Euro",                     "Europe",   "Europe & Central Asia",
                   "LSO", "Lesotho",           "Loti",                     "Africa",   "Sub-Saharan Africa",
                   "QAT", "Qatar",             "Qatari Rial",              "Asia",     "Middle East & North Africa",
                   "TWN", "Taiwan",            "New Taiwan Dollar",        "Asia",     "East Asia & Pacific",
                   "TTO", "Trinidad & Tobago", "Trinidad & Tobago Dollar", "Americas", "Latin America & Caribbean")
strings


### 2.1.2. STORAGE
#    - Character
#    - Factor


### 2.1.3. OPERATIONS
#    - Equality: ==
#    - Equality: %in%
#    - Not equality: !=

strings$country == "France"
factor(strings$country) == "France"
strings$continent %in% c("Africa", "Americas", "Asia")
strings$currency != "Euro"
!strings$currency %in% c("Euro", "Loti", "Cuban Peso")


#Ergo
strings[strings$continent %in% c("Africa", "Europe"),]
sum(strings$region == "Latin America & Caribbean")


### 2.1.4. EXERCISE

# Show me the observations from Argentina
# How many observations from Colombia?
glimpse(ches_la)

## 2.1.5. BINARY VARIABLES

#Also called dummy or dichotomous.

ches_la$president

#red:
#elected:
#democracy:
#alliance: 
#successful coup d'état
#reg_aut of an ethnic group



### 2.1.6. STRINGR PACKAGE

library(stringr)
str_remove(strings$region, "Latin ")
str_replace(strings$country, "&", "and")
str_to_upper(strings$currency)
str_sub(strings$iso3c, end = 2)





## 2.2. ORDINAL

#     - Categorical ordinal: Order, distance not known


### 2.2.1. DATAFRAME
ords <- tibble(nom = c("Tània", "Pablo", "Sílvia", "Íñigo", "Carles", "Basha", "Isabel", "Santiago"),
               naixement = factor(c("1970s", "1970s", "1980s", "1960s", "1960s", "1980s", "1970s", "1970s"),
                               ordered = TRUE, levels = c("1960s", "1970s", "1980s")),
               independencia = factor(c("A favor", "Neutral", "A favor", "Neutral", "A favor", "A favor", "En contra", "En contra"),
                                      ordered = T, levels = c("En contra", "Neutral", "A favor")),
               immigracio =  factor(c("A favor", "A favor", "En contra", "Neutral", "Neutral", "A favor", "Neutral", "En contra"),
                                    ordered = T, levels = c("En contra", "Neutral", "A favor")),
               liberalisme = c("Baix", "Molt baix", "Mitjà", "Alt", "Alt", "Molt baix", "Molt alt", "Mitjà"))

#fixem-nos que liberalisme NO és ordinal i ho hauria de ser
ords




### 2.2.2. STORAGE
#    - Ordered factor

ords$liberalisme 

ords$liberalisme
class(ords$liberalisme)
typeof(ords$liberalisme)
unclass(ords$liberalisme)


### 2.2.3. OPERATIONS
#    - Equality: ==
#    - Equality: %in%
#    - Not equality: !=
#    - More than: >
#    - More or equal than: >=
#    - Less than: <
#    - Less or equal than: <=

#Qui té un nivell de liberalisme mitjà o més alt?
ords$liberalisme >= "Mitjà"
ords[ ,c(1,5)]

#Qui va néixer abans dels 1980s?
ords$naixement < 1980
ords[ , 1:2]

#Qui està en contra de la immigració?
ords$immigracio == "En contra"
ords[ , ]



### 2.2.4. FORCATS PACKAGE
library(forcats)
fct_relevel(ords$liberalisme, "Molt baix", "Baix", "Mitjà", "Alt", "Molt alt")
fct_rev(ords$liberalisme)
fct_other(ords$liberalisme, keep = c("Molt alt", "Alt"))
plot(fct_infreq(ords$liberalisme))
plot(fct_rev(fct_infreq(ords$liberalisme)))



## 2.3. INTERVAL

#     - Numeric interval: Order, distance is known, zero no meaning


### 2.3.1. DATAFRAME
polity <- tibble(country = c("United States", "Bolivia", "Australia", "Azerbaijan",
                             "USSR", "Timor Leste", "Eritrea", "Qatar", "Gambia"),
                 year = c(1776, 1825, 1901, 1991, 1922, 2002, 1993, 1971, 1965),
                 polity2 = c(0, -3, 10, -3, -7, 6, -6, -10, 8))
polity


### 2.3.2. STORAGE
#    - Numeric
#    - Integer
#    - Date


### 2.3.3. OPERATIONS
#    - Equality: ==
#    - Equality: %in%
#    - Not equality: !=
#    - More than: >
#    - More or equal than: >=
#    - Less than: <
#    - Less or equal than: <=
#    - Sometimes, sums and differences: +, -


# País fundat més tard de l'any 1950
polity$year > 1950
polity[,]

# Quin país té el màxim de democràcia?
polity$polity2 == max(polity$polity2)
polity[,]

#Diferència entre l'any mínim i l'any màxim de fundació (rang).
max(polity$year) - min(polity$year)




## 2.4. RATIO

#     - Numeric ratio: Order, distance is known, zero has meaning


### 2.4.1. DATAFRAME
ratio <- tibble(country = c("USA", "UKG", "FRN", "GMY", "ITA", "RUS", "JPN"),
                gdpcap = c(76329.32, 46126.12, 40886.34, 48718.02, 34776.90, 15270.2, 33823.24),
                lifeexp = c(76.4, 81.1, 82.3, 81.5, 83.2, 69, 84.3),
                tpop = c(333287564, 66971400, 67971316, 83797997, 58940431, 144236930, 125124992))
ratio




### 2.4.2. STORAGE
#    - Numeric

# Scientific notation: 1.2e+07?
12000000

# Scientific notation: 2.50e+08
250000000

options(scipen=999)
options(scipen=0)

### 2.4.3. OPERATIONS
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



## Quins països tenen una esperança de vida superior a 80 anys?
ratio$lifeexp > 80
ratio[, ]

## Població, en milions
ratio$tpop / 1000000

## Calculem PIB total (PIB per càpita * població)
ratio$gdpcap * ratio$tpop

## Quina és la mitjana de PIB per càpita?
mean(ratio$gdpcap)

## Quin és el país amb el mínim de població?
ratio$tpop == min(ratio$tpop)
ratio[,]



### 2.4.4. EXeRcIsE!!!!!

#https://www.idescat.cat/indicadors/?id=aec&n=15903&t=202200


# Poblacions per sobre els 1000 mts


# Poblacions d'"Osona"


# Població amb major altitud


# Població amb menys superfície

