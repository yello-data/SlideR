#####################################################################
########################### DATASETS ################################
#####################################################################



#####################################################################
# PART 1. SUMMARY
#####################################################################

#Key idea. When we quantify the world, 
# - Observations: What we study.
#     - Normally we put the observations in the of a dataset.
# - Variables: Characteristics of what we study.
#     - Normally we put the variables in the columns of a dataset.

# Concepts
"Table" > "Dataframe"
"Table" > "Dataset"
"Dataframe" < "Dataset"

"Variable" == "Column" #not always
"Observation" == "Row" #not always


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
uni


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
suicide

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
strings <- tribble(~iso3c, ~country, ~currency, ~continent, ~region,
                   "CMR", "Cameroon", "CFA Franc BEAC", "Africa", "Sub-Saharan Africa",
                   "COL", "Colombia", "Colombian Peso", "Americas", "Latin America & Caribbean",
                   "CUB", "Cuba", "Cuban Peso", "Americas", "Latin America & Caribbean",
                   "FRA", "France", "Euro", "Europe", "Europe & Central Asia",
                   "LSO", "Lesotho", "Loti", "Africa", "Sub-Saharan Africa",
                   "QAT", "Qatar", "Qatari Rial", "Asia", "Middle East & North Africa",
                   "TWN", "Taiwan", "New Taiwan Dollar", "Asia", "East Asia & Pacific",
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
ords <- tibble(donor = c("US-MCC", "Canada-GAC", "Germany-BMZ-GIZ", "Korea-KOICA", "Australia-DFAT", "Spain-AECID", 
                         "Saudi Arabia-KSRelief", "Norway-MFA", "China-MOFCOM", "Turkey-TIKA"),
               ati = factor(c("Very Good", "Good", "Good", "Good", "Good", "Fair",
                              "Poor", "Poor", "Very Poor", "Very Poor"),
                            ordered = TRUE,
                            levels = c("Very Poor", "Poor", "Fair", "Good", "Very Good")),
               regime_type = factor(c("Flawed Democracy", "Full Democracy", "Full Democracy", "Full Democracy", "Full Democracy", 
                                      "Flawed Democracy", "Authoritarian", "Full Democracy", "Authoritarian", "Hybrid Regime"), 
                                    ordered = TRUE,
                                    levels = c("Authoritarian", "Hybrid Regime", "Flawed Democracy", "Full Democracy")))
ords

### 2.2.2. STORAGE
#    - Ordered factor

ords$ati <- factor(ords$ati,
                   ordered = TRUE,
                   levels = c("Very Poor", "Poor", "Fair", "Good", "Very Good"))

ords$ati
class(ords$ati)
typeof(ords$ati)
unclass(ords$ati)


### 2.2.3. OPERATIONS
#    - Equality: ==
#    - Equality: %in%
#    - Not equality: !=
#    - More than: >
#    - More or equal than: >=
#    - Less than: <
#    - Less or equal than: <=


ords$regime_type >= "Hybrid Regime"

ords[ords$regime_type != "Full Democracy",]
ords[ords$ati >= "Good",]
ords[ords$ati < "Fair",]


### 2.2.4. FORCATS PACKAGE
library(forcats)
fct_relevel(ords$regime_type, "Hybrid Regime", "Full Democracy", "Flawed Democracy", "Authoritarian")
fct_rev(ords$ati)
fct_other(ords$ati, keep = c("Good", "Very Good"))
plot(fct_infreq(ords$ati))
plot(fct_rev(fct_infreq(ords$ati)))



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



# Difference between the foundation of the US and the foundation of the USSR
polity$year[polity$country == "United States"] - polity$year[polity$country == "USSR"]

# Which political regimes had more democracy than the US in the year of their foundation?
polity$country[polity$polity2[polity$country == "United States"] > polity$polity2]



## 2.4. RATIO

#     - Numeric ratio: Order, distance is known, zero has meaning


### 2.4.1. DATAFRAME
ratio <- tibble(country = c("USA", "UKG", "FRN", "GMY", "ITA", "RUS", "JPN"),
                milex = c(980000, 7895671, 1023651, 12000000, 669412, 5984123, 1699970),
                milper = c(334, 394, 581, 2750, 581, 1789, 957),
                tpop = c(131028, 47762, 41900, 79798, 44020, 170317, 71380),
                cinc = c(0.182, 0.0997, 0.0396, 0.178, 0.0270, 0.138, 0.0591))
ratio


### 2.4.2. STORAGE
#    - Numeric

# 1.2e+07?
12000000

# 2.50+e08
250000000


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



## Which countries have military personnel above 1M?
ratio$country[ratio$milper > 1000]

## How many times the military expenditure of Germany is superior to the other great powers?
ratio$milex_gmy <- ratio$milex[ratio$country == "GMY"] / ratio$milex
ratio

## Which country has a military personnel below 1 percent of its population?
ratio$country[1 > ratio$milper / ratio$tpop * 100]

## Which country had higher CINC index?
ratio$country[ratio$cinc == max(ratio$cinc)]

## Which country had less military expenditure?
ratio$country[ratio$milex == min(ratio$milex)]


## Which countries have military personnel above 1M?
cinc[cinc$milper > 10000, ]


### 2.4.4. EXeRcIsE!!!!!

cinc <- read_csv("https://correlatesofwar.org/wp-content/uploads/NMC_v4_0.csv")
cinc

# Countries in 1816?

# Highest cinc index?

# Lowest military personnel per capita (milper/tpop)?



#####################################################################
# PART 3. RECODING
#####################################################################


## 3.1. BOOLEAN OPERATORS



## 3.2. IF_ELSE



## 3.3. CASE_WHEN



## 3.4. FACTOR



## 3.5. RECODE










