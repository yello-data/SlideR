#####################################################################
#################### OBJECTS AND FUNCTIONS ##########################
#####################################################################



#####################################################################
# PART 1. SUMMARY
#####################################################################

#Key idea. Working with R is: 
# A) creating objects
# B) applying functions to objects

# A. OBJECTS
## Vector:        c(value1, value2, value3, value4, ...)
## Dataframe:     tibble(vector1, vector2, vector3, vector4, ...)
library(dplyr)
elections <- tibble(country = c("Colombia", "Japan", "Germany", "Chile", "New Zealand"), 
                    year = as.integer(c(2022, 2021, 2021, 2021, 2020)), 
                    date = as.Date(c("2022/05/29", "2021/10/31", "2021/09/26", "2021/11/21", "2020/10/17")), 
                    turnout = c(54.98, 55.97, 76.58, 47.33, 82.24), 
                    continent = factor(c("America", "Asia", "Europe", "America", "Oceania")), 
                    presidential = c(TRUE, FALSE, FALSE, TRUE, FALSE))
elections

# B. FUNCTIONS
## Function:      function(arg1, arg2, arg3, arg4)
dim(elections)
unique(elections$year)
mean(elections$turnout)
sort(elections$turnout, decreasing = TRUE)





#####################################################################
# PART 2. OBJECTS
#####################################################################

## 2.1. BASIC RULES

# A. Save an object 
name <- 100
name

# B. Names that an object can receive
1object <- 34 # error
objec! <- 44 # error
object first <- 54 # error
object_first <- 64 #correct
`object first` <- 74 #correct

# C. Overwrite
name <- 100
name <- 50
name

# D. Case sensitive
Num <- 1001
num <- 101
Num
num






## 2.2. VALUES

### 2.2.1. NUMERIC VALUES

d <- 4
d
class(d)

# Collapse operations
t <- (4 + 6 + 7) / 3
t

# Use objects previously created
d + t + 1




## 2.2.2. CHARACTER VALUES

z <- "Obama"
class(z)

# Case sensitive
"Obama" == "OBama"
"OBAMA" == "OBAMA"


## 2.2.3. LOGICAL VALUES

blonde <- T
blonde
thin <- F
thin

class(blonde)




#####################################################################
# PART 3. VECTORS
#####################################################################

## Vector:        c(value1, value2, value3, value4, ...)
# *A value is technically a vector of length one

## 3.1. NUMERIC (DOUBLE) VECTORS

numeric <- c(4, 5, 6, 7, 8)
num_dec <- c(54.98, 55.97, 76.58, 47.33, 82.24)
class(numeric)

# Can use objects inside the vector
e <- 3
num_mix <- c(1, 3, e, 4, d)

#Two points, create a sequence
1:100
num_points <- c(e, 4:7)


## Operations between numeric vectors (+, - , *, /)
num_mix / 10 #between a vector an one value
numeric + num_dec #between two vectors of equal length


# To check the length of a vector
length(num_dec)




## 3.2. INTEGER VECTORS

#They can be stored with an L at the end of each value...
integer_L <- c(3L, 6L, 7L, 5L, 9L)
integer_L
class(integer_L)

#...or with the function as.integer()
integer_F <- as.integer(c(2022, 2021, 2021, 2021, 2020))
integer_F


## 3.3. CHARACTER VECTORS

character <- c("Colombia", "Japan", "Germany", "Chile", "New Zealand")
class(character)

# Can store anything
numbers <- c("4", "8", "15", "16", "23")
mingle <- c("4$", "אַפֿגהאַניסטאַן", "الإحصاء", "%16", "統計学3")

#Error!!!
c("C", "J", G", "C", "N")
"


## 3.4. FACTORS

factor <- factor(c("America", "Asia", "Europe", "America", "Oceania"))
class(factor)
unclass(factor) #actually, a factor is an integer with a label!


## 3.5. LOGICAL VECTORS

logic <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
class(logic)

act <- c("Johnny Depp", "Leo DiCaprio", "Jessica Chastain", "Lady Gaga", "Anthony Hopkins")
hair <- c("brown",      "blond",        "red",              "blond",     "white")
born <- c(1963,          1974,           1977,               1986,        1937)

act == "Johnny Depp"
born > 1975


## Symbol OR (`|`)
blond_or_1975 <- hair == "blond" | born > 1975

## Symbol AND (`&`)
blond_and_1975 <- hair == "blond" & born > 1975


## 3.6. DATE VECTORS

date <- as.Date(c("2022/05/29", "2021/10/31", "2021/09/26", "2021/11/21", "2020/10/17"))
class(date)

as.Date("2021/02/23") - as.Date("2018/05/15")


## 3.7. COMMON ERRORS, Do read the console!!!!!
c(4, 6 7, 8)
c(TRUE, FALSE, TRUE
c("France, "Spain", "UK")
"





#####################################################################
# 4. DATAFRAME (or TIBBLE)
#####################################################################

## Dataframe:     tibble(vector1, vector2, vector3, vector4, ...)


## 4.1. CREATE A DATAFRAME

# Option A: Create each vector, then join vectors
country <- c("Colombia", "Japan", "Germany", "Chile", "New Zealand")
year <- as.integer(c(2022, 2021, 2021, 2021, 2020))
date <- as.Date(c("2022/05/29", "2021/10/31", "2021/09/26", "2021/11/21", "2020/10/17"))
turnout <- c(54.98, 55.97, 76.58, 47.33, 82.24)
continent <- factor(c("America", "Asia", "Europe", "America", "Oceania"))
presidential <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
elections <- tibble(country, year, date, turnout, continent, presidential)

# Option B: Do it in a single saving
elections <- tibble(country = c("Colombia", "Japan", "Germany", "Chile", "New Zealand"), 
                    year = as.integer(c(2022, 2021, 2021, 2021, 2020)), 
                    date = as.Date(c("2022/05/29", "2021/10/31", "2021/09/26", "2021/11/21", "2020/10/17")), 
                    turnout = c(54.98, 55.97, 76.58, 47.33, 82.24), 
                    continent = factor(c("Europe", "Asia", "Europe", "America", "Oceania")), 
                    presidential = c(TRUE, FALSE, FALSE, TRUE, FALSE))

# Explore a dataframe
elections
dim(elections)
glimpse(elections)
summary(elections)



## 4.2. USE A DATAFRAME

### To employ a vector in a dataframe, use $
elections$country

elections$turnout <- elections$turnout / 100
elections$turnout




#####################################################################
#5. OBJECT SELECTION
#####################################################################

## Selection:      object[ ]




## 5.1. SELECTING VECTORS 

## Use a numeric vector:      vector[numeric_vector]
## Use a logical vector:      vector[logical_vector]


### 5.1.1. Numeric vector
elections$country
elections$country[c(1,3)]
elections$country[2:5]
elections$country[-c(3)]

### 5.1.2. Logical vector (equal length)
elections$country
elections$country[c(FALSE, TRUE, FALSE, TRUE, FALSE)]
elections$country[elections$presidential]


## 5.2. SELECTING DATAFRAMES

df[rows,columns]

## Examples
elections[1:3, ]
elections[c(2,5), 1:4]
elections[elections$presidential, -c(2,6)]
elections[c(1:3), c("country","presidential")]
elections[elections$presidential,]



## 5.3. ExErCiSe!!!! 

# Packages and file
library(dplyr)
library(readr)
ches_la <- read_csv("https://www.chesdata.eu/s/ches_la_2020_aggregate_level_v01.csv")

# Observe the ches_la object
glimpse(ches_la)

# Which countries are in the dataset?
## Hint: Unique names of vector country
unique(ches_la$country)

# Select Colombia and create a new object named ches_co
## Hint: Use selection claudators and a logical vector in the rows
ches_co <- ches_la[ches_la$country == "Colombia", ]
ches_co

# Observe the new dataframe (how many observations?)
glimpse(ches_co)

# Keep party variables, lrecon, galtan, crime, regions, ethnic_minorities
ches_co[, c("party_id", "party_abb", "party", "lrecon", "galtan", "crime", "regions", "ethnic_minorities")]

# Overwrite the object ches_co with the transformation
ches_co <- ches_co[, c("party_id", "party_abb", "party", "lrecon", "galtan", "crime", "regions", "ethnic_minorities")]





#####################################################################
# 6. FUNCTIONS
#####################################################################

## Function:      function(arg1, arg2, arg3, arg4)


## 6.1. HOW TO CREATE A FUNCTION

### With one argument
per_tres <- function(x) {x * 3}

per_tres(x = 4)
per_tres(x = elections$turnout)
per_tres(x = 5:20)

### With more arguments
per_tres_menys <- function(x, y = 0) {x * 3 - y}

per_tres_menys(x = 10, y = 5)

## 6.2. HOW FUNCTIONS WORK

### Not necessary to use the names of arguments, but respect order
per_tres_menys(10, 5)
### Order can be consulted
args(per_tres_menys)
### Some arguments have default responses
per_tres_menys(10)
### Functions can operate inside other functions 
per_tres(per_tres_menys(10, 5))
per_tres_menys(per_tres(10), 5)


## 6.2. TYPE OF FUNCTIONS

#nig <- read_csv("data/nig.csv")
nig <- read_csv("https://www.jordimas.cat/files/nig.csv")

### 6.2.1. Without arguments
ls()
names(installed.packages()[, 1])
search()
getwd()

### 6.2.2. With normally one argument

# Glimpse the nig dataframe:
glimpse(nig)

# Which are the different languages of the surveyed people?
unique(nig$language)

# How many different languages do we have in the survey?
length(unique(nig$language))

# How many citizens have soldiers nearby?
table(nig$soldiers_nearby)

# Does people trust with the armed forced?
barplot(table(nig$armed_forces_trust))

# A clarification:
barplot(table(nig$armed_forces_trust),
        names.arg	= c("A lot", "Don't know", "Just little", "Not at all", "Refused", "Somewhat"))


### 6.2.3. With several arguments

#### Easy to remember
c()
tibble()
table() 

table(nig$situation_country, nig$electricity_nearby)

#### Hard to remember -> Help!!
sample()
args(sample)
?sample

## x
sample(1:10)

## size
sample(1:100, 2)

## replace
moneda <- c("Cara", "Creu")
sample(moneda, 5, replace = TRUE)

## prob
moneda <- c("Cara", "Creu")
sample(moneda, 10, replace = TRUE, prob = c(0.2, 0.8))


### Help

### Quadre d'ajuda: -> ?function
### Google
### Cheat sheets
### Stackoverflow

#### ?mean
mean(elections$turnout)
mean(elections$turnout, trim = 0.2)

## ?str_replace
str_replace(elections$country, "Germany", "France")

## ?who
who


## Exercises

### Build this sequence: 1, 5, 10, 15, 20, 25, 30, 35
seq()
### Build this sequence: "G" "T" "G" "T" "G" "T"
rep()






#####################################################################
# PART 7. IMPORT FUNCTIONS
#####################################################################

library(readr)
library(foreign)
library(haven)
library(readxl)

read_csv("data/gapminder.csv") #readr
read_csv2("data/gapminder2.csv") #readr
read_tsv("data/gapminder3.tsv") #readr
read_delim("data/gapminder4.txt", delim = "/") #readr

tibble(foreign::read.dta("data/gapminder5.dta")) #foreign
load("data/gapminder6.Rdata") #base
tibble(foreign::read.spss("data/gapminder7.sav", to.data.frame = T)) #haven
read_xlsx("data/gapminder8.xlsx", sheet = 2) #excel

