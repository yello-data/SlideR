#####################################################################
######################## DATA MANAGEMENT ############################
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
# PART 1. TIDY DATA
#####################################################################

#- pivot_longer(df, cols, names_to, names_to)
#- pivot_longer(df, cols, names_to, names_to)
#- separate(df, col, into, sep)
#- unite(df, col, ..., sep)


## 1.1. UNTIDY DATA

### 1.1.1. Pivot longer
#- pivot_longer(df, cols, names_to, names_to)

table1
pivot_longer(table1, cols = cases:population, 
             names_to = "variables", values_to = "values")

### 1.1.2. Pivot wider
#- pivot_wider(df, names_from, values_from)

table2
pivot_wider(table2, names_from = type, values_from = count)


## 1.1.3. Separate
#- separate(df, col, into, sep)

separate(table3, rate, into = c("cases", "population"))


## 1.1.4. Unite
#- unite(df, col, ..., sep)


# 1.2. ExErCiSe!!!

?world_bank_pop
world_bank_pop |> 
  pivot_longer(`2000`:`2017`, names_to = "year", values_to = "vals") |> 
  pivot_wider(names_from = indicator, values_from = vals)

tidyr::relig_income |> 
  select(1:6)





#####################################################################
# PART 2. JOIN DATA
#####################################################################

full_join
left_join
right_join
inner_join

## 2.1. SITUATION 1: SAME JOINING COLUMN

### 2.1.1. Dataframe 1
df1 <- tibble(country = c("France", "Germany", "Czechia"),
              code = c("FR", "DE", "CZ"),
              gdpcap = c(43518, 50801, 26378),
              pop = c(67, 83, 11))

### 2.1.2. Dataframe 2
df2 <- tibble(country = c("French Republic", 
                          "Federal Republic of Germany",
                          "Czech Republic"),
              cowc = c("FR", "DE", "CZ"),
              lang = c("French", "German", "Czech"))

### 2.1.3. Join
full_join(df1, df2, by = c("code" = "cowc"))

## 2.1. SITUATION 2: DIFFERENT JOINING COLUMN

### 2.2.1. Dataframe 1
df1 <- tibble(country = c("France", "Germany", "Czechia"),
              code = c("FR", "DE", "CZ"),
              gdpcap = c(43518, 50801, 26378),
              pop = c(67, 83, 11))

### 2.2.2. Dataframe 2
df2 <- tibble(country = c("French Republic", 
                          "Federal Republic of Germany",
                          "Czech Republic"),
              cowc = c("FRN", "GMY", "CZR"),
              lang = c("French", "German", "Czech"))

### 2.2.3. Join
library(countrycode)
df1$cowc <- countrycode(df1$code, "iso2c", "cowc")
df <- full_join(df1, df2, by = c("cowc"))



countrycode


#####################################################################
# PART 3. MISSING DATA
#####################################################################



#####################################################################
# PART 4. CODEBOOKS
#####################################################################






