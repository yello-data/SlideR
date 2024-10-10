#Rulers, Elections and Irregular Governance dataset (v2021.8)
reign <- read_csv("data/REIGN_2021_8.csv")

library(tidyverse)
reign |> 
  select(ccode, country, leader, year, month, elected, age, male, militarycareer, tenure_months, government) |> 
  filter(leader %in% c("Zelensky", "Clinton")) |>
  slice(92:120) |> 
  select(ccode:tenure_months, gov_democracy) |> 
  head(10)


reign |> 
  group_by(leader) |> 
  filter(tenure_months == max(tenure_months)) |> 
  ggplot(aes(x = age)) +
  geom_histogram()

unique(reign$country)

reign |> 
  filter(country %in% c("USA", "UKG", "Belgium", "Brazil", "Uruguay", "China", "India")) |> 
  group_by(leader) |> 
  filter(tenure_months == max(tenure_months)) |> 
  ggplot(aes(x = country, y = age)) +
  geom_boxplot()


reign |>
  select(country, leader, year, age, male, tenure_months, government) |> 
  mutate(age = if_else(age > 65, "Majors de 65 anys", "65 anys o menys")) |> 
  group_by(country, leader, government) |> 
  filter(tenure_months == max(tenure_months),
         government %in% c("Presidential Democracy", "Parliamentary Democracy",
                           "Party-Military", "Monarchy")) |> 
  ggplot(aes(x = government, y = tenure_months, col = as.character(male),
             shape = age)) +
  geom_point(position = position_jitter(width = .1), alpha = 0.5) +
  scale_color_grey() +
  coord_flip() +
  theme_light()


reign |>
  transmute(leader, age, male = as.character(male), tenure_months, government) |> 
  mutate(age = if_else(age > 65, "More than 65 years", "65 or less years")) |> 
  group_by(leader, government) |> 
  filter(tenure_months == max(tenure_months),
         government %in% c("Presidential Democracy", "Parliamentary Democracy",
                           "Party-Military", "Monarchy")) |> 
  filter(leader %in% c("Zelensky", "Clinton", "Vladimir Putin", "Thatcher")) |> 
  ungroup() |> 
  select(-tenure_months) |> 
  pivot_longer(age:government, names_to = "variables", values_to = "values")



reign |> 
  filter(leader %in% c("Zelensky", "Clinton", "Putin", "Thatcher", "De Gaulle")) |> 
  group_by(leader) |> 
  filter(year == first(year) & month == first(month)) |> 
  select(country, leader, year, government, militarycareer)






