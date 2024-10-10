library(tidyverse)

ucdp <- read_csv("GEDEvent_v24_0_3.csv")[-1,] |> 
  select(id, year, type = type_of_violence, conflict = conflict_name,
         side_a, side_b, where_coordinates, country, country_id, region,
         date_start:low)

glimpse(ucdp)

unique(ucdp$year)

ucdp |> 
  group_by(country) |> 
  summarize(best = sum(best)) |> 
  arrange(desc(best))

ucdp_isr <- ucdp |> 
  filter(country == "Israel") 

ucdp_isr |> 
  glimpse()
  count(where_coordinates, sort = T)

ucdp_isr |> 
  ggplot(aes(x = where_coordinates, y = deaths_civilians)) +
  geom_boxplot() +
  coord_flip()


a <- tibble(n = 1:5, v = n - 5:1)
a$v == 0
IQR(a$n) <= diff(range(a$n))
median(c(1, 12, 3, 5, 4, 6, 7, 8, 9, 10, 2))
IQR(c(1, 60, 50, 130, 10))  
sum(c(3, 7, 2, 4) != 5, na.rm = TRUE) 
