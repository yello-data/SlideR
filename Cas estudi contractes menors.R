# 4.1. CAS ESTUDI
#difplot(df = catc_diffs18, break.point = "2018-03-09")
range(cm$data_adjudicacio, na.rm = T)
range(cm$import_adjudicat, na.rm = T)



cm <- contractes_menors |> 
  filter(data_adjudicacio < as.Date("2019/01/01") & data_adjudicacio > as.Date("2017/12/31"),
         import_adjudicat > 0, import_adjudicat < 25000) |> 
  mutate(dates = if_else(between(data_adjudicacio, as.Date("2018-02-20"), as.Date("2018-03-09")),
                         "Deadline", "Altre"),
         import = if_else(between(import_adjudicat, 21000, 22500), "22000", "Altre"))

mutate(import_cat = case_when(import_adjudicat > 50000 ~ "Molt per sobre",
                              import_adjudicat > 19000 ~ "Per sobre",
                              import_adjudicat > 18000 ~ "18s",
                              import_adjudicat > 17000 ~ "17s",
                              import_adjudicat > 16000 ~ "16s",
                              import_adjudicat > 15000 ~ "15s",
                              import_adjudicat > 14000 ~ "14s",
                              import_adjudicat > 13000 ~ "13s",
                              import_adjudicat > 10000 ~ "Ok",
                              TRUE ~ "Petit"))

cm |> 
  ggplot(aes(x = data_adjudicacio, y = import_adjudicat)) +
  geom_point(alpha = 0.2) +
  geom_vline(xintercept = as.Date("2018-03-09"), lty = 2, col = "blue")

cm |> 
  filter(dates == "Deadline" & import == "22000") |> 
  arrange(desc(import_adjudicat)) |> 
  count(proveidor, sort = T) |> 
  View()


contractes_menors |>
  filter(between(import_adjudicat, 17500, 18500)) |> 
  count(import_adjudicat, sort = T) |> 
  head(10) |> 
  pull(import_adjudicat) |> 
  range()


contractes_menors |> 
  filter(between(import_adjudicat, 17545, 18150)) |> 
  count(organ_contractant, sort = T)

contractes_menors |> 
  filter(between(import_adjudicat, 17545, 18150)) |> 
  count(proveidor, sort = T)





