qog <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_cs_jan23.csv")
library(countrycode)

qog |>
  transmute(ccown = ccodecow, ccowc = ccodealp, pais = cname,
            continent = countrycode(ccodecow, "cown", "continent"),
            regio = countrycode(ccodecow, "cown", "region"),
         poblacio = pwt_pop,
         pibcap_2011 = mad_gdppc,
         pibcap_1900 = mad_gdppc1900,
         despesa_educacio = wdi_expedu,
         despesa_militar = wdi_expmil,
         religio_llib = case_when(ccp_freerel == 1 ~ "Sí",
                                  ccp_freerel == 2 ~ "No",
                                  .default = NA),
         n_aliances = atop_number,
         confianca_altres = wvs_trust,
         co2_capita = wdi_co2,
         gini = wdi_gini,
         democracia = vdem_libdem) |> 
  write_csv("qog.csv")
