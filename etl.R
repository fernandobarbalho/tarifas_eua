library(tidyverse)

comercio_paises_mundo <- 
  read_csv("TradeData_4_4_2025_15_19_17.csv") %>%
  bind_rows(
    read_csv("TradeData_4_4_2025_15_19_37.csv")
  )


comercio_paises_eua <- 
  read_csv("TradeData_4_4_2025_15_20_19.csv") %>%
  bind_rows(
    read_csv("TradeData_4_4_2025_15_20_11.csv")
  )




trabalho_mundo<-
comercio_paises_mundo %>%
  select(reporterISO, reporterDesc, flowDesc, partnerDesc,  primaryValue ) %>%
  summarise(total= sum(primaryValue),
            .by= c(reporterISO, reporterDesc, flowDesc, partnerDesc) ) %>%
  pivot_wider(names_from = flowDesc, values_from = total ) %>%
  janitor::clean_names() %>%
  mutate(saldo = export - import,
         saldo_rel_exportacao = (saldo/export)*100) 



trabalho_eua<-
comercio_paises_eua %>%
  select(reporterISO, reporterDesc, flowDesc, partnerDesc,  primaryValue ) %>%
  summarise(total= sum(primaryValue),
            .by= c(reporterISO, reporterDesc, flowDesc, partnerDesc) ) %>%
  pivot_wider(names_from = flowDesc, values_from = total ) %>%
  janitor::clean_names() %>%
  mutate(saldo = export - import,
         saldo_rel_exportacao = (saldo/export)*100) 


comercio_exterior<-
  bind_rows(trabalho_mundo, trabalho_eua)


saveRDS(comercio_exterior, "comercio_exterior.rds")
