
# Mapa do comércio exterior (comércio com EUA)

## Column

### Mapa saldo comercial de bens e serviços referência EUA

```{r dpi=300}
renderPlotly({
  
  dados_mapa<-  
    world %>%
    left_join(
      comercio_exterior %>%
        filter(partner_desc == "USA") %>%
        rename(iso_a3 = reporter_iso) %>%
        mutate(saldo_rel_exportacao = round(saldo_rel_exportacao,1)) %>%
        inner_join(
          tarifas_trump %>%
            select(iso_a3, iso_a2)
        )
    ) 
  
  
  
  map<-
    
    dados_mapa %>%
    ggplot() +
    geom_sf(aes(fill = cut(saldo_rel_exportacao, 
                           breaks = c(-Inf, -100, seq(-90, 90, by = 10), 100, Inf)),
                text = paste(name_long, ifelse(is.na(saldo_rel_exportacao),"",": ") , ifelse(is.na(saldo_rel_exportacao),"",saldo_rel_exportacao) , ifelse(is.na(saldo_rel_exportacao),"","%")))) +
    scale_fill_discrete_divergingx(palette = "Rdbu",
                                   na.value = "gray50") +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#57A8C2"),
      text = element_text(colour = "black")
    ) +
    coord_sf(xlim = c(-180,180), ylim=c(-40,82))+
    labs(
      title = "Saldo da balança comercial com EUA (% das exportações)",
      caption = "Fonte: Guardian.",
      fill= "saldo"
    )
  
  
  ggplotly(map, tooltip = c("text"))
})




```


# Mapa do comércio exterior (comércio com mundo)

## Column

### Mapa saldo comercial de bens e serviços referência EUA

```{r dpi=300}
renderPlotly({
  
  dados_mapa<-  
    world %>%
    left_join(
      comercio_exterior %>%
        filter(partner_desc == "World") %>%
        rename(iso_a3 = reporter_iso) %>%
        mutate(saldo_rel_exportacao = round(saldo_rel_exportacao,1)) %>%
        inner_join(
          tarifas_trump %>%
            select(iso_a3, iso_a2)
        )
    ) 
  
  
  
  map<-
    
    dados_mapa %>%
    ggplot() +
    geom_sf(aes(fill = cut(saldo_rel_exportacao, 
                           breaks = c(-Inf, -100, seq(-90, 90, by = 10), 100, Inf)),
                text = paste(name_long, ifelse(is.na(saldo_rel_exportacao),"",": ") , ifelse(is.na(saldo_rel_exportacao),"",saldo_rel_exportacao) , ifelse(is.na(saldo_rel_exportacao),"","%")))) +
    scale_fill_discrete_divergingx(palette = "Rdbu",
                                   na.value = "gray50") +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#57A8C2"),
      text = element_text(colour = "black")
    ) +
    coord_sf(xlim = c(-180,180), ylim=c(-40,82))+
    labs(
      title = "Saldo da balança comercial com mundo (% das exportações)",
      caption = "Fonte: Guardian.",
      fill= "saldo"
    )
  
  
  ggplotly(map, tooltip = c("text"))
})




```
dado_texto<- tibble(x=-160, y=-50, text = "Elaboração: Fernando Barbalho")   

fab<-
  world %>%
  left_join(
    tarifas_trump %>%
      rename(tarifa = alleged_tariff_charged_to_us)
  )

map<-  
  world %>%
  left_join(
    tarifas_trump %>%
      rename(tarifa = alleged_tariff_charged_to_us)
  ) %>%
  ggplot() +
  geom_sf(aes(fill = tarifa, text = paste(name_long, ifelse(is.na(tarifa),"",": ") , ifelse(is.na(tarifa),"",tarifa) , ifelse(is.na(tarifa),"","%")))) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void() +
  theme(
    panel.background = element_rect(fill=cor_mar),
    text = element_text(colour = "black"),
    
  ) +
  coord_sf(xlim = c(-180,180), ylim=c(-50,82))+
  geom_text(data = dado_texto, aes(x=x, y=y, label= text) , color = "white") +
  geom_sf_text(aes(label= name_long))+
  labs(
    title = "Tarifas impostas aos EUA (conforme alegado pelo governo dos EUA)",
    caption = "Fonte: Guardian.",
    fill= "%"
  )
map


boxplot(tabela$saldo_comercial_rel_pais_USA)

quantile(tabela$saldo_comercial_rel_pais_USA, na.rm=TRUE)


tabela %>%
  mutate(resultado = ifelse(saldo_comercial_rel_pais_USA>0,"Superávit","Défict")) %>%
  ggplot() +
  geom_point(aes(x= alleged_tariff_charged_to_us , y= new_us_tariff, fill=resultado, size= abs(saldo_comercial_rel_pais_USA)  ),pch= 21, color= "#606060") +
  theme_light() +
  theme(
    panel.background = element_rect(fill= "black"),
    panel.grid = element_blank()
  ) +
  scale_fill_discrete_qualitative(palette= "Dark 2") +
  labs(
    fill = "Resultado",
    size = "%"
  )
  


tabela %>%
  mutate(resultado = case_when(
    saldo_comercial_rel_pais_USA>=0 ~"Superávit",
    saldo_comercial_rel_pais_USA<0 ~"Défict",
    is.na(saldo_comercial_rel_pais_USA) ~ "Sem informação")
  ) %>%
  summarise(n(), .by = resultado)


tabela %>%
  mutate(resultado = case_when(
    saldo_comercial_rel_pais_USA>=0 ~"Superávit",
    saldo_comercial_rel_pais_USA<0 ~"Défict",
    is.na(saldo_comercial_rel_pais_USA) ~ "Sem informação")
  ) %>%
  filter(resultado != "Sem informação") %>%
  ggplot() +
  geom_boxplot(aes(x= resultado , y= new_us_tariff),fill= NA, color= "white") +
  geom_text(data= tibble(x="Défict", y=50, label= "N Superávit = 48"), aes(x=x, y=y,label= label),color = "white")+
  geom_text(data= tibble(x="Défict", y=45, label= "N Défict = 76"), aes(x=x, y=y,label= label),color = "white")+
  
  theme_light() +
  theme(
    panel.background = element_rect(fill= "black"),
    panel.grid = element_blank()
  ) +
  scale_fill_discrete_qualitative(palette= "Dark 2") +
  labs(
    title = "Distribuição das novas tarifas por resultado do fluxo de comércio",
    x= "Resultado fluxo com EUA",
    y= "Nova Tarifa (%)"
  )
