library(dplyr)
library(purrr)
library(tidyverse)
library(readxl)


dados_UOM <- read_csv2("C:\\Users\\sabrina.franca\\Downloads\\UOM_03_2026.csv")
nrow(dados_UOM )

mapa_regiao <- tibble::tribble(
  ~SG_UF, ~regiao,
  "AC", "Norte", "AP", "Norte", "AM", "Norte", "PA", "Norte", "RO", "Norte", "RR", "Norte", "TO", "Norte",
  "AL", "Nordeste", "BA", "Nordeste", "CE", "Nordeste", "MA", "Nordeste", "PB", "Nordeste", 
  "PE", "Nordeste", "PI", "Nordeste", "RN", "Nordeste", "SE", "Nordeste",
  "DF", "Centro-Oeste", "GO", "Centro-Oeste", "MT", "Centro-Oeste", "MS", "Centro-Oeste",
  "ES", "Sudeste", "MG", "Sudeste", "RJ", "Sudeste", "SP", "Sudeste",
  "PR", "Sul", "RS", "Sul", "SC", "Sul"
)

dados_UOM <- dados_UOM %>%
  left_join(mapa_regiao, by = "SG_UF")

dados_UOM_UF <- dados_UOM %>% group_by(SG_UF) %>%  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% arrange(desc(prop))

dados_UOM_UF  <- dados_UOM_UF  %>%
  left_join(mapa_regiao, by = "SG_UF")

sum(dados_UOM$UOM_02_VAL_REP)



UOM_UF <- ggplot(dados_UOM_UF, aes(x = reorder(SG_UF, -n), y = n)) +
  geom_col(fill = "#1A3D64", width = 0.6, alpha = 0.7) +
  geom_text(
    aes(y = n, label = n),
    vjust = -0.5, size = 5, color = "black"
  ) +
  
  labs(
    x = "Unidade Federativa",
    y = ""
  )+
  theme_minimal(base_size = 14)+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(face = "bold",  size = 12),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


ggsave("C:\\Users\\sabrina.franca\\OneDrive - Ministério da Saúde\\Documentos\\saude bucal\\output\\UOM_UF.png", UOM_UF,
       bg = "transparent",
       width = 14,
       height = 5,
       dpi = 300)



dados_UOM %>% group_by(regiao) %>%  summarise(n=n()) %>% 
  mutate(prop = n/sum(n) * 100) %>% arrange(desc(prop))
