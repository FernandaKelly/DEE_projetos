
library(tidyverse)
library(readxl)
library(sf)


dados_agro2022 <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/DEE/DEE_projetos/Dados/mapa_part_agro_2022.xlsx") %>% 
  dplyr::mutate(
    faixa_123 = dplyr::case_when(
      part_agro <= 12.042 ~ "1° quartil",
      part_agro > 12.042 & part_agro < 38.008 ~ "2° quartil",
      part_agro >= 38.008 ~ "3° quartil"),
    faixa_123_factor = factor(faixa_123,
                               levels = c("1° quartil",
                                          "2° quartil",
                                          "3° quartil")),
    
    faixa_4 = dplyr::case_when(
      part_agro <= 10.00 ~ "0 a 10 (n = 104)",
      part_agro >= 10.003 & part_agro <= 25 ~ "10.1 a 25 (n = 135)",
      part_agro >= 25.03 & part_agro <= 40.00 ~ "25.1 a 40 (n = 147)",
      part_agro >= 40.07 ~ "Acima de 40.1 (n = 111)"),
    faixa_4_factor = factor(faixa_4,
                              levels = c("0 a 10 (n = 104)",
                                         "10.1 a 25 (n = 135)",
                                         "25.1 a 40 (n = 147)",
                                         "Acima de 40.1 (n = 111)")))

MUNICIPIOS <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/DEE/DEE_projetos/Dados/ShapeRS/MUNICIPIOS.xlsx")
MUNICIPIOS_MP <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/DEE/DEE_projetos/Dados/ShapeRS/MUNICIPIOS_MP.xlsx")
shapefile <- st_read("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/DEE/DEE_projetos/Dados/ShapeRS/municipiors4326.shp")

tableSHAPE_principal <- shapefile  %>% 
  dplyr::left_join(MUNICIPIOS_MP, by = c("CODIBGE" = "codigo_ibge" ))


merge_Mapa <- shapefile %>% 
  #dplyr::mutate(CODIBGE = base::as.character(CODIBGE)) %>% 
  dplyr::left_join(dados_agro2022,
                   by = c("CODIBGE" = "cod"))

###################

library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = part_agro)) +
  scale_fill_gradient(
    low = "mistyrose",
    high = "deeppink3",
    name = "Agro"
  ) +
  coord_sf() +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey80", linetype = "dashed")
  )

##########################

library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_4_factor)) +
  scale_fill_manual(
    name = "Porcentagem (%)",
    values = c(
      "0 a 10 (n = 104)"        = "#BFBFBF", #E4003A
      "10.1 a 25 (n = 135)"     = "#FBBA00", #BFBFBF
      "25.1 a 40 (n = 147)"     = "#69A82F", #FBBA00
      "Acima de 40.1 (n = 111)" = "#E4003A" #69A82F
    )
  ) +
  coord_sf() +
  #labs(title = "Agro: 2022") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

