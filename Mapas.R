
library(tidyverse)
library(readxl)
library(sf)


dados_fs <- read_excel("C:/Users/fernanda-romeiro/Downloads/dados_fs.xlsx") %>% 
  dplyr::mutate(
    faixa_ticket_2020 = dplyr::case_when(
      Ticmed_2020 <= 200 ~ "Até 200",
      Ticmed_2020 > 200 & Ticmed_2020 <= 500 ~ "200,01 a 500",
      Ticmed_2020 > 500 & Ticmed_2020 <= 1000 ~ "500,01 a 1000",
      Ticmed_2020 > 1000 ~ "Acima de 1000"
    ),
    faixa_ticket_2021 = dplyr::case_when(
      Ticmed_2021 <= 200 ~ "Até 200",
      Ticmed_2021 > 200 & Ticmed_2021 <= 500 ~ "200,01 a 500",
      Ticmed_2021 > 500 & Ticmed_2021 <= 1000 ~ "500,01 a 1000",
      Ticmed_2021 > 1000 ~ "Acima de 1000"),
    
    faixa_ticket_2022 = dplyr::case_when(
      Ticmed_2022 <= 200 ~ "Até 200",
      Ticmed_2022 > 200 & Ticmed_2022 <= 500 ~ "200,01 a 500",
      Ticmed_2022 > 500 & Ticmed_2022 <= 1000 ~ "500,01 a 1000",
      Ticmed_2022 > 1000 ~ "Acima de 1000"),
    
    faixa_ticket_2023 = dplyr::case_when(
      Ticmed_2023 <= 200 ~ "Até 200",
      Ticmed_2023 > 200 & Ticmed_2023 <= 500 ~ "200,01 a 500",
      Ticmed_2023 > 500 & Ticmed_2023 <= 1000 ~ "500,01 a 1000",
      Ticmed_2023 > 1000 ~ "Acima de 1000"),
    
    faixa_ticket_2024 = dplyr::case_when(
      Ticmed_2024 <= 200 ~ "Até 200",
      Ticmed_2024 > 200 & Ticmed_2024 <= 500 ~ "200,01 a 500",
      Ticmed_2024 > 500 & Ticmed_2024 <= 1000 ~ "500,01 a 1000",
      Ticmed_2024 > 1000 ~ "Acima de 1000")
  ) %>% 
  dplyr::mutate(faixa_ticket_2020 = factor(faixa_ticket_2020,
                                           levels = c("Até 200",
                                                      "200,01 a 500",
                                                      "500,01 a 1000",
                                                      "Acima de 1000")),
                
                faixa_ticket_2021 = factor(faixa_ticket_2021,
                                           levels = c("Até 200",
                                                      "200,01 a 500",
                                                      "500,01 a 1000",
                                                      "Acima de 1000")),
                
                faixa_ticket_2022 = factor(faixa_ticket_2022,
                                           levels = c("Até 200",
                                                      "200,01 a 500",
                                                      "500,01 a 1000",
                                                      "Acima de 1000")),
                
                faixa_ticket_2023 = factor(faixa_ticket_2023,
                                           levels = c("Até 200",
                                                      "200,01 a 500",
                                                      "500,01 a 1000",
                                                      "Acima de 1000")),
                
                faixa_ticket_2024 = factor(faixa_ticket_2024,
                                           levels = c("Até 200",
                                                      "200,01 a 500",
                                                      "500,01 a 1000",
                                                      "Acima de 1000"))
                
  )



MUNICIPIOS <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/MVIOL/MVIOL_project/Dados/ShapeRS/MUNICIPIOS.xlsx")

MUNICIPIOS_MP <- read_excel("Coisinhas/ShapeRS/MUNICIPIOS_MP.xlsx")


shapefile <- st_read("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/MVIOL/MVIOL_project/Dados/ShapeRS/municipiors4326.shp")

Ticket 2020


tableSHAPE_principal <- shapefile  %>% 
  dplyr::left_join(MUNICIPIOS_MP, by = c("CODIBGE" = "codigo_ibge" ))


#| warning: false

merge_Mapa <- shapefile %>% 
  #dplyr::mutate(CODIBGE = base::as.character(CODIBGE)) %>% 
  dplyr::left_join(dados_fs,
                   by = c("CODIBGE" = "cod"))


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticket_2020)) +
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c(
      "Até 200" = "#BFBFBF",
      "200,01 a 500" = "#FBBA00",
      "500,01 a 1000" = "#69A82F",
      "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Uruguaiana: 2020") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2021


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticket_2021)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Uruguaiana: 2021") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2022


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticket_2022)) +
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c(
      "Até 200" = "#BFBFBF",
      "200,01 a 500" = "#FBBA00",
      "500,01 a 1000" = "#69A82F",
      "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Uruguaiana: 2022") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2023


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticket_2023)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c(
      "Até 200" = "#BFBFBF",
      "200,01 a 500" = "#FBBA00",
      "500,01 a 1000" = "#69A82F",
      "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Uruguaiana: 2023") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2024


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticket_2024)) +
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c(
      "Até 200" = "#BFBFBF",
      "200,01 a 500" = "#FBBA00",
      "500,01 a 1000" = "#69A82F",
      "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Uruguaiana: 2024") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

DEMAIS

Ticket 2020


ticket_medio_completo <- read_excel("C:/Users/fernanda-romeiro/Downloads/ticket_medio_completo.xlsx") %>% 
  dplyr::mutate(
    faixa_ticketDEMAIS_2020 = dplyr::case_when(
      ticket_DEMAIS_2020 <= 200 ~ "Até 200",
      ticket_DEMAIS_2020 > 200 & ticket_DEMAIS_2020 <= 500 ~ "200,01 a 500",
      ticket_DEMAIS_2020 > 500 & ticket_DEMAIS_2020 <= 1000 ~ "500,01 a 1000",
      ticket_DEMAIS_2020 > 1000 ~ "Acima de 1000"
    ),
    faixa_ticketDEMAIS_2021 = dplyr::case_when(
      ticket_DEMAIS_2021 <= 200 ~ "Até 200",
      ticket_DEMAIS_2021 > 200 & ticket_DEMAIS_2021 <= 500 ~ "200,01 a 500",
      ticket_DEMAIS_2021 > 500 & ticket_DEMAIS_2021 <= 1000 ~ "500,01 a 1000",
      ticket_DEMAIS_2021 > 1000 ~ "Acima de 1000"),
    
    faixa_ticketDEMAIS_2022 = dplyr::case_when(
      ticket_DEMAIS_2022 <= 200 ~ "Até 200",
      ticket_DEMAIS_2022 > 200 & ticket_DEMAIS_2022 <= 500 ~ "200,01 a 500",
      ticket_DEMAIS_2022 > 500 & ticket_DEMAIS_2022 <= 1000 ~ "500,01 a 1000",
      ticket_DEMAIS_2022 > 1000 ~ "Acima de 1000"),
    
    faixa_ticketDEMAIS_2023 = dplyr::case_when(
      ticket_DEMAIS_2023 <= 200 ~ "Até 200",
      ticket_DEMAIS_2023 > 200 & ticket_DEMAIS_2023 <= 500 ~ "200,01 a 500",
      ticket_DEMAIS_2023 > 500 & ticket_DEMAIS_2023 <= 1000 ~ "500,01 a 1000",
      ticket_DEMAIS_2023 > 1000 ~ "Acima de 1000"),
    
    faixa_ticketDEMAIS_2024 = dplyr::case_when(
      ticket_DEMAIS_2024 <= 200 ~ "Até 200",
      ticket_DEMAIS_2024 > 200 & ticket_DEMAIS_2024 <= 500 ~ "200,01 a 500",
      ticket_DEMAIS_2024 > 500 & ticket_DEMAIS_2024 <= 1000 ~ "500,01 a 1000",
      ticket_DEMAIS_2024 > 1000 ~ "Acima de 1000")
  ) %>% 
  dplyr::mutate(across(faixa_ticketDEMAIS_2020:faixa_ticketDEMAIS_2024,
                       ~ base::factor(.x,
                                      levels = c("Até 200",
                                                 "200,01 a 500",
                                                 "500,01 a 1000",
                                                 "Acima de 1000"))))



tableSHAPE_principal <- shapefile  %>% 
  dplyr::left_join(MUNICIPIOS_MP, by = c("CODIBGE" = "codigo_ibge" ))


#| warning: false

merge_Mapa <- shapefile %>% 
  #dplyr::mutate(CODIBGE = base::as.character(CODIBGE)) %>% 
  dplyr::left_join(ticket_medio_completo,
                   by = c("CODIBGE" = "cod"))


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketDEMAIS_2020)) +
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c(
      "Até 200" = "#BFBFBF",
      "200,01 a 500" = "#FBBA00",
      "500,01 a 1000" = "#69A82F",
      "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Demais RS: 2020") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2021


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketDEMAIS_2021)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Demais RS: 2021") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2022


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketDEMAIS_2022)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Demais RS: 2022") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2023


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketDEMAIS_2023)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Demais RS: 2023") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2024


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketDEMAIS_2024)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Demais RS: 2024") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

TOTAL

Ticket 2020


ticket_medio_completo <- read_excel("C:/Users/fernanda-romeiro/Downloads/ticket_medio_completo.xlsx") %>% 
  dplyr::mutate(
    faixa_ticketTOTAL_2020 = dplyr::case_when(
      ticket_TOTAL_2020 <= 200 ~ "Até 200",
      ticket_TOTAL_2020 > 200 & ticket_TOTAL_2020 <= 500 ~ "200,01 a 500",
      ticket_TOTAL_2020 > 500 & ticket_TOTAL_2020 <= 1000 ~ "500,01 a 1000",
      ticket_TOTAL_2020 > 1000 ~ "Acima de 1000"
    ),
    faixa_ticketTOTAL_2021 = dplyr::case_when(
      ticket_TOTAL_2021 <= 200 ~ "Até 200",
      ticket_TOTAL_2021 > 200 & ticket_TOTAL_2021 <= 500 ~ "200,01 a 500",
      ticket_TOTAL_2021 > 500 & ticket_TOTAL_2021 <= 1000 ~ "500,01 a 1000",
      ticket_TOTAL_2021 > 1000 ~ "Acima de 1000"),
    
    faixa_ticketTOTAL_2022 = dplyr::case_when(
      ticket_TOTAL_2022 <= 200 ~ "Até 200",
      ticket_TOTAL_2022 > 200 & ticket_TOTAL_2022 <= 500 ~ "200,01 a 500",
      ticket_TOTAL_2022 > 500 & ticket_TOTAL_2022 <= 1000 ~ "500,01 a 1000",
      ticket_TOTAL_2022 > 1000 ~ "Acima de 1000"),
    
    faixa_ticketTOTAL_2023 = dplyr::case_when(
      ticket_TOTAL_2023 <= 200 ~ "Até 200",
      ticket_TOTAL_2023 > 200 & ticket_TOTAL_2023 <= 500 ~ "200,01 a 500",
      ticket_TOTAL_2023 > 500 & ticket_TOTAL_2023 <= 1000 ~ "500,01 a 1000",
      ticket_TOTAL_2023 > 1000 ~ "Acima de 1000"),
    
    faixa_ticketTOTAL_2024 = dplyr::case_when(
      ticket_TOTAL_2024 <= 200 ~ "Até 200",
      ticket_TOTAL_2024 > 200 & ticket_TOTAL_2024 <= 500 ~ "200,01 a 500",
      ticket_TOTAL_2024 > 500 & ticket_TOTAL_2024 <= 1000 ~ "500,01 a 1000",
      ticket_TOTAL_2024 > 1000 ~ "Acima de 1000")
  ) %>% 
  dplyr::mutate(across(faixa_ticketTOTAL_2020:faixa_ticketTOTAL_2024,
                       ~ base::factor(.x,
                                      levels = c("Até 200",
                                                 "200,01 a 500",
                                                 "500,01 a 1000",
                                                 "Acima de 1000"))))



#| warning: false

merge_Mapa <- shapefile %>% 
  #dplyr::mutate(CODIBGE = base::as.character(CODIBGE)) %>% 
  dplyr::left_join(ticket_medio_completo,
                   by = c("CODIBGE" = "cod"))


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketTOTAL_2020)) +
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c(
      "Até 200" = "#BFBFBF",
      "200,01 a 500" = "#FBBA00",
      "500,01 a 1000" = "#69A82F",
      "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Total: 2020") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2021


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketTOTAL_2021)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Total: 2021") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2022


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketTOTAL_2022)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Total: 2022") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2023


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketTOTAL_2023)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Total: 2023") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

Ticket 2024


library(ggplot2)
ggplot(merge_Mapa) +
  geom_sf(aes(fill = faixa_ticketTOTAL_2024)) +
  
  scale_fill_manual(
    name = "Ticket Médio (R$)",
    values = c( "Até 200" = "#BFBFBF",
                "200,01 a 500" = "#FBBA00",
                "500,01 a 1000" = "#69A82F",
                "Acima de 1000" = "#E4003A"
    )
  ) +
  coord_sf() +
  labs(title = "Total: 2024") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,     # centraliza
      size = 14,
      face = "bold"
    )
  )

