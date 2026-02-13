library(tidyverse)
library(magrittr)
library(readxl)

###########################
# DADOS
##########################

rodovias_rs <- read_excel("Dados/rodovias_rs.xlsx")


###################################################
ranking_estado <- dados %>%
  group_by(Estado) %>%
  summarise(
    Nota_CLP = weighted.mean(Nota, Extensao_km),
    Extensao_Total = sum(Extensao_km)
  ) %>%
  arrange(desc(Nota_CLP))

ranking_estado
###################################################



###################################################
ranking_final <- ranking_estado %>%
  mutate(
    min_val = min(Nota_CLP, na.rm = TRUE),
    max_val = max(Nota_CLP, na.rm = TRUE),
    Qualidade_0_100 = ((Nota_CLP - min_val) / 
                         (max_val - min_val)) * 100
  ) %>%
  select(-min_val, -max_val) %>%
  arrange(desc(Qualidade_0_100))

ranking_final
###################################################










