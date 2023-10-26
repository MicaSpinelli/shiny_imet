# Tabla resumen de los últimos datos

library(tidyverse)
library(gt)
library(glue)
library(foreign)
library(herramientas)
library(webshot2)
library(shiny)
library(shinydashboard)
library(highcharter)
library(lubridate)
library(comunicacion)

source("custom_box.R")

# PALETAS Y FUENTES ------------------------------------------------------------
# Paleta colores Presidencia
cols_arg <- "#37BBED" # Celeste Institucional

# Secundarios
cols_arg2 <- c("#EE3D8F", # "ROJO"
               "#50B8B1", # "VERDE"
               "#F7941E","#FFD100","#D7DF23", "#9283BE")

familia_fuente <- "Encode Sans"

turismo_internacional_vias <- read_file_srv("/srv/DataDNMYE/imet/serie_ti_vias.csv")
# NOTA: los datos de esta base corresponden a turistas nada más (no inlcuye excursionistas)

data_grafico_ti <- turismo_internacional_vias %>%
  filter(year >=2019 ) %>% 
  pivot_longer(cols = c(3:length(.)), names_to = "indicador", values_to = "n")  %>%
  filter(str_detect(string = indicador, pattern = "total", negate = T)) %>%
  mutate(direccion = case_when(str_detect(indicador, "emisivo") ~ "emisivo", T ~ "receptivo"), 
         month = str_pad(month, 2, "left", 0), 
         period = ymd(as.character(glue::glue("{year}/{month}/01")))) %>% 
  select(- c(year, month, indicador)) %>% 
  group_by(direccion, period) %>% 
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = "direccion", 
              values_from = "n") 

# GRAFICO
grafico_ti <- ggplot(data_grafico_ti)+
  geom_line(aes(period, receptivo),
            size = 1, alpha = .5) 


 
  