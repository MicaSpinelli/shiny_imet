# Tabla resumen de los últimos datos

#Colores para value box https://rdrr.io/cran/shinydashboard/man/validColors.html
#Iconos para value box https://fontawesome.com/search?q=hotel&o=r&m=free&s=solid

library(googlesheets4)
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
library(plotly) #para graficos interactivos
library(shinyWidgets) #para los popup
library(htmlwidgets) #para cambiar cursor

#seteo lenguaje en español
Sys.setlocale(locale = "es_AR.UTF-8")

googlesheets4::gs4_deauth()


#Source para cargar la función 
source("custom_box.R")

#Cargo bases de datos

turismo_internacional_vias <- read_file_srv("/srv/DataDNMYE/imet/serie_ti_vias.csv") #los datos de esta base corresponden a turistas nada más (no inlcuye excursionistas)

turismo_interno <- read_file_srv("/srv/DataDNMYE/imet/serie_evyth.csv")

eoh <- read_file_srv("/srv/DataDNMYE/imet/eoh_imet.csv") %>% 
  mutate(year= as.numeric(str_sub(indice_tiempo, end = 4L)),
         month= as.numeric(str_sub(indice_tiempo, start = 6L, end = 7L)))

emae <- read_file_srv("/srv/DataDNMYE/economia2/emae_imet.csv") 


empleo <- read_file_srv("/srv/DataDNMYE/economia2/empleo_imet.csv") %>% 
  filter(sector=="hoteles_y_restaurantes") 
  
tur_mundo <-  read_file_srv("/srv/DataDNMYE/economia2/tur_mundo_omt.xlsx")
                
  
conectividad_internacional <- read_file_srv("/srv/DataDNMYE/imet/internacional_empresas_completa.csv") 

conectividad_cabotaje <- read_file_srv("/srv/DataDNMYE/imet/cabotaje_empresas_completa.csv") 
  




#Armo bases para graficos
#Turismo internacional

data_grafico_ti <- turismo_internacional_vias %>%
  #filter(year >=2018) %>% 
  pivot_longer(cols = c(3:length(.)), names_to = "indicador", values_to = "n")  %>%
  filter(str_detect(string = indicador, pattern = "total", negate = T)) %>%
  mutate(direccion = case_when(str_detect(indicador, "emisivo") ~ "emisivo", T ~ "receptivo"), 
         month = str_pad(month, 2, "left", 0), 
         period = ymd(as.character(glue::glue("{year}/{month}/01")))) %>% 
  select(- c(year, month, indicador)) %>% 
  group_by(direccion, period) %>% 
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = "direccion", 
              values_from = "n") %>% 
  mutate(month=as.numeric(substr(period, 6,7)))


data_grafico_evyth <- turismo_interno %>%
  filter(row_number() !=n()) %>% #para cuando no queremos que salga la ultima fila (porque es otro trimestre)
  select(anio, trimestre,tur) %>% 
  mutate(date= lubridate::yq(paste(anio, trimestre, "-")),
        turistas = ifelse(is.na(tur), 0, tur),
         variacion_tur = round(turistas/lag(turistas, n = 4)-1,2), 
         turistas = round(turistas/1000000,1),
         trim= paste0(trimestre,"° trim "))


data_grafico_eoh <- eoh%>% 
  mutate(date= lubridate::ym(paste(year, month, "-")),
         year= as.numeric(str_sub(indice_tiempo, end = 4L)),
         month= as.numeric(str_sub(indice_tiempo, start = 6L, end = 7L)),
         viajeros_total= (viajeros_res+viajeros_nores)/1000000,
         pernoc_total= (pernoc_res+pernoc_nores)/1000000,
         viajeros_tot= round(viajeros_total,1),
         pernoc_tot= round(pernoc_total,1),
         var_ia_viajeros=round(viajeros_total/lag(viajeros_total, n=12)-1,2),
         var_ia_pernoc=round(pernoc_total/lag(pernoc_total, n=12)-1,2)) 
  #filter(year>=2018)

data_grafico_empleo <- empleo %>% 
  select(-sector) %>% 
  rename(empleo_hyr_ce=trabajadores_registrados,
         empleo_hyr_se=trabajadores_registrados_desest) %>% 
  mutate(year=as.numeric(substr(fecha, 1,4)),
         month=as.numeric(substr(fecha, 6,7)),
         var_ia=round(empleo_hyr_ce/lag(empleo_hyr_ce, n=12)-1,3))
  
data_grafico_emae <- emae %>% 
  select(year, month, emae_hyr_ce, emae_total_ce) %>% 
  mutate(date= lubridate::ym(paste(year, month, "-")),
         var_ia_emae_hyr=round(emae_hyr_ce/lag(emae_hyr_ce, n=12)-1,2),
         var_19_emae_hyr=round(emae_hyr_ce/lag(emae_hyr_ce, n=48)-1,2),
         var_ia_total=round(emae_total_ce/lag(emae_total_ce, n=12)-1,3))

data_grafico_conectividad_internacional  <-  conectividad_internacional %>% 
  rename(anio = anio_local, mes = mes_local,empresa = empresa_agrup_def) %>% 
  filter(regular_noregular == 1) %>%
  select(anio, mes, pax) %>% 
  group_by(anio, mes) %>% 
  summarise(pax=sum(pax)) %>% 
  ungroup() %>% 
  mutate(date= lubridate::ym(paste(anio, mes, "-")),
         var_ia_pax_int=round(pax/lag(pax, n=12)-1,3),
         pax_miles=round(pax/1000,1))
  

data_grafico_conectividad_cabotaje  <-  conectividad_cabotaje %>% 
  rename(anio = anio_local,
         mes = mes_local,
         empresa = empresa_agrup_def ) %>%  
  select(anio, mes, pax) %>% 
  group_by(anio, mes) %>% 
  summarise(pax=sum(pax)) %>% 
  ungroup() %>% 
  mutate(date= lubridate::ym(paste(anio, mes, "-")),
         var_ia_pax_cab=round(pax/lag(pax, n=12)-1,3),
         pax_mill=round(pax/1000000,1))
  
 data_grafico_mundo <- tur_mundo %>%
   mutate_at(.vars = vars(everything()),
             .funs = ~ as.numeric(.)) %>% 
   mutate(date= lubridate::ym(paste(anio, mes, "-")),
          var_ia=round(tur_mundo/lag(tur_mundo, n=12)-1,2))

 
 #Armo ultimos meses
 
 ultimo_ti <- turismo_internacional_vias %>% 
   tail(1) %>% 
   pull(month)
 
 ultimo_eoh <- eoh %>% 
   tail(1) %>% 
   pull(month)
 
 ultimo_conectividad <- conectividad_internacional %>% 
   tail(1) %>% 
   pull(mes_local)
 
 ultimo_evyth <- turismo_interno %>% 
   filter(row_number() !=n()) %>% #para cuando no queremos que salga la ultima fila (porque es otro trimestre)
   tail(1) %>%
   pull(trimestre)
 
 ultimo_tur_mundo <- tur_mundo %>% 
   tail(1) %>%
   pull(mes)
 
 ultimo_emae <- emae %>% 
   tail(1) %>%
   pull(month)
 
 ultimo_empleo <- data_grafico_empleo %>% 
   tail(1) %>%
   pull(month) %>% 
   as.numeric()   
  