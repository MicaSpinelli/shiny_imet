####################GRAFICOS####################################

function(input, output) {
  
########TURISMO INTERNACIONAL#######################  
  #Creo y renderizo plotly a partir de ggplot
  output$grafico_ti_receptivo <- renderPlotly({ #idPlot
    
    #Armo ggplot y lo guardo en objeto
    grafico_ti_receptivo <- ggplot(data_grafico_ti)+
      geom_line(aes(period, receptivo, group = 1,
                 text = paste('Fecha:', format(period,"%b%y"), #argumento para etiqueta interactiva
                              '<br>Viajes de turistas receptivos:',lbl_int(receptivo))),
                size = 1, alpha = .5) +
      theme_void()
    
    #Paso ggplot a plotly y borro ejes (repetir para todos)
    ggplotly(grafico_ti_receptivo, 
             tooltip = "text") %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F)
    
  })
  
#################EVYTH######################
  
  output$grafico_evyth <- renderPlotly({
    
    grafico_evyth <- ggplot(data_grafico_evyth)+
      geom_col(aes(date, turistas,
                   text = paste('Fecha:', fecha, #argumento para etiqueta interactiva
                                  '<br>Turistas internos:',format(turistas, decimal.mark = ",")))) +
      theme_void()
    
    ggplotly(grafico_evyth, 
             tooltip = "text") %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F)
  })
  
##############EOH##############
  output$grafico_eoh_viajeros <- renderPlotly({
    
    grafico_eoh_viajeros <- ggplot(data_grafico_eoh)+
      geom_area(aes(date, viajeros_tot), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.5, 
                aes(date, viajeros_tot, group = 1,
                   text = paste('Fecha:', format(date,"%b%y"), #argumento para etiqueta interactiva
                                '<br>Viajeros hospedados:',format(viajeros_tot, decimal.mark = ",")))) +
      theme_void()
    
    ggplotly(grafico_eoh_viajeros, 
             tooltip = "text") %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F)
  })
  
  output$grafico_eoh_pernoc <- renderPlotly({
    
    grafico_eoh_pernoc <- ggplot(data_grafico_eoh)+
      geom_line(aes(date, pernoc_tot, group=1,
                   text = paste('Fecha:', format(date,"%b%y"), #argumento para etiqueta interactiva
                                '<br>Pernoctaciones:',format(pernoc_tot, decimal.mark = ",")))) +
      theme_void()
    
    ggplotly(grafico_eoh_pernoc, 
             tooltip = "text") %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F)
  })
###############EMPLEO###########################
  output$grafico_empleo_hyr <- renderPlotly({
    
    grafico_empleo_hyr <- ggplot(data_grafico_empleo)+
      geom_line(aes(date, empleo_hyr_ce, group=1,
                    text = paste('Fecha:', format(date,"%b%y"), #argumento para etiqueta interactiva
                                 '<br>Trabajadores:',format(empleo_hyr_ce, decimal.mark = ",")))) +
      theme_void()
    
    ggplotly(grafico_empleo_hyr, 
             tooltip = "text") %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F)
  })
  
###############EMAE#############################
  output$grafico_emae_hyr <- renderPlotly({
    
    grafico_emae_hyr <- ggplot(data_grafico_emae)+
      geom_col(aes(date, var_ia_emae_hyr,
                    text = paste('Fecha:', format(date,"%b%y"), #argumento para etiqueta interactiva
                                 '<br>Var i.a.:',format(lbl_percent(var_ia_emae_hyr), decimal.mark = ",")))) +
      theme_void()
    
    ggplotly(grafico_emae_hyr, 
             tooltip = "text") %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F)
  })
  
###############CONECTIVIDAD INTERNACIONAL#############################
  output$grafico_conectividad_int <- renderPlotly({
    
    grafico_conectividad_int <- ggplot(data_grafico_conectividad_internacional)+
      geom_line(aes(date, pax, group=1,
                   text = paste('Fecha:', format(date,"%b%y"), #argumento para etiqueta interactiva
                                '<br>Pasajeros:',format(pax, decimal.mark = ",")))) +
      theme_void()
    
    ggplotly(grafico_conectividad_int, 
             tooltip = "text") %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F)
  })
  
###############CONECTIVIDAD CABOTAJE#############################
  output$grafico_conectividad_cab <- renderPlotly({
    
    grafico_conectividad_cab <- ggplot(data_grafico_conectividad_cabotaje)+
      geom_line(aes(date, pax, group=1,
                    text = paste('Fecha:', format(date,"%b%y"), #argumento para etiqueta interactiva
                                 '<br>Pasajeros:',format(pax, decimal.mark = ",")))) +
      theme_void()
    
    ggplotly(grafico_conectividad_cab, 
             tooltip = "text") %>% 
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F)
  })
###########CREO LOS VALUE BOX###################  
############TURISMO INTERNACIONAL###############  
    
  #Renderizo valuebox usando función personalizada
  output$box_ti_receptivo <- renderValueBox({ #id a usar en la UI
    
    
    mes <- data_grafico_ti %>% 
      tail(1) %>% 
      pull(period) %>% 
      months()
    
    anio <- data_grafico_ti %>% 
      tail(1) %>% 
      pull(period) %>% 
      year()
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_ti <- data_grafico_ti %>% 
      tail(1) %>% 
      pull(receptivo)
    
    
    interanual_dato_ti <- data_grafico_ti %>% 
      filter(row_number() == nrow(data_grafico_ti)-12) %>% 
      pull(receptivo)
    
    
    var_ia_ti <- lbl_percent((ultimo_dato_ti/interanual_dato_ti-1)) 
    
    #uso función personalizada
    valueBoxSpark(
      value = lbl_int(ultimo_dato_ti), #indicador
      subtitle = ifelse(var_ia_ti>0, paste0("+",var_ia_ti," var i.a"),paste0(var_ia_ti," var i.a")), #texto de abajo
      description = "viajes de turistas receptivos", #texto debajo del indicador
      minititle = paste0("Turismo internacional ", mes," ", anio), #texto de arriba
      icon = icon("plane-arrival"), #icono de fontawesome
      infoID = "tur_internacional", #id para icono de info
      idPlot = "grafico_ti_receptivo", #id del renderPlotly
      color = "aqua" #color
    )
    
  })

############EVYTH###############    
  output$box_evyth_viajeros <- renderValueBox({ #id a usar en la UI
    
    #mes del dato a mostrar
    trim_evyth <- data_grafico_evyth%>% 
      tail(1) %>% 
      pull(fecha)
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_evyth <- data_grafico_evyth%>% 
      tail(1) %>% 
      pull(turistas) %>% 
      format(decimal.mark = ",")
    
    
    var_ia_evyth <- data_grafico_evyth%>% 
      tail(1) %>% 
      pull(variacion_tur)
      
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_evyth, " millones"), #indicador
      subtitle = ifelse(var_ia_evyth>0, paste0("+",lbl_percent(var_ia_evyth)," var i.a"),paste0(lbl_percent(var_ia_evyth)," var i.a")), #texto de abajo
      description = "cantidad de turistas internos", #texto debajo del indicador
      minititle = paste0("Turismo interno ", trim_evyth), #texto de arriba
      icon = icon("car"), #icono de fontawesome
      infoID = "tur_interno", #id para icono de info
      idPlot = "grafico_evyth", #id del renderPlotly
      color = "green" #color
    )
    
  })

############EOH VIAJEROS###############  
  output$box_eoh_viajeros <- renderValueBox({ #id a usar en la UI
    
    mes_eoh <- data_grafico_eoh %>% 
      tail(1) %>% 
      pull(date) %>% 
      months()
    
    anio_eoh <- data_grafico_eoh %>% 
      tail(1) %>% 
      pull(date) %>% 
      year()
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_eoh_viajeros <- data_grafico_eoh%>% 
      tail(1) %>% 
      pull(viajeros_tot) %>% 
      format(decimal.mark = ",")
    
    
    var_ia_eoh_viajeros <- data_grafico_eoh%>% 
      tail(1) %>% 
      pull(var_ia_viajeros)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_eoh_viajeros, " millones"), #indicador
      subtitle = ifelse(var_ia_eoh_viajeros>0, paste0("+",lbl_percent(var_ia_eoh_viajeros)," var i.a"),paste0(lbl_percent(var_ia_eoh_viajeros)," var i.a")), #texto de abajo
      description = "cantidad de viajeros hospedados", #texto debajo del indicador
      minititle = paste0("Ocupación hotelera ", mes_eoh, " ", anio_eoh), #texto de arriba
      icon = icon("hotel"), #icono de fontawesome
      infoID = "tur_eoh", #id para icono de info
      idPlot = "grafico_eoh_viajeros", #id del renderPlotly
      color = "light-blue" #color
    )
    
  })
  
  
############EOH PERNOCTACIONES############### 
  
  output$box_eoh_pernoctes <- renderValueBox({ #id a usar en la UI
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_eoh_pernoctes <- data_grafico_eoh%>% 
      tail(1) %>% 
      pull(pernoc_tot) %>% 
      format(decimal.mark = ",")
    
    
    var_ia_eoh_pernoctes <- data_grafico_eoh%>% 
      tail(1) %>% 
      pull(var_ia_pernoc)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_eoh_pernoctes, " millones de noches"), #indicador
      subtitle = ifelse(var_ia_eoh_pernoctes>0, paste0("+",lbl_percent(var_ia_eoh_pernoctes)," var i.a"),paste0(lbl_percent(var_ia_eoh_pernoctes)," var i.a")), #texto de abajo
      description = "cantidad de pernoctaciones", #texto debajo del indicador
      minititle = paste0("Ocupación hotelera ", mes_eoh, " ", anio_eoh), #texto de arriba
      icon = icon("bed"), #icono de fontawesome
      infoID = "tur_eoh_p", #id para icono de info
      idPlot = "grafico_eoh_pernoc", #id del renderPlotly
      color = "orange" #color
    )
    
  })
  
############EMPLEO###############  
  output$box_empleo_hyr <- renderValueBox({ #id a usar en la UI
    
    mes_empleo <- data_grafico_empleo %>% 
      tail(1) %>% 
      pull(date) %>% 
      months()
    
    anio_empleo <- data_grafico_empleo %>% 
      tail(1) %>% 
      pull(date) %>% 
      year()
    
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_empleo_hyr_ce <- data_grafico_empleo%>% 
      tail(1) %>% 
      pull(empleo_hyr_ce) %>% 
      round(1) %>% 
      format(decimal.mark=",")
    
    
    var_m_empleo_hyr <- data_grafico_empleo%>% 
      tail(1) %>% 
      pull(var_mensual)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_empleo_hyr_ce, " mil"), #indicador
      subtitle = ifelse(var_m_empleo_hyr>0, paste0("+",lbl_percent(var_m_empleo_hyr)," var mensual"),paste0(lbl_percent(var_m_empleo_hyr)," var mensual")), #texto de abajo
      description = "trabajadores en el sector “Hoteles y Restaurantes”", #texto debajo del indicador
      minititle = paste0("Empleo privado registrado ", mes_empleo, " ", anio_empleo), #texto de arriba
      icon = icon("briefcase"), #icono de fontawesome
      infoID = "empleo_hyr", #id para icono de info
      idPlot = "grafico_empleo_hyr", #id del renderPlotly
      color = "red" #color
    )
    
  })
  
  output$box_empleo_hyr <- renderValueBox({ #id a usar en la UI
    
    mes_empleo <- data_grafico_empleo %>% 
      tail(1) %>% 
      pull(date) %>% 
      months()
    
    anio_empleo <- data_grafico_empleo %>% 
      tail(1) %>% 
      pull(date) %>% 
      year()
    
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_empleo_hyr_ce <- data_grafico_empleo%>% 
      tail(1) %>% 
      pull(empleo_hyr_ce) %>% 
      round(1) %>% 
      format(decimal.mark=",")
    
    
    var_m_empleo_hyr <- data_grafico_empleo%>% 
      tail(1) %>% 
      pull(var_mensual)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_empleo_hyr_ce, " mil"), #indicador
      subtitle = ifelse(var_m_empleo_hyr>0, paste0("+",lbl_percent(var_m_empleo_hyr)," var mensual"),paste0(lbl_percent(var_m_empleo_hyr)," var mensual")), #texto de abajo
      description = "trabajadores en el sector “Hoteles y Restaurantes”", #texto debajo del indicador
      minititle = paste0("Empleo privado registrado ", mes_empleo, " ", anio_empleo), #texto de arriba
      icon = icon("briefcase"), #icono de fontawesome
      infoID = "empleo_hyr", #id para icono de info
      idPlot = "grafico_empleo_hyr", #id del renderPlotly
      color = "red" #color
    )
    
  })
############EMAE###############
  output$box_emae_hyr <- renderValueBox({ #id a usar en la UI
    
    mes_emae <- data_grafico_emae %>% 
      tail(1) %>% 
      pull(date) %>% 
      months()
    
    anio_emae <- data_grafico_emae %>% 
      tail(1) %>% 
      pull(date) %>% 
      year()
    
    
    #calculo de ultimo dato y var i.a.
    var_ia_emae_hyr <- data_grafico_emae%>% 
      tail(1) %>% 
      pull(var_ia_emae_hyr)
    
    
    
    valueBoxSpark(
      value = ifelse (var_ia_emae_hyr>0, paste0("+",lbl_percent(var_ia_emae_hyr)," var i.a."),paste0(lbl_percent(var_ia_emae_hyr)," var i.a.")), #indicador
      subtitle =  "NULL",
      description = "actividad económica en “Hoteles y Restaurantes”", #texto debajo del indicador
      minititle = paste0("Estimador mensual de actividad económica (EMAE) ", mes_emae, " ", anio_emae), #texto de arriba
      icon = icon("coins"), #icono de fontawesome
      infoID = "emae_hyr", #id para icono de info
      idPlot = "grafico_emae_hyr", #id del renderPlotly
      color = "yellow" #color
    )
    
  })
  
###############CONECTIVIDAD INTERNACIONAL#####################
  output$box_conectividad_int <- renderValueBox({ #id a usar en la UI
    
    mes_conectividad <- data_grafico_conectividad_internacional %>% 
      tail(1) %>% 
      pull(date) %>% 
      months()
    
    anio_conectividad <- data_grafico_conectividad_internacional %>% 
      tail(1) %>% 
      pull(date) %>% 
      year()
    
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_pax_int <- data_grafico_conectividad_internacional%>% 
      tail(1) %>% 
      pull(pax) %>% 
      format(decimal.mark=",")
    
    var_ia_conectividad_int <- data_grafico_conectividad_internacional %>% 
      tail(1) %>% 
      pull(var_ia_pax_int)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_pax_int, " miles"), #indicador
      subtitle =  ifelse(var_ia_conectividad_int>0, paste0("+",lbl_percent(var_ia_conectividad_int)," var i.a."),paste0(lbl_percent(var_ia_conectividad_int)," var i.a.")), #texto de abajo
      description = "pasajeros transportados en vuelos internacionales", #texto debajo del indicador
      minititle = paste0("Conectividad aérea internacional ", mes_conectividad, " ", anio_conectividad), #texto de arriba
      icon = icon("plane-up"), #icono de fontawesome
      infoID = "conectividad_i", #id para icono de info
      idPlot = "grafico_conectividad_int", #id del renderPlotly
      color = "yellow" #color
    )
    
  })
  
  ###############CONECTIVIDAD CABOTAJE#####################
  output$box_conectividad_cab <- renderValueBox({ #id a usar en la UI
    
    mes_conectividad <- data_grafico_conectividad_cabotaje %>% 
      tail(1) %>% 
      pull(date) %>% 
      months()
    
    anio_conectividad <- data_grafico_conectividad_cabotaje %>% 
      tail(1) %>% 
      pull(date) %>% 
      year()
    
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_pax_cab <- data_grafico_conectividad_cabotaje%>% 
      tail(1) %>% 
      pull(pax) %>% 
      format(decimal.mark=",")
    
    var_ia_conectividad_cab <- data_grafico_conectividad_cabotaje %>% 
      tail(1) %>% 
      pull(var_ia_pax_cab)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_pax_cab, " millones"), #indicador
      subtitle =  ifelse(var_ia_conectividad_cab>0, paste0("+",lbl_percent(var_ia_conectividad_cab)," var i.a."),paste0(lbl_percent(var_ia_conectividad_cab)," var i.a.")), #texto de abajo
      description = "pasajeros transportados en vuelos de cabotaje", #texto debajo del indicador
      minititle = paste0("Conectividad aérea de cabotaje ", mes_conectividad, " ", anio_conectividad), #texto de arriba
      icon = icon("plane-up"), #icono de fontawesome
      infoID = "conectividad_c", #id para icono de info
      idPlot = "grafico_conectividad_cab", #id del renderPlotly
      color = "yellow" #color
    )
    
  })
  
#Observe para mostrar popup al clickear el icono de info
  
  observeEvent(input$tur_internacional,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Turismo internacional", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/turismo_internacional",
                                                                              "Tablero interactivo"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$tur_interno,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Turismo interno", #titulo del popup
                   text = HTML(paste("Para más información visitá los",tags$a( href="https://www.yvera.tur.ar/sinta/informe/info/encuesta-de-viajes-y-turismo-de-los-hogares-evyth",
                                                                              "Informes técnicos"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$tur_eoh,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Ocupación hotelera", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/eoh.html",
                                                                               "Tablero"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$tur_eoh_p,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Ocupación hotelera", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/eoh.html",
                                                                              "Tablero"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$empleo_hyr,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Empleo privado registrado", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://biblioteca.yvera.tur.ar/coyuntura.html",
                                                                              "Informe mensual de estadísticas de turismo"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$emae_hyr,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Estimador mensual de actividad económica (EMAE)", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://biblioteca.yvera.tur.ar/coyuntura.html",
                                                                              "Informe mensual de estadísticas de turismo"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$tur_omt,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Turismo en el mundo", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://biblioteca.yvera.tur.ar/coyuntura.html",
                                                                              "Informe mensual de estadísticas de turismo"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$conectividad_c,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Conectividad", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/conectividad/",
                                                                              "Tablero interactivo"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$conectividad_i,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Conectividad", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/conectividad/",
                                                                              "Tablero interactivo"))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
}  
  

