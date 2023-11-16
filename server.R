####################GRAFICOS####################################

function(input, output) {
  sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                 title = "Bienvenida", #titulo del popup 
                 text = HTML(paste ("El tablero <b>ÚLTIMOS DATOS DEL TURISMO EN ARGENTINA</b> presenta los principales indicadores publicados en el", tags$a( href="https://www.yvera.tur.ar/sinta/",
                                                                          "SINTA",target = '_blank'),"y, además, indicadores de otros organismos, que dan a conocer los resultados más recientes del turismo en Argentina y a nivel mundial.")),  #texto descriptivo, se pueden poner etiquetas html
                 btn_labels = "OK",
                 html = TRUE
  )
  
  
########TURISMO INTERNACIONAL#######################  
  #Creo y renderizo plotly a partir de ggplot
  output$grafico_ti_receptivo <- renderPlotly({ #idPlot
    
    if (input$serie_ti_receptivo == F) {
    
    #Armo ggplot y lo guardo en objeto
    grafico_ti_receptivo <- ggplot(data_grafico_ti)+
      geom_area(aes(period, receptivo), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.8,
                aes(period, receptivo, group = 1,
                 text = paste('Fecha:', format(period,"%b. %Y"), #argumento para etiqueta interactiva
                              '<br>Viajes de turistas no residentes:',lbl_int(receptivo)))) +
      theme_void()
    
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_ti_receptivo <- data_grafico_ti %>%
        filter(month == ultimo_ti) %>% 
        ggplot(aes(period, receptivo, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', format(period,"%b. %Y"), #argumento para etiqueta interactiva
                                  '<br>Viajes de turistas no residentes:',lbl_int(receptivo)))) +
        theme_void()
    }
    
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
    
    if (input$serie_evyth == F) {
    
    grafico_evyth <- ggplot(data_grafico_evyth)+
      geom_area(aes(date, turistas), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha= 0.8,
               aes(date, turistas, group=1,
                   text = paste('Fecha:', trim, anio, #argumento para etiqueta interactiva
                                  '<br>Turistas internos:',format(turistas, decimal.mark = ","),"M"))) +
      theme_void()
    
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_evyth <- data_grafico_evyth %>%
        filter(trimestre == ultimo_evyth) %>% 
        ggplot(aes(date, turistas, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', trim, anio,#argumento para etiqueta interactiva
                                  '<br>Turistas internos:',format(turistas, decimal.mark = ","),"M"))) +
        theme_void()
    }
    
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
  
##############EOH VIAJEROS##############
  output$grafico_eoh_viajeros <- renderPlotly({
    
    if (input$serie_eoh_viajeros == F) {
    
    grafico_eoh_viajeros <- ggplot(data_grafico_eoh)+
      geom_area(aes(date, viajeros_tot), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.8,
               aes(date, viajeros_tot, group = 1,
                   text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                '<br>Viajeros hospedados:',format(viajeros_tot, decimal.mark = ","),"M"))) +
      theme_void()
    
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_eoh_viajeros <- data_grafico_eoh %>%
        filter(month == ultimo_eoh) %>% 
        ggplot(aes(date, viajeros_tot, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                  '<br>Viajeros hospedados:',format(viajeros_tot, decimal.mark = ","),"M"))) +
        theme_void()
    }
    
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
  

###############EMPLEO###########################
  output$grafico_empleo_hyr <- renderPlotly({
    
    if (input$serie_empleo == F) {
    
    grafico_empleo_hyr <- ggplot(data_grafico_empleo)+
      geom_area(aes(fecha, empleo_hyr_ce), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.8, 
                aes(fecha, empleo_hyr_ce, group=1,
                    text = paste('Fecha:', format(fecha,"%b. %Y"), #argumento para etiqueta interactiva
                                 '<br>Trabajadores:',format(lbl_decimal(empleo_hyr_ce),1),"mil"))) +
      theme_void()
    
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_empleo_hyr <- data_grafico_empleo %>%
        filter(month == ultimo_empleo) %>% 
        ggplot(aes(fecha, empleo_hyr_ce, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', format(fecha,"%b. %Y"), #argumento para etiqueta interactiva
                                  '<br>Trabajadores:', format(lbl_decimal(empleo_hyr_ce),1),"mil"))) +
        theme_void()
    }
    
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
    
    if (input$serie_emae == F) {
    
    grafico_emae_hyr <- ggplot(data_grafico_emae)+
      geom_area(aes(date, emae_hyr_ce), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.8, 
                aes(date, emae_hyr_ce, group=1,
                    text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                 '<br>Índice (2004=100):',format(lbl_decimal(emae_hyr_ce),1),
                                 '<br>Var. i.a.:',format(lbl_percent(var_ia_emae_hyr), decimal.mark = ",")))) +
      theme_void()
    
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_emae_hyr <- data_grafico_emae %>%
        filter(month == ultimo_emae) %>% 
        ggplot(aes(date,emae_hyr_ce, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                  '<br>Índice (2004=100):',format(lbl_decimal(emae_hyr_ce),1),
                                  '<br>Var. i.a.:',format(lbl_percent(var_ia_emae_hyr), decimal.mark = ",")))) +
        theme_void()
    }
    
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
  
##############EOH PERNOCTES#################
  output$grafico_eoh_pernoc <- renderPlotly({
    
    if (input$serie_eoh_pernoc == F) {
    
    grafico_eoh_pernoc <- ggplot(data_grafico_eoh)+
      geom_area(aes(date, pernoc_tot), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.8, group=1,
                aes(date, pernoc_tot, 
                    text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                 '<br>Pernoctaciones:',format(pernoc_tot, decimal.mark = ","),"M"))) +
      theme_void()
    
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_eoh_pernoc <- data_grafico_eoh %>%
        filter(month == ultimo_eoh) %>% 
        ggplot(aes(date,pernoc_tot, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                  '<br>Pernoctaciones:',format(pernoc_tot, decimal.mark = ","),"M"))) +
        theme_void()
    }
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
###############CONECTIVIDAD INTERNACIONAL#############################
  output$grafico_conectividad_int <- renderPlotly({
    
    if (input$serie_conectividad_int == F) {
    
    grafico_conectividad_int <- ggplot(data_grafico_conectividad_internacional)+
      geom_area(aes(date, pax), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.8,
                aes(date, pax, group=1,
                   text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                '<br>Pasajeros:',format(pax_miles, decimal.mark = ","),"mil"))) +
      theme_void()
    
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_conectividad_int <- data_grafico_conectividad_internacional %>%
        filter(mes == ultimo_conectividad) %>% 
        ggplot(aes(date, pax, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                  '<br>Pasajeros:',format(pax_miles, decimal.mark = ","),"mil"))) +
        theme_void()
    }
    
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
    
    if (input$serie_conectividad_cab == F) {
    
    grafico_conectividad_cab <- ggplot(data_grafico_conectividad_cabotaje)+
      geom_area(aes(date, pax), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.8,
                aes(date, pax, group=1,
                    text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                 '<br>Pasajeros:',format(pax_mill, decimal.mark = ","),"M"))) +
      theme_void()
    
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_conectividad_cab <- data_grafico_conectividad_cabotaje %>%
        filter(mes == ultimo_conectividad) %>% 
        ggplot(aes(date,pax, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                  '<br>Pasajeros:',format(pax_mill, decimal.mark = ","),"M"))) +
        theme_void()
    }
    
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

###############TURISMO MUNDO#############################
  output$grafico_tur_mundo <- renderPlotly({
    
    if (input$serie_tur_mundo == F) {
    
    grafico_tur_mundo <- ggplot(data_grafico_mundo)+
      geom_area(aes(date, tur_mundo), fill = "white", alpha = 0.3)+
      geom_line(colour="white",alpha = 0.8,
                aes(date, tur_mundo, group=1,
                    text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                 '<br>Turistas:',lbl_decimal(tur_mundo,1),"M"))) +
      theme_void()
    } else {
      
      #Armo ggplot y lo guardo en objeto
      grafico_tur_mundo <- data_grafico_mundo %>%
        filter(mes == ultimo_tur_mundo) %>% 
        ggplot(aes(date,tur_mundo, group = 1 )) +
        geom_col(fill = "white", alpha = 0.6,
                 aes(text = paste('Fecha:', format(date,"%b. %Y"), #argumento para etiqueta interactiva
                                  '<br>Turistas:',lbl_decimal(tur_mundo,1),"M"))) +
        theme_void()
    }
    
    ggplotly(grafico_tur_mundo, 
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
      subtitle = ifelse(var_ia_ti>0, paste0("+",var_ia_ti," var. i.a."),paste0(var_ia_ti," var. i.a.")), #texto de abajo
      description = paste0("viajes de turistas no residentes - ",mes," ", anio), #texto debajo del indicador
      minititle = "TURISMO INTERNACIONAL",  #texto de arriba
      icon = icon("plane-arrival"), #icono de fontawesome
      infoID = "tur_internacional", #id para icono de info
      idPlot = "grafico_ti_receptivo",
      serieBtn = "serie_ti_receptivo", var_tiempo = mes,#id del renderPlotly
      color = "aqua" #color
    )
    
  })

############EVYTH###############    
  output$box_evyth_viajeros <- renderValueBox({ #id a usar en la UI
    
    #mes del dato a mostrar
    trim_evyth <- data_grafico_evyth%>% 
      tail(1) %>% 
      pull(trim)
    
    anio_evyth <- data_grafico_evyth%>% 
      tail(1) %>% 
      pull(anio)
    
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
      subtitle = ifelse(var_ia_evyth>0, paste0("+",lbl_percent(var_ia_evyth)," var. i.a."),paste0(lbl_percent(var_ia_evyth)," var. i.a.")), #texto de abajo
      description = paste0("turistas internos - ",trim_evyth, anio_evyth), #texto debajo del indicador
      minititle = "TURISMO INTERNO", #texto de arriba
      icon = icon("car"), #icono de fontawesome
      infoID = "tur_interno", #id para icono de info
      idPlot = "grafico_evyth", #id del renderPlotly
      serieBtn = "serie_evyth", var_tiempo = trim_evyth,
      color = "teal" #color
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
      subtitle = ifelse(var_ia_eoh_viajeros>0, paste0("+",lbl_percent(var_ia_eoh_viajeros)," var. i.a."),paste0(lbl_percent(var_ia_eoh_viajeros)," var. i.a.")), #texto de abajo
      description =  paste0("viajeros hospedados - ",mes_eoh, " ", anio_eoh), #texto debajo del indicador
      minititle ="OCUPACIÓN HOTELERA",  #texto de arriba
      icon = icon("hotel"), #icono de fontawesome
      infoID = "tur_eoh", #id para icono de info
      idPlot = "grafico_eoh_viajeros", #id del renderPlotly
      serieBtn = "serie_eoh_viajeros", var_tiempo=mes_eoh,
      color = "olive" #color
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
    
    var_ia_emae_total <- data_grafico_emae%>% 
      tail(1) %>% 
      pull(var_ia_total)
    
    
    
    valueBoxSpark(
      value = ifelse (var_ia_emae_hyr>0, paste0("+",lbl_percent(var_ia_emae_hyr)," var. i.a."),paste0(lbl_percent(var_ia_emae_hyr)," var. i.a.")), #indicador
      subtitle = ifelse(var_ia_emae_total>0, paste0("+",lbl_percent(var_ia_emae_total)," var. i.a. total economía"),paste0(lbl_percent(var_ia_emae_total)," var. i.a. total economía")), #texto de abajo
      description = paste0("act. económica en “Hoteles y Restaurantes” - ", mes_emae, " ", anio_emae), #texto debajo del indicador
      minititle = "ESTIMADOR MENSUAL DE ACT. ECONÓMICA", #texto de arriba
      icon = icon("coins"), #icono de fontawesome
      infoID = "emae_hyr", #id para icono de info
      idPlot = "grafico_emae_hyr", #id del renderPlotly
      serieBtn = "serie_emae", var_tiempo = mes_emae,
      color = "orange" #color
    )
    
  })

############EMPLEO###############  
  output$box_empleo_hyr <- renderValueBox({ #id a usar en la UI
    
    mes_empleo <- data_grafico_empleo %>% 
      tail(1) %>% 
      pull(fecha) %>% 
      months()
    
    anio_empleo <- data_grafico_empleo %>% 
      tail(1) %>% 
      pull(fecha) %>% 
      year()
    
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_empleo_hyr_ce <- data_grafico_empleo%>% 
      tail(1) %>% 
      pull(empleo_hyr_ce) 
    
    
    var_ia_empleo_hyr <- data_grafico_empleo%>% 
      tail(1) %>% 
      pull(var_ia) 
    
    
    
    valueBoxSpark(
      value = paste0 (lbl_decimal(ultimo_dato_empleo_hyr_ce,1)," mil"), #indicador
      subtitle = ifelse(var_ia_empleo_hyr>0, paste0("+",lbl_percent(var_ia_empleo_hyr)," var. i.a."),paste0(lbl_percent(var_ia_empleo_hyr)," var. i.a.")), #texto de abajo
      description = paste0("trabajadores en “Hoteles y Restaurantes” - ", mes_empleo, " ", anio_empleo), #texto debajo del indicador
      minititle = "EMPLEO PRIVADO REGISTRADO", #texto de arriba
      icon = icon("briefcase"), #icono de fontawesome
      infoID = "empleo_hyr", #id para icono de info
      idPlot = "grafico_empleo_hyr", #id del renderPlotly
      serieBtn = "serie_empleo", var_tiempo= mes_empleo,
      color = "yellow" #color
    )
    
  })
  ############EOH PERNOCTACIONES############### 
  
  output$box_eoh_pernoctes <- renderValueBox({ #id a usar en la UI
    
    mes_eoh <- data_grafico_eoh %>% 
      tail(1) %>% 
      pull(date) %>% 
      months()
    
    anio_eoh <- data_grafico_eoh %>% 
      tail(1) %>% 
      pull(date) %>% 
      year()
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_eoh_pernoctes <- data_grafico_eoh%>% 
      tail(1) %>% 
      pull(pernoc_tot) %>% 
      format(decimal.mark = ",")
    
    
    var_ia_eoh_pernoctes <- data_grafico_eoh%>% 
      tail(1) %>% 
      pull(var_ia_pernoc)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_eoh_pernoctes, " millones"), #indicador
      subtitle = ifelse(var_ia_eoh_pernoctes>0, paste0("+",lbl_percent(var_ia_eoh_pernoctes)," var. i.a."),paste0(lbl_percent(var_ia_eoh_pernoctes)," var. i.a.")), #texto de abajo
      description = paste0("pernoctaciones - ",mes_eoh, " ", anio_eoh), #texto debajo del indicador
      minititle ="OCUPACIÓN HOTELERA", #texto de arriba
      icon = icon("bed"), #icono de fontawesome
      infoID = "tur_eoh_p", #id para icono de info
      idPlot = "grafico_eoh_pernoc", #id del renderPlotly
      serieBtn = "serie_eoh_pernoc", var_tiempo = mes_eoh,
      color = "green" #color
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
      pull(pax_miles) %>% 
      format(decimal.mark=",")
    
    var_ia_conectividad_int <- data_grafico_conectividad_internacional %>% 
      tail(1) %>% 
      pull(var_ia_pax_int)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_pax_int, " mil"), #indicador
      subtitle =  ifelse(var_ia_conectividad_int>0, paste0("+",lbl_percent(var_ia_conectividad_int)," var. i.a."),paste0(lbl_percent(var_ia_conectividad_int)," var. i.a.")), #texto de abajo
      description = paste0("pasajeros en vuelos internacionales - ", mes_conectividad, " ", anio_conectividad), #texto debajo del indicador
      minititle = "CONECTIVIDAD AÉREA INTERNACIONAL", #texto de arriba
      icon = icon("plane-up"), #icono de fontawesome
      infoID = "conectividad_i", #id para icono de info
      idPlot = "grafico_conectividad_int", #id del renderPlotly
      serieBtn = "serie_conectividad_int", var_tiempo=mes_conectividad,#id del renderPlotly
      color = "fuchsia" #color
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
      pull(pax_mill) %>% 
      format(decimal.mark=",")
    
    var_ia_conectividad_cab <- data_grafico_conectividad_cabotaje %>% 
      tail(1) %>% 
      pull(var_ia_pax_cab)
    
    
    
    valueBoxSpark(
      value = paste0 (ultimo_dato_pax_cab, " millones"), #indicador
      subtitle =  ifelse(var_ia_conectividad_cab>0, paste0("+",lbl_percent(var_ia_conectividad_cab)," var. i.a."),paste0(lbl_percent(var_ia_conectividad_cab)," var. i.a.")), #texto de abajo
      description = paste0("pasajeros en vuelos de cabotaje - ", mes_conectividad, " ", anio_conectividad), #texto debajo del indicador
      minititle = "CONECTIVIDAD AÉREA DE CABOTAJE", #texto de arriba
      icon = icon("plane-up"), #icono de fontawesome
      infoID = "conectividad_c", #id para icono de info
      idPlot = "grafico_conectividad_cab", #id del renderPlotly
      serieBtn = "serie_conectividad_cab", var_tiempo = mes_conectividad,
      color = "red" #color
    )
    
  })

###############TURISMO EN EL MUNDO#####################
  output$box_tur_mundo <- renderValueBox({ #id a usar en la UI
    
    mes_mundo <- data_grafico_mundo %>% 
      tail(1) %>% 
      pull(date) %>% 
      months()
    
    anio_mundo <- data_grafico_mundo %>% 
      tail(1) %>% 
      pull(date) %>% 
      year()
    
    
    #calculo de ultimo dato y var i.a.
    ultimo_dato_mundo <- data_grafico_mundo%>% 
      tail(1) %>%
      pull(tur_mundo) %>% 
      as.numeric()
    
    var_ia_mundo <- data_grafico_mundo %>% 
      tail(1) %>% 
      pull(var_ia)
    
    
    
    valueBoxSpark(
      value = paste0(lbl_decimal(ultimo_dato_mundo,1), " millones"), #indicador
      subtitle =   ifelse(var_ia_mundo>0, paste0("+",lbl_percent(var_ia_mundo)," var. i.a."),paste0(lbl_percent(var_ia_mundo)," var. i.a.")), #texto de abajo
      description = paste0("llegadas de turistas internacionales al mundo - ",mes_mundo," ", anio_mundo), #texto debajo del indicador
      minititle = "TURISMO EN EL MUNDO",#texto de arriba
      icon = icon("earth-americas"), #icono de fontawesome
      infoID = "tur_mundo", #id para icono de info
      idPlot = "grafico_tur_mundo", #id del renderPlotly
      serieBtn = "serie_tur_mundo", var_tiempo = mes_mundo,
      color = "maroon" #color
    )
    
  })
  
#Observe para mostrar popup al clickear el icono de info
  
  observeEvent(input$tur_internacional,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Turismo internacional", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/turismo_internacional",
                                                                              "tablero interactivo",target = '_blank'), "o los", tags$a( href="https://www.yvera.tur.ar/sinta/informe/info/turismo-internacional",
                                                                                                                       "informes técnicos.",target = '_blank'), "También podés descargar los", tags$a( href="https://datos.yvera.gob.ar/dataset?groups=turismo-internacional",
                                                                                                                                                                                     "datos abiertos.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$tur_interno,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Turismo interno", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/interno.html",
                                                                              "reporte de los últimos datos",target = '_blank'), "o los",tags$a( href="https://www.yvera.tur.ar/sinta/informe/info/encuesta-de-viajes-y-turismo-de-los-hogares-evyth",
                                                                                                                                    "informes técnicos.",target = '_blank'),"También podés descargar los", tags$a( href="https://datos.yvera.gob.ar/dataset?groups=turismo-interno",
                                                                                                                                                                                                 "datos abiertos.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$tur_eoh,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Ocupación hotelera", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/eoh.html",
                                                                              "reporte de los últimos datos",target = '_blank'),"o los",tags$a(href="https://www.yvera.tur.ar/sinta/informe/info/encuesta-de-ocupacion-hotelera-eoh",
                                                                                                                             "informes técnicos.",target = '_blank'), "también podés descargar los", tags$a(href="https://datos.yvera.gob.ar/dataset?groups=sector-hotelero",
                                                                                                                                                                                          "datos abiertos.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  observeEvent(input$emae_hyr,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Estimador mensual de actividad económica (EMAE)", #titulo del popup
                   text = HTML(paste("Información proveniente del INDEC en base al", tags$a( href="https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-9-48",
                                                                              "Estimador mensual de actividad económica (EMAE).",target = '_blank'),"Para más información visitá el",tags$a( href="https://biblioteca.yvera.tur.ar/coyuntura.html",
                                                                              "Informe mensual de estadísticas de turismo.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })

  observeEvent(input$empleo_hyr,{ #infoID que se puso en la función de arriba 
  
  sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                 title = "Empleo privado registrado", #titulo del popup
                 text = HTML(paste("Información proveniente del MTEySS en base a la",tags$a( href="https://www.argentina.gob.ar/trabajo/estadisticas",
                                                                            "Situación y Evolución del Trabajo Registrado (SIPA).",target = '_blank')," Para más información visitá el",tags$a( href="https://biblioteca.yvera.tur.ar/coyuntura.html",
                                                                            "Informe mensual de estadísticas de turismo.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                 btn_labels = NA,
                 html = TRUE#no mostrar botones que vienen por default
  )
})
  observeEvent(input$tur_eoh_p,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Ocupación hotelera", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/eoh.html",
                                                                              "reporte de los últimos datos",target = '_blank'),"o los",tags$a(href="https://www.yvera.tur.ar/sinta/informe/info/encuesta-de-ocupacion-hotelera-eoh",
                                                                                                                             "informes técnicos.",target = '_blank'), "también podés descargar los", tags$a(href="https://datos.yvera.gob.ar/dataset?groups=sector-hotelero",
                                                                                                                                                                                           "datos abiertos.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })

  observeEvent(input$conectividad_i,{ #infoID que se puso en la función de arriba 
  
  sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                 title = "Conectividad aérea", #titulo del popup
                 text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/conectividad/",
                                                                            "tablero interactivo.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                 btn_labels = NA,
                 html = TRUE#no mostrar botones que vienen por default
  )
})
  
  observeEvent(input$conectividad_c,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Conectividad aérea", #titulo del popup
                   text = HTML(paste("Para más información visitá el",tags$a( href="https://tableros.yvera.tur.ar/conectividad/",
                                                                              "tablero interactivo.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
  
  
  
  observeEvent(input$tur_mundo,{ #infoID que se puso en la función de arriba 
    
    sendSweetAlert(type = "info", #tipo popup: info muestra el "¡"
                   title = "Turismo en el mundo", #titulo del popup
                   text = HTML(paste("Información proveniente del", tags$a( href="https://www.unwto.org/tourism-data/global-and-regional-tourism-performance",
                                                                          "panel de datos de turismo de la OMT.",target = '_blank')," Para más información visitá el",tags$a( href="https://biblioteca.yvera.tur.ar/coyuntura.html",
                                                                              "Informe mensual de estadísticas de turismo.",target = '_blank'))),  #texto descriptivo, se pueden poner etiquetas html
                   btn_labels = NA,
                   html = TRUE#no mostrar botones que vienen por default
    )
  })
}  
  

