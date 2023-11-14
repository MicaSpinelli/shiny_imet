valueBoxSpark <- function(value, subtitle, description, minititle = NULL, 
                          icon = NULL, color = "aqua",
                          idPlot, infoID, serieBtn, var_tiempo= "último mes") {
  
  #creo icono de info con id
  info_icon <- tags$small(
    actionButton(inputId = infoID, icon = icon("info-circle"), label = NULL, style = "background-color: transparent; color: white; border-color: transparent;"),
    class = "pull-right"
  )
  
  serie_icon <- tags$small(
    prettyToggle(
      inputId = serieBtn,
      label_on = "Ver serie completa",
      icon_on = icon("chart-line"),
      status_on = "success",
      status_off = "success",
      label_off = paste0("Ver ",var_tiempo),
      icon_off = icon("chart-simple"),
      outline = TRUE,
      value = FALSE,
      plain = TRUE
    ), 
    style = "background-color: transparent !important"
    
  )
  
  
  #creo caja con todos los elementos
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      if (!is.null(icon)) div(class = "icon-large", icon, style = "bottom: unset !important;"),
      if(!is.null(minititle)) h4(minititle),#tags$small(minititle, style = "color: white;"),
      h3(value),
      if (!is.null(description)) h5(description),
      plotlyOutput(idPlot, width = "auto", height = 100),
      serie_icon,
      info_icon,
      h4(subtitle)
    )
  )
  
  
  div(boxContent)
}



