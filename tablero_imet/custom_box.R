# valueBoxSpark <- function(value, subtitle, description, icon = NULL, color = "aqua", info = NULL, 
#                           width = 4, href = NULL, spark = NULL, height_spark = 100,minititle = NULL) {
#   
#   spark <- plotly::ggplotly(spark +
#                               theme_void(), height = height_spark) %>% 
#     plotly::layout(paper_bgcolor='rgba(0,0,0,0)',
#            plot_bgcolor='rgba(0,0,0,0)', 
#            xaxis = list(
#              type = "linear", 
#              ticks = "", 
#              anchor = "y", 
#              mirror = FALSE, 
#              showgrid = FALSE, 
#              showline = FALSE, 
#              zeroline = FALSE, 
#              autorange = TRUE, 
#              showticklabels = FALSE
#            ), 
#            yaxis = list(
#              type = "linear", 
#              ticks = "", 
#              anchor = "x", 
#              mirror = FALSE, 
#              showgrid = FALSE, 
#              showline = FALSE, 
#              zeroline = FALSE, 
#              autorange = TRUE, 
#              showticklabels = FALSE
#            )) %>% 
#     plotly::config(displayModeBar = F)
#   
#   boxContent <- div(
#     class = paste0("small-box bg-", color),
#     div(
#       class = "inner",
#       if(!is.null(minititle)) tags$small(minititle, style = "color: black;"),
#       h3(value),
#       if (!is.null(description)) h4(description),
#       # tags$span(style = paste0("height:", height_spark), hc_size(spark, height = "100vh")),
#       tags$span(spark),
#       if (!is.null(subtitle)) p(subtitle)
#     ),
#     if (!is.null(icon)) div(class = "icon-large", icon)
#   )
#   
#   if (!is.null(href)) 
#     boxContent <- a(href = href, boxContent)
#   
#   div(class = if (!is.null(width)) 
#     paste0("col-sm-", width), boxContent)
#   
# }


#este se armo el 1/11
valueBoxSpark <- function(value, subtitle, description, minititle = NULL, 
                          icon = NULL, color = "aqua",
                          idPlot, infoID) {
  
  #creo icono de info con id
  info_icon <- tags$small(
    actionButton(inputId = infoID, icon = icon("info-circle"), label = NULL, style = "background-color: transparent; color: white; border-color: transparent;"),
    class = "pull-right"
  )
  
  #creo caja con todos los elementos
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      if (!is.null(icon)) div(class = "icon-large", icon, style = "bottom: unset !important;"),
      if(!is.null(minititle)) tags$small(minititle, style = "color: black;"),
      h3(value),
      if (!is.null(description)) h4(description),
      plotlyOutput(idPlot, width = "auto", height = 100),
      info_icon,
      p(subtitle)
    )
  )
  
  
  div(boxContent)
}



