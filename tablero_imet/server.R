#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output) {
    
  output$box_ti <- renderValueBox({
    
    mes <- as.character(month(floor_date(Sys.Date(),"month")- months(2),label = T,abbr = F)) 
      
    
    ultimo_dato_ti <- data_grafico_ti %>% 
     tail(1) %>% 
      pull(receptivo)

    
    interanual_dato_ti <- data_grafico_ti %>% 
      filter(row_number() == nrow(data_grafico_ti)-12) %>% 
      pull(receptivo)
        
    
    var_ia_ti <- lbl_percent((ultimo_dato_ti/interanual_dato_ti-1))
    
      valueBoxSpark(
      value = lbl_int(ultimo_dato_ti),
      subtitle = paste0(var_ia_ti," variación interanual"),
      description = "viajes de turistas receptivos",
      icon = icon("plane"),
      color = "aqua",
      spark = grafico_ti,
      minititle = paste0("Turismo internacional ", mes),
      width = NULL 
    )
      

  })
    
}



