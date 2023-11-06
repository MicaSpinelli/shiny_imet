#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

dashboardPage(
    dashboardHeader(),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(column(4, valueBoxOutput("box_ti_receptivo",width = "100%")),
                 column(4, valueBoxOutput("box_evyth_viajeros", width = "100%")),
                 column(4, valueBoxOutput("box_eoh_viajeros", width = "100%"))
                        
    ),
    fluidRow(column(4, valueBoxOutput("box_eoh_pernoctes",width = "100%")),
             column(4, valueBoxOutput("box_empleo_hyr", width = "100%")),
             column(4, valueBoxOutput("box_emae_hyr", width = "100%"))
             
    ),
    fluidRow(column(4, valueBoxOutput("box_conectividad_cab",width = "100%")),
             column(4, valueBoxOutput("box_conectividad_int", width = "100%")),
             column(4, valueBoxOutput("box_tur_mundo", width = "100%"))
             
    )
))



