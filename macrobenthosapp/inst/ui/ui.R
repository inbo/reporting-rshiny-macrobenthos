jscode <- '
$(document).on("shiny:connected", function(e) {
var jsHeight = window.innerHeight;
Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'

shinyUI(fluidPage(
  theme = shinytheme("united"),
  # themeSelector(),
  titlePanel(paste0("Monitoring macrobenthos Zeeschelde en getijgebonden zijrivieren (",
                    min(dens$jaar),"-", max(dens$jaar),")")),
  fluidRow(
    column(3, selectizeInput(inputId = "soort", label = "soort",
                             options = list(dropdownParent = 'body'),
                             choices = sort(unique(dens$soort))
    )),
    column(3, selectizeInput(inputId = "variabele", label = "variabele",
                             options = list(dropdownParent = 'body'),
                             choices = c("densiteit", "biomassa")
    )),
    column(3, selectizeInput(inputId = "jaar", label = "jaar",
                             options = list(dropdownParent = 'body'),
                             choices = c("alle jaren", sort(unique(as.character(dens$jaar)), decreasing = TRUE))
    )),
    column(3, selectizeInput(inputId = "tidaal", label = "tidaal",
                             options = list(dropdownParent = 'body'),
                             choices = c("beide","inter","sub")
    ))
  ),
  tabsetPanel(
    tabPanel("Kaart",
             fluidRow(
               # column(2, tableOutput("loctab")),
               column(8, offset = 2,
                      tags$script(jscode),
                      uiOutput("leafl1")
                      # leafletOutput("map", width = "100%", height = "650px")
               )
             )),
    tabPanel("Grafieken",
             sidebarPanel(width = 2,
                          radioButtons('gt1', 'Selecteer type grafiek',
                                       c(variabele = 'dens1',
                                         'aantal stalen' = 'cnt1'),
                                       selected = 'dens1')
             ),
             mainPanel(width = 10,
                       fluidRow(
                         column(8,
                                # offset=1,
                                plotOutput("grafiek1", width = "130%",
                                           height = "700px")))
             )
    ),
    tabPanel("Tabel",
             fluidRow(column(8, dataTableOutput("tabel"))
             ))
  )
))
