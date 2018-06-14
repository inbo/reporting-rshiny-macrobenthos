shinyServer(function(input, output) {

  pts2 <- reactive({
    if (input$variabele == "densiteit") {
      ptsvar <- pts$densiteit
    } else {
      ptsvar <- pts$biomassa
    }
    calc <- (ptsvar + min(ptsvar[ptsvar != 0]))/max(ptsvar)
    pts$varsize <- (sqrt(calc)) * 10 + 2
    pts2 <- pts
    pts2
  })

  # Leaflet Map -----

  # create a reactive value that will store the click position
  # data_of_click <- reactiveValues(clickedMarker=NULL)
  output$map <- renderLeaflet({
    pts2 <- pts2()
    pts2 <- pts2[pts2$soort == input$soort,]
    if (input$jaar == "alle jaren") {
      pts2 <- pts2
    } else {
      pts2 <- pts2[pts2$jaar == input$jaar,]
    }

    if (input$tidaal == "beide") {
      pts2 <- pts2
    } else {
      pts2 <- pts2[pts2$tidaal == input$tidaal,]
    }

    if (input$variabele == "densiteit") {
      pts2 <- pts2[order(pts2$densiteit),]
      pts2var <- pts2$densiteit
      title <- HTML("densiteit (ind/m<sup>2</sup>)")
    } else {
      pts2 <- pts2[order(pts2$densiteit),]
      pts2var <- pts2$biomassa
      title <- HTML("biomassa (g/m<sup>2</sup>)")}
      pal2 <- colorNumeric(palette = "Spectral", domain = pts2var)

      leaflet(pts2, options = list(center = c(51.18, 4.3), zoom = 10)) %>%
        addProviderTiles(group = "CartoDB.Positron", providers$CartoDB.Positron) %>%
        addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=y&hl=en&src=app&x={x}&y={y}&z={z}&s=G", group = "Google Hybrid", attribution = 'Google') %>%
        addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", group = "Google Aerial", attribution = 'Google') %>%
        addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=m&hl=en&src=app&x={x}&y={y}&z={z}&s=G", group = "Google Roads", attribution = 'Google') %>%
        addProviderTiles('Esri.WorldImagery', group = "Esri satellite") %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles(group = "OpenTopoMap", providers$OpenTopoMap) %>%
        addCircleMarkers(group = "densiteit", radius = ~pts2$varsize,
                         color = "white", weight = 1, layerId = ~locatie,
                         fillColor = ~pal2(pts2var), opacity = .6,
                         fillOpacity = .6, label = ~htmlEscape(locatie)) %>%
        addLegend("bottomright", pal = pal2, title = title,
                  values = ~pts2var, opacity = 1) %>%
      addLayersControl(baseGroups = c("CartoDB.Positron", "Google Hybrid",
                                      "Google Aerial", "Google Roads",
                                      "Esri satellite", "OpenStreetMap",
                                      "OpenTopoMap"),
                       options = layersControlOptions(collapsed = FALSE))
  })

  output$leafl1 <- renderUI({
    if (!is.null(input$GetScreenHeight)) {
      width  <- session$clientData$output_image1_width
      # print(session$clientData)
      height <- session$clientData$output_image1_height
      leafletOutput("map", width = "100%", height = input$GetScreenHeight*.7)
    }
  })

  observeEvent(input$map_marker_click, {
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    print(event)
    if (is.null(event))
      return()
    isolate({
      d <- pts2()
      sgh <- d[d$locatie == event$id,]
      content <-  paste(
        "<b>locatie:</b>", as.character(unique(sgh$locatie)), "<br>",
        "<b>jaar:</b>", as.character(unique(sgh$jaar)), "<br>",
        "<b>waterlichaam:</b>", as.character(unique(sgh$waterlichaam)), "<br>",
        "<b>waterloop:</b>", as.character(unique(sgh$waterloop)), "<br>",
        "<b>fysiotoop:</b>", as.character(unique(sgh$fysiotoop)), "<br>",
        "<b>tidaal:</b>", as.character(unique(sgh$tidaal)), "<br><br>",
        paste0("<b>", as.character(sgh$soort), "</b><br>",
               "   ", as.character(round(sgh$densiteit, 1)),
               " ind/m<sup>2</sup> - - - ",
               as.character(round(sgh$biomassa, 4)), " g/m<sup>2</sup>",
               collapse = "<br>")
      )
      leafletProxy("map") %>% addPopups(event$lng, event$lat, content)
    })
  })

  # # store the click
  # observeEvent(input$map_marker_click,{
  #   data_of_click$clickedMarker <- input$map_marker_click
  # })
  # # make table
  # output$loctab <- renderTable({
  #   my_place <- data_of_click$clickedMarker$locatie
  #   if(is.null(my_place)){my_place="DD08_01"}
  #   data.frame(pts[pts$locatie==my_place,c("locatie","fysiotoop","soort","densiteit")])
  # })

  # Graph -----
  output$grafiek1 <- renderPlot({
    dens$jaar <- factor(dens$jaar)
    ifelse(input$jaar == "alle jaren",
           dens2 <- dens,
           dens2 <- dens[dens$jaar == input$jaar,])
    if (input$tidaal == "beide") {
      dens2 <- dens2
    } else {
      dens2 <- dens2[dens2$tidaal == input$tidaal,]
    }
    grafdat <- dens2
    if (input$gt1 == "cnt1") {
      if (input$jaar != "alle jaren") {
        grafdat <- grafdat[grafdat$soort == input$soort,]
        grafdat <- dcast(grafdat, jaar + waterloop + fysiotoop ~ .,
                         length, drop = FALSE)

        ggplot(grafdat, aes(x = waterloop, y = ., fill = fysiotoop)) +
          geom_bar(stat = "identity", position = position_dodge()) +
          scale_x_discrete(drop = FALSE) +
          scale_y_continuous(
            breaks = function(x) {
              unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
            }) +
          labs(x = "", y = "aantal stalen")
        # + facet_wrap(~jaar, ncol = 1)
      } else {
        grafdat <- dens[dens$jaar != "alle jaren",]
        grafdat <- dcast(grafdat, waterloop + fysiotoop ~ .,
                         length, drop = FALSE)

        ggplot(grafdat, aes(x = waterloop, y = ., fill = fysiotoop)) +
          geom_bar(stat = "identity", position = position_dodge()) +
          scale_x_discrete(drop = FALSE) +
          scale_y_continuous(
            breaks = function(x) {
              unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
            }) +
          labs(x = "", y = "aantal stalen")}
    } else {
      grafdat <- dcast(grafdat, jaar + waterloop + fysiotoop + locatie ~ soort,
                       sum, value.var = input$variabele)
      grafdat <- grafdat[c("jaar", "waterloop", "fysiotoop", "locatie",
                           input$soort)]
      names(grafdat)[names(grafdat) == names(grafdat)[5]] <- input$variabele

      if (input$variabele == "densiteit") {
        ggplot(grafdat, aes(x = waterloop,
                            y = densiteit + min(grafdat$densiteit[grafdat$densiteit != 0]),
                            fill = fysiotoop)) +
          geom_boxplot() +
          scale_x_discrete(drop = FALSE) +
          scale_fill_discrete(drop = FALSE) +
          labs(x = "", y = HTML("densiteit (ind/m2)")) +
          scale_y_log10()
        # + facet_wrap(~jaar, ncol = 1, drop = FALSE)
      } else {
        ggplot(grafdat, aes(x = waterloop,
                            y = biomassa + min(grafdat$biomassa[grafdat$biomassa != 0]),
                            fill = fysiotoop)) +
          geom_boxplot() +
          scale_x_discrete(drop = FALSE) +
          scale_fill_discrete(drop = FALSE) +
          labs(x = "", y = HTML("biomassa (g/m2)")) +
          scale_y_log10()
        # + facet_wrap(~jaar, ncol = 1, drop = FALSE)
      }
    }
  })

  # Table -----
  output$tabel <- renderDataTable({
    ifelse(input$jaar == "alle jaren",
           dens2 <- dens[dens$soort == input$soort,],
           dens2 <- dens[dens$soort == input$soort & dens$jaar == input$jaar,])
    if (input$tidaal == "beide") {
      dens2 <- dens2
    } else {
      dens2 <- dens2[dens2$tidaal == input$tidaal,]
    }
    dens2$densiteit <- round(dens2$densiteit, 1)
    subset(dens2, select = -c(soort, campagne))
  })
})
