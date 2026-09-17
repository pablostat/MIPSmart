# server.R
library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)

server <- function(input, output, session) {
  
  # ==========================================
  # REACTIVE VALUES
  # ==========================================
  
  selected_trap <- reactiveVal(NULL)
  selected_location <- reactiveVal(NULL)
  
  # Reactive participatory records
  records <- reactiveVal(participatory_records)
  
  # ==========================================
  # TAB 1: SURVEILLANCE MAP
  # ==========================================
  
  output$trap_map <- renderLeaflet({
    leaflet(trap_locations) %>%
      addTiles() %>%
      setView(lng = -67.1, lat = -12.2, zoom = 10) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        layerId = ~id,
        label = ~location,
        popup = ~paste("<b>", location, "</b><br>Altitud:", altitude, "m"),
        radius = 10,
        color = "#2c7a4a",
        fillColor = "#4CAF50",
        fillOpacity = 0.8,
        # icon = makeIcon(
        #   iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
        #   iconWidth = 25, iconHeight = 41,
        #   iconAnchorX = 12, iconAnchorY = 41
        # )
      )
  })
  
  # Click event on map
  observeEvent(input$trap_map_marker_click, {
    click <- input$trap_map_marker_click
    if(!is.null(click$id)) {
      selected_trap(as.numeric(click$id))
      loc <- trap_locations %>% filter(id == as.numeric(click$id))
      selected_location(loc)

      # Update weather info in participatory tab
      weather <- get_weather(loc$lat, loc$lng)
      output$weather_info <- renderPrint({
        cat("🌡️ Temperatura:", weather$temperature, "°C\n")
        cat("💧 Humedad:", weather$humidity, "%\n")
        cat("💨 Viento:", weather$wind_speed, "m/s\n")
        cat("🏔️ Altitud:", loc$altitude, "m.s.n.m.\n")
      })

      # Show notification
      # showNotification(paste("Ubicación seleccionada:", loc$location),
      #                  type = "success", duration = 3)
      
      showNotification(
        ui = paste("Ubicación seleccionada:", loc$location),
        type = "message",  
        duration = 5    
      )
    }
  })
  
  # ==========================================
  # COUNT GRAPH FOR SELECTED TRAP
  # ==========================================
  
  output$count_graph <- renderPlotly({
    req(selected_trap())
    trap_id <- selected_trap()
    
    # data <- trap_counts %>% filter(trap_id == 6)
    # data <- trap_counts %>% filter(trap_id == trap_id)
    datos <- trap_counts[trap_counts$trap_id==trap_id,]
    # loc <- trap_locations %>% filter(id == 8)1
    # loc <- trap_locations %>% filter(id == trap_id)
    loc <- trap_locations[trap_locations$id==trap_id,]
    
    if(nrow(datos) == 0) {
      return(plot_ly() %>%
               layout(title = "Sin datos para esta trampa"))
    }
    
    plot_ly(datos, x = ~date, y = ~count, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#2c7a4a', width = 3),
            marker = list(color = '#1e5a35', size = 10),
            hoverinfo = 'text',
            text = ~paste("Fecha:", date, "<br>Conteo:", count)) %>%
      layout(
        title = paste("Conteo de B. cockerelli -", loc$location),
        xaxis = list(title = "Fecha", tickangle = -45),
        yaxis = list(title = "Número de insectos"),
        hovermode = 'x unified'
      )
    
  })
  
  # ==========================================
  # TAB 2: PARTICIPATORY SYSTEM
  # ==========================================
  
  # Auto-fill weather on load (default location)
  observe({
    default_loc <- trap_locations[1, ]
    weather <- get_weather(default_loc$lat, default_loc$lng)
    output$weather_info <- renderPrint({
      cat("🌡️ Temperatura:", weather$temperature, "°C\n")
      cat("💧 Humedad:", weather$humidity, "%\n")
      cat("💨 Viento:", weather$wind_speed, "m/s\n")
      cat("🏔️ Altitud:", default_loc$altitude, "m.s.n.m.\n")
    })
  })
  
  # Submit new participatory record
  observeEvent(input$submit_record, {
    req(input$farmer_name, input$host_plant)
    
    # Get current weather (simulated)
    weather <- get_weather(-12.1, -77.05)  # Default Lima coordinates
    
    new_record <- data.frame(
      record_id = max(records()$record_id) + 1,
      date = Sys.Date(),
      farmer = input$farmer_name,
      host_plant = input$host_plant,
      pest_present = ifelse(input$pest_present == "yes", "Sí", "No"),
      count_estimate = input$est_count,
      image_evidence = if(!is.null(input$photo_evidence)) input$photo_evidence$name else NA,
      latitude = -12.1,
      longitude = -77.05,
      altitude = sample(trap_locations$altitude, 1),
      temperature = weather$temperature,
      humidity = weather$humidity,
      wind_speed = weather$wind_speed
    )
    
    # Add to records
    new_records <- bind_rows(records(), new_record)
    records(new_records)
    
    # Calculate confidence for this record
    confidence <- calculate_confidence(new_record)
    
    # showNotification(
    #   ui = paste("✅ Registro exitoso! Confianza de identificación:", confidence, "%"),
    #   type = "success", duration = 5
    # )
    
    showNotification(
      ui = paste("✅ Registro exitoso! Confianza de identificación:", confidence, "%"),,
      type = "message",  
      duration = 5    
    )
    
    # Reset form
    updateTextInput(session, "farmer_name", value = "")
    updateNumericInput(session, "est_count", value = 0)
    updateRadioButtons(session, "pest_present", selected = "no")
    updateSelectInput(session, "host_plant", selected = "Papa")
  })
  
  # ==========================================
  # INDICATORS FOR PARTICIPATORY TAB
  # ==========================================
  
  output$confidence_score <- renderValueBox({
    current_records <- records()
    if(nrow(current_records) > 0) {
      last_record <- current_records[nrow(current_records), ]
      conf <- calculate_confidence(last_record)
    } else {
      conf <- 0
    }
    valueBox(
      value = paste0(conf, "%"),
      subtitle = "Confianza de identificación (último registro)",
      icon = icon("check-circle"),
      color = if(conf >= 70) "green" else if(conf >= 40) "yellow" else "red"
    )
  })
  
  output$records_today <- renderValueBox({
    today_records <- records() %>% filter(date == Sys.Date())
    valueBox(
      value = nrow(today_records),
      subtitle = "Registros hoy",
      icon = icon("calendar-day"),
      color = "blue"
    )
  })
  
  output$positive_rate <- renderValueBox({
    positive <- records() %>% filter(pest_present == "Sí") %>% nrow()
    total <- nrow(records())
    rate <- ifelse(total > 0, round(positive/total * 100, 1), 0)
    valueBox(
      value = paste0(rate, "%"),
      subtitle = "Tasa de detección positiva",
      icon = icon("bug"),
      color = if(rate > 50) "orange" else "green"
    )
  })
  
  output$records_table <- renderDT({
    
    datosPR <- records()
    datosPR <- datosPR[,c("farmer","date","host_plant","pest_present","count_estimate","temperature","humidity")]
    # records() %>%
    #   select(farmer, date, host_plant, pest_present, count_estimate, temperature, humidity) %>%
    datosPR %>%
      datatable(
        options = list(
          pageLength = 5,
          dom = 'Bfrtip',
          scrollX = TRUE
        ),
        rownames = FALSE,
        caption = "Últimos registros participativos"
      ) %>%
      formatStyle(
        'pest_present',
        backgroundColor = styleEqual(c("Sí", "No"), c("#ffcccc", "#ccffcc"))
      )
  })
  
  # ==========================================
  # TAB 3: IPM MODULE
  # ==========================================
  
  output$bio_table <- renderDT({
    ipm_data$biological %>%
      datatable(
        options = list(dom = 't', pageLength = 10),
        rownames = FALSE,
        caption = "Agentes de control biológico"
      ) %>%
      formatStyle('effectiveness',
                  background = styleColorBar(range(ipm_data$biological$effectiveness), 'lightgreen'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  output$chem_table <- renderDT({
    ipm_data$chemical %>%
      datatable(
        options = list(dom = 't', pageLength = 10),
        rownames = FALSE,
        caption = "Ingredientes activos químicos"
      )
  })
  
  output$cultural_table <- renderDT({
    ipm_data$cultural %>%
      datatable(
        options = list(dom = 't', pageLength = 10),
        rownames = FALSE,
        caption = "Prácticas culturales"
      )
  })
  
  output$ipm_location_recommendation <- renderUI({
    if(!is.null(selected_location())) {
      loc <- selected_location()
      HTML(paste0(
        "<div style='background: #e8f5e9; padding: 15px; border-radius: 8px;'>",
        "<h4><i class='fas fa-map-pin'></i> Recomendaciones para: <strong>", loc$location, "</strong></h4>",
        "<p><b>Altitud:</b> ", loc$altitude, " m.s.n.m.</p>",
        "<hr>",
        "<h5>🔹 Control Biológico prioritario:</h5>",
        "<ul><li>Aplicar <b>Beauveria bassiana</b> en horas de la mañana</li>",
        "<li>Instalar <b>septos de feromona</b> para monitoreo (1 por ha)</li></ul>",
        "<h5>🔹 Control Químico (umbral > 5 insectos/trampa):</h5>",
        "<ul><li>Rotar ingredientes activos para evitar resistencia</li>",
        "<li>Usar <b>Abamectina</b> en etapas tempranas</li></ul>",
        "<h5>🔹 Prácticas culturales:</h5>",
        "<ul><li>Implementar <b>cobertura vegetal</b> con leguminosas</li>",
        "<li>Manejo de residuos post-cosecha</li></ul>",
        "<p style='color: #856404; margin-top: 10px;'><i class='fas fa-info-circle'></i> Basado en condiciones de la ubicación</p>",
        "</div>"
      ))
    } else {
      HTML("<div style='padding: 20px; background: #f8d7da; border-radius: 8px;'>
            <i class='fas fa-exclamation-triangle'></i> 
            <b>Seleccione una ubicación</b> en el mapa de vigilancia para ver recomendaciones específicas.
           </div>")
    }
  })
  
  # ==========================================
  # TAB 4: ILCYM INDICATORS
  # ==========================================
  
  output$ilcym_graph <- renderPlotly({
    plot_ly(ilcym_smooth, x = ~temp, y = ~R0, type = 'scatter', mode = 'lines',
            line = list(color = '#d32f2f', width = 4),
            fill = 'tozeroy', fillcolor = 'rgba(211, 47, 47, 0.2)',
            hoverinfo = 'text',
            text = ~paste("Temperatura:", temp, "°C<br>R₀:", round(R0, 2))) %>%
      layout(
        title = "Tasa Neta de Reproducción (R₀) - Bactericera cockerelli",
        xaxis = list(title = "Tiempo (dias)", range = c(0, 15)),
        yaxis = list(title = "R₀", range = c(0, 70)),
        annotations = list(
          x = 24, y = 65,
          text = "Óptimo: ~24-26°C"
          # showarrow = TRUE,
          # arrowhead = 1,
          # ax = 0, ay = -30
        )
      )
  })
  
  # ILCYM indicators (using participatory records)
  output$total_records_ilcym <- renderValueBox({
    valueBox(
      value = nrow(records()),
      subtitle = "Total registros participativos",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$avg_confidence <- renderValueBox({
    confidences <- sapply(1:nrow(records()), function(i) {
      calculate_confidence(records()[i, ])
    })
    avg_conf <- ifelse(length(confidences) > 0, round(mean(confidences), 1), 0)
    valueBox(
      value = paste0(avg_conf, "%"),
      subtitle = "Riesgo de sobrevivencia",
      icon = icon("percent"),
      color = if(avg_conf >= 70) "green" else "orange"
    )
  })
  
  output$high_confidence_pct <- renderValueBox({
    confidences <- sapply(1:nrow(records()), function(i) {
      calculate_confidence(records()[i, ])
    })
    high <- sum(confidences >= 70, na.rm = TRUE)
    total <- length(confidences)
    pct <- ifelse(total > 0, round(high/total * 100, 1), 0)
    valueBox(
      value = paste0(pct, "%"),
      subtitle = "Riesgo de propagacion",
      icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$peak_R0_temp <- renderValueBox({
    # Find temperature where R0 is maximum
    max_R0 <- max(ilcym_smooth$R0)
    peak_temp <- ilcym_smooth$temp[which.max(ilcym_smooth$R0)]
    valueBox(
      value = paste0(round(peak_temp, 1), "°C"),
      subtitle = "Temp. óptima R₀ máxima",
      icon = icon("thermometer-half"),
      color = "red"
    )
  })
  
}