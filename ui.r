# ui.R
library(shinydashboard)
library(leaflet)
library(plotly)

header <- dashboardHeader(
  title = tags$div(
    # tags$img(src = "https://via.placeholder.com/40x40?text=SENASA", height = 30, style = "margin-right: 5px;"),
    # tags$img(src = "https://via.placeholder.com/40x40?text=INIA", height = 30, style = "margin-right: 5px;"),
    # tags$img(src = "https://via.placeholder.com/40x40?text=CIP", height = 30, style = "margin-right: 10px;"),
    tags$img(src="images/senasa.png", height = 30, style = "margin-right: 5px;"),
    tags$img(src="images/inia.png", height = 30, style = "margin-right: 5px;"),
    tags$img(src="images/cip.png", height = 30, style = "margin-right: 10px;"),
    "MIPSmart - Gestión Integrada de Plagas"
    
    #  tags$img(src="images/senasa.png",height = 400, width = 600,alt="something went wrong",deleteFile=FALSE)
  ),
  titleWidth = 700
)

sidebar <- dashboardSidebar(
  width = 280,
  sidebarMenu(
    id = "tabs",
    menuItem("Vigilancia y Monitoreo", tabName = "surveillance", icon = icon("map-marked-alt")),
    menuItem("Registro Participativo", tabName = "participatory", icon = icon("users")),
    menuItem("Manejo Integrado (IPM)", tabName = "ipm", icon = icon("leaf")),
    menuItem("Indicadores ILCYM", tabName = "ilcym", icon = icon("chart-line"))
  ),
  hr(),
  conditionalPanel(
    condition = "input.tabs == 'surveillance'",
    h5("📍 Seleccione un punto en el mapa", style = "padding-left: 15px; color: #2c3e50;")
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f9; }
      .small-box { border-radius: 10px; }
      .info-box { border-radius: 8px; }
      .leaflet-container { border-radius: 12px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); }
      .btn-primary { background-color: #2c7a4a; border-color: #1e5a35; }
      .btn-primary:hover { background-color: #1e5a35; }
    "))
  ),
  tabItems(
    # ============================================
    # TAB 1: SURVEILLANCE & MONITORING
    # ============================================
    tabItem(
      tabName = "surveillance",
      fluidRow(
        box(
          title = "Mapa de Trampas - Bactericera cockerelli",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = "600px",
          leafletOutput("trap_map", height = "550px")
        )
      ),
      fluidRow(
        box(
          title = "Conteo de Insectos en Trampa Seleccionada",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("count_graph", height = "300px")
        )
      )
    ),
    
    # ============================================
    # TAB 2: PARTICIPATORY SYSTEM
    # ============================================
    tabItem(
      tabName = "participatory",
      fluidRow(
        box(
          title = "📋 Registro Participativo de Plagas",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          h5("Registro rápido para agricultores", style = "color: #856404;"),
          hr(),
          textInput("farmer_name", "Nombre del Productor", value = "", placeholder = "Ej: Juan Pérez"),
          selectInput("host_plant", "Planta Hospedera", 
                      choices = c("Papa", "Tomate", "Pimiento", "Berengena", "Otro")),
          numericInput("est_count", "Conteo estimado de B. cockerelli", value = 0, min = 0, max = 100),
          radioButtons("pest_present", "¿Presencia de la plaga?", choices = c("Sí" = "yes", "No" = "no"), inline = TRUE),
          fileInput("photo_evidence", "Subir evidencia fotográfica", accept = c("image/png", "image/jpeg")),
          actionButton("submit_record", "✅ Registrar", class = "btn-primary", icon = icon("check")),
          br(), br(),
          h5("🌤️ Condiciones automáticas del clima", style = "color: #0c5460;"),
          verbatimTextOutput("weather_info")
        ),
        box(
          title = "📊 Indicadores de Identificación",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          valueBoxOutput("confidence_score", width = 12),
          valueBoxOutput("records_today", width = 6),
          valueBoxOutput("positive_rate", width = 6),
          br(),
          DTOutput("records_table")
        )
      )
    ),
    
    # ============================================
    # TAB 3: IPM MODULE
    # ============================================
    tabItem(
      tabName = "ipm",
      h3("Manejo Integrado de Plagas - Bactericera cockerelli", style = "color: #1a5e2a;"),
      fluidRow(
        box(
          title = "🦟 Control Biológico",
          status = "success",
          solidHeader = TRUE,
          width = 4,
          DTOutput("bio_table")
        ),
        box(
          title = "🧪 Control Químico",
          status = "danger",
          solidHeader = TRUE,
          width = 4,
          DTOutput("chem_table")
        ),
        box(
          title = "🌱 Producción de Semilla",
          status = "info",
          solidHeader = TRUE,
          width = 4,
          DTOutput("cultural_table")
        )
      ),
      fluidRow(
        box(
          title = "📍 Recomendaciones para la ubicación seleccionada",
          status = "primary",
          width = 12,
          htmlOutput("ipm_location_recommendation")
        )
      )
    ),
    
    # ============================================
    # TAB 4: ILCYM INDICATORS
    # ============================================
    tabItem(
      tabName = "ilcym",
      fluidRow(
        box(
          title = "📈 Tasa Neta de Reproducción (R₀) - ILCYM",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("ilcym_graph", height = "400px"),
          br(),
          h5("Interpretación:"),
          p("La Tasa Neta de Reproducción (R₀) indica el número de hembras que produce una hembra durante su vida. 
            Valores > 1 indican crecimiento poblacional. El pico óptimo de temperatura para B. cockerelli es de ~24-26°C.")
        )
      ),
      fluidRow(
        box(
          title = "Indicadores de Correcta Identificación",
          status = "info",
          width = 12,
          valueBoxOutput("total_records_ilcym", width = 3),
          valueBoxOutput("avg_confidence", width = 3),
          valueBoxOutput("high_confidence_pct", width = 3),
          valueBoxOutput("peak_R0_temp", width = 3)
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green")