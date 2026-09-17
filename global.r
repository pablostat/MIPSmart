# global.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(jsonlite)  # For weather API simulation

# Set seed for reproducibility
set.seed(123)

# ==========================================
# 1. BRANDING & IMAGERY (SENASA, INIA, CIP)
# ==========================================
# In a real app, place logos in /www/ folder.
# For this demo, we use HTML/CSS with text badges.

# ==========================================
# 2. SIMULATED DATA
# ==========================================


# Trap locations (Bactericera cockerelli monitoring)
trap_locations <- data.frame(
  id = 1:8,
  lat = c(-12.046, -12.100, -12.200, -12.300, -11.950, -12.150, -12.250, -12.350),
  lng = c(-67.042, -67.150, -67.080, -67.200, -67.300, -67.050, -67.120, -67.250),
  location = c("La Molina", "Chaclacayo", "Cieneguilla", "Lurigancho", 
               "Santa Anita", "Ate", "Vitarte", "Huaycan"),
  altitude = c(240, 650, 350, 800, 200, 550, 450, 700)
)

# Generate trap count data for each location (dates)
generate_counts <- function(location_id, n_dates = 20) {
  dates <- seq.Date(Sys.Date() - n_dates*2, Sys.Date(), by = "2 days")[1:n_dates]
  data.frame(
    trap_id = location_id,
    date = dates,
    count = rpois(n_dates, lambda = sample(2:15, 1))
  )
}

trap_counts <- bind_rows(lapply(trap_locations$id, generate_counts))

# ==========================================
# 3. ILCYM LIFE PARAMETER (Net Reproduction Rate - R0)
# ==========================================
# Simulating ILCYM output based on temperature

ilcym_data <- data.frame(
  temperature = seq(10, 35, by = 1),
  R0 = c(0, 0.5, 2.5, 8, 18, 32, 45, 55, 60, 58, 48, 32, 18, 8, 2, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)
# Smooth using spline
ilcym_spline <- spline(ilcym_data$temperature, ilcym_data$R0, n = 100)
ilcym_smooth <- data.frame(temp = ilcym_spline$x, R0 = ilcym_spline$y)

# ==========================================
# 4. PARTICIPATORY RECORDS (Simulated)
# ==========================================

participatory_records <- data.frame(
  record_id = 1:15,
  date = Sys.Date() - sample(1:30, 15),
  farmer = paste("Productor", LETTERS[1:15]),
  host_plant = sample(c("Papa", "Tomate", "Pimiento", "Berengena"), 15, replace = TRUE),
  pest_present = sample(c("Sí", "No"), 15, replace = TRUE, prob = c(0.7, 0.3)),
  count_estimate = sample(1:20, 15, replace = TRUE),
  image_evidence = rep("evidence_placeholder.jpg", 15),
  latitude = trap_locations$lat[sample(1:8, 15, replace = TRUE)],
  longitude = trap_locations$lng[sample(1:8, 15, replace = TRUE)],
  altitude = trap_locations$altitude[sample(1:8, 15, replace = TRUE)],
  temperature = round(rnorm(15, 22, 3), 1),
  humidity = round(rnorm(15, 65, 10), 0),
  wind_speed = round(runif(15, 0.5, 5), 1)
)

# ==========================================
# 5. IPM RECOMMENDATIONS DATABASE
# ==========================================

ipm_data <- list(
  biological = data.frame(
    strategy = c("Feromonas (monitoreo)", "Feromonas (control masivo)", 
                 "Baculovirus (BT talco)", "Beauveria bassiana CCB LE-265",
                 "Paecilomyces lilacinus cepa 251", "Metarhizium anisopliae"),
    target = rep("B. cockerelli", 6),
    application = c("Trampas con septos", "Confusión de machos", "Suspensión acuosa",
                    "Aplicación foliar", "Suelo/raíz", "Foliar/suelo"),
    effectiveness = c(85, 70, 75, 80, 65, 78)
  ),
  chemical = data.frame(
    active_ingredient = c("Thiamethoxam + Lambda-cialotrina", "Abamectina", "Imidacloprid"),
    formulation = c("WG", "EC", "SC"),
    dose = c("0.25 kg/ha", "1 L/ha", "0.5 L/ha"),
    reentry_days = c(7, 3, 5)
  ),
  cultural = data.frame(
    practice = c("Cobertura vegetal", "Labranza mínima", "Manejo de residuos", "Fertilización orgánica"),
    description = c("Uso de leguminosas como cobertura", "Reducción de alteración del suelo",
                    "Incorporación de restos de cosecha", "Compost y estiércol bien descompuesto"),
    benefit = c("Hábitat para enemigos naturales", "Conservación de humedad", "Eliminación de inóculo", "Nutrición balanceada")
  )
)

# ==========================================
# 6. HELPER FUNCTIONS
# ==========================================

# Auto-fetch weather (simulated)
get_weather <- function(lat, lng) {
  # In production, use openweathermap or similar API
  list(
    temperature = round(rnorm(1, 22, 3), 1),
    humidity = round(rnorm(1, 65, 10), 0),
    pressure = round(rnorm(1, 1013, 5), 0),
    wind_speed = round(runif(1, 0.5, 5), 1)
  )
}

# Calculate identification confidence indicator
calculate_confidence <- function(record) {
  # Based on: count_estimate, host_plant match, image presence, etc.
  confidence <- 50
  if(record$count_estimate > 0) confidence <- confidence + 15
  if(record$host_plant %in% c("Papa", "Tomate")) confidence <- confidence + 20
  if(!is.na(record$image_evidence)) confidence <- confidence + 15
  return(min(confidence, 99))
}