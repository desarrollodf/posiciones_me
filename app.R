# Librerías

library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(rsconnect)
library(RColorBrewer)
library(readxl)
library(shinyBS)

series_plazos <- c(
  "Hasta 35 días", "36-185 días", "186 días o más"
  )
plazos <- read_xlsx("datos.xlsx") %>%
  mutate(Serie = factor(Serie, levels = series_plazos))
plazos_neto <- plazos %>%
  group_by(Fecha) %>%
  reframe(Valor=sum(Valor), Serie="Neto")

# Shiny

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    
    body, .container-fluid, .main-panel {
      background-color: transparent !important;
      margin: 0 !important;
      padding-bottom: 0 !important;
    }
  
  "))
  ),
  
  tags$h4(
    "Posiciones en Dólares",
    tags$span(
      icon("question-circle"), 
      id = "info_icon", 
      style = "cursor: pointer; color: #007BFF;"
    )
  ),
  bsTooltip(
    id = "info_icon",
    title = paste(
      "Los valores sobre cero reflejan posiciones que se inclinan hacia un alza",
      "del precio del dólar en Chile, mientras que bajo cero apuntan a una caída.<br><br>",
      "Los no residentes representan en Chile una parte significativa de las posiciones abiertas en forwards,",
      "y suelen mostrar tendencias especulativas, además de cubrir riesgos de inversiones en activos chilenos."
    ),
    placement = "right",
    trigger = "click"
  ),
  
  tags$p("Forwards de extranjeros con bancos locales."),
  tags$p("Por: Benjamín Pescio", style = "font-size: 12px; margin: 1;"),
  
  absolutePanel(
    id = "panel-periodo",
    top = 10, right = 10, fixed = TRUE, draggable = FALSE,
    shinyWidgets::radioGroupButtons(
      inputId = "periodo",
      label = NULL,
      choices = c(
        "12 meses" = "1",
        "10 años" = "10"
      ),
      selected = "1",
      size = "xs",
      direction = "vertical",
      status = "default"
    )
  ),
  
  fluidRow(
    column(width = 12,
           div(
             id = "grafico-container",
             class = "fade-in",
             style = "position: relative; left: -10px;",
             plotlyOutput("grafico", height = "300px", width = "100%")
           ),
           tags$div(
             style = "display: flex; justify-content: space-between; align-items: center;
                    margin-top: -10px;",
             tags$div(
               style = "font-size: 11px; color: black; display: flex; flex-direction: column; align-items: flex-start;",
               tags$img(src = "icono_flecha.svg", height = "30px", style = "margin-bottom: 2px;"),
               "Fuente: Banco Central"
             ),
             tags$img(src = "footer.png", height = "35px")
           )
    )
  )
)

# ---------------------------------------------------------
# Cálculos estáticos
# ---------------------------------------------------------

mediana_nores <- median(plazos_neto$Valor, na.rm = TRUE)

colores <- setNames(
  colorRampPalette(c("#D73535", "#FFCDC9"))(length(series_plazos)),
  names(series_plazos)
)

# Todas las series en todas las fechas
# Separamos valores positivos y negativos para apilarlos correctamente
plazos_apilados <- plazos %>%
  complete(Fecha, Serie, fill = list(Valor = 0)) %>%
  mutate(
    Pos = pmax(Valor, 0),
    Neg = pmin(Valor, 0)
  )

server <- function(input, output, session) {
  
  output$grafico <- renderPlotly({
    
    ancho_linea = case_when(
      input$periodo == "1" ~ 2,
      TRUE ~ 1
    )
    
    # ---------------------------------------------------------
    # Filtrar período
    # ---------------------------------------------------------
    
    fecha_min <- max(plazos_apilados$Fecha, na.rm = TRUE) - years(input$periodo)
    
    filtrado <- plazos_apilados %>%
      filter(Fecha >= fecha_min)
    
    neto_filtrado <- plazos_neto %>%
      filter(Fecha >= fecha_min)
    
    
    # ---------------------------------------------------------
    # Función para áreas apiladas
    # ---------------------------------------------------------
    
    area <- function(p, y, grupo, mostrar_leyenda = TRUE) {
      
      add_trace(
        p,
        x = ~Fecha,
        y = y,
        type = "scatter",
        mode = "lines",
        
        color = ~Serie,
        colors = colores,
        
        legendgroup = ~Serie,
        stackgroup = grupo,
        
        line = list(width = 0),
        
        showlegend = mostrar_leyenda,
        
        # Valor original, con signo
        customdata = ~Valor,
        
        hovertemplate = if (mostrar_leyenda) {
          paste0(
            "%{fullData.name}<br>",
            "%{customdata:,.0f}",
            "<extra></extra>"
          )
        } else {
          NULL
        },
        
        hoverinfo = if (mostrar_leyenda) NULL else "skip"
      )
    }
    
    
    # ---------------------------------------------------------
    # Gráfico
    # ---------------------------------------------------------
    
    grafico <- plot_ly(filtrado) %>%
      
      # Valores positivos
      area(
        y = ~Pos,
        grupo = "positivo",
        mostrar_leyenda = TRUE
      ) %>%
      
      # Valores negativos
      area(
        y = ~Neg,
        grupo = "negativo",
        mostrar_leyenda = FALSE
      ) %>%
      
      # Línea neta
      add_trace(
        data = neto_filtrado,
        x = ~Fecha,
        y = ~Valor,
        type = "scatter",
        mode = "lines",
        name = "Neto",
        line = list(
          color = "black",
          width = ancho_linea
        ),
        hovertemplate = paste0(
          "Neto<br>",
          "%{y:,.0f}",
          "<extra></extra>"
        ),
        inherit = FALSE
      )
    
    
    # ---------------------------------------------------------
    # Layout
    # ---------------------------------------------------------
    
    grafico %>%
      plotly::layout(
        
        dragmode = FALSE,
        hovermode = "x unified",
        
        xaxis = list(
          title = "",
          hoverformat = "%d-%m-%Y"
        ),
        
        yaxis = list(
          title = "Millones de dólares (US$)",
          tickformat = ",.0f"
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        
        # Mediana histórica
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0,
            x1 = 1,
            yref = "y",
            y0 = mediana_nores,
            y1 = mediana_nores,
            line = list(
              width = 1.5,
              dash = "dash",
              color = "black"
            )
          )
        ),
        
        annotations = list(
          list(
            xref = "paper",
            x = 0.5,
            xanchor = "right",
            yref = "y",
            y = mediana_nores,
            yshift = 8,
            text = "Mediana histórica",
            showarrow = FALSE,
            font = list(size = 10)
          )
        ),
        
        legend = list(
          orientation = "h",
          x = -0.04, y = 1.24,
          xanchor = "left",
          font = list(size = 10)
        )
      ) %>%
      
      plotly::config(
        displayModeBar = FALSE,
        displaylogo = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE,
        responsive = TRUE,
        locale = "es"
      )
  })
}

shinyApp(ui, server)