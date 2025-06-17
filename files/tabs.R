# DIVECOFOR 1.0
# Copyright (C) 2023 [INIFAP]
# Maintainer: Ernesto Rubio (ernestorub@gmail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.
options(encoding = "UTF-8")
ui_tab6 <- function() {
  tabPanel(
    "Ayuda",
    fluidPage(
      mainPanel(

        tabsetPanel(
          # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
          tabPanel("Información Importante", 
                   tags$iframe(style="height:500px; width:100%; scrolling=yes", 
                               src="LEEME.md")),
          tabPanel("Manual de usuario", 
                   tags$iframe(style="height:600px; width:100%; scrolling=yes", 
                               src="MANUAL_DE_USUARIO_DIVECOFOR.pdf")),

        )
      )
    )
  )
}


ui_tab5 <- function() {
  tabPanel(
    # titlePanel("My Shiny App"),
    "Inicio",
    fluidPage(
      mainPanel(
        uiOutput("display_table_or_image2")
      )
    )
  )
}

ui_tab1 <- function() {
  tabPanel(
    "Estimación del IVI",
    fluidPage(
      tags$head(
        tags$style(HTML('
        #sidebarLayout {
            background-color: #f6f3ea;
        }
      .math-text {
        font-style: italic;
        font-family: "Latin Modern", Modern, modern; /* You can choose a math-like font here */
      }
    '))
      ),
      titlePanel("Valores de Importancia (IVI)"),
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Seleccionar un archivo CSV"),
          selectInput("column1", label = "Seleccionar la columna de especie:", choices = ""),
          selectInput("column2", label = HTML('Área Basal o de Área de Copa <span class="math-text">(m<sup>2</sup>)</span>:'), choices = ""),
          selectInput("column3", label = "Sitio de muestreo:", choices = ""),
          numericInput("a", label = HTML('Ingresar el área por sitio <span class="math-text">(m<sup>2</sup>)</span>:'), value = 10000),
          actionButton("create_dataframe", "Calcular"),
          downloadButton("download_csv", "Exportar a CSV"),
          downloadButton("download_excel", "Exportar a Excel")
        ),
        mainPanel(
          uiOutput("display_table_or_image"),
          textOutput(HTML("status_message"))
        )
      )
    )
  )
}

ui_tab2 <- function() {
  tabPanel(
    "Gráficos IVI",
    fluidPage(
      h2("Gráficos a partir del IVI"),
      hr(),
      fluidRow(
        column(3,
          style = "background-color:#f6f3ea;",
          h4("Seleccionar columnas y variales"),
          selectInput("plot_column", label = "Seleccionar Columna:", choices = NULL),
          selectInput("plot_type", "Tipo de Gráfico",
            choices = c("Barras" = "bar", "Puntos" = "points")
          ),
          textInput(inputId = "y_name", label = "Etiqueta en y"),
          textInput(inputId = "x_name", label = "Etiqueta en x"),
          selectInput("theme", "Seleccionar tema",
            choices = c("Blanco y Negro" = "bn", "Clásico" = "cl", "Gris" = "gr")
          ),
        ),
        column(
          5,
          # h4("Gráfico"),
          plotOutput("plot"),
        ),
        column(3,
          style = "background-color:#f6f3ea;",
          offset = 1,
          # offset = 0.5,
          h4("Ajustar valores para exportar"),
          colourInput("col1", "Color", "#4D484AF5", allowTransparent = T),
          numericInput("plot_width", "Ancho (cm):", value = 10),
          numericInput("plot_height", "Alto (cm):", value = 9),
          numericInput("plot_resolution", "Resolución (DPI):", value = 300),
          downloadButton("export_plot", "Exportar (PNG)")
        )
      )
    )
  )
}

ui_tab3 <- function() {
  tabPanel(
    "Índices",
    fluidPage(
      h2("Índices de diversidad por sitio de muestreo"),
      mainPanel(
        uiOutput("tabla_diversidad"),
        textOutput("status_message_diversidad"),
        downloadButton("DIV_download_csv", "Exportar a CSV"),
        downloadButton("DIV_download_excel", "Exportar a Excel")
        
      )
    )
  )
}


ui_tab4 <- function() {
  tabPanel(
    "Estructura forestal",
    fluidPage(
      h2("Estructura forestal por sitio de muestreo"),
      sidebarLayout(
        sidebarPanel(
          # selectInput("column4", label = "Seleccionar la columna de especie:", choices = ""),
          selectInput("column5", label = HTML('Seleccionar la columna de diámetro (<span class="math-text">d, cm</span>):'), choices = ""),
          selectInput("column6", label = HTML('Seleccionar la columna de altura (<span class="math-text">h, m</span>):'), choices = ""),
          selectInput("column7", label = HTML('Seleccionar la columna de área de copa <span class="math-text">(m<sup>2</sup>)</span>:'), choices = ""),
          selectInput("column8", label = "Sitio de muestreo:", choices = ""),
          numericInput("a2", label = HTML('Ingresar el área por sitio <span class="math-text">(m<sup>2</sup>)</span>:'), value = 10000),
          actionButton("EST_create_dataframe", "Calcular"),
          downloadButton("EST_download_csv", "Exportar a CSV"),
          downloadButton("EST_download_excel", "Exportar a Excel")
        ),
        mainPanel(
          uiOutput("tabla_estructura"),
          textOutput("status_message_estructura")
        )
      )
    )
  )
}
