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
options(save.workspace = "no")
source("files/funciones.R")
source("files/tabs.R")

########################################################
####################### ui #############################
########################################################

ui <- navbarPage(
  title = "DIVECOFOR",
  theme = shinytheme("sandstone"),
  ui_tab5(),
  ui_tab1(),
  ui_tab2(),
  ui_tab3(),
  ui_tab4(),
  ui_tab6()

)

########################################################
####################### server #########################
########################################################
server <- function(input, output, session) {
  observeEvent(input$cerrar, {
    stopApp()
  })
  
  welcome_image_path <- "www/welcome_image.png"
  welcome_image_path2 <- "www/plot-min.png"
  
  output$welcome_image <- renderImage(
    {
      list(
        src = welcome_image_path,
        width = "100%"
      ) # Adjust the width as needed
    },
    deleteFile = FALSE
  )
  
  output$display_table_or_image <- renderUI({
    if (input$create_dataframe > 0) {
      tableOutput("selected_data") 
    } else {
      imageOutput("welcome_image") 
    }
  })
  
  output$welcome_image2 <- renderImage(
    {
      list(
        src = welcome_image_path2,
        width = "100%"
      ) 
    },
    deleteFile = FALSE
  )
  
  output$display_table_or_image2 <- renderUI({

      imageOutput("welcome_image2") 
  })

  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    columns <- names(data())
    updateSelectInput(session, "column1", choices = columns)
    updateSelectInput(session, "column2", choices = columns)
    updateSelectInput(session, "column3", choices = columns)
    updateSelectInput(session, "column4", choices = columns)
    updateSelectInput(session, "column5", choices = columns)
    updateSelectInput(session, "column6", choices = columns)
    updateSelectInput(session, "column7", choices = columns)
    updateSelectInput(session, "column8", choices = columns)
  })

  # Define selected data for IVI tab
  selected_data <- eventReactive(input$create_dataframe, {
    req(input$file, input$column1, input$column2, input$column3)
    selected_cols <- c(input$column1, input$column2, input$column3)
    data_subset <- subset(data(), select = selected_cols)
    return(data_subset)
  })

  # Define selected data for Estructura forestal tab
  selected_data_estructura <- eventReactive(input$EST_create_dataframe, {
    req(input$file, input$column5, input$column6, input$column7, input$column8)
    selected_cols <- c(input$column5, input$column6, input$column7, input$column8)
    data_subset <- subset(data(), select = selected_cols)
    return(data_subset)
  })


  # Create a reactive expression to receive the result
  ivi_result <- eventReactive(input$create_dataframe, {
    req(input$file, input$column1, input$column2, input$column3)
    selected_cols <- c(input$column1, input$column2, input$column3)
    data_subset <- subset(data(), select = selected_cols)
    colnames(data_subset) <- c("x", "y", "z")
    result <- IVI_index_table(x, y, z, data = data_subset, a = input$a)
    return(result)
  })

  ########################################################
  ####################### IVI ############################
  ########################################################
  output$selected_data <- renderTable({
    if (input$create_dataframe > 0) {
      new_data <- selected_data()
      colnames(new_data) <- c("x", "y", "z")
      result <- IVI_index_table(x, y, z, data = new_data, a = input$a)
      return(result)
    }
  })

  # Download as CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("IVI_result.csv", sep = "")
    },
    content = function(file) {
      if (input$create_dataframe > 0) {
        new_data <- selected_data()
        colnames(new_data) <- c("x", "y", "z")
        result <- IVI_index_table(x, y, z, data = new_data, a = input$a)
        write.csv(result, file, row.names = FALSE)
      }
    }
  )

  # Download as Excel
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("IVI_result.xlsx", sep = "")
    },
    content = function(file) {
      if (input$create_dataframe > 0) {
        new_data <- selected_data()
        colnames(new_data) <- c("x", "y", "z")
        result <- IVI_index_table(x, y, z, data = new_data, a = input$a)
        write.xlsx(result, file, row.names = FALSE)
      }
    }
  )

  # Display a status message
  output$status_message <- renderText({
    if (input$create_dataframe > 0) {
      "Nota: La columna Xha corresponde a la variable ingresada en área basal o de copa."
    } else {
      ""
    }
  })

  ########################################################
  ################ Diversidad ############################
  ########################################################
  output$tabla_diversidad <- renderTable({
    if (input$create_dataframe > 0) {
      new_data <- selected_data()
      colnames(new_data) <- c("x", "y", "z")
      result <- diversidad(x, y, z, data = new_data, a = input$a)
      colnames(result)[1] <- "Sitio"
      return(result)
    }
  })

  # Download as Excel
  output$DIV_download_excel <- downloadHandler(
    filename = function() {
      paste("DIV_result.xlsx", sep = "")
    },
    content = function(file) {
      if (input$create_dataframe > 0) {
        new_data <- selected_data()
        colnames(new_data) <- c("x", "y", "z")
        result <- diversidad(x, y, z, data = new_data, a = input$a)
        write.xlsx(result, file, row.names = FALSE)
      }
    }
  )

  # Download as CSV
  output$DIV_download_csv <- downloadHandler(
    filename = function() {
      paste("DIV_result.csv", sep = "")
    },
    content = function(file) {
      if (input$create_dataframe > 0) {
        new_data <- selected_data()
        colnames(new_data) <- c("x", "y", "z")
        result <- diversidad(x, y, z, data = new_data, a = input$a)
        write.csv(result, file, row.names = FALSE)
      }
    }
  )
  
  
  
  ########################################################
  ################ ESTRUCTURA ############################
  ########################################################
  output$tabla_estructura <- renderTable({
    if (input$EST_create_dataframe > 0) {
      new_data <- selected_data_estructura()
      colnames(new_data) <- c("y", "h", "dc", "z")
      result <- estructura(y = y, h = h, dc = dc, z = z, data = new_data, a = input$a2)
      colnames(result)[1] <- "Sitio"
      return(result)
    }
  })

  # Download as Excel
  output$EST_download_excel <- downloadHandler(
    filename = function() {
      paste("EST_result.xlsx", sep = "")
    },
    content = function(file) {
      if (input$EST_create_dataframe > 0) {
        new_data <- selected_data_estructura()
        colnames(new_data) <- c( "y", "h", "dc", "z")
        result <- estructura(y = y, h = h, dc = dc, z = z, data = new_data, a = input$a2)
        write.xlsx(result, file, row.names = FALSE)
      }
    }
  )

  # Download as CSV
  output$EST_download_csv <- downloadHandler(
    filename = function() {
      paste("EST_result.csv", sep = "")
    },
    content = function(file) {
      if (input$EST_create_dataframe > 0) {
        new_data <- selected_data_estructura()
        colnames(new_data) <- c( "y", "h", "dc", "z")
        result <- estructura(y = y, h = h, dc = dc, z = z, data = new_data, a = input$a2)
        write.csv(result, file, row.names = FALSE)
      }
    }
  )

  output$status_message_estructura <- renderText({
    if (input$EST_create_dataframe > 0) {
      "Nota: Se presentan las variables de estructura forestal. Donde: Nha = número de árboles por hectárea; 
      Gha = área basal por hectárea; dm	= diámetro normal promedio; d_sd = desviación estádar del diámetro;
      hm = altura promedio;	h_sd = Desviación estádar de la altura; dg = diámetro médio cuadrático; 
      h_dom = altura dominante (promedio de altura de los 100 árboles más grandes en una hectárea); 
      d_dom = diámetro dominante; ACha = área de copa por hectárea en metros cuadrados"
    } else {
      ""
    }
  })

  
########################################################
####################### Gráficos #######################
########################################################

  available_columns <- reactive({
    if (!is.null(ivi_result())) {
      names(ivi_result())
    } else {
      character(0)
    }
  })

  observe({
    updateSelectInput(session, "plot_column", choices = available_columns())
  })

  selected_data_plot <- reactive({
    if (!is.null(ivi_result())) {
      selected_col <- input$plot_column
      # selected_col2 <- input$plot_column2
      data.frame(x = ivi_result()[, selected_col], y = ivi_result()[, 1])
    } else {
      data.frame(x = numeric(0), y = numeric(0))
    }
  })

  output$plot <- renderPlot({
    data_sorted <- selected_data_plot()

    data_sorted$y <- factor(data_sorted$y, 
      levels = data_sorted$y[order(data_sorted$x, decreasing = F)]
    )

    p <- ggplot(data_sorted, aes(x = x, y = y)) +
      ylab(input$y_name) +
      xlab(input$x_name)

    if (input$theme == "bn") {
      p <- p + theme_bw() %+replace% theme(axis.text.y = element_text(face = "italic", hjust = 1, margin = margin(5, 5, 5, 5)))
    } else if (input$theme == "cl") {
      p <- p + theme_classic() %+replace% theme(axis.text.y = element_text(face = "italic", hjust = 1, margin = margin(5, 5, 5, 5)))
    } else if (input$theme == "gr") {
      p <- p + theme_gray() %+replace% theme(axis.text.y = element_text(face = "italic", hjust = 1, margin = margin(5, 5, 5, 5)))
    }

    if (input$plot_type == "points") {
      p <- p + geom_point(color = input$col1)
    } else if (input$plot_type == "bar") {
      p <- p + geom_bar(stat = "identity",fill = input$col1)
    }

    return(p)
  })

  output$export_plot <- downloadHandler(
    filename = function() {
      paste("plot_export.png", sep = "")
    },
    content = function(file) {
      data_sorted <- selected_data_plot()
      data_sorted$y <- factor(data_sorted$y, # Factor levels in decreasing order
        levels = data_sorted$y[order(data_sorted$x, decreasing = F)]
      )
      p <- ggplot(data_sorted, aes(x = x, y = y)) +
        ylab(input$y_name) +
        xlab(input$x_name)

      if (input$theme == "bn") {
        p <- p + theme_bw() %+replace% theme(axis.text.y = element_text(face = "italic", hjust = 1, margin = margin(5, 5, 5, 5)))
      } else if (input$theme == "cl") {
        p <- p + theme_classic() %+replace% theme(axis.text.y = element_text(face = "italic", hjust = 1, margin = margin(5, 5, 5, 5)))
      } else if (input$theme == "gr") {
        p <- p + theme_gray() %+replace% theme(axis.text.y = element_text(face = "italic", hjust = 1, margin = margin(5, 5, 5, 5)))
      }

      if (input$plot_type == "points") {
        p <- p + geom_point(color = input$col1)
      } else if (input$plot_type == "bar") {
        p <- p + geom_bar(stat = "identity",fill = input$col1)
      }

      tmp_file <- tempfile(fileext = ".png")

      ggsave(tmp_file, plot = p, units = "cm", width = input$plot_width, height = input$plot_height, dpi = input$plot_resolution)

      file.copy(tmp_file, file)

      unlink(tmp_file)
    }
  )

  
  # session$onSessionEnded(function() {
  #   stopApp()
  # })
  
  # session$onSessionEnded(function() {
  #   # Cierra Chrome Portable cuando se cierre la app
  #   system("taskkill /F /IM GoogleChromePortable.exe", intern = FALSE)
  #   stopApp()
  # })

}


shinyApp(ui, server)
