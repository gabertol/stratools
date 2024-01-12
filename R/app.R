#' log_plot
#'
#' @description
#' log_plot is a wrap-up function to plot a single sedime
#'        Name of the column in data for sub-facies. Default is "sub".
#' @param gs_less_size
#'        Value used for grain size calculations. Default is 5.
#' @example
#'
#'

app<-function(){

library(shiny)

  ui <- fluidPage(
    titlePanel("Plotar Corr_Plot a partir de um CSV"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Selecione um arquivo CSV:",
                  accept = c(".csv")
        ),
        checkboxInput("legend", "Mostrar Legenda", value = TRUE),

        # Opções para selecionar colunas
        selectInput("sample_col", "Coluna de Amostra:", ""),
        selectInput("bed_thick_col", "Coluna de Espessura:", ""),
        selectInput("bed_id_col", "Coluna de ID da Camada:", ""),
        selectInput("phi_col", "Coluna de Phi:", ""),
        selectInput("facies1_col", "Coluna de Facies 1:", ""),
        selectInput("facies2_col", "Coluna de Facies 2:", ""),
        selectInput("color_col", "Coluna de Cor:", ""),

        # Botões lógicos
        checkboxInput("grainsize_checkbox", "Ativar Tamanho de Grãos", value = TRUE),
        checkboxInput("x_scale_checkbox", "Ativar Escala X", value = TRUE),

        # Botão para atualizar o plot
        actionButton("update_button", "Atualizar Plot"),

        # Botões para exportar em diferentes formatos
        downloadButton("download_png", "Exportar PNG"),
        downloadButton("download_pdf", "Exportar PDF"),
        downloadButton("download_eps", "Exportar EPS")
      ),
      mainPanel(
        plotOutput("corr_plot")
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    data <- reactive({
      req(input$file)
      read.csv(input$file$datapath)
    })

    observe({
      choices <- colnames(data())
      updateSelectInput(session, "sample_col", choices = choices)
      updateSelectInput(session, "bed_thick_col", choices = choices)
      updateSelectInput(session, "bed_id_col", choices = choices)
      updateSelectInput(session, "phi_col", choices = choices)
      updateSelectInput(session, "facies1_col", choices = choices)
      updateSelectInput(session, "facies2_col", choices = choices)
      updateSelectInput(session, "color_col", choices = choices)
    })

    output$corr_plot <- renderPlot({
      corr_plot(
        dataframe = data(),
        show_legend = input$legend,
        direction = input$direction,
        color_name = input$color_col,
        labels_facies_name = input$facies2_col,
        grainsize = input$grainsize_checkbox,
        x_scale = input$x_scale_checkbox
      )
    })

    output$download_png <- downloadHandler(
      filename = function() {
        paste("corr_plot", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file)
        # Certifique-se de que os argumentos aqui correspondam ao seu gráfico
        corr_plot(
          dataframe = data(),
          show_legend = input$legend,
          direction = input$direction,
          color_name = input$color_col,
          labels_facies_name = input$facies2_col,
          grainsize = input$grainsize_checkbox,
          x_scale = input$x_scale_checkbox
        )
      },
      contentType = "image/png"  # Especifica o tipo de conteúdo
    )

    # Função para exportar o gráfico em PDF
    output$download_pdf <- downloadHandler(
      filename = function() {
        sprintf("corr_plot_%s.pdf", Sys.Date())
      },
      content = function(file) {
        pdf(file)
        corr_plot(
          dataframe = data(),
          show_legend = input$legend,
          direction = input$direction,
          color_name = input$color_col,
          labels_facies_name = input$facies2_col,
          grainsize = input$grainsize_checkbox,
          x_scale = input$x_scale_checkbox
        )
      },
      contentType = "application/pdf"  # Especifica o tipo de conteúdo
    )

    # Função para exportar o gráfico em EPS
    output$download_eps <- downloadHandler(
      filename = function() {
        sprintf("corr_plot_%s.eps", Sys.Date())
      },
      content = function(file) {
        postscript(file)
        corr_plot(
          dataframe = data(),
          show_legend = input$legend,
          direction = input$direction,
          color_name = input$color_col,
          labels_facies_name = input$facies2_col,
          grainsize = input$grainsize_checkbox,
          x_scale = input$x_scale_checkbox
        )
      },
      contentType = "application/postscript"  # Especifica o tipo de conteúdo
    )
  }

  # Crie e inicie o aplicativo Shiny
  shinyApp(ui, server)


}
