#### UI Side ####
ui <- fluidPage(
  titlePanel(
    title = em(strong('Beispiel für die Übersicht in Projekt 7')),
    windowTitle = 'Beispiel für die Übersicht in Projekt 7'),
  hr(),
  plotOutput('Histogramm'),
  hr(),
  wellPanel(
    sliderInput(inputId = 'Stichprobengroesse',
      label = 'Stichprobengröße',
      value = 1000, min = 100, max = 10000, step = 100),
    sliderInput(inputId = 'Breaks',
      label = 'Balken im Histogramm',
      value = 10, min = 2, max = 100),
    fluidRow(
      column(6, numericInput(inputId = 'Mittelwert',
        label = 'Mittelwert',
        value = 100, min = -1000, max = 1000)),
      column(6, selectInput(inputId = 'Mass',
        label = 'Maß der zentralen Tendenz (Stichprobe)',
        choices = c('Kein Maß', 'Median', 'Mittelwert')))
    ),
    fluidRow(
      column(6, numericInput(inputId = 'Standardabweichung',
        label = 'Standardabweichung',
        value = 15, min = 0, max = 1000)),
      column(3, radioButtons(inputId = 'Normalverteilung',
        label = 'Normalverteilungskurve einzeichnen',
        inline = TRUE,
        choices = c('Ja', 'Nein'),
        selected = 'Nein')),
      column(3,
        br(),
        actionButton(inputId = 'Update',
          label = 'Histogramm aktualisieren'))
    )
  ),
  hr()
)

#### Server Side ####
server <- function(input, output, session) {
  rv <- reactiveValues(data = rnorm(1000, mean = 100, sd = 15))
  observeEvent(input$Update, {
    rv$data <- rnorm(input$Stichprobengroesse,
      mean = input$Mittelwert,
      sd = input$Standardabweichung)
  })
  output$Histogramm <- renderPlot({
    zentraleTendenz <- input$Mass
    Normalverteilungskurve <- input$Normalverteilung
    hist(rv$data, breaks = input$Breaks, freq = FALSE)
    if(Normalverteilungskurve == 'Ja'){
      curve(
        dnorm(x,
          mean = isolate(input$Mittelwert),
          sd = isolate(input$Standardabweichung)),
        add = TRUE, lwd = 3)
    }
    if(zentraleTendenz == 'Median'){
      abline(v = median(rv$data), lwd = 1, col = 'orange')
    } else if (zentraleTendenz == 'Mittelwert'){
      abline(v = mean(rv$data), lwd = 1, col = 'red')
    } else {}
  })
}

shinyApp(ui, server)
