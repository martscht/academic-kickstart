library(shiny)
ui <- fluidPage(
  theme = "bootstrap.css", style = "background: #337ab7; color: white",
  tags$head(tags$style(".navbar {background-color: #eded00;}",
                       ".navbar-default .navbar-brand {color: black;}")),
  titlePanel(
    h1(strong("Suchanfragen deutscher Parteien"), align = "center"),
      windowTitle = "Suchanfragen deutscher Parteien"),
  wellPanel(style = "background: #87CEFA; border-color: #2e6da4",
            navbarPage(
              strong("Darstellungsart"),
              tabPanel("Liniendiagramm", plotOutput("Liniendiagramm")),
              tabPanel("Liniendiagramm (kumuliert)", plotOutput("Liniendiagramm_kumuliert")),
              tabPanel("Balkendiagramm (kumuliert)", plotOutput("Balkendiagramm")),
              tabPanel("Kuchendiagramm (kumuliert)", plotOutput("Kuchendiagramm"))
            ),
            br(),
            fluidRow(
              column(6,
                     dateRangeInput(
                       inputId = "Datum", label = "Zeitspanne",
                       start = "2000-01-01", format = "yyyy-mm-dd")),
              column(6, textInput(
                inputId = "Titel", label = "Titel der Grafik"))
            ),
            fluidRow(
              column(9, checkboxGroupInput(
                inputId = "Parteien", label = "Parteien", inline = TRUE,
                choices = c("AfD", "SPD", "FDP", "DieGrüne", "DieLinke",
                            "Tierschutzpartei", "CDU", "MLPD", "NPD"))),
              column(3, br(), actionButton(
                inputId = "Update", label = strong("Diagramm erzeugen"),
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
            )
  )
)

server <- function(input, output, session) {
  # Paket laden
  library(ggplot2)

  # Daten einlesen
  p1_long <- readRDS('p1_long.rds')
  p1_wide <- readRDS('p1_wide.rds')

  # Monate formatieren
  p1_long$nMonat <- as.character(p1_long$Monat)
  p1_long$nMonat <- paste0(p1_long$nMonat, '-01')
  p1_long$nMonat <- strptime(p1_long$nMonat, format="%Y-%m-%d")
  p1_long$nMonat <- as.POSIXct(p1_long$nMonat)

  p1_wide$nMonat <- as.character(p1_wide$Monat)
  p1_wide$nMonat <- paste0(p1_wide$nMonat, '-01')
  p1_wide$nMonat <- strptime(p1_wide$nMonat, format="%Y-%m-%d")
  p1_wide$nMonat <- as.POSIXct(p1_wide$nMonat)

  # Farbkodierung der Parteien
  farben <- c('AfD' = 'deepskyblue', 'CDU' = 'black', 'DieGrüne' = 'green3',
              'DieLinke' = 'magenta', 'FDP' = 'gold', 'MLPD' = 'orange',
              'NPD' = 'brown', 'SPD' = 'red', 'Tierschutzpartei' = 'darkblue')

  # Kumulative Suchanfragen
  p1_long <- p1_long[order(p1_long$Partei), ]
  p1_long$Prozent_kumuliert <- unlist(tapply(p1_long$Prozent, p1_long$Partei, cumsum))

  # Datenauswahl (Zeitplots)
  rv <- reactiveValues(data = p1_long)
  observeEvent(input$Update, {
    rv$data <- p1_long[is.element(p1_long$Partei, input$Parteien)
                       & as.POSIXct(input$Datum[1]) < p1_long$nMonat
                       & as.POSIXct(input$Datum[2]) > p1_long$nMonat,]})

  # Liniendiagramm
  output$Liniendiagramm <- renderPlot(
    ggplot(data = rv$data, aes(x = nMonat, y = Prozent, group = Partei)) +
      geom_line(aes(colour = Partei)) +      # Liniendiagramm
      xlab('Zeitraum') +                     # Beschriftung x-Achse
      ylab('Anfragen (in % des Monats-Maximums)') + # Beschriftung y-Achse
      ggtitle(input$Titel) +                 # Überschrift
      scale_color_manual(values = farben) +
      theme_bw()
  )

  # Kumuliertes Liniendiagramm
  output$Liniendiagramm_kumuliert <- renderPlot({
    rv$data$Prozent_kumuliert <- unlist(tapply(rv$data$Prozent, rv$data$Partei, cumsum))
    
    ggplot(data = rv$data, aes(x = nMonat, y = Prozent_kumuliert, group = Partei)) +
      geom_line(aes(colour = Partei)) +      # Liniendiagramm
      xlab('Zeitraum') +                     # Beschriftung x-Achse
      ylab('Anfragen (in % des Monats-Maximums/kumuliert)') + # Beschriftung y-Achse
      ggtitle(input$Titel) +                 # Überschrift
      scale_color_manual(values = farben) +
      theme_bw()
  })

  # Datenauswahl (Gesamtplots)
  rv2 <- reactiveValues(data = p1_wide[,2:10])
  observeEvent(input$Update, {
    rv2$data <- p1_wide[as.POSIXct(input$Datum[1]) < p1_wide$nMonat
                        & as.POSIXct(input$Datum[2]) > p1_wide$nMonat,
                        input$Parteien]})


  # Balkendiagramm
  output$Balkendiagramm <- renderPlot({
    p1_new <- data.frame(names(rv2$data), colSums(rv2$data))
    names(p1_new)[1]<-paste("Partei")
    names(p1_new)[2]<-paste("Prozent_kumuliert")

    ggplot(p1_new, aes(x = Partei, y = Prozent_kumuliert, fill = Partei)) +
      scale_fill_manual(values = farben) +
      geom_col(color = "black") +
      theme_bw() +
      xlab("Partei") +
      ylab("kumulierte Suchhäufigkeiten") +
      ggtitle(input$Titel)
  })

  # Kuchendiagramm
  output$Kuchendiagramm <- renderPlot({
    p1_new <- data.frame(names(rv2$data), colSums(rv2$data))
    names(p1_new)[1]<-paste("Partei")
    names(p1_new)[2]<-paste("Prozent_kumuliert")
    p1_new <- p1_new[order(p1_new$Partei, decreasing = TRUE), ]

    ggplot(p1_new, aes(x = "", y = Prozent_kumuliert, fill = Partei)) +
      scale_fill_manual(values = farben) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle(input$Titel) +
      theme(axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(colour = 'black'),
            axis.title = element_blank()) +
      scale_y_continuous(
        breaks = cumsum(p1_new$Prozent_kumuliert) - p1_new$Prozent_kumuliert/2,
        labels = p1_new$Partei)
  })
}

shinyApp(ui, server)
