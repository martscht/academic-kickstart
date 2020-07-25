library(shiny)
ui <- fluidPage(
  theme = "bootstrap.css", style = "background: #337ab7; color: white",
  tags$head(tags$style(".navbar {background-color: #eded00;}",
                       ".navbar-default .navbar-brand {color: black;}")),
  titlePanel(
    h1(strong("Suchanfragen deutscher Parteien"), align = "center")),
  wellPanel(style = "background: #87CEFA; border-color: #2e6da4",
            navbarPage(
              strong("Darstellungsart"),
              tabPanel("Liniendiagramm", plotOutput("Liniendiagramm")),
              tabPanel("Liniendiagramm (kumuliert)", plotOutput("Liniendiagramm_kumuliert")),
              tabPanel("Balkendiagramm", plotOutput("Balkendiagramm_kumuliert")),
              tabPanel("Balkendiagramm 2", plotOutput("Balkendiagramm2_kumuliert")),
              tabPanel("Kuchendiagramm", plotOutput("Kuchendiagramm")),
              tabPanel("Kuchendiagramm 2", plotOutput("Kuchendiagramm2"))
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
                inputId = "Update", label = strong("Graphik erzeugen"),
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
            )
  )
)

server <- function(input, output, session) {
  p1_long <- readRDS('p1_long.rds')
  p1_wide <- readRDS('p1_wide.rds')
  p1_long$nMonat <- as.character(p1_long$Monat)
  p1_long$nMonat <- paste0(p1_long$nMonat, '-01')
  p1_long$nMonat <- strptime(p1_long$nMonat, format="%Y-%m-%d")
  p1_long$nMonat <- as.POSIXct(p1_long$nMonat)
  p1_wide$nMonat <- as.character(p1_wide$Monat)
  p1_wide$nMonat <- paste0(p1_wide$nMonat, '-01')
  p1_wide$nMonat <- strptime(p1_wide$nMonat, format="%Y-%m-%d")
  p1_wide$nMonat <- as.POSIXct(p1_wide$nMonat)
  farben <- c('AfD' = 'deepskyblue', 'CDU' = 'black', 'DieGrüne' = 'green3',
              'DieLinke' = 'magenta', 'FDP' = 'gold', 'MLPD' = 'orange',
              'NPD' = 'brown', 'SPD' = 'red', 'Tierschutzpartei' = 'darkblue')
  for (i in 1:190) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[1:i])}
  for (i in 191:380) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[191:i])}
  for (i in 381:570) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[381:i])}
  for (i in 571:760) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[571:i])}
  for (i in 761:950) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[761:i])}
  for (i in 951:1140) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[951:i])}
  for (i in 1141:1330) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[1141:i])}
  for (i in 1331:1520) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[1331:i])}
  for (i in 1521:1710) {p1_long$Prozent_kumuliert[i] <- sum(p1_long$Prozent[1521:i])}
  library(ggplot2)
  library(dplyr)
  rv <- reactiveValues(data = p1_long)
  observeEvent(input$Update, {
    rv$data <- p1_long[is.element(p1_long$Partei, input$Parteien)
                       & as.POSIXct(input$Datum[1]) < p1_long$nMonat
                       & as.POSIXct(input$Datum[2]) > p1_long$nMonat,]})
  output$Liniendiagramm <- renderPlot(
    ggplot(data = rv$data, aes(x = nMonat, y = Prozent, group = Partei)) +
      geom_line(aes(colour = Partei)) +      # Liniendiagramm
      xlab('Zeitraum') +                     # Beschriftung x-Achse
      ylab('Anfragen (in % des Monats-Maximums)') + # Beschriftung y-Achse
      ggtitle(input$Titel) +                 # Überschrift
      scale_color_manual(values = farben) +
      theme_bw()
  )
  output$Liniendiagramm_kumuliert <- renderPlot(
    ggplot(data = rv$data, aes(x = nMonat, y = Prozent_kumuliert, group = Partei)) +
      geom_line(aes(colour = Partei)) +      # Liniendiagramm
      xlab('Zeitraum') +                     # Beschriftung x-Achse
      ylab('Anfragen (in % des Monats-Maximums/kumuliert)') + # Beschriftung y-Achse
      ggtitle(input$Titel) +                 # Überschrift
      scale_color_manual(values = farben) +
      theme_bw()
  )
  rv2 <- reactiveValues(data = p1_wide[,2:10])
  observeEvent(input$Update, {
    rv2$data <- p1_wide[as.POSIXct(input$Datum[1]) < p1_wide$nMonat
                        & as.POSIXct(input$Datum[2]) > p1_wide$nMonat,
                        input$Parteien]})
  output$Balkendiagramm_kumuliert <- renderPlot(
    barplot(height = colSums(rv2$data),
            beside = FALSE, ylim = c(0, 2000),
            col = c('deepskyblue', 'red', 'gold', 'green3', 'magenta',
                    'darkblue', 'black', 'orange', 'brown'),
            main = input$Titel)
  )
  output$Balkendiagramm2_kumuliert <- renderPlot({
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
  output$Kuchendiagramm <- renderPlot(
    pie(x = colSums(rv2$data), main = input$Titel,
        col = c('deepskyblue', 'red', 'gold', 'green3', 'magenta', 'darkblue',
                'black', 'orange', 'brown')
    )
  )
  output$Kuchendiagramm2 <- renderPlot({
    p1_new <- data.frame(names(rv2$data), colSums(rv2$data))
    names(p1_new)[1]<-paste("Partei")
    names(p1_new)[2]<-paste("Prozent_kumuliert")
    p1_new <- p1_new %>%
      arrange(desc(Partei)) %>%
      mutate(prop = Prozent_kumuliert / sum(p1_new$Prozent_kumuliert) *100) %>%
      mutate(ypos = cumsum(prop) - 0.5*prop)
    ggplot(p1_new, aes(x = "", y = Prozent_kumuliert, fill = Partei)) +
      scale_fill_manual(values = farben) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle(input$Titel) +
      theme(axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(colour = 'black', size = 20),
            axis.title = element_blank()) +
      scale_y_continuous(
        breaks = cumsum(p1_new$Prozent_kumuliert) - p1_new$Prozent_kumuliert/2,
        labels = p1_new$Partei)
  })
}

shinyApp(ui, server)
