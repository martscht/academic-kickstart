library(shiny)
ui <- pageWithSidebar(
  h1(strong(em("Hier kann man das Sudoku lösen!")),
     align = "center", style = 'background: aqua'),
  sidebarPanel(
    h3("Willst du dein eigenes Sudoku lösen? Lade dein Sudoku einfach hier hoch!"),
    br(),
    fileInput(inputId = "Matrix",
              label = "Füge hier dein eigenes Sudoku (als .rds) ein!",
              accept = ".rds",
              buttonLabel = "Datei hochladen",
              placeholder = "Noch keine Datei hochgeladen."),
    actionButton(inputId = "use",
                 label = "Sudoku benutzen"),
    h3("Wo willst du eine Zahl einsetzen?"),
    numericInput(inputId = "row",
                 label = "Reihe",
                 value = '', min = 1, max = 9),
    numericInput(inputId = "column",
                 label = "Spalte",
                 value = '', min = 1, max = 9),
    numericInput(inputId = "num",
                 label = "Zahl",
                 value = '', min = 1, max = 9),
    actionButton(inputId = "go",
                 label = "Zahl einsetzen")),
  mainPanel(
    wellPanel(
      plotOutput("Sudokus", width = "640px", height = "600px"),
      align = "center"
    )
  )
)

server <- function(input, output, session) {
  BeispielSudoku <- readRDS('BeispielSudokuLeicht.rds')
  library(plot.matrix)
  rv <- reactiveValues(data = BeispielSudoku)
  observeEvent(input$use, {
    rv$data <- readRDS(input$Matrix$datapath)
  })
  meinSudoku <- reactive({
    readRDS(input$Matrix$datapath)
  })
  observeEvent(input$go, {
    if(is.null(input$Matrix) & is.na(BeispielSudoku[input$row, input$column])){
      rv$data[input$row, input$column] <- input$num
    }
    if(!is.null(input$Matrix)) {
      if(is.na(meinSudoku()[input$row, input$column])) {
        rv$data[input$row, input$column] <- input$num
      }
    }
  })
  output$Sudokus <- renderPlot({
    par(cex.axis = 1.5, col.axis = 'darkblue', mar = c(4,4,0,2))
    plot(rv$data, border = F,
         col = c("yellow" , "orange", "red", "violet", "lightblue",
                 "cornflowerblue", "lightgreen", "chartreuse3", "lightsalmon4"),
         na.col = "white", xlab = "", ylab = "", text.cell = list(cex = 2),
         key = NULL, fmt.cell='%.0f', na.print = '',
         xlim = c(0.5, 9.5), ylim = c(0.5, 9.5))
    abline(h = 0.5, lwd = 5)
    abline(h = 1.5, lwd = 0.5)
    abline(h = 2.5, lwd = 0.5)
    abline(h = 3.5, lwd = 2)
    abline(h = 4.5, lwd = 0.5)
    abline(h = 5.5, lwd = 0.5)
    abline(h = 6.5, lwd = 2)
    abline(h = 7.5, lwd = 0.5)
    abline(h = 8.5, lwd = 0.5)
    abline(h = 9.5, lwd = 5)
    abline(v = 0.5, lwd = 5)
    abline(v = 1.5, lwd = 0.5)
    abline(v = 2.5, lwd = 0.5)
    abline(v = 3.5, lwd = 2)
    abline(v = 4.5, lwd = 0.5)
    abline(v = 5.5, lwd = 0.5)
    abline(v = 6.5, lwd = 2)
    abline(v = 7.5, lwd = 0.5)
    abline(v = 8.5, lwd = 0.5)
    abline(v = 9.5, lwd = 5)
  })
}

shinyApp(ui, server)
