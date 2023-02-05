#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(shinyFiles)
# library(colourpicker)

# Define UI for application that draws a histogram
# shinyUI(fluidPage(
shinyAppUI  <- shinyUI(fluidPage(
  tags$head(
    tags$style(type="text/css", ".inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                                   .inline .form-group { display: table-row;}")
  ),

  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
  )),

  # Application title
  titlePanel("NIMO-T"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      div(class = "label-left",
          shinyDirButton("directory", "Folder select", "Please select a folder"),
          textInput("sep_value", "Separator", value = ";")),
      hr(),
      div(class = "label-left",
          uiOutput("moreControls"),
          uiOutput("plot1"),
          uiOutput("plot2"),
          uiOutput("plot3")
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  id = "tabset",
                  tabPanel("Plot",
                           # verbatimTextOutput("message"),
                           # verbatimTextOutput("filepaths"),
                           # verbatimTextOutput("directorypath"),
                           # verbatimTextOutput("savefile"),
                           uiOutput("savePlot"),
                           uiOutput("savePara"),
                           plotOutput("distPlot", height = "800px")
                           # dataTableOutput("tbl_out")
                  ),
                  tabPanel("Geology",
                           sliderInput("trsp", "Transparenz (%) :",
                                       min = 0, max = 100,
                                       value = 70),
                           fluidRow(
                             h3("Schichten"),
                             p("max 10. schichten"),
                             column(3,
                                    textInput("name1", "Name 1", value = ""),
                                    textInput("name2", "Name 2", value = ""),
                                    textInput("name3", "Name 3", value = ""),
                                    textInput("name4", "Name 4", value = ""),
                                    textInput("name5", "Name 5", value = ""),
                                    textInput("name6", "Name 6", value = ""),
                                    textInput("name7", "Name 7", value = ""),
                                    textInput("name8", "Name 8", value = ""),
                                    textInput("name9", "Name 9", value = ""),
                                    textInput("name10", "Name 10", value = "")

                             ),
                             column(3,
                                    numericInput("depth1", "Tiefe 1", value = ""),
                                    numericInput("depth2", "Tiefe 2", value = ""),
                                    numericInput("depth3", "Tiefe 3", value = ""),
                                    numericInput("depth4", "Tiefe 4", value = ""),
                                    numericInput("depth5", "Tiefe 5", value = ""),
                                    numericInput("depth6", "Tiefe 6", value = ""),
                                    numericInput("depth7", "Tiefe 7", value = ""),
                                    numericInput("depth8", "Tiefe 8", value = ""),
                                    numericInput("depth9", "Tiefe 9", value = ""),
                                    numericInput("depth10", "Tiefe 10", value = "")
                             ),
                             column(3,
                                    colourpicker::colourInput("col1", label = "Farbe 1", value = "white"),
                                    colourpicker::colourInput("col2", label = "Farbe 2", value = "white"),
                                    colourpicker::colourInput("col3", label = "Farbe 3", value = "white"),
                                    colourpicker::colourInput("col4", label = "Farbe 4", value = "white"),
                                    colourpicker::colourInput("col5", label = "Farbe 5", value = "white"),
                                    colourpicker::colourInput("col6", label = "Farbe 6", value = "white"),
                                    colourpicker::colourInput("col7", label = "Farbe 7", value = "white"),
                                    colourpicker::colourInput("col8", label = "Farbe 8", value = "white"),
                                    colourpicker::colourInput("col9", label = "Farbe 9", value = "white"),
                                    colourpicker::colourInput("col10", label = "Farbe 10", value = "white")
                             )

                           )
                  ),
                  tabPanel("Namen Kurven",
                           fluidRow(
                             h3(""),
                             p("Name der Kurve angeben..."),
                             uiOutput("tempNames")
                           )
                  ),
                  tabPanel("Statistiken",
                           fluidRow(
                             h3("Statistiken"),
                             uiOutput("stat")

                           )
                  )
      )
    )
  )
))
