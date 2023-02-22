#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# yaml save config!

# library(shiny)
# library(shinyFiles)
# library(colourpicker)
# library(yaml)

# shinyFilesExample()
# source("fx.R")

# Define server logic required to draw a histogram
shinyAppServer <- shinyServer(function(input, output, session) {
  volumes <- c(Home = path_home(),  "R Installation" = R.home(),  getVolumes()())

  # shinyFileChoose(input, "file", roots = volumes, session = session)
  # by setting `allowDirCreate = FALSE` a user will not be able to create a new directory
  shinyDirChoose(
    input,
    "directory",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )
  #shinyFileSave(input, "save", roots = volumes, session = session, restrictions = system.file(package = "base"))

  observe({
    # cat("\ninput$directory value:\n\n")
    # print(input$directory)
  })

  observe({
    # cat("\ninput$save value:\n\n")
    # print(input$save)
  })



  output$directorypath <- renderPrint({
    if (is.integer(input$directory)) {
      cat("No directory has been selected (shinyDirChoose)")
    } else {
      # parseDirPath(volumes, input$directory)
      #
      # # print(zmax)
      #
      # DIR <- parseDirPath(volumes, input$directory)
      # fPaths <- getNimoTFiles(DIR)
      #
      # TT <- lapply( fPaths, getNimoT)



    }
  })

  #output$message <- renderPrint({
  #   if(input$savePDF != 0){
  #     cat("PDF im Ordner erstellt!")
  #   }
  # })


  output$savefile <- renderPrint({
    if (is.integer(input$save)) {
      cat("No file-save path has been set (shinyFileSave)")
    } else {
      parseSavePath(volumes, input$save)
    }
  })



  DIR0 <- reactive({
    if (is.integer(input$directory)) {
      cat("No directory has been selected (shinyDirChoose)")
      return(NULL)
    } else {
      parseDirPath(volumes, input$directory)

      print("DIRDIRDIR")

      DIR <- parseDirPath(volumes, input$directory)
      return(DIR)

    }
  })

  TT0 <- reactive({
    if (is.null(DIR0())) {
      cat("No directory has been selected (shinyDirChoose)")
      return(NULL)
    } else {
      DIR <- DIR0()
      fPaths <- getNimoTFiles(DIR)
      TT <- lapply(fPaths, getNimoT)
      # Adapt gradient / velocity
      for (k in seq_along(TT)) {
        # Temperatur gradient
        TT[[k]][, 4] <-
          getGrad(TT[[k]][, 2], TT[[k]][, 1], k = 11, sigma = 15)
        # velocity
        TT[[k]][, 5] <-
          getGrad(TT[[k]][, 3], TT[[k]][, 1], k = 11, sigma = 15)
      }
      # print("TT TT TT")

      return(TT)
    }
  })



  list_curves <- reactive({
    if (is.null(TT0())) {
      return(NULL)
    } else{
      clist <- list()
      for (i in seq_along(TT0())) {
        clist[[paste0("Curve ", i)]] <- i
      }
      # print("LIST_CURCVES")
      return(clist)
    }
  })

  curve_names <- reactive({
    clist <- list()
    if (is.null(defaultsReac())) {
      for (i in seq_along(TT0())) {
        clist[[paste0("Curve ", i)]] <- paste0(i)
      }
      return(clist)
    } else{
      for (i in seq_along(TT0())) {
        clist[[paste0("Curve ", i)]] <- defaultsReac()[["cnames"]][i]
      }
      # defaultsReac()[["cnames"]]
      return(clist)
    }
  })

  output$tempNames <- renderUI({
    if (!is.null(list_curves()) && length(list_curves()) > 0) {
      sapply(curve_names(), print)
      tagList(# textInput("name1", "Name 1", value = ""),
        lapply(seq_along(curve_names()),  function(i, x = curve_names()) {
          textInput(paste0("cname_", i), paste0("Name ", i), value = x[[i]])
        }, curve_names()))
    }
  })

  observe({
    if(!is.null(defaultsReac())){
      for(i in seq_along(defaultsReac()[["geo_v"]][["name"]])){
        print(defaultsReac()[["geo_v"]][["name"]][i])
        updateTextInput(session, paste0("name", i),  value = defaultsReac()[["geo_v"]][["name"]][i])
      }
      for(i in seq_along(defaultsReac()[["geo_v"]][["pos"]])){
        print(defaultsReac()[["geo_v"]][["pos"]][i])
        updateTextInput(session, paste0("depth", i),  value = defaultsReac()[["geo_v"]][["pos"]][i])
      }
      for(i in seq_along(defaultsReac()[["geo_v"]][["col"]])){
       print(defaultsReac()[["geo_v"]][["col"]][i])
        colourpicker::updateColourInput(session, paste0("col", i),  value = defaultsReac()[["geo_v"]][["col"]][i])
      }
      updateSliderInput(session, "trsp", value =  defaultsReac()[["geo_v"]][["trsp"]])
      updateTabsetPanel(session, "tabset", selected = NULL)
    }
  })




  defaultsReac <- reactive({
    if (is.null(TT0())) {
      return(NULL)
    } else{
      TT <- TT0()
      zmin <- 0
      zmax <- getFUN(TT, max, id = 1, na.rm = TRUE)
      ylim <-  c(zmax, zmin)

      # print(unlist(input, use.names = FALSE))

      # print("update!")
      fPath_para <- file.path(DIR0(), "settings.yaml")
      if (file.exists(fPath_para)) {
        defaults <- yaml::read_yaml(fPath_para)
        for(i in seq_along(list_curves())){
          updateTextInput(session, paste0("cname_", i),  value = defaults[["cnames"]][i])
        }
        updateTextInput(session, "proj_nr",  value = defaults[["proj_nr"]])
        updateTextInput(session, "proj_lab",  value = defaults[["proj_lab"]])
        updateTextInput(session, "plot_title",  value = defaults[["title"]])
        updateTextInput(session, "zlab",  value = defaults[["zlab"]])

        updateNumericInput(session, "zmax",  value = defaults[["zmax"]])
        updateNumericInput(session, "dz",  value = defaults[["dz"]])

        # for(i in 1:3){
        #   print(paste("update 1, 2, 3!  dx =", defaults[["dx"]][i]))
        #   updateNumericInput(session, paste0("dx", i), value = defaults[["dx"]][i])
        #   updateNumericInput(session, paste0("xlab", i), value = defaults[["xlab"]][i])
        #   updateNumericInput(session, paste0("xmin", i),
        #                      value = defaults[[paste0("xlim", i)]][1])
        #   updateNumericInput(session, paste0("xmax", i),
        #                      value = defaults[[paste0("xlim", i)]][2])
        # }
        updateTabsetPanel(session, "tabset", selected = NULL)

      } else{
        # here tweaked defaults
        defaults <- list(
          title = "",
          proj_nr = "",
          proj_lab = "",
          geo_v = list(
            name = NULL,
            pos  = NULL,
            col  = NULL,
            trsp = 70
          ),
          xlab = c(
            # "Temperatur (°C)",
            "Temperatur (\u00B0C)",
            "Temperaturgradient (K/m)",
            "Sinkgeschwindigkeit (m/s)"
          ),
          xlim1 = getFUN(TT, range, id = 2, na.rm = TRUE),
          xlim2 = getFUN(TT, range, id = 4, na.rm = TRUE),
          xlim3 = getFUN(TT, range, id = 5, na.rm = TRUE),
          zlab = "Tiefe (m)",
          dz = diff(pretty(ylim, n = 10))[1],
          zmax = zmax,
          cnames = rep("", length(list_curves()))
        )
        defaults[["dx"]] = c(diff(pretty(defaults[["xlim1"]], n = 10))[1],
                             diff(pretty(defaults[["xlim2"]], n = 10))[1],
                             diff(pretty(defaults[["xlim3"]], n = 10))[1])

      }
      return(defaults)
    }
  })


  # printPDF <- 0
  # savePara <- 0
  md5 <- 0
  update <- 0L

  # observeEvent(input$savePDF, {
  #   test$a <- 1L
  #   print(paste("SAVE PDF", test$a))
  #   printPDF <- 1
  # })

  savePara <- reactiveValues(a = 0L)
  observeEvent(input$savePara, {
    savePara$a <- 1
    print(paste("Save_para: ",input$savePara, " it= ", savePara$a))
  })

  printPDF <- reactiveValues(a = 0L)
  observeEvent(input$savePDF, {
    printPDF$a <- 1
    print(paste("Print_PDF: ",input$savePDF, " it= ", printPDF$a))
  })


  # observe({

      # print(DIR0())
      # print("input$savePDF")
      # print(input$savePDF)
      # print(printPDF)
  output$distPlot <- renderPlot({

      if (is.integer(input$directory)) {
        cat("No directory has been selected (shinyDirChoose)")
      } else {
        # parseDirPath(volumes, input$directory)

        defaults <- defaultsReac()
        fPath_para <- file.path(DIR0(), "settings.yaml")

        # print(paste("test$a = ", test$a))


        TT <- TT0()
        zmin <- 0
        zmax <- getFUN(TT, max, id = 1, na.rm = TRUE)
        ylim <-  c(zmax, zmin)

        xlim1 <-
          ifelse(c(inputExists(input$xmin1), inputExists(input$xmax1)),
                 c(input$xmin1, input$xmax1),
                 # no
                 defaults[["xlim1"]])                    # yes
        xlim1 <- sort(xlim1)
        if (diff(xlim1) < 1e-7)
          xlim1[2] <- xlim1[1] + 2e-7

        xlim2 <-
          ifelse(c(inputExists(input$xmin2), inputExists(input$xmax2)),
                 c(input$xmin2, input$xmax2),
                 # no
                 defaults[["xlim2"]])                    # yes
        xlim2 <- sort(xlim2)
        if (diff(xlim2) < 1e-7)
          xlim2[2] <- xlim2[1] + 2e-7

        # xlim3 <- getFUN(TT, range, id = 5, na.rm = TRUE)
        xlim3 <-
          ifelse(c(inputExists(input$xmin3), inputExists(input$xmax3)),
                 c(input$xmin3, input$xmax3),
                 # no
                 defaults[["xlim3"]])                    # yes
        xlim3 <- sort(xlim3)
        if (diff(xlim3) < 1e-7)
          xlim3[2] <- xlim3[1] + 2e-7

        zmax <- ifelse(inputExists(input$zmax),
                       max(c(abs(input$zmax), 1)),            # no
                       defaults[["zmax"]])                    # yes
        # print(zmax)
        zlab <- ifelse(inputExists(input$zlab) && input$zlab != "",
                       input$zlab,
                       # no
                       defaults[["zlab"]])
        xlab1 <-
          ifelse(inputExists(input$xlab1) && input$xlab1 != "",
                 input$xlab1,
                 # no
                 defaults[["xlab"]][1]) # yes
        xlab2 <-
          ifelse(inputExists(input$xlab2) && input$xlab2 != "",
                 input$xlab2,
                 # no
                 defaults[["xlab"]][2]) # yes
        xlab3 <-
          ifelse(inputExists(input$xlab3) && input$xlab3 != "",
                 input$xlab3,
                 # no
                 defaults[["xlab"]][3]) # yes

        # print("OK3")

        ylim <- c(zmax, zmin)

        dz <- ifelse(inputExists(input$dz) && abs(input$dz) > 1e-7,
                     abs(input$dz),
                     # no
                     defaults[["dz"]]) # yes
        dx1 <-
          ifelse(inputExists(input$dx1) && abs(input$dx1) > 1e-7,
                 abs(input$dx1),
                 # no
                 defaults[["dx"]][1]) # yes
        dx2 <-
          ifelse(inputExists(input$dx2) && abs(input$dx2) > 1e-7,
                 abs(input$dx2),
                 # no
                 defaults[["dx"]][2]) # yes
        dx3 <-
          ifelse(inputExists(input$dx3) && abs(input$dx3) > 1e-7,
                 abs(input$dx3),
                 # no
                 defaults[["dx"]][3])


        # if(!is.null(list_curves()) && length(list_curves()) > 0){
        if (inputExists(input$cname_1) && input$cname_1 != "") {
          cnames <-
            sapply(seq_along(list_curves()), function(x, input) {
              input[[paste0("cname_", x)]]
            }, input)
          if (is.list(cnames))
            cnames <- rep("", length(list_curves()))
          cnames <- trimStr(cnames)
          test <- all(cnames == "")
          # print("TEST:", test)
        } else{
          cnames <- defaults[["cnames"]]
        }


        geo_v <- defaults[["geo_v"]]
        if (inputExists(input$depth1)) {
          # print(paste0("--- ->" , input$depth1, " <- ---"))
          geol_names <-
            sapply(1:10, function(x, input) {
              input[[paste0("name", x)]]
            }, input)
          geol_pos <-
            abs(sapply(1:10, function(x, input) {
              getNumShiny(input[[paste0("depth", x)]])
            }, input))
          geol_col <-
            sapply(1:10, function(x, input) {
              input[[paste0("col", x)]]
            }, input)
          trsp <- input$trsp
          # take no NA-values
          test1 <- !is.na(geol_pos)
          # take only strickly increasing values
          test2 <- geol_pos[test1] >= cummax(geol_pos)[test1]

          geo_v <- list(
            name = geol_names[test1][test2],
            pos  = geol_pos[test1][test2],
            col  = geol_col[test1][test2],
            trsp = input$trsp
          )
        }

        # print("OK4")
        col_abline <- "grey40"

        # print("OK5")

        mathplotlib_col = c(
          '#1f77b4',
          '#ff7f0e',
          '#2ca02c',
          '#d62728',
          '#9467bd',
          '#8c564b',
          '#e377c2',
          '#7f7f7f',
          '#bcbd22',
          '#17becf'
        )


        # print(zmax)

        plot_title <- input$plot_title
        proj_nr <- input$proj_nr
        proj_lab <- input$proj_lab


        hlines <- seq(0, by = dz, to = zmax)

        # if (length(input$savePara) > 0 && input$savePara > savePara) {
        if(savePara$a > 0){
          print("WRITE PARAMETERS")
          PARA <- list(
            title = plot_title,
            proj_nr = proj_nr,
            proj_lab = proj_lab,
            geo_v = geo_v,
            xlab = c(xlab1, xlab2, xlab3),
            dx = c(dx1, dx2, dx3),
            xlim1 = xlim1,
            xlim2 = xlim2,
            xlim3 = xlim3,
            zlab = zlab,
            dz = dz,
            zmax = zmax,
            cnames = cnames
          )
          write_yaml(PARA, fPath_para)
          # savePara <- input$savePara
          savePara$a <- 0
          # md5 <- tools::md5sum(fPath_para)
        }

        add_legend_geol <- FALSE
        # print("test legend ", input$legend_geol)
        if (!is.null(input$legend_geol) &&
            input$legend_geol && sum(geo_v$name != "") > 0) {
          add_legend_geol <- TRUE
          # print("___ ADD LEGEND ___")
        }




        oma <- c(0, 4, 4, 2)
        # print(getwd())
        if (add_legend_geol) {
          oma[1] <- oma[1] + 4
        }



        if (length(input$checkGroup) > 0) {

           # savePdf <- 0
          cex <- 1
           if(printPDF$a > 0){
             print("OK")
             CairoPDF(
               file = file.path(DIR0(), "NIMOT.pdf"),
               width = 11.7,
               height = 8.3
             )
             cex <- 0.75
             # printPDF$a <-0
           }


          addzlabels <- TRUE
          addzlab <- zlab
          mar <- c(1, 1.1, 4.1, 0.5)
          mar1 <- mar
          mar2 <- mar
          mar3 <- mar
          if(length(input$checkGroup) == 2 && printPDF$a > 0){
            mar1 <- mar + c(0, 10, 0, 0)
            mar2 <- mar + c(0, 0, 0, 10)

          }
          if(length(input$checkGroup) == 1 && printPDF$a > 0) mar1 <- mar + c(0, 20, 0, 20)
          funpar <- function(it, mar = list(mar1, mar2, mar3)){
            if(it == 1) par(mar = mar[[1]])
            if(it == 2) par(mar = mar[[2]])
            if(it == 3) par(mar = mar[[3]])
          }
          it <- 0

          par(
            mfrow = c(1, length(input$checkGroup)),
            mar = mar,
            oma = oma,
            cex = 1
          )
          # temperature
          if ("1" %in% input$checkGroup) {
            it <- it + 1
            funpar(it)
            plotNimoT(
              TT,
              id = 2,
              xlim1,
              ylim,
              dx1,
              xlab1,
              zlab = addzlab,
              hlines,
              geo_v,
              zlabels = addzlabels,
              sel = input$checkgroup_plot1,
              cex = cex
            )
            addzlabels <- FALSE
            addzlab <- ""

            if (all(cnames != "") &&
                !is.null(input$legend_temp) && input$legend_temp) {
              graphics::legend(
                "bottomright",
                legend = cnames,
                lwd = 1.5,
                col = mathplotlib_col[seq_along(cnames)],
                cex = cex
              )
            }
            # if (add_legend_geol) {
            #   addLegend(geo_v)
            #   add_legend_geol <- FALSE
            # }
          }
          # temperature gradient
          if ("2" %in% input$checkGroup) {
            it <- it + 1
            funpar(it)
            plotNimoT(
              TT,
              id = 4,
              xlim2,
              ylim,
              dx2,
              xlab2,
              zlab = addzlab,
              hlines,
              geo_v,
              zlabels = addzlabels,
              sel = input$checkgroup_plot2,
              cex = cex
            )
            addzlabels <- FALSE
            addzlab <- ""
            if (all(cnames != "") &&
                !is.null(input$legend_grad) && input$legend_grad) {
              graphics::legend(
                "bottomright",
                legend = cnames,
                lwd = 1.5,
                col = mathplotlib_col[seq_along(cnames)],
                cex = cex
              )
            }
            # if (add_legend_geol) {
            #   addLegend(geo_v)
            #   add_legend_geol <- FALSE
            # }
          }
          # velocity
          if ("3" %in% input$checkGroup) {
            it <- it + 1
            funpar(it)
            plotNimoT(
              TT,
              id = 5,
              xlim3,
              ylim,
              dx3,
              xlab3,
              zlab = addzlab,
              hlines,
              geo_v,
              zlabels = addzlabels,
              sel = input$checkgroup_plot3,
              cex = cex
            )
            addzlabels <- FALSE
            addzlab <- ""
            if (all(cnames != "") &&
                !is.null(input$legend_vel) && input$legend_vel) {
              graphics::legend(
                "bottomright",
                legend = cnames,
                lwd = 1.5,
                col = mathplotlib_col[seq_along(cnames)],
                cex = cex
              )
            }
            # if (add_legend_geol) {
            #   addLegend(geo_v)
            #   add_legend_geol <- FALSE
            # }
          }
          par(mar = mar)
          # if (add_legend_geol) {
          #   addLegend(geo_v)
          #   add_legend_geol <- FALSE
          # }
          graphics::mtext(
            paste(input$proj_nr, "\n", input$proj_lab),
            side = 3,
            outer = TRUE,
            adj = 0,
            line = 1,
            cex = 0.75
          )

          addlogo(
            logo,
            xl = 1,
            yl = par()$mar[3] + 2.5,
            size = 0.25
          )
          graphics::title(main = plot_title, outer = TRUE)

          xpos <- NULL #par()$usr[2] + (par()$omi[4] + par()$mai[4] - par()$din[1] )*fac
          if(length(input$checkGroup) == 3){
            fac <- 1 / par()$pin[1] * diff(par()$usr[1:2])
            xpos <- par()$usr[2] + (par()$omi[4] + par()$omi[2] + par()$mai[4] + par()$mai[2] - par()$din[1] )*fac
          }
          if (add_legend_geol) {
            addLegend(geo_v, cex = cex, xpos = xpos)
            add_legend_geol <- FALSE
          }

          if(printPDF$a > 0){
            print("OK")
            grDevices::dev.off()
            printPDF$a <-0
          }
        }
      }

    })

  output$savePlot <- renderUI({
    # if(!is.null(file_list())){
    if (is.integer(input$directory)) {
      return(NULL)

    } else{
      tagList(actionButton("savePDF", "PDF erstellen"))
    }
  })

  output$savePara <- renderUI({
    # if(!is.null(file_list())){
    if (is.integer(input$directory)) {
      return(NULL)
    } else{
      tagList(actionButton("savePara", "Einstellungen speichern"))
    }
  })

  output$moreControls <- renderUI({
    # if(!is.null(file_list())){
    if (is.integer(input$directory)) {
      return(NULL)

    } else{
      tagList(
        # sliderInput("n", "N", 1, 1000, 500),
        # downloadButton('export', "Download pdf"),
        checkboxGroupInput(
          "checkGroup",
          p("Auswahl"),
          choices = list(
            "Temperatur" = 1,
            "Temp-Gradient" = 2,
            "Sinkgeschwindigkeit" = 3
          ),
          selected = c(1, 2, 3)
        ),
        textInput("proj_nr", "Projekt-Nr."),
        textInput("proj_lab", "Projekt Name"),
        textInput("plot_title", "Title"),
        numericInput("zmax", "z-max", value = NULL),
        textInput("zlab", "zlab", value = NULL),
        numericInput("dz", "dz", value = NULL),
        checkboxInput("legend_geol", "Legend Geol. hinfügen", FALSE)
      )

    }
  })



  output$stat <- renderUI({
    TT <- TT0()
    lapply(TT, function(x)
      print(summary(x[, 4])))
    tagList(
      h4("Temperatur"),
      lapply(TT, function(x)
        renderPrint(summary(x[, 2]))),
      h4("Temperatur Gradient"),
      lapply(TT, function(x)
        renderPrint(summary(x[, 4]))),
      h4("Geschwindigkeit"),
      lapply(TT, function(x)
        renderPrint(summary(x[, 5])))
    )
  })


  output$plot1 <- renderUI({
    if ("1" %in% input$checkGroup) {
      tagList(
        hr(),
        h4("Plot Temperatur"),
        checkboxGroupInput(
          "checkgroup_plot1",
          p("Auswahl"),
          choices = list_curves(),
          selected = seq_along(list_curves())
        ),
        numericInput("dx1", "dx", value = defaultsReac()[["dx"]][1]),
        textInput("xlab1", "xlab", value = defaultsReac()[["xlab"]][1]),
        numericInput("xmin1", "x-min", value = defaultsReac()[["xlim1"]][1]),
        numericInput("xmax1", "x-max", value = defaultsReac()[["xlim1"]][2]),
        checkboxInput("legend_temp", "Legend hinfügen", FALSE)
      )

    }
  })
  output$plot2 <- renderUI({
    if ("2" %in% input$checkGroup) {
      tagList(
        hr(),
        h4("Plot Temperaturgradient"),
        checkboxGroupInput(
          "checkgroup_plot2",
          p("Auswahl"),
          choices = list_curves(),
          selected = seq_along(list_curves())
        ),
        numericInput("dx2", "dx", value = defaultsReac()[["dx"]][2]),
        textInput("xlab2", "xlab", value = defaultsReac()[["xlab"]][2]),
        numericInput("xmin2", "x-min", value = defaultsReac()[["xlim2"]][1]),
        numericInput("xmax2", "x-max", value = defaultsReac()[["xlim2"]][2]),
        checkboxInput("legend_grad", "Legend hinfügen", FALSE)
      )

    }
  })
  output$plot3 <- renderUI({
    if ("3" %in% input$checkGroup) {
      tagList(
        hr(),
        h4("Plot Sinkgeschwindigkeit"),
        checkboxGroupInput(
          "checkgroup_plot3",
          p("Auswahl"),
          choices = list_curves(),
          selected = seq_along(list_curves())
        ),
        numericInput("dx3", "dx", value = defaultsReac()[["dx"]][3]),
        textInput("xlab3", "xlab", value = defaultsReac()[["xlab"]][3]),
        numericInput("xmin3", "x-min", value = defaultsReac()[["xlim3"]][1]),
        numericInput("xmax3", "x-max", value = defaultsReac()[["xlim3"]][2]),
        checkboxInput("legend_vel", "Legend hinfügen", FALSE)
      )

    }
  })



})
