

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               maxColorValue = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  ## Save the color
  invisible(t.col)
}
## END


# from https://stackoverflow.com/questions/38439211/figure-labels-add-text-on-graphs-in-the-same-location-despite-figure-size
# example:
# par(xpd = NA)
# legend(x = line2user(line = 8, side = 2), y = line2user(line = 5.5, side = 3), lab,
#        horiz = FALSE, col=rev(cols),
#        fill=rev(cols),
#        bty = "n", ncol = 2
# )
# par(xpd = FALSE)
line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
  switch(side,
         '1' = grconvertY(-line * y_off, 'npc', 'user'),
         '2' = grconvertX(-line * x_off, 'npc', 'user'),
         '3' = grconvertY(1 + line * y_off, 'npc', 'user'),
         '4' = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
}

addfiglab <- function(lab, xl = par()$mar[2], yl = par()$mar[3]) {
  text(x = line2user(xl, 2), y = line2user(yl, 3),
       lab, xpd = NA, font = 2, cex = 1.5, adj = c(0, 1))

}


# v <- diff(grconvertX(c(0, par()$din[1] ), from = "inches", to = "user"))
# par()$din[1] / v
# example: addlogo(logo, xl = 1, yl = par()$mar[3] + 3.5, size = 0.25)
addlogo <- function(logo, xl = par()$mar[4],
                    yl = par()$mar[3], size = 0.45) {
  if(!is.null(logo)){
    x <- line2user(xl, 4)
    y <- line2user(yl, 3)
    dims <- dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
    ux <- abs(diff(grconvertX(par()$usr[1:2], from = "user", to = "inches")))
    uy <- abs(diff(grconvertX(par()$usr[3:4], from = "user", to = "inches")))
    # ux*3 + 3*sum(par()$mai[c(2, 4)]) + sum(par()$omi[c(2, 4)])  ==  par()$din[1]
    #
    sizex <- abs(par()$din[1] * size * diff(par()$usr[1:2])/ux )
    sizey <- dims[1]/dims[2] * abs(sizex / diff(par()$usr[1:2]) * diff(par()$usr[3:4]))/2
    rasterImage(logo, x-(sizex), y + sizey, x, y, interpolate=TRUE, xpd = NA)
  }
}


# bottom right outside plot area but relative to plot area
addLegend <- function(geo_v, xpos = NULL, cex = 0.75){
  sel <- geo_v$name != "" & !duplicated(geo_v$name)
  par(xpd = NA)
  print(paste("SUM SEL = ", paste(sum(sel), collapse = " ; ")))
  print(paste("widht SEL = ", strwidth(geo_v$name[sel])))
  on.exit(par(xpd = FALSE), add = TRUE)
  lg_pos <- line2user(3, 1)
  # legend(x = line2user(0, side = 2),
  if(is.null(xpos)){
    fac <- 1 / par()$pin[1] * diff(par()$usr[1:2])
    xpos <- par()$usr[2] + (par()$omi[4] + par()$mai[4] - par()$din[1] )*fac
  }
  legend(x = xpos,
         y = line2user(1, side = 1),
         legend = geo_v$name[sel],
         fill  = geo_v$col[sel],
         # horiz = TRUE,
         bty = "n",
         ncol = sum(sel),
         text.width = strwidth(geo_v$name[sel]),
         cex  = cex

  )
}

# u <- c("lkjlkj", "kd", " lkjkj  dsf", " lkj lkj lkjlj ljljkl")
# strwidth(u)

# diff(grconvertX(c(0, par()$din[2] ), from = "inches", to = "user"))
# u <- diff(grconvertX(par()$usr[1:2], from = "user", to = "inches"))
# u*3 + 3*sum(par()$mai[c(2, 4)]) + sum(par()$omi[c(2, 4)])
# par()$din[1]
#
# win <- 0.2 * par()$din[1]
# win * diff(par()$usr[1:2])/u


evalNimoT <- function(Ti, dz = 0.1,  k = 5, sigma = 0){

  # x <- Ti[, 3]
  # sel <- which(x >= cummax(x)) #seq_along(x) >= which(x >= cummax(x))[1]
  #
  # diff(Ti[sel, 3])
  #
  # plot(Ti[sel, 3])
  #
  # sel <- c(TRUE, diff(Ti[, 3]) > 0)
  vz <- seq(from = 0, by = dz, to = max(Ti[,3]))
  n <- length(vz)
  Tint <- matrix(0, nrow = n, ncol = 5)
  # tiefe
  Tint[, 1] <- vz
  # TEMPERATURE
  Tint[, 2] <- signal::interp1(x = Ti[, 3], y = Ti[,4],
                               xi = vz,
                               method = "linear", extrap = TRUE)
  # SECONDS
  Tint[, 3] <- signal::interp1(x = Ti[, 3], y = Ti[,2],
                               xi = vz,
                               method = "linear", extrap = TRUE)
  # Temperatur gradient
  Tint[, 4] <- getGrad(Tint[, 2], Tint[, 1],  k = k, sigma = sigma)
  # velocity
  Tint[, 5] <- getGrad(Tint[, 3], Tint[, 1],  k = k, sigma = sigma)
  return(Tint)
}
smoothData <- function(x, k = 11, sigma = 15){
  x1 <- stats::runmed(x, k = 11)
  x2 <- stats::smooth.spline(x1)$y
  if(sigma > 0)   x2 <-  mmand::gaussianSmooth(x2, sigma  = sigma)
  return(x2)
}

getGrad <- function(x, d,  k = 5, sigma =10){
  # dest <- doremi::calculate.gold(time = d, signal = x, embedding = 5, n = 2)

  gradx <- numeric(length(x))
  d <- mean(diff(d))
  x <- smoothData(x, k = k, sigma = sigma)
  gradx[2:length(x)] <- diff(x) /d
  return(gradx)
}

readNimoT <- function(fPath, sep = ";"){

  Ti0 <- read.table(fPath, sep = sep, header = TRUE)

  # select the rows corresponding to the fall of the NIMO-T (depth increases)
  # istart <- min(which(Ti0[, 5] > 0.001))
  # iend <- min(which.max(Ti0[, 5]))
  sel <- Ti0[, 5] >= cummax(Ti0[, 5])
  # 1 = date/time
  # 2 = seconds
  # 3 = depth
  # 4 = temperatur
  # Ti <- Ti0[istart:iend, c(1, 2, 5, 6)]
  Ti <- Ti0[sel & Ti0[, 5] > 0.001, c(1,2,5,6)]
  Ti <- Ti[!duplicated(Ti[, 3]), ]
  # Ti[, 1] <- lubridate::dmy_hms(Ti[,1])
  return(Ti)
}

plotBox <- function(geo_v){
  if(length(geo_v$pos) > 0 ){
    geo_v$pos <- c(0, geo_v$pos) #, par()$usr[3])
    for(i in seq_len(length(geo_v$pos)-1)){
      rect(par()$usr[1]-100, geo_v$pos[i], par()$usr[3]+100, geo_v$pos[i+1],
           col = t_col(geo_v$col[i], geo_v$trsp),
           border = NA)
    }
  }
}
vabline <- function(x, dx){
  seq(floor(min(x)/dx) * dx,
      by = dx,
      to = ceiling(max(x)/dx) * dx)
}

plotNimoT <- function(TT, id = 2, xlim, ylim, dx, xlab, zlab,
                      hlines, geo_v, col_abline = "grey40", zlabels = FALSE,
                      col = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
                              '#9467bd', '#8c564b', '#e377c2',
                              '#7f7f7f', '#bcbd22', '#17becf'),sel = NULL,
                      cex = 0.75
){
  if(is.null(sel)) sel <- seq_along(TT)
  plot(0, 0, type = "n", ylim = ylim, xlim = xlim, bty = "n", axes = FALSE,
       xlab = "", ylab = "", yaxs = "i")
  plotBox(geo_v)
  vlines <- vabline(xlim, dx)
  abline(h = hlines,
         v = vlines,
         col = col_abline)
  for(k in seq_along(TT)){
    if(k %in% sel)     lines(TT[[k]][, id], TT[[k]][, 1], lwd = 1.5, col = col[k])
  }
  box(lwd = 1.15)
  axis(2, labels = zlabels, at = hlines, tck = -0.025, lwd.ticks = 1.15,
       cex.axis = cex)
  axis(3, at = vlines, tck = -0.025, lwd.ticks = 1.15,
       cex.axis = cex)
  mtext(xlab, side = 3, line = 3, cex = cex)
  mtext(zlab, side = 2, line = 3, cex = cex)
}

# coderange = c(170:176)
# asciitable_printable = data.frame(
#   coderange,
#   as.raw(coderange),
#   row.names=rawToChar(as.raw(coderange),multiple=TRUE)
# )
# colnames(asciitable_printable) <- c("dec","hex")
# asciitable_printable
# chr <- function(n) { rawToChar(as.raw(n)) }
# chr(176) # 97


getNimoT <- function(fPath, sep = ";"){
  Ti <- readNimoT(fPath, sep = sep)
  evalNimoT(Ti)
}

getNimoTFiles <- function(DIR){
  FILES0 <- list.files(DIR , pattern  = "(\\.csv)$")
  sel <- grepl('Log[0-9]+', FILES0)
  # if(any(sel)){
  #  FILES <- FILES0[sel]
  # }else{
  #  FILES <- FILES0
  # }
  FILES <- FILES0
  #
  # if(length(FILES) == 1){
  #   ID <- 0
  # }else{
  #   u <- regexpr('Lodfg[0-9]+', FILES)
  #   if(any(u == -1)){
  #     ID <- paste0("Log", seq_along(FILES))
  #   }else{
  #     ID <- regmatches(FILES, u)
  #   }
  # }
  fPath <- file.path(DIR, FILES)
  return(fPath)
}

getNimoTID <- function(DIR){
  FILES0 <- list.files(DIR , pattern  = "(\\.csv)$")
  if(length(FILES0) == 1){
    ID <- "Log 1"
  }else{
    sel <- grepl('Log[0-9]+', FILES0, ignore.case = TRUE)
    if(any(sel)){

      FILES <- FILES0[sel]

      if(length(FILES) == 1){
        ID <- 0
      }else{
        u <- regexpr('Log[0-9]+', FILES)
        if(any(u == -1)){
          ID <- paste0("Log", seq_along(FILES))
        }else{
          ID <- regmatches(FILES, u)
        }
      }
    }else{
      # u <- regexpr("([0-9]+)", FILES0)
      v <- regexpr("(\\D+\\d+)", FILES0)
      if(any(v == -1)){
        ID <- paste0("Log", seq_along(FILES0))
      }else{
        # ID <- regmatches(FILES, u)
        ID <- regmatches(FILES0, v)
      }
      # regmatches(FILES0, u)
    }
  }
  return(ID)
}

.getFUN <- function(x, FUN, id, ...){
  FUN(x[, id], ...)
}
getFUN <- function(x, FUN, id, ...){
  FUN(sapply(x, .getFUN, FUN, id, ...), ...)
}

inputExists <- function(x){
  !(is.null(x) || is.na(x))
}

getNumShiny <- function(x){
  if(!(is.null(x) || is.na(x))){
    x
  }else{
    NA
  }
}


# returns string w/o leading or trailing whitespace
trimStr <- function (x) gsub("^\\s+|\\s+$", "", x)

# logo_url <- "https://raw.githubusercontent.com/emanuelhuber/FEFLOWpy/main/GT_Logo_RGB_D.png"
# logo <- tryCatch(
#           {u <- download.file(logo_url,"test.png",mode="wb")
#             logo <- png::readPNG("test.png")
#             return(logo)},
#           error = function(e){
#             return(NULL)
#           }
        # )


# create data
# download.file(logo_url,"test.png",mode="wb")
# logo <- png::readPNG("test.png")
# usethis::use_data(logo)
