# UI
ui <- shinydashboardPlus::dashboardPagePlus(
  sidebar_fullCollapse = TRUE,
  # theme = shinythemes::shinytheme("superhero"),

  header = shinydashboardPlus::dashboardHeaderPlus(title = "evniche", enable_rightsidebar = TRUE,
                                                   rightSidebarIcon = "gears"),

  # ---------------
  # left panel
  sidebar = shinydashboard::dashboardSidebar(
      # ---
        # ---

        width = 400,
        h4("Niche features:"),

        ## data
        h5("Data"),
        fluidRow(
          column(width = 5, strong("Initial data")),
          column(width = 4),
          column(width = 3, conditionalPanel("input.backt == 'CSV'",
                                             strong("Sep.")))
        ),
        fluidRow(
          column(
            width = 5,
            selectInput("backt", NULL, choices = c("NONE", "CSV", "Raster"))
          ),
          column(
            width = 4,
            conditionalPanel(
              "input.backt == 'CSV'",
              shinyFiles::shinyFilesButton(
                id = "file_data1", label = "Select file",
                title = "Select file (.csv)", multiple = FALSE
              )
            ),
            conditionalPanel(
              "input.backt == 'Raster'",
              shinyFiles::shinyFilesButton(
                id = "file_data2", label = "Select files",
                title = "Select files (.tif, .asc, or .bil)", multiple = TRUE
              )
            )
          ),
          column(width = 3,
                 conditionalPanel("input.backt == 'CSV'",
                                  textInput("sep", NULL, value = ",")))
        ),

        fluidRow(
          conditionalPanel("input.backt == 'CSV'",
                           column(width = 6,
                                  textInput("lon", "Longitude (optional)",
                                            value = "")),
                           column(width = 6,
                                  textInput("lat", "Latitude (optional)",
                                            value = "")))
        ),

        ## variables
        fluidRow(
          column(width = 8,
                 conditionalPanel("input.backt != 'NONE'",
                                  tags$b(tags$small("Available variables")))),
          column(width = 4, tags$b(tags$small("Ellipsoid limit")))
        ),
        fluidRow(
          column(width = 8,
                 conditionalPanel("input.backt == 'CSV'",
                                  textOutput(outputId = "varscsv")),
                 conditionalPanel("input.backt == 'Raster'",
                                  textOutput(outputId = "varsras"))),
          column(width = 4,
                 numericInput("elevel", NULL, value = 99, min = 1,
                              max = 99.99, step = 1))
        ),

        fluidRow(style = "height:10px"),
        h5("Variable ranges"),
        fluidRow(
          column(width = 6, textInput("v1nam", "Variable 1 (name)", value = "")),
          column(width = 3, textInput("v1min", "Minimum", value = "")),
          column(width = 3, textInput("v1max", "Maximum", value = ""))
        ),

        fluidRow(
          column(width = 6, textInput("v2nam", "Variable 2", value = "")),
          column(width = 3, textInput("v2min", "Minimum", value = "")),
          column(width = 3, textInput("v2max", "Maximum", value = ""))
        ),

        fluidRow(
          column(width = 6, textInput("v3nam", "Variable 3", value = "")),
          column(width = 3, textInput("v3min", "Minimum", value = "")),
          column(width = 3, textInput("v3max", "Maximum", value = ""))
        ),

        fluidRow(
          column(width = 6, textInput("v4nam", "Variable 4", value = "")),
          column(width = 3, textInput("v4min", "Minimum", value = "")),
          column(width = 3, textInput("v4max", "Maximum", value = ""))
        ),

        ## covariances
        conditionalPanel(
          "input.v1nam != '' & input.v1min != '' & input.v1max != '' &
          input.v2nam != '' & input.v2min != '' & input.v2max != ''",

          fluidRow(style = "height:10px"),
          h5("Covariance values"),
          sliderInput("cov12", "Variables 1-2", min = -1, max = 1,
                      value = 0, step = 0.1),

          conditionalPanel(
            "input.v3nam != '' & input.v3min != '' & input.v3max != ''",
            sliderInput("cov13", "Variables 1-3", min = -1, max = 1,
                        value = 0, step = 0.1),
            conditionalPanel(
              "input.v4nam != '' & input.v4min != '' & input.v4max != ''",
              sliderInput("cov14", "Variables 1-4", min = -1, max = 1,
                          value = 0, step = 0.1)
            ),

            sliderInput("cov23", "Variables 2-3", min = -1, max = 1,
                        value = 0, step = 0.1),

            conditionalPanel(
              "input.v4nam != '' & input.v4min != '' & input.v4max != ''",
              sliderInput("cov24", "Variables 2-4", min = -1, max = 1,
                          value = 0, step = 0.1),
              sliderInput("cov34", "Variables 3-4", min = -1, max = 1,
                          value = 0, step = 0.1)
            )
          )
        )

  ),
  # ---------------

  # ---------------
  # main panel
  body = shinydashboard::dashboardBody(
    column(
      width = 12,

      ## visualization
      fluidRow(
        fluidRow(
          column(
            width = 7,
            h4("Visualization:"),
            fluidRow(
              column(width = 4, selectInput("space", "Visualization space",
                                            selected = "Environmental",
                                            choices = c("Environmental",
                                                        "Geographic"))),
              column(width = 4,
                     conditionalPanel(
                       "input.space == 'Environmental'",
                       numericInput("nback", "Max. background",
                                    value = 1000, min = 1, step = 100)),
                     conditionalPanel(
                       "input.space == 'Geographic'",
                       selectInput("layer", "Base layer", selected = "NONE",
                                   choices = c("NONE")))),

              column(width = 4,
                     conditionalPanel(
                       "input.space == 'Geographic'",
                       selectInput("laypal", "Color layer",
                                   selected = "rev. terrain",
                                   choices = c("#FFFFFF", "#EEEEEE", "#D8FFE6",
                                               "#EAF7FF", "#000000", "#6C6C6C",
                                               "#152458", "#15582C", "ca. viridis",
                                               "ca. magma", "blues", "heat",
                                               "terrain", "topo", "rev. terrain"))))
            ),
          ),
          column(width = 5, textOutput(outputId = "ellmet"))
        ),


        conditionalPanel("input.space == 'Environmental'",
                         plotOutput(outputId = "pplote")),

        conditionalPanel("input.space == 'Geographic'",
                         plotOutput(outputId = "pplotg"))
      ),

      ## graphical parameters
      br(),
      fluidRow(
        h4("Graphical parameters:"),
        fluidRow(
          column(width = 2, numericInput("marbo", "Margin bottom", value = 4.5,
                                         min = 0, step = 0.1)),
          column(width = 2, numericInput("marle", "Margin left", value = 4.5,
                                         min = 0, step = 0.1)),
          column(width = 2, numericInput("marto", "Margin top", value = 1,
                                         min = 0, step = 0.1)),
          column(width = 2, numericInput("marri", "Margin right", value = 1,
                                         min = 0, step = 0.1)),
          column(width = 2, numericInput("asp", "Aspect ratio y/x", value = 1,
                                         min = 0.1, step = 0.1)),
          column(width = 2, numericInput("cex", "Magnify", value = 1.2,
                                         min = 0, step = 0.1))
        )
      ),

      fluidRow(
        fluidRow(
          column(width = 2, textInput("bgcol", "Background color",
                                      value = "black")),
          column(width = 2, textInput("axcol", "Axes color", value = "white")),
          column(width = 2, numericInput("pch", "Point type", value = 1,
                                         min = 0, max = 20, step = 1)),
          column(width = 2, numericInput("ptcex", "Point size", value = 1,
                                         min = 0, step = 0.1)),
          column(width = 2,
                 conditionalPanel(
                   "input.space == 'Environmental'",
                   textInput("pcol", "Point color", value = "gray65")),
                 conditionalPanel(
                   "input.space == 'Geographic'",
                   sliderInput("xlim", "X limits", min = -1, max = 1,
                               ticks = FALSE, value = c(-0.5, 0.5), step = 0.1)
                 )),
          column(width = 2,
                 conditionalPanel(
                   "input.space == 'Environmental'",
                   numericInput("palp", "Point opacity", value = 1,
                                min = 0, max = 1, step = 0.1)),
                 conditionalPanel(
                   "input.space == 'Geographic'",
                   sliderInput("ylim", "Y limits", min = -1, max = 1,
                               ticks = FALSE, value = c(-0.5, 0.5), step = 0.1)
                 ))
        )
      ),

      fluidRow(
        fluidRow(
          column(width = 2,
                 conditionalPanel(
                   "input.space == 'Environmental'",
                   numericInput("lty", "Line type", value = 1, min = 0,
                                max = 6, step = 1))),
          column(width = 2,
                 conditionalPanel(
                   "input.space == 'Environmental'",
                   numericInput("lwd", "Line width", value = 1, min = 1))),
          column(width = 2,
                 conditionalPanel(
                   "input.space == 'Environmental'",
                   textInput("lcol", "Line color", value = "yellow"))),
          column(width = 2,
                 conditionalPanel(
                   "input.space == 'Environmental'",
                   numericInput("lalp", "Line opacity", value = 1,
                                min = 0, max = 1, step = 0.1)))
        )
      ),

      ## export plot
      fluidRow(style = "height:10px"),
      fluidRow(
        h4("Export plot:"),
        fluidRow(
          column(width = 2, textInput("fnam", "File name",
                                      value = "Vniche_plot")),
          column(width = 2, selectInput("form", "Format", selected = "PNG",
                                        choices = c("PNG", "JPG",
                                                    "TIFF", "BMP"))),
          column(width = 2, numericInput("pwi", "Width", value = 7, min = 0)),
          column(width = 2, numericInput("phe", "Height", value = 7, min = 0)),
          column(width = 2, selectInput("units", "Units", selected = "in",
                                        choices = c("px", "mm", "cm", "in"))),
          column(width = 2, numericInput("res", "Resolution (ppi)", value = 300,
                                         min = 0))
        ),

        fluidRow(
          column(
            width = 2,
            shinyFiles::shinyDirButton(id = "pedir",
                                       label = "Directory",
                                       title = "Select directory")
          ),
          column(width = 2, actionButton("pexport", label = "Export plot"))
        ),
        br()
      )
    )
  ),
  # ---------------

  # ---------------
  # right panel
  rightsidebar = shinydashboardPlus::rightSidebar(
    width = 400,
    column(
      width = 12,
        # Analysis options
        h4("Analysis options:"),

        ## new data from ellipsoids
        h5("Generate new data"),
        fluidRow(
          column(width = 6, strong("Using")),
          column(width = 3, strong("N"))
        ),
        fluidRow(
          column(width = 6, selectInput("ndfrom", NULL,
                                        choices = c("Ellipsoid", "Background"))),
          column(width = 3, textInput("ndatn", NULL, value = 100)),
          column(width = 3, actionButton("ndrun", label = "Run"))
        ),

        ## other calculations
        tags$h5("Calculations"),
        fluidRow(
          column(width = 6, strong("Mahalanobis / Suitability")),
          column(width = 6, strong("Truncate suitability"))
        ),
        fluidRow(
          column(width = 6, actionButton("calcrun", label = "Run calculations")),
          column(width = 6, selectInput("trunc", NULL, selected = "TRUE",
                                        choices = c("TRUE", "FALSE")))
        ),

        ## display options
        h5("Display predictions"),
        fluidRow(
          column(width = 6, selectInput("dispon", "Display on",
                                        selected = "New data",
                                        choices = c("New data", "Background"))),
          column(width = 6, selectInput("dispred", "Prediction",
                                        selected = "Mahalanobis",
                                        choices = c("Mahalanobis",
                                                    "Suitability",
                                                    "Suitability trunc.")))
        ),

        ## color for predictions
        fluidRow(
          column(width = 6, selectInput("dispal", "Color palette",
                                        selected = "ca. viridis",
                                        choices = c("ca. viridis", "ca. magma",
                                                    "blues", "heat", "terrain",
                                                    "topo", "rev. terrain",
                                                    "bpy")))
        ),

        ## export result options
        fluidRow(style = "height:5px"),
        h4("Export results:"),

        ### directory
        shinyFiles::shinyDirButton(id = "redir",
                                   label = "Directory for results",
                                   title = "Select directory"),
        br(), br(),

        ### export metadata
       strong("Niche metadata (new folder name)"),
        fluidRow(
          column(width = 8, textInput("nmeta", NULL,
                                      value = "Niche_meta")),
          column(width = 4, actionButton("metexp", label = "Export"))
        ),

        ### export new data
        tags$b(tags$small("New data")),
        fluidRow(
          column(width = 8, textInput("nnew", NULL,
                                      value = "New_data")),
          column(width = 4, actionButton("newexp", label = "Export"))
        ),

        ### export mahalanobis
        tags$b(tags$small("Mahalanobis")),
        fluidRow(
          column(width = 8, textInput("nmaha", NULL,
                                      value = "Maha_dist")),
          column(width = 4, actionButton("mahexp", label = "Export"))
        ),

        ### export suitability
        tags$b(tags$small("Suitability")),
        fluidRow(
          column(width = 8, textInput("nsuit", NULL,
                                      value = "Suitability")),
          column(width = 4, actionButton("suiexp", label = "Export"))
        ),

        ### export truncated suitability
        tags$b(tags$small("Suitability truncated")),
        fluidRow(
          column(width = 8, textInput("nsuitin", NULL,
                                      value = "Suitability_truncated")),
          column(width = 4, actionButton("suinexp", label = "Export"))
        )
    )
  )
  # ---------------
)


# Server
server <- function(input, output, session) {
  # ---------------
  # directories and file names
  roots <- c(
    shinyFiles::getVolumes()(),
    "Current Working Directory" = '.',
    "Home" = fs::path_home()
  )

  shinyFiles::shinyFileChoose(input = input, id = "file_data1",
                              session = session, roots = roots)
  shinyFiles::shinyFileChoose(input = input, id = "file_data2",
                              session = session, roots = roots)

  shinyFiles::shinyDirChoose(input = input, id = "pedir",
                             session = session, roots = roots)
  shinyFiles::shinyDirChoose(input = input, id = "redir",
                             session = session, roots = roots)
  # ---------------

  # ---------------
  # reactive values
  datain <- reactiveValues(csv = NULL, raster = NULL, vcsv = NULL, vcsvs = NULL,
                           vraster = NULL, vrasters = NULL, lonlat = NULL)
  rbase <- reactiveValues(raslay = NULL)
  datauser <- reactiveValues(varus = NULL, namran = NULL, elevel = NULL)
  vnvacv <- reactiveValues(vars = NULL, covs = NULL, covsel = NULL,
                           coval = NULL)
  ppar <- reactiveValues(cx = NULL, cb = NULL, ca = NULL, cl = NULL, cp = NULL,
                         pcx = NULL , lty = NULL, lwd = NULL, pch = NULL ,
                         asp = NULL, mar = NULL , mfrow = NULL)
  explot <- reactiveValues(fnam = NULL, form = NULL, pwi = NULL, phe = NULL,
                           units = NULL, pedir = NULL)
  calcdat <- reactiveValues(metadat = NULL, newdat = NULL, trunc = NULL)

  observe({
    ppar$cx <- input$cex
    ppar$cb <- input$bgcol
    ppar$ca <- input$axcol
    ppar$cl <- scales::alpha(input$lcol, input$lalp)
    ppar$cp <- scales::alpha(input$pcol, input$palp)
    ppar$lty <- input$lty
    ppar$lwd <- input$lwd
    ppar$asp <- input$asp
    ppar$pcx <- input$ptcex
    ppar$pch <- input$pch
    ppar$mar <- c(input$marbo, input$marle, input$marto, input$marri)
  })

  observe({
    explot$fnam <- input$fnam
    explot$form <- input$form
    explot$pwi <- input$pwi
    explot$phe <- input$phe
    explot$units <- input$units
    explot$res <- input$res
  })
  # ---------------

  # ---------------
  # reading data
  ## csv files
  observeEvent(input$file_data1, {
    tryCatch({
      paths <- shinyFiles::parseFilePaths(roots = roots, input$file_data1)
      if (dim(paths)[1] == 1) {datain$csv <- paths}
    },
    error = function(err) {
      showNotification(ui =  paste0(err, ". File couldn't be selected"),
                       duration = 5, type = "error")
    })
  })

  ## raster files
  observeEvent(input$file_data2, {
    tryCatch({
      paths <- shinyFiles::parseFilePaths(roots = roots, input$file_data2)
      if (dim(paths)[1] >= 2) {datain$raster <- paths}
    },
    error = function(err) {
      showNotification(ui =  paste0(err, ". At least 2 layers are needed"),
                       duration = 5, type = "error")
    })
  })

  ## csv variables available
  output$varscsv <- renderText({
    if (!is.null(datain$csv)) {
      fipath <- as.character(datain$csv$datapath)

      dcsv <- data.table::fread(fipath, sep = input$sep)
      datain$vcsv <- dcsv
      dnam <- colnames(datain$vcsv)

      if (input$lon != "" & input$lat != "") {
        datain$lonlat <- c(input$lon, input$lat)
        dnam <- dnam[!dnam %in% datain$lonlat]
      }

      if (nrow(datain$vcsv) > input$nback) {
        datain$vcsvs <- datain$vcsv[sample(nrow(datain$vcsv), input$nback),
                                    !dnam %in% datain$lonlat]
      } else {
        datain$vcsvs <- datain$vcsv[, !dnam %in% datain$lonlat]
      }

      paste(dnam, collapse = ",  ")
    }
  })

  ## raster variables available
  output$varsras <- renderText({
    if (!is.null(datain$raster)) {
      fipath <- as.character(datain$raster$datapath)

      vsta <- raster::stack(fipath)
      dnam <- names(vsta)

      datain$vraster <- na.omit(cbind(cell = 1:(raster::ncell(vsta)), vsta[]))
      if (nrow(datain$vraster) > input$nback) {
        datain$vrasters <- datain$vraster[sample(nrow(datain$vraster),
                                                 input$nback), -1]
      } else {
        datain$vrasters <- datain$vraster[, -1]
      }
      rbase$raslay <- vsta[[1]]

      paste(dnam, collapse = ",  ")
    }
  })
  # ---------------

  # ---------------
  # variances
  observe({
    vin <- c(
      ifelse(sum(c(input$v1nam != "", input$v1min != "",
                   input$v1max != "")) == 3, 1, 0),
      ifelse(sum(c(input$v2nam != "", input$v2min != "",
                   input$v2max != "")) == 3, 2, 0),
      ifelse(sum(c(input$v3nam != "", input$v3min != "",
                   input$v3max != "")) == 3, 3, 0),
      ifelse(sum(c(input$v4nam != "", input$v4min != "",
                   input$v4max != "")) == 3, 4, 0)
    )
    datauser$varus <- which(vin > 0) # if vars are not 1 and 2

    if (length(datauser$varus) >= 2) {
      if (identical(datauser$varus, 1:length(datauser$varus))) {
        ranges <- sapply(datauser$varus, function(x){
          c(as.numeric(unlist(input[[paste0("v", x, "min")]])),
            as.numeric(unlist(input[[paste0("v", x, "max")]])))
        })

        colnames(ranges) <- sapply(datauser$varus, function(x) {
          input[[paste0("v", x, "nam")]]
        })
        #print(ranges)
        datauser$namran <- ranges
      } else {
        showNotification(ui = "Please use variable boxes in order",
                         duration = 5, type = "error")
      }
    }

    if(!is.null(datauser$namran)) {
      vnvacv$vars <- var_from_range(datauser$namran)
    }
  })

  # covariance limits
  observe({
    if (!is.null(datauser$namran)) {
      vnvacv$covs <- covariance_limits(range = datauser$namran)

      combs <- combn(datauser$varus, 2)
      combs <- vapply(1:ncol(combs), FUN.VALUE = character(1), function(x) {
        paste0(combs[, x], collapse = "")
      })
      vnvacv$covsel <- paste0("cov", combs)

      mms <- lapply(1:nrow(vnvacv$covs), function(x){
        as.list(round_covlimstep(vnvacv$covs[x, 1], vnvacv$covs[x, 2]))
      })
      names(mms) <- vnvacv$covsel

      for (i in vnvacv$covsel) {
        updateSliderInput(session, i, value = 0, min = mms[[i]][["min"]],
                          max = mms[[i]][["max"]], step = mms[[i]][["step"]])
      }
    }
  })
  # ---------------

  # ---------------
  # visualization options
  #observe({
  #  if (!is.null(rbase$raslay)) {
  #    updateSelectInput(session, "space",
  #                      choices = c("Environmental", "Geographic"))
  #  } else {
  #    updateSelectInput(session, "space",
  #                      choices = c("Environmental"))
  #  }
  #})

  # environmental plot
  #output$pplote <- renderPlot({
  #  ## when ellipsoid is defined
  #  if (!is.null(vnvacv$covs)) {
  #    ### preparing data and parameters
  #    datauser$elevel <- input$elevel
  #
  #    nl <- length(vnvacv$covs)
  #    ppar$mfrow <- c(ceiling(nl / ceiling(sqrt(nl))), ceiling(sqrt(nl)))
  #
  #    par(mfrow = ppar$mfrow, bg = ppar$cb, cex = ppar$cx, mar = ppar$mar,
  #        col = ppar$ca, col.axis = ppar$ca, col.lab = ppar$ca, fg = ppar$ca)
  #
  #    covsval <- vector("numeric")
  #    for (i in 1:length(vnvacv$covsel)) {
  #      covsval[i] <- input[[vnvacv$covsel[i]]]
  #    }
  #    vnvacv$coval <- covsval
  #
  #    matvc <- var_cov_matrix(vnvacv$vars, covariances = vnvacv$coval)
  #    cents <- apply(datauser$namran, 2, mean)
  #
  #    sel <- gsub("cov", "", vnvacv$covsel)
  #
  #    ### plotting
  #    for (i in 1:length(vnvacv$covsel)) {
  #      scv <- as.numeric(strsplit(sel[i], "")[[1]])
  #      vp <- datauser$varus[scv]
  #
  #      ellc1 <- ellipse::ellipse(x = matvc[scv, scv], centre = cents[scv],
  #                                level = datauser$elevel / 100)
  #
  #      if (!is.null(datain$vrasters) & input$backt == "Raster") {
  #        blims <- apply(rbind(datain$vrasters[, vp], ellc1), 2, range)
  #        plot(blims, col = NA, asp = ppar$asp)
  #        points(datain$vrasters[, vp], col = ppar$cp, pch = ppar$pch,
  #               cex = ppar$pcx)
  #      }
  #
  #      if (!is.null(datain$vcsvs) & input$backt == "CSV") {
  #        blims <- apply(rbind(datain$vcsvs[, vp], ellc1), 2, range)
  #        plot(blims, col = NA, asp = ppar$asp)
  #        points(datain$vcsvs[, vp], col = ppar$cp, pch = ppar$pch,
  #               cex = ppar$pcx)
  #      }
  #
  #      if (!is.null(datain$vrasters) & !is.null(datain$vcsvs)) {
  #        blims <- apply(ellc1, 2, range)
  #        plot(blims, col = NA, asp = ppar$asp)
  #      }
  #
  #      lines(ellc1, col = ppar$cl, lty = ppar$lty, lwd = ppar$lwd)
  #    }
  #
  #
  #  } else {
  #    ## only background
  #    par(bg = ppar$cb, cex = ppar$cx, mar = ppar$mar, col = ppar$ca,
  #        col.axis = ppar$ca, col.lab = ppar$ca, fg = ppar$ca)
  #
  #    if (!is.null(datain$vrasters) & input$backt == "Raster") {
  #      blims <- apply(datain$vrasters[, 1:2], 2, range)
  #      plot(blims, col = NA, asp = ppar$asp)
  #      points(datain$vrasters[, 1:2], col = ppar$cp, pch = ppar$pch,
  #             cex = ppar$pcx)
  #    }
  #
  #    if (!is.null(datain$vcsvs) & input$backt == "CSV") {
  #      blims <- apply(datain$vcsvs[, 1:2], 2, range)
  #      plot(blims, col = NA, asp = ppar$asp)
  #      points(datain$vcsvs[, 1:2], col = ppar$cp, pch = ppar$pch,
  #             cex = ppar$pcx)
  #    }
  #  }
  #})

  # geographic plot
  #output$pplotg <- renderPlot({
  #  if (!is.null(datain$raster)) {
  #    par(bg = ppar$cb, cex = ppar$cx, col = ppar$ca, col.axis = ppar$ca,
  #        col.lab = ppar$ca, fg = ppar$ca)
  #    raster::plot(rbase$raslay, asp = ppar$asp)
  #  }
  #})
  # ---------------

  # ---------------
  # export plot
  #observeEvent(input$pexport, {
  #
  #})
  # ---------------

  # ---------------
  # analysis
  ## metadata generation
  #observe({
  #  if(!is.null(datain$vcsv) | !is.null(datain$vraster)) {
  #    updateSelectInput(session, "ndfrom",
  #                      choices = c("Ellipsoid", "Background"))
  #    updateSelectInput(session, "dispon",
  #                      choices = c("New data", "Background"))
  #  } else {
  #    updateSelectInput(session, "ndfrom",
  #                      choices = c("Ellipsoid"))
  #    updateSelectInput(session, "dispon",
  #                      choices = c("New data"))
  #  }
  #})

  ## calculations
  #observeEvent(input$ndrun, {
  #
  #})
  #
  #observeEvent(input$calcrun, {
  #
  #})

  ## display results
  #observe({
  #  calcdat$trunc <- input$trunc
  #  if (calcdat$trunc == FALSE) {
  #    updateSelectInput(session, "dispred",
  #                      choices = c("Mahalanobis", "Suitability"))
  #  } else {
  #    updateSelectInput(session, "dispred",
  #                      choices = c("Mahalanobis", "Suitability",
  #                                  "Suitability trunc."))
  #  }
  #})
  # ---------------

  # ---------------
  # export results

  # ---------------
}


# shiny app
shinyApp(ui = ui, server = server)
