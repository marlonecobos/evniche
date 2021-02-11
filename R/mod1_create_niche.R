# UI
mod_create_niche_ui <- function(id) {
  ns <- NS(id)

  # conditions for covariance slidebars
  cond_csv <- paste0("input['", ns("backt"), "'] == 'CSV'")
  cond_raster <- paste0("input['", ns("backt"), "'] == 'Raster'")
  cond_none <- paste0("input['", ns("backt"), "'] != 'NONE'")

  cond_v12 <- paste0("input['", ns("v1nam"), "'] != '' & input['",
                     ns("v1min"), "'] != '' & input['", ns("v1max"),
                     "'] != '' & input['", ns("v2nam"), "'] != '' & input['",
                     ns("v2min"), "'] != '' & input['", ns("v2max"), "'] != ''")
  cond_v3 <- paste0("input['", ns("v3nam"), "'] != '' & input['",
                    ns("v3min"), "'] != '' & input['", ns("v3max"), "'] != ''")
  cond_v4 <- paste0("input['", ns("v4nam"), "'] != '' & input['",
                    ns("v4min"), "'] != '' & input['", ns("v4max"), "'] != ''")

  # complete panel
  tabPanel(
    title = "1. Create evniche",

    column(
      width = 12,
      fluidRow(
        # ---------------
        # left panel
        column(
          width = 3,

          sidebarPanel(
            width = 12,
            tags$h4("Define niche parameters:"),

            ## data
            tags$h5("Data:"),
            fluidRow(
              column(width = 5, tags$b(tags$small("Initial data"))),
              column(width = 4),
              column(width = 3, conditionalPanel(cond_csv,
                                                 tags$b(tags$small("Sep.")))),
              style = "height:25px"
            ),
            fluidRow(
              column(
                width = 5,
                selectInput(ns("backt"), NULL, choices = c("NONE", "CSV",
                                                           "Raster"))
              ),
              column(
                width = 4,
                conditionalPanel(
                  cond_csv,
                  shinyFiles::shinyFilesButton(
                    id = ns("file_data1"), label = "Select file",
                    title = "Select file (.csv)", multiple = FALSE,
                    style = "background-color:#305C98"
                  )
                ),
                conditionalPanel(
                  cond_raster,
                  shinyFiles::shinyFilesButton(
                    id = ns("file_data2"), label = "Select files",
                    title = "Select files (.tif, .asc, or .bil)", multiple = TRUE,
                    style = "background-color:#305C98"
                  )
                )
              ),
              column(width = 3,
                     conditionalPanel(cond_csv,
                                      textInput(ns("sep"), NULL, value = ",")))
            ),

            fluidRow(
              conditionalPanel(cond_csv,
                               column(width = 6,
                                      textInput(ns("lon"), "Longitude (optional)",
                                                value = "")),
                               column(width = 6,
                                      textInput(ns("lat"), "Latitude (optional)",
                                                value = "")))
            ),

            ## variables
            fluidRow(
              column(width = 12,
                     conditionalPanel(cond_none,
                                      tags$b(tags$small("Available variables"))))
            ),
            fluidRow(
              column(width = 12,
                     conditionalPanel(cond_none,
                                      textOutput(outputId = ns("vars"))))
            ),

            fluidRow(style = "height:10px"),
            tags$h5("Variable names and ranges:"),
            fluidRow(
              column(width = 6, textInput(ns("v1nam"), "Variable 1",
                                          value = "")),
              column(width = 3, textInput(ns("v1min"), "Minimum", value = "")),
              column(width = 3, textInput(ns("v1max"), "Maximum", value = ""))
            ),

            fluidRow(
              column(width = 6, textInput(ns("v2nam"), "Variable 2", value = "")),
              column(width = 3, textInput(ns("v2min"), "Minimum", value = "")),
              column(width = 3, textInput(ns("v2max"), "Maximum", value = ""))
            ),

            fluidRow(
              column(width = 6, textInput(ns("v3nam"), "Variable 3", value = "")),
              column(width = 3, textInput(ns("v3min"), "Minimum", value = "")),
              column(width = 3, textInput(ns("v3max"), "Maximum", value = ""))
            ),

            fluidRow(
              column(width = 6, textInput(ns("v4nam"), "Variable 4", value = "")),
              column(width = 3, textInput(ns("v4min"), "Minimum", value = "")),
              column(width = 3, textInput(ns("v4max"), "Maximum", value = ""))
            ),

            ## covariances
            conditionalPanel(
              cond_v12,

              fluidRow(style = "height:10px"),
              tags$h5("Covariance values"),
              sliderInput(ns("cov12"), "Variables 1-2", min = -1, max = 1,
                          value = 0, step = 0.1),

              conditionalPanel(
                cond_v3,
                sliderInput(ns("cov13"), "Variables 1-3", min = -1, max = 1,
                            value = 0, step = 0.1),
                conditionalPanel(
                  cond_v4,
                  sliderInput(ns("cov14"), "Variables 1-4", min = -1, max = 1,
                              value = 0, step = 0.1)
                ),

                sliderInput(ns("cov23"), "Variables 2-3", min = -1, max = 1,
                            value = 0, step = 0.1),

                conditionalPanel(
                  cond_v4,
                  sliderInput(ns("cov24"), "Variables 2-4", min = -1, max = 1,
                              value = 0, step = 0.1),
                  sliderInput(ns("cov34"), "Variables 3-4", min = -1, max = 1,
                              value = 0, step = 0.1)
                )
              )
            )
          )
        ),
        # ---------------

        # ---------------
        # main panel
        column(
          width = 6,

          ## visualization
          fluidRow(
            fluidRow(column(width = 12, tags$h4("Visualization:"))),

            fluidRow(
              column(width = 3,
                     selectInput(ns("ndim"), "Number of dimensions",
                                 selected = "2D", choices = c("2D", "3D"))),
              column(width = 6),
              column(width = 3,
                     conditionalPanel(
                       cond_none,
                       numericInput(ns("nback"), "Max. background",
                                    value = 1000, min = 1, step = 100)))
            ),

            plotOutput(outputId = ns("pplote"))
          ),

          ## graphical parameters
          br(),
          fluidRow(
            tags$h4("Graphical parameters:"),
            fluidRow(
              column(width = 2, numericInput(ns("marbo"), "Margin bottom",
                                             value = 4.5, min = 0, step = 0.1)),
              column(width = 2, numericInput(ns("marle"), "Margin left",
                                             value = 4.5, min = 0, step = 0.1)),
              column(width = 2, numericInput(ns("marto"), "Margin top",
                                             value = 1, min = 0, step = 0.1)),
              column(width = 2, numericInput(ns("marri"), "Margin right",
                                             value = 1, min = 0, step = 0.1)),
              column(width = 2, numericInput(ns("asp"), "Aspect ratio y/x",
                                             value = 1, min = 0.1, step = 0.1)),
              column(width = 2, numericInput(ns("cex"), "Magnify", value = 1.2,
                                             min = 0, step = 0.1))
            )
          ),

          fluidRow(
            fluidRow(
              column(width = 2,
                     selectInput(ns("bgcol"), "Background color",
                                 selected = "black",
                                 choices = c("black", "white", "gray65"))),
              column(width = 2,
                     selectInput(ns("axcol"), "Axes color", selected = "white",
                                 choices = c("white", "black", "gray65"))),
              column(width = 2, numericInput(ns("pch"), "Point type", value = 1,
                                             min = 0, max = 20, step = 1)),
              column(width = 2, numericInput(ns("ptcex"), "Point size",
                                             value = 1, min = 0, step = 0.1)),
              column(width = 2,
                     selectInput(ns("pcol"), "Point color", selected = "green",
                                 choices = c("green", "white", "yellow",
                                             "black", "gray65"))),
              column(width = 2,
                     numericInput(ns("palp"), "Point opacity", value = 1,
                                  min = 0, max = 1, step = 0.1))
            )
          ),

          fluidRow(
            fluidRow(
              column(width = 2, selectInput(ns("lty"), "Line type",
                                            selected = 1, choices = 0:6)),
              column(width = 2, numericInput(ns("lwd"), "Line width", value = 1,
                                             min = 0.1, step = 0.1)),
              column(width = 2,
                     selectInput(ns("lcol"), "Line color", selected = "yellow",
                                 choices = c("yellow", "green", "white",
                                             "black", "gray65"))),
              column(width = 2, numericInput(ns("lalp"), "Line opacity", value = 1,
                                             min = 0, max = 1, step = 0.1))
            )
          )
        ),
        # ---------------

        # ---------------
        # right panel
        column(
          width = 3,
          sidebarPanel(
            width = 12,

            ## results
            tags$h4("Virtual niche features:"),
            # limit and object name
            fluidRow(
              column(width = 4, tags$b(tags$small("Limit"))),
              column(width = 5, tags$b(tags$small("Object name"))),
              column(width = 3),
              style = "height:25px"
            ),

            fluidRow(
              column(width = 4,
                     numericInput(ns("elevel"), NULL, value = 0.99,
                                  min = 0.01, max = 0.99, step = 0.01)),
              column(width = 5,
                     textInput(ns("obnam"), NULL, value = "evniche1")),
              column(width = 3,
                     actionButton(ns("fixo"), label = "Fix",
                                  style = "background-color:#128BCC"))
            ),

            # other features
            fluidRow(column(width = 12, tags$b(tags$small("Centroid"))),
                     style = "height:25px"),
            tableOutput("centr"),

            fluidRow(column(width = 12, tags$b(tags$small("Covariance matrix"))),
                     style = "height:25px"),
            tableOutput("comat"),

            fluidRow(column(width = 12, tags$b(tags$small("Semi-axes length"))),
                     style = "height:25px"),
            tableOutput("saxle"),

            fluidRow(column(width = 12, tags$b(tags$small("Volume"))),
                     style = "height:25px"),
            textOutput("volu"),

            ## exporting ellipsoid meta
            br(),
            tags$h4("Export results:"),

            ### directory
            shinyFiles::shinyDirButton(id = ns("redir"),
                                       label = "Directory for results",
                                       title = "Select directory",
                                       style = "background-color:#4F5560"),
            br(),
            fluidRow(style = "height:15px"),

            ### export metadata
            tags$h5("Ellipsoid metadata:"),
            fluidRow(
              column(width = 8,
                     tags$b(tags$small("Niche metadata (new folder name)"))),
              column(width = 4),
              style = "height:25px"
            ),
            fluidRow(
              column(width = 8, textInput(ns("nmeta"), NULL,
                                          value = "Niche_meta")),
              column(width = 4, actionButton(ns("metexp"), label = "Export",
                                             style = "background-color:#F16D34"))
            ),

            ## exporting plot
            #fluidRow(style = "height:15px"),
            tags$h5("Virtual niche plot:"),
            fluidRow(
              column(width = 8,
                     tags$b(tags$small("File name"))),
              column(width = 4),
              style = "height:25px"
            ),
            fluidRow(
              column(width = 8, textInput(ns("fnam"), NULL,
                                          value = "evniche_plot")),
              column(width = 4, actionButton(ns("pexport"), label = "Export",
                                             style = "background-color:#F16D34"))
            ),
            fluidRow(
              column(width = 4, selectInput(ns("form"), "Format",
                                            selected = "PNG",
                                            choices = c("PNG", "JPG",
                                                        "TIFF", "BMP"))),
              column(width = 4, numericInput(ns("pwi"), "Width", value = 7,
                                             min = 0)),
              column(width = 4, numericInput(ns("phe"), "Height", value = 7,
                                             min = 0)),
            ),
            fluidRow(
              column(width = 4,
                     selectInput(ns("units"), "Units", selected = "in",
                                 choices = c("px", "mm", "cm", "in"))),
              column(width = 4, numericInput(ns("res"), "Resolution (ppi)",
                                             value = 300, min = 0))
            )
          )
        )
      ),
    )
  )
}


# server
mod_create_niche_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    shinyFiles::shinyDirChoose(input = input, id = "redir",
                               session = session, roots = roots)
    # ---------------

    # ---------------
    # reactive values
    datain <- reactiveValues(csv = NULL, raster = NULL, vcsv = NULL, vraster = NULL,
                             sample = NULL, lonlat = NULL, raslay = NULL)
    datauser <- reactiveValues(varus = NULL, namran = NULL, covmat = NULL,
                               elevel = NULL)
    vnvacv <- reactiveValues(vars = NULL, covs = NULL, covsel = NULL,
                             coval = NULL)
    ppar <- reactiveValues(cx = NULL, cb = NULL, ca = NULL, cl = NULL, cp = NULL,
                           pcx = NULL , lty = NULL, lwd = NULL, pch = NULL ,
                           asp = NULL, mar = NULL , mfrow = NULL)
    export <- reactiveValues(redir = NULL, nmeta = NULL, fnam = NULL, form = NULL,
                             pwi = NULL, phe = NULL, units = NULL, res = NULL)
    ellmeta <- reactiveValues(metadata = NULL)

    observe({
      datauser$elevel <- input$elevel

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
      export$redir <- input$redir
      export$nmeta <- input$nmeta
      export$fnam <- input$fnam
      export$form <- input$form
      export$pwi <- input$pwi
      export$phe <- input$phe
      export$units <- input$units
      export$res <- input$res
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

    ## variables available
    output$vars <- renderText({
      ### csv
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
          datain$sample <- datain$vcsv[sample(nrow(datain$vcsv), input$nback),
                                       !dnam %in% datain$lonlat]
        } else {
          datain$sample <- datain$vcsv[, !dnam %in% datain$lonlat]
        }

        paste(dnam, collapse = ",  ")
      }

      ### raster
      if (!is.null(datain$raster)) {
        fipath <- as.character(datain$raster$datapath)

        vsta <- raster::stack(fipath)
        dnam <- names(vsta)

        datain$vraster <- na.omit(cbind(cell = 1:(raster::ncell(vsta)), vsta[]))
        if (nrow(datain$vraster) > input$nback) {
          datain$sample <- datain$vraster[sample(nrow(datain$vraster),
                                                 input$nback), -1]
        } else {
          datain$sample <- datain$vraster[, -1]
        }
        datain$raslay <- vsta[[1]]

        paste(dnam, collapse = ",  ")
      }
    })
    # ---------------

    # ---------------
    # variance covariance matrix
    ## variances
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
          datauser$namran <- ranges
        } else {
          showNotification(ui = "Please use variable boxes in order", ### not working
                           duration = 5, type = "error")
        }
      }

      if(!is.null(datauser$namran)) {
        vnvacv$vars <- var_from_range(datauser$namran)
      }
    })

    ## covariance limits
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
          updateSliderInput(session, ns(i), value = 0, min = mms[[i]][["min"]],
                            max = mms[[i]][["max"]], step = mms[[i]][["step"]])
        }
      }
    })

    ## variance covariance matrix
    observe({
      if (!is.null(vnvacv$vars) & !is.null(vnvacv$covsel)) {
        vnvacv$coval <- vapply(vnvacv$covsel, FUN.VALUE = numeric(1), function(x) {
          input[[x]]
        })

        datauser$covmat <- var_cov_matrix(vnvacv$vars, vnvacv$coval)
      }
    })
    # ---------------

    # ---------------
    # ellipsoid niche
    output$ellmet <- renderUI({
      if (!is.null(datauser$covmat)) {
        ## centroid
        cent <- centroid(range = datauser$namran)

        ## ellipsoid characteristics
        ellmeta$metadata <- ell_features(centroid = cent,
                                         covariance_matrix = datauser$covmat,
                                         level = datauser$elevel)

        HTML(print_ell_meta(ellmeta$metadata, F))
      }
    })
    # ---------------
  })
}


ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  title = "evniche",
  mod_create_niche_ui("create_niche")
)

server <- function(input, output, session) {
  mod_create_niche_server("create_niche")
}

shinyApp(ui, server)
