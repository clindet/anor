body_download_tabItem <- tabItem("download",
            fluidRow(
              box(
                title = "Downloader for databases",
                width = 12,
                status = "primary",
                selectInput("download.name", "Name of Database",
                            choices = download.database(show.all.names = TRUE), multiple = TRUE),
                shiny::uiOutput("download.version.selector"),
                shiny::uiOutput("download.buildver.selector"),
                textInput("database.dir", "Directory stored databases",
                          normalizePath(tempdir(), winslash = "/")),
                textInput("extra.paramters", "Extra parameters", "list()"),
                actionButton("download_run", "Run")
              )
            )
    )
