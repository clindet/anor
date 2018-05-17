body_download_tabItem <- tabItem("download",
            fluidRow(
              box(
                title = "Downloader for databases",
                width = 12,
                status = "primary",
                selectInput("download.name", "Name of Database",
                            choices = annovarR::download.database(show.all.names = TRUE), multiple = TRUE),
                shiny::uiOutput("download.version.selector"),
                shiny::uiOutput("download.buildver.selector"),
                shiny::uiOutput("database.dir.input"),
                textInput("extra.paramters", "Extra parameters", "list(license = '')"),
                actionButton("download_run", "Run")
              )
            )
    )
