body_download_tabItem <- tabItem("download",
            fluidRow(
              box(
                title = "Downloader for databases",
                width = 12,
                status = "primary",
                checkboxInput("show.all.names", label = "Show all download names"),
                selectInput("download.name", "Name of Database",
                            choices = download.database(show.all.names = TRUE), multiple = TRUE),
                textInput("version", "Version of Database"),
                checkboxInput("show.all.versions", label = "Show all versions of database"),
                checkboxInput("show.all.buildvers", label = "Show all buildver"),
                #selectInput("database.dir.1", "Directory stored databases (Auto)",
                #            choices = list.dirs('.')),
                textInput("database.dir", "Directory stored databases",
                          normalizePath(tempdir(), winslash = "/")),
                checkboxInput("verbose", label = "Print debug message", value = TRUE),
                textInput("extra.paramters", "Extra parameters", "list()"),
                actionButton("download_run", "Run")
              )
            ),
            fluidRow(
              box(
                title = "Output",
                width = 6,
                height = "100%",
                status = "warning",
                verbatimTextOutput("download_output")
              ),
              box(
                title = "Output of Log",
                width = 6,
                height = "100%",
                status = "warning",
                verbatimTextOutput("log_output")
              )
            )
    )