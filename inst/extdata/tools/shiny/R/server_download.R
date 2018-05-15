parse_buildver_object <- function(download.name, download.version) {
    buildver_object <- download.database(download.name,
                      version = download.version,
                      show.all.buildvers = TRUE)
    x <- unique(unname(unlist(buildver_object)))
    return(x)
}

download_section_server <- function(input, output) {
  observeEvent(input$download_run, {
    req(input$download.name)
    req(input$download.version)

      if (input$database.dir == "") {
        database.dir <- tempdir()
      } else {
        database.dir <- input$database.dir
      }
      version <- input$download.version
      buildver <- input$download.buildver
      if (any(input$download.version == "")) {
        index <- input$download.version == ""
        version[index] <- download.database(download.name = input$download.name[index],
                                                                   show.all.versions = TRUE)[1]
        input$download.version <- version
      }
      if (any(input$download.buildver == "")) {
        index <- input$download.buildver == ""
        buildver[index] <- download.database(download.name = input$download.name[index],
                                             version = version[index], show.all.buildvers = TRUE)[1]
      }
      if (any(input$download.name != "")) {
        extra.params <- eval(parse(text = input$extra.paramters))
        params <- list(download.name = input$download.name, database.dir = database.dir,
                       verbose = TRUE, version = version, buildver = buildver)
        params <- config.list.merge(params, extra.params)
        progress <- shiny::Progress$new()
        for (i in 1:100) progress$set(message = "Running download.database function,  waiting please...",
                                      value = i/100)
        Sys.sleep(2)
        on.exit(progress$close())
        print(params)
        messg <- capture.output(string <- do.call("download.database", params))
      }
  })
  observeEvent(input$download.name, {
    output$download.version.selector <- shiny::renderUI({
      selectInput("download.version", "Version of Database",
                  choices = download.database(input$download.name, show.all.versions = TRUE), multiple = TRUE)
    })
    observeEvent(input$download.version, {
      output$download.buildver.selector <- shiny::renderUI({
        selectInput("download.buildver", "Buildver version of Database",
                    choices = parse_buildver_object(download.name = input$download.name,
                                                    download.version = input$download.version),
                    multiple = FALSE)
      })
    })
  })
  return(output)
}
