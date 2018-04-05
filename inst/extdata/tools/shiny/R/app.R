pkgs.shiny <- c("shinydashboard", "configr", "annovarR", "data.table", "shinyjs", "DT", "DBI", "RSQLite")
sapply(pkgs.shiny, function(x) {
  status <- suppressWarnings(require(x, character.only = TRUE))
  if (!status) {
    install.packages(x)
  }
})
for(i in pkgs.shiny) {
  suppressMessages(require(i, character.only = TRUE))
}

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)

# Read configuration file and set the environment vars
config.file <-  Sys.getenv("ANNOVARR_SHINY_CONFIG", 
                             system.file("extdata", "config/shiny.config.toml", package = "annovarR"))
config <- read.config(config.file, file.type = "toml")
db_type <- config$shiny_db$db_type
db_path <- normalizePath(config$shiny_db$db_path, mustWork = FALSE)
upload_table <- config$shiny_db_table$upload_data_table_name
upload_table_colnames <- config$shiny_db_table$upload_data_table_colnames
upload_dir <- normalizePath(config$shiny_upload$upload_dir, mustWork = FALSE)


options(shiny.maxRequestSize=30000*1024^2) 


if (skin == "")
  skin <- "blue"


sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    id = "tabs",
    menuItem("Introduction", tabName = "introduction", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("File Viewer", tabName = "file_viewer", icon = icon("file")),
    menuItem("Upload", tabName = "upload", icon = icon("cloud-upload")),
    menuItem("Downloader", icon = icon("cloud-download"), tabName = "download"),
    menuItem("Charts", icon = icon("bar-chart-o"),
      menuSubItem("Chart sub-item 1", tabName = "subitem1"),
      menuSubItem("Chart sub-item 2", tabName = "subitem2")
    ),
    menuItem("Source code for app", icon = icon("file-code-o"),
      href = "https://github.com/JhuangLab/annovarR/blob/master/R/app.R"
    )
  )
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem("introduction",
            fluidRow(
              box(
                title = "Introduction",
                width = 12,
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                includeMarkdown("https://raw.githubusercontent.com/JhuangLab/annovarR/master/README.md")
              )
            )
    ),
    tabItem("dashboard",
      fluidRow(
        box(
          title = "Distribution",
          status = "primary",
          plotOutput("plot1", height = 240),
          height = 300
        ),
        tabBox(
          height = 300,
          tabPanel("View 1",
            plotOutput("scatter1", height = 230)
          ),
          tabPanel("View 2",
            plotOutput("scatter2", height = 230)
          )
        )
      ),

      # Boxes with solid headers
      fluidRow(
        box(
          title = "Histogram control", width = 4, solidHeader = TRUE, status = "primary",
          sliderInput("count", "Count", min = 1, max = 500, value = 120)
        ),
        box(
          title = "Appearance",
          width = 4, solidHeader = TRUE,
          radioButtons("fill", "Fill", # inline = TRUE,
            c(None = "none", Blue = "blue", Black = "black", red = "red")
          )
        ),
        box(
          title = "Scatterplot control",
          width = 4, solidHeader = TRUE, status = "warning",
          selectInput("spread", "Spread",
            choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80, "100%" = 100),
            selected = "60"
          )
        )
      ),

      # Solid backgrounds
      fluidRow(
        box(
          title = "Title 4",
          width = 4,
          background = "black",
          "A box with a solid black background"
        ),
        box(
          title = "Title 5",
          width = 4,
          background = "light-blue",
          "A box with a solid light-blue background"
        ),
        box(
          title = "Title 6",
          width = 4,
          background = "maroon",
          "A box with a solid maroon background"
        )

      )
    ),
    tabItem("file_viewer",
      fluidRow(
        box(
          title = "File List",
          status = "primary",
          width = 12,
          DT::dataTableOutput("files_info_DT")
        ),
        box(
          title = "File Preview",
          width = 12,
          status = "primary",
          DT::dataTableOutput("file_preview_DT"),
          textOutput("file_preview")
          )
      )
    ),
    tabItem("upload",
            fluidRow(
              box(
                title = "Upload",
                width = 12,
                status = "primary",
                # Input: Select a file ----
                fileInput("upload.file", "Choose Need Uploaded File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     "application/octet-stream",
                                     config$shiny_upload$supported_file_type)),
                selectInput("upload.file.type", "FileType",
                            choices = config$shiny_upload$supported_file_type),
                selectInput("upload.genome.version", "GenomeVersion",
                            choices = config$shiny_upload$supported_genome_version),
                textAreaInput("upload.file.description", "Description", row = 10),
                actionButton("upload_save", "Save", class = "btn btn-primary", disabled = "disabled"),
                inlineCSS(list("#upload_save" = "color: white"))
                ),
              box(
                title = "Preview",
                width = 12,
                status = "primary",
                DT::dataTableOutput("upload_file_preview_DT"),
                textOutput("upload_file_preview")
                )
            )
    ),
    tabItem("download",
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
  )
)

messages <- dropdownMenu(type = "messages",
  messageItem(
    from = "Sales Dept",
    message = "Sales are steady this month."
  ),
  messageItem(
    from = "New User",
    message = "How do I register?",
    icon = icon("question"),
    time = "13:45"
  ),
  messageItem(
    from = "Support",
    message = "The new server is ready.",
    icon = icon("life-ring"),
    time = "2014-12-01"
  )
)

notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning",
  notificationItem(
    text = "5 new users today",
    icon("users")
  ),
  notificationItem(
    text = "12 items delivered",
    icon("truck"),
    status = "success"
  ),
  notificationItem(
    text = "Server load at 86%",
    icon = icon("exclamation-triangle"),
    status = "warning"
  )
)

tasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
  taskItem(value = 90, color = "green",
    "Documentation"
  ),
  taskItem(value = 17, color = "aqua",
    "Project X"
  ),
  taskItem(value = 75, color = "yellow",
    "Server deployment"
  ),
  taskItem(value = 80, color = "red",
    "Overall project"
  )
)

header <- dashboardHeader(
  title = "annovarR Shiny APP",
  messages,
  notifications,
  tasks
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output, session) {

  ## Dashbord section
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    if (is.null(input$count) || is.null(input$fill))
      return()

    data <- histdata[seq(1, input$count)]
    color <- input$fill
    if (color == "none")
      color <- NULL
    hist(data, col = color, main = NULL)
  })

  output$scatter1 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "blue")
  })

  output$scatter2 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "red")
  })

  # File viewr section
  output$files_info_DT <- DT::renderDataTable({
     con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
     sql <- sprintf("SELECT * FROM %s", upload_table)
     if (!upload_table %in% DBI::dbListTables(con)) {
       info <- matrix(data = NA, nrow = 1, ncol = length(upload_table_colnames))
       info <- as.data.frame(info)
       colnames(info) <- upload_table_colnames
       info <- info[-1,]
     } else {
       info <- dbGetQuery(con, sql)
     }
     colnames(info)[1] <- "ID"
     Action <- sprintf("<button id = 'files_view_%s' name = 'files_view_%s' type='button' class = 'btn btn-primary action-button shiny-bound-input'>View</button>", info[,1], info[,1])
     Action <- sprintf("%s <button id = 'files_del_%s' name = 'files_del_%s' type='button' class = 'btn btn-primary action-button shiny-bound-input'>Del</button>", Action, info[,1], info[,1])
     info <- cbind(info[1], Action, info[2:ncol(info)])
     DBI::dbDisconnect(con)
     return(info)
  }, rownames = FALSE, editable = TRUE,
    caption = 'All files stored in annovarR shinyapp Web service', escape = FALSE,
    extensions = c("Buttons", "FixedColumns", "Scroller"),
    options = list(
      autoWidth = TRUE,
      dom = 'Bfrtlip',
      deferRender = TRUE,
      searchHighlight = TRUE,
      scrollX = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      columnDefs = list(list(width = "100px", targets = "_all")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#487ea5', 'color': '#fff'});",
        "}")
      ),
      selection = "none"
  )

con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
if (upload_table %in% DBI::dbListTables(con)) {
  sql <- sprintf("SELECT id FROM %s", upload_table)
  upload_table_data <- DBI::dbGetQuery(con, sql)
  files_ids <- upload_table_data[,1]
  set_preview <- function(id) {
    shinyjs::onclick(sprintf("files_view_%s", id), function(event){
      shinyjs::reset("file_preview_DT")
          output$file_preview_DT <- DT::renderDataTable({
            sql <- sprintf("SELECT file_path,file_type FROM %s where id=%s", upload_table, id)
              upload_table_data <- DBI::dbGetQuery(con, sql)
              print(upload_table_data)
              if (upload_table_data$file_type %in% c("txt", "csv")) {
                file_content <- fread(upload_table_data$file_path)
                return(file_content)
              }
            }, rownames = FALSE, editable = FALSE,
            caption = 'All files stored in annovarR shinyapp Web service', escape = FALSE,
            extensions = c("Buttons", "Scroller"),
            options = list(
              dom = 'Bfrtlip',
              searchHighlight = TRUE,
              scrollX = TRUE,
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#487ea5', 'color': '#fff'});",
                "}")
            ), selection = "single")
    })
  }
  for(i in files_ids) {
    set_preview(i)
  }

}




  # Upload section
  output$upload_file_preview_DT <- DT::renderDataTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$upload.file)
      shinyjs::toggleState(id="upload_save")
      if (input$upload.file.type %in% c("txt", "csv")) {
        df <- fread(input$upload.file$datapath)
        return(df)
      }
  }, 
  extensions = c("Buttons", "FixedColumns", "Responsive"),
    options = list(
      dom = 'Bfrtlip',
      searchHighlight = TRUE,
      scrollX = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#487ea5', 'color': '#fff'});",
        "}")
    ),
    selection = "none"
  )

  output$upload_file_preview <- renderText({
      req(input$upload.file)
      if (!input$upload.file.type %in% c("txt", "csv")) {
        return(input$upload.file$datapath)
      }
  })
  #shinyjs::addClass(class="btn btn-primary", id="upload_save")
  observeEvent(input$upload_save, {
    req(input$upload.file) 

    if (db_type == "sqlite") {
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      if (upload_table %in% DBI::dbListTables(con)) {
        sql <- sprintf("select last_insert_rowid() from %s", upload_table)
        ids <- DBI::dbGetQuery(con, sql)
        id <- as.numeric(rownames(ids)[nrow(ids)]) + 1
      } else {
        id <- 1
      }
      assign(upload_table_colnames[1], input$upload.file$name)
      destfile <- sprintf("%s/%s", upload_dir, id)
      if (!dir.exists(upload_dir)) {
        dir.create(upload_dir, recursive = TRUE)
      }
      assign(upload_table_colnames[7], tools::md5sum(input$upload.file$datapath))
      file.rename(input$upload.file$datapath, destfile)
      assign(upload_table_colnames[2], destfile)
      assign(upload_table_colnames[3], file.size(destfile))
      assign(upload_table_colnames[4], input$upload.file.type)
      assign(upload_table_colnames[5], input$upload.genome.version)
      assign(upload_table_colnames[6], format(Sys.time(), "%Y %H:%M:%S"))
      assign(upload_table_colnames[8], input$upload.file.description)
      row_data <- NULL
      for(var in c("id", upload_table_colnames)) {
        row_data <- c(row_data, get(var))
      }
      row_data <- data.frame(row_data)
      row_data <- t(row_data)
      row_data <- as.data.frame(row_data)
      colnames(row_data) <- c("id", upload_table_colnames)
      print(row_data)
      
      DBI::dbWriteTable(con, config$shiny_db_table$upload_data_table_name, row_data, append = TRUE, row.names = FALSE)
      DBI::dbDisconnect(con)
      shinyjs::alert("Upload and update database successful!")
      shinyjs::reset("upload.file")
      shinyjs::toggleState(id="upload_save")
      #shinyjs::toggleClass(id="shiny-tab-file_viewer")
      #shinyjs::runjs("$(.sidebar-menu a:last).tab('show')")
      #shinyjs::addClass(class="active", id="shiny-tab-file_viewer")
    }
  })

  
  ## Download section
  observeEvent(input$download_run, {
    if (is.null(input$show.all.names) || input$show.all.names == FALSE) {
      if (input$database.dir == "") {
        database.dir <- tempdir()
      } else {
        database.dir <- input$database.dir
      }
      if (input$version == "") {
        version <- NULL
      } else {
        version <- input$version
      }
      if (any(input$download.name != "")) {
        extra.params <- eval(parse(text = input$extra.paramters))
        params <- list(download.name = input$download.name, show.all.versions = input$show.all.versions,
                       database.dir = database.dir, show.all.buildvers = input$show.all.buildvers,
                       verbose = input$verbose, version = version)
        params <- config.list.merge(params, extra.params)
        messg <- capture.output(string <- do.call("download.database", params))
        if (length(messg) == 0 ){
          messg <- NULL
        }
        tryCatch({output$log_output <- renderPrint({print(messg)})}, error = function(e) {
          output$log_output <- renderPrint({print(NULL)})
        })
        tryCatch({output$download_output <- renderPrint({print(string)})}, error = function(e) {
          output$download_output <- renderPrint({print(NULL)})
        })
      } else {
        return()
      }
    } else {
      string <- download.database(show.all.names = TRUE)
      output$download_output <- renderPrint({print(string)})
    }
  })
}

shinyApp(ui, server, enableBookmarking = "url")
