pkgs.shiny <- c("shinydashboard", "configr", "annovarR")
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
if (skin == "")
  skin <- "blue"


sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Download", icon = icon("cloud-download"), tabName = "download"
    ),
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
    tabItem("download",
            fluidRow(
              box(
                title = "Download",
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

shinyApp(ui, server)
