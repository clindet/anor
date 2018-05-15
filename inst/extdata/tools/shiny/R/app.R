pkgs.shiny <- c("shinycssloaders", "Cairo", "maftools", "shinydashboard", "configr",
  "annovarR", "data.table", "shinyjs", "DT", "DBI", "RSQLite")
sapply(pkgs.shiny, function(x) {
  status <- suppressWarnings(require(x, character.only = TRUE))
  if (!status) {
    tryCatch(install.packages(x), error = function(e) {
      source("https://bioconductor.org/biocLite.R")
      biocLite(x)
    })
  }
})
for (i in pkgs.shiny) {
  suppressMessages(require(i, character.only = TRUE))
}

# source UI required code config.R sourced in the body_upload_ui.R
files <- list.files(".", "^ui_")
files <- files[!files %in% c("app.R", "config.R")]
for (i in files) {
  source(i)
}

header <- dashboardHeader(title = "annovarR Shiny APP", messages, notifications,
  tasks)

sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(id = "navbar_tabs",
    menuItem("Introduction", tabName = "introduction", icon = icon("home")),
    menuItem("Visulization", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("File Viewer", tabName = "file_viewer", icon = icon("file")),
    menuItem("Upload", tabName = "upload", icon = icon("cloud-upload")),
    menuItem("Downloader", icon = icon("cloud-download"), tabName = "download"),
    menuItem("Source code for app", icon = icon("file-code-o"),
             href = "https://github.com/JhuangLab/annovarR/blob/master/inst/extdata/tools/shiny/R/app.R")
  )
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(type = "text/javascript", src = "custom.js")),
    tabItems(body_introduction_tabItem,
      body_file_viewer_tabItem,
      body_visulization_tabItem,
      body_upload_tabItem,
      body_download_tabItem
    )
  )

# Defined shiny UI
ui <- function(request) {
  dashboardPage(header, sidebar, body, skin = skin)
}

server <- function(input, output, session) {
  # Visulization section (maftools)
  files <- c("server_maftools.R", "server_download.R", "server_upload_file.R")
  for (i in files) {
    source(i)
  }
  output <- maftools_server(input, output)
  output <- server_upload_file(input, output, session)$output
  session <- server_upload_file(input, output, session)$session
  output <- download_section_server(input, output)
}
shinyApp(ui, server)
