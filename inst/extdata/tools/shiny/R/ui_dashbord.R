source("config.R")
body_dashbord_tabItem <- tabItem("dashboard",
    fluidRow(
      box(
        title = "Status of system",
        width = 12,
        status = "primary",
        collapsible = TRUE,
        uiOutput("system_monitor")
        ),
      box(
        title = "Task table query",
        width = 12,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        textInput("task_table_key", "Key"),
        actionButton("task_table_button", "Query"),
        hr(),
        DT::dataTableOutput("task_table_DT"),
        verbatimTextOutput("task_table_log")

      ),
      box(
        title = "Session info of R service",
        width = 12,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        withSpinner(verbatimTextOutput("r_session_info_monitor"), type = 8)
      ),
      box(
        title = "Installed packages of R service",
        width = 12,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        withSpinner(DT::dataTableOutput("r_packages_info_monitor"), type = 8)
      ),
      box(
        title = "Environment variables of R service",
        width = 12,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        withSpinner(DT::dataTableOutput("r_env_info_monitor"), type = 8)
      )
    )
)
