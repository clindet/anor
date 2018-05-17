source("config.R")
server_upload_file <- function(input, output, session) {
  # File viewr section
  render_files_info_DT <- function() {
    output$files_info_DT <- DT::renderDataTable({
      info <- featch_files()
      colnames(info)[1] <- "ID"
      Action <- sprintf("<button id = 'files_view_%s' name = 'files_view_%s' type='button' class = 'btn btn-primary action-button shiny-bound-input'>View</button>",
                        info[, 1], info[, 1])
      Action <- sprintf("%s <button id = 'files_del_%s' name = 'files_del_%s' type='button' class = 'btn btn-primary action-button shiny-bound-input'>Del</button>",
                        Action, info[, 1], info[, 1])
      info <- cbind(info[1], Action, info[2:ncol(info)])
      return(info)
    }, rownames = FALSE, editable = FALSE, caption = "All files stored in annovarR shinyapp Web service",
    escape = FALSE, extensions = c("Buttons", "FixedColumns", "Scroller"),
    options = list(autoWidth = TRUE, dom = "Bfrtlip", deferRender = TRUE,
                   searchHighlight = TRUE, scrollX = TRUE, lengthMenu = list(list(5,
                   10, 25, 50, -1), list(5, 10, 25, 50, "All")), buttons = c("copy",
                   "csv", "excel", "pdf", "print"), columnDefs = list(list(width = "100px",
                   targets = "_all")), initComplete = DT::JS("function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': '#487ea5', 'color': '#fff'});",
                   "}")), selection = "none")
  }

  render_files_info_DT()

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  if (upload_table %in% DBI::dbListTables(con)) {
    sql <- sprintf("SELECT id FROM %s", upload_table)
    upload_table_data <- DBI::dbGetQuery(con, sql)
    files_ids <- upload_table_data[, 1]
    set_preview <- function(id) {
      shinyjs::onclick(sprintf("files_view_%s", id), function(event) {
        shinyjs::reset("file_preview_DT")
        output$file_preview_DT <- DT::renderDataTable({
          sql <- sprintf("SELECT file_path,file_type FROM %s where id=%s",
          upload_table, id)
          upload_table_data <- DBI::dbGetQuery(con, sql)
          if (upload_table_data$file_type %in% c("txt", "csv")) {
          file_content <- fread(upload_table_data$file_path)
          return(file_content)
          }
        }, rownames = FALSE, editable = FALSE, caption = "All files stored in annovarR shinyapp Web service",
          escape = FALSE, extensions = c("Buttons", "Scroller"), options = list(dom = "Bfrtlip",
          searchHighlight = TRUE, scrollX = TRUE, buttons = c("copy", "csv",
            "excel", "pdf", "print"), initComplete = DT::JS("function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#487ea5', 'color': '#fff'});",
            "}")), selection = "single")
      })
    }
    delete_file_item <- function(id) {
      shinyjs::onclick(sprintf("files_del_%s", id), function(event) {
        shinyjs::runjs(sprintf("
     if(!confirm('Confirm to delete？')){
        window.event.returnValue = false;
     } else {
        var tb_id = $('#files_info_DT :first').attr('id').replace('_wrapper', '')
        tb_id = eval(tb_id)
        tb_id.deleteRow($('#files_del_%s').closest('tr').index() + 1);
        $('#delete_confirmed').click();
     };",
          id))
        updateQueryString(sprintf("?delete_id=%s", id), mode = "push")
      })

    }
    for (i in files_ids) {
      set_preview(i)
      delete_file_item(i)
    }
  }

  observeEvent(input$delete_confirmed, {
    id <- getQueryString()
    status <- DBI::dbSendQuery(con, sprintf("DELETE FROM %s WHERE id=%s", upload_table,
      id))
    print(status)
    shinyjs::alert("Delete successful!")
    render_files_info_DT()
  })

  # Upload section
  output$upload_file_preview_DT <- DT::renderDataTable({

    # input$file1 will be NULL initially. After the user selects and uploads a file,
    # head of that data file by default, or all rows if selected, will be shown.
    req(input$upload.file)
    shinyjs::enable(id = "upload_save")
    if (input$upload.file.type %in% c("txt", "csv")) {
      df <- fread(input$upload.file$datapath)
      return(df)
    }
  }, extensions = c("Buttons", "FixedColumns", "Responsive"), options = list(dom = "Bfrtlip",
    searchHighlight = TRUE, scrollX = TRUE, buttons = c("copy", "csv", "excel",
      "pdf", "print"), initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#487ea5', 'color': '#fff'});",
      "}")), selection = "none")

  output$upload_file_preview <- renderText({
    req(input$upload.file)
    if (!input$upload.file.type %in% c("txt", "csv")) {
      return(input$upload.file$datapath)
    }
  })
  # shinyjs::addClass(class='btn btn-primary', id='upload_save')
  observeEvent(input$upload_save, {
    req(input$upload.file)

    if (db_type == "sqlite") {
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      sql <- sprintf("select id from %s", upload_table)
      ids <- DBI::dbGetQuery(con, sql)
      if (nrow(ids) == 0) {
        id <- 1
      } else {
        id <- RSQLite::dbGetQuery(con, 
                  sprintf("SELECT seq from sqlite_sequence where name = '%s'",
                          upload_table))
        id <- as.numeric(id) + 1
      }
      assign(upload_table_colnames[1], input$upload.file$name)
      destfile <- sprintf("%s/%s", upload_dir, id)
      assign(upload_table_colnames[7], tools::md5sum(input$upload.file$datapath))
      tryCatch(file.rename(input$upload.file$datapath, destfile),
        warning = function(w) {
        file.copy(input$upload.file$datapath, destfile, overwrite = TRUE)
      })
      assign(upload_table_colnames[2], destfile)
      assign(upload_table_colnames[3], file.size(destfile))
      assign(upload_table_colnames[4], input$upload.file.type)
      assign(upload_table_colnames[5], input$upload.genome.version)
      assign(upload_table_colnames[6], format(Sys.time(), "%Y %H:%M:%S"))
      assign(upload_table_colnames[8], input$upload.file.description)
      row_data <- NULL
      for (var in c("id", upload_table_colnames)) {
        row_data <- c(row_data, get(var))
      }
      row_data <- data.frame(row_data)
      row_data <- t(row_data)
      row_data <- as.data.frame(row_data)
      colnames(row_data) <- c("id", upload_table_colnames)


      DBI::dbWriteTable(con, config$shiny_db_table$upload_data_table_name,
        row_data, append = TRUE, row.names = FALSE)
      DBI::dbDisconnect(con)
      shinyjs::alert("Upload and update database successful!")
      shinyjs::reset("upload.file")
      shinyjs::toggleState(id = "upload_save")
      # update file view UI and ovserve the preview and delete event
      render_files_info_DT()
      set_preview(id)
      delete_file_item(id)
      # Chose the navbar
      updateNavbarPage(session, "navbar_tabs", selected = "file_viewer")
    }
  })
  return(list(output = output, session = session))
}
