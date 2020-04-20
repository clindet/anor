# preprocess before to query with database
before.query.steps <- function(dat = data.table(), anno.name = "", buildver = "hg19", 
  database.dir = Sys.getenv("anor_DB_DIR", ""), db_col_order = 1:5, index_cols = c("chr", 
    "start"), matched_cols = c("chr", "start", "end", "ref", "alt"), format_dat_fun = format.cols, 
  dbname_fixed = NULL, table_name_fixed = NULL, setdb_fun = set.db, set_table_fun = set.table, 
  db.type = NULL, db.file.prefix = NULL, mysql.connect.params = list(), sqlite.connect.params = list(), 
  verbose = FALSE) {
  db.type.check(db.type)
  dat <- input.dat.check(dat)
  if (is.null(dat)) {
    return(NULL)
  }
  dbname_fixed <- database.dir.check(dbname_fixed = dbname_fixed, database.dir = database.dir, 
    db.type = db.type)
  dat <- input.dat.initial(dat, format_dat_fun, verbose)
  dat <- dat[, 1:length(db_col_order)]
  dat.names <- names(dat)
  db.file.prefix <- db.file.prefix.initial(db.type, db.file.prefix)
  # dbname is path of sqlite or text database or is dbname of MySQL database
  dbname <- dbname.initial(anno.name, dbname_fixed, setdb_fun, buildver, database.dir, 
    db.type, db.file.prefix, mysql.connect.params, sqlite.connect.params)
  # table.name initial
  table.name <- table.name.initial(anno.name, table_name_fixed, buildver, set_table_fun)
  
  # database.params initial
  database.params <- database.params.initial(db.type, dbname, table.name, sqlite.connect.params, 
    mysql.connect.params)
  sqlite.connect.params <- database.params[["sqlite"]]
  mysql.connect.params <- database.params[["mysql"]]
  print.db.info(dbname, db.type, mysql.connect.params, verbose)
  
  database.con <- connect.db(dbname, db.type, sqlite.connect.params, mysql.connect.params, 
    verbose)
  tb.colnames <- db.tb.colnames(dbname = dbname, db.type = db.type, sqlite.connect.params, 
    mysql.connect.params)
  info.msg("Database colnames:%s", paste0(tb.colnames, collapse = ", "), verbose = verbose)
  
  print.vb(index_cols, verbose = verbose)
  print.vb(dat, verbose = verbose)
  # Get unique records, params is pass to select.dat.full.match and get matched
  # data table from database
  dup <- !duplicated(dat)
  params <- dat[dup, index_cols, with = FALSE]
  
  # Sync the colnames between input cols and database table cols which be used to
  # select data
  tb.colnames.raw <- tb.colnames
  tb.colnames <- tb.colnames[db_col_order]
  index_cols.order <- match(colnames(dat), index_cols)
  index_cols.order <- index_cols.order[!is.na(index_cols.order)]
  matched_cols.order <- match(colnames(dat), matched_cols)
  matched_cols.order <- matched_cols.order[!is.na(matched_cols.order)]
  colnames(params) <- tb.colnames[index_cols.order]
  info.msg(sprintf("After drop duplicated, %s colnum total %s line be used to select dat from database (%s).", 
    paste0(index_cols, collapse = ","), nrow(params), paste0(names(params), collapse = ",")), 
    verbose = verbose)
  print.vb(params, verbose = verbose)
  return(list(dat = dat, dat.names = dat.names, params = params, database.con = database.con, 
    tb.colnames = tb.colnames, tb.colnames.raw = tb.colnames.raw, table.name = table.name, 
    index_cols.order = index_cols.order, matched_cols.order = matched_cols.order, 
    dbname = dbname))
}

# after query process
after.query.steps <- function(dat = NULL, selected.db.tb = NULL, format_db_tb_fun = NULL, 
  db_col_order = NULL, tb.colnames = NULL, tb.colnames.raw = NULL, tb.matched_cols = NULL, 
  matched_cols = NULL, full.matched_cols = NULL, full.matched_cols.raw = NULL, 
  inferior_col = NULL, inferior_col.raw = NULL, superior_col = NULL, superior_col.raw = NULL, 
  dbname = NULL, return_col_index = NULL, return_col_names = NULL, return_col_names_profix = NULL, 
  database.con = NULL, db.type = NULL, dat.names = NULL, params = NULL, get.final.table.fun = get.full.match.final.table, 
  query.type = "full", verbose = FALSE) {
  if (query.type == "full") {
    selected.db.tb <- do.call(format_db_tb_fun, list(db.tb = selected.db.tb, 
      input.dat = dat, tb.matched_cols = tb.matched_cols))
  } else {
    selected.db.tb <- do.call(format_db_tb_fun, list(db.tb = selected.db.tb, 
      input.dat = params, full.matched_cols = full.matched_cols, inferior_col = inferior_col, 
      superior_col = superior_col, tb.matched_cols = c(full.matched_cols, inferior_col, 
        superior_col)))
  }
  info.msg(sprintf("Total %s line be selected from database:", nrow(selected.db.tb)), 
    verbose = verbose)
  print.vb(selected.db.tb, verbose = verbose)
  # Check return_col_index, if empty return the all of cols in database without
  # matched cols
  if (all(return_col_index == "")) {
    all.cols <- 1:ncol(selected.db.tb)
    return_col_index <- all.cols[!all.cols %in% db_col_order]
  }
  
  # If selected data is empty, return NA matrix according the return_col_index and
  # return_col_names
  if (nrow(selected.db.tb) == 0) {
    empty.col <- return.empty.col(dat, tb.colnames.raw, return_col_index, return_col_names, 
      return_col_names_profix)
    disconnect.db(database.con, db.type)
    return(empty.col)
  }
  
  # Sync colnames between selected data and input data
  selected.db.tb <- sync.colnames(selected.db.tb, db_col_order, dat.names)
  tb.colnames <- colnames(selected.db.tb)
  info.msg(sprintf("After sync colnames, the selected data colnames:%s", paste0(tb.colnames, 
    collapse = ",")), verbose = verbose)
  selected.colnames <- tb.colnames[return_col_index]
  
  if (!is.null(matched_cols)) {
    selected.db.tb <- get.final.table.fun(dat, selected.db.tb, matched_cols, 
      selected.colnames, verbose)
  } else {
    selected.db.tb <- get.final.table.fun(dat, selected.db.tb, inferior_col.raw, 
      superior_col.raw, selected.colnames, verbose)
  }
  
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", dbname), 
    verbose = verbose)
  disconnect.db(database.con, db.type)
  
  # Final process on result
  result <- selected.db.tb
  if (any(return_col_names != "")) {
    colnames(result) <- return_col_names
  } else {
    colnames(result) <- tb.colnames[return_col_index]
  }
  if (!is.null(return_col_names_profix) && return_col_names_profix != "") {
    colnames(result) <- paste0(return_col_names_profix, colnames(result))
  }
  info.msg("Returned data:", verbose = verbose)
  print.vb(result, verbose = verbose)
  return(result)
}

# Sync database and input table colnames
sync.colnames <- function(result = NULL, col.order = "", col.names = "") {
  colnames(result)[col.order] <- col.names
  return(result)
}

# Return NA cols using ordered colnames
return.empty.col <- function(dat.list = NULL, tb.colnames = "", return_col_index = "", 
  return_col_names = "", return_col_names_profix = "") {
  result <- NULL
  for (i in 1:length(return_col_index)) {
    result <- cbind(result, rep(NA, length(dat.list[[1]])))
  }
  result <- as.data.table(result)
  if (any(return_col_names != "")) {
    colnames(result) <- return_col_names
  } else {
    colnames(result) <- tb.colnames[return_col_index]
  }
  if (!is.null(return_col_names_profix) && return_col_names_profix != "") {
    colnames(result) <- paste0(return_col_names_profix, colnames(result))
  }
  return(result)
}

# print.db.info Print the database info
print.db.info <- function(dbname = NULL, db.type = NULL, mysql.connect.params = list(), 
  verbose = TRUE) {
  if (db.type != "mysql" && !file.exists(dbname)) {
    stop(sprintf("%s database not existed, please check the database dir or setdb_fun function again.", 
      dbname))
  }
  if (db.type != "mysql") {
    info.msg(sprintf("Database path:%s", dbname), verbose = verbose)
  } else {
    info.msg(sprintf("Host: %s, Database:%s", mysql.connect.params$host, dbname), 
      verbose = verbose)
  }
}

# Merge selected data and input data and get final output
get.full.match.final.table <- function(dat = NULL, selected.db.tb = NULL, matched_cols = "", 
  selected.colnames = "", verbose = FALSE) {
  # Generate a unique id to get final result according the input data table
  id <- 1:nrow(dat)
  dat <- cbind(dat, id)
  dat <- as.data.table(lapply(dat, function(x) as.character(x)))
  selected.db.tb <- as.data.table(lapply(selected.db.tb, function(x) as.character(x)))
  
  # Set data.table key to accelerate merge step
  keys <- paste0(matched_cols, collapse = "\",\"")
  text <- sprintf("setkey(dat, \"%s\")", keys)
  eval(paste0(text = text))
  text <- sprintf("setkey(selected.db.tb, \"%s\")", keys)
  eval(paste0(text = text))
  
  selected.db.tb <- merge(selected.db.tb, dat, all = TRUE, allow.cartesian = TRUE)
  rm(dat)
  gc()
  selected.db.tb$id <- as.numeric(selected.db.tb$id)
  setkey(selected.db.tb, id)
  selected.db.tb <- selected.db.tb[!is.na(selected.db.tb$id), ]
  selected.db.tb <- selected.db.tb[!duplicated(selected.db.tb$id), ]
  info.msg(sprintf("Total %s line be processed.", nrow(selected.db.tb)), verbose = verbose)
  info.msg(sprintf("Matched data using %s colnums %s:", paste0(matched_cols, collapse = ",")))
  print.vb(selected.db.tb, verbose = verbose)
  selected.db.tb <- selected.db.tb[, selected.colnames, with = FALSE]
}

# Merge selected data and input data and get final output
get.region.match.final.table <- function(dat = NULL, selected.db.tb = NULL, inferior_col = "", 
  superior_col = "", selected.colnames = "", verbose = FALSE) {
  rm(dat)
  gc()
  setkey(selected.db.tb, "xid")
  selected.db.tb <- selected.db.tb[!duplicated(selected.db.tb$xid), ]
  selected.db.tb <- selected.db.tb[, selected.colnames, with = FALSE]
  info.msg(sprintf("Total %s line be processed.", nrow(selected.db.tb)), verbose = verbose)
  print.vb(selected.db.tb, verbose = verbose)
  return(selected.db.tb)
}
