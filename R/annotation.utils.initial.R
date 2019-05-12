# Check db.type
db.type.check <- function(db.type = NULL) {
  if (is.null(db.type)) {
    stop("Please set db.type value.")
  } else if (!db.type %in% c("txt", "sqlite", "mysql")) {
    stop("db.type only support txt/sqlite and mysql.")
  }
}

# Input dat validation check step
input.dat.check <- function(dat = "") {
  if (!is.list(dat)) {
    stop("Input dat format must be a list.")
  }
  if (!is.data.table(dat)) {
    dat <- as.data.table(dat)
  }
  if (nrow(dat) == 0) {
    return(NULL)
  } else {
    return(dat)
  }
}

# Input dat initial to ready analysis
input.dat.initial <- function(dat = NULL, format_dat_fun = NULL, verbose = FALSE) {
  info.msg(sprintf("Total %s lines be inputed, colnames is %s.", nrow(dat), paste0(colnames(dat), 
    collapse = ", ")), verbose = verbose)
  if (is.null(format_dat_fun)) {
    return(dat)
  }
  print.vb(dat, verbose = verbose)
  # format_dat_fun can standardize the input data
  info.msg("Formating the input data.", verbose = verbose)
  dat <- do.call(format_dat_fun, list(dat.input = dat))
  info.msg(sprintf("After formated, total %s lines be hold back, colnames is %s.", 
    nrow(dat), paste0(colnames(dat), collapse = ", ")), verbose = verbose)
  print.vb(dat, verbose = verbose)
  return(dat)
}

# Initial db file prefix
db.file.prefix.initial <- function(db.type = NULL, db.file.prefix = NULL) {
  if (is.null(db.file.prefix)) {
    db.file.prefix <- db.type
  }
  return(db.file.prefix)
}

# Database and database dir check
database.dir.check <- function(dbname_fixed = NULL, database.dir = NULL, db.type = NULL) {
  if (!is.null(dbname_fixed) && !is.null(database.dir) && !is.null(db.type) && 
    file.exists(sprintf("%s/%s.%s", database.dir, dbname_fixed, db.type))) {
    dbname_fixed <- sprintf("%s/%s.%s", database.dir, dbname_fixed, db.type)
    return(dbname_fixed)
  }
  if (is.null(dbname_fixed) && (is.null(database.dir) || database.dir == "")) {
    stop("Parameter database.dir not be setted.")
  } else if (is.null(dbname_fixed) && !dir.exists(database.dir)) {
    stop(sprintf("%s directory not existed.", database.dir))
  }
  return(dbname_fixed)
}

# Set dbname in annotation.R annotation.pos.utils
dbname.initial <- function(anno.name = "", dbname_fixed = NULL, setdb_fun = NULL, 
  buildver = "hg19", database.dir = NULL, db.type = NULL, db.file.prefix = "sqlite", 
  mysql.connect.params = NULL, sqlite.connect.params = NULL) {
  if (is.null(dbname_fixed) && !is.null(setdb_fun)) {
    setdb_fun.args <- methods::formalArgs(setdb_fun)
    setdb_fun.params <- list(anno.name = anno.name, buildver = buildver, database.dir = database.dir, 
      db.type = db.type)
    if ("mysql.connect.params" %in% setdb_fun.args) {
      setdb_fun.params <- config.list.merge(setdb_fun.params, list(mysql.connect.params = mysql.connect.params))
    }
    if ("sqlite.connect.params" %in% setdb_fun.args) {
      setdb_fun.params <- config.list.merge(setdb_fun.params, list(sqlite.connect.params = sqlite.connect.params))
    }
    if ("db.file.prefix" %in% setdb_fun.args) {
      setdb_fun.params <- config.list.merge(setdb_fun.params, list(db.file.prefix = db.file.prefix))
    }
    dbname <- do.call(setdb_fun, setdb_fun.params)
  } else if (!is.null(dbname_fixed)) {
    dbname <- dbname_fixed
  } else {
    dbname <- "default"
  }
  if (db.type %in% c("txt", "sqlite") && !file.exists(dbname)) {
    stop(sprintf("%s %s format database dose not exist.", dbname, db.type))
  }
  return(dbname)
}

# Initial table name
table.name.initial <- function(anno.name = NULL, table_name_fixed = NULL, buildver = "hg19", 
  set_table_fun = NULL) {
  if (is.null(table_name_fixed) && !is.null(set_table_fun)) {
    set_table_fun.params <- list(anno.name = anno.name, buildver = buildver)
    table.name <- do.call(set_table_fun, set_table_fun.params)
  } else if (!is.null(table_name_fixed)) {
    table.name <- table_name_fixed
  } else {
    table.name <- "default"
  }
  return(table.name)
}

# Initial databae connect params
database.params.initial <- function(db.type = NULL, dbname = "", table.name = "", 
  sqlite.connect.params = list(), mysql.connect.params = list()) {
  if (db.type == "sqlite") {
    sqlite.connect.params$dbname <- dbname
    sqlite.connect.params$table.name <- table.name
  } else if (db.type == "mysql") {
    mysql.connect.params$dbname <- dbname
    mysql.connect.params$table.name <- table.name
  }
  return(list(sqlite = sqlite.connect.params, mysql = mysql.connect.params))
}
