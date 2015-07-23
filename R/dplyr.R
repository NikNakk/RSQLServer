#' Connect to SQLServer or Sybase
#'
#' Use \code{src_sqlserver} to connect to an existing SQL Server or Sybase
#' database, and \code{tbl} to connect to tables within that database.
#'
#' @template sqlserver-parameters
#' @return a dplyr SQL based src with subclass \code{sqlserver}
#' @examples
#' \dontrun{
#' library(dplyr)
#' # Connection basics ---------------------------------------------------------
#' # To connect to TEST database, assumed to be specified in your ~/sql.yaml
#' # file (see \code{\link{have_test_server}}), first create a src:
#' my_src <- src_sqlserver("TEST")
#' # Then reference a tbl within that src
#' my_tbl <- tbl(my_src, "my_table")
#' # Methods -------------------------------------------------------------------
#' # You can then inspect table and perform actions on it
#' dim(my_tbl)
#' colnames(my_tbl)
#' head(my_tbl)
#' # Data manipulation verbs ---------------------------------------------------
#' filter(my_tbl, this.field == "that.value")
#' select(my_tbl, from.this.field:to.that.field)
#' arrange(my_tbl, this.field)
#' mutate(my_tbl, squared.field = field ^ 2)
#' # Group by operations -------------------------------------------------------
#' by_field <- group_by(my_tbl, field)
#' group_size(by_field)
#' by_field %>% summarise(ave = mean(numeric.field))
#' # See dplyr documentation for further information on data operations
#' }
#' @export
src_sqlserver <- function (server, file = NULL, database = "",
  type = "sqlserver", port = "", properties = list()) {
  con <- dbConnect(SQLServer(), server, file, database , type, port, properties)
  info <- dbGetInfo(con)
  dplyr::src_sql("sqlserver", con, info = info)
}

#' @importFrom dplyr src_desc
#' @export
src_desc.src_sqlserver <- function (x) {
  info <- x$info
  paste0(info$db.product.name, ' version ', info$db.version, " [", info$user, "]")
}

#' @importFrom dplyr tbl
#' @export
tbl.src_sqlserver <- function (src, from, ...) {
  dplyr::tbl_sql("sqlserver", src = src, from = from, ...)
}

## Math (scalar) functions - no change across versions based on eyeballing:
# MSSQL 2000: https://technet.microsoft.com/en-us/library/aa258862(v=sql.80).aspx
# MSSQL 2005: https://technet.microsoft.com/en-us/library/ms177516(v=sql.90).aspx
# MSSQL 2008: https://technet.microsoft.com/en-us/library/ms177516(v=sql.100).aspx
# MSSQL 2008(r2): https://technet.microsoft.com/en-us/library/ms177516(v=sql.105).aspx
# MSSQL 2012: https://technet.microsoft.com/en-us/library/ms177516(v=sql.110).aspx

## Aggregate functions
# MSSQL 2005: https://technet.microsoft.com/en-US/library/ms173454(v=sql.90).aspx
# MSSQL 2008: https://technet.microsoft.com/en-US/library/ms173454(v=sql.100).aspx
# MSSQL 2008r2*: https://technet.microsoft.com/en-US/library/ms173454(v=sql.100).aspx
# MSSQL 2012*: https://technet.microsoft.com/en-US/library/ms173454(v=sql.110).aspx
# MSSQL 2014: https://technet.microsoft.com/en-US/library/ms173454(v=sql.120).aspx
#' @importFrom dplyr src_translate_env
#' @export
src_translate_env.src_sqlserver <- function (x) {
  dplyr::sql_variant(
    scalar = dplyr::base_scalar,
    aggregate = dplyr::sql_translator(.parent = dplyr::base_agg,
      n = function() dplyr::sql("COUNT(*)"),
      mean = dplyr::sql_prefix('AVG'),
      sd = dplyr::sql_prefix("STDEV"),
      sdp = dplyr::sql_prefix("STDEVP"),
      varp = dplyr::sql_prefix("VARP")
    ),
    window = dplyr::base_win
  )
}

#' @importFrom utils head
#' @export
head.tbl_sqlserver <- function (x, n = 6L, ...) {
  assertthat::assert_that(length(n) == 1, n > 0L)
  build_query(x, n)$fetch()
}

#' @export
sql_join.SQLServerConnection <- function (con, x, y, type = "inner", by = NULL, ...) {
  join <- switch(type, left = sql("LEFT"), inner = sql("INNER"), 
                 right = sql("RIGHT"), full = sql("FULL"), stop("Unknown join type:", 
                                                                type, call. = FALSE))
  by <- dplyr:::common_by(by, x, y)
  using <- FALSE # all(by$x == by$y)
  x_names <- dplyr:::auto_names(x$select)
  y_names <- dplyr:::auto_names(y$select)
  
  uniques <- dplyr:::unique_names(x_names, y_names, by$x[by$x == by$y])
  if (is.null(uniques)) {
    sel_vars <- unique(c(x_names, y_names))
  } else {
    x <- update(x, select = setNames(x$select, uniques$x))
    y <- update(y, select = setNames(y$select, uniques$y))
    by$x <- unname(uniques$x[by$x])
    by$y <- unname(uniques$y[by$y])
    sel_vars <- unique(c(uniques$x, uniques$y))
  }
  t_name_x <- dplyr:::unique_name()
  t_name_y <- dplyr:::unique_name()
  sel_tbl <- rep(t_name_y, length(sel_vars))
  sel_tbl[sel_vars %in% dplyr:::auto_names(x$select)] <- t_name_x
  if (type == "right") {
    sel_tbl[sel_vars %in% by$y] <- t_name_y
  }
  sel_qual <- paste0(sql_escape_ident(con, sel_tbl), ".",
                     sql_escape_ident(con, sel_vars))
  by_shared <- by$x[by$x %in% by$y]
  # This ensures that columns used in by with a name common to both tables
  # will always have a value
  if (length(by_shared) > 0L) {
    sel_qual[sel_vars %in% by_shared] <-
      vapply(sel_vars[sel_vars %in% by_shared], function(var) {
        paste0("COALESCE(", paste(sql_escape_ident(con, c(t_name_x, t_name_y)),
                                  sql_escape_ident(con, var),
                                  sep = ".",
                                  collapse = ", "),
                ") AS ", sql_escape_ident(con, var))
        },
        "")
  }
  sel_sql <- sql_vector(sel_qual, collapse = ", ", parens = FALSE)
  if (using) {
    cond <- build_sql("USING ", lapply(by$x, ident), con = con)
  } else {
    on <- sql_vector(paste0(sql_escape_ident(con, t_name_x), ".",
                            sql_escape_ident(con, by$x), 
                            " = ",
                            sql_escape_ident(con, t_name_y), ".",
                            sql_escape_ident(con, by$y)),
                     collapse = " AND ", 
                     parens = TRUE)
    cond <- build_sql("ON ", on, con = con)
  }
                    
  from <- build_sql("SELECT ", sel_sql, " FROM ", sql_subquery(con, x$query$sql, t_name_x), 
                    "\n\n", join, " JOIN \n\n", sql_subquery(con, y$query$sql, t_name_y), 
                    "\n\n", cond, con = con)
  attr(from, "vars") <- lapply(sel_vars, as.name)
  from
}

#
# #' @importFrom dplyr compute
# #' @export
# compute.tbl_sqlserver <- function (x, name = random_table_name(),
#   temporary = TRUE, ...) {
#   name <- paste0(if (temporary) dplyr::sql("#"), name)
#   db_save_query(x$src$con, x$query$sql, name = name, temporary = temporary)
#   update(dplyr::tbl(x$src, name), group_by = dplyr::groups(x))
# }
#
# #' @importFrom dplyr intersect
# #' @export
# intersect.tbl_sqlserver <- function(x, y, copy = FALSE, ...) {
#   # SQL Server 2000 does not support INTERSECT or EXCEPT
#   assertthat::assert_that(x$src$info$db.version > 8, y$src$info$db.version > 8)
#   y <- auto_copy(x, y, copy)
#   sql <- dplyr::sql_set_op(x$src$con, x, y, "INTERSECT")
#   update(tbl(x$src, sql), group_by = dplyr::groups(x))
# }
#
# #' @importFrom dplyr setdiff
# #' @export
# setdiff.tbl_sqlserver <- function(x, y, copy = FALSE, ...) {
#   # SQL Server 2000 does not support INTERSECT or EXCEPT
#   assertthat::assert_that(x$src$info$db.version > 8, y$src$info$db.version > 8)
#   y <- auto_copy(x, y, copy)
#   sql <- dplyr::sql_set_op(x$src$con, x, y, "EXCEPT")
#   update(tbl(x$src, sql), group_by = dplyr::groups(x))
# }
#
#
