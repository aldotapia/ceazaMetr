#' Get sensors list
#'
#' This function retrieves the list of sensors for a given station, use `e_cod` from `getStationsList` to get the appropriate code for the station.
#'
#' @param p_cod string, The code of the provider, currently only "ceazamet" is supported
#' @param e_cod int or string, Station code, mandatory field
#' @param user string, user name, add your email or user name (default: NULL)
#' @param tm_cod string, type of measurement, it must be a subset of c("ta_c", "hr", "vv_ms", "dv", "rs_w", "pp_mm") (default: NULL)
#' @param cols string or vector of strings, Columns to retrieve, it must be a subset of c("e_cod", "s_cod", "tf_nombre", "um_notacion", "s_altura", "s_ultima_lectura", "tm_cod", "s_primera_lectura") (default: NULL)
#' @param filename string, path to save the data frame as a CSV file (default: NULL)
#'
#' @return A data frame with the sensors list
#'
#' @examples
#' getSensorsList(e_cod = 9, user = "anon@host.com")
#' getSensorsList(
#'   e_cod = 6, user = "anon@host.com",
#'   cols = c(
#'     "e_cod", "s_cod", "tf_nombre", "um_notacion",
#'     "s_altura", "s_ultima_lectura"
#'   )
#' )
#' getSensorsList(e_cod = 6, user = "anon@host.com", tm_cod = "ta_c")
#' getSensorsList(
#'   e_cod = 6, user = "anon@host.com",
#'   cols = c("e_cod", "s_cod", "tm_cod", "s_altura")
#' )
#'
#' @export
getSensorsList <- function(p_cod = "ceazamet",
                           e_cod,
                           user = NULL,
                           tm_cod = NULL,
                           cols = NULL,
                           filename = NULL) {
  baseurl <- "http://www.ceazamet.cl/ws/pop_ws.php"
  fn <- "GetListaSensores"
  d_p_cod <- c("ceazamet")
  d_tm_cod <- c("ta_c", "hr", "vv_ms", "dv", "rs_w", "pp_mm")
  d_cols <- c(
    "e_cod", "s_cod", "tf_nombre", "um_notacion", "s_altura",
    "s_ultima_lectura", "tm_cod", "s_primera_lectura"
  )

  if (!is.null(filename)) {
    if (!grepl(pattern = "\\.csv$", x = filename)) {
      filename <- paste0(filename, ".csv")
    }
    # check if path exists
    if (!file.exists(dirname(filename))) {
      stop("Path does not exist")
    }
  }

  if (missing(e_cod)) {
    stop("e_cod must be provided")
  }
  if (p_cod %in% d_p_cod) {
    outurl <- paste(baseurl, "?fn=", fn, "&p_cod=", p_cod, "&e_cod=", e_cod, sep = "")
  } else {
    stop("p_cod must be one of ", paste(d_p_cod, collapse = ", "))
  }
  if (!is.null(user)) {
    outurl <- paste(outurl, "&user=", user, sep = "")
  } else {
    cat("Advertencia: usuario no provisto\n")
    cat("Warning: user not provided\n")
    usr <- paste0(Sys.info()[["user"]], "@noemail.com")
    outurl <- paste(outurl, "&user=", usr, sep = "")
  }
  if (!is.null(tm_cod)) {
    if (tm_cod %in% d_tm_cod) {
      outurl <- paste(outurl, "&tm_cod=", tm_cod, sep = "")
    } else {
      stop("tm_cod must be one of ", paste(d_tm_cod, collapse = ", "))
    }
  }
  if (!is.null(cols)) {
    args <- paste0("c", seq(from = 0, to = length(cols) - 1, by = 1))
    for (i in seq_along(cols)) {
      outurl <- paste(outurl, "&", args[i], "=", cols[i], sep = "")
    }
  }
  response <- httr::GET(outurl, httr::progress())
  if (response$status_code != 200) {
    stop("Error: ", response$status_code)
  }
  t <- strsplit(httr::content(response, "parsed"), split = "\n", fixed = T, useBytes = T)
  t <- t[[1]]
  if (length(t) <= 5) {
    stop("No data found, check arguments (it could be invalid p_cod)")
  }
  t <- t[5:length(t)]
  t <- gsub(pattern = "#", replacement = "", x = t)
  t <- strsplit(t, split = ",", fixed = T)
  cnames <- t[[1]]
  t <- do.call(rbind, t[2:length(t)])
  t <- as.data.frame(t)
  names(t) <- cnames
  for (i in seq_along(t)) {
    t[, i] <- utils::type.convert(t[, i], na.strings = "", as.is = TRUE)
  }
  cat(paste0(
    "ES:\n",
    "    Datos provistos por CEAZA a traves del sistema CEAZA-Met.\n",
    "    Revisar condiciones de uso en www.ceazamet.cl\n"
  ))
  cat(paste0(
    "EN:\n",
    "    Data provided by CEAZA through the CEAZA-Met system.\n",
    "    Check terms of use at www.ceazamet.cl\n"
  ))

  if (!is.null(filename)) {
    utils::write.csv(t, file = filename, row.names = FALSE)
  }

  return(t)
}
