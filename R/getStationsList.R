#' Get stations list
#'
#' Get the list of stations available in the CEAZA-Met through the web service provide. Use this output (e_cod) to get sensors and data from the stations by using the `getSensorsList` and `getSensorData` functions.
#'
#' @param p_cod string, The code of the provider, mandatory field (default: ceazamet)
#' @param e_owner string, Specify "ceaza" to retrieve only CEAZA stations (default: NULL)
#' @param e_cod int or string, Station code (default: NULL)
#' @param user string, User name, add your email or user name (default: NULL)
#' @param cols string or vector of strings, Columns to retrieve, it must be a subset of c("e_lat", "e_lon", "e_altitud", "e_ultima_lectura", "e_cod", "e_nombre", "e_primera_lectura", "e_cod_provincia") (default: NULL)
#' @param geo boolean, Sort stations list by latitude? (TRUE for north to south) (default: NULL)
#' @param filename string, path to save the data frame as a CSV file (default: NULL)
#'
#' @return A data frame with the stations list
#'
#' @examples
#' getStationsList()
#' getStationsList(geo = 1)
#' getStationsList(user = "anon@host.com", cols = c(
#'   "e_lat", "e_lon",
#'   "e_altitud", "e_ultima_lectura", "e_cod", "e_nombre",
#'   "e_primera_lectura"
#' ))
#' getStationsList(
#'   p_cod = "ceazamet", e_owner = "ceaza",
#'   user = "anon@host.com"
#' )
#' @export
getStationsList <- function(p_cod = "ceazamet",
                            e_owner = NULL,
                            e_cod = NULL,
                            user = NULL,
                            cols = NULL,
                            geo = NULL,
                            filename = NULL) {
  baseurl <- "http://www.ceazamet.cl/ws/pop_ws.php"
  fn <- "GetListaEstaciones"
  d_p_cod <- c("ceazamet", "changolab")
  d_e_owner <- "ceaza"
  d_geo <- c(TRUE, FALSE)
  d_cols <- c(
    "e_lat", "e_lon", "e_altitud", "e_ultima_lectura", "e_cod",
    "e_nombre", "e_primera_lectura", "e_cod_provincia"
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

  if (p_cod %in% d_p_cod) {
    outurl <- paste(baseurl, "?fn=", fn, "&p_cod=", p_cod, sep = "")
  } else {
    stop("p_cod must be one of ", paste(d_p_cod, collapse = ", "))
  }
  if (!is.null(e_owner)) {
    if (e_owner %in% d_e_owner) {
      outurl <- paste(outurl, "&e_owner=", e_owner, sep = "")
    } else {
      stop(paste("e_owner must be", d_e_owner))
    }
  }
  if (!is.null(e_cod)) {
    outurl <- paste(outurl, "&e_cod=", e_cod, sep = "")
  }
  if (!is.null(user)) {
    outurl <- paste(outurl, "&user=", user, sep = "")
  } else {
    cat("Advertencia: usuario no provisto\n")
    cat("Warning: user not provided\n")
    usr <- paste0(Sys.info()[["user"]], "@noemail.com")
    outurl <- paste(outurl, "&user=", usr, sep = "")
  }
  if (!is.null(cols)) {
    if (all(cols %in% d_cols)) {
      args <- paste0("c", seq(from = 0, to = length(cols) - 1, by = 1))
      for (i in seq_along(cols)) {
        outurl <- paste(outurl, "&", args[i], "=", cols[i], sep = "")
      }
    } else {
      stop("cols must be a subset of ", paste(d_cols, collapse = ", "))
    }
  }
  if (!is.null(geo)) {
    if (all(geo %in% d_geo)) {
      outurl <- paste(outurl, "&geo=", paste(as.numeric(geo), collapse = ""), sep = "")
    } else {
      stop("geo must be a subset of ", paste(d_geo, collapse = ", "))
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
