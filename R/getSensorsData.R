#' Get data series from an specific sensor
#'
#' This functions retrieves the data series from a specific sensor in a specific time interval. The output is a data frame, although if json is asked, extra information is showed
#'
#' @param s_cod int or string, Station code, mandatory field
#' @param start_date string, Start date in YYYY-MM-DD or YYYY-MM-DD HH:MM:SS format, mandatory field
#' @param end_date string, End date in YYYY-MM-DD or YYYY-MM-DD HH:MM:SS format, mandatory field
#' @param user string, user name, add your email or user name (default: NULL)
#' @param interv string, Time interval, it must be a subset of c("hora"/hour, "dia"/day, "mes"/month) (default: NULL)
#' @param header boolean, omit header? (default: NULL)
#' @param nan_value string or int, value to replace NaN (default: NULL)
#' @param output string, output format, it must be a subset of c("", "json") (default: NULL, equvalent to "")
#' @param number_format string, number and decimal length, in formant "n.d" like "10.3" (default: NULL)
#' @param filename string, path to save the data frame as a CSV file (default: NULL)
#'
#' @return a data frame with the sensor data
#'
#' @examples
#' \dontrun{
#' getSensorData(
#'   s_cod = "RPLTA", start_date = "2012-08-20",
#'   end_date = "2012-08-27", user = "anon@host.com"
#' )
#' getSensorData(
#'   s_cod = "RPLTA", start_date = "2019-05-13 12:00:00",
#'   end_date = "2019-05-13 15:00:00", user = "anon@host.com"
#' )
#' getSensorData(
#'   s_cod = "RPLTA", start_date = "2012-08-20",
#'   end_date = "2012-08-27", interv = "dia"
#' )
#' getSensorData(
#'   s_cod = "RPLTA", start_date = "2016-08-20",
#'   end_date = "2016-08-27", user = "anon@host.com", nan_value = 99999
#' )
#' getSensorData(
#'   s_cod = 29, start_date = "2015-08-01",
#'   end_date = "2015-08-02", nan_value = "", number_format = "10.3"
#' )
#' getSensorData(
#'   s_cod = 29, start_date = "2015-08-01",
#'   end_date = "2015-08-02", output = "json"
#' )
#' }
#'
#' @export
getSensorData <- function(s_cod,
                          start_date,
                          end_date,
                          user = NULL,
                          interv = NULL,
                          header = NULL,
                          nan_value = NULL,
                          output = NULL,
                          number_format = NULL,
                          filename = NULL) {
  baseurl <- "http://www.ceazamet.cl/ws/pop_ws.php"
  fn <- "GetSerieSensor"

  default_value <- "&p_cod=ceazamet"
  d_interv <- c("hora", "dia", "mes")
  d_header <- c(TRUE, FALSE)
  d_nan_value <- c("", "NAN")
  d_output <- c("", "json")

  if (!is.null(filename)) {
    if (!grepl(pattern = "\\.csv$", x = filename)) {
      filename <- paste0(filename, ".csv")
    }
    # check if path exists
    if (!file.exists(dirname(filename))) {
      stop("Path does not exist")
    }
  }

  if (missing(s_cod) | missing(start_date) | missing(end_date)) {
    stop("s_cod, fecha_inicio and fecha_fin must be provided")
  }
  if ((!grepl(pattern = "^\\d{4}-\\d{2}-\\d{2}$", x = start_date)) &
    (!grepl(pattern = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", x = start_date))) {
    stop("start_date must have YYYY-MM-DD or YYYY-MM-DD HH:MM:SS format")
  }
  if ((!grepl(pattern = "^\\d{4}-\\d{2}-\\d{2}$", x = end_date)) &
    (!grepl(pattern = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", x = end_date))) {
    stop("start_date must have YYYY-MM-DD or YYYY-MM-DD HH:MM:SS format")
  }
  outurl <- paste(baseurl, "?fn=", fn, default_value, "&s_cod=", s_cod,
    "&fecha_inicio=", start_date, "&fecha_fin=",
    end_date,
    sep = ""
  )
  if (!is.null(user)) {
    outurl <- paste(outurl, "&user=", user, sep = "")
  } else {
    cat("Advertencia: usuario no provisto\n")
    cat("Warning: user not provided\n")
    usr <- paste0(Sys.info()[["user"]], "@noemail.com")
    outurl <- paste(outurl, "&user=", usr, sep = "")
  }
  if (!is.null(interv)) {
    if (interv %in% d_interv) {
      outurl <- paste(outurl, "&interv=", interv, sep = "")
    } else {
      stop("interv must be one of ", paste(d_interv, collapse = ", "))
    }
  }
  if (!is.null(header)) {
    if (header %in% d_header) {
      outurl <- paste(outurl, "&encabezado=", as.numeric(header), sep = "")
    } else {
      stop("header must be one of ", paste(d_header, collapse = ", "))
    }
  }
  if (!is.null(nan_value)) {
    if ((nan_value %in% d_nan_value) | (is.numeric(nan_value) & is.finite(nan_value))) {
      outurl <- paste(outurl, "&valor_nan=", nan_value, sep = "")
    } else {
      stop("nan_value must be one of ", paste(d_nan_value, collapse = ", "))
    }
  }
  if (!is.null(output)) {
    if (output %in% d_output) {
      outurl <- paste(outurl, "&tipo_resp=", output, sep = "")
    } else {
      stop("output must be one of ", paste(d_output, collapse = ", "))
    }
  }
  if (!is.null(number_format)) {
    # check if format is number.number
    if (grepl(pattern = "^\\d+\\.\\d+$", x = number_format)) {
      outurl <- paste(outurl, "&formato_nro=", number_format, sep = "")
    } else {
      stop("number_format must have number.number format like 10.3")
    }
  }
  outurl <- utils::URLencode(outurl)
  response <- httr::GET(outurl, httr::progress())
  if (response$status_code != 200) {
    stop("Error: ", response$status_code)
  }
  if (!is.null(output)) {
    if (output == "json") {
      t <- httr::content(response, as = "text", type = "application/json", encoding = "UTF-8")
      if (nchar(t) <= 1) {
        stop("No data found, check arguments (it could be invalid p_cod)")
      }
      # convert json to data.frame
      t <- jsonlite::fromJSON(t)
      cat(paste0("Station name: ", t$e_nombre, "\n"))
      cat(paste0("Station code: ", t$e_cod, "\n"))
      cat(paste0("Sensor code: ", t$s_cod, "\n"))
      cat(paste0("Sensor height: ", t$s_altura, "\n"))
      cat(paste0("Type: ", t$tf_nombre, "\n"))
      cat(paste0("Units: ", t$um_notacion, "\n"))
      t <- t$serie
    }
  } else {
    t <- httr::content(response, as = "text")
    t <- strsplit(t, split = "\n", fixed = T)
    t <- t[[1]]
    if (!is.null(header)) {
      if (header == 1) {
        if (length(t) <= 1) {
          stop("No data found, check arguments (it could be invalid p_cod)")
        }
        t <- strsplit(t, split = ",", fixed = T)
        t <- do.call(rbind, t)
        t <- as.data.frame(t)
      }
    } else {
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
    }
  }
  for (i in seq_along(t)) {
    if (is.null(nan_value)) {
      t[, i] <- utils::type.convert(t[, i], na.strings = "", as.is = TRUE)
    } else {
      t[, i] <- utils::type.convert(t[, i], na.strings = as.character(nan_value), as.is = TRUE)
    }
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
