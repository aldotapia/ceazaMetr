#' Compute ET0
#'
#' Compute evapotranspiration using the FAO-56 Penman-Monteith method
#'
#' @param lat numeric, latitude in decimal degrees. Must be between -90 and 90. Mandatory
#' @param lon numeric, longitude in decimal degrees. Must be between -180 and 180. Mandatory
#' @param elev numeric, elevation in meters above sea level. Mandatory
#' @param month integer or vector, month of the year. Mandatory
#' @param t_max numeric or vector, maximum temperature in degrees Celsius. Mandatory
#' @param t_min numeric or vector, minimum temperature in degrees Celsius. Mandatory
#' @param rh_max numeric or vector, maximum relative humidity in percentage. Mandatory
#' @param rh_min numeric or vector, minimum relative humidity in percentage. Mandatory
#' @param ws_mean numeric or vector, mean wind speed in meters per second. Mandatory
#' @param sr_mean numeric or vector, mean solar radiation in Watts per square meter. Mandatory
#' @param debug logical, print debug information
#' @param user character, user email
#'
#' @return numeric value or vector of ET0 in mm/day

#'
#' @examples
#' \dontrun{
#' et0(lat = -30, lon = -70, elev = 20,
#'     month = 1, t_max = 29.6, t_min = 22.8,
#'     rh_max = 90, rh_min = 70, ws_mean = 0.2,
#'     sr_mean = 200)
#'
#' et0(lat = -30, lon = -70, elev = 20,
#'     month = c(1,2), t_max = c(29.6, 30), t_min = c(22.8, 21),
#'     rh_max = c(90, 80), rh_min = c(70, 60), ws_mean = c(0.2, 0.9),
#'     sr_mean = c(200, 180))
#' }
#'
#' @export
et0 = function(lat, lon, elev, month, t_max, t_min,
               rh_max, rh_min, ws_mean, sr_mean, debug=FALSE,
               user=NULL){

  baseurl = "http://www.ceazamet.cl/ws/ws_et0.php"
  fn = "et0"

  if(is.null(lat) | is.null(lon) | is.null(elev) | is.null(month) |
     is.null(t_max) | is.null(t_min) | is.null(rh_max) | is.null(rh_min) |
     is.null(ws_mean) | is.null(sr_mean)){
    stop("lat, lon, elev, month, t_max, t_min, rh_max, rh_min, ws_mean, sr_mean are required")
  }
  if(length(month) != length(t_max) | length(month) != length(t_min) |
     length(month) != length(rh_max) | length(month) != length(rh_min) |
     length(month) != length(ws_mean) | length(month) != length(sr_mean)){
    stop("month, t_max, t_min, rh_max, rh_min, ws_mean, sr_mean should have the same length")
  }
  if(!all(sapply(list(lat, lon, elev, month, t_max, t_min, rh_max,
                      rh_min, ws_mean, sr_mean), is.numeric))){
    stop("lat, lon, elev, month, t_max, t_min, rh_max, rh_min, ws_mean, sr_mean should be numeric")
  }
  if(length(month)>1){
    month = paste0(month, collapse = ",")
    t_max = paste0(t_max, collapse = ",")
    t_min = paste0(t_min, collapse = ",")
    rh_max = paste0(rh_max, collapse = ",")
    rh_min = paste0(rh_min, collapse = ",")
    ws_mean = paste0(ws_mean, collapse = ",")
    sr_mean = paste0(sr_mean, collapse = ",")
  }
  if(!is.logical(debug)){
    stop("debug should be logical")
  }

  if(is.null(user)){

    cat('Advertencia: usuario no provisto\n')
    cat('Warning: user not provided\n')
    user = paste0(Sys.info()[["user"]], "@noemail.com")
  }else{
    if(!is.character(user)){
      stop("user should be character")
    }
  }

  outurl = glue::glue('{baseurl}?fn={fn}&debug={as.numeric(debug)}&',
                      'usuario={user}&lat={lat}&lon={lon}&alt={elev}&',
                      'mes={month}&t_max={t_max}&t_min={t_min}&',
                      'hr_max={rh_max}&hr_min={rh_min}&vv_prom={ws_mean}&',
                      'rs_prom={sr_mean}')

  outurl = utils::URLencode(outurl)
  response = httr::GET(outurl, httr::progress("down"))
  if(response$status_code != 200){
    stop("Error: ", response$status_code)
  }

  t = httr::content(response, as = "text", encoding = "UTF-8")

  if(nchar(t)<=1){
    return(NA)
  }else{
    if(debug){
      t = gsub(pattern = "<br />", replacement = "\n", x = t)
      t = gsub(pattern = "<br>", replacement = "\n", x = t)
      cat(paste0(t,'\n'))
      t = gsub(pattern = '.*ET0=', replacement = '', x = t)
    }
    t = as.numeric(strsplit(t, split = ",", fixed = T)[[1]])
  }
  cat(paste0("ES:\n",
             "    Datos provistos por CEAZA a traves del sistema CEAZA-Met.\n",
             "    Revisar condiciones de uso en www.ceazamet.cl\n"))
  cat(paste0("EN:\n",
             "    Data provided by CEAZA through the CEAZA-Met system.\n",
             "    Check terms of use at www.ceazamet.cl\n"))

  return(t)
}
