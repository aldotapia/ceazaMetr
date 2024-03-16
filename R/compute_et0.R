#' Compute ET0 locally
#'
#' This function computes the reference evapotranspiration (ET0) using the Penman-Monteith method. It's based on "Step by step calculation of the Penman-Monteith Evapotranspiration (FAO-56 Method)" by H. Allen, R. Pereira, D. Raes, and M. Smith.
#'
#' There are 4 different ways to calculate the vapor pressure (ea) depending on the input data:
#' 1. If both rh_max and rh_min are provided, ea is calculated as the average of the vapor pressure at the maximum and minimum temperature.
#' 2. If only rh_max is provided, ea is calculated as the vapor pressure at the minimum temperature.
#' 3. If only rh_mean is provided, ea is calculated as the vapor pressure at the mean temperature.
#' 4. If none of the relative humidity values are provided, ea is calculated as the vapor pressure at the mean temperature.
#'
#' @param date Date or str. Date in YYYY-MM-DD format (Mandatory)
#' @param lat numeric. Latitude in decimal degrees (Mandatory)
#' @param elev numeric. Elevation in meters above sea level (Mandatory)
#' @param t_max numeric. Maximum temperature in degrees Celsius (Mandatory)
#' @param t_min numeric. Minimum temperature in degrees Celsius (Mandatory)
#' @param ws_mean numeric. Mean wind speed in m/s (Mandatory)
#' @param sr_mean numeric. Mean solar radiation in W/m^2/day (Mandatory)
#' @param rh_max numeric. Maximum relative humidity in percentage (Optional)
#' @param rh_min numeric. Minimum relative humidity in percentage (Optional)
#' @param rh_mean numeric. Mean relative humidity in percentage (Optional)
#' @param ws_h numeric. Height of the wind speed measurement in meters (Optional, default = 2)
#'
#' @return
#' A numeric value with the reference evapotranspiration (ET0) in mm/day.
#'
#' @examples
#' compute_et0(date='2024-03-09',lat=-30.631261,elev=80,t_min=7.91,t_max=27.69,
#'             rh_min=37.71,rh_max=93.2,ws_mean=1.364,sr_mean=272.824
#'             ) # WS Software ET0: 4.32 mm
#' compute_et0(date='2024-03-09',lat=-30.631261,elev=80,t_min=7.91,t_max=27.69,
#'             rh_max=93.2,ws_mean=1.364,sr_mean=272.824
#'             )# WS Software ET0: 4.32 mm
#' compute_et0(date='2024-03-09',lat=-30.631261,elev=80,t_min=7.91,t_max=27.69,
#'             rh_mean=66.81,ws_mean=1.364,sr_mean=272.824
#'             )# WS Software ET0: 4.32 mm
#' compute_et0(date='2024-03-09',lat=-30.631261,elev=80,t_min=7.91,t_max=27.69,
#'             ws_mean=1.364,sr_mean=272.824
#'             )# WS Software ET0: 4.32 mm
#' compute_et0(date='2024-03-02',lat=-30.592033,elev=292,t_min=11.82,
#'             t_max=26.72,rh_min=51.18,rh_max=94.1,ws_mean=2.471,
#'             sr_mean=259.846, ws_h = 5)# WS Software ET0: 4.29 mm
#'
#' @export
compute_et0 = function(date,
                       lat,
                       elev,
                       t_max,
                       t_min,
                       ws_mean,
                       sr_mean,
                       rh_max=NULL,
                       rh_min=NULL,
                       rh_mean=NULL,
                       ws_h = 2){
  if(!all(sapply(list(lat, elev, t_max, t_min,  ws_mean, sr_mean), is.numeric))){
    stop("lat, elev, t_max, t_min, ws_mean, sr_mean should be numeric")
  }
  if(!lubridate::is.Date(date)){
    if(is.character(date)){
      if(grepl(pattern = "^\\d{4}-\\d{2}-\\d{2}$", x = date)){
        date = as.Date(date)
      }else{
        stop("date should be in YYYY-MM-DD format")
      }
    }
  }
  t_mean = (t_max + t_min)/2
  sr_mj = sr_mean*0.0864
  ws_mean = ws_mean*(4.87/(log(67.8*ws_h-5.42)))
  delta = 4098*(0.6108*exp((17.27*t_mean)/(t_mean+237.3)))/((t_mean+237.3)^2)
  P = 101.3*((293-0.0065*elev)/293)^5.26
  gamma = 0.000665*P
  DT = delta/(delta+gamma*(1+0.34*ws_mean))
  PT = gamma/(delta+gamma*(1+0.34*ws_mean))
  TT = (900/(t_mean+273))*ws_mean
  e_tmax = 0.6108*exp((17.27*t_max)/(t_max+237.3))
  e_tmin = 0.6108*exp((17.27*t_min)/(t_min+237.3))
  e_s = (e_tmax+e_tmin)/2
  if(!is.null(rh_max)){
    if(!is.null(rh_min)){
      if(!all(sapply(list(rh_max, rh_min), is.numeric))){
        stop("rh_max and rh_min should be numeric")
      }else{
        ea = (e_tmin*(rh_max/100)+e_tmax*(rh_min/100))/2
      }
    }else{
      if(!is.numeric(rh_max)){
        stop("rh_max should be numeric")
      }else{
        ea = e_tmin*(rh_max/100)
      }
    }
  }else{
    if(!is.null(rh_mean)){
      if(!is.numeric(rh_mean)){
        stop("rh_mean should be numeric")
      }else{
        ea = (rh_mean/100)*((e_tmin+e_tmax)/2)
      }
    }else{
      ea = 0.6108*exp((17.27*t_mean)/(t_mean+237.3))
    }
  }
  j = as.numeric(format(date, format = "%j"))
  dr = 1+0.033*cos(2*pi*j/365)
  delta_s = 0.409*sin((2*pi*j/365)-1.39)
  lat_r = lat*pi/180
  ws = acos(-tan(lat_r)*tan(delta_s))
  ra = (24*60/pi)*0.0820*dr*(ws*sin(lat_r)*sin(delta_s)+cos(lat_r)*cos(delta_s)*sin(ws))
  rso = (0.75+0.00002*elev)*ra
  rns = (1-0.23)*sr_mj
  rnl = 0.000000004903*(((t_max+273.16)^4+(t_min+273.16)^4)/2)*(0.34-0.14*sqrt(ea))*((1.35*sr_mj/rso)-0.35)
  rn = rns-rnl
  rng = 0.408*rn
  etrad = DT*rng
  etwind = PT*TT*(e_s-ea)
  et0 = etrad+etwind
  return(et0)
}
