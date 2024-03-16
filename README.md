![logo](man/figures/logo.png)

# ceazaMetr

## Description
 
`ceazaMetr` is an R package for using [CEAZA-Met](https://www.ceazamet.cl) Web Services for getting stations information, sensors metadata and the data of CEZA-Met meteorological network. The package is designed to work with the [CEAZA-Met API](https://www.ceazamet.cl/ws/pop_ws_doc.php), which provides access to the stations of the [Center for Advanced Studies in Arid Zones](http://www.ceaza.cl) (CEAZA) in Chile.

Also, some useful functions will be added in future realeases. So far, the package is in development has the following extra function:

 - `compute_et0()`: Calculate the reference evapotranspiration (ETo) using the FAO-56 Penman-Monteith method. This functions requires latitude, elevation, date, maximum temperature, minimum temperature, mean wind speed and mean solar radiation. There are 4 different methods to calculate actual vapor pressure, which are:
 
   - 1st method: using maximum and minimum Relative Humidity (%).
   - 2nd method: using maximum Relative Humidity (%).
   - 3rd method: using mean Relative Humidity (%).
   - 4th method: using minimum temperature, asssuming when temperature is close to t_min, the air is (or close to) saturated.

## Installation

You can install the development version from [GitHub](https://github.com/aldotapia/ceazaMetr) with:

```R
# install.packages("devtools")
devtools::install_github("aldotapia/ceazaMetr")
```

## Usage

There are 5 functions in this small package, which are:

- `getStationsList()`: Get the stations information of the CEAZA-Met network.
- `getSensorsList()`: Get the sensors codes and metadata of the CEAZA-Met network.
- `getSensorsData()`: Get the data serie of a specific sensor of the CEAZA-Met network.
- `et0()`: Calculate the reference evapotranspiration (ETo) using the FAO-56 Penman-Monteith method using CEAZA-Met web services.
- `compute_et0()`: Calculate the reference evapotranspiration (ETo) using the FAO-56 Penman-Monteith method using the package functions.

### Examples

```R
library(ceazaMetr)

getStationsList()
getSensorsList(e_cod = 6, user = "anon@host.com", tm_cod = "ta_c")
getSensorData(
  s_cod = "RPLTA", start_date = "2012-08-20",
  end_date = "2012-08-27", user = "anon@host.com"
)
et0(lat = -30, lon = -70, elev = 20,
    month = 1, t_max = 29.6, t_min = 22.8,
    rh_max = 90, rh_min = 70, ws_mean = 0.2,
    sr_mean = 200)
compute_et0(date = '2024-03-09', lat = -30.631261, elev = 80,
            t_min = 7.91, t_max = 27.69, rh_min = 37.71,
            rh_max = 93.2, ws_mean = 1.364, sr_mean = 272.824) 
```

More examples in each function documentation.

### Call to inform

If you are going to use this package, please register sending an email to ceazamet@ceaza.cl, with: name, email, sector, organization, use that data will have, frequency and amount of data downloaded regularly.

Also, cite the source of the data in your work, for example:

 - *Data provided by CEAZA through the CEAZA-Met system*
 - *Datos provistos por CEAZA a traves del sistema CEAZA-Met*
