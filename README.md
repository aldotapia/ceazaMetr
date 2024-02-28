<center>
    <img src="man/figures/logo.png">
</center>
----------------
# ceazaMetr

## Description
 
`ceazaMetr` is an R package for using [CEAZA-Met](https://www.ceazamet.cl) Web Services for getting stations information, sensors metadata and the data of CEZA-Met meteorological network. The package is designed to work with the [CEAZA-Met API](https://www.ceazamet.cl/ws/pop_ws_doc.php), which provides access to the stations of the [Center for Advanced Studies in Arid Zones](http://www.ceaza.cl) (CEAZA) in Chile.


## Installation

You can install the development version from [GitHub](https://github.com/aldotapia/ceazaMetr) with:

```R
# install.packages("devtools")
devtools::install_github("aldotapia/ceazaMetr")
```

## Usage

There are 3 functions in this small package, which are:

- `getStationsList()`: Get the stations information of the CEAZA-Met network.
- `getSensorsList()`: Get the sensors codes and metadata of the CEAZA-Met network.
- `getSensorsData()`: Get the data serie of a specific sensor of the CEAZA-Met network.

### Examples

```R
library(ceazaMetr)

getStationsList()
getSensorsList(e_cod = 6, user = "anon@host.com", tm_cod = "ta_c")
getSensorData(
  s_cod = "RPLTA", start_date = "2012-08-20",
  end_date = "2012-08-27", user = "anon@host.com"
)
```

More examples in each function documentation.
