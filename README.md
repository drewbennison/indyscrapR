# indyscrapR
`indyscrapR` is an R package written by Drew Bennison that retrieves IndyCar data complied by [*The Single Seater*](https://thesingleseater.com/). The data is a combination of stats tracked by *TSS* and data from IndyCar on race results. 

## Installation
The package can be installed via GitHub with:
``` R
devtools::install_github("drewbennison/indyscrapR")
```
After installation, load the package using:
``` R
library("indyscrapR")
```
To use `indyscrapR`, you will also need the following packages:
``` R
#install.packages("tidyverse")
library("tidyverse")
#install.packages("lubridate")
library("lubridate")
```
## Functions
`race_results(season, race_number)` returns stats for individual races or all races in a season if `race_number` is set to 0.

`season_stats(season, track_type` returns full season stats by year and track type. The default `track_type` is "all", but can be set to "oval", "road", or "street".

`current_elo_ratings()` returns the most recently updated Elo ratings for all IndyCar drivers starting in 2008. 

`historical_elo_ratings()` returns historical Elo ratings for all drivers by date.
