# indyscrapR
`indyscrapR` is an R package written by Drew Bennison that retrieves IndyCar data complied by [*The Single Seater*](https://thesingleseater.com/). The data is a combination of stats tracked by *TSS* and data from IndyCar on race results. 

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
