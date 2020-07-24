#' Load IndyCar Race Results
#'
#' This function loads in results and stats from IndyCar races.
#' @export
race_results <- function(season=2020, race_number=0) {
  dt <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")
  #return full season stats
  if(race_number !=0){
    dt2 <- dplyr::filter(dt, year==season, raceNumber==race_number)
    dt2 <- dplyr::select(dt, -xPtsATP25, -xPtsATP)
    return(dt2)
  } else {
    dt2 <- dplyr::filter(dt, year==season)
    dt2 <- dplyr::select(dt2, -xPtsATP25, -xPtsATP)
    return(dt2)
  }
  }
