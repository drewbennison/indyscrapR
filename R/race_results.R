#' Load IndyCar Race Results
#'
#' This function loads in results and stats from IndyCar races.
#' @importFrom magittr "%>%"
#' @export
race_results <- function(season=2020, race_number=0) {
  dt <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")
  #return full season stats
  if(race_number !=0){
    dt2 <- dt %>% dplyr::filter(year==season, raceNumber==race_number) %>%
      dplyr::select(-xPtsATP25, -xPtsATP)
    return(dt2)
  } else {
    dt2 <- dt %>% dplyr::filter(year==season) %>%
      dplyr::select(-xPtsATP25, -xPtsATP)
    return(dt2)
  }
  }
