#' Load IndyCar Race Results
#'
#' This function loads in results and stats from IndyCar races.
#' @export
race_results <- function(year=2020, race_number=1) {
  dt <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")
  #can use advanced stats
  if(year>2018){
    dt2 <- dt %>% filter(year==year, raceNumber==race_number) %>%
      select(-ledPts, -ledMostPts, -polePoints, -xPtsATP25, -xPts)
    return(dt2)
  } else {
    dt2 <- dt
    return(dt2)
  }

  }
