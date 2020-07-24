#' Load IndyCar Race Results
#'
#' This function loads in results and stats from IndyCar races.
#' @importFrom magrittr "%>%"
#' @export
race_results <- function(season=2020, race_number=0) {
  dt <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

  #calculate average passing efficiency
  avgPE <- dt %>% dplyr::filter(!is.na(passesFor)) %>%
    dplyr::select(driver, st, passesFor, passesAgainst) %>%
    dplyr::mutate(passEff = passesFor/(passesFor+passesAgainst),
                  passEff = ifelse(is.na(passEff),.5,passEff)) %>%
    dplyr::group_by(st) %>%
    dplyr::summarise(avgPE = mean(passEff)) %>%
    dplyr::select(st, avgPE)

  #Calculate average finishing position from every starting position
  afp <- dt %>%
    dplyr::group_by(st) %>%
    dplyr::summarise(xFP = mean(fin))

  #merge in extra_positions
  dt <- dt %>%
    dplyr::left_join(afp, by=c("st" = "st")) %>%
    dplyr::mutate(extra_positions=xFP-fin)

  #merge in average passing efficiency
  dt <- dt %>%
    dplyr::mutate(passEff = passesFor/(passesFor+passesAgainst),
                  passEff = ifelse(is.na(passEff), .5, passEff)) %>%
    dplyr::left_join(avgPE, by="st") %>%
    dplyr::mutate(adj_pass_eff = passEff-avgPE)


  #return full season stats
  if(race_number !=0){
    dt2 <- dt %>% dplyr::mutate(favorableStart = ifelse(lapOneChange>=0, 1,
                                                 ifelse(lapOneChange<0, 0, NA)),
                                RunningCheck = ifelse(status=="running",1,0)) %>%
      dplyr::filter(year==season, raceNumber==race_number) %>%
      dplyr::select(-xPtsATP25, -xPtsATP)
    return(dt2)
  } else {
    dt2 <- dt %>% dplyr::mutate(favorableStart = ifelse(lapOneChange>=0, 1,
                                                 ifelse(lapOneChange<0, 0, NA)),
                                RunningCheck = ifelse(status=="running",1,0)) %>%
      dplyr::filter(year==season) %>%
      dplyr::select(-xPtsATP25, -xPtsATP)
    return(dt2)
  }
  }
