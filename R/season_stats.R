#' Calculate full season stats
#'
#' This function calculates full season basic and advanced IndyCar stats.
#' Advanced stats are not available for all seasons right now.
#'
#' @param season (required): Season year, 4 digit format
#' @param track_type (required): Can be set to "all" (default), "oval", "road", or "street".
#'
#' @importFrom magrittr "%>%"
#' @export
season_stats <- function(season=2020, track_type="all") {
  dt <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

  if(track_type=="all") {

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

  #calculate season stats
  driver_season_stats <- dt %>%
    dplyr::filter(year==season) %>%
    dplyr::mutate(favorableStart = ifelse(lapOneChange>=0, 1,
                                          ifelse(lapOneChange<0, 0, NA)),
                  RunningCheck = ifelse(status=="running",1,0)) %>%
    dplyr::group_by(driver) %>%
    dplyr::summarise(StartRetention = 100*mean(favorableStart),
           StartPM = sum(lapOneChange),
           Races = dplyr::n(),
           PMperStart = StartPM/Races,
           Pts=sum(pts),
           xPoints = sum(xPts),
           AFP = mean(fin),
           DevFP = sd(fin),
           ASP = mean(st),
           DevSP = sd(st),
           ATP = mean(atp),
           DevATP = sd(atp),
           ATP25 = mean(atp25, na.rm = TRUE),
           DevATP25 = sd(atp25, na.rm = TRUE),
           PassEff = 100*mean(passEff),
           AdjPassEff = 100*mean(adj_pass_eff),
           RunPerc = 100*mean(RunningCheck),
           AFS = mean(fastLapRank),
           Top5Perc = 100*(sum(inTopFive)/sum(laps)),
           AEP = mean(extra_positions)) %>%
    dplyr::select(driver, Races, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart)
  return(driver_season_stats)
  }

  else{

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

    #calculate season stats
    driver_season_stats <- dt %>%
      dplyr::filter(type==track_type,
                    year==season) %>%
      dplyr::mutate(favorableStart = ifelse(lapOneChange>=0, 1,
                                            ifelse(lapOneChange<0, 0, NA)),
                    RunningCheck = ifelse(status=="running",1,0)) %>%
      dplyr::group_by(driver) %>%
      dplyr::summarise(StartRetention = 100*mean(favorableStart),
                       StartPM = sum(lapOneChange),
                       Races = dplyr::n(),
                       PMperStart = StartPM/Races,
                       Pts=sum(pts),
                       xPoints = sum(xPts),
                       AFP = mean(fin),
                       DevFP = sd(fin),
                       ASP = mean(st),
                       DevSP = sd(st),
                       ATP = mean(atp),
                       DevATP = sd(atp),
                       ATP25 = mean(atp25, na.rm = TRUE),
                       DevATP25 = sd(atp25, na.rm = TRUE),
                       PassEff = 100*mean(passEff),
                       AdjPassEff = 100*mean(adj_pass_eff),
                       RunPerc = 100*mean(RunningCheck),
                       AFS = mean(fastLapRank),
                       Top5Perc = 100*(sum(inTopFive)/sum(laps)),
                       AEP = mean(extra_positions)) %>%
      dplyr::select(driver, Races, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart)
    return(driver_season_stats)
  }
}
