#' Calculate IndyCar Full Season Stats
#'
#' This function calculates full season basic and advanced IndyCar stats.
#' Advanced stats are not available for all seasons, so some columns might be
#' 'NA' or have default values in them.
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
    dplyr::filter(year==season) %>%
    dplyr::left_join(afp, by=c("st" = "st")) %>%
    dplyr::mutate(extra_positions=xFP-fin)

  #merge in average passing efficiency
  dt <- dt %>%
    dplyr::mutate(passEff = passesFor/(passesFor+passesAgainst),
           passEff = ifelse(is.na(passEff), .5, passEff)) %>%
    dplyr::left_join(avgPE, by="st") %>%
    dplyr::mutate(adj_pass_eff = passEff-avgPE)

  driver_season_stats <- dt %>%
    dplyr::group_by(driver) %>%
    dplyr::summarise(favorableStart = ifelse(lapOneChange>=0, 1,
                                   ifelse(lapOneChange<0, 0, NA)),
           StartRetention = 100*mean(favorableStart),
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
           RunningCheck = ifelse(status=="running",1,0),
           RunPerc = 100*mean(RunningCheck),
           AFS = mean(fastLapRank),
           Top5Perc = 100*(sum(inTopFive)/sum(laps)),
           AEP = mean(extra_positions)) %>%
    #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
    #dplyr::distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS) %>%
    dplyr::select(driver, Races, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart)
  return(driver_season_stats)
  }

  else{
    #calculate average pass efficiency
    avgPE <- dt %>% dplyr::filter(!is.na(passesFor)) %>%
      dplyr::select(driver, st, passesFor, passesAgainst) %>%
      dplyr::mutate(passEff = passesFor/(passesFor+passesAgainst),
             passEff = ifelse(is.na(passEff),.5,passEff)) %>%
      dplyr::group_by(st) %>%
      dplyr::summarise(avgPE = mean(passEff)) %>%
      dplyr::select(st, avgPE)

    #Calculate AFP from every starting position
    afp <- dt %>%
      dplyr::group_by(st) %>%
      dplyr::summarise(xFP = mean(fin))

    dt <- dt %>%
      dplyr::filter(year==season, type==track_type) %>%
      dplyr::left_join(afp, by=c("st" = "st")) %>%
      dplyr::mutate(extra_positions=xFP-fin)

    dt <- dt %>%
      dplyr::mutate(passEff = passesFor/(passesFor+passesAgainst),
             passEff = ifelse(is.na(passEff), .5, passEff)) %>%
      dplyr::left_join(avgPE, by="st") %>%
      dplyr::mutate(adj_pass_eff = passEff-avgPE)

    driver_season_stats <- dt %>%
      dplyr::group_by(driver) %>%
      dplyr::mutate(percFavorablePass = 100*(sum(passesFor)/(sum(passesFor)+sum(passesAgainst))),
             favorableStart = ifelse(lapOneChange>=0, 1,
                                     ifelse(lapOneChange<0, 0, NA)),
             StartRetention = 100*mean(favorableStart),
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
             ATP25 = mean(atp25),
             DevATP25 = sd(atp25),
             PassEff = 100*mean(passEff),
             AdjPassEff = 100*mean(adj_pass_eff),
             RunningCheck = ifelse(status=="running",1,0),
             RunPerc = 100*mean(RunningCheck),
             AFS = mean(fastLapRank),
             Top5Perc = 100*(sum(inTopFive)/sum(laps)),
             AEP = mean(extra_positions)) %>%
      #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
      dplyr::distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS) %>%
      dplyr::select(driver, Races, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart)
  return(driver_season_stats)
  }
}
