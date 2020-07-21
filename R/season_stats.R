#' Calculate IndyCar Full Season Stats
#'
#' This function calculates full season basic and advanced IndyCar stats.
#' Advanced stats are not available for all seasons, so some columns might be
#' 'NA' or have default values in them.
#' @export
season_stats <- function(season=2020, track_type="all") {
  dt <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

  if(track_type=="all") {
  #calculate average pass efficiency
  avgPE <- dt %>% filter(!is.na(passesFor)) %>%
    select(driver, st, passesFor, passesAgainst) %>%
    mutate(passEff = passesFor/(passesFor+passesAgainst),
           passEff = ifelse(is.na(passEff),.5,passEff)) %>%
    group_by(st) %>%
    summarise(avgPE = mean(passEff)) %>%
    select(st, avgPE)

  #Calculate AFP from every starting position
  afp <- dt %>%
    group_by(st) %>%
    summarise(xFP = mean(fin))

  dt <- dt %>%
    filter(year==season) %>%
    left_join(afp, by=c("st" = "st")) %>%
    mutate(xFPDifference=xFP-fin)

  dt <- dt %>%
    mutate(passEff = passesFor/(passesFor+passesAgainst),
           passEff = ifelse(is.na(passEff), .5, passEff)) %>%
    left_join(avgPE, by="st") %>%
    mutate(AdjPassEff = passEff-avgPE)

  driver_season_stats <- dt %>%
    group_by(driver) %>%
    mutate(percFavorablePass = 100*(sum(passesFor)/(sum(passesFor)+sum(passesAgainst))),
           favorableStart = ifelse(lapOneChange>=0, 1,
                                   ifelse(lapOneChange<0, 0, NA)),
           StartRetention = 100*mean(favorableStart),
           StartPM = sum(lapOneChange),
           Races = n(),
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
           AdjPassEff = 100*mean(AdjPassEff),
           RunningCheck = ifelse(status=="running",1,0),
           RunPerc = 100*mean(RunningCheck),
           AFS = mean(fastLapRank),
           Top5Perc = 100*(sum(inTopFive)/sum(laps)),
           AEP = mean(xFPDifference)) %>%
    #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
    distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS) %>%
    select(driver, Races, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart)
  return(driver_season_stats)
  }

  else{
    #calculate average pass efficiency
    avgPE <- dt %>% filter(!is.na(passesFor)) %>%
      select(driver, st, passesFor, passesAgainst) %>%
      mutate(passEff = passesFor/(passesFor+passesAgainst),
             passEff = ifelse(is.na(passEff),.5,passEff)) %>%
      group_by(st) %>%
      summarise(avgPE = mean(passEff)) %>%
      select(st, avgPE)

    #Calculate AFP from every starting position
    afp <- dt %>%
      group_by(st) %>%
      summarise(xFP = mean(fin))

    dt <- dt %>%
      filter(year==season, type==track_type) %>%
      left_join(afp, by=c("st" = "st")) %>%
      mutate(xFPDifference=xFP-fin)

    dt <- dt %>%
      mutate(passEff = passesFor/(passesFor+passesAgainst),
             passEff = ifelse(is.na(passEff), .5, passEff)) %>%
      left_join(avgPE, by="st") %>%
      mutate(AdjPassEff = passEff-avgPE)

    driver_season_stats <- dt %>%
      group_by(driver) %>%
      mutate(percFavorablePass = 100*(sum(passesFor)/(sum(passesFor)+sum(passesAgainst))),
             favorableStart = ifelse(lapOneChange>=0, 1,
                                     ifelse(lapOneChange<0, 0, NA)),
             StartRetention = 100*mean(favorableStart),
             StartPM = sum(lapOneChange),
             Races = n(),
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
             AdjPassEff = 100*mean(AdjPassEff),
             RunningCheck = ifelse(status=="running",1,0),
             RunPerc = 100*mean(RunningCheck),
             AFS = mean(fastLapRank),
             Top5Perc = 100*(sum(inTopFive)/sum(laps)),
             AEP = mean(xFPDifference)) %>%
      #SELECT DRIVER AND ANY VARIBLES BEFORE YOU SELECT DISTINCT
      distinct(driver, StartRetention, StartPM, Races, PMperStart, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS) %>%
      select(driver, Races, Pts, xPoints, AFP, DevFP, ASP, DevSP, ATP, DevATP, ATP25, DevATP25, PassEff, AdjPassEff, RunPerc, Top5Perc, AEP, AFS, StartRetention, StartPM, PMperStart)
  return(driver_season_stats)
  }
}
