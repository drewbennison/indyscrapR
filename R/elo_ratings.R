#' Load IndyCar Elo Ratings
#'
#' This function loads in the current IndyCar Elo ratings.
#' @export
current_elo_ratings <- function() {
  elo_ratings_file <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv")
  elo_ratings <- elo_ratings_file %>% filter(year>2000) %>%
    mutate(date=ymd(date)) %>%
    group_by(driver) %>%
    slice(which.max(as.Date(date, '%m/%d/%Y'))) %>%
    mutate(EloRating = round(EloRating),
           PreviousEloRating = round(PreviousEloRating)) %>%
    arrange(-EloRating) %>%
    select(-year) %>%
    select(-PreviousEloRating) %>%
    rename(Driver = driver) %>%
    rename(LastUpdated = date)
  return(elo_ratings)
}

#' Load Historical IndyCar Elo Ratings
#'
#' This function loads in historical IndyCar Elo ratings.
#' @export
historical_elo_ratings <- function() {
elo_ratings_file <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv")
elo_ratings <- elo_ratings_file %>% filter(year>2000) %>%
  mutate(EloRating = round(EloRating)) %>%
  select(driver, date, year, EloRating) %>%
  rename(season = year)
return(elo_ratings)

  }
