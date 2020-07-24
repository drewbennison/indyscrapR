#' Load IndyCar Elo Ratings
#'
#' This function loads in the current IndyCar Elo ratings.
#'
#' @importFrom magrittr "%>%"
#' @export
current_elo_ratings <- function() {
  elo_ratings_file <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv")
  elo_ratings <- elo_ratings_file %>% dplyr::filter(year>2000) %>%
    dplyr::mutate(date=ymd(date)) %>%
    dplyr::group_by(driver) %>%
    dplyr::slice(dplyr::which.max(as.Date(date, '%m/%d/%Y'))) %>%
    dplyr::mutate(EloRating = round(EloRating),
           PreviousEloRating = round(PreviousEloRating)) %>%
    dplyr::arrange(-EloRating) %>%
    dplyr::select(-year) %>%
    dplyr::select(-PreviousEloRating)
  return(elo_ratings)
}

#' Load Historical IndyCar Elo Ratings
#'
#' This function returns historical IndyCar Elo ratings.
#' @export
historical_elo_ratings <- function() {
elo_ratings_file <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv")
elo_ratings <- elo_ratings_file %>% dplyr::filter(year>2000) %>%
  dplyr::select(driver, date, year, EloRating) %>%
  dplyr::rename(season = year)
return(elo_ratings)
}
