#' Plot the season-long championship points standings
#'
#'
#' @importFrom magrittr "%>%"
#' @export

plot_championship_points <- function(season=2020, drivers){
  if(is.na(drivers)){
    warning("No drivers entered. Please set drivers=c('Driver1', 'Driver')")
  }

  dt <- read.csv("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/master_backup/indycar_results.csv")

  dt2 <- dt %>% dplyr::filter(year==season, driver %in% drivers) %>%
    dplyr::select(driver, raceNumber, pts) %>%
    dplyr::group_by(driver) %>%
    dplyr::arrange(raceNumber) %>%
    dplyr::mutate(sum_points = cumsum(pts)) %>%
    ggplot2::ggplot(ggplot2::aes(x=raceNumber, y=sum_points, color=driver)) + ggplot2::geom_line()

  return(dt2)
}
