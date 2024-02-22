#' Read a FARS data file
#' 
#' Reads a FARS (Fatality Analysis Reporting System) data file in CSV format.
#' 
#' @param filename The path to the FARS data file.
#' @return A tbl_df containing the data from the FARS data file.
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate FARS data file name
#' 
#' Generates the filename for a FARS data file for a given year.
#' 
#' @param year The year for which the FARS data file name is generated.
#' @return A character string representing the filename for the FARS data file.
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data files for multiple years
#' 
#' Reads FARS (Fatality Analysis Reporting System) data files for multiple years and combines them into a list.
#' 
#' @param years A vector of years for which FARS data files are to be read.
#' @return A list containing tbl_df objects for each year's FARS data.
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize FARS data for multiple years
#' 
#' Summarizes FARS (Fatality Analysis Reporting System) data for multiple years, counting the number of accidents per month.
#' 
#' @param years A vector of years for which FARS data is to be summarized.
#' @return A tbl_df containing the summarized data.
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot FARS data for a specific state and year
#' 
#' Plots FARS (Fatality Analysis Reporting System) data for a specific state and year on a map.
#' 
#' @param state.num The numeric code representing the state for which data is to be plotted.
#' @param year The year for which FARS data is to be plotted.
#' @return A map plot of FARS data for the specified state and year.
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
