#' This function reads a csv file and returns a table
#' An error message will be printed if the file does not exist
#' Another error message will be printed if you download an invalid laft-hand side to the assignment
#'
#' @param filename  a csv file that may or may not exist
#'
#' @return a dplyr tbl_df, a wrapper around the data that came from the csv file.
#'     If the file does not exist, an error message will be printed.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @source extdate/accident_year.csv.bz2
#'
#'
#' @export
#'
#' @examples
#' \dontrun{x <- fars_read('myFile.csv')}
#' \dontrun{system.file("extdata", "accident_year.csv.bz2", package = "packFars"). }
#'

fars_read <- function(filename) {
  
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Title make_filename
#'
#' This function defines years as an integer and includes the number of accidents per year
#'
#' @param year  A year as a string, with no restrictions on format
#'
#' @return a filename in the format 'accident_year.csv.bz2'. As a side effect, the filename is printed by the function.
#'
#' @source extdate/accident_year.csv.bz2
#' 
#' @examples newFileName <- make_filename('2017')
#' \dontrun{system.file("extdata", "accident_year.csv.bz2", package = "packFars")}

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Title fars_read_years
#'
#' This function assigns invalid years to NULL and assigns those years with more than one to a list
#'
#' An error will be thrown and the function will be halted if the year is invalid
#'
#' Uses make_filename(year)
#'      fars_read(file)
#'
#' @param years one or more years as an atomic value or a list
#'
#' @return Creates one or more datasets based on year number.  Returns NULL if there is an error
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
#'
#' @examples
#' \dontrun{
#'      fars_read_years(1999)
#'      fars_read_years(as.list(1999, 2000, 2001))
#'      fars_read_years(1999:2016)
#' }

fars_read_years <- function(years) {
  lapply(years, function(year) {
    MONTH <- year <- NULL
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

#' Title fars_summarize_years
#'
#' Takes those obsverations with more than one years and places them in a new data
#' frame which includes the number of values, year, month, and spread of year
#'
#' Uses fars_read_years(years)
#'
#' @param years One or more years, no error checking
#'
#' @return A wide data frame of counts by month and year,
#'
#' @importFrom  dplyr bind_rows
#' @importFrom  dplyr group_by
#' @importFrom  dplyr summarize
#' @importFrom  tidyr spread
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'      fars_summarize_years(1999)
#'      fars_summarize_years(as.list(1999, 2000, 2001))
#'      fars_summarize_years(1999:2016)
#' }


fars_summarize_years <- function(years) {
  year <- MONTH <- n <- NULL
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Title fars_map_state
#'
#' This function stores these tables in a new file and accepts a state number and year
#' from the program
#'
#' Error checks to make sure the state number exists
#' If so, uses maps and graphics to create plots based on latitude and longitude from the data file
#'
#' Uses make_filename(year)
#'      fars_read(filename)
#'
#' @param state.num Number of a state
#' @param year The year in question
#'
#' @return A plot or set of plots based on latitude and longitude from the data file
#'
#' @export

#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
#'

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  STATE <- NULL
  
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