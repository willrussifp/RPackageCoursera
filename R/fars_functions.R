#' Read Fatality Analysis Reporting System data
#'
#' This is a simple function that silently reads a 
#'    csv file of Fatality Analysis Reporting System
#'    data into a tibble. It will check to make sure 
#'    the file exists before attempting to read it.
#'
#' @param filename A path (character) to the file 
#'    the function will read. Note that this can 
#'    be a compressed file.
#'
#' @return This function returns a tibble (a tidyverse
#'    version of a data frame) containing the Fatality 
#'    Analysis Reporting System data.
#'   
#' @examples
#' \dontrun {
#' fars_read("data/accident_2013.csv.bz2")
#' }
#' 
#' @import readr dplyr
#' 
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generate a filename based from a year
#'
#' This function generates the filename where
#'    the data for a specified year is stored.
#'    Note: The working directory must be set
#'    to the directory that contains the data
#'    files.
#'    
#' Note: This function will not warn you if 
#'    the file for the specified year does 
#'    not exist.
#'    
#' @param year The year as an integer or 
#'    character for which to generate the
#'    filename.
#'    
#' @return This function returns the filename
#'    as a character string of Fatality 
#'    Analysis Reporting System data for 
#'    the specified year.
#'    
#' @examples
#' make_filename(2013)
#' make_filename("2013")
#' 
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple years of FARS data
#' 
#' This function returns a tibble of tidy
#'    data (one row per observation) with the
#'    month and year of the observation.
#' 
#' This function will return a warning
#'    if the data for a specified year 
#'    cannot be found.
#'    
#' Note: This function requires magrittr or tidyr
#'    is already loaded or it will return
#'    an error.
#' 
#' @param years A vector or list of integers
#'    or characters of years for which to 
#'    return the data
#'    
#' @return This function returns a list of
#'   tibbles, one per specified year, with 
#'   one row per observation. The columns 
#'   return are MONTH and year.
#'   
#' @examples
#' \dontrun {
#' fars_read_years(2013)
#' fars_read_years(c(2013, 2014))
#' fars_read_years(2013:2015)
#' fars_read_years(c("2013", "2015"))
#' fars_read_years(list(2013, 2014))
#' }
#' 
#' @import dplyr
#' 
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

#' Summarize FARS data across years
#' 
#' This function creates a summary table
#'    of the counts of observations by 
#'    month across the specified years.
#'    
#'    This function will return a warning
#'    if the data for a specified year 
#'    cannot be found.
#'    
#'    Note: This function requires magrittr
#'    is already loaded or it will return
#'    an error.
#'    
#' @param years A vector or list of integers
#'    or characters of years for which
#'    to summarize the data
#' 
#' @return This function returns a tibble
#'    with one row for each month and a 
#'    column for the month number followed
#'    by one column per specified year.
#'    These columns contain the observation
#'    counts for the relevant month and year.
#' 
#' @examples 
#' \dontrun{
#' fars_summarize_years(2013)   
#' fars_summarize_years(c(2013, 2014))
#' fars_summarize_years(2013:2015)
#' fars_summarize_years(c("2013", "2015"))
#' fars_summarize_years(list(2013, 2014))
#' }
#' 
#' @import dplyr tidyr
#' 
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
           dplyr::group_by(year, MONTH) %>% 
           dplyr::summarize(n = n()) %>%
           tidyr::spread(year, n)
}

#' Plot FARS observation locations within a state
#' 
#' This function plots a map of the specified state
#'    and the location of each observation in the 
#'    FARS data for a specified year. 
#'    
#' The function will return a message if there 
#'    were no accidents for the specified state
#'    and year.
#'    
#' Note: The function will return an error if the 
#'    specified state.num is not valid. Additionally,
#'    some state numbers, for example "2", cause an 
#'    error due to specified plot regions being out 
#'    of bounds.
#'    
#' @param state.num An integer or character string
#'    of an integer specifing which state to plot. 
#'    An error will be thrown if an invalid state 
#'    number is specified.
#' 
#' @param year An integer or character string of 
#'    an integer specifying the year for which 
#'    to plot the accidents.
#'    
#' @return If there is data for the specified
#'    state and year, the function will plot a
#'    map of the state and the locations of
#'    the accidents for that year. If there are 
#'    no recorded accidents for that year and 
#'    state then the function will return a 
#'    message to notify that case.
#' 
#' @examples 
#' \dontrun{
#' fars_map_state(13, 2013)
#' fars_map_state(13, "2013")
#' fars_map_state("13", 2013)
#' fars_map_state("13", "2013")
#' }
#' 
#' @import dplyr maps
#'   
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
