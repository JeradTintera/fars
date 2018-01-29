#'Read in data from the Fatality Analysis Reporting System (FARS).
#'
#'\code{fars_read} reads in the data from a csv file, returning a
#'dataframe
#'
#'@param filename A character string with a valid file name.
#'
#'@return This function returns a dataframe.
#'
#'@details This function will raise an error if the file referenced
#' in \code{filename} doesn't exist.
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#'
#'@examples df <- fars_read(file)
#'head(df)


fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#'Construct a file name.
#'
#'\code{make_file} takes in a character string referencing a year
#'and returns a file name as a character string.
#'
#'@param year A character string referencing a valid year,
#'four-digit format (i.e. 2013).
#'
#'@return This function returns a file name as a character string.
#'
#'@examples filename <- make_file('2013')
#'{filename}

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#'Retrieve a selection of data from FARS files.
#'
#'\code{fars_read_years} reads in a vector of character strings
#'representing years. It returns a vector of data frames with
#'the month and year of every accident in the corresponding year.
#'
#'@param years A vector of character strings representing years,
#'four-digit format (i.e. 2013).
#'
#'@return This function returns a vector of data frames.
#'
#'@details This function will raise an error if there is no file
#'that matches the year in \code{years}.
#'
#'@importfrom dplyr mutate select
#'@importFrom magrittr %>%
#'
#'@examples years <- c('2013', '2014')
#'years_vector <- fars_read_years(years)

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

#'Summarize a selection of data from FARS files.
#'
#'\code{fars_summarize_years} reads in a vector of character strings
#'representing years. It returns a data frame with the number of accidents
#'by year and month, with a column for every year in \code{years} and
#'a row for every month.
#'
#'@param years A vector of character strings representing years,
#'four-digit format (i.e. 2013).
#'
#'@return This function returns a data frame.
#'
#'@details This function will raise an error if there is no file
#'that matches the year in \code{years}.
#'
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom tidyr spread
#'@importFrom magrittr %>%
#'
#'@examples years <- c('2013', '2014')
#'accident_summary <- fars_summarize_years(years)
#'
#'@export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#'Plot accident data on a map.
#'
#'\code{fars_map_state} takes a state and a year and plots on a map
#'all accidents from the FARS dataset for that state and year.
#'
#'@param state.num An integer that represents a state. For more information,
#'please see the \href{https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812449}{FARS data manual}, pg. 60.
#'Single digit numbers should be formatted as single digit, not double digit
#'as indicated in the manual.
#'
#'@param year A character string representing a year,
#'four-digit format (i.e. 2013)
#'
#'@return None
#'
#'@details This function will raise an error if there is no file
#'that matches \code{year}. This function will also raise an error
#'if the \code{state.num} doesn't match a number from the data.
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@examples fars_map_state(36,'2013')
#'
#'@export

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
