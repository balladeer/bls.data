library(httr)

#' Get data from the BLS public API.
#'
#' @param series_id input character vector
#' @return data.frame with data for the requested series id.
#' @export
#' @examples
#' get_bls_data()
#' get_bls_data('LAUCN040010000000005')
get_bls_data <- function(series_id='LAUCN040010000000005') {
    url <- paste0('http://api.bls.gov/publicAPI/v2/timeseries/data/', series_id)
    url <- URLencode(url)
    response <- httr::GET(url, add_headers('content-type'='application/json'))

    c <- content(response)

    if (length(c$message) > 0){
        stop(c$message)
    } else {
        series_id <- c$Results$series[[1]]$seriesID
        data <- c$Results$series[[1]]$data

        df <- data.frame(year = as.numeric(sapply(data, '[[', 'year')),
                         period = sapply(data, '[[', 'period'),
                         periodName = sapply(data, '[[', 'periodName'),
                         value = sapply(data, '[[', 'value'))
        df <- df[order(df$year, df$period), ]
        return(df)
    }
}
