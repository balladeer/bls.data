library(httr)

#' Get data from the BLS public API.
#'
#' @param series_id input character vector
#' @return data.frame with data for the requested series id.
#' @export
#' @examples
#' get_bls_data(registration_key='BLS-provided key')
#' get_bls_data('LAUCN040010000000005', registration_key='BLS-provided key')
get_bls_data <- function(series_ids='LAUCN040010000000005', registration_key=NA) {
    url <- 'http://api.bls.gov/publicAPI/v2/timeseries/data/'

    # Convert `series_ids` into a list
    if (length(series_ids) < 2) {
        series_ids <- list(series_ids)
    }

    # Check registration_key
    if (class(registration_key) == 'logical') {
        stop('argument registration_key required')
    }

    body_list = list(seriesid = series_ids, registrationKey=registration_key)
    response = httr::POST(url, body=rjson::toJSON(body_list), config=add_headers('content-type'='application/json'))

    c <- httr::content(response)

    if (length(c$message) > 0) {
        stop(c$message)
    }
    final_df <- NA
    first = TRUE
    for (current in c$Results$series) {
        cur_series_id <- current$seriesID
        df <- data.frame(year=as.numeric(sapply(current$data, '[[', 'year')),
                         period=sapply(current$data, '[[', 'period'),
                         periodName=sapply(current$data, '[[', 'periodName'),
                         value=as.numeric(sapply(current$data, '[[', 'value')))

        names(df) <- c('year', 'period', 'periodName', cur_series_id)
        if (first) {
            final_df <- df
        } else{
            final_df <- merge(final_df, df, by=c('year', 'period', 'periodName'), all=TRUE, sort=TRUE)
        }
        first = FALSE
    }
    return(final_df)
}
