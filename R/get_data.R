library(httr)

#' Get data from the BLS public API.
#'
#' @param series_id input character vector
#' @return data.frame with data for the requested series id.
#' @export
#' @examples
#' get_bls_data(registration_key='BLS-provided key')
#' get_bls_data('LAUCN040010000000005', registration_key='BLS-provided key')
#' get_bls_data(c("LAUCN040010000000005", "LAUCN040010000000006"), registration_key='BLS-provided key')
get_bls_data <- function(series_ids='LAUCN040010000000005', registration_key=NA) {
    url <- 'https://api.bls.gov/publicAPI/v2/timeseries/data/'

    # Convert `series_ids` into a list
    if (length(series_ids) < 2) {
        series_ids <- list(series_ids)
    }

    # Check registration_key is provided
    if (class(registration_key) == 'logical') {
        stop('argument registration_key required')
    }

    body_list = list(seriesid = series_ids, registrationKey=registration_key)
    response = httr::POST(url, body=rjson::toJSON(body_list), config=add_headers('content-type'='application/json'))

    c <- httr::content(response)

    if (c$status != "REQUEST_SUCCEEDED") {
        print(c$status)
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

    # Make sure series_id columns are in same relative order
    if (length(series_ids) >= 2) {
        final_df <- final_df[, c('year', 'period', 'periodName', series_ids)]
    }

    return(final_df)
}
