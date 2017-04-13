library(httr)
library(rjson)

#' Get descriptive information about a BLS SeriesID.
#'
#' You mush supply a registration_key
#'
#' @param series_ids input character vector
#' @return nested list with series ids as top-level named elements
#' @export
#' @examples
#' get_bls_info()
#' get_bls_info(series_id=c("LAUCN040010000000005", "LAUCN040010000000006"))
get_bls_info <- function(series_ids='LAUCN040010000000005', registration_key=NA) {
    url <- 'https://api.bls.gov/publicAPI/v2/timeseries/data/'

    # Convert `series_ids` into a list
    if (length(series_ids) < 2) {
      series_ids <- list(series_ids)
    }

    # Check registration_key
    if (class(registration_key) == 'logical') {
        stop('argument registration_key required')
    }

    body_list = list(seriesid = series_ids, registrationKey=registration_key, catalog=TRUE)
    response = httr::POST(url, body=rjson::toJSON(body_list), config=add_headers('content-type'='application/json'))

    c <- httr::content(response)

    if (c$status != "REQUEST_SUCCEEDED") {
        print(c$status)
        stop(c$message)
    }

    result = list()
    for (d in c$Results$series) {
        result[[d$seriesID]] <- d$catalog
    }
    return(result)
}
