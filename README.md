Working on a package to make querying data from the BLS public API in R a bit easier.

Provides two main functions:

* `get_bls_info()` returns information regarding a series id (e.g survey name, series title, etc)
* `get_bls_data()` retrieves a data.frame containing the data for the requested series ids.

Both functions have a `registration_key` argument.  You must request a key from the [BLS website](http://data.bls.gov/registrationEngine/)

TODO:

* Add input validation
* Add response validation
* Add some tests

During all of the above steps: improve documentation.
