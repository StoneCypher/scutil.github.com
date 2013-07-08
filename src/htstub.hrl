
-record(htstub_uri, {

    scheme       = "http"    :: atom(),
    user         = undefined :: string() | undefined,
    password     = undefined :: string() | undefined,
    host                     :: string(),
    port         = 80        :: pos_integer(),
    path         = ""        :: string(),
    path_params  = []        :: list(),
    query_params = []        :: list(),
    fragment     = undefined :: string() | undefined

}).
