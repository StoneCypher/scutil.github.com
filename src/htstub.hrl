
-record(htstub_uri, {

    scheme       = http      :: atom(),
    user         = undefined :: string() | undefined,
    password     = undefined :: string() | undefined,
    host                     :: string(),
    port         = 80        :: pos_integer(),
    path         = ""        :: string(),
    path_params  = []        :: list(),
    query_params = []        :: list(),
    fragment     = undefined :: string() | undefined

}).





-record(htstub_config, {

    start_immediate = true                         :: true | false,
    ip              = {0,0,0,0}                    :: inet:ipaddress(),
    addrtype        = inet                         :: inet:address_family(),
    port            = 80                           :: pos_integer(),
    verbose         = quiet                        :: verbose | quiet,
    server_name     = "htstub server"              :: string(),
    handler         = fun htstub:default_handler/1 :: function(),
    middleware      = []                           :: list()

}).





-record(htstub_request, {

    request   = true,
    headers   = {0,0,0,0} :: list(),
    arguments = inet      :: list()

}).
