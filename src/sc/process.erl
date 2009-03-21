





%% @since Version 175

get_linked_processes() ->

    [U] = [ V || 
        {links,V} <- process_info(self()) 
    ],

    U.
