




%% @since Version 211

make_node(Name, Host) ->

    list_to_atom(Name ++ "@" ++ Host).
