




%% @spec record_member(E::any(), R::record()) -> true | false

%% @doc <span style="color:red">TODO: Needs Example</span> Checks whether E is a member element of record R, analogous to `lists::member(E, L)'.  This function does not have examples because the shell does not correctly handle records; <span style="color:red">todo: add examples later</span>

%% @since Version 123

record_member(E, R) -> 

    tuple_member(E, R, 2, size(R)).  % just skip the 1st elem
