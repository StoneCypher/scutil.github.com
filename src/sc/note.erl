




%% @spec make_notebook(Notebook::filename()) -> ok | { error, E }

%% @doc {@section Persistence} <span style="color:#888;font-style:italic">(Called automatically)</span> Creates a "notebook", which is just a convenience wrapped DETS set table.

%% @since Version 83

%% @todo complete

make_notebook(Notebook) ->

    { error, not_yet_implemented }.





%% @spec remove_notebook(Notebook::filename()) -> ok | { error, E }

%% @doc {@section Persistence} Attempts to remove a notebook; all data in the removed notebook is lost permanently.

%% @since Version 83

%% @todo complete

remove_notebook(Notebook) -> 

    { error, not_yet_implemented }.





%% @spec has_notebook(Notebook::filename()) -> true | false

%% @doc {@section Persistence} Returns a boolean true or false whether a notebook under a given filename already exists.  Does not attempt to create the notebook under any circumstances.

%% @since Version 83

%% @todo complete

has_notebook(Notebook) ->

    { error, not_yet_implemented }.




%% @equiv annote(Notebook, [{NoteName, NewValue}])

annote(Notebook, NoteName, NewValue) -> 

    annote(Notebook, [{NoteName, NewValue}] ).



%% @spec annote(Notebook::filename(), List::kv_list()) -> ok | { error, E }

%% @doc {@section Persistence} Stores a key/value pair to a notebook file, overwriting a match existing key if present; if the notebook does not exist, it is automatically created.    DETS opening and closing are automatically managed.

%% @since Version 83

annote(Notebook, NameValuePair) when is_list(Notebook), is_tuple(NameValuePair) -> 

    annote(Notebook, [NameValuePair]);



annote(Notebook, NameValuePairs) when is_list(Notebook), is_list(NameValuePairs) ->

    get_notebook_table(Notebook),
    [ dets:insert(Notebook, {Term, Value}) || {Term, Value} <- NameValuePairs ],
    close_notebook_table(Notebook).




%% @spec read_note(Notebook::filename(), NoteName::any()) -> { value, V } | { error, E }

%% @doc {@section Persistence} Read a key/value pair from a notebook file.  DETS opening and closing are automatically managed.

%% @since Version 83

read_note(Notebook, NoteName) when is_list(Notebook) ->

    get_notebook_table(Notebook),

    CurrentConfig = case dets:match(Notebook, {NoteName, '$1'}) of

        [] ->
            undefined;

        [[X]] ->
            { value, X }

    end,

    close_notebook_table(Notebook),
    CurrentConfig.






%% @spec has_note(Notebook::filename(), NoteName::any()) -> true | false | { error, E }

%% @doc {@section Persistence} Checks if a given note exists in a given notebook, returning boolean atoms as an answer.  Checking whether a note exists in a missing notebook is considered an error.  DETS opening and closing are automatically managed.

%% @since Version 83

%% @todo complete

has_note(Notebook, NoteName) ->

    { error, not_yet_implemented }.





%% @spec remove_note(Notebook::filename(), NoteName::any()) -> ok | { error, E }

%% @doc {@section Persistence} Removes a note by name from a given notebook.  Removing a nonexistant note is considered an error.

%% @since Version 83

remove_note(Notebook, NoteName) when is_atom(Notebook), is_atom(NoteName) ->

    remove_note(Notebook, [NoteName]);



remove_note(Notebook, NoteNames) when is_atom(Notebook), is_list(NoteNames) ->

    get_notebook_table(Notebook),
    [ dets:delete(Notebook, NoteName) || NoteName <- NoteNames ],
    close_notebook_table(Notebook).





%% @private

get_notebook_table(TableName) when is_list(TableName) -> 

    dets:open_file(TableName, [{type, set}] ).





%% @private

close_notebook_table(TableName) when is_list(TableName) ->

    dets:close(TableName).





%%%%%%%%%%%%%%%%%%%%
%%
%%   TODO CODE





% memory() ->
%
%    receive
%
%        terminate -> ok;
%
%        { Sender, store, Key, Val } ->
%            put(Key, Val),
%            Sender ! { memory_set_to, Key, Val },
%            memory();
%
%        { Sender, fetch, Key } ->
%            Sender ! { memory_found, get(Key) },
%            memory();
%
%    end.
%
%
%
%
% test() ->
%
%    register(mem, spawn(?MODULE, memory, [])),
%
%     mem ! { self(), store, foo, bar },
%     io:format("~p", [receive X -> X end]),
%
%     mem ! { self(), fetch, foo },
%     io:format("~p", [receive X -> X end]).





% Rename around the "annote" family

%install(ConfigName)                when is_atom(ConfigName)                                                                 -> install(ConfigName, []).
%install(ConfigName, DefaultConfig) when is_atom(ConfigName) andalso (is_list(DefaultConfig) orelse is_tuple(DefaultConfig)) ->
%
%    case is_installed(ConfigName) of
%
%        false -> configure(ConfigName, DefaultConfig), { ok, now_installed };
%        true  -> { error, already_installed, uninstall_first }
%
%    end.
%
%
%
%
%
%is_installed(ConfigName) when is_atom(ConfigName) ->
%
%    case dets:is_dets_file(sanitize_filename(ConfigName)) of
%
%        { error, _ } -> false;
%        _            -> true
%
%    end.
%
%
%
%
%
%uninstall(ConfigName) when is_atom(ConfigName) ->
%
%
%    case is_installed(ConfigName) of
%
%        false -> { error, not_installed };
%        true  ->
%
%            file:delete(sanitize_filename(ConfigName)),
%            { ok, uninstalled }
%
%    end.
%
%
%
%
%
%get_config(ConfigName) when is_atom(ConfigName) ->
%
%    get_table(ConfigName),
%    CurrentConfig = dets:match(sanitize_filename(ConfigName), '$1'),
%    close_table(ConfigName),
%    CurrentConfig.
