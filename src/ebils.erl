-module(ebils).

-export([load/2,
    load/3,
    load/4,
    search/1,
    search/2,
    unload/0,
    unload/1,
    reload/2,
    reload/3,
    reload/4,
    fetch/2]).

-define(DEFAULT_WORKERS, 100).
-define(DEFAULT_NAME, ebils).

-type found() :: {ok, {non_neg_integer(), non_neg_integer()}, pid()}.
-type notfound() :: {error, notfound}. 

-spec load(Binary :: binary(), Pattern :: binary()) -> true.
load(Binary, Pattern) ->
    load(?DEFAULT_NAME, Binary, Pattern, ?DEFAULT_WORKERS).

-spec load(Name :: atom(), Binary :: binary(), Pattern :: binary()) -> true.
load(Name, Binary, Pattern) ->
    load(Name, Binary, Pattern, ?DEFAULT_WORKERS).

-spec load(Name :: atom(), Binary :: binary(), Pattern :: binary(),
    Workers :: non_neg_integer()) -> true.
load(Name, Binary, Pattern, Workers) ->
    % split the binary in chunks of size
    % but resize when found some delimiter in it so
    % searching in chunks will return correctly data
    Size = byte_size(Binary),
    ByteSize = Size div Workers,
    Chunks = chunks(Binary, ByteSize),
    % resize by delimiter
    ChunksResized = resize(Chunks, Pattern),
    % since chunks resized could have more items
    % than `WORKERS` just take the length and create
    % the correct workers
    W = lists:map(fun(Chunk) ->
        Id = base64:encode(crypto:strong_rand_bytes(50)),
        {ok, Pid} = ebils_worker:start_link(binary_to_atom(Id), Chunk),
        Pid
    end, ChunksResized),
    % store the workers into a public ets so can be added
    % more process to the actual one
    try
        [{Name, Store}] = ets:tab2list(Name), 
        ets:insert(Name, {Name, Store ++ W})
    catch
        _:_ ->
            ets:new(Name, [public, named_table]),
            ets:insert(Name, {Name, W})
    end.

-spec reload(Binary :: binary() | list(), Pattern :: binary()) -> ok.
reload(Binary, Pattern) when is_binary(Binary) ->
    reload(?DEFAULT_NAME, Binary, Pattern, ?DEFAULT_WORKERS);
reload(Binaries, Pattern) when is_list(Binaries) ->
    Binary = binary:list_to_bin(Binaries),
    reload(?DEFAULT_NAME, Binary, Pattern, ?DEFAULT_WORKERS).

-spec reload(Name :: atom(), Binary :: binary() | list(), Pattern :: binary()) -> ok.
reload(Name, Binary, Pattern) when is_binary(Binary) ->
    reload(Name, Binary, Pattern, ?DEFAULT_WORKERS);
reload(Name, Binaries, Pattern) when is_list(Binaries) ->
    Binary = binary:list_to_bin(Binaries),
    reload(Name, Binary, Pattern, ?DEFAULT_WORKERS).

-spec reload(Name :: atom(), Binary :: binary() | list(), Pattern :: binary(),
    Workers :: non_neg_integer()) -> ok.
reload(Name, Binaries, Pattern, Workers) when is_list(Binaries) ->
    Binary = binary:list_to_bin(Binaries),
    reload(Name, Binary, Pattern, Workers);
reload(Name, Binary, Pattern, Workers) when is_binary(Binary) ->
    % split the binary in chunks of size
    % but resize when found some delimiter in it so
    % searching in chunks will return correctly data
    Size = byte_size(Binary),
    ByteSize = Size div Workers,
    Chunks = chunks(Binary, ByteSize),
    % resize by delimiter
    ChunksResized = resize(Chunks, Pattern),
    % since chunks resized could have more items
    % than `WORKERS` just take the length and create
    % the correct workers
    W = lists:map(fun(Chunk) ->
        Id = base64:encode(crypto:strong_rand_bytes(50)),
        {ok, Pid} = ebils_worker:start_link(binary_to_atom(Id), Chunk),
        Pid
    end, ChunksResized),
    % get the old workers to remove after adding new processes
    [{_NameOld, StoreOld}] = ets:tab2list(Name),
    % store the workers into a public ets so can be added
    % more process to the actual one
    try
        [{Name, _Store}] = ets:tab2list(Name),
        ets:insert(Name, {Name, W})
    catch
        _:_ ->
            ets:new(Name, [public, named_table]),
            ets:insert(Name, {Name, W})
    end,
    lists:foreach(fun(Pid) ->
        gen_server:stop(Pid)
    end, StoreOld).

-spec unload() -> ok.
unload() ->
    unload(?DEFAULT_NAME).

-spec unload(Name :: atom()) -> ok.
unload(Name) ->
    [{Name, Store}] = ets:tab2list(Name),
    true = ets:delete(Name),
    lists:foreach(fun(Pid) ->
        gen_server:stop(Pid)
    end, Store).

-spec search(Pattern :: binary()) -> notfound() | found(). 
search(Pattern) ->
    search(?DEFAULT_NAME, Pattern).

-spec search(Name :: atom(), Pattern :: binary()) -> notfound() | found().
search(Name, Pattern) ->
    Self = self(),
    [{Name, Workers}] = ets:tab2list(Name),
    % create a spawn for the collector
    Pid = spawn(fun() -> collector(length(Workers), 1, Self) end),
    FuncSpec = {gen_server, cast},
    ExtraArgs = [{search, Pid, Pattern}],
    PidR = spawn(fun() -> _Rpc = rpc:pmap(FuncSpec, ExtraArgs, Workers) end),
    case ebils_pipe:receive_result(1000) of
        {ok, Found, Worker} ->
            exit(Pid, normal),
            exit(PidR, brutal_kill),
            {ok, Found, Worker};
        timeout             ->
            {error, notfound}
    end.

fetch(Pid, Found) ->
    ok = gen_server:cast(Pid, {Found, self()}),
    receive
        Binary -> {ok, Binary}
    after 2000 ->
        {error, crashing}
    end.

%% INTERNAL

-spec collector(Max :: non_neg_integer(), Counter :: non_neg_integer(),
    CallePid :: pid()) -> notfound() | found(). 
collector(Max, Counter, CallePid) ->
    receive
        {{found, Found}, Pid}        ->
            % Found is a {StartPos, Len} of the
            % match, where Pid is the worker (gen_server)
            % controlling the chunk, just make whatever you want!
            CallePid ! {ok, Found, Pid};
        nomatch                      ->
            collector(Max, Counter, CallePid)
    end.

-spec chunks(Binary :: binary(), ByteSize :: non_neg_integer()) -> [ binary() ].
chunks(<<>>, _)          -> [];
chunks(Binary, ByteSize) ->
    case byte_size(Binary)>ByteSize of
        true  ->
            <<Chunk:ByteSize/binary, Rest/binary>> = Binary,
            [ Chunk | chunks(Rest, ByteSize) ];
        false ->
            [ Binary | chunks(<<>>, ByteSize) ]
    end.

-spec resize(Chunks :: [ binary() ], Pattern :: binary()) -> [ binary() ].
resize([], _Pattern)              -> [];
resize([Chunk | [] ], _Pattern)   -> [Chunk];
resize([Chunk | Chunks], Pattern) ->
    case binary:last(Chunk) of
        [Pattern] ->
            [ Chunk | resize(Chunks, Pattern) ];
        _         ->
            % search the missing piece in the next position of the
            % chunks, so the next chunk must have it!
            [Chunk0 | Chunks0] = Chunks,
            % compile pattern & size of chunk
            CPattern = binary:compile_pattern(Pattern),
            Size = byte_size(Chunk0),
            % match pattern
            case binary:match(Chunk0, CPattern) of
                {Found, 1} ->
                    Piece = binary:part(Chunk0, {0, Found + 1}),
                    NewChunk = <<Chunk/binary, Piece/binary>>,
                    % resize the chunk 0
                    NewChunk0 = binary:part(Chunk0, {Found + 1, Size - (Found + 1)}),
                    [ NewChunk | resize([NewChunk0 | Chunks0], Pattern) ];
               nomatch     ->
                 % what happens if next chunk cannot complete previous one, just concat
                 % the actual chunk with the new one. BE CAREFUL BECAUSE IF YOUR PROVIDED
                 % BINARY HAS THIS BEHAVIOUR MAYBE THE SEARCHES WILL BE INCONSISTENCE!
                 NewChunk = <<Chunk/binary, Chunk0/binary>>,
                 [ NewChunk | resize(Chunks0, Pattern) ]
           end
    end.

-spec binary_to_atom(Bin :: binary()) -> atom().
binary_to_atom(Bin) ->
    L = binary_to_list(Bin),
    list_to_atom(L).
