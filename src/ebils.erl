-module(ebils).

-export([load/2,
    load/3,
    load/4,
    search/1,
    search/2,
    unload/0,
    unload/1]).

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
    [{Name, Workers}] = ets:tab2list(Name),
    % create a spawn for the collector
    Self = self(),
    Pid = spawn(fun() -> collector(length(Workers), 1, Self) end),
    FuncSpec = {gen_server, cast},
    ExtraArgs = [{search, Pid, Pattern}],
    _Set = rpc:pmap(FuncSpec, ExtraArgs, Workers),
    % simple receive to collect results ;-)
    receive
        Result -> Result
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
        nomatch when Max > Counter   ->
            collector(Max, Counter+1, CallePid);
        nomatch                      ->
            CallePid ! {error, notfound}
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
            {Found, 1} = binary:match(Chunk0, CPattern),
            Piece = binary:part(Chunk0, {0, Found + 1}),
            NewChunk = <<Chunk/binary, Piece/binary>>,
            % resize the chunk 0
            NewChunk0 = binary:part(Chunk0, {Found + 1, Size - (Found + 1)}),
            [ NewChunk | resize([NewChunk0 | Chunks0], Pattern) ]
    end.

-spec binary_to_atom(Bin :: binary()) -> atom().
binary_to_atom(Bin) ->
    L = binary_to_list(Bin),
    list_to_atom(L).
