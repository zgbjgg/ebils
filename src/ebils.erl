-module(ebils).

-export([load/2,
    load/3,
    search/2]).

-define(DEFAULT_WORKERS, 100).

load(Binary, Pattern) ->
    load(Binary, Pattern, ?DEFAULT_WORKERS).

load(Binary, Pattern, Workers) ->
    % split the binary in chunks of size
    % but resize when found some delimiter in it so
    % searching in chunks will return correctly data
    Size = byte_size(Binary),
    ByteSize = Size div Workers,
    Chunks = chunks(Binary, ByteSize),
    % resize by delimiter
    ChunksResized = resize(Chunks, Pattern),
    % since chunks resized could have more items
    % than `?WORKERS` just take the length and create
    % the correct workers
    lists:map(fun(Chunk) ->
        Id = base64:encode(crypto:strong_rand_bytes(50)),
        {ok, Pid} = ebils_worker:start_link(binary_to_atom(Id), Chunk),
        Pid
    end, ChunksResized).

search(Workers, Pattern) ->
    FuncSpec = {gen_server, cast},
    ExtraArgs = [{search, self(), Pattern}],
    _Set = rpc:pmap(FuncSpec, ExtraArgs, Workers),
    % simple receive to collect results ;-)
    receive
        {{found, Found}, Pid} ->
            % Found is a {{StartPos, Len}, Worker} of the
            % match, where Worker is the pid of the gen_server
            % controlling the chunk, just make whatever you want!
            {{found, Found}, Pid}    
    after 3000 ->
        timeout
    end.

%% INTERNAL

chunks(<<>>, _)          -> [];
chunks(Binary, ByteSize) ->
    case byte_size(Binary)>ByteSize of
        true  ->
            <<Chunk:ByteSize/binary, Rest/binary>> = Binary,
            [ Chunk | chunks(Rest, ByteSize) ];
        false ->
            [ Binary | chunks(<<>>, ByteSize) ]
    end.

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

binary_to_atom(Bin) ->
    L = binary_to_list(Bin),
    list_to_atom(L).
