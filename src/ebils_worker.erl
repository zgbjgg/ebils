-module(ebils_worker).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2]).

-record(state, {chunk = <<>>}).

start_link(Id, Chunk) ->
    gen_server:start_link({local, Id}, ?MODULE, [Chunk], []).

init([Chunk]) ->
    {ok, #state{chunk = Chunk}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({{get, {found, {Pos, Len}}, XtraLen}, FromPid}, State=#state{chunk = Chunk}) ->
    % check if position is > than length of chunk, return only until eof
    MaybeLength = Pos + Len + XtraLen,
    XtraLen0 = case byte_size(Chunk)<MaybeLength of
        true ->
            Offset = MaybeLength - byte_size(Chunk),
            XtraLen - Offset;
        false ->
            XtraLen
    end,
    Binary = binary:part(Chunk, Pos, Len + XtraLen0),
    FromPid ! Binary,
    {noreply, State};
handle_cast({search, Pid, Binary}, State=#state{chunk = Chunk}) ->
    % proceed to search in our chunk of data and delivery response
    % to the `PID` (represents who performs search).
    
    % compile the binary
    Pattern = binary:compile_pattern(Binary),

    % search in chunk
    case binary:match(Chunk, Pattern) of
        nomatch -> 
            Pid ! nomatch; % propagate error, not found!
        Found   ->
            % propagate found to the pid performing the action
            Pid ! { {found, Found}, self() }
    end,
 
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_, State, _Extra) ->
    {ok, State}.

terminate(_, _State) ->
    ok.
