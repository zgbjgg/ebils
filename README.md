# ebils
EBILS - Erlang Binary Lightweight Search

[![Hex.pm](https://img.shields.io/hexpm/v/ebils.svg)](https://hex.pm/packages/ebils)
[![Build Status](https://github.com/zgbjgg/ebils/actions/workflows/erlang.yml/badge.svg)](https://github.com/zgbjgg/ebils/actions/workflows/erlang.yml)
[![License: MIT](https://img.shields.io/github/license/zgbjgg/ebils.svg)](https://raw.githubusercontent.com/zgbjgg/ebils/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/dt/ebils.svg)](https://hex.pm/packages/ebils)
[![Hex.pm](https://img.shields.io/hexpm/dw/ebils.svg)](https://hex.pm/packages/ebils)

A single binary search for large and huge binary into pure erlang, this uses small chunk of binaries to search in them using single processes and delivering message to the main process.

With single tests of a large binary this method can be 17x faster that single `binary:match` and allows to you seek results in the process.

A single example:

```erlang
1> {ok, File} = file:read_file("test.data").
{ok,<<"AAAAAAAA|J|89\nBBBBBBBB|J|89\nCCCCCCC|J|89\nDDDDDDDD|J|89\nEEEEEEE|J|89\nFFFFFFFF|J|89\n"...>>}
2> byte_size(File).
234179947
3> ebils:load(File, <<"\n">>).
true
4> timer:tc(ebils, search, [<<"ZbZbZbZbZb">>]).
{4031,{{ok,{2341677,13},<0.161.0>}}
5> {Found, Pid} = ebils:search(<<"ZbZbZbZbZb">>).
{ok,{2341677,13},<0.160.0>}
6> ebils:fetch(Pid, {get, {found, Found}, 4}).
{ok,<<"ZbZbZbZbZb|J|89">>}
```
In the example:

* Line 1: Load a huge file
* Line 2: Check the byte size ( it's very large :-D )
* Line 3: Parse file into chunks and load into memory
* Line 4: Perform a single search, the result is the position of the match
* Line 5: Perform a search and store result in a var to use after
* Line 6: Extract a binary part of the chunk where found the match

## API

Load a binary into the system to use after for seeking matches, you can specify the name for the workers,
the length of workers created to process the binary and the pattern to split the chunks.

### ebils:load/2 ###

`ebils:load(Binary::binary(), Pattern::binary()) -> true`

### ebils:load/3 ###

`ebils:load(Name::atom(), Binary::binary(), Pattern::binary()) -> true`

### ebils:load/4 ###

`ebils:load(Name::atom(), Binary::binary(), Pattern::binary(), Workers::non_neg_integer()) -> true`

Search a single binary into the preloaded binary, you can specify the binary to seek and the name for the workers,
(default name to ebils, used with ebils:search/1)

### ebils:search/1 ###

`ebils:search(Binary::binary()) -> {ok, {non_neg_integer(), non_neg_integer()}, pid()}`

### ebils:search/2 ###

`ebils:search(Name::atom(), Binary()) -> {ok, {non_neg_integer(), non_neg_integer()}, pid()}`

Get the data from the process where the binary was found. Use a simple gen_server call using the pid where found 
the data and the tuple of Found data, also you should provide a third parameter containing the size of data to retrieve.

### ebils:fetch/2 ###

`ebils:fetch(pid(), {get, {found, {non_neg_integer(), non_neg_integer}}, integer}) -> {ok, binary()}`

Unload all data kept in processes and kill the processes, you can specify the name for the workers, this method will be helpful
to re-run the data (if binary changes)

### ebils:unload/0 ###

`ebils:unload() -> ok`

### ebils:unload/1 ###

`ebils:unload(Name::atom()) -> ok`

Refresh the chunks and the workers (processes) with new binaries in their state

### ebils:reload/2 ###

`ebils:reload(Binary :: binary() | [binary(), ...], Pattern :: binary()) -> ok`

### ebils:reload/3 ###

`ebils:reload(Name :: atom(), Binary :: binary() | [binary(), ...], Pattern :: binary()) -> ok`

### ebils:reload/4 ###

`ebils:reload(Name :: atom(), Binary :: binary() | [binary(), ...], Pattern :: binary(), Workers :: non_neg_integer()) -> ok`
