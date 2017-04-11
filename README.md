# ebils
EBILS - Erlang Binary Lightweight Search

A single binary search for large and huge binary into pure erlang, this uses small chunk of binaries to search in them using single processes and delivering message to the main process.

With single tests of a large binary this method can be 17x faster that single `binary:match` and allows to you seek results in the process.

A single example:

```erlang
1> {ok, File} = file:read_file("test.data").
{ok,<<"AAAAAAAA|J|89\nBBBBBBBB|J|89\nCCCCCCC|J|89\nDDDDDDDD|J|89\nEEEEEEE|J|89\nFFFFFFFF|J|89\n"...>>}
2> byte_size(File).
234179947
3> W = ebils:load(File, <<"\n">>).
[<0.62.0>,<0.63.0>,<0.64.0>,<0.65.0>,<0.66.0>,<0.67.0>,
 <0.68.0>,<0.69.0>,<0.70.0>,<0.71.0>,<0.72.0>,<0.73.0>,
 <0.74.0>,<0.75.0>,<0.76.0>,<0.77.0>,<0.78.0>,<0.79.0>,
 <0.80.0>,<0.81.0>,<0.82.0>,<0.83.0>,<0.84.0>,<0.85.0>,
 <0.86.0>,<0.87.0>,<0.88.0>,<0.89.0>,<0.90.0>|...]
4> timer:tc(ebils, search, [W, <<"ZbZbZbZbZb">>]).
{4031,{{found,{2341677,13}},<0.161.0>}}
5> {Found, Pid} = ebils:search(W, <<"ZbZbZbZbZb">>).
{{found,{2341677,13}},<0.160.0>}
6> gen_server:call(Pid, {get, Found, 4}).
{ok,<<"CUAO960306KE8|0|0">>}
```
In the example:

* Line 1: Load a huge file
* Line 2: Check the byte size ( it's very large :-D )
* Line 3: Parse file into chunks and load into memory
* Line 4: Perform a single search, the result is the position of the match
* Line 5: Perform a search and store result in a var to use after
* Line 6: Extract a binary part of the chunk where found the match

