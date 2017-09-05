Replicated Sets
=====

Optimized ORSET without ordering constraints


```
$ rebar3 shell
===> Verifying dependencies...
===> Compiling rset
Erlang/OTP 20 [erts-9.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [kernel-poll:false]

Eshell V9.0  (abort with ^G)
1> application:start(rset).
ok
2> replica:create([a,b,c]).
ok
3> replica:add(a, x).
{ok,{x,0,a}}
4> replica:add(a, y).
{ok,{y,1,a}}
5> replica:add(a, z).
{ok,{z,2,a}}
6>
6>
6> replica:elements(a).
{ok,[z,y,x]}
7> replica:elements(b).
{ok,[z,y,x]}
8> replica:elements(c).
{ok,[z,y,x]}
9>
9>
9> replica:delete(c, y).
{ok,#{a => [{1,1}],b => [],c => []}}
10>
10>
10> replica:elements(a).
{ok,[z,x]}
11> replica:elements(b).
{ok,[z,x]}
12> replica:elements(c).
{ok,[z,x]}
```
