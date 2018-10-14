Alexander
=====

An OTP library for cutting the Gordian knot of stuck OTP processes.

One of the hardest problems to debug in Erlang/OTP is why a
gen_server/gen_statem/etc is deadlocked. Often this is due to a blocking call
cycle where A calls B who calls C who calls A. A is waiting for the return from
B when the second call comes in, so it cannot respond to it, which means all the
processes deadlock until the call timeout hits, or forever if the timeouts are
infinite.

When a process is stuck like this, you can't use sys:get_status or install a
debug trace, those use messages, which cannot be handled. All you can do is look
at erlang:process_info for the process, which often does not contain enough
information to understand why it is deadlocked.

Alexander, the Great, original cutter of the Gordian knot, is a parse transform
that instruments blocking OTP calls by writing the blocking call's information
to the process dictionary before the blocking call and then removing it
afterwards. It can then use that information to detect call cycles because other
processes can read the process dictionary via erlang:process_info. So if A calls
B, A first records it is calling B (and is thus unavailable to handle a call
until it returns) and then examines B's process dictionary to see if B is
blocked making a call of its own. If B is blocked we follow the chain of
processes until we run out of processes to follow, or we find a loop. If a loop
is detected, an exception is thrown.

How to use it
===========

Simply enable the parse transform on the modules you want to check for call
cycles:

```erlang
-compile([{parse_transform, alexander}]).
```

Or use it in your Erlang compiler flags:

```
{parse_transform, alexander}
```

Alexander can only see call cycles between processes it has transformed at
compile time, so you might want to use something like rebar3's overrides
feature:

```erlang
{overrides, [{add, [{erl_opts, [{parse_transform, alexander}]}]}]}.
```

For this to work you likely need a compiled copy of alexander in your OTP code
path somewhere. If you are using other parse transforms, Alexander will try to
run them for you before applying its own transform. This should work for most
use cases but it is not well tested.
