Horse
=====

Integrated performance testing.

Goals
-----

Horse is designed to provide quick feedback on the performance
of units of code, for example a function or a group of functions.

Horse works in a manner similar to the `eunit` application: it
will export automatically all the performance test functions,
run them one after another and give you a convenient report.

There are two main use cases for Horse.

You are optimizing your application and found a function or
group of functions to be too slow or using too much CPU. You
can write Horse tests, measure the time it takes to perform
that operation, and then modify your code until you get an
improvement in performance.

You are modifying a critical section of your code. You do not
want to inadvertently kill the performance of your application
if you make the wrong modification. You can write Horse tests
and ensure any modification will keep the performance at least
on par to what it was before.

Disclaimer
----------

Horse may tell you your code is faster, despite the code performing
slower when in production under heavy load. If that happens it
generally means you are using NIFs but not always. It's meant to
give you a quick indication of how your code performs and is by
no means a definitive proof of the performance of your code.

Horse may run a test slower than it should, at times. This is
because there are many factors coming in, including your hardware,
the other processes running on your machine, or even the other
processes running in the Erlang VM. Repeating tests helps get
a better view but it is by no means a perfect solution.

Use with caution.

Usage
-----

You can add the following rule to your Makefile to run Horse
on your application every time you run `make perfs`.

``` Makefile
deps/horse:
	git clone -n -- https://github.com/extend/horse $(DEPS_DIR)/horse
	cd $(DEPS_DIR)/horse ; git checkout -q master
	$(MAKE) -C $(DEPS_DIR)/horse

perfs: ERLC_OPTS += -DPERF=1 +'{parse_transform, horse_autoexport}'
perfs: clean deps deps/horse app
	$(gen_verbose) erl -noshell -pa ebin deps/horse/ebin \
		-eval 'horse:app_perf($(PROJECT)), init:stop().'
```

In your source files, you should put your test in an `ifdef` block
like this:

``` erlang
-ifdef(PERF).
%% Your tests here.
-endif.
```

All functions that begin with `horse_` will be ran as performance
tests. You can put anything inside the function, measurements will
be made for the whole call.

``` erlang
-ifdef(PERF).
horse_do_nothing() ->
    ok.
-endif.
```

When the code you need to test is very fast, you may want to
execute it many times to get a more interesting output. Horse
provides the special function `horse:repeat/2`. This function
takes an integer as first argument, which is the number of times
the expression in the second argument should be executed.

``` erlang
-ifdef(PERF).
horse_rfc2019() ->
    horse:repeat(100000,
        doit()
    ).
-endif.
```

You should repeat the test until you get a time between 0.1s
and 1s to get a better overview.

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
