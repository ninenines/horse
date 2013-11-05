%% Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(horse).

-export([app_perf/1]).
-export([mod_perf/1]).

%% These might be interesting later on.
%% @todo horse_init, horse_end
%% @todo horse_init_per_test, horse_end_per_test

app_perf(App) when is_atom(App) ->
	io:format("Running horse on application ~s~n", [App]),
	ok = application:load(App),
	{ok, Modules} = application:get_key(App, modules),
	_ = [mod_perf(M) || M <- lists:sort(Modules)],
	ok.

mod_perf(Mod) when is_atom(Mod) ->
	Perfs = [F || {F, 0} <- Mod:module_info(exports),
		"horse_" =:= string:substr(atom_to_list(F), 1, 6)],
	_ = [fun_perf(Mod, Fun) || Fun <- Perfs],
	ok.

fun_perf(Mod, Fun) when is_atom(Mod), is_atom(Fun) ->
	%% Dry run.
	_ = Mod:Fun(),
	%% Proper run.
	Before = os:timestamp(),
	_Val = Mod:Fun(),
	After = os:timestamp(),
	%% Results.
	Time = timer:now_diff(After, Before),
	"horse_" ++ Name = atom_to_list(Fun),
	io:format("~s:~s in ~b.~6.10.0bs~n",
		[Mod, Name, Time div 1000000, Time rem 1000000]),
	ok.
