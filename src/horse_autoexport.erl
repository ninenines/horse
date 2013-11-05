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

-module(horse_autoexport).

-export([parse_transform/2]).

parse_transform([File, Module|Forms], _) ->
	Exports = [F || {attribute, _, export, [{F, 0}]} <- Forms],
	AutoExports = [{attribute, 0, export, [{F, 0}]}
		|| {function, _, F, 0, _} <- Forms,
			"horse_" =:= string:substr(atom_to_list(F), 1, 6),
			false =:= lists:member(F, Exports)],
	replace_calls([File, Module|AutoExports ++ Forms]).

replace_calls(Forms) ->
	lists:flatten([replace_call(Form) || Form <- Forms]).

replace_call(
	{function, Fu, Name, 0, [
		{clause, Cl, [], [], [
			{call, Ca, {remote, _, {atom, _, horse}, {atom, _, repeat}}, [
				Repeat,
				Expr
			]}
		]}
	]}
) when Repeat > 0 ->
	GenName = list_to_atom("generated_" ++ atom_to_list(Name)),
	[
		{function, Fu, Name, 0, [
			{clause, Cl, [], [], [
				{call, Ca, {atom, Ca, GenName}, [Repeat]}
			]}
		]},
		{function, Ca, GenName, 1, [
			{clause, Ca, [{integer, Ca, 0}], [], [
				{atom, Ca, ok}
			]},
			{clause, Ca, [{var, Ca, 'N'}], [], [
				Expr,
				{call, Ca, {atom, Ca, GenName}, [
					{op, Ca, '-', {var, Ca, 'N'}, {integer, Ca, 1}}
				]}
			]}
		]}
	];
replace_call(Form) ->
	Form.
