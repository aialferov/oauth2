%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 13 May 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(oauth2).

-export([start/0, stop/0]).

-export([auth_url/2, auth_url/3, auth_result/1, auth_state/1]).
-export([request_token/3, refresh_token/2]).

-include("oauth2.hrl").
-include("oauth2_config.hrl").

-define(ContentType, "application/x-www-form-urlencoded").

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

auth_url(Network, OAuth) -> auth_url(Network, OAuth, []).
auth_url(Network, OAuth, State) -> utils_http:url(?AuthUri(Network),
	?AuthUrlOptions(Network, OAuth, State), encode_value).

auth_state(Result) -> utils_lists:keyfind("state", Result).

auth_result(Result) -> auth_result(Result,
	utils_lists:keyfind2("error", Result)).
auth_result(Result, false) -> utils_lists:keyfind("code", Result);
auth_result(_Result, Error) -> {error, Error}.

request_token(Network, OAuth, Code) ->
	set_token(token(request, Network, OAuth, Code), OAuth).

refresh_token(Network, OAuth) -> set_token(restore_refresh_token(
	OAuth, token(refresh, Network, OAuth, [])), OAuth).

token(Mode, Network, OAuth, Code) -> read_token(Network, httpc:request(post, {
	?AccessTokenUri(Network), [], ?ContentType, utils_http:query_string(
		?AccessTokenOptions(Mode, Network, OAuth, Code), encode_value)
}, [], [])).

read_token(Network, {ok, {{_, 200, _}, _Headers, Body}})
	when Network == live; Network == google
->
	{ok, lists:foldl(fun
		({<<"access_token">>, V}, Acc) -> Acc#oauth2_token{access = btl(V)};
		({<<"refresh_token">>, V}, Acc) -> Acc#oauth2_token{refresh = btl(V)};
		(_, Acc) -> Acc
	end, #oauth2_token{}, jsx:decode(ltb(Body)))};

read_token(facebook, {ok, {{_, 200, _}, _Headers, Body}}) ->
	{ok, #oauth2_token{access = utils_lists:keyfind2(
		"access_token", utils_http:read_query(Body))}};

read_token(Network, {ok, {{_, _, _}, _Headers, Body}}) ->
	read_error(Network, jsx:decode(ltb(Body))).

read_error(facebook, [{<<"error">>, Error}]) -> {error, Error};
read_error(_, Error) -> {error, Error}.

restore_refresh_token(OAuth, {ok, T = #oauth2_token{refresh = undefined}}) ->
	{ok, T#oauth2_token{refresh = OAuth#oauth2.token#oauth2_token.refresh}};
restore_refresh_token(_OAuth, Other) -> Other.

set_token({ok, Token}, OAuth) -> {ok, OAuth#oauth2{token = Token}};
set_token(Error, _OAuth) -> Error.

btl(Binary) -> binary_to_list(Binary).
ltb(List) -> list_to_binary(List).
