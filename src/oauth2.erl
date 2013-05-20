%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 13 May 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(oauth2).

-export([start/0, stop/0]).
-export([auth_url/2, auth_url/3, auth_url_result/1, auth_url_result/2]).
-export([request_access_token/3, refresh_access_token/3]).

-include("oauth2.hrl").
-include("oauth2_config.hrl").

-define(ContentType, "application/x-www-form-urlencoded").

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

auth_url(Network, OAuth2Client) -> auth_url(Network, OAuth2Client, []).
auth_url(Network, OAuth2Client, State) -> utils_http:url(?AuthUri(Network),
	?AuthUrlOptions(Network, OAuth2Client, State), encode_value).

auth_url_result(Result) -> auth_url_result(Result, []).
auth_url_result(Result, State) -> utils_monad:do_simple([
	fun() -> case utils_lists:keyfind("error", Result) of
		false -> ok; Error -> {error, Error} end end,
	fun() ->
		ReceivedState = utils_lists:keyfind("state", Result),
		case ReceivedState == State of
			true -> ok; false -> {error, {invalid_state, ReceivedState}} end
	end,
	fun() -> {ok, utils_lists:keyfind("code", Result)} end
]).

request_access_token(Network, OAuth2Client, Code) ->
	access_token(request, Network, OAuth2Client, Code).

refresh_access_token(Network, OAuth2Client, Token) ->
	access_token(refresh, Network, OAuth2Client, Token).

access_token(Mode, Network, OAuth2Client, Grant) ->
	read_access_token(Network, httpc:request(post, {
		?AccessTokenUri(Network), [], ?ContentType,
		utils_http:query_string(?AccessTokenOptions(
			Mode, Network, OAuth2Client, Grant), encode_value)
	}, [], [])).

read_access_token(Network, {ok, {{_, 200, _}, _Headers, Body}})
	when Network == live; Network == google
->
	Result = jsx:decode(list_to_binary(Body)),
	{ok, {
		binary_to_list(utils_lists:keyfind(<<"access_token">>, Result)),
		binary_to_list(utils_lists:keyfind(<<"refresh_token">>, Result))
	}};

read_access_token(facebook, {ok, {{_, 200, _}, _Headers, Body}}) ->
	Result = utils_http:read_query(Body),
	{ok, utils_lists:keyfind("access_token", Result)};

read_access_token(Network, {ok, {{_, _, _}, _Headers, Body}}) ->
	read_error(Network, jsx:decode(list_to_binary(Body))).

read_error(facebook, [{<<"error">>, Error}]) -> {error, Error};
read_error(_, Error) -> {error, Error}.
