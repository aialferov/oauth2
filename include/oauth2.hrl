%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 16 May 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-record(oauth2, {token, client}).

-record(oauth2_token, {access, refresh}).
-record(oauth2_client, {id, secret, redirect_uri}).
