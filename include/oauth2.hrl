%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 16 May 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-record(oauth2, {access_token, refresh_token}).
-record(oauth2_client, {client_id, client_secret, redirect_uri}).
