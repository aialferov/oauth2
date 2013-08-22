%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 16 May 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-define(AuthUri(Network), case Network of
	live -> "https://login.live.com/oauth20_authorize.srf";
	google -> "https://accounts.google.com/o/oauth2/auth";
	facebook -> "https://www.facebook.com/dialog/oauth";
	vkontakte -> "https://oauth.vk.com/authorize"
end).

-define(AccessTokenUri(Network), case Network of
	live -> "https://login.live.com/oauth20_token.srf";
	google -> "https://accounts.google.com/o/oauth2/token";
	facebook -> "https://graph.facebook.com/oauth/access_token";
	vkontakte -> "https://oauth.vk.com/access_token"
end).

-define(AuthUrlOptions(Network, OAuth, State), [
	{"client_id", OAuth#oauth2.client#oauth2_client.id},
	{"redirect_uri", OAuth#oauth2.client#oauth2_client.redirect_uri},
	{"state", State},
	{"response_type", "code"}
] ++ case Network of
	live -> [
		{"scope", "wl.messenger wl.offline_access"}
	];
	google -> [
		{"scope", "email profile https://www.googleapis.com/auth/googletalk"},
		{"approval_prompt", "force"},
		{"access_type", "offline"}
	];
	facebook -> [
		{"scope", "xmpp_login"},
		{"display", "popup"}
	];
	vkontakte -> [
		{"scope", "messages"}
	];
	_ -> []
end).

-define(AccessTokenOptions(Mode, Network, OAuth, Code), case Mode of
	request -> ?AccessTokenRequestOptions(Network, OAuth, Code);
	refresh -> ?AccessTokenRefreshOptions(Network, OAuth)
end).

-define(AccessTokenRequestOptions(_Network, OAuth, Code), [
	{"client_id", OAuth#oauth2.client#oauth2_client.id},
	{"client_secret", OAuth#oauth2.client#oauth2_client.secret},
	{"redirect_uri", OAuth#oauth2.client#oauth2_client.redirect_uri},
	{"grant_type", "authorization_code"},
	{"code", Code}
]).

-define(AccessTokenRefreshOptions(Network, OAuth), [
	{"client_id", OAuth#oauth2.client#oauth2_client.id},
	{"client_secret", OAuth#oauth2.client#oauth2_client.secret}
] ++ case Network of
	live -> [
		{"grant_type", "refresh_token"},
		{"refresh_token", OAuth#oauth2.token#oauth2_token.refresh}
	];
	google -> [
		{"grant_type", "refresh_token"},
		{"refresh_token", OAuth#oauth2.token#oauth2_token.refresh}
	];
	facebook -> [
		{"grant_type", "fb_exchange_token"},
		{"fb_exchange_token", OAuth#oauth2.token#oauth2_token.access},
		{"redirect_uri", OAuth#oauth2.client#oauth2_client.redirect_uri}
	];
	_ -> []
end).
