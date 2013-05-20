%%%-------------------------------------------------------------------
%%% Created: 21 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, oauth2, [
	{id, "oauth2"},
	{vsn, "0.0.1"},
	{description, "Simple OAuth2 client"},
	{modules, [
		oauth2,
		oauth2_config
	]},
	{registered, []},
	{applications, [kernel, stdlib, ssl, inets, utils]}
]}.
