%%
%% @doc Handler for social login via PayPal.
%%

-module(cowboy_social_paypal).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    get_authorize_url/1,
    get_access_token/2,
    get_user_profile/2
  ]).

%%
%%------------------------------------------------------------------------------
%% OAUTH2 Application flow
%%------------------------------------------------------------------------------
%%

%%
%% get URL of provider authorization page
%%
get_authorize_url(Opts)  ->
  << "https://identity.x.com/xidentity/resources/authorize", $?,
    (cowboy_request:urlencode([
      {client_id, key(client_id, Opts)},
      {redirect_uri, key(callback_uri, Opts)},
      {response_type, <<"code">>},
      {scope, key(scope, Opts)}
    ]))/binary >>.

%%
%% exchange authorization code for auth token
%%
get_access_token(Code, Opts) ->
  {ok, Auth} = cowboy_request:post_for_json(
    <<"https://identity.x.com/xidentity/oauthtokenservice">>, [
      {code, Code},
      {client_id, key(client_id, Opts)},
      {client_secret, key(client_secret, Opts)},
      {redirect_uri, key(callback_uri, Opts)},
      {grant_type, <<"authorization_code">>}
    ]),
  {ok, [
    {access_token, key(<<"access_token">>, Auth)},
    {token_type, <<"Bearer">>},
    {expires_in, key(<<"expires_in">>, Auth)}
  ]}.

%%
%% extract info from user profile
%%
get_user_profile(Auth, _Opts) ->
  {ok, Result} = cowboy_request:get_json(
    <<"https://identity.x.com/xidentity/resources/profile/me">>, [
      {oauth_token, key(access_token, Auth)}
    ]),
  % NB: provider returns {status: ..., identity: Profile}
  Profile = key(<<"identity">>, Result),
  {ok, [
    {id, << "paypal:", (key(<<"userId">>, Profile))/binary >>},
    {provider, <<"paypal">>},
    {email, hd(key(<<"emails">>, Profile))},
    {name, key(<<"fullName">>, Profile)}
  ]}.

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.
