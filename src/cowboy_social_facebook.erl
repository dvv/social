%%
%% @doc Handler for social login via Facebook.
%%

-module(cowboy_social_facebook).
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
  << "https://www.facebook.com/dialog/oauth", $?,
    (cowboy_request:urlencode([
      {client_id, key(client_id, Opts)},
      {redirect_uri, key(callback_uri, Opts)},
      {scope, key(scope, Opts)}
    ]))/binary >>.

%%
%% exchange authorization code for auth token
%%
get_access_token(Code, Opts) ->
  % NB: facebook uses GET
  {ok, Auth} = cowboy_request:get_json(
    <<"https://graph.facebook.com/oauth/access_token">>, [
      {code, Code},
      {client_id, key(client_id, Opts)},
      {client_secret, key(client_secret, Opts)},
      {redirect_uri, key(callback_uri, Opts)}
    ]),
  {ok, [
    {access_token, key(<<"access_token">>, Auth)},
    {token_type, <<"Bearer">>},
    {expires, key(<<"expires">>, Auth)}
  ]}.

%%
%% extract info from user profile
%%
get_user_profile(Auth, _Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    <<"https://graph.facebook.com/me">>, [
      {access_token, key(access_token, Auth)},
      {fields, <<"id,email,name,picture,gender,locale">>}
    ]),
  {ok, [
    {id, << "facebook:", (key(<<"id">>, Profile))/binary >>},
    {provider, <<"facebook">>},
    % {email, key(<<"email">>, Profile)},
    {name, key(<<"name">>, Profile)},
    {avatar, key(<<"url">>, key(<<"data">>, key(<<"picture">>, Profile)))},
    {gender, key(<<"gender">>, Profile)},
    {locale, key(<<"locale">>, Profile)}
  ]}.

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.
