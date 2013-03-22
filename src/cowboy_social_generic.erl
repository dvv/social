%%
%% @doc Handler for social login via a generic OAuth2 provider.
%%

-module(cowboy_social_generic).
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
  << (key(authorize_url, Opts))/binary, $?,
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
    key(access_token_url, Opts), [
      {code, Code},
      {client_id, key(client_id, Opts)},
      {client_secret, key(client_secret, Opts)},
      {redirect_uri, key(callback_uri, Opts)},
      {grant_type, <<"authorization_code">>}
    ]),
  {ok, [
    {access_token, key(<<"access_token">>, Auth)},
    {token_type, key(<<"token_type">>, Auth)},
    {expires_in, key(<<"expires_in">>, Auth)}
  ]}.

%%
%% extract info from user profile
%%
get_user_profile(Auth, Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    key(profile_url, Opts), [
      {access_token, key(access_token, Auth)}
    ]),
  Name = key(provider_name, Opts),
  {ok, [
    {id, << Name/binary, ":", (key(<<"id">>, Profile))/binary >>},
    {provider, Name},
    {email, key(<<"email">>, Profile)},
    {name, key(<<"name">>, Profile)},
    {avatar, key(<<"picture">>, Profile)},
    {gender, key(<<"gender">>, Profile)},
    {locale, key(<<"locale">>, Profile)}
  ]}.

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  key(Key, List, <<>>).

key(Key, List, Def) ->
  case lists:keyfind(Key, 1, List) of
    {_, Value} -> Value;
    _ -> Def
  end.
