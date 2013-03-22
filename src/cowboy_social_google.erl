%%
%% @doc Handler for social login via Google.
%%

-module(cowboy_social_google).
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
  << "https://accounts.google.com/o/oauth2/auth", $?,
    (cowboy_request:urlencode([
      {client_id, key(client_id, Opts)},
      {redirect_uri, key(callback_uri, Opts)},
      {response_type, <<"code">>},
      {scope, << "https://www.googleapis.com/auth/userinfo.email ",
                 "https://www.googleapis.com/auth/userinfo.profile ",
                 (key(scope, Opts))/binary >>}
    ]))/binary >>.

%%
%% exchange authorization code for auth token
%%
get_access_token(Code, Opts) ->
  {ok, Auth} = cowboy_request:post_for_json(
    <<"https://accounts.google.com/o/oauth2/token">>, [
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
get_user_profile(Auth, _Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    <<"https://www.googleapis.com/oauth2/v1/userinfo">>, [
      {access_token, key(access_token, Auth)}
    ]),
  {ok, [
    {id, << "google:", (key(<<"id">>, Profile))/binary >>},
    {provider, <<"google">>},
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
