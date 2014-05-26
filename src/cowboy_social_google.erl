%%
%% @doc Handler for social login via Google.
%%

-module(cowboy_social_google).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    authorize/1,
    access_token/2,
    user_profile/2
  ]).

%%
%% get URL of provider authorization page
%%
authorize(Opts)  ->
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
access_token(Code, Opts) ->
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
user_profile(Token, _Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    <<"https://www.googleapis.com/oauth2/v1/userinfo">>, [
      {access_token, Token}
    ]),
  false = lists:keyfind(<<"error">>, 1, Profile),
  {ok, [
    {id, << "google:", (key(<<"id">>, Profile))/binary >>},
    {provider, <<"google">>},
    {email, key(<<"email">>, Profile)},
    {name, key(<<"name">>, Profile)},
    {picture, key(<<"picture">>, Profile)},
    {gender, key(<<"gender">>, Profile)},
    {locale, key(<<"locale">>, Profile)},
    {raw, Profile}
  ]}.

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.
