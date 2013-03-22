%%
%% @doc Handler for social login via Github.
%%

-module(cowboy_social_github).
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
  << "https://github.com/login/oauth/authorize", $?,
    (cowboy_request:urlencode([
      {client_id, key(client_id, Opts)},
      {redirect_uri, key(callback_uri, Opts)},
      {scope, key(scope, Opts)}
    ]))/binary >>.

%%
%% exchange authorization code for auth token
%%
get_access_token(Code, Opts) ->
  {ok, Auth} = cowboy_request:post_for_json(
    <<"https://github.com/login/oauth/access_token">>, [
      {code, Code},
      {client_id, key(client_id, Opts)},
      {client_secret, key(client_secret, Opts)},
      {redirect_uri, key(callback_uri, Opts)}
    ]),
  {ok, [
    {access_token, key(<<"access_token">>, Auth)},
    {token_type, key(<<"token_type">>, Auth)},
    {expires_in, 0}
  ]}.

%%
%% extract info from user profile
%%
get_user_profile(Auth, _Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    <<"https://api.github.com/user">>, [
      {access_token, key(access_token, Auth)}
    ]),
  {ok, [
    {id, << "github:",
      (list_to_binary(integer_to_list(key(<<"id">>, Profile))))/binary >>},
    {provider, <<"github">>},
    {email, key(<<"email">>, Profile)},
    {name, key(<<"name">>, Profile)},
    {avatar, key(<<"avatar_url">>, Profile)}
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
