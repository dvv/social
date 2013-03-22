%%
%% @doc Handler for social login via Yandex.
%%

-module(cowboy_social_yandex).
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
  << "https://oauth.yandex.ru/authorize", $?,
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
    <<"https://oauth.yandex.ru/token">>, [
      {code, Code},
      {client_id, key(client_id, Opts)},
      {client_secret, key(client_secret, Opts)},
      {redirect_uri, key(callback_uri, Opts)},
      {grant_type, <<"authorization_code">>}
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
    <<"https://login.yandex.ru/info">>, [
      {oauth_token, key(access_token, Auth)},
      {format, <<"json">>}
    ]),
  {ok, [
    {id, << "yandex:", (key(<<"id">>, Profile))/binary >>},
    {provider, <<"yandex">>},
    {email, key(<<"default_email">>, Profile)},
    {name, key(<<"real_name">>, Profile)},
    % {avatar, key(<<"picture">>, Profile)},
    {gender, case key(<<"sex">>, Profile) of
                1 -> <<"female">>; _ -> <<"male">> end}
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
