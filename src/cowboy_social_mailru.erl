%%
%% @doc Handler for social login via Mail.Ru.
%%

-module(cowboy_social_mailru).
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
  << "https://connect.mail.ru/oauth/authorize", $?,
    (cowboy_request:urlencode([
      {client_id, key(client_id, Opts)},
      {redirect_uri, key(callback_uri, Opts)},
      {response_type, <<"code">>}
    ]))/binary >>.

%%
%% exchange authorization code for auth token
%%
get_access_token(Code, Opts) ->
  {ok, Auth} = cowboy_request:post_for_json(
    <<"https://connect.mail.ru/oauth/token">>, [
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
  Sig = md5hex(<<
      "app_id=", (key(client_id, Opts))/binary,
      "method=users.getInfosecure=1session_key=",
      (key(access_token, Auth))/binary,
      (key(secret_key, Opts))/binary >>),
  % NB: provider returns list of data for uids; we need only the first
  {ok, [Profile]} = cowboy_request:get_json(
    <<"http://www.appsmail.ru/platform/api">>, [
      {app_id, key(client_id, Opts)},
      {method, <<"users.getInfo">>},
      {secure, <<"1">>},
      {session_key, key(access_token, Auth)},
      {sig, Sig}
    ]),
  {ok, [
    {id, << "mailru:", (key(<<"uid">>, Profile))/binary >>},
    {provider, <<"mailru">>},
    {email, key(<<"email">>, Profile)},
    {name, << (key(<<"first_name">>, Profile))/binary, " ",
              (key(<<"last_name">>, Profile))/binary >>},
    {avatar, key(<<"pic">>, Profile)},
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

md5hex(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0b",[N]) ||
        N <- binary_to_list(erlang:md5(binary_to_list(Bin)))])).
