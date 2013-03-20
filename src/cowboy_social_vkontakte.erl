%%
%% @doc Handler for social login via VKontakte.
%%

-module(cowboy_social_vkontakte).
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
  << "https://oauth.vk.com/authorize", $?,
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
    <<"https://oauth.vk.com/access_token">>, [
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
get_user_profile(Auth, Opts) ->
  {ok, Profiles} = cowboy_request:get_json(
    <<"https://api.vk.com/method/users.get">>, [
      {access_token, key(access_token, Auth)},
      {fields, key(scope, Opts)}
    ]),
  % NB: provider returns list of data for uids; we need only the first
  [Profile] = key(<<"response">>, Profiles),
  {ok, [
    {id, << "vkontakte:",
      (list_to_binary(integer_to_list(key(<<"uid">>, Profile))))/binary >>},
    {provider, <<"vkontakte">>},
    % {email, key(<<"email">>, Profile)},
    {name, << (key(<<"first_name">>, Profile))/binary, " ",
              (key(<<"last_name">>, Profile))/binary >>},
    {avatar, key(<<"photo">>, Profile)},
    {gender, case key(<<"sex">>, Profile) of
                1 -> <<"female">>; _ -> <<"male">> end}
  ]}.

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.
