%%
%% @doc Handler for social login via Yandex.
%%

-module(cowboy_social_yandex).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    user_profile/2
  ]).

%%
%% extract info from user profile
%%
user_profile(Auth, _Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    <<"https://login.yandex.ru/info">>, [
      {oauth_token, Auth},
      {format, <<"json">>}
    ]),
  true = Profile =/= [],
  {ok, [
    {id, << "yandex:", (key(<<"id">>, Profile))/binary >>},
    {provider, <<"yandex">>},
    {email, key(<<"default_email">>, Profile)},
    {name, key(<<"display_name">>, Profile)},
    {picture, key(<<"picture">>, Profile, null)},
    {gender, case key(<<"sex">>, Profile) of
                1 -> <<"female">>; _ -> <<"male">> end},
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

key(Key, List, Def) ->
  case lists:keyfind(Key, 1, List) of
    {_, Value} -> Value;
    _ -> Def
  end.
