%%
%% @doc Handler for social login via VKontakte.
%%

-module(cowboy_social_vkontakte).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    user_profile/2
  ]).

%%
%% extract info from user profile
%%
user_profile(Auth, Opts) ->
  {ok, Profiles} = cowboy_request:get_json(
    <<"https://api.vk.com/method/users.get">>, [
      {access_token, Auth},
      {fields, key(scope, Opts, <<"uid,first_name,last_name,sex,photo">>)}
    ]),
  % NB: provider returns list of data for uids; we need only the first
  false = lists:keyfind(<<"error">>, 1, Profiles),
  [Profile] = key(<<"response">>, Profiles),
pecypc_log:info({prof, Profile}),
  {ok, [
    {id, << "vkontakte:",
      (list_to_binary(integer_to_list(key(<<"uid">>, Profile))))/binary >>},
    {provider, <<"vkontakte">>},
    % {email, key(<<"email">>, Profile)},
    {name, << (key(<<"first_name">>, Profile))/binary, " ",
              (key(<<"last_name">>, Profile))/binary >>},
    {picture, key(<<"photo">>, Profile)},
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
