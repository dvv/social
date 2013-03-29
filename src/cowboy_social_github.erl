%%
%% @doc Handler for social login via Github.
%%

-module(cowboy_social_github).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    user_profile/2
  ]).

%%
%% extract info from user profile
%%
user_profile(Auth, _Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    <<"https://api.github.com/user">>, [
      {access_token, Auth}
    ]),
  false = lists:keyfind(<<"message">>, 1, Profile),
  {ok, [
    {id, << "github:",
      (list_to_binary(integer_to_list(key(<<"id">>, Profile))))/binary >>},
    {provider, <<"github">>},
    {email, key(<<"email">>, Profile)},
    {name, key(<<"name">>, Profile)},
    {picture, key(<<"avatar_url">>, Profile)},
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
