%%
%% @doc Handler for social login via Facebook.
%%

-module(cowboy_social_facebook).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    user_profile/2
  ]).

%%
%% extract info from user profile
%%
user_profile(Auth, _Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    <<"https://graph.facebook.com/me">>, [
      {access_token, Auth},
      {fields, <<"id,email,name,picture,gender,locale">>}
    ]),
  false = lists:keyfind(<<"error">>, 1, Profile),
  {ok, [
    {id, << "facebook:", (key(<<"id">>, Profile))/binary >>},
    {provider, <<"facebook">>},
    {email, key(<<"email">>, Profile)},
    {name, key(<<"name">>, Profile)},
    {picture, key(<<"url">>, key(<<"data">>, key(<<"picture">>, Profile)))},
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
