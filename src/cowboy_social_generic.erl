%%
%% @doc Handler for social login via a generic OAuth2 provider.
%%

-module(cowboy_social_generic).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    user_profile/2
  ]).

%%
%% extract info from user profile
%%
user_profile(Auth, Opts) ->
  {ok, Profile} = cowboy_request:get_json(
    key(profile_url, Opts), [
      {access_token, Auth}
    ]),
  Name = key(provider_name, Opts),
  false = lists:keyfind(<<"error">>, 1, Profile),
  {ok, [
    {id, << Name/binary, ":", (key(<<"id">>, Profile))/binary >>},
    {provider, Name},
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
