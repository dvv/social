%%
%% @doc Handler for social login via PayPal.
%%

-module(cowboy_social_paypal).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    user_profile/2
  ]).

%%
%% extract info from user profile
%%
user_profile(Auth, _Opts) ->
  {ok, Result} = cowboy_request:get_json(
    <<"https://identity.x.com/xidentity/resources/profile/me">>, [
      {oauth_token, Auth}
    ]),
  % NB: provider returns {status: ..., identity: Profile}
  Profile = key(<<"identity">>, Result),
  {ok, [
    {id, << "paypal:", (key(<<"userId">>, Profile))/binary >>},
    {provider, <<"paypal">>},
    {email, hd(key(<<"emails">>, Profile))},
    {name, key(<<"fullName">>, Profile)},
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
