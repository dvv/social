%%
%% @doc Handler for social login via this app provider.
%%

-module(cowboy_social_native).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    user_profile/2
  ]).

%%
%% extract info from user profile
%%
user_profile(_Auth, _Opts) ->
  {ok, [
    {id, << "native:nyi" >>},
    {provider, <<"native">>},
    {email, <<"nyi@nyi.nyi">>},
    {name, <<"nyi">>},
    {picture, null},
    {gender, <<"male">>},
    {locale, <<"ru">>}
  ]}.

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

% key(Key, List) ->
%   {_, Value} = lists:keyfind(Key, 1, List),
%   Value.
