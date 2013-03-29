%%
%% @doc Handler for social login via Mail.Ru.
%%

-module(cowboy_social_mailru).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    user_profile/2
  ]).

%%
%% extract info from user profile
%%
user_profile(Auth, Opts) ->
  Sig = md5hex(<<
      "app_id=", (key(client_id, Opts))/binary,
      "method=users.getInfosecure=1session_key=", Auth/binary,
      (key(secret_key, Opts))/binary >>),
  % NB: provider returns list of data for uids; we need only the first
  {ok, [Profile]} = cowboy_request:get_json(
    <<"http://www.appsmail.ru/platform/api">>, [
      {app_id, key(client_id, Opts)},
      {method, <<"users.getInfo">>},
      {secure, <<"1">>},
      {session_key, Auth},
      {sig, Sig}
    ]),
  % NB: {<<"error">>, _} means error occured
  false = is_tuple(Profile),
  {ok, [
    {id, << "mailru:", (key(<<"uid">>, Profile))/binary >>},
    {provider, <<"mailru">>},
    {email, key(<<"email">>, Profile)},
    {name, << (key(<<"first_name">>, Profile))/binary, " ",
              (key(<<"last_name">>, Profile))/binary >>},
    {picture, key(<<"pic">>, Profile)},
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

md5hex(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0b",[N]) ||
        N <- binary_to_list(erlang:md5(binary_to_list(Bin)))])).
