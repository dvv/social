%%
%% @doc Handler for social login via Loginza.Ru OpenID provider.
%%

-module(cowboy_social_loginza).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

% -behaviour(cowboy_rest_handler).
-export([
    init/3,
    terminate/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    put_resource/2
  ]).

init(_Transport, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

terminate(_Reason, _Req, _State) ->
  ok.

rest_init(Req, State) ->
  {ok, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"x-www-form-urlencoded">>, []}, put_resource}
  ], Req, State}.

put_resource(Req, State) ->
  case cowboy_req:body_qs(Req) of
    {ok, Body, Req2} when is_list(Body) ->
% pecypc_log:info({data, Body}),
      case lists:keyfind(<<"token">>, 1, Body) of
        false ->
          {halt, respond(500, <<>>, Req), State};
        {_, Token} ->
          verify_token(Token, Req2, State)
      end;
    _ ->
      {halt, respond(500, <<>>, Req), State}
  end.

verify_token(Token, Req, State) ->
  try cowboy_request:get_json(<<"http://loginza.ru/api/authinfo">>, [
      {id, key(id, State)},
      {token, Token},
      {sig, md5hex(<< Token/binary, (key(secret, State))/binary >>)}
    ])
  of
    {ok, Response} ->
% pecypc_log:info({res, Response}),
      case lists:keyfind(<<"error_type">>, 1, Response) of
        false ->
% pecypc_log:info({auth, Response}),
          Req2 = cowboy_req:set_resp_header(
              <<"content-type">>, <<"application/json; charset=UTF-8">>, Req),
          {halt, respond(200, jsx:encode(user_profile(Response)), Req2), State};
        _ ->
          {halt, respond(500, key(<<"error_message">>, Response), Req), State}
      end;
    _Else ->
      {halt, respond(500, <<>>, Req), State}
  catch _:_ ->
    {halt, respond(500, <<>>, Req), State}
  end.

user_profile(Profile) ->
  % {_, Provider} = lists:keyfind(<<"provider">>, 1, Profile),
  {_, Name} = lists:keyfind(<<"name">>, 1, Profile),
  [
    {id, key(<<"identity">>, Profile)},
    % {id, << Provider/binary, ":", (key(<<"uid">>, Profile))/binary >>},
    % {provider, Provider},
    {email, key(<<"email">>, Profile, null)},
    {name, << (key(<<"first_name">>, Name))/binary, " ",
              (key(<<"last_name">>, Name))/binary >>},
    {picture, key(<<"photo">>, Profile)},
    {gender, case key(<<"gender">>, Profile) of
                "F" -> <<"female">>; _ -> <<"male">> end},
    {raw, Profile}
  ].

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

respond(Status, Body, Req) ->
  {ok, Req2} = cowboy_req:reply(Status, cowboy_req:set_resp_body(Body, Req)),
  Req2.

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.

key(Key, List, Def) ->
  case lists:keyfind(Key, 1, List) of
    {_, Value} -> Value;
    _ -> Def
  end.

md5hex(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0b",[N]) ||
        N <- binary_to_list(erlang:md5(binary_to_list(Bin)))])).
