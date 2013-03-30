%%
%% @doc Handler for calling social providers API.
%%

-module(cowboy_social_profile).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

% -behaviour(cowboy_rest_handler).
-export([
    init/3,
    terminate/3,
    rest_init/2,
    is_authorized/2,
    content_types_provided/2
  ]).

-export([
    get_json/2
  ]).

-record(state, {
    action,
    options,
    token
  }).

init(_Transport, Req, Opts) ->
  {Action, Req2} = cowboy_req:binding(action, Req, <<"login">>),
  {upgrade, protocol, cowboy_rest, Req2, #state{
      action = Action,
      options = Opts
    }}.

terminate(_Reason, _Req, _State) ->
  ok.

rest_init(Req, State) ->
  {ok, Req, State}.

%%
%% `Authorization: Bearer TOKEN` or `?access_token=TOKEN` required
%%
is_authorized(Req, State) ->
  case cowboy_req:header(<<"authorization">>, Req) of
    {<< "Bearer ", Bearer/binary >>, Req2} ->
      {true, Req2, State#state{token = Bearer}};
    {undefined, Req2} ->
      case cowboy_req:qs_val(<<"access_token">>, Req2) of
        {undefined, Req3} ->
          {{false, <<"Bearer">>}, Req3, State};
        {Token, Req3} ->
          {true, Req3, State#state{token = Token}}
      end;
    {_, Req2} ->
      {{false, <<"Bearer">>}, Req2, State}
  end.

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, get_json}
  ], Req, State}.

get_json(Req, State = #state{action = Action, options = Opts, token = Token}) ->
  % @fixme atoms are not purged!
  {_, Provider} = lists:keyfind(provider, 1, Opts),
  Mod = binary_to_atom(<<
        "cowboy_social_", (atom_to_binary(Provider, latin1))/binary
      >>, latin1),
  Fun = binary_to_atom(Action, latin1),
  case Mod:Fun(Token, Opts) of
    {ok, Result} ->
      {jsx:encode(Result), Req, State};
    {error, Error} ->
      {jsx:encode([{error, Error}]), Req, State}
  end.
