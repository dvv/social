%%
%%------------------------------------------------------------------------------
%% !!! Hackish reuse of cowboy_client undocumented interface !!!
%%------------------------------------------------------------------------------
%%

-module(cowboy_request).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([get_json/2]).
-export([post_for_json/2]).
-export([request/4]).
-export([urlencode/1]).

-record(client, {
  state = wait :: wait | request | response | response_body,
  opts = [] :: [any()],
  socket = undefined :: undefined | inet:socket(),
  transport = undefined :: module(),
  timeout = 5000 :: timeout(), %% @todo Configurable.
  buffer = <<>> :: binary(),
  connection = keepalive :: keepalive | close,
  version = {1, 1} :: cowboy_http:version(),
  response_body = undefined :: undefined | non_neg_integer()
}).

request(Method, URL, Headers, Body) ->
% pecypc_log:info({req, Method, URL, Body}),
  {ok, Client0} = cowboy_client:init([]),
  % NB: have to degrade protocol to not allow chunked responses
  Client = Client0#client{version = {1, 0}},
  {ok, Client2} = cowboy_client:request(Method, URL, [
      {<<"connection">>, <<"close">>},
      {<<"accept-encoding">>, <<"identity">>},
      {<<"accept">>, <<"application/json">>},
      {<<"pragma">>, <<"no-cache">>},
      {<<"cache-control">>,
          <<"private, max-age: 0, no-cache, must-revalidate">>}
      | Headers
    ], Body, Client),
  Result = case cowboy_client:response(Client2) of
    {ok, 200, _ResHeaders, Client3} ->
      case Client3#client.state of
        % @fixme dirty hack, reports only first read chunk
        request ->
          {ok, Client3#client.buffer};
        response_body ->
          case cowboy_client:response_body(Client3) of
            {ok, ResBody, _} ->
              {ok, ResBody};
            Else ->
              Else
          end
      end;
    _Else ->
% pecypc_log:info({reqerr, _Else}),
      {error, failed}
  end,
% pecypc_log:info({res, Result}),
  Result.

urlencode(Bin) when is_binary(Bin) ->
  cowboy_http:urlencode(Bin);
urlencode(Atom) when is_atom(Atom) ->
  urlencode(atom_to_binary(Atom, latin1));
urlencode(List) when is_list(List) ->
  urlencode(List, <<>>).
urlencode([], Acc) ->
  Acc;
urlencode([{K, V} | T], <<>>) ->
  urlencode(T, << (urlencode(K))/binary, $=, (urlencode(V))/binary >>);
urlencode([{K, V} | T], Acc) ->
  urlencode(T, << Acc/binary, $&,
    (urlencode(K))/binary, $=, (urlencode(V))/binary >>).

parse({ok, JSON}) ->
  case jsx:decode(JSON, [{error_handler, fun(_, _, _) -> {error, badarg} end}])
  of
    {error, _} ->
      {ok, cowboy_http:x_www_form_urlencoded(JSON)};
    Hash ->
      {ok, Hash}
  end;
parse(_) ->
  {error, badarg}.

get_json(URL, Data) ->
  parse(request(<<"GET">>,
    << URL/binary, $?,
        (urlencode(Data))/binary >>, [], <<>>)).

post_for_json(URL, Data) ->
  parse(request(<<"POST">>, URL, [
      {<<"content-type">>, <<"application/x-www-form-urlencoded">>}
    ], urlencode(Data))).
