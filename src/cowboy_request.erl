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
-export([make_uri/3]).

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
      % {<<"accept">>, <<"application/json, text/html">>},
      {<<"accept">>, <<"application/json">>},
      {<<"content-type">>, <<"application/x-www-form-urlencoded">>},
      {<<"pragma">>, <<"no-cache">>},
      {<<"cache-control">>,
          <<"private, max-age: 0, no-cache, must-revalidate">>}
      | Headers
    ], Body, Client),
  Result = case cowboy_client:response(Client2) of
    {ok, Status, _ResHeaders, Client3} ->
      case Client3#client.state of
        % @fixme dirty hack, reports only first read chunk
        request ->
% pecypc_log:info({buffer, _Status, Client3}),
          {ok, Status, Client3#client.buffer};
        response_body ->
          case cowboy_client:response_body(Client3) of
            {ok, ResBody, _} ->
% pecypc_log:info({body, _Status, ResBody}),
              % @todo analyze Status
              {ok, Status, ResBody};
            Else ->
              Else
          end
      end;
    _Else ->
% pecypc_log:info({reqerr, _Else}),
      {error, <<"server_error">>}
  end,
% pecypc_log:info({res, Result}),
  Result.

make_uri(Scheme, Host, Path) ->
  << Scheme/binary, "://", Host/binary, Path/binary >>.

urlencode(Bin) when is_binary(Bin) ->
  cowboy_http:urlencode(Bin);
urlencode(Atom) when is_atom(Atom) ->
  urlencode(atom_to_binary(Atom, latin1));
urlencode(Int) when is_integer(Int) ->
  urlencode(list_to_binary(integer_to_list(Int)));
urlencode({K, undefined}) ->
  << (urlencode(K))/binary, $= >>;
urlencode({K, V}) ->
  << (urlencode(K))/binary, $=, (urlencode(V))/binary >>;
urlencode(List) when is_list(List) ->
  binary_join([urlencode(X) || X <- List], << $& >>).

binary_join([], _Sep) ->
  <<>>;
binary_join([H], _Sep) ->
  << H/binary >>;
binary_join([H | T], Sep) ->
  << H/binary, Sep/binary, (binary_join(T, Sep))/binary >>.

parse(JSON) ->
  case jsx:decode(JSON, [{error_handler, fun(_, _, _) -> {error, badarg} end}])
  of
    {error, _} ->
      {ok, cowboy_http:x_www_form_urlencoded(JSON)};
    {incomplete, _} ->
      {ok, []};
    Hash ->
      {ok, Hash}
  end.

get_json(URL, Data) ->
  case request(<<"GET">>, <<
        URL/binary, $?, (urlencode(Data))/binary >>, [], <<>>)
  of
    {ok, 200, JSON} -> parse(JSON);
    _ -> {error, badarg}
  end.

post_for_json(URL, Data) ->
  case request(<<"POST">>, URL, [
      {<<"content-type">>, <<"application/x-www-form-urlencoded">>}
    ], urlencode(Data))
  of
    {ok, 200, JSON} -> parse(JSON);
    _ -> {error, badarg}
  end.
