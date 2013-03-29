%%% ----------------------------------------------------------------------------
%%%
%%% @doc Skeleton for a OAuth2 provider.
%%%
%%% ----------------------------------------------------------------------------

-module(cowboy_social_provider).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

% -behaviour(cowboy_rest_handler).
-export([
    init/3,
    terminate/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    post_is_create/2
  ]).

% getters
-export([
    get_html/2
  ]).

% setters
-export([
    put_form/2,
    put_json/2
  ]).

-record(state, {
    options,
    data,
    client_id,
    client_secret,
    redirect_uri,
    scope,
    opaque,
    response_type
  }).

init(_Transport, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, #state{options = Opts}}.

terminate(_Reason, _Req, _State) ->
  ok.

rest_init(Req, State) ->
  {ok, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, put_json},
    {{<<"application">>, <<"x-www-form-urlencoded">>, []}, put_form}
  ], Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"text">>, <<"html">>, []}, get_html}
  ], Req, State}.

%%------------------------------------------------------------------------------
%% Authorization Request
%%------------------------------------------------------------------------------

get_html(Req, State) ->
  case cowboy_req:qs_val(<<"redirect_uri">>, Req) of
    {undefined, Req2} ->
      fail(Req2, State#state{data = <<"invalid_request">>});
    {RedirectUri, Req2} ->
      check_valid_redirect_uri(Req2, State#state{redirect_uri = RedirectUri})
  end.

check_valid_redirect_uri(Req, State = #state{redirect_uri = RedirectUri}) ->
  {ClientId, Req2} = cowboy_req:qs_val(<<"client_id">>, Req),
  {Opaque, Req3} = cowboy_req:qs_val(<<"state">>, Req2, <<>>),
  case verify_redirection_uri(ClientId, RedirectUri) of
    ok ->
      check_response_type(Req3, State#state{client_id = ClientId,
          opaque = Opaque});
    % NB: do not redirect to unauthorized URI
    {error, mismatch} ->
      fail(Req3, State#state{data = <<"unauthorized_client">>,
          opaque = Opaque});
    % another validation error
    {error, badarg} ->
      fail(Req3, State#state{data = <<"invalid_request">>, opaque = Opaque})
  end.

check_response_type(Req, State) ->
  case cowboy_req:qs_val(<<"response_type">>, Req) of
    {<<"code">>, Req2} ->
      check_scope(Req2, State#state{response_type = <<"code">>});
    {<<"token">>, Req2} ->
      check_scope(Req2, State#state{response_type = <<"token">>});
    {_, Req2} ->
      fail(Req2, State#state{data = <<"unsupported_response_type">>})
  end.

check_scope(Req, State = #state{client_id = ClientId}) ->
  {Scope, Req2} = cowboy_req:qs_val(<<"scope">>, Req),
  case authorize_client_credentials(ClientId, implicit, Scope) of
    {ok, _, Scope2} ->
      authorization_decision(Req2, State#state{scope = Scope2});
    {error, scope} ->
      fail(Req, State#state{data = <<"invalid_scope">>});
    {error, _} ->
      fail(Req, State#state{data = <<"invalid_request">>})
  end.

%%------------------------------------------------------------------------------
%% Authorization Response
%%------------------------------------------------------------------------------

%%
%% @todo this route per se must be accessible by authenticated resourse owners!
%%

%%
%% @todo no-cache for these two
%%

authorization_decision(Req, State = #state{response_type = <<"code">>,
    client_id = ClientId, redirect_uri = RedirectUri,
    scope = Scope, opaque = Opaque,
    options = Opts
  }) ->
  % respond with form containing authorization code.
  % NB: flow continues after form submit ok
  Code = termit:encode_base64(
      {Opaque, ClientId, RedirectUri, Scope},
      key(code_secret, Opts)),
  {<<
      "<p>Client: \"", ClientId/binary, "\" asks permission for scope:\"", Scope/binary, "\"</p>",
      "<form action=\"", RedirectUri/binary, "\" method=\"get\">",
      "<input type=\"hidden\" name=\"code\" value=\"", Code/binary, "\" />",
      "<input type=\"hidden\" name=\"state\" value=\"", Opaque/binary, "\" />",
      "<input type=\"submit\" value=\"ok\" />",
      "</form>",
      "<form action=\"", RedirectUri/binary, "\" method=\"get\">",
      "<input type=\"hidden\" name=\"error\" value=\"access_denied\" />",
      "<input type=\"hidden\" name=\"state\" value=\"", Opaque/binary, "\" />",
      "<input type=\"submit\" value=\"nak\" />",
      "</form>"
    >>, Req, State};

authorization_decision(Req, State = #state{response_type = <<"token">>,
    client_id = ClientId, redirect_uri = RedirectUri,
    scope = Scope, opaque = Opaque,
    options = Opts
  }) ->
  % authorize client and get authorized scope
  case authorize_client_credentials(ClientId, implicit, Scope) of
    {ok, Identity, Scope2} ->
      % respond with form containing token
      Token = token({Identity, Scope2}, Scope2, Opts),
      TokenBin = urlencode(Token),
      {<<
          "<p>Client: \"", ClientId/binary, "\" asks permission for scope:\"", Scope2/binary, "\"</p>",
          "<form action=\"", RedirectUri/binary, "#", TokenBin/binary, "&state=", Opaque/binary, "\" method=\"get\">",
          "<input type=\"submit\" value=\"ok\" />",
          "</form>",
          "<form action=\"", RedirectUri/binary, "#error=access_denied&state=", Opaque/binary, "\" method=\"get\">",
          "<input type=\"submit\" value=\"nak\" />",
          "</form>"
        >>, Req, State};
    {error, scope} ->
      fail(Req, State#state{data = <<"invalid_scope">>});
    {error, _} ->
      fail(Req, State#state{data = <<"unauthorized_client">>})
  end.

%%------------------------------------------------------------------------------
%% Error Response
%%------------------------------------------------------------------------------

%% no redirect_uri is known or it's invalid -> respond with error
fail(Req, State = #state{data = Error, redirect_uri = undefined}) ->
  {ok, Req2} = cowboy_req:reply(400, [
      {<<"content-type">>, <<"application/json; charset=UTF-8">>},
      {<<"cache-control">>, <<"no-store">>},
      {<<"pragma">>, <<"no-cache">>}
    ], jsx:encode([{error, Error}]), Req),
  {halt, Req2, State};
%% redirect_uri is valid -> pass error to redirect_uri as fragment
fail(Req, State = #state{data = Error, response_type = <<"token">>,
    redirect_uri = RedirectUri, opaque = Opaque}) ->
  % redirect to redirect URI with data urlencoded
  {ok, Req2} = cowboy_req:reply(302, [
      {<<"location">>, << RedirectUri/binary, $#,
            (urlencode([
                {error, Error},
                {state, Opaque}
              ]))/binary >>},
      {<<"cache-control">>, <<"no-store">>},
      {<<"pragma">>, <<"no-cache">>}
    ], <<>>, Req),
  {halt, Req2, State};
%% redirect_uri is valid -> pass error to redirect_uri as querystring
fail(Req, State = #state{data = Error,
    redirect_uri = RedirectUri, opaque = Opaque}) ->
  % redirect to redirect URI with data urlencoded
  {ok, Req2} = cowboy_req:reply(302, [
      {<<"location">>, << RedirectUri/binary, $?,
            (urlencode([
                {error, Error},
                {state, Opaque}
              ]))/binary >>},
      {<<"cache-control">>, <<"no-store">>},
      {<<"pragma">>, <<"no-cache">>}
    ], <<>>, Req),
  {halt, Req2, State}.

%%------------------------------------------------------------------------------
%% Access Token Request
%%------------------------------------------------------------------------------

post_is_create(Req, State) ->
  {true, Req, State}.

put_json(Req, State) ->
  {ok, JSON, Req2} = cowboy_req:body(Req),
  case jsx:decode(JSON, [{error_handler, fun(_, _, _) -> {error, badarg} end}])
  of
    {error, _} ->
      {false, Req2, State};
    {incomplete, _} ->
      {false, Req2, State};
    Data ->
      request_token(Req2, State#state{data = Data})
  end.

put_form(Req, State) ->
  {ok, Data, Req2} = cowboy_req:body_qs(Req),
  request_token(Req2, State#state{data = Data}).

request_token(Req, State = #state{data = Data}) ->
  % try
    case lists:keyfind(<<"grant_type">>, 1, Data) of
      {_, <<"authorization_code">>} ->
        authorization_code_flow_stage2(Req, State);
      {_, <<"refresh_token">>} ->
        refresh_token(Req, State);
      {_, <<"password">>} ->
        password_credentials_flow(Req, State);
      {_, <<"client_credentials">>} ->
        client_credentials_flow(Req, State);
      _ ->
        fail(Req, State#state{data = <<"unsupported_grant_type">>})
    % end
  % catch _:_ ->
    % fail(Req, State#state{data = <<"invalid_request">>})
  end.

%%------------------------------------------------------------------------------
%% Access Token Response
%%------------------------------------------------------------------------------

%%
%% Exchange authorization code for access token.
%%
authorization_code_flow_stage2(Req, State = #state{
    data = Data, options = Opts
  }) ->
  ClientId = key(<<"client_id">>, Data),
  ClientSecret = key(<<"client_secret">>, Data),
  RedirectUri = key(<<"redirect_uri">>, Data),
  % decode token and ensure its validity
  % NB: code is expired after code_ttl seconds since issued
  case termit:decode_base64(
      key(<<"code">>, Data),
      key(code_secret, Opts),
      key(code_ttl, Opts))
  of
    {ok, {_, ClientId, RedirectUri, Scope}} ->
      % authorize client and get authorized scope
      case authorize_client_credentials(ClientId, ClientSecret, Scope) of
        {ok, Identity, Scope2} ->
          % respond with token
          % NB: can also issue refresh token
          issue_token(Req, State, {Identity, Scope2}, Scope2, Opts);
        {error, scope} ->
          fail(Req, State#state{data = <<"invalid_scope">>});
        {error, _} ->
          fail(Req, State#state{data = <<"unauthorized_client">>})
      end;
    {error, _} ->
      fail(Req, State#state{data = <<"invalid_grant">>})
  end.

%%
%% Refresh an access token.
%%
refresh_token(Req, State = #state{data = Data, options = Opts}) ->
  case termit:decode_base64(
      key(<<"refresh_token">>, Data),
      key(refresh_secret, Opts),
      key(refresh_ttl, Opts))
  of
    {ok, {Identity, Scope}} ->
      issue_token(Req, State, {Identity, Scope}, Scope, Opts);
    {error, _} ->
      fail(Req, State#state{data = <<"invalid_grant">>})
  end.

%%
%% Request access token for a resource owner.
%%
password_credentials_flow(Req, State = #state{
    data = Data, options = Opts
  }) ->
  % @todo ensure scheme is https
  case authorize_username_password(
      key(<<"username">>, Data),
      key(<<"password">>, Data),
      key(<<"scope">>, Data))
  of
    {ok, Identity, Scope} ->
      issue_token(Req, State, {Identity, Scope}, Scope, Opts);
    {error, scope} ->
      fail(Req, State#state{data = <<"invalid_scope">>});
    {error, _} ->
      fail(Req, State#state{data = <<"invalid_client">>})
  end.

%%
%% Request access code for a client.
%%
client_credentials_flow(Req, State = #state{
    data = Data, options = Opts
  }) ->
  % @todo ensure scheme is https
  case authorize_client_credentials(
      key(<<"client_id">>, Data),
      key(<<"client_secret">>, Data),
      key(<<"scope">>, Data))
  of
    {ok, Identity, Scope} ->
      issue_token(Req, State, {Identity, Scope}, Scope, Opts);
    {error, scope} ->
      fail(Req, State#state{data = <<"invalid_scope">>});
    {error, _} ->
      fail(Req, State#state{data = <<"invalid_client">>})
  end.

%%
%% Respond with access token.
%%
issue_token(Req, State, Context, Scope, Opts) ->
  {ok, Req2} = cowboy_req:reply(200, [
      {<<"content-type">>, <<"application/json; charset=UTF-8">>},
      {<<"cache-control">>, <<"no-store">>},
      {<<"pragma">>, <<"no-cache">>}
    ], jsx:encode(token(Context, Scope, Opts)), Req),
  {halt, Req2, State}.

token(Data, Scope, Opts) ->
  AccessToken = termit:encode_base64(Data, key(token_secret, Opts)),
  [
      {access_token, AccessToken},
      {token_type, <<"Bearer">>},
      {expires_in, key(token_ttl, Opts)},
      {scope, Scope}
    ].

token(Data, Scope, Opts, with_refresh) ->
  AccessToken = termit:encode_base64(Data, key(token_secret, Opts)),
  RefreshToken = termit:encode_base64(Data, key(refresh_secret, Opts)),
  [
      {access_token, AccessToken},
      {token_type, <<"Bearer">>},
      {expires_in, key(token_ttl, Opts)},
      {scope, Scope},
      {refresh_token, RefreshToken}
    ].

%%
%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.

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

%%
%%------------------------------------------------------------------------------
%% OAuth2 backend functions
%%------------------------------------------------------------------------------
%%

% ok | {error, mismatch}
authorize_username_password(undefined, _Password, _Scope) ->
  {error, mismatch};
authorize_username_password(Username, _Password, Scope) ->
  {ok, {user, Username}, Scope}.

% ok | {error, mismatch}
authorize_client_credentials(undefined, _ClientSecret, _Scope) ->
  {error, mismatch};
authorize_client_credentials(_ClientId, _ClientSecret, undefined) ->
  {error, scope};
authorize_client_credentials(_ClientId, _ClientSecret, <<"foo">>) ->
  {error, scope};
authorize_client_credentials(ClientId, _ClientSecret, Scope) ->
  {ok, {client, ClientId}, Scope}.

% ok | {error, mismatch} | {error, badarg}
verify_redirection_uri(undefined, _RedirectUri) ->
  {error, badarg};
verify_redirection_uri(_ClientId, _RedirectUri) ->
  ok.
  % {error, mismatch}.
