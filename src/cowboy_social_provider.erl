%%
%% @doc Handler for OAuth2 provider.
%%

-module(cowboy_social_provider).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(cowboy_http_handler).
-export([init/3, terminate/3, handle/2]).

%%
%%------------------------------------------------------------------------------
%% OAUTH2 Application flow
%%------------------------------------------------------------------------------
%%

init(_Transport, Req, Opts) ->
  {ok, Req, Opts}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, Opts) ->
  % extract flow action name
  {Action, Req2} = cowboy_req:binding(action, Req),
  % perform flow action
  {ok, Req3} = handle_request(Action, Req2, Opts),
  {ok, Req3, undefined}.

%%
%% Client asks user to authorize client and provide access code
%%
handle_request(<<"authorize">>, Req, Opts)  ->
  % extract parameters
  {Data, Req2} = cowboy_req:qs_vals(Req),
  case lists:keyfind(<<"client_id">>, 1, Data) of
    false ->
      handle_flow(username_and_password, Data, Req2, Opts);
    _ ->
      handle_flow(client_id_and_secret, Data, Req2, Opts)
  end;

%%
%% Client requests access token in exchange to access code.
%% NB: we read data from body, hence POST
%%
handle_request(<<"access_token">>, Req, Opts)  ->
  % extract parameters
  {ok, Data, Req2} = cowboy_req:body_qs(Req),
  % State = key(<<"state">>, Data),
  ClientId = key(<<"client_id">>, Data),
  ClientSecret = key(<<"client_secret">>, Data),
  RedirectUri = key(<<"redirect_uri">>, Data),
  _GrantType = key(<<"grant_type">>, Data),
  % decode token and ensure its validity
  % @todo State instead of _
  {ok, {_, ClientId, RedirectUri, Scope}} =
      % NB: code is expired after code_ttl seconds since issued
      termit:decode_base64(
          key(<<"code">>, Data),
          key(code_secret, Opts),
          key(code_ttl, Opts)
        ),
  % authorize client and get authorized scope
  {ok, Identity, Scope2} =
      authorize_client_credentials(ClientId, ClientSecret, Scope),
  % respond with token
  issue_token({Identity, Scope2}, Req2, Opts);

%%
%% Catchall
%%
handle_request(_, Req, _) ->
  {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req),
  {ok, Req2, undefined}.

%%
%% Request access code for a client.
%%
handle_flow(client_id_and_secret, Data, Req, Opts) ->
  ClientId = key(<<"client_id">>, Data),
  RedirectUri = key(<<"redirect_uri">>, Data),
  % State = key(<<"state">>, Data),
  Scope = key(<<"scope">>, Data),
  % redirect URI fits the client?
  case verify_redirection_uri(ClientId, RedirectUri) of
    % yes
    ok ->
      % generate authorization code
      State = nonce(),
      Code = termit:encode_base64(
          {State, ClientId, RedirectUri, Scope},
          key(code_secret, Opts)),
      % show authorization form
      {M, F} = key(authorization_form, Opts),
      M:F(Code, RedirectUri, State, Req);
    % no
    {error, mismatch} ->
      % return error
      cowboy_req:reply(302, [
          {<<"location">>, << RedirectUri/binary, $?, "error=redirect_uri" >>}
        ], Req)
  end;

%%
%% Request access token for a user.
%%
handle_flow(username_and_password, Data, Req, Opts) ->
  {ok, Identity, Scope} = authorize_username_password(
      key(<<"username">>, Data),
      key(<<"password">>, Data),
      key(<<"scope">>, Data)),
  issue_token({Identity, Scope}, Req, Opts).

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.

nonce() ->
  base64:encode(crypto:strong_rand_bytes(16)).

issue_token(Data, Req, Opts) ->
  Token = termit:encode_base64(Data, key(token_secret, Opts)),
  cowboy_req:reply(200, [
      {<<"content-type">>, <<"application/json">>}
    ], jsx:encode([
      {access_token, Token},
      {token_type, <<"Bearer">>},
      {expires_in, key(token_ttl, Opts)}
    ]), Req).

%%
%%------------------------------------------------------------------------------
%% OAuth2 backend functions
%%------------------------------------------------------------------------------
%%

% ok | {error, mismatch}
authorize_username_password(Username, _Password, Scope) ->
  {ok, {user, Username}, Scope}.

% ok | {error, mismatch}
authorize_client_credentials(ClientId, _ClientSecret, Scope) ->
  {ok, {client, ClientId}, Scope}.

% ok | {error, mismatch}
verify_redirection_uri(_ClientId, _RedirectUri) ->
  ok.
