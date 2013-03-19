%%
%% @doc Handler for social login via OAuth2 providers.
%%

-module(cowboy_social).
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

%%
%% {"/auth/:provider/:action", cowboy_social, [...]}.
%%
handle(Req, Opts) ->
  Provider = key(provider, Opts),
  % extract flow action name
  {Action, Req2} = cowboy_req:binding(action, Req),
  % construct callback URI
  {SelfUri, Req3} = cowboy_req:host_url(Req2),
  [FullPath] = cowboy_req:get([path], Req3),
  CallbackUrl = << SelfUri/binary, FullPath/binary >>,
  % perform flow action
  {ok, Req4} = handle_request(Action, Provider, Opts, CallbackUrl, Req3),
  {ok, Req4, undefined}.

%%
%% redirect to provider authorization page, expect it to redirect
%% to our next handler
%%
handle_request(<<"login">>, P, O, U, Req)  ->
  AuthUrl = << (cowboy_social_providers:authorize_url(P))/binary, $?,
      (cowboy_request:urlencode([
        {<<"client_id">>, key(client_id, O)},
        {<<"redirect_uri">>, binary:replace(U, <<"/login">>, <<"/callback">>)},
        {<<"response_type">>, <<"code">>},
        {<<"scope">>, key(scope, O)}
      ]))/binary >>,
  cowboy_req:reply(303, [{<<"location">>, AuthUrl}], <<>>, Req);

%%
%% provider redirected back to us with authorization code
%%
handle_request(<<"callback">>, P, O, U, Req) ->
  case cowboy_req:qs_val(<<"code">>, Req) of
    {undefined, Req2} ->
      finish({error, nocode}, Req2);
    {Code, Req2} ->
      get_access_token(P, O, U, Code, Req2)
  end;

%%
%% catchall
%%
handle_request(_, _, _, _, Req) ->
  {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req),
  {ok, Req2, undefined}.

%%
%% exchange authorization code for auth token
%%
get_access_token(P, O, U, Code, Req) ->
  case cowboy_request:post_for_json(cowboy_social_providers:token_url(P), [
      {<<"code">>, Code},
      {<<"client_id">>, key(client_id, O)},
      {<<"client_secret">>, key(client_secret, O)},
      {<<"redirect_uri">>, U},
      {<<"grant_type">>, <<"authorization_code">>}
    ])
  of
    {ok, Auth} ->
      get_user_profile(P, O, Auth, Req);
    _ ->
      finish({error, notoken}, Req)
  end.

%%
%% use auth tocken to extract info from user profile
%%
get_user_profile(P, O, Auth, Req) ->
  AccessToken = key(<<"access_token">>, Auth),
  case cowboy_request:get_json(cowboy_social_providers:profile_url(P), [
      {<<"access_token">>, AccessToken}
        | cowboy_social_providers:custom_data(P, AccessToken, O)
    ])
  of
    {ok, Profile} ->
      finish({ok, cowboy_social_providers:normalize_profile(P, Auth, Profile)},
          Req);
    _ ->
      finish({error, noprofile}, Req)
  end.

%%
%% finalize application flow by calling callback handler
%%
finish(Status, Req) ->
  {{M, F}, Req2} = cowboy_req:meta(callback, Req),
  M:F(Status, Req2).

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.
