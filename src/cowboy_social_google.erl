%%
%% @doc Handler for social login via OAuth2 providers.
%%

-module(cowboy_social_google).
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
%% {"/auth/google/:action", cowboy_social_google, [...]}.
%%
handle(Req, Opts) ->
  % extract flow action name
  {Action, Req2} = cowboy_req:binding(action, Req),
  % construct callback URI
  {SelfUri, Req3} = cowboy_req:host_url(Req2),
  [FullPath] = cowboy_req:get([path], Req3),
  CallbackUrl = << SelfUri/binary, FullPath/binary >>,
  % perform flow action
  {ok, Req4} = handle_request(Action, Opts, CallbackUrl, Req3),
  {ok, Req4, undefined}.

%%
%% redirect to provider authorization page, expect it to redirect
%% to our next handler
%%
handle_request(<<"login">>, Opts, U, Req)  ->
  AuthUrl = << (authorize_url())/binary, $?,
      (cowboy_request:urlencode([
        {<<"client_id">>, key(client_id, Opts)},
        {<<"redirect_uri">>, binary:replace(U, <<"/login">>, <<"/callback">>)},
        {<<"response_type">>, <<"code">>},
        {<<"scope">>, key(scope, Opts)}
      ]))/binary >>,
  cowboy_req:reply(303, [{<<"location">>, AuthUrl}], <<>>, Req);

%%
%% provider redirected back to us with authorization code
%%
handle_request(<<"callback">>, Opts, U, Req) ->
  case cowboy_req:qs_val(<<"code">>, Req) of
    {undefined, Req2} ->
      finish({error, nocode}, Req2, Opts);
    {Code, Req2} ->
      get_access_token(Opts, U, Code, Req2)
  end;

%%
%% catchall
%%
handle_request(_, _, _, Req) ->
  {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req),
  {ok, Req2, undefined}.

%%
%% exchange authorization code for auth token
%%
get_access_token(Opts, U, Code, Req) ->
  case cowboy_request:post_for_json(token_url(), [
      {<<"code">>, Code},
      {<<"client_id">>, key(client_id, Opts)},
      {<<"client_secret">>, key(client_secret, Opts)},
      {<<"redirect_uri">>, U},
      {<<"grant_type">>, <<"authorization_code">>}
    ])
  of
    {ok, Auth} ->
      get_user_profile(Opts, Auth, Req);
    _ ->
      finish({error, notoken}, Req, Opts)
  end.

%%
%% use auth tocken to extract info from user profile
%%
get_user_profile(Opts, Auth, Req) ->
  AccessToken = key(<<"access_token">>, Auth),
  case cowboy_request:get_json(profile_url(), [
      {<<"access_token">>, AccessToken}
    ])
  of
    {ok, Profile} ->
      finish({ok, normalize_profile(Auth, Profile)},
          Req, Opts);
    _ ->
      finish({error, noprofile}, Req, Opts)
  end.

%%
%% finalize application flow by calling callback handler
%%
finish(Status, Req, Opts) ->
  {M, F} = key(callback, Opts),
  M:F(Status, Req).

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.

authorize_url() ->
  <<"https://accounts.google.com/o/oauth2/auth">>.

token_url() ->
  <<"https://accounts.google.com/o/oauth2/token">>.

profile_url() ->
  <<"https://www.googleapis.com/oauth2/v1/userinfo">>.

normalize_profile(_A, P) ->
  [
    {id, << "google:", (key(<<"id">>, P))/binary >>},
    {provider, <<"google">>},
    {email, key(<<"email">>, P)},
    {name, key(<<"name">>, P)},
    {avatar, key(<<"picture">>, P)},
    {gender, key(<<"gender">>, P)},
    {locale, key(<<"locale">>, P)}
  ].

