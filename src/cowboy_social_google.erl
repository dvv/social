%%
%% @doc Handler for social login via Google.
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
  % compose full redirect URI
  {SelfUri, Req2} = cowboy_req:host_url(Req),
  {ok, Req2, lists:keyreplace(callback_uri, 1, Opts,
      {callback_uri, << SelfUri/binary, (key(callback_uri, Opts))/binary >>})}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, Opts) ->
  % extract flow action name
  {Action, Req2} = cowboy_req:binding(action, Req),
  % perform flow action
  {ok, Req3} = handle_request(Action, Req2, Opts),
  {ok, Req3, undefined}.

%%
%% redirect to provider authorization page, expect it to redirect
%% to our next handler
%%
handle_request(<<"login">>, Req, Opts)  ->
  AuthUrl = << (authorize_url())/binary, $?,
      (cowboy_request:urlencode([
        {<<"client_id">>, key(client_id, Opts)},
        {<<"redirect_uri">>, key(callback_uri, Opts)},
        {<<"response_type">>, <<"code">>},
        {<<"scope">>, key(scope, Opts)}
      ]))/binary >>,
  cowboy_req:reply(303, [{<<"location">>, AuthUrl}], <<>>, Req);

%%
%% provider redirected back to us with authorization code
%%
handle_request(<<"callback">>, Req, Opts) ->
  case cowboy_req:qs_val(<<"code">>, Req) of
    {undefined, Req2} ->
      finish({error, nocode}, Req2, Opts);
    {Code, Req2} ->
      get_access_token(Code, Req2, Opts)
  end;

%%
%% catchall
%%
handle_request(_, Req, _) ->
  {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req),
  {ok, Req2, undefined}.

%%
%% exchange authorization code for auth token
%%
get_access_token(Code, Req, Opts) ->
  try cowboy_request:post_for_json(token_url(), [
      {<<"code">>, Code},
      {<<"client_id">>, key(client_id, Opts)},
      {<<"client_secret">>, key(client_secret, Opts)},
      {<<"redirect_uri">>, key(callback_uri, Opts)},
      {<<"grant_type">>, <<"authorization_code">>}
    ])
  of
    {ok, Auth} ->
      get_user_profile(Auth, Req, Opts);
    _ ->
      finish({error, notoken}, Req, Opts)
  catch _:_ ->
    finish({error, notoken}, Req, Opts)
  end.

%%
%% use auth token to extract info from user profile
%%
get_user_profile(Auth, Req, Opts) ->
  AccessToken = key(<<"access_token">>, Auth),
  try cowboy_request:get_json(profile_url(), [
      {<<"access_token">>, AccessToken}
    ])
  of
    {ok, Profile} ->
      finish({ok, normalize_auth(Auth), normalize_profile(Profile)}, Req, Opts);
    _ ->
      finish({error, noprofile}, Req, Opts)
  catch _:_ ->
    finish({error, noprofile}, Req, Opts)
  end.

%%
%% finalize application flow by calling callback handler
%%
finish(Status, Req, Opts) ->
  {M, F} = key(handler, Opts),
  M:F(Status, Req).

%%
%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%

key(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.

%%
%%------------------------------------------------------------------------------
%% Provider details
%%------------------------------------------------------------------------------
%%

authorize_url() ->
  <<"https://accounts.google.com/o/oauth2/auth">>.

token_url() ->
  <<"https://accounts.google.com/o/oauth2/token">>.

profile_url() ->
  <<"https://www.googleapis.com/oauth2/v1/userinfo">>.

normalize_auth(Auth) ->
  [
    {access_token, key(<<"access_token">>, Auth)},
    {token_type, key(<<"token_type">>, Auth)},
    {expires_in, key(<<"expires_in">>, Auth)}
  ].

normalize_profile(Raw) ->
  [
    {id, << "google:", (key(<<"id">>, Raw))/binary >>},
    {provider, <<"google">>},
    {email, key(<<"email">>, Raw)},
    {name, key(<<"name">>, Raw)},
    {avatar, key(<<"picture">>, Raw)},
    {gender, key(<<"gender">>, Raw)},
    {locale, key(<<"locale">>, Raw)}
  ].
