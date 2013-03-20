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
  % compose full redirect URI
  case key(callback_uri, Opts) of
    << "http://", _/binary >> -> {ok, Req, Opts};
    << "https://", _/binary >> -> {ok, Req, Opts};
    Relative ->
      {SelfUri, Req2} = cowboy_req:host_url(Req),
      {ok, Req2, lists:keyreplace(callback_uri, 1, Opts,
        {callback_uri, << SelfUri/binary, Relative/binary >>})}
  end.

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
  cowboy_req:reply(302, [
      {<<"location">>, (key(provider, Opts)):get_authorize_url(Opts)}
    ], <<>>, Req);

%%
%% provider redirected back to us with authorization code
%%
handle_request(<<"callback">>, Req, Opts) ->
  case cowboy_req:qs_val(<<"code">>, Req) of
    {undefined, Req2} ->
      finish({error, nocode}, Req2, Opts);
    {Code, Req2} ->
      try get_access_token(Code, Req2, Opts) of
        Result -> Result
      catch _:_ ->
        finish({error, notoken}, Req2, Opts)
      end
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
  {ok, Auth} = (key(provider, Opts)):get_access_token(Code, Opts),
  get_user_profile(Auth, Req, Opts).

%%
%% use auth token to extract info from user profile
%%
get_user_profile(Auth, Req, Opts) ->
  {ok, Profile} = (key(provider, Opts)):get_user_profile(Auth, Opts),
  finish({ok, Auth, Profile}, Req, Opts).

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
