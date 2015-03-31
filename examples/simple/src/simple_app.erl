-module(simple_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(HTTP_PORT, 8080).
-define(COWBOY_REF, cowboy).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([{'_', routes()}]),
  {ok, _} = cowboy:start_http(?COWBOY_REF, 100,
                              [{port, ?HTTP_PORT}],
                              [{env, [{dispatch, Dispatch}]}]),
  simple_sup:start_link().

stop(_State) ->
  cowboy:stop_lister(?COWBOY_REF).

%% ===================================================================
%% Internal
%% ===================================================================

routes() ->
  case application:get_env(oauth_providers) of
    {ok, OAuthProviders} ->
      [{"/auth/:provider/:action", cowboy_social, OAuthProviders}]
  end.
