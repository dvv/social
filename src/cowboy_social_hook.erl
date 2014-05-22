%%
%% @doc Behavior for a post-callback hook.
%%

-module(cowboy_social_hook).

-type token_props() :: [{atom(), binary()}].
-export_type([token_props/0]).

%% XXX - R15B required for this syntax
-callback execute(TokenProps, Req, State)
  -> {ok, TokenProps, Req, State}
  | {error, binary(), Req, State}
  when TokenProps::token_props(), Req::cowboy:req(), State::any().
