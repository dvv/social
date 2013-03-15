-module(cowboy_social_providers).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([authorize_url/1]).
-export([custom_data/3]).
-export([normalize_profile/3]).
-export([profile_url/1]).
-export([token_url/1]).

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
%% Providers
%%------------------------------------------------------------------------------
%%

authorize_url(google) -> <<"https://accounts.google.com/o/oauth2/auth">>;
authorize_url(github) -> <<"https://github.com/login/oauth/authorize">>;
authorize_url(twitter) -> <<"https://api.twitter.com/oauth/authorize">>;
authorize_url(vkontakte) -> <<"https://oauth.vk.com/authorize">>;
authorize_url(yandex) -> <<"https://oauth.yandex.ru/authorize">>.

token_url(google) -> <<"https://accounts.google.com/o/oauth2/token">>;
token_url(github) -> <<"https://github.com/login/oauth/access_token">>;
token_url(twitter) -> <<"https://api.twitter.com/oauth/access_token">>;
token_url(vkontakte) -> <<"https://oauth.vk.com/access_token">>;
token_url(yandex) -> <<"https://oauth.yandex.ru/token">>.

profile_url(google) -> <<"https://www.googleapis.com/oauth2/v1/userinfo">>;
profile_url(github) -> <<"https://api.github.com/user">>;
profile_url(twitter) -> <<"https://api.twitter.com/1/users/lookup.json">>;
profile_url(vkontakte) -> <<"https://api.vk.com/method/users.get">>;
profile_url(yandex) -> <<"https://login.yandex.ru/info">>.

custom_data(twitter, AccessToken, _ProviderOpts) ->
  UserId = key(<<"user_id">>, AccessToken),
  [{<<"user_id">>, UserId}];
custom_data(vkontakte, _AccessToken, ProviderOpts) ->
  Scope = key(scope, ProviderOpts),
  [{<<"fields">>, Scope}];
custom_data(yandex, AccessToken, _ProviderOpts) ->
  [{<<"oauth_token">>, AccessToken}, {<<"format">>, <<"json">>}];
custom_data(_, _, _) ->
  [].

normalize_profile(google, _A, P) ->
  [
    {id, << "google:", (key(<<"id">>, P))/binary >>},
    {provider, <<"google">>},
    {email, key(<<"email">>, P)},
    {name, key(<<"name">>, P)},
    {avatar, key(<<"picture">>, P)},
    {gender, key(<<"gender">>, P)},
    {locale, key(<<"locale">>, P)}
  ];

normalize_profile(github, _A, P) ->
  [
    {id, << "github:",
      (list_to_binary(integer_to_list(key(<<"id">>, P))))/binary >>},
    {provider, <<"github">>},
    {email, key(<<"email">>, P)},
    {name, key(<<"name">>, P)},
    {avatar, key(<<"avatar_url">>, P)}
  ];

normalize_profile(twitter, _A, P) ->
  [
    {id, << "twitter:", (key(<<"id">>, P))/binary >>},
    {provider, <<"twitter">>},
    {name, key(<<"name">>, P)},
    {avatar, key(<<"profile_image_url_https">>, P)}
  ];

normalize_profile(vkontakte, _A, P0) ->
  % NB: provider returns list of data for uids; we need only the first
  [P] = key(<<"response">>, P0),
  [
    {id, << "vkontakte:",
      (list_to_binary(integer_to_list(key(<<"uid">>, P))))/binary >>},
    {provider, <<"vkontakte">>},
    % {email, key(<<"email">>, P)},
    {name, << (key(<<"first_name">>, P))/binary, " ",
              (key(<<"last_name">>, P))/binary >>},
    {avatar, key(<<"photo">>, P)},
    {gender, case key(<<"sex">>, P) of 1 -> <<"female">>; _ -> <<"male">> end}
  ];

normalize_profile(yandex, _A, P) ->
  [
    {id, << "yandex:", (key(<<"id">>, P))/binary >>},
    {provider, <<"yandex">>},
    {email, key(<<"default_email">>, P)},
    {name, key(<<"real_name">>, P)},
    % {avatar, key(<<"picture">>, P)},
    {gender, case key(<<"sex">>, P) of 1 -> <<"female">>; _ -> <<"male">> end}
  ].
