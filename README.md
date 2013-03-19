Social
==============

Handler for social login via OAuth2 providers.

Usage
--------------

Register your application with callback URI pointing back to your site plus `/auth/:provider/callback`, and supply `ClientID` and `ClientSecret` in provider's configuration proplist.
Ensure you started native erlang 'ssl' appication.

Then in your client-side code:
```html
<a href="/auth/google/login">Auth via Google</a>
```

Router configuration
--------------

```erlang
{"/auth/google/:action", cowboy_social_google, [
  % At the end of the flow this handler will be called as
  % Mod:Fun({ok, Auth, Profile}, Req) or Mod:Fun({error, Reason}, Req)
  {handler, {Mod, Fun}},
  {client_id, <<"440647648374.apps.googleusercontent.com">>},
  {client_secret, <<"...">>},
  {scope, <<"https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile">>},
  {callback_uri, <<"/auth/google/callback">>}
]}.
{"/auth/github/:action", cowboy_social_github, [
  {handler, {Mod, Fun}},
  {client_id, <<"883b68d607abddc24f77">>},
  {client_secret, <<"...">>},
  {scope, <<>>},
  {callback_uri, <<"/auth/github/callback">>}
]}.
{"/auth/yandex/:action", cowboy_social_yandex, [
  {handler, {Mod, Fun}},
  {client_id, <<"f44bd59ddfbe408ab1d29151126385a6">>},
  {client_secret, <<"...">>},
  {scope, <<>>},
  {callback_uri, <<"/auth/yandex/callback">>}
]}.
{"/auth/vkontakte/:action", cowboy_social_vkontakte, [
  {handler, {Mod, Fun}},
  {client_id, <<"3473116">>},
  {client_secret, <<"...">>},
  {scope, <<"uid,first_name,last_name,sex,photo">>},
  {callback_uri, <<"/auth/vkontakte/callback">>}
]}.
{"/auth/mailru/:action", cowboy_social_mailru, [
  {handler, {Mod, Fun}},
  {client_id, <<"701614">>},
  {client_secret, <<"...">>},
  {secret_key, <<"...">>},
  {scope, <<>>},
  {callback_uri, <<"/auth/mailru/callback">>}
]}.
```

License (MIT)
-------

Copyright (c) 2013 Vladimir Dronnikov <dronnikov@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
