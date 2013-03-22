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
% Authorize via Google
{"/auth/google/:action", cowboy_social, [
  {provider, cowboy_social_google},
  % At the end of the flow this handler will be called as
  % Mod:Fun({ok, Auth, Profile}, Req, State) or Mod:Fun({error, Reason}, Req, State)
  {handler, {Mod, Fun}},
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {scope, <<>>}, % additional permissions
  {callback_uri, <<"/auth/google/callback">>}
]}.

% In case of compliant provider we can just tune provider
{"/auth/good-provider/:action", cowboy_social, [
  {provider, cowboy_social_generic},
  {handler, {Mod, Fun}},
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {scope, <<>>}, % additional permissions
  {callback_uri, <<"/auth/good-provider/callback">>},
  % tune generic provider
  {provider_name, <<"good-provider">>},
  {authorize_url, <<"https://good.provider.org/oauth2/authorize">>},
  {access_token_url, <<"https://good.provider.org/oauth2/access_token">>},
  {profile_url, <<"https://good.provider.org/profile">>}
]}.
```

Supported providers
--------------
- Facebook
- Generic one (no separate module required, just some parameters for the handler)
- Github
- Google
- Mail.ru
- PayPal
- Vkontakte
- Yandex
- add more, this is very simple

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
