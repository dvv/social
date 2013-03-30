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
% Use :action binding
{"/auth/facebook/:action", cowboy_social, [
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {callback_uri, <<"/auth/facebook/callback">>},
  {scope, <<"email">>},
  {authorize_uri, <<"https://www.facebook.com/dialog/oauth">>},
  {token_uri, <<"https://graph.facebook.com/oauth/access_token">>}
]},
{"/auth/github/:action", cowboy_social, [
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {callback_uri, <<"/auth/github/callback">>},
  {scope, <<>>},
  {authorize_uri, <<"https://github.com/login/oauth/authorize">>},
  {token_uri, <<"https://github.com/login/oauth/access_token">>}
]},
{"/auth/google/:action", cowboy_social, [
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {callback_uri, <<"/auth/google/callback">>},
  {scope, << "https://www.googleapis.com/auth/userinfo.email ",
             "https://www.googleapis.com/auth/userinfo.profile" >>},
  {authorize_uri, <<"https://accounts.google.com/o/oauth2/auth">>},
  {token_uri, <<"https://accounts.google.com/o/oauth2/token">>}
]},
{"/auth/mailru/:action", cowboy_social, [
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {secret_key, <<"f431aea09762dbad13c2440955e12aee">>},
  {callback_uri, <<"/auth/mailru/callback">>},
  {scope, <<>>},
  {authorize_uri, <<"https://connect.mail.ru/oauth/authorize">>},
  {token_uri, <<"https://connect.mail.ru/oauth/token">>}
]},
{"/auth/paypal/:action", cowboy_social, [
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {callback_uri, <<"/auth/paypal/callback">>},
  {scope, <<"https://identity.x.com/xidentity/resources/profile/me">>},
  {authorize_uri, <<"https://identity.x.com/xidentity/resources/authorize">>},
  {token_uri, <<"https://identity.x.com/xidentity/oauthtokenservice">>}
]},
{"/auth/vkontakte/:action", cowboy_social, [
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {callback_uri, <<"/auth/vkontakte/callback">>},
  {scope, <<"uid,first_name,last_name,sex,photo">>},
  {authorize_uri, <<"https://oauth.vk.com/authorize">>},
  {token_uri, <<"https://oauth.vk.com/access_token">>}
]},
{"/auth/yandex/:action", cowboy_social, [
  {client_id, <<"...">>},
  {client_secret, <<"...">>},
  {callback_uri, <<"/auth/yandex/callback">>},
  {scope, <<>>},
  {authorize_uri, <<"https://oauth.yandex.ru/authorize">>},
  {token_uri, <<"https://oauth.yandex.ru/token">>}
]}.
```

Supported providers
--------------
- Facebook
- Github
- Google
- Mail.ru
- PayPal
- Vkontakte
- Yandex
- Add more yourself -- this is very simple -- tune `scope`, `authorize_uri` and `token_uri` options.

Please, consider to feedback here successful options you found for another providers.

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
