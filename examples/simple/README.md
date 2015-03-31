# Simple Social Example

This example shows the basic usage of social for OAuth.

By default, the HTTP server listens on port 8080.

## Configuration

In `etc/app.config` there is a commented list of OAuth provider
configs.  To get the example working, uncomment the desired sections
and fill in the client id, secret, and any other necessary values. You
should be able to get these values from the OAuth provider.

If no OAuth providers are configured you will be unable to use social.

## Usage

To build and run the application:

```shell
# fetch the deps, compile the app, and build the release
make

# start the application in the foreground
./_rel/simple_example/bin/simple_example foreground
```

You should now be able to login into a configured OAuth provider at
`auth/$provider/login`.

## Environment

This was tested using:

- rebar 2.5.1-1-ge9f62c4
- relx 1.1.0 (there were issues with 1.2.0)
