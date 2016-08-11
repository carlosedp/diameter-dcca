# Erlang Diameter Credit Control Application

This repository contains an example DCCA application server and client built in Erlang.

There is also an updated version of the server and client modules as OTP applications. They can be fetched from my repositories https://github.com/carlosedp/dcca-server-OTP and https://github.com/carlosedp/dcca-client-OTP.

To build the modules and diameter dictionaries, use the provided Makefile.

To start the modules use:

## Server

    erl
    server:start().


## Client
    erl
    client:start().
    client:test().

## Seagull

Seagull can also be used to generate Diameter traffic to the server. The configuration and scenario xml are in Seagull dir.

The application can be downloaded from [http://gull.sourceforge.net/doc/](http://gull.sourceforge.net/doc/).


