# Erlang Diameter Credit Control Application

This repository contains an example DCCA application server and client built in Erlang.

To build the modules and diameter dictionaries, use the provided Makefile.

To start the modules use:

## Server

    erl
    server:start().


## Client
    erl
    client:start().
    client:charge_event().

## Seagull

Seagull can also be used to generate Diameter traffic to the server. The configuration and scenario xml are in Seagull dir.

The application can be downloaded from [http://gull.sourceforge.net/doc/](http://gull.sourceforge.net/doc/).


