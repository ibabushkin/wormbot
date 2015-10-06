# wormbot - a minimalistic IRC bot
Let's dive straight in:

## Features
* rejoin on kick
* multiple channels
* modules
* multiple command prefixes + "user commands"
* builtin command `:c` to list modules and their state (executable?)

## Module system
Every module is just an executable script in a special directory.
It is called by the bot with commandline args, and it's output
is piped to irc.

### Example
```sh
$ cat scripts/test.py
#!/bin/env python
print("...")
```
So if someone writes `:test` into a channel where the bot
is present, the script above will get executed and the output
will be seen.

Note that you can just use a prefix of the script's name and
the first matching script will be used.

Some modules, however, need to know who invoked them. This is possible
by looking up the environment variable `NICKNAME` that is set in the
script's environment.

## Configuring and Installing
Currently, this is done via editing the source of `Bot.hs`.

### Installation via cabal-install
Assuming you have GHC and cabal-install:
```sh
$ git clone https://github.com/ibabushkin/wormbot
$ cd wormbot/
$ cabal build
$ cabal install
```
Now you have a binary called `wormbot` in your `~/.cabal/bin`.

## Contributing
Do it. Pull requests with scripts, as well as additions to the core,
are welcome. Open issues if you feel it is needed.
