# wormbot - a minimalistic IRC bot
Let's dive straight in:

## Features
* rejoin on kick
* multiple channels
* modules

## Module system
Every module is just an executable script in a special directory.
It is called by the bot with commandline args, and it's output
is piped to irc.

### Example
```sh
$ cat scripts/commands.sh
#!/usr/bin/env sh

ls scripts/
```
So if someone writes `!commands` into a channel where the bot
is present, the script above will get executed and the output
will be seen.

Note that you can just use a prefix of the script's name and
the first matching script will be used.

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
