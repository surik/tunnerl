# tunnerl [![Build Status](https://travis-ci.org/surik/tunnerl.svg?branch=master)](https://travis-ci.org/surik/tunnerl)

Erlang SOCKS server


### Features

 * SOCKS v4 and SOCKS v4A 
   * connect command only

 * SOCKS v5 
   * only username authorization 
   * connect command only
   * IPv4 and IPv6
   
### Usage

Build:

    $ git clone https://github.com/surik/tunnerl.git
    $ cd tunnerl
    $ rebar3 compile

Edit tunnerl.config and then:

    $ ./start.sh
