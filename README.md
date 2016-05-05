# tunnerl

Erlang SOCKS server


### Features

 * SOCKS v4 
   + connect command only

 * SOCKS v5 
   + only username authorization 
   + connect command only

   
### Usage

Build:

    $ git clone https://github.com/surik/tunnerl.git
    $ cd tunnerl
    $ rebar3 compile

Edit tunnerl.config and then:

    $ ./start.sh
