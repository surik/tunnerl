# tunnerl

Erlang SOCKS server


### Features

 * SOCKS v4 
   + without ident
   + connect command only

 * SOCKS v5 
   + without authorization 
   + connect command only

   
### Usage

Build:

    $ git clone https://github.com/surik/tunnerl.git
    $ cd tunnerl
    $ rebar get-deps compile

Edit tunnerl.config and then:

    $ ./start.sh
