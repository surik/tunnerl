#~/usr/bin/env sh

erl -pa ebin deps/*/ebin -config tunnerl.config -s tunnerl -noshell
