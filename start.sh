#~/usr/bin/env sh

erl -pa ebin _build/default/lib/*/ebin -config tunnerl.config -s tunnerl -noshell $@
