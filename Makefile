SHELL=/bin/bash

DIALYZER_APPS = \
	erts stdlib crypto public_key inets xmerl sasl tools kernel

compile:
	./rebar3 compile

doc:	compile
	./rebar3 compile doc

eunit:	compile
	./rebar3 eunit

## create the script
generate: 	compile
	./rebar3 compile escriptize

clean:
	rm -rf .eunit
	rm -rf `find . -name *.beam`

dialyze:	compile
	dialyzer --plt .dialyzer_plt  -pa _build/default/lib/xref_analyze/ebin -Wno_return \
		     --apps _build/default/lib/xref_analyze/ebin | tee dialyzer_output.txt > /dev/null
	./filter_output.sh dialyzer_output.txt dialyzer_filter_warnings.txt

.create_plt:
	dialyzer --no_check_plt --build_plt  --output_plt .dialyzer_plt \
		     --apps $(DIALYZER_APPS)

xref:		compile
	./rebar3 xref  | sed  '1!G;h;$!d'  | tail -n +2 | sed '1!G;h;$!d' | tee xref_output.txt  > /dev/null
	./filter_output.sh xref_output.txt xref_filter_warnings.txt

check: xref dialyze eunit

etags: clean
	find . -name \*.[eh]rl | ./erltags -s -f -o tags
