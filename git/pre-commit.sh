#!/bin/bash

eval $(opam env)

which ocp-indent 2>&1 >/dev/null || opam install -y 'ocp-indent>=1.7.0'

[ -L ./.git/hooks/pre-commit ] || ln -s ../../git/pre-commit.sh .git/hooks/pre-commit

exit_code=0

for fname in $(find src tests -name '*.ml' -or -name '*.mli'); do
    if [ "$(echo $fname | grep cppo)" == '' ]; then
	d1=$(cat "$fname")
	d2=$(ocp-indent "$fname")

	if [[ "$d1" = "$d2" ]]; then
	    echo -e "\033[32m[Passed]\033[0m $fname is already formatted."
	else
	    echo -e "\033[31m[Failed]\033[0m $fname is NOT formatted."
	    ocp-indent -i "$fname"
	    exit_code=1
	fi
    else
	echo -e "\033[33m[Skipped]\033[0m $fname is skipped."
    fi
done

exit $exit_code
