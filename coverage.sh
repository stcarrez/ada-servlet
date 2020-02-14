#!/bin/sh
NAME=servletada.cov
lcov --quiet --base-directory . --directory . -c --include "*/ada-servlet/src/*" -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
