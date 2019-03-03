#!/bin/sh
lcov --base-directory . --directory . -c -o servlet.cov >/dev/null
bin/servlet_harness -t 120 -xml servlet-aunit.xml -config test.properties
(lcov --base-directory . --directory . -c -o servlet.cov
lcov --remove servlet.cov "/usr*" -o servlet.cov 
lcov --remove servlet.cov "/opt*" -o servlet.cov 
lcov --remove servlet.cov "regtests*" -o servlet.cov
lcov --remove servlet.cov "*/ada-util/*" -o servlet.cov
lcov --remove servlet.cov ada-servlet/b__servlet_harness.adb -o servlet.cov ) >/dev/null
rm -rf cover
genhtml -o ./cover -t "test coverage" --num-spaces 4 servlet.cov
 
