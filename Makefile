NAME=servletada

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XSERVLET_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XSERVLET_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XELADA_BUILD=relocatable -XEL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XSECURITYADA_BUILD=relocatable -XSECURITY_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable -XAWS_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTILADA_HTTP_AWS_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTILADA_HTTP_AWS_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_UNIT_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTIL_UNIT_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test:: setup regtests/servlet-testsuite.adb
	$(GNATMAKE) $(GPRFLAGS) -p -Pservletada_tests $(MAKE_ARGS)

# Build and run the unit tests
test:	build-test runtest

runtest:
	DIR=`pwd`; \
	export LD_LIBRARY_PATH="$$DIR/lib/$(NAME)/relocatable:$$DIR/lib/$(NAME)/relocatable:$$LD_LIBRARY_PATH"; \
	export PATH="$$DIR/lib/$(NAME)/relocatable:$$DIR/lib/$(NAME)unit/relocatable:$$PATH"; \
	bin/servlet_harness -xml servlet-aunit.xml -config test.properties

regtests/servlet-testsuite.adb: regtests/servlet-testsuite.gpb Makefile.conf
	gnatprep -DSERVLET_SERVER=$(SERVLET_SERVER) regtests/servlet-testsuite.gpb $@

# Build the coverage data and make a report using lcov and genhtml
coverage:  coverage-init runtest coverage-capture coverage-report

COVERAGE_OPTIONS= \
		 --directory src --directory servletunit --directory regtests \
		 --directory obj --directory obj/servlet/static --directory obj/servletunit/static

coverage-init:
	lcov --no-external --initial --capture $(COVERAGE_OPTIONS) \
		 --output-file servlet-coverage.info

coverage-capture:
	lcov --no-external --capture $(COVERAGE_OPTIONS) \
		 --output-file servlet-coverage.info
	lcov --remove servlet-coverage.info '*.ads' -o servlet-coverage-body.info

coverage-report:
	mkdir -p cov
	genhtml --ignore-errors source servlet-coverage-body.info --legend --title "Ada Server Faces" \
			--output-directory cov

$(eval $(call ada_library,$(NAME)))

ifeq ($(HAVE_AWS),yes)
$(eval $(call ada_library,servletada_aws))
endif

$(eval $(call ada_library,servletada_unit))

