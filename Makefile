NAME=servletada
VERSION=1.7.1

DIST_DIR=ada-servlet-$(VERSION)
DIST_FILE=ada-servlet-$(VERSION).tar.gz

MAKE_ARGS += -XSERVLET_BUILD=$(BUILD)

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
ifeq ($(HAVE_AWS),yes)
SHARED_MAKE_ARGS += -XAWS_BUILD=relocatable
endif

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test:: lib-setup
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

# Build and run the unit tests
test:	build runtest

runtest:
	DIR=`pwd`; \
	export LD_LIBRARY_PATH="$$DIR/lib/$(NAME)/relocatable:$$DIR/lib/$(NAME)/relocatable:$$LD_LIBRARY_PATH"; \
	export PATH="$$DIR/lib/$(NAME)/relocatable:$$DIR/lib/$(NAME)unit/relocatable:$$PATH"; \
	bin/servlet_harness -l $(NAME): -xml servlet-aunit.xml -config test.properties

samples:
	cd samples/aws && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)
	cd samples/ews && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

$(eval $(call ada_library,servletada,.))
$(eval $(call ada_library,servletada_aws,aws))
$(eval $(call ada_library,servletada_ews,ews))
$(eval $(call ada_library,servletada_unit,unit))

# $(eval $(call ada_library,servletada_all))

$(eval $(call alire_publish,alire.toml,se/servletada,servletada-$(VERSION).toml))
$(eval $(call alire_publish,unit/alire.toml,servletada_unit-$(VERSION).toml))
$(eval $(call alire_publish,ews/alire.toml,se/servletada_ews,servletada_ews-$(VERSION).toml))
$(eval $(call alire_publish,aws/alire.toml,se/servletada_aws,servletada_aws-$(VERSION).toml))

.PHONY: samples
