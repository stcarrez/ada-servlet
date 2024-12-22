NAME=servletada
VERSION=1.7.1

DIST_DIR=ada-servlet-$(VERSION)
DIST_FILE=ada-servlet-$(VERSION).tar.gz

MAKE_ARGS += -XSERVLET_BUILD=$(BUILD)

-include Makefile.conf

HAVE_AWS?=yes
HAVE_EWS?=yes

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

DEFAULT_ADA_PROJECT_PATH=$(SRC_ROOT)

ifeq ($(HAVE_AWS),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/aws
endif

ifeq ($(HAVE_EWS),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/ews
endif

DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/unit:$(ADA_PROJECT_PATH)

# Build executables for all mains defined by the project.
build-test:: lib-setup
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

# Build and run the unit tests
test:	build samples runtest

runtest:
	-DIR=`pwd`; \
	export LD_LIBRARY_PATH="$$DIR/lib/$(NAME)/relocatable:$$DIR/lib/$(NAME)/relocatable:$$LD_LIBRARY_PATH"; \
	export PATH="$$DIR/lib/$(NAME)/relocatable:$$DIR/lib/$(NAME)unit/relocatable:$$PATH"; \
	bin/servlet_harness -l $(NAME): -xml servlet-aunit.xml -config test.properties

samples:
ifeq ($(HAVE_AWS),yes)
	cd samples/aws && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)
endif
ifeq ($(HAVE_EWS),yes)
	cd samples/ews && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)
endif

$(eval $(call ada_library,servletada,.))
$(eval $(call ada_library,servletada_unit,unit))

ifeq ($(HAVE_AWS),yes)
$(eval $(call ada_library,servletada_aws,aws))
endif

ifeq ($(HAVE_EWS),yes)
$(eval $(call ada_library,servletada_ews,ews))
endif

# $(eval $(call ada_library,servletada_all))

$(eval $(call alire_publish,.,se/servletada,servletada-$(VERSION).toml))
$(eval $(call alire_publish,unit,se/servletada_unit,servletada_unit-$(VERSION).toml))

ifeq ($(HAVE_AWS),yes)
$(eval $(call alire_publish,ews,se/servletada_ews,servletada_ews-$(VERSION).toml))
endif

ifeq ($(HAVE_EWS),yes)
$(eval $(call alire_publish,aws,se/servletada_aws,servletada_aws-$(VERSION).toml))
endif

setup::
	echo "HAVE_AWS=$(HAVE_AWS)" >> Makefile.conf
	echo "HAVE_EWS=$(HAVE_EWS)" >> Makefile.conf

.PHONY: samples
