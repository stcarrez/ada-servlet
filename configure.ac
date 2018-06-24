dnl Autoconf configure script for Ada Servlet
dnl Copyright (C) 2009 - 2018 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/servlet.ads)

AC_CANONICAL_SYSTEM
dnl AM_MAINTAINER_MODE

# Current release settings
SERVLET_MAJOR_VERSION=1
SERVLET_MINOR_VERSION=2
SERVLET_MICRO_VERSION=0
SERVLET_VERSION=$SERVLET_MAJOR_VERSION.$SERVLET_MINOR_VERSION.$SERVLET_MICRO_VERSION

AM_GNAT_CHECK_GPRBUILD

# checking for local tools
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S

# Set the version number of Ada Server Faces
AC_SUBST(SERVLET_VERSION)
AC_SUBST(SERVLET_MAJOR_VERSION)
AC_SUBST(SERVLET_MINOR_VERSION)
AC_SUBST(SERVLET_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

SERVLET_LIBDIR="lib"
AC_SUBST(SERVLET_LIBDIR)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)

##########################################
# Check to enable/disable shared library
##########################################
AM_SHARED_LIBRARY_SUPPORT
AM_DISTRIB_SUPPORT
AM_COVERAGE_SUPPORT

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],
  [git@github.com:stcarrez/ada-util.git],
  [Building Ada EL requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_util}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Expression Language library
##########################################
AM_GNAT_FIND_PROJECT([ada-el],[Ada Expression Language Library],[el],
  [git@github.com:stcarrez/ada-el.git],
  [Building Ada Servlet requires the Ada EL Library.],
  [
    EL_DIR=${gnat_project_dir_el}
  ])
AC_SUBST(EL_DIR)

##########################################
# Ada Security library
##########################################
AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
  [git@github.com:stcarrez/ada-security.git],
  [Building Ada Servlet requires the Ada Security Library.],
  [
    SECURITY_DIR=${gnat_project_dir_security}
  ])
AC_SUBST(SECURITY_DIR)

##########################################
# Ada Web Server
##########################################
AM_GNAT_CHECK_AWS(
  [
    SERVLET_HAVE_AWS=no
    SERVLET_SERVER=none
    WITH_SERVER=
  ], [
    SERVLET_SERVER=aws
    SERVLET_HAVE_AWS=yes
    WITH_SERVER="with \"${gnat_project_name}\";";
    WITH_SERVLET_AWS="with \"servlet_aws\";";
  ])
AC_SUBST(SERVLET_SERVER)
AC_SUBST(SERVLET_HAVE_AWS)
AC_SUBST(WITH_SERVER)
AC_SUBST(WITH_SERVLET_AWS)

AM_UTIL_CHECK_INSTALL

AC_OUTPUT(
Makefile servlet.gpr servlet_core.gpr servlet_aws.gpr servlet_tests.gpr samples.gpr servlet_unit.gpr
distrib/servlet.gpr
distrib/servlet_core.gpr
distrib/servlet_aws.gpr
)

