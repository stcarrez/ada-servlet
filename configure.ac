dnl Autoconf configure script for Ada Servlet
dnl Copyright (C) 2009 - 2021 Stephane Carrez
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

AC_INIT
AC_CONFIG_SRCDIR([src/servlet.ads])

# Current release settings
SERVLET_MAJOR_VERSION=1
SERVLET_MINOR_VERSION=6
SERVLET_MICRO_VERSION=0
SERVLET_VERSION=$SERVLET_MAJOR_VERSION.$SERVLET_MINOR_VERSION.$SERVLET_MICRO_VERSION

# Set the version number of Ada Server Faces
AC_SUBST(SERVLET_VERSION)
AC_SUBST(SERVLET_MAJOR_VERSION)
AC_SUBST(SERVLET_MINOR_VERSION)
AC_SUBST(SERVLET_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(servletada)
AM_GNAT_LIBRARY_SETUP(servletada_aws)

AM_GNAT_FIND_ADA_UTIL([Ada Servlet])
AM_GNAT_FIND_ADA_EL([Ada Servlet])
AM_GNAT_FIND_ADA_SECURITY([Ada Servlet])

SERVLET_HAVE_EWS=no
WITH_SERVLET_EWS=''
SERVER_EWS=''
AM_GNAT_FIND_PROJECT([ews],[Embedded Web Server],[ews],
 [https://github.com/simonjwright/ews],
 [],
 [
   EWS_DIR=${ac_cv_gnat_project_dir_ews}
   SERVLET_HAVE_EWS=yes
   WITH_SERVLET_EWS="with \"servletada_ews\";";
   SERVER_EWS="\"ews\""
 ])
AC_SUBST(EWS_DIR)
AC_SUBST(SERVLET_HAVE_EWS)
AC_SUBST(WITH_SERVLET_EWS)

AM_GNAT_CHECK_AWS(
  [
    SERVLET_HAVE_AWS=no
    SERVLET_SERVER=none
    WITH_SERVER=
    SERVER_AWS=''
  ], [
    SERVLET_SERVER=aws
    SERVLET_HAVE_AWS=yes
    WITH_SERVER=$ac_cv_gnat_project_with_aws
    WITH_SERVLET_AWS="with \"servletada_aws\";";
    SERVER_AWS="\"aws\""
  ])
AC_SUBST(SERVLET_SERVER)
AC_SUBST(SERVLET_HAVE_AWS)
AC_SUBST(WITH_SERVER)
AC_SUBST(WITH_SERVLET_AWS)
AM_GNAT_AWS_VERSION

AC_MSG_CHECKING([using AWS http server])
SERVLET_AWS_VERSION=1
case $AWS_VERSION in
  *2019*|*202*|20.0|22.0)
     SERVLET_AWS_VERSION=2
     ;;

  3.3.2|*2017*|*2018*)
     SERVLET_AWS_VERSION=1
     ;;

  *)
     SERVLET_AWS_VERSION=2
     ;;
esac
AC_MSG_RESULT(${SERVLET_AWS_VERSION})
AC_SUBST(SERVLET_AWS_VERSION)

if test x"$SERVER_EWS" != x && test x"$SERVER_AWS" != x; then
  SERVLET_SERVER_TYPES=",$SERVER_EWS,$SERVER_AWS"
else
  if test x"$SERVER_EWS" != x; then
    SERVLET_SERVER_TYPES=",$SERVER_EWS"
  fi
  if test x"$SERVER_AWS" != x; then
    SERVLET_SERVER_TYPES=",$SERVER_AWS"
  fi
fi

AC_SUBST(SERVLET_SERVER_TYPES)

AC_CONFIG_FILES([Makefile.conf
servletada.gpr
servletada_aws.gpr
servletada_ews.gpr
servletada_all.gpr
servletada_tests.gpr
servletada_unit.gpr
samples.gpr
])
AC_OUTPUT

