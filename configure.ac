dnl Autoconf configure script for Ada Servlet
dnl Copyright (C) 2009 - 2019 Stephane Carrez
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

# Current release settings
SERVLET_MAJOR_VERSION=1
SERVLET_MINOR_VERSION=3
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

AM_GNAT_CHECK_AWS(
  [
    SERVLET_HAVE_AWS=no
    SERVLET_SERVER=none
    WITH_SERVER=
  ], [
    SERVLET_SERVER=aws
    SERVLET_HAVE_AWS=yes
    WITH_SERVER=$ac_cv_gnat_project_with_aws
    WITH_SERVLET_AWS="with \"servletada_aws\";";
  ])
AC_SUBST(SERVLET_SERVER)
AC_SUBST(SERVLET_HAVE_AWS)
AC_SUBST(WITH_SERVER)
AC_SUBST(WITH_SERVLET_AWS)
AM_GNAT_AWS_VERSION

AC_OUTPUT(
Makefile.conf
servletada.gpr
servletada_aws.gpr
servletada_all.gpr
servletada_tests.gpr
servletada_unit.gpr
samples.gpr
)

