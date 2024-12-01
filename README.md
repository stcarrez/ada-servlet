# Ada Servlet

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/servletada.json)](https://alire.ada.dev/crates/servletada)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-servlet/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-servlet/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-servlet/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/ada-servlet/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-servlet/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/ada-servlet/summary)
[![Download](https://img.shields.io/badge/download-1.7.0-brightgreen.svg)](http://download.vacs.fr/ada-servlet/ada-servlet-1.7.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-servlet)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-servlet/1.7.0.svg)](Commits)

Ada Servlet allows to create web applications using the same pattern
as the Java Servlet (See JSR 154, JSR 315). 

The Ada Servlet library is used by the [Ada Server Faces](https://gitlab.com/stcarrez/ada-asf) framework,
the [OpenAPI Ada library](https://github.com/stcarrez/swagger-ada)
and [Ada Web Application](https://gitlab.com/stcarrez/ada-awa)
to provide server web requests.

## Version 1.8.0   - Under development
  - Fix #15: wrong client_secret initialization in OAuth servlet: copy paste error

## Version 1.7.1   - Aug 2024
  - Cleanup build environment to drop configure

[List all versions](https://gitlab.com/stcarrez/ada-servlet/blob/master/NEWS.md)

## Using with Alire

If you are using [Alire](https://alire.ada.dev/) in your project, run the following command
within your [Alire](https://alire.ada.dev/) project to use the library:

```
alr with servletada
```

For the web server, you have the choice between:

* [AWS](https://github.com/AdaCore/aws)
* [EWS](https://github.com/simonjwright/ews)

Choose one of the following crates:

```
alr with servletada_aws
alr with servletada_ews
```

## Using without Alire

If you don't have [Alire](https://alire.ada.dev/) or want to build and install the library
on a specific place, run a `setup` command to configure the build as well as installation
directory.

The `HAVE_ALIRE` configuration allows you to disable the build with [Alire](https://alire.ada.dev/),
the `HAVE_AWS` controls the support for AWS and the `HAVE_EWS` controls the support for EWS.

```
make setup BUILD=debug PREFIX=/build/install HAVE_ALIRE=no HAVE_EWS=no HAVE_AWS=yes
```

Since this build method does not verify that all dependencies are met, make sure that you
have already built and install the following components and they are available to `gprbuild`
through `ADA_PROJECT_PATH` if needed:

* [Ada Security Library](https://gitlab.com/stcarrez/ada-security/)
* [Ada EL Library](https://gitlab.com/stcarrez/ada-el/)
* [Ada Utility Library](https://gitlab.com/stcarrez/ada-util/)

Then build, run the unit tests and install by using:

```
make
make test
make install
```

To use the installed libraries, make sure your `ADA_PROJECT_PATH` contains the directory
where you installed the libraries (configured by the `PREFIX=<path>` option in the setup phase).
The installed GNAT projects are the same as those used when using [Alire](https://alire.ada.dev/).

## Samples

The samples can be built using:

```
make samples
```

or

```
cd samples
alr build
```

## Documentation

The Ada Server Faces sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/ada-asf/wiki

