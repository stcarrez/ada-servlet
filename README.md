# Ada Servlet

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/servletada.json)](https://alire.ada.dev/crates/servletada)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-servlet/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-servlet/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-servlet/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/ada-servlet/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-servlet/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/ada-servlet/summary)
[![Download](https://img.shields.io/badge/download-1.6.0-brightgreen.svg)](http://download.vacs.fr/ada-servlet/ada-servlet-1.6.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-servlet)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-servlet/1.6.0.svg)](Commits)

Ada Servlet allows to create web applications using the same pattern
as the Java Servlet (See JSR 154, JSR 315). 

The Ada Servlet library is used by the [Ada Server Faces](https://gitlab.com/stcarrez/ada-asf)
framework and [Ada Web Application](https://gitlab.com/stcarrez/ada-awa)
to provide server web requests.

## Version 1.7.0   - Aug 2023
  - Feature #14: Monitoring the health of the AWS instance

[List all versions](https://gitlab.com/stcarrez/ada-servlet/blob/master/NEWS.md)

## Build with Alire

```
alr with servletada
alr with servletada_aws
alr with servletada_ews
```

## Build with configure

To build Ada Servlet, you will need:

* Ada Util     (https://gitlab.com/stcarrez/ada-util          2.6.0)
* Ada EL       (https://gitlab.com/stcarrez/ada-el            1.8.5)
* Ada Security (https://gitlab.com/stcarrez/ada-security      1.4.1)
* XML/Ada      (https://libre.adacore.com/libre/tools/xmlada/  23)

For the web server, you have the choice between:

* [AWS](https://github.com/AdaCore/aws)
* [EWS](https://github.com/simonjwright/ews)

Build with the following commands:
```
   ./configure
   make
```

The samples can be built using:
```
   make samples
```
   
The unit tests are built using:
```
   make test
```

## Documentation

The Ada Server Faces sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/ada-asf/wiki

