# Ada Servlet

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/servletada.json)](https://alire.ada.dev/crates/servletada)
[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Bionic-Ada-Servlet.svg)](https://jenkins.vacs.fr/job/Bionic-Ada-Servlet/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Bionic-Ada-Servlet.svg)](https://jenkins.vacs.fr/job/Bionic-Ada-Servlet/)
[![codecov](https://codecov.io/gh/stcarrez/ada-servlet/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-servlet)
[![Download](https://img.shields.io/badge/download-1.6.0-brightgreen.svg)](http://download.vacs.fr/ada-servlet/ada-servlet-1.6.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-servlet)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-servlet/1.6.0.svg)

Ada Servlet allows to create web applications using the same pattern
as the Java Servlet (See JSR 154, JSR 315). 

The Ada Servlet library is used by the [Ada Server Faces](https://github.com/stcarrez/ada-asf)
framework and [Ada Web Application](https://github.com/stcarrez/ada-awa)
to provide server web requests.

## Version 1.6.0   - Aug 2022
- Fix #4: Alire servletada_aws GNAT project fails due to missing Naming rule
- Fix #5: The Input_Line_Size_Limit parameter is not taken into account
- Fix #6: GNAT configuration project is not correct to build with debugging
- Fix #7: Constraint error raised when matching empty path routes
- Fix #11: Support for Embedded Web Server
- Fix #12: Support for multiple response types in REST operations

[List all versions](https://github.com/stcarrez/ada-servlet/blob/master/NEWS.md)

## Build

To build Ada Servlet, you will need:

* Ada Util     (https://github.com/stcarrez/ada-util          2.5.0)
* Ada EL       (https://github.com/stcarrez/ada-el            1.8.5)
* Ada Security (https://github.com/stcarrez/ada-security      1.4.1)
* AWS          (https://libre.adacore.com/libre/tools/aws/     2018, 2019)
* XML/Ada      (https://libre.adacore.com/libre/tools/xmlada/  4.4)

Build with the following commands:
```
   ./configure
   make
```

The samples can be built using:
```
   gnatmake -Psamples
```
   
The unit tests are built using:
```
   gnatmake -Ptests
```

And unit tests are executed with:
```
   bin/asf_harness
```

## Documentation

The Ada Server Faces sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/ada-asf/wiki

