# Ada Servlet

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/servletada.json)](https://alire.ada.dev/crates/servletada)
[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Servlet.svg)](https://jenkins.vacs.fr/job/Ada-Servlet/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Servlet.svg)](https://jenkins.vacs.fr/job/Ada-Servlet/)
[![codecov](https://codecov.io/gh/stcarrez/ada-servlet/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-servlet)
[![Download](https://img.shields.io/badge/download-1.5.1-brightgreen.svg)](http://download.vacs.fr/ada-servlet/ada-servlet-1.5.1.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-servlet)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-servlet/1.5.1.svg)

Ada Servlet allows to create web applications using the same pattern
as the Java Servlet (See JSR 154, JSR 315). 

The Ada Servlet library is used by the [Ada Server Faces](https://github.com/stcarrez/ada-asf)
framework and [Ada Web Application](https://github.com/stcarrez/ada-awa)
to provide server web requests.

## Version 1.5.1   - Feb 2021
- Cleanup the examples
- Fix registration and support of application with an empty registration URI

[List all versions](https://github.com/stcarrez/ada-servlet/blob/master/NEWS.md)

## Build

To build Ada Servlet, you will need:

* Ada Util     (https://github.com/stcarrez/ada-util          2.4.0)
* Ada EL       (https://github.com/stcarrez/ada-el            1.8.2)
* Ada Security (https://github.com/stcarrez/ada-security      1.4.0)
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

