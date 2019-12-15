# Ada Servlet

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Servlet.svg)](https://jenkins.vacs.fr/job/Ada-Servlet/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Servlet.svg)](https://jenkins.vacs.fr/job/Ada-Servlet/)
[![Download](https://img.shields.io/badge/download-1.3.0-brightgreen.svg)](http://download.vacs.fr/ada-servlet/ada-servlet-1.3.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-servlet/1.3.0.svg)

Ada Servlet allows to create web applications using the same pattern
as the Java Servlet (See JSR 154, JSR 315). 

## Build

To build Ada Servlet, you will need:

* Ada Util     (https://github.com/stcarrez/ada-util          2.0.0)
* Ada EL       (https://github.com/stcarrez/ada-el            1.7.0)
* Ada Security (https://github.com/stcarrez/ada-security      1.2.1)
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

