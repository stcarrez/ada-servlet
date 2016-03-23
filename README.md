# Ada Server Faces

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Server-Faces.svg)](http://jenkins.vacs.fr/job/Ada-Server-Faces/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Server-Faces.svg)](http://jenkins.vacs.fr/job/Ada-Server-Faces/)
[![Download](https://img.shields.io/badge/download-1.1.0-brightgreen.svg)](http://download.vacs.fr/ada-asf/ada-asf-1.1.0.tar.gz)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-asf/1.1.0.svg)

Ada Server Faces allows to create web applications using the same pattern
as the Java Server Faces (See JSR 252, JSR 314 and JSR 344). 

To build ASF, you will need:

* Ada Util     (http://code.google.com/p/ada-util             1.8.0)
* Ada EL       (http://code.google.com/p/ada-el               1.6.0)
* Ada Security (http://code.google.com/p/ada-security         1.1.2)
* AWS          (http://libre.adacore.com/libre/tools/aws/     2.11)
* XML/Ada      (http://libre.adacore.com/libre/tools/xmlada/  4.3)

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

# Documentation

The Ada Server Faces sources as well as a wiki documentation is provided on:

   http://code.google.com/p/ada-asf/

# Licenses

Ada Server Faces integrates the Javascript library jQuery licensed under
MIT or GPL (See http://jquery.org/license/).

Ada Server Faces integrates a generated version of 960 grid system
licensed under MIT or GPL (See http://960.gs/ and http://grids.heroku.com/
for the CSS generator). 