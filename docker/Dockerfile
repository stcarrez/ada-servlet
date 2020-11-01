FROM ciceron/ada-util:latest

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

RUN mkdir -p /usr/src \
   && cd /usr/src \
   && git clone https://github.com/stcarrez/ada-el.git ada-el \
   && git clone https://github.com/stcarrez/ada-security.git ada-security \
   && git clone https://github.com/stcarrez/ada-servlet.git ada-servlet \
   && cd /usr/src/ada-el \
   && ./configure --disable-shared --prefix=/usr \
   && make -s \
   && make -s install \
   && cd /usr/src/ada-security \
   && ./configure --disable-shared --prefix=/usr \
   && make -s \
   && make -s install \
   && cd /usr/src/ada-servlet \
   && ./configure --disable-shared --prefix=/usr \
   && make -s \
   && make -s install

WORKDIR /usr/src
