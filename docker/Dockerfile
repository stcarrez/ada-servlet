FROM ciceron/alire-bookworm:latest

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

RUN apt-get update \
   && apt-get install -y libssl-dev

RUN mkdir -p /usr/src \
   && alr index --update-all \
   && cd /usr/src \
   && git clone https://github.com/stcarrez/ada-servlet.git ada-servlet \
   && cd /usr/src/ada-servlet \
   && make -s setup BUILD=debug PREFIX=/usr HAVE_ALIRE=yes HAVE_EWS=no HAVE_AWS=yes \
   && make -s build \
   && make -s install

WORKDIR /usr/src
