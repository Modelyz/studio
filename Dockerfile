FROM debian:11 AS build

WORKDIR /srv
RUN apt-get update \
    && apt-get install -y --no-install-recommends rsync curl gzip npm uglifyjs cabal-install libghc-aeson-dev libghc-optparse-applicative-dev \
    && curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    && gunzip elm.gz \
    && chmod +x elm \
    && mv elm /usr/local/bin/

COPY back /srv/back/
COPY front /srv/front/
COPY build.sh /srv/
RUN ./build.sh -o


FROM debian:11

ENV DEBIAN_FRONTEND noninteractive
ENV LASTBUILD 2022051201
ENV LANG C.UTF-8

COPY --from=build /srv/build/ /srv/build/
WORKDIR /srv/build
VOLUME /srv/data
EXPOSE 8080
CMD ["/srv/build/studio","-d", "/srv/build", "-f", "/srv/data/eventstore.txt"]
