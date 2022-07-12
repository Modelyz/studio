FROM debian:11 AS build

ARG WSS
ENV WSS $WSS
WORKDIR /srv
ADD https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz elm.gz
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        cabal-install \
        curl \
        gettext-base \
        gzip \
        libghc-aeson-dev \
        libghc-http-types-dev \
        libghc-optparse-applicative-dev \
        libghc-scientific-dev \
        libghc-unordered-containers-dev \
        libghc-uuid-dev \
        libghc-uuid-types-dev \
        libghc-wai-dev \
        libghc-wai-websockets-dev \
        libghc-warp-dev \
        libghc-warp-tls-dev \
        libghc-websockets-dev \
        npm \
        rsync \
        uglifyjs \
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
