FROM haskell:9.2 AS build
#########################

ENV LASTBUILD 2023021201
ARG WSS
ENV WSS $WSS
WORKDIR /srv
ADD https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz elm.gz
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        gettext-base \
        gzip \
        markdown \
        npm \
        rsync \
        uglifyjs \
    && gunzip elm.gz \
    && chmod +x elm \
    && mv elm /usr/local/bin/

RUN cabal update
COPY back /srv/back/
COPY front /srv/front/
COPY build.sh /srv/
COPY CHANGELOG.md /srv/
RUN ./build.sh optimize check_changelog

FROM debian:bookworm
####################

ENV DEBIAN_FRONTEND noninteractive
ENV LANG C.UTF-8

COPY --from=build /srv/build/ /srv/build/
WORKDIR /srv/build
VOLUME /srv/data
EXPOSE 8080
CMD ["/srv/build/studio","-d", "/srv/build", "-f", "/srv/data/eventstore.txt"]
