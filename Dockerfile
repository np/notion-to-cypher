# syntax=docker/dockerfile:1

FROM haskell:9.6 AS build
WORKDIR /src
COPY . .
RUN cabal update && \
    cabal install exe:notion-to-memgraph-web \
      --installdir=/usr/local/bin \
      --install-method=copy \
      --overwrite-policy=always

FROM debian:stable-slim
COPY --from=build /usr/local/bin/notion-to-memgraph-web /usr/local/bin/notion-to-memgraph-web
EXPOSE 8080
ENTRYPOINT ["notion-to-memgraph-web"]
