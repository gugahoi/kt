FROM hairyhenderson/gomplate:v2.2.0 as gomplate

FROM lachlanevenson/k8s-kubectl:v1.7.12 as kubectl

FROM haskell:7 as build
RUN cabal update
COPY --from=gomplate /gomplate /usr/bin/
COPY --from=kubectl /usr/local/bin/kubectl /usr/bin/
WORKDIR /app
COPY ./kt.cabal /app/
RUN cabal install --only-dependencies -j4
COPY . /app/
RUN cabal build --ghc-options '-static -optl-static -optl-pthread'

FROM alpine:3.6
COPY --from=gomplate /gomplate /usr/bin/
COPY --from=kubectl /usr/local/bin/kubectl /usr/bin/
COPY meta /opt/meta
COPY --from=build /app/dist/build/kt/kt /usr/bin/
WORKDIR /app
ENTRYPOINT ["/usr/bin/kt"]
