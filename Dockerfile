FROM hairyhenderson/gomplate as gomplate

FROM lachlanevenson/k8s-kubectl:v1.7.12 as kubectl

FROM haskell:8.2
RUN cabal update
RUN cabal install shake
COPY --from=gomplate /gomplate /usr/bin/
COPY --from=kubectl /usr/local/bin/kubectl /usr/bin/
