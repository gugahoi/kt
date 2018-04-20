FROM hairyhenderson/gomplate:v2.4.0 as gomplate

FROM lachlanevenson/k8s-kubectl:v1.9.5 as kubectl

FROM alpine:3.6 as app
RUN apk add -U bash
COPY --from=gomplate /gomplate /usr/bin/
COPY --from=kubectl /usr/local/bin/kubectl /usr/bin/
COPY ./kt /usr/bin/
WORKDIR /app
ENTRYPOINT ["/usr/bin/kt"]
