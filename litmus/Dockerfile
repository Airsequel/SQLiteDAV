# Container that runs the Litmus WebDAV compliance test suite.
# Upstream: https://github.com/notroj/litmus
FROM debian:bookworm-slim

RUN apt-get update \
  && apt-get install --no-install-recommends --yes \
       ca-certificates \
       litmus \
  && rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["litmus"]
