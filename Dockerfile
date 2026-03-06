FROM haskell:9.6 AS build
WORKDIR /opt/emm
COPY emm.cabal ./
COPY src/ src/
COPY app/ app/
RUN cabal update && cabal build exe:emm && cabal install exe:emm --install-method=copy --installdir=/opt/emm/bin

FROM debian:bookworm-slim
COPY --from=build /opt/emm/bin/emm /usr/local/bin/emm
CMD ["emm"]
