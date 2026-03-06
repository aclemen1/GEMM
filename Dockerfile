FROM haskell:9.6 AS build
WORKDIR /opt/gemm
COPY gemm.cabal ./
COPY src/ src/
COPY app/ app/
RUN cabal update && cabal build exe:gemm && cabal install exe:gemm --install-method=copy --installdir=/opt/gemm/bin

FROM debian:bookworm-slim
COPY --from=build /opt/gemm/bin/gemm /usr/local/bin/gemm
CMD ["gemm"]
