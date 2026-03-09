FROM haskell:9.6 AS build
WORKDIR /opt/gemm
COPY gemm.cabal ./
COPY src/ src/
COPY app/ app/
RUN mkdir -p /opt/gemm/bin && cabal update && cabal build exe:gemm && cp $(cabal list-bin exe:gemm) /opt/gemm/bin/gemm

FROM debian:bookworm-slim
ENV LANG=C.UTF-8
COPY --from=build /opt/gemm/bin/gemm /usr/local/bin/gemm
CMD ["gemm"]
