
FROM haskell:9.6.7-slim AS build

WORKDIR /app

COPY flp-fun.cabal cabal.project ./
COPY src ./src
COPY app ./app
COPY test ./test


RUN cabal update
RUN cabal build exe:flp-fun
RUN mkdir -p /out && cp "$(cabal list-bin exe:flp-fun)" /out/flp-fun

FROM build AS test

RUN cabal test

FROM debian:bookworm-slim AS runtime

RUN apt-get update \
    && apt-get install -y --no-install-recommends diffutils libgmp10 libffi8 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /out/flp-fun /usr/local/bin/flp-fun


ENTRYPOINT ["flp-fun"]
