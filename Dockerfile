FROM haskell:9.6.7 as builder
COPY . .
RUN ls -al
RUN cabal update
RUN cabal build -j4

FROM ubuntu:22.04
COPY --from=builder /dist-newstyle/build/x86_64-linux/ghc-9.6.7/hs-data-service-0.1.0.0/x/hs-user-service/build/hs-user-service/hs-user-service /usr/local/bin/
CMD ["hs-user-service"]