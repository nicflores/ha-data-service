FROM haskell:9.12.2 as builder
COPY . .
RUN ls -al
RUN cabal update
RUN cabal build -j4

FROM public.ecr.aws/amazonlinux/amazonlinux:2023
COPY --from=builder /dist-newstyle/build/x86_64-linux/ghc-9.6.7/hs-data-service-0.1.0.0/x/hs-data-service/build/hs-data-service/hs-data-service /usr/local/bin/

CMD ["hs-data-service"]