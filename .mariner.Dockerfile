# For best cache performance, keep the version tag in sync with the bblts in your stack.yaml
FROM artprod.dev.bloomberg.com/haskell/bblts:14.12.1
COPY . /build/
WORKDIR /build/

RUN stack build
RUN stack test
