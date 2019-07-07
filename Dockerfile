FROM haskell:8.6.5

RUN export PATH=$(stack path --local-bin):$PATH
RUN apt-get update
RUN apt-get install -y libpq-dev

RUN mkdir -p /app/user /app/user/src /app/user/elm /app/user/test /app/user/app
WORKDIR /app/user
COPY stack.yaml ./
COPY *.cabal ./
RUN stack build --dependencies-only

COPY . /app/user

RUN stack install

# Error the build if the tests don't pass
RUN stack test

CMD ["flora-api"]

# TODO: make this part work
# FROM alpine:latest
# RUN apk --no-cache add ca-certificates libpq
# WORKDIR /root/
# COPY --from=0 /root/.local/bin/flora-api .
# CMD ["./flora-api"]
