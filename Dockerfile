FROM haskell:8.4.3

RUN export PATH=$(stack path --local-bin):$PATH
RUN apt-get update
RUN apt-get install -y libpq-dev

RUN mkdir -p /app/user
WORKDIR /app/user
COPY stack.yaml .
COPY *.cabal ./
RUN stack build --dependencies-only

COPY . /app/user

RUN stack install

CMD flora-api
