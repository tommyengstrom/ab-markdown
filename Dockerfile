FROM fpco/stack-build:lts-16
RUN stack setup
RUN apt-get install nodejs
RUN npm -g config set user root
RUN npm install -g elm-test
RUN stack install base aeson containers control-bool deepseq elm-bridge html5-entity QuickCheck quickcheck-arbitrary-adt text transformers unordered-containers uuid random mtl
RUN npm install -g elm
COPY app app
COPY src src
COPY tests tests
COPY extended extended
COPY runTests.sh .
COPY ab-markdown.cabal .
COPY package.yaml .
COPY stack.yaml .
RUN stack test --fast
RUN ./runTests.sh

