from haskell:8.0.2

ENV STACK_ROOT=/stack-root

RUN stack config set system-ghc --global true && \
    stack install ghc-mod stylish-haskell

VOLUME /stack-root
