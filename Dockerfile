FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
  mit-scheme \
  ghc \
  gprolog