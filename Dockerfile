FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
ARG BOOLECTOR_VERSION=3.2.1
ARG Z3_VERSION=4.8.8
ARG CVC4_VERSION=1.8

WORKDIR /workspace
RUN apt-get update && apt-get install -y \
    autoconf \
    automake \
    autotools-dev \
    bc \
    bison \
    build-essential \
    clang \
    clang-format \
    cloc \
    cmake \
    curl \
    flex \
    gawk \
    gcc \
    gcc-riscv64-linux-gnu \
    git \
    gperf \
    htop \
    libffi-dev \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libtinfo5 \
    llvm \
    make \
    patchutils \
    pkg-config \
    python3 \
    python3-jinja2 \
    python3-pandas \
    python3-pip \
    python3-scipy \
    python3-venv \
    ripgrep \
    software-properties-common \
    texinfo \
    tmux \
    unzip \
    vim \
    wget \
    zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

# Grisette
RUN curl -sSL https://get.haskellstack.org/ | sh
# RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN apt update && apt install -y libpcre2-dev libpcre3-dev && rm -rf /var/lib/apt/lists/*

# Racket
RUN add-apt-repository -y ppa:plt/racket \
  && apt-get install -y racket && rm -rf /var/lib/apt/lists/*

RUN raco pkg install -D --auto raco-cross
RUN raco cross --version 8.1 --workspace /workspace/rosette-3
RUN raco cross --version 8.1 --workspace /workspace/rosette-4

RUN raco cross --workspace /workspace/rosette-3 pkg install --auto -D csv-reading ansi-color text-table
RUN raco cross --workspace /workspace/rosette-4 pkg install --auto -D csv-reading ansi-color text-table

# Install Boolector with CaDiCal backend
RUN git clone 'https://github.com/boolector/boolector.git' && \
    cd boolector && git checkout ${BOOLECTOR_VERSION} && \
    ./contrib/setup-cadical.sh && \
    ./contrib/setup-btor2tools.sh && \
    ./configure.sh --prefix /opt/boolector/${BOOLECTOR_VERSION}-cadical && cd build && make -j $(nproc) && make install && \
    cd ../.. && rm -rfv ./boolector

# Install Z3
RUN git clone 'https://github.com/Z3Prover/z3.git' && \
    cd z3 && git checkout z3-${Z3_VERSION} && \
    python3 scripts/mk_make.py --prefix /opt/z3/${Z3_VERSION} && \
    cd build && \
    make -j $(nproc) && make install && \
    cd ../.. && rm -rfv z3

RUN mkdir /opt/cvc4 \
  && wget http://cvc4.cs.stanford.edu/downloads/builds/x86_64-linux-opt/cvc4-1.8-x86_64-linux-opt -nv -O /opt/cvc4/cvc4 \
  && chmod +x /opt/cvc4/cvc4

ENV CVC4=/opt/cvc4/cvc4
ENV BOOLECTOR=/opt/boolector/${BOOLECTOR_VERSION}-cadical/bin/boolector
ENV Z3=/opt/z3/${Z3_VERSION}/bin/z3
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib64"

# Rosette

RUN cd /workspace/rosette-3 \
  && git clone https://github.com/sorawee/rosette.git \
  && cd rosette \
  && git checkout 55941b8aa332853731c40ebc4e4bad5eb53513f9 \
  && raco cross --workspace /workspace/rosette-3 pkg install --auto -D

RUN cd /workspace/rosette-4 \
  && git clone https://github.com/sorawee/rosette.git \
  && cd rosette \
  && git checkout 9fefa68ad171be497050230f7a8a65f62a99c5d9 \
  && raco cross --workspace /workspace/rosette-4 pkg install --auto -D

RUN rm -f /workspace/rosette-3/rosette/bin/* \
  && rm -f /workspace/rosette-4/rosette/bin/* \
  && ln -s $CVC4 /workspace/rosette-3/rosette/bin/cvc4 \
  && ln -s $BOOLECTOR /workspace/rosette-3/rosette/bin/boolector \
  && ln -s $Z3 /workspace/rosette-3/rosette/bin/z3 \
  && ln -s $CVC4 /workspace/rosette-4/rosette/bin/cvc4 \
  && ln -s $BOOLECTOR /workspace/rosette-4/rosette/bin/boolector \
  && ln -s $Z3 /workspace/rosette-4/rosette/bin/z3

# The rest
RUN stack install --resolver lts-18.17 \
  bytestring \
  constraints \
  deepseq \
  generic-deriving \
  hashable \
  intern \
  monad-coroutine \
  monad-memo \
  mtl \
  regex-base \
  regex-pcre \
  sbv \
  template-haskell \
  transformers \
  unordered-containers \
  vector-sized \
  vector \
  array \
  hashtables \
  loch-th \
  lens \
  mmorph \
  typerep-map \
  th-lift-instances \
  hspec \
  hspec-junit-formatter \
  QuickCheck \
  utf8-string \
  megaparsec \
  parser-combinators \
  timeit \
  clock \
  formatting \
  either \
  text \
  Glob
  
COPY grisette-haskell /workspace/grisette-haskell
RUN cd grisette-haskell \
  && mkdir -p scripts/solvers \
  && ln -s $CVC4 scripts/solvers/cvc4 \
  && ln -s $BOOLECTOR scripts/solvers/boolector \
  && ln -s $Z3 scripts/solvers/z3 \
  && stack build \
  && cd ..

COPY README.md /workspace/README.md
#COPY diff-testing /workspace/diff-testing
COPY rosette-benchmarks-3 /workspace/rosette-benchmarks-3
COPY rosette-benchmarks-4 /workspace/rosette-benchmarks-4
COPY interface /workspace/interface
COPY perf /workspace/perf
#COPY examples /workspace/examples
COPY .gitignore /workspace/.gitignore

RUN git config --global user.name "Your Name"
RUN git config --global user.email yourname@example.com
RUN git init
RUN git add .
RUN git commit -m "initial content"

# RUN rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["bash"]
