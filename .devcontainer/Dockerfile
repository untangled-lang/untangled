FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04

RUN apt-get update && apt-get upgrade -y
# Basic tools
RUN apt-get install -y sudo curl wget git zsh micro
RUN apt-get install -y build-essential cmake
RUN apt-get install -y gdb
# LLVM
RUN apt-get install -y software-properties-common apt-utils
RUN curl https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic main"
RUN apt-get update
RUN apt-get install -y llvm-14-dev clang-14 libclang-14-dev lldb-14

# Set up a non-root user
RUN adduser --disabled-password --gecos '' tangler
RUN echo "tangler:tangler" | chpasswd
RUN adduser tangler sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER tangler
RUN cp /etc/zsh/newuser.zshrc.recommended ~/.zshrc
WORKDIR /home/tangler

# Make llvm 14 the default for this user
RUN echo 'export PATH=/usr/lib/llvm-14/bin:$PATH' >> ~/.zshrc

# Build debugir
RUN git clone https://github.com/vaivaswatha/debugir
RUN sudo apt-get install zlib1g-dev
RUN cd debugir && mkdir build && cd build && cmake -DCMAKE_BUILD_TYPE=Release ../ && cmake --build . --verbose
RUN mkdir -p ~/.local/bin
RUN ln -s /home/tangler/debugir/build/debugir ~/.local/bin/debugir
RUN echo 'export PATH=$PATH:~/.local/bin' >> ~/.zshrc
