# Setup Centos Stream 9

## Some common stuff

```shell
sudo dnf install -y epel-release
sudo dnf upgrade -y
sudo dnf install -y tmux git snapd
sudo systemctl enable --now snapd.socket
sudo ln -s /var/lib/snapd/snap /snap
sudo snap install emacs --classic --edge
sudo dnf install -y gcc-toolset-12 # check of latest gcc toolset version on https://pkgs.org/search/?q=gcc
sudo dnf install -y clang clang-tools-extra llvm pinentry llvm-toolset llvm-devel
sudo dnf install -y liburing
```

## Golang

Install g

```shell
curl -sSL https://git.io/g-install | sh -s
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
nvm install 18
npm install -g prettier
```

```shell
mkdir -p ~/github.com/fardream
cd ~/github.com/fardream
git clone git@github.com:fardream/profiles.git
git clone git@github.com:fardream/cryto_notes.git
```

```shell
mkdir -p ~/.emacs.d
mkdir -p ~/.gnupg
mkdir -p ~/.bashrc.d
ln -s ~/github.com/fardream/profiles/init.el ~/.emacs.d/
ln -s ~/github.com/fardream/profiles/tmux.conf ~/.tmux.conf
ln -s ~/github.com/fardream/profiles/gpg-agent.conf ~/.gnupg/gpg-agent.conf
ln -s ~/github.com/fardream/profiles/bashrc.sh ~/.bashrc.d/
ln -s ~/github.com/fardream/profiles/gitconfig ~/.gitconfig
```

```shell
go install github.com/bazelbuild/bazelisk@latest
go install github.com/bazelbuild/buildtools/buildifier@latest
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/stringer@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
go install mvdan.cc/gofumpt@latest
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install github.com/planetscale/vtprotobuf/cmd/protoc-gen-go-vtproto@latest
go install golang.org/x/tools/gopls@latest
go install honnef.co/go/tools/cmd/staticcheck@latest
go install github.com/haya14busa/goplay/cmd/goplay@latest
go install github.com/nao1215/gup@latest
go install github.com/go-delve/delve/cmd/dlv@latest
go install github.com/bazelbuild/bazel-gazelle/cmd/gazelle@latest
go install golang.org/x/tools/cmd/godoc@latest
go install golang.org/x/pkgsite/cmd/pkgsite@latest
```

mkl

```shell
tee > /tmp/oneAPI.repo << EOF
[oneAPI]
name=Intel® oneAPI repository
baseurl=https://yum.repos.intel.com/oneapi
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://yum.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
EOF

sudo mv /tmp/oneAPI.repo /etc/yum.repos.d/
```
