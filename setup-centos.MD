# Setup Centos Stream 9

## Some common stuff

```shell
sudo dnf install epel-release
sudo dnf upgrade
sudo dnf install tmux git snapd
sudo systemctl enable --now snapd.socket
sudo ln -s /var/lib/snapd/snap /snap
sudo snap install emacs --classic
sudo dnf install gcc-toolset-12 # check of latest gcc toolset version on https://pkgs.org/search/?q=gcc
sudo dnf install clang clang-tools-extra llvm
```

## Golang

Install g
```shell
curl -sSL https://git.io/g-install | sh -s
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
