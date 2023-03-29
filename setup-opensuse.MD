# Setup OpenSuse

## Update to tumbleweed.

`sudo bash`

```shell
mkdir /etc/zypp/repos.d/old
mv /etc/zypp/repos.d/*.repo /etc/zypp/repos.d/old

zypper ar -f -c http://download.opensuse.org/tumbleweed/repo/oss repo-oss
zypper ar -f -c http://download.opensuse.org/tumbleweed/repo/non-oss repo-non-oss
zypper ar -f -c http://download.opensuse.org/tumbleweed/repo/debug repo-debug
zypper ar -f -c http://download.opensuse.org/update/tumbleweed/ repo-update

zypper cc -a && zypper ref && zypper dup --allow-vendor-change
```

```shell
sudo zypper install -y gcc gcc-c++ tmux clang clang-tools
```

```shell
sudo zypper addrepo --refresh \
    https://download.opensuse.org/repositories/system:/snappy/openSUSE_Tumbleweed \
    snappy
sudo zypper --gpg-auto-import-keys refresh
sudo zypper dup --from snappy
sudo zypper install snapd
sudo systemctl enable --now snapd
sudo systemctl enable --now snapd.apparmor
sudo snap install emacs --classic --edge
```

```shell
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
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
