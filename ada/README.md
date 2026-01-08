# Install
See https://github.com/alire-project/alire/blob/master/doc/getting-started.md

Download latest version of Alire package manager:
```shell
wget https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-x86_64-linux.zip
unzip alr-2.1.0-bin-x86_64-linux.zip
mv bin/alr ~/.local/bin/  # or another place in PATH
alr toolchain --select gnat_native gprbuild  # install build system
```

then in `ada/` root `alr build --release` and then exec `./ada/bin/main`
