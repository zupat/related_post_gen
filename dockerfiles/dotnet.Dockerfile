# install dotnet
RUN su -c "git clone https://aur.archlinux.org/dotnet-core-bin.git /home/builduser/dotnet" builduser

RUN su -c "cd /home/builduser/dotnet && makepkg -si --noconfirm --needed --noprogressbar" builduser

#precompile
RUN git clone https://github.com/jinyus/related_post_gen.git /tmp/repo && cd /tmp/repo && ./run.sh csharp && ./run.sh fsharp
