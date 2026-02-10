RUN su -c "git clone https://aur.archlinux.org/swift-bin.git /home/builduser/swift" builduser

RUN su -c "cd /home/builduser/swift && makepkg -si --noconfirm --needed --noprogressbar" builduser

#precompile
RUN git clone https://github.com/jinyus/related_post_gen.git /tmp/repo && cd /tmp/repo && ./run.sh swift
