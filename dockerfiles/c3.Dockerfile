# install c3
RUN wget 'https://github.com/c3lang/c3c/releases/download/v0.7.9/c3-linux.tar.gz' -O /home/builduser/c3.tar.gz

RUN tar zxvf /home/builduser/c3.tar.gz -C /home/builduser

# comilper looks for libxml2.so.2
RUN cp /usr/lib/libxml2.so /usr/lib/libxml2.so.2

ENV PATH="$PATH:/home/builduser/c3"
