FROM ubuntu:16.04

ENV DEPS  build-essential git libicu-dev elinks libxml2-dev libxslt1-dev \
          aspell tidy libpcre3 libpcre3-dev tnef tcl locales\
          libmysqlclient-dev software-properties-common \
          jq xpdf antiword docx2txt mysql-client \
          wget curl libjpeg-turbo8 libfreetype6 vim \
          libicu55 fontconfig libx11-6 libxext6 libxrender1 libxcb1 \
          xfonts-base xfonts-75dpi xorg libxrender-dev libodbc1 \
          unixodbc libpq5 htop sharutils zip redis-tools apt-transport-https ca-certificates

RUN set -x 
RUN apt-get -y update \
  && DEBIAN_FRONTEND=noninteractive apt-get install -y  $DEPS \
  && rm -rf /var/lib/apt/lists/*

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN mkdir -p /app/bin
WORKDIR /app

COPY bin/* /app/bin/
COPY dockerbin/* /app/bin/
ENV PATH=$PATH:/app/bin

CMD ["xss-sanitize-mackey"]
