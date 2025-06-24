
FROM eclipse-temurin:17-jdk-jammy AS build-openmole-sources

MAINTAINER Romain Reuillon <romain.reuillon@iscpif.fr>, Sebastien Rey-Coyrehourcq <sebastien.rey-coyrehourcq@univ-rouen.fr> Jonathan Passerat-Palmbach <j.passerat-palmbach@imperial.ac.uk>

RUN apt-get update && apt-get install --no-install-recommends -y gnupg curl

RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
RUN chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg

RUN curl -SLO https://deb.nodesource.com/nsolid_setup_deb.sh
RUN chmod 500 nsolid_setup_deb.sh
RUN ./nsolid_setup_deb.sh 18

RUN apt-get update && apt-get install --no-install-recommends -y gnupg libgnutls30 ntp apt-transport-https ca-certificates git git-lfs curl sbt python3 python3-pycurl bash tar gzip ca-certificates-java openssh-client nodejs tree
RUN update-ca-certificates


WORKDIR /home/root

RUN mkdir -p .ssh && \
    chmod 0700 .ssh

COPY scripts/config .ssh/config

RUN git clone https://gitlab.openmole.org/openmole/openmole.git

WORKDIR /home/root/openmole

COPY scripts/compile.sh .
RUN ["sh", "compile.sh"]

FROM eclipse-temurin:17-jdk-jammy AS openmole

RUN curl -SLO https://deb.nodesource.com/nsolid_setup_deb.sh
RUN chmod 500 nsolid_setup_deb.sh
RUN ./nsolid_setup_deb.sh 18

RUN apt-get update && apt-get install --no-install-recommends -y wget apt-transport-https gnupg ca-certificates gosu tar gzip sudo locales runc cryptsetup fuse fuse2fs libfuse-dev libglib2.0-dev squashfs-tools uidmap nodejs

RUN wget https://github.com/sylabs/singularity/releases/download/v4.1.2/singularity-ce_4.1.2-focal_amd64.deb
RUN dpkg -i singularity-ce_4.1.2-focal_amd64.deb

ARG GID
ARG UID

RUN addgroup --gid $GID mole && useradd -m -d /home/mole -s /bin/sh -g mole -u $UID mole 

COPY ./scripts/docker-entrypoint.sh /usr/local/bin

RUN chmod +x /usr/local/bin/docker-entrypoint.sh

WORKDIR /home/mole/

COPY --from=build-openmole-sources --chown=mole:mole home/root/openmole/openmole/bin/openmole/target/assemble/ .
RUN chmod +x openmole

EXPOSE 8443
VOLUME /home/mole/workspace

RUN chown mole:mole /home/mole

ENV MEM=4G

ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["sh", "-c", "./openmole --mem ${MEM} --rest --port 8443 --remote --workspace /home/mole/workspace"]
