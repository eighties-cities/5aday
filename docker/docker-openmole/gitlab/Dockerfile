FROM hseeberger/scala-sbt

RUN curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | bash 
RUN apt update && apt install -y git-lfs 

RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g reveal-md




