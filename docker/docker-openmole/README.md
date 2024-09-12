# Docker base image for OpenMOLE

Before any launch, the port, define the uid/gid env used for the build :

```shell script
export UID=$(id -u)
export GID=$(id -g)
```

**WARNING :** This env variables are used during the build process to fix permission of files/folder into named volume, so image generated are dedicated to one user !  

You need to change some options used by docker-compose to build and run this image. Best way was to copy/rename the `.env.example` file in project folder into `.env` : 

```
PORT=8081
VOLNAME=MoleWorkspace_myname
COMPOSE_PROJECT_NAME=myproject
MEM=8G
```

If the named volume `VOLNAME`  don't already exist you need to create it : 

```
docker volume create MoleWorkspace_myname
```

The following command build OpenMOLE, and create a start-to-run docker environment. To use it:

```shell script
docker-compose build --build-arg UID="$(id -u)" --build-arg GID="$(id -g)"
docker-compose up -d
```

If you want to backup or access your mole workspace, docker create a dedicated named volume : 

**Tested with :**
- openmole 10-dev
- openmole 11-dev

