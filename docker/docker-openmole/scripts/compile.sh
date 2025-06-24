#!/bin/sh

(
  git fetch &&
  git submodule init &&
  git submodule update &&
  git lfs fetch &&

  git checkout dev &&

  (cd build-system && sbt clean publishLocal) &&
  (cd libraries && sbt clean publishLocal) &&
  (cd openmole && sbt clean publishLocal) &&
  (cd openmole && sbt "project openmole" assemble)
)
