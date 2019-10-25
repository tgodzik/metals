---
id: maven
title: Maven
---

Maven is one of the most common build tools in the JVM ecosystem and it also
allows for using scala through the
[scala-maven-plugin](https://davidb.github.io/scala-maven-plugin/usage.html).
The [scalor-maven-plugin](https://github.com/random-maven/scalor-maven-plugin)
is not currently supported and requires a new plugin for bloop to be
implemented.

## Automatic installation

The first time you open Metals in a new workspace it prompts you to import the
build. Select "Import build" to start automatic installation.

This will create all the needed Bloop files, however there will be a warning,
since the SemanticDB plugin was not added yet in the automatic import. This will
be added later. Most features should work without it, however some require
SemanticDB files to be provided alongside compiled data. To do that we need a
couple of steps that are explained in the manual installation section.

## Manual installation

For current Metals snapshots all you need to run the manual installation is:

`mvn ch.epfl.scala:maven-bloop_2.10:@BLOOP_VERSION@:bloopInstall -DdownloadSources=true`
