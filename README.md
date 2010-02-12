# Circumflex — lightweight Scala-based Web framework and ORM

## 1. Prerequisites

* Java 6 SDK
* Apache Maven 2.1 or later
* Git

## 2. Build from sources

Clone Circumflex repository:

    git clone git://github.com/inca/circumflex.git

Build with Maven:

    cd circumflex
    mvn clean install

## 3. Create new project

Create a new project from Circumflex Archetype:

    cd /path/to/my/projects
    mvn archetype:generate

and choose `circumflex-archetype` from list.

## 4. Observe bundled documentation

    cd circumflex/circumflex-docs
    mvn jetty:run

and point your browser to http://localhost:8180.

