# README #

### What is this repository for? ###

This is the database for the Savage Worlds Game Aide application

### How do I get set up? ###

* build: docker build --tag thejimbarrows/swga_datastore --rm .
* run:  docker run --publish 5432:5432 --name datastore thejimbarrows/swga_datastore:latest
* run pgadmin:
* docker run --detach --publish 5050:5050 --name datastore-pgadmin --link datastore:db thajeztah/pgadmin4
