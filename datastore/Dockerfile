FROM postgres:10.2

ENV POSTGRES_DB=swga_database
ENV POSTGRES_USER=swga_database
ENV POSTGRES_PASSWORD=swga_database

RUN apt-get update -qq && \
    apt-get install -y apt-utils postgresql-contrib

ADD sql/*.sql /docker-entrypoint-initdb.d/
