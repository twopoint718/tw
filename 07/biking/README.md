# Haskell Biking app

This application is a demonstration of some of techniques that Haskell employs to make web development nicer.
Some of these include, strongly-typed parameters, forms, and HTML output.
All of these techniques together help prevent things like code injections (be that HTML or SQL).

## Installation

Build the application with `stack`:

    stack build

This application expects there to be a PostgreSQL database containing a few tables:

    createdb biking
    psql --dbname=biking --file=db/schema.sql

This will load the schema in `db/schema.sql` and create the tables that the application will use.

You can then start the application with:

    stack exec biking

The environment variables `DBHOST`, `DBNAME`, `DBUSER`, and `DBPASS` can be used to configure the application if you don't want to run with the defaults:

    DBNAME=biking_dev stack exec biking

Would launch the application using the database `biking_dev`.
