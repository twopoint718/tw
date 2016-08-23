DBEXISTS = $(shell psql --list | grep '\bbiking\b')
ISMIGRATED = $(shell psql biking -Atc 'select version from schema_version')
DBNAME = biking

all: database schema

# Run if DB hasn't been seeded
schema: db/schema.sql database
ifeq ($(ISMIGRATED),)
	psql $(DBNAME) --file=db/schema.sql
endif

# Run if database doesn't exist
database:
ifeq ($(DBEXISTS),)
	createdb $(DBNAME)
endif
