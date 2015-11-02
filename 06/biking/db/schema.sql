DROP TABLE IF EXISTS bike_rides;
DROP TABLE IF EXISTS riders;

CREATE TABLE riders (
  id    SERIAL PRIMARY KEY,
  name  VARCHAR NOT NULL,
  email VARCHAR NOT NULL
);

CREATE UNIQUE INDEX email_on_riders ON riders (email);

CREATE TABLE bike_rides (
  id       SERIAL PRIMARY KEY,
  rider_id INTEGER REFERENCES riders (id),
  date     DATE NOT NULL,
  distance REAL NOT NULL
);

-- SEEDS

INSERT INTO riders (name, email)
VALUES
  ('Chris Wilson', 'chris@sencjw.com'),
  ('Pee-wee Herman', 'peewee@example.com');

INSERT INTO bike_rides (rider_id, date, distance) SELECT id, '2015-07-01', 5.
FROM riders WHERE email = 'chris@sencjw.com';

INSERT INTO bike_rides (rider_id, date, distance) SELECT id, '2015-06-11', 5.1
FROM riders WHERE email = 'chris@sencjw.com';

INSERT INTO bike_rides (rider_id, date, distance) SELECT id, '2014-11-12', 2.2
FROM riders WHERE email = 'peewee@example.com';

-- example
/*SELECT name, date, distance
FROM bike_rides
JOIN riders ON riders.id = bike_rides.rider_id
ORDER BY date;*/
