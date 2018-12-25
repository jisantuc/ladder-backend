-- rambler up

CREATE TYPE day_of_week AS ENUM (
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday',
  'Sunday'
);

CREATE TYPE session AS ENUM (
  'Spring',
  'Summer',
  'Fall'
);

CREATE TABLE venues (
  id uuid primary key,
  name varchar(255) not null,
  phone varchar(10) not null,
  address varchar(255) not null,
  league_nights day_of_week[] not null,
  cost float
);

CREATE TABLE players (
  id uuid primary key,
  email varchar(255) not null,
  first_name varchar(255) not null,
  last_name varchar(255) not null,
  accepting_matches boolean not null
);

CREATE TABLE seasons (
  id uuid primary key,
  year int not null,
  session session not null
);

CREATE TABLE matchups (
  id uuid primary key,
  player1 uuid references players(id) not null,
  player2 uuid references players(id) not null,
  week int not null,
  season uuid references seasons(id) not null
);

CREATE TABLE matches (
  id uuid primary key,
  matchup uuid references matchups(id) not null,
  venue uuid references venues(id) not null,
  start_time timestamp with time zone not null,
  recorded timestamp with time zone not null,
  player1wins int not null,
  player2ins int not null
);

CREATE TABLE ratings (
  id uuid primary key,
  season uuid references seasons(id) not null,
  week int not null,
  rating int not null,
  player uuid references players(id) not null
);

-- rambler down

DROP TABLE ratings;
DROP TABLE matches;
DROP TABLE venues;
DROP TABLE matchups;
DROP TABLE players;
DROP TABLE seasons;

DROP TYPE day_of_week;
DROP TYPE session;
