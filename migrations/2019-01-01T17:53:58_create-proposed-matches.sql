-- rambler up

CREATE TABLE proposed_matches (
  id uuid primary key,
  matchup uuid references matchups(id) not null,
  proposed_by uuid references players(id) not null,
  venue uuid references venues(id) not null,
  match_time timestamp with time zone not null,
  accepted boolean not null,
  canceled boolean not null
);

-- rambler down

DROP TABLE proposed_matches;

