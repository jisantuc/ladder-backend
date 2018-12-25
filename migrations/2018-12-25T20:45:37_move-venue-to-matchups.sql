-- rambler up

ALTER TABLE matchups ADD COLUMN venue uuid REFERENCES venues(id);
UPDATE matchups SET venue = matches.venue FROM matches WHERE matchups.id = matches.matchup;
ALTER TABLE matches DROP COLUMN venue;

-- rambler down

ALTER TABLE matches ADD COLUMN venue uuid REFERENCES venues(id);
UPDATE matches SET venue = matchups.venue FROM matchups WHERE matches.matchup = matchups.id;
ALTER TABLE matchups DROP COLUMN venue;
