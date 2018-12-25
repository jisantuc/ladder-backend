-- rambler up

ALTER TABLE matchups ADD COLUMN date timestamp with time zone;
UPDATE matchups SET date = matches.start_time FROM matches where matches.matchup = matchups.id;

-- rambler down

ALTER TABLE matchups DROP COLUMN date;
