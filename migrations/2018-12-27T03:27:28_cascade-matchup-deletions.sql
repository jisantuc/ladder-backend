-- rambler up

ALTER TABLE matches DROP CONSTRAINT matches_matchup_fkey;
ALTER TABLE matches
  ADD CONSTRAINT matches_matchup_fkey
  FOREIGN KEY (matchup)
  REFERENCES matchups(id)
  ON DELETE CASCADE;

-- rambler down

ALTER TABLE matches DROP CONSTRAINT matches_matchup_fkey;
ALTER TABLE matches
  ADD CONSTRAINT matches_matchup_fkey
  FOREIGN KEY (matchup)
  REFERENCES matchups(id);
