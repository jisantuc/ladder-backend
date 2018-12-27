-- rambler up

ALTER TABLE matches
ADD COLUMN validated bool NOT NULL,
ADD COLUMN submitted_by uuid REFERENCES players(id);

-- rambler down

ALTER TABLE matches
DROP COLUMN validated,
DROP COLUMN submitted_by;

