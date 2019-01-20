-- rambler up

ALTER TABLE matches ALTER COLUMN player1_wins DROP NOT NULL;
ALTER TABLE matches ALTER COLUMN player2_wins DROP NOT NULL;

-- rambler down

ALTER TABLE matches ALTER COLUMN player1_wins SET DEFAULT 0;
ALTER TABLE matches ALTER COLUMN player1_wins SET NOT NULL;
ALTER TABLE matches ALTER COLUMN player2_wins SET DEFAULT 0;
ALTER TABLE matches ALTER COLUMN player2_wins SET NOT NULL;
