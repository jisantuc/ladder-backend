-- rambler up

ALTER TABLE venues ADD COLUMN is_active boolean NOT NULL DEFAULT 'true';

-- rambler down

ALTER TABLE venues DROP COLUMN is_active;
