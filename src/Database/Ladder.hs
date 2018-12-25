module Database.Ladder (Handle(..)) where

import Database.PostgreSQL.Simple as Postgres

data Handle = Handle { conn :: Postgres.Connection }
