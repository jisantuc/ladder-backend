-- rambler up

CREATE TABLE client_info (
  player uuid references players(id) not null,
  password varchar(255) not null
);

-- rambler down

DROP TABLE client_info;

