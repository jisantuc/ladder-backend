# ladder

A haskell servant API for a singles pool league where players schedule their own matches and are rated by the Elo algorithm.

## Developing

You'll need:

- a postgresql service running somewhere
- [`rambler`](https://github.com/elwinar/rambler/releases/tag/4.2.0)
- [`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install)

To set up the database, `sudo` to your `postgres` user wherever that is (in a container or on your host or wherever) and run `./scripts/dbinit`. If you want to change the defaults for the database name, user, password, host, or port, you can do that with environment variables (see the [`dbinit`](scripts/dbinit) script).

Then, copy `rambler.json.bak` to `rambler.json`, tell `rambler` where you database is and how to be allowed to talk to it, and `rambler apply`. To create a new migration, you can run `scripts/newmigration some-description-for-it`, which will create a timestamped new migration with the short description you provided in the `migrations` directory.

You're now ready to develop against the db / api, congrats!

## Why isn't this containerized it's 2019

I'm developing in crouton, which means no docker. There might be a docker-compose eventually to help travis out, but so far that hasn't been necessary to run a database and some haskell tests. Maybe it will be eventually, but I'm not expecting it to be. :man_shrugging:

If you don't like putting things on your host and want to keep things containerized, there are nice postgres images around, and `stack` has support for doing everything in docker instead of on your host, but for now I'm prioritizing dev simplicity and targeting a single machine deployment.
