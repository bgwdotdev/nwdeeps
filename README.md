# nwdeeps

A dps meter and cooldown tracker for neverwinter nights, specifically targeted at the Arelith server.

## Dependencies

* erlang v28

## Usage

You must turn on all logging inside neverwinter nights, this can be done by:

* Options
* Game Options
* Game
* Under "Logging", enable "Game Log Chat All"

Find your log folder, for example:

```
/home/bgw/.var/app/com.valvesoftware.Steam/.local/share/Steam/steamapps/compatdata/704450/pfx/drive_c/users/steamuser/Documents/Neverwinter Nights/logs
```

Set this folder to the NWDEEPS_PATH environment variable

```sh
export NWDEEPS_PATH=/path/to/logs
```

Run the program

```
gleam run
# or if pre-bundled
./entrypoint.sh run
```

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
