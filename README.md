# Christmas Calendars 2020

![all solutions run without crashing](https://github.com/christianfosli/christmas-calendars-2020/workflows/all%20solutions%20run%20without%20crashing/badge.svg)

My solutions for code calendars Christmas 2020.

* [Advent of Code](https://adventofcode.com/2020)

* [Knowit Julekalender](https://julekalender.knowit.no/)

---

I'm mostly doing these in fsharp, to get familiar with the language.

`.fsx` files are fsharp interactive files.
Run them with

```console
dotnet fsi <filename>
```

This requires the dotnet sdk.
Alternatively you could use docker:

```console
docker run --rm -it -v "$(pwd):/app" \
	mcr.microsoft.com/dotnet/sdk:5.0 \
	/bin/bash -c "cd /app && dotnet fsi <filename>"
```

I'll include a Dockerfile where I think it makes sense.
That makes running with docker a bit simpler:

```console
docker build -t dayX .
docker run --rm dayX
```
