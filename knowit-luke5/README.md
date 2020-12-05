# Knowit Julekalender Day 5

I included a Dockerfile and a docker-compose file for this one,
because I'm using F# 5 features, and writing a file.

The docker-compose file maps the current directory to `/app` in the container,
so that the plot file is accessible on the host machine afterwards.

```console
docker-compose up
```
