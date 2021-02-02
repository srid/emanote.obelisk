Quickly play with emanote using docker:

1. First, copy both `./doc/templates/` and `./doc/style.css` (from emanote Git repo) into your notebook directory. These are files are used to render the HTML & CSS. Emanote does not provide one by itself.
2. Install docker, and run the following command:

```bash
cd /path/to/notebook
docker run --rm -it -p 3000:3000 -v $(pwd):/app ghcr.io/srid/emanote emanote /app
```
