# Run Shiny App for Microbe Decoder

This directory contains scripts and files to
-   Run the Shiny app
-   Update data
-   Test app
-   Build a Docker container image

To deploy the app on the server, see `MicrobeDecoder/Deploy/README.md`.

## Run the Shiny App (RStudio)
### Requirements
-   RStudio

### Launch the App
In RStudio, open

    MicrobeDecoder/Shiny/MicrobeDecoder/app.R

Click **Run App** (or run `shiny::runApp()`).

## Update App Data
### Requirements
-   RStudio

### Run Preprocessing Script

In RStudio, open and run:
    MicrobeDecoder/Shiny/MicrobeDecoder/preprocessing/dataPreprocessing.R

## Build Docker Container
### Requirements
-   Docker

### Build Image
Start Docker. Then in Powershell, run

```
cd C:\path\to\MicrobeDecoder
```

or in Ubuntu

``` bash
cd /path/to/MicrobeDecoder
```

then run 

``` bash
docker build -f Docker/Dockerfile -t tjhackmann/microbedecoder:latest .
```

### Test Container
In Powershell or Ubuntu, run
``` bash
docker run --rm -p 3838:3838 tjhackmann/microbedecoder:latest
```

In browser (e.g., Chrome), open
    http://localhost:3838/

### Push Image to Docker Hub
In Ubuntu, run
``` bash
docker login
docker push tjhackmann/microbedecoder:latest
```