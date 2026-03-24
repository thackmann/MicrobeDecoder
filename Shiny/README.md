# Run Shiny App for Microbe Decoder

This directory contains scripts and files to
-   Run the Shiny app
-   Update data
-   Test app
-   Build a Docker container image

To deploy the app on the server, see `MicrobeDecoder/Deploy/README.md`.

## 1. Run the Shiny App (RStudio)
### Requirements
-   RStudio

### Launch the App
In RStudio, open

    MicrobeDecoder/Shiny/MicrobeDecoder/app.R

Click **Run App** (or run `shiny::runApp()`).

## 2. Update App Data
### Requirements
-   RStudio

### Step 1: Move Required Database Files

Move `MicrobeDecoder/Database/database.zip` to `MicrobeDecoder/Shiny/MicrobeDecoder/data/database/database.zip`.  

Similarly, move `MicrobeDecoder/Database/gene_functions/gene_functions_database.zip` to `MicrobeDecoder/Shiny/MicrobeDecoder/data/gene_functions/gene_functions_database.zip`

### Step 2: Run Preprocessing Script

In RStudio, open and run:
    MicrobeDecoder/Shiny/MicrobeDecoder/preprocessing/dataPreprocessing.R


## 4. Build Docker Container
### Requirements
-   Docker

### Step 1: Build Image
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

### Step 2: Test Container
In Powershell or Ubuntu, run
``` bash
docker run --rm -p 3838:3838 tjhackmann/microbedecoder:latest
```

In browser (e.g., Chrome), open

    http://localhost:3838/

### Step 3: Push Image to Docker Hub
In Ubuntu, run
``` bash
docker login
docker push tjhackmann/microbedecoder:latest
```