## Run Shiny App for Microbe Decoder
These files run the app for Microbe Decoder.  They also update data and test the app.  

To execute, open and run `MicrobeDecoder/Shiny/MicrobeDecoder/app.R` in RStudio.  

To update data for the app, open and run `MicrobeDecoder/Shiny/MicrobeDecoder/preprocessing/dataPreprocessing.R` in RStudio.  Before running, make sure to move `MicrobeDecoder/Database/database.zip` to `MicrobeDecoder/Shiny/MicrobeDecoder/data/database/database.zip`.  Similarly, move `MicrobeDecoder/Database/gene_functions/gene_functions_database.zip` to `MicrobeDecoder/Shiny/MicrobeDecoder/data/gene_functions/gene_functions_database.zip`

To test, open and execute `MicrobeDecoder/Shiny/MicrobeDecoder/tests/runTests.R` in RStudio.