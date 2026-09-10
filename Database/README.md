## Build Database for Microbe Decoder
These files build the database.  They draw on data from LPSN, Bergey's Manual, BacDive, NCBI, GOLD, and IMG.  They are not called during app execution.

To execute, first open and run assembleDatabase.R in RStudio.  This will output a database file to `MicrobeDecoder/Shiny/MicrobeDecoder/data/database.zip`.  

Next, open and run getGeneFunctions.R in RStudio.  This will output a gene functions file `MicrobeDecoder/Shiny/MicrobeDecoder/data/gene_functions.rds`.