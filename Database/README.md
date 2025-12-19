## Build Database for Microbe Decoder
These files build the database.  They draw on data from LPSN, Bergey's Manual, BacDive, NCBI, GOLD, and IMG.  They are not called during app execution.

To execute, open and run assembleDatabase.R in RStudio.  After execution, move `MicrobeDecoder/Database/database.zip` to `MicrobeDecoder/Shiny/MicrobeDecoder/data/database.zip`.  Repeat by moving `MicrobeDecoder/Database/gene_functions.rds` to `MicrobeDecoder/Shiny/MicrobeDecoder/data/gene_functions.rds`.