## Build Metabolic Networks for Microbe Decoder
These files build the reference network models.  They draw on data from KEGG.  They are not called during app execution.

To execute, open these files in RStudio
1. `MicrobeDecoder/Networks/keggPathwaysToNetworks.R`
2. `MicrobeDecoder/Networks/otherReactionsToNetworks.R`
3. `MicrobeDecoder/Networks/mergeNetworks.R`
4. `MicrobeDecoder/Networks/configureNetwork.R`

Run in order.  After execution, move `MicrobeDecoder/Networks/mainNetwork/main.csv` to app and zip (forming `MicrobeDecoder/Shiny/MicrobeDecoder/data/reference_networks/main.zip`).