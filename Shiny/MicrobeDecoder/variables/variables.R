# Load Internal Data for Shiny App
# This script defines variables for the app.  These are
# variables used by multiple modules.  
# Author: Timothy Hackmann
# Date: 26 February 25

# Colors for plots
  #Matches those in Powerpoint
  red_color=rgb(red=255, green=0, blue=0, maxColorValue = 255)
  gold_color=rgb(red=255, green=192, blue=0, maxColorValue = 255)
  green_color=rgb(red=0, green=176, blue=80, maxColorValue = 255)
  blue_color=rgb(red=0, green=112, blue=192, maxColorValue = 255)
  purple_color=rgb(red=112, green=48, blue=160, maxColorValue = 255)
  grey_color = rgb(red=127, green=127, blue=127, maxColorValue = 255) 

# Names of variables in database
  taxonomy_var <- c("Phylum", "Class", "Order", "Family", "Genus", 
                         "Species", "Subspecies", "Strain")
  
  database_var <- c("LPSN Page", "Bergey Article", "GTDB ID",
                    "NCBI Taxonomy ID", "GOLD Organism ID", 
                    "GOLD Project ID", "IMG Genome ID", "BacDive ID")
  
  metabolism_var <- c(
    "Type of metabolism (FAPROTAX)", "Type of metabolism (Fermentation Explorer)", 
    "Metabolites produced (BacDive)", "Metabolites utilized (BacDive)",
    "Metabolites produced (Fermentation Explorer)", "Major metabolites produced (Fermentation Explorer)", 
    "Minor metabolites produced (Fermentation Explorer)", 
    "Metabolites utilized (Fermentation Explorer)",
    "Enzyme activity (BacDive)"
  )
  
  physiology_var <- c(
    "Oxygen tolerance (BacDive)", "Pathogenicity (BacDive)", 
    "Indole test (BacDive)", "Voges Proskauer (BacDive)", 
    "Motility (BacDive)", "Antibiotic resistance (BacDive)", 
    "Antibiotic sensitivity (BacDive)"
  )

  growth_var <- c(
  "Temperature for growth in degrees (BacDive)", "Salt for growth in moles per liter (BacDive)", 
  "pH for growth (BacDive)", "Incubation period in days (BacDive)"
  )
    
  morphology_var <- c(
    "Cell shape (BacDive)", "Cell length in microns (BacDive)", "Cell width in microns (BacDive)",
    "Flagellum arrangement (BacDive)", "Gram stain (BacDive)", "Spore formation (BacDive)"
  )
  
  isolation_var <- c(
    "Isolation category 1 (BacDive)", "Isolation category 2 (BacDive)", "Isolation category 3 (BacDive)"
  )
    
# Links to external websites
  url_AnaerobeManual <- shiny::a("Anaerobe Laboratory Manual", href="https://search.worldcat.org/title/anaerobe-laboratory-manual/oclc/2546699", target="_blank")
  url_BacDive <- shiny::a("BacDive", href="https://bacdive.dsmz.de/", target="_blank")
  url_Bergey <- shiny::a("Bergey's Manual of Systematics of Archaea and Bacteria", href="https://onlinelibrary.wiley.com/doi/book/10.1002/9781118960608", target="_blank")
  url_CC <- shiny::a("CC by 4.0 license", href="https://creativecommons.org/licenses/by/4.0/", target="_blank")
  url_DADA2 <- shiny::a("DADA2", href="https://benjjneb.github.io/dada2/", target="_blank")
  url_FAPROTAX <- shiny::a("FAPROTAX", href="https://pages.uoregon.edu/slouca/LoucaLab/archive/FAPROTAX/lib/php/index.php", target="_blank")
  url_FAPROTAX_license <- shiny::a("this license", href="https://pages.uoregon.edu/slouca/LoucaLab/archive/FAPROTAX/lib/php/index.php?section=License", target="_blank")
  url_FermentationExplorer <- shiny::a("Fermentation Explorer", href="https://www.science.org/doi/10.1126/sciadv.adg8687", target="_blank")
  url_fairuse <- shiny::a("fair use", href="https://www.copyright.gov/fair-use/", target="_blank")
  url_fbar <- shiny::a("fbar", href="https://cran.r-project.org/web/packages/fbar/index.html", target="_blank")
  url_GOLD <- shiny::a("GOLD", href="https://gold.jgi.doe.gov/", target="_blank")
  url_GTDB <- shiny::a("GTDB", href="https://gtdb.ecogenomic.org/", target="_blank")
  url_IMG <- shiny::a("IMG/M", href="https://img.jgi.doe.gov/m/", target="_blank")
  url_JGI <- shiny::a("this notice", href="https://jgi.doe.gov/disclaimer/", target="_blank")
  url_KAAS <- shiny::a("KAAS", href="https://www.genome.jp/kegg/kaas/", target="_blank")
  url_LPSN <- shiny::a("LPSN", href="https://www.bacterio.net/", target="_blank")
  url_MIT <- shiny::a("MIT license", href="https://opensource.org/license/mit", target="_blank")
  url_NCBI <- shiny::a("NCBI", href="https://www.ncbi.nlm.nih.gov/taxonomy", target="_blank")
  url_QIIME2 <- shiny::a("QIIME2", href="https://qiime2.org/", target="_blank")
