# Run Tests for Plumber Server
# This script tests the Plumber server for the Shiny app.  To use, open server.R 
# in another instance of R first, then run this script.  
# Requirements
# -File for server at server.R
# Author:  Timothy Hackmann
# Date:  17 Apr 27

BASE_URL <- "http://localhost:8000"

run_curl <- function(cmd) {
  cat("\n", cmd, "\n", sep = "")
  system(cmd)
  cat("\n")
}

# =============================================================================
# Status
# =============================================================================
cat("=== Status ===\n")
run_curl(paste0('curl ', BASE_URL, '/status'))


# =============================================================================
# Predict traits from taxonomy
# =============================================================================
cat("=== Compute: Taxonomy ===\n")

writeLines('{
  "query_taxa": {
    "NCBI Domain": ["Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria"],
    "NCBI Phylum": ["Bacillota", "Bacteroidota", "Spirochaetota", "Bacillota", "Actinomycetota"],
    "NCBI Class": ["Clostridia", "Bacteroidia", "Spirochaetia", "Clostridia", "Actinomycetes"],
    "NCBI Order": ["Eubacteriales", "Bacteroidales", "Spirochaetales", "Lachnospirales", "Mycobacteriales"],
    "NCBI Family": ["Clostridiaceae", "Porphyromonadaceae", "Treponemataceae", "Lachnospiraceae", "Corynebacteriaceae"],
    "NCBI Genus": ["Clostridium", "unclassified", "Treponema", "unclassified", "Corynebacterium"],
    "NCBI Species": ["Clostridium lundense", "unclassified", "Treponema ruminis", "unclassified", "Corynebacterium vitaeruminis"]
  },
  "traits_to_predict": ["Type of metabolism (FAPROTAX)"],
  "ignore_NA": true,
  "simple_names": true,
  "ignore_species": true,
  "system_taxonomy": "NCBI",
  "callback_url": "https://httpbin.org/post"
}', "body.json")

run_curl(paste0('curl -X POST ', BASE_URL, '/compute/taxonomy -H "Content-Type: application/json" -d @body.json'))


# =============================================================================
# Predict traits with metabolic networks
# =============================================================================
cat("=== Compute: Networks (selected_organisms) ===\n")

writeLines('{
  "selected_organisms": ["Escherichia coli"],
  "reference_network": "Fermentation of glucose",
  "substrates": ["D-Glucose"],
  "products": [
    "Acetate", "(S)-Lactate", "(R)-Lactate", "Ethanol", "Succinate",
    "Propanoate", "Butanoic acid", "Formate", "Hydrogen", "CO2"
  ],
  "unbalanced_intermediates": [
    "NAD+", "NADH",
    "ATP", "ADP",
    "Orthophosphate",
    "H2O", "H+", "CO2"
  ],
  "all_subunits": true,
  "callback_url": "https://httpbin.org/post"
}', "body.json")

run_curl(paste0('curl -X POST ', BASE_URL, '/compute/networks -H "Content-Type: application/json" -d @body.json'))


cat("=== Compute: Networks (gene_functions) ===\n")

writeLines('{
    "gene_functions": {
      "gene_oid": [2897346301, 2897343589, 2897344361, 2897343589],
      "Genome ID": [2897341779, 2897341779, 2897341779, 2897341779],
      "Genome Name": [
        "Escherichia coli ATCC 11775",
        "Escherichia coli ATCC 11775",
        "Escherichia coli ATCC 11775",
        "Escherichia coli ATCC 11775"
      ],
      "KO": [
        "KO:K00016 - LDH, ldh; L-lactate dehydrogenase [EC:1.1.1.27]",
        "KO:K03777 - dld; D-lactate dehydrogenase (quinone) [EC:1.1.5.12]",
        "KO:K03778 - ldhA; D-lactate dehydrogenase [EC:1.1.1.28]",
        "KO:K03777 - dld; D-lactate dehydrogenase (quinone) [EC:1.1.5.12]"
      ]
    },
    "reference_network": "Fermentation of glucose",
    "substrates": ["Pyruvate"],
    "products": ["(S)-Lactate", "(R)-Lactate"],
    "unbalanced_intermediates": ["NAD+", "NADH", "H+"],
    "all_subunits": true,
    "callback_url": "https://httpbin.org/post"
}', "body.json")

run_curl(paste0('curl -X POST ', BASE_URL, '/compute/networks -H "Content-Type: application/json" -d @body.json'))


# =============================================================================
# Predict traits with machine learning
# =============================================================================
cat("=== Compute: ML (selected_organisms) ===\n")

writeLines('{
  "selected_organisms": ["Escherichia coli"],
  "model_names": ["Fermentation (type of metabolism)"],
  "callback_url": "https://httpbin.org/post"
}', "body.json")

run_curl(paste0('curl -X POST ', BASE_URL, '/compute/ml -H "Content-Type: application/json" -d @body.json'))


cat("=== Compute: ML (gene_functions) ===\n")

writeLines('{
  "gene_functions": {
    "gene_oid": [2897346301, 2897343589, 2897344361, 2897343589],
    "Genome ID": [2897341779, 2897341779, 2897341779, 2897341779],
    "Genome Name": [
      "Escherichia coli ATCC 11775",
      "Escherichia coli ATCC 11775",
      "Escherichia coli ATCC 11775",
      "Escherichia coli ATCC 11775"
    ],
    "KO": [
      "KO:K00016 - LDH, ldh; L-lactate dehydrogenase [EC:1.1.1.27]",
      "KO:K03777 - dld; D-lactate dehydrogenase (quinone) [EC:1.1.5.12]",
      "KO:K03778 - ldhA; D-lactate dehydrogenase [EC:1.1.1.28]",
      "KO:K03777 - dld; D-lactate dehydrogenase (quinone) [EC:1.1.5.12]"
    ]
  },
  "model_names": ["Anaerobe (oxygen tolerance)"],
  "callback_url": "https://httpbin.org/post"
}', "body.json")

run_curl(paste0('curl -X POST ', BASE_URL, '/compute/ml -H "Content-Type: application/json" -d @body.json'))


# =============================================================================
# Parameters
# =============================================================================
cat("=== Parameters: Taxonomy ===\n")
run_curl(paste0('curl ', BASE_URL, '/parameters/taxonomy'))

cat("=== Parameters: Networks (Fermentation of glucose) ===\n")
run_curl(paste0('curl -G ', BASE_URL, '/parameters/networks --data-urlencode "selected_reaction=Fermentation of glucose"'))

cat("=== Parameters: Networks (Methanogenesis) ===\n")
run_curl(paste0('curl -G ', BASE_URL, '/parameters/networks --data-urlencode "selected_reaction=Methanogenesis"'))

cat("=== Parameters: ML ===\n")
run_curl(paste0('curl ', BASE_URL, '/parameters/ml'))

# Clean up
file.remove("body.json")