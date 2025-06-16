# Example commands for Web API of Microbe Decoder
## Status
```
curl https://api.microbe-decoder.org/status
```
## Predict traits from taxonomy
```
curl -X POST https://api.microbe-decoder.org/compute/taxonomy \
  -H "Content-Type: application/json" \
  -d '{
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
  }'
 ```
## Predict traits with metabolic networks
### Using selected_organisms
```
curl -X POST https://api.microbe-decoder.org/compute/networks \
  -H "Content-Type: application/json" \
  -d '{
    "selected_organisms": ["Escherichia coli"],
    "reference_reactions": "Fermentation of glucose",
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
  }'
 ```
 
### Using gene_functions
```
curl -X POST https://api.microbe-decoder.org/compute/networks \
  -H "Content-Type: application/json" \
  -d '{
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
    "reference_reactions": "Fermentation of glucose",
    "substrates": ["Pyruvate"],
    "products": ["(S)-Lactate", "(R)-Lactate"],
    "unbalanced_intermediates": ["NAD+", "NADH", "H+"],
    "all_subunits": true,
    "callback_url": "https://httpbin.org/post"
  }'
```
## Predict traits with machine learning
### Using selected_organisms
```
 curl -X POST https://api.microbe-decoder.org/compute/ml \
  -H "Content-Type: application/json" \
  -d '{
    "selected_organisms": ["Escherichia coli"],
    "model_names": ["Fermentation (type of metabolism)"],
    "callback_url": "https://httpbin.org/post"
  }'
}'
```	 
 ### Using gene_functions
 ```
 curl -X POST https://api.microbe-decoder.org/compute/ml \
  -H "Content-Type: application/json" \
  -d '{
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
  }'
```		
## Parameters
```
curl https://api.microbe-decoder.org/parameters/taxonomy
```
```
curl -G https://api.microbe-decoder.org/parameters/networks \
  --data-urlencode "selected_reaction=Fermentation of glucose"
```
```
curl -G https://api.microbe-decoder.org/parameters/networks \
  --data-urlencode "selected_reaction=Methanogenesis"
```
```
curl https://api.microbe-decoder.org/parameters/ml
```