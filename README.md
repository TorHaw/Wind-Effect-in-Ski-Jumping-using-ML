
# Modeling Wind Effect in Ski Jumping Using Machine Learning

Authors:
MIDN Toren Hawk,
MIDN Travis Hockin,
Dr. Ola Elfmark,
and Dr. Gavin Taylor

---

## Overview

This repository provides a fully reproducible pipeline for:

1. Downloading official FIS Ski Jumping competition result PDFs
2. Parsing PDFs into structured row-per-jump datasets
3. Building processed CSV datasets
4. Running statistical and machine learning analyses
5. Producing publication-grade figures and tables

The project evaluates the effectiveness of FIS wind compensation using both linear modeling and machine learning methods across male and female World Cup competitions.

---

## Repository Structure

```
.
├── CITATION.cff
├── LICENSE
├── Makefile
├── README.md
│
├── data/
│   ├── raw/
│   │   ├── female_data_links.txt
│   │   ├── male_data_links.txt
│   │   ├── female_PDFs/
│   │   └── male_PDFs/
│   │
│   └── processed/
│       ├── female_WC_21-25.csv
│       └── male_WC_21-25.csv
│
├── results/
│   ├── female/
│   │   ├── figures/
│   │   └── tables/
│   └── male/
│       ├── figures/
│       └── tables/
│
└── src/
    ├── analysis.R
    ├── pdf_scraper.py
    ├── Original_Code_With_Comments_Female.Rmd
    └── Original_Code_With_Comments_Male.Rmd
```

---

## Data Flow

```
PDF Links (.txt)
        ↓
download_links.sh
        ↓
data/raw/*_PDFs/
        ↓
pdf_scraper.py
        ↓
data/processed/*.csv
        ↓
analysis.R
        ↓
results/{female,male}/figures
results/{female,male}/tables
```

---

## Requirements

### Python

* Python 3.9+
* pandas
* pdfplumber

Install:

```bash
pip install pandas pdfplumber
```

---

### R

* R 4.0+
* tidyverse
* caret
* randomForest
* mgcv
* ggplot2

Install from R:

```r
install.packages(c(
  "tidyverse",
  "caret",
  "randomForest",
  "mgcv",
  "ggplot2"
))
```

---

## Makefile Workflow (Recommended)

All steps are automated using the `Makefile`.

### Run the Entire Pipeline

```bash
make all
```

This executes:

1. `make download`
2. `make scrape`
3. `make analyze`

---

## Step-by-Step Targets

### Create Required Folders

```bash
make dirs
```

---

### Download PDFs

Reads:

* `data/raw/female_data_links.txt`
* `data/raw/male_data_links.txt`

Downloads to:

* `data/raw/female_PDFs/`
* `data/raw/male_PDFs/`

```bash
make download
```

---

### Scrape PDFs → Build Processed CSVs

Parses all PDFs recursively and writes:

* `data/processed/female_WC_21-25.csv`
* `data/processed/male_WC_21-25.csv`

```bash
make scrape
```

You may also run directly:

```bash
python3 src/pdf_scraper.py --both
```

---

### Run Statistical / ML Analysis

Consumes processed CSVs and generates:

* Publication-quality figures
* Summary performance tables

Outputs written to:

```
results/female/
results/male/
```

Run:

```bash
make analyze
```

Or directly:

```bash
Rscript src/analysis.R
```

---

## Cleaning

Remove generated outputs:

```bash
make clean_results
```

Remove downloaded PDFs (keeps folder structure):

```bash
make clean_pdfs
```

Remove processed CSVs:

```bash
make clean_processed
```

Full reset (keeps link files):

```bash
make clean_all
```

---

## Reproducibility Notes

* All paths are relative to the project root.
* The pipeline assumes execution from the repository root directory.
* The scraper is robust to minor formatting differences in FIS PDFs.
* Each competition produces up to two rows per athlete (one per round).
* Disqualified or incomplete jumps may produce missing values.
* The scraper populates some rows with missing column data. We discared these before anlysis.

---

## Citation

See `CITATION.cff` for proper citation metadata.

---

## License

This project is licensed under the MIT License (see `LICENSE`).

---

## Notes for Reviewers

* The `Original_Code_With_Comments_*.Rmd` files are archival references of earlier development stages.
* The production pipeline is contained in:

  * `src/pdf_scraper.py`
  * `src/analysis.R`
  * `Makefile`

