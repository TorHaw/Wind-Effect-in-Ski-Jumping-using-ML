SHELL := /bin/bash

.PHONY: help all dirs download scrape analyze clean_results clean_pdfs clean_processed clean_all

PYTHON ?= python3
R ?= Rscript

RAW_DIR := data/raw
PROC_DIR := data/processed
RESULTS_DIR := results

FEMALE_LINKS := $(RAW_DIR)/female_data_links.txt
MALE_LINKS   := $(RAW_DIR)/male_data_links.txt

FEMALE_PDF_DIR := $(RAW_DIR)/female_PDFs
MALE_PDF_DIR   := $(RAW_DIR)/male_PDFs

FEMALE_CSV := $(PROC_DIR)/female_WC_21-25.csv
MALE_CSV   := $(PROC_DIR)/male_WC_21-25.csv

ANALYSIS_R := src/analysis.R
SCRAPER_PY := src/pdf_scraper.py
DL_SCRIPT  := data/download_links.sh

help:
	@echo "Targets:"
	@echo "  make all            Run download -> scrape -> analyze"
	@echo "  make dirs           Create expected folders"
	@echo "  make download       Download PDFs into data/raw/{female_PDFs,male_PDFs}"
	@echo "  make scrape         Parse PDFs into data/processed/*.csv"
	@echo "  make analyze        Run R analysis (writes to results/)"
	@echo "  make clean_results  Remove results/"
	@echo "  make clean_pdfs     Remove downloaded PDFs (keeps folders)"
	@echo "  make clean_processed Remove processed CSVs"
	@echo "  make clean_all      Clean results + PDFs + processed CSVs"

all: download scrape analyze

dirs:
	mkdir -p $(FEMALE_PDF_DIR) $(MALE_PDF_DIR) $(PROC_DIR) $(RESULTS_DIR)

download: dirs
	@if [ ! -f "$(FEMALE_LINKS)" ]; then echo "Missing: $(FEMALE_LINKS)"; exit 1; fi
	@if [ ! -f "$(MALE_LINKS)" ]; then echo "Missing: $(MALE_LINKS)"; exit 1; fi
	bash $(DL_SCRIPT)

scrape: dirs
	$(PYTHON) $(SCRAPER_PY) --both

analyze: dirs $(FEMALE_CSV) $(MALE_CSV)
	$(R) $(ANALYSIS_R)

clean_results:
	rm -rf $(RESULTS_DIR)

clean_pdfs:
	find $(FEMALE_PDF_DIR) -type f \( -iname "*.pdf" \) -delete
	find $(MALE_PDF_DIR)   -type f \( -iname "*.pdf" \) -delete

clean_processed:
	rm -f $(FEMALE_CSV) $(MALE_CSV)

clean_all: clean_results clean_pdfs clean_processed