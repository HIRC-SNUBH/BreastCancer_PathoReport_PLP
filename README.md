# Breast Cancer Patient-Level Prediction

This repository contains R scripts and configurations used to perform patient-level prediction (PLP) analysis and to generate summary reports. The materials are provided in support of the accompanying research publication.

---

## Repository Structure
```
├── config/ # Database connection settings 
├──covariates # Covariates Concept ID 
├──library # Patient Level Prediction Libraries
├── br_cx_plp_run.r # Primary script for PLP analysis 
├── report_plp_result.r # Script for generating the analysis report 
├── DESCRIPTION # Metadata for R package-style usage (optional) 
└── README.md # This document
```
---

## Execution Guide

### 1. Running the Patient-Level Prediction

To execute the PLP analysis:

1. Modify the database connection details located in the `config/` directory to match the target OMOP CDM instance.
2. Run the primary analysis script using one of the following methods:
   ```bash
   Rscript br_cx_plp_run.r
   ```
Alternatively, the script can be executed interactively within an R environment such as RStudio.

### 2. Generating the Report

To generate the PLP result report:

Place the result files produced by `br_cx_plp_run.r` into the `result/` directory of the current working directory.

Execute the reporting script via:
   ```bash
   Rscript report_plp_result.r
   ```
This script processes the output and produces a summary report.

---

### License
This repository is distributed under the MIT License.
Refer to the LICENSE file for full license terms and conditions.
