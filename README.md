# **MOLTSA – Molten Salt Thermochemical Data Platform**

**MOLTSA** is an open, web-based R/Shiny application for accessing, visualizing, and analyzing thermochemical data relevant to molten-salt research and CALPHAD workflows. It overlays literature datasets with CALPHAD-modeled properties (e.g., phase equilibria and $\Delta _{mix} H$) and provides utilities that shorten the path from experimental data to validated thermodynamic model inputs.

---

## **Key capabilities**

### **Visualization**

- Interactive pseudo-binary phase diagram viewer with literature/model overlays and export
- Interactive enthalpy of mixing plots with linked metadata tables
- **3D ternary / pseudo-ternary viewer** supporting FactSage® `.fig` surfaces with optional experimental CSV overlays

### **CALPHAD-adjacent utilities**

- **Optimizer-file retrieval** (FactSage® Optimizer-compatible inputs)
- **δ₁₂** parameter calculation for liquid–liquid $\Delta _{mix} H$ analyses/correlations
- **Heat-capacity fitting** (piecewise Maier–Kelley coefficients directly usable in Gibbs-energy minimization codes)
- **DSC pipelines** (temperature calibration, uncertainty propagation, application of the IUPAC zero-rate method where applicable)

### **FAIR-aligned data layer**

- Curated CSV/JSON datasets with embedded metadata (units, composition basis, method, provenance, DOI when available)
- Validation at ingest + unit/basis harmonization designed for CALPHAD-style workflows

---

## **Technology stack (high level)**

- **Framework & packaging:** Shiny + golem (production-grade modular structure; explicit dependencies),
- **Data handling:** data.table, dplyr,
- **Visualization/UI:** ggplot2, plotly, highcharter, DT, rhandsontable, bs4Dash, shinyWidgets, shinythemes, shinycssloaders,
- **Auth (optional):** shinymanager,
- **Communication:** blastula

---

## **Repository structure**

- `app.R` – application entry point (launches the golem app)
- `DESCRIPTION`, `NAMESPACE` – package metadata and dependency declarations
- `R/` – Shiny modules + helpers + compute/services
- `inst/` – internal package resources (assets/templates, as applicable)
- `data/` – packaged data objects
- `dev/` – development scripts (utilities)
- `man/` – documentation generated from roxygen2
- `tests/` – tests (testthat)
- `.Rbuildignore`, `.rscignore`, `.gitignore` – build/deploy/version-control ignores
- `README.md` – readme file for moltsa

---

## **Installation (recommended)**

Because this is a **golem-packaged Shiny app**, the cleanest workflow is to install it like an R package.

### 1) Clone

```
git clone https://github.com/JackAnthonyWilson/MOLTSA.git
cd MOLTSA
```

### 2) Install R dependencies

In R:

```
install.packages(c("devtools", "remotes")) 

# installs Imports from DESCRIPTION devtools::install_deps(dependencies = TRUE)
```

### 3) Run locally
```
shiny::runApp("app.R")`
```

---

## **R package dependencies**

Declared in `DESCRIPTION` under `Imports:`:

- blastula
- bs4Dash
- config
- data.table
- dplyr
- DT
- glue
- golem
- highcharter
- htmlwidgets
- janitor
- keyring
- plotly
- rhandsontable
- shiny
- shinycssloaders
- shinymanager
- shinythemes
- shinyWidgets
- Ternary

---

## **Usage overview

MOLTSA is organized individual modules, including:

- **Phase Diagrams** — pseudo-binary phase equilibria data with literature/model overlays
- **Enthalpy of Mixing** — experimental $\Delta _{mix}H$ data visualization with modeled values
- **Ternary `.fig` Viewer** — interactive 3D phase equilibria viewer and (FactSage® `.fig` + optional experimental CSV overlays)
- **Optimizer Files** — search/filter/download optimizer-ready inputs for FactSage® Optimizer workflows
- **Parametric Descriptors** — δ₁₂ calculator
- **Heat Capacity** — Maier–Kelley parameter fitting for CALPHAD-ready workflows
- **DSC Tools** — calibration, uncertainty propagation, and application of the IUPAC zero-rate extrapolation when multiple ramp rates are provided

---

## **Data schema (summary)**

All experimental and computational datasets conform to a unified schema designed for CALPHAD-style modeling and automated re-optimization workflows:

- **Metadata:** dataset identifiers, chemical system, phase information, authorship, source reference, DOI (where available)
- **Data fields:** temperature, composition (mole fraction), and property-specific quantities
- **Provenance:** method (e.g., DSC/PPMS), processing steps, calibration details

---

## **Data policy and attribution**

MOLTSA stores **extracted numeric datasets and metadata** (CSV/JSON) and does **not** store or redistribute copyrighted full-text content (e.g., PDFs). Attribution is maintained for all datasets (including DOI where available), and takedown/removal is supported upon rights-holder request.

---

## **License**

Released under **GPL-3.0** (see `LICENSE` / `LICENSE.md`).

---

## **Contact**

**Developer:** Jack A. Wilson  
**Email:** jackwilson.mchem@gmail.com