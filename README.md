# **MOLTSA – Molten Salt Thermochemical Data Platform**

**MOLTSA** is an open, web-based R-Shiny application for accessing, visualizing, and analyzing thermochemical data relevant to molten salt reactor research. It overlays literature datasets with CALPHAD-modeled properties (e.g., phase equilibria and $\Delta _{mix} H$) and provides utilities that shorten the path from experimental data to validated thermodynamic model inputs.

A live development of the application is available at:
http://moltsa.com

---

## **Key capabilities**

### **Visualization**

- Interactive pseudo-binary phase diagram viewer with experimental/model overlays and export functions
- Interactive enthalpy of mixing plots with linked metadata tables
- 3D ternary / pseudo-ternary viewer supporting FactSage® `.fig` files and experimental data overlays

### **CALPHAD-adjacent utilities**

- Optimizer-file retrieval (FactSage Optimizer-compatible)
- δ₁₂ parameter calculation for liquid–liquid $\Delta _{mix} H$ correlations
- Heat capacity fitting (piecewise Maier–Kelley coefficients directly usable in Gibbs free energy minimization codes)
- DSC/thermal analysis tools (temperature calibration, uncertainty propagation, application of the IUPAC zero-rate method)

### **FAIR-aligned data**

- Curated CSV/JSON thermochemical datasets with traceable metadata 
- Validation at ingest + unit/basis harmonization designed for CALPHAD

---

## **Repository structure**

- `app.R` – application entry point (launches the golem app)
- `DESCRIPTION`, `NAMESPACE` – package metadata and dependency declarations
- `R/` – Shiny modules + helpers + compute/services
- `inst/` – internal package resources (assets/templates, as applicable)
- `data/` – packaged data objects
- `dev/` – development scripts (utilities)
- `man/` – documentation generated from roxygen2
- `.Rbuildignore`, `.gitignore` – build/deploy/version-control ignores
- `README.md` – readme file for moltsa

---

## **Local installation**

For most users, the hosted deployment at [moltsa.com](https://moltsa.com) is the preferred access route; local installation is primarily intended for development or offline use. A local build of MOLTSA requires installation of the R programming language and some R packages.

### Windows

#### Prerequisites

Install the following:
- **R** (version 4.3 or later recommended)
- **RStudio** (optional, but helpful)
- **Rtools** for the installed R version
#### Installation steps
1. Clone or download the repository.
2. Open R or RStudio in the project directory.
3. Install required R packages:

```
install.packages(c("remotes", "pkgload"))  
remotes::install_deps()
```


4. Load the application package and launch the app:
```
pkgload::load_all()  
run_app()
```

The app will open in a web browser, or provide the local URL if it does not open automatically.

#### Notes
- If local installation is not desirable, users can access the deployed version of the application directly at moltsa.com.

### Linux

#### 1) Install R
```
sudo apt install r-base
```
#### 2) Install system dependencies
```
sudo apt update
sudo apt install -y \
  build-essential \
  cmake \
  pkg-config \
  libuv1-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev
```

#### 2) Clone repository

```
git clone https://github.com/JackAnthonyWilson/MOLTSA.git
cd MOLTSA
```

#### 3) Install R dependencies and run locally
in R:
```
install.packages(c("pkgload", "remotes"))  
remotes::install_deps()  
pkgload::load_all()  
run_app()
```

If you are running WSL and do not have a browser installed, paste the port directly into your browser in Windows.
i.e., when you see:
```
Listening on http://127.0.0.1:XXXX
```
go to your browser and input
```
http://localhost:XXXX
```

#### Notes
- If local installation is not desirable, users can access the deployed version of the application directly at moltsa.com.
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

## **Usage overview**

MOLTSA is organized individual modules, including:

- Phase Diagrams – pseudo-binary phase equilibria data with literature/model overlays
- Enthalpy of Mixing – experimental $\Delta _{mix}H$ data visualization with modeled values
- Ternary `.fig` Viewer – interactive 3D phase equilibria viewer and (FactSage® `.fig` + optional experimental CSV overlays)
- Optimizer Files – search/filter/download optimizer files 
- Parametric Descriptors – δ₁₂ calculator
- Heat Capacity – Maier–Kelley parameter fitting 
- DSC Tools – calibration, uncertainty propagation, and application of the IUPAC zero-rate method

---

## **Data policy and attribution**

MOLTSA stores extracted numeric datasets and metadata (CSV/JSON) and does not store or redistribute copyrighted full-text content (e.g., PDFs). Attribution is maintained for all datasets (including DOI where available), and takedown/removal is supported upon rights-holder request. The full privacy policy is hosted on a static webpage at [moltsa.com/privacy-policy.html](https://moltsa.com/privacy-policy.html)

---

## **License**

Released under **GPL-3.0** (see `LICENSE` / `LICENSE.md`).

---

## **Contact**

**Developer:** Jack A. Wilson  
**Email:** jackwilson.mchem@gmail.com
