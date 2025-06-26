# Reproducing the results

This repository contains all code, data, and configuration needed to reproduce the analysis for the article  
*[Linking Trait Items of Self-Control to Broader Conceptualizations of Self-Control Using Machine Learning](https://doi.org/10.31234/osf.io/3jsy8_v1)*

### Step 1: Install prerequisites

Make sure the following software is installed on your system:

- [Git](https://git-scm.com/) 
- [Docker](https://www.docker.com/get-started)
- [GNU Make](https://www.gnu.org/software/make/)

### Step 2: Clone the repository 

Open your system terminal (e.g., Terminal app on macOS or Git Bash on Windows) and run:

```
git clone https://github.com/anabelbue/predicting_selfcontrol.git
cd predicting_selfcontrol
```

### Step 3: Run the full pipeline

1. Make sure Docker is running in the background
2. Open the R project by double-clicking the predicting_selfcontrol.Rproj file (or open RStudio and load the .Rproj manually)
3. In the RStudio Terminal tab, type:

```
make all
```

This will:

- Set up the entire R environment and install packages automatically via Docker (using the [`repro`](https://github.com/aaronpeikert/repro) package)
- Run the entire analysis pipeline
- Create all outputs in the appropriate folders:

```
ML results/
EFA results/
tables/
plots/
```

Please note that in the default version, time-intensive computational steps are excluded (i.e., they will not be run again, but the already generated output saved in the repository will be used).  
This concerns the elastic net models in the script `03_item_selection.R`. To also reproduce these models, simply change `run_elastic_net_models` from `FALSE` to `TRUE` in the section of that script. 
