PROJECT := analysisselfcontrol
WORKDIR := $(CURDIR)

# Docker run command 
RUN1 = docker run --rm -v $(WORKDIR):/home/rstudio --user $(shell id -u):$(shell id -g) ghcr.io/anabelbue/predicting_selfcontrol:main Rscript

# list below your targets and their recipes
all:
	Rscript -e "repro::automate()"
	$(RUN1) Scripts/01_functions.R
	$(RUN1) Scripts/02_data_prep.R
	$(RUN1) Scripts/03_item_selection.R
	$(RUN1) Scripts/04_dimension_reduction.R
	$(RUN1) Scripts/05_final_analysis.R
	$(RUN1) Scripts/06_ABCD_coding.R

include .repro/Makefile_Docker
