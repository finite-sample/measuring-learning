.PHONY: restore analysis estimation figures paper manuscript format lint test check ci-docker clean

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

analysis:
	Rscript R/01_analysis.R

estimation: tabs/estimator_comparison.csv

tabs/estimator_comparison.csv: R/irt.R R/estimator_comparison.R R/simulate.R R/02_estimator_comparison.R renv.lock
	Rscript R/02_estimator_comparison.R

figures: analysis estimation
	Rscript R/03_figures.R

paper: figures
	$(MAKE) manuscript

manuscript:
	Rscript -e 'options(tinytex.install_packages = FALSE); rmarkdown::render("ms/main.Rmd", knit_root_dir = getwd(), quiet = TRUE)'

format:
	Rscript -e 'styler::style_dir("R"); styler::style_dir("tests")'

lint:
	Rscript -e 'l <- unlist(lapply(c("R", "tests"), lintr::lint_dir), recursive = FALSE); print(l); quit(status = as.integer(length(l) > 0))'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

check: paper lint test

ci-docker:
	docker run --rm -v "$(PWD):/project" \
		-v "$${DP_DATA_ROOT:-$(abspath ../dp-data)}:/dp-data:ro" \
		-e DP_DATA_ROOT=/dp-data -w /project rocker/verse:4.6.0 \
		bash -lc "Rscript -e 'install.packages(\"renv\", repos = \"https://cloud.r-project.org\")' && make restore check"

clean:
	Rscript -e 'unlink("build", recursive = TRUE)'
