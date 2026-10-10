.PHONY: restore analysis estimation figures paper manuscript format lint test check ci-docker clean

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

analysis:
	Rscript R/04_run_all.R simulations empirical

estimation: tabs/estimator_comparison.csv

tabs/estimator_comparison.csv: R/00_common.R R/01_simulations.R R/04_run_all.R renv.lock
	Rscript R/04_run_all.R estimation

figures: analysis estimation
	Rscript R/04_run_all.R figures

paper: figures

paper manuscript:
	Rscript R/04_run_all.R manuscript

format:
	Rscript -e 'styler::style_dir("R"); styler::style_dir("tests")'

lint:
	Rscript -e 'l <- unlist(lapply(c("R", "tests"), lintr::lint_dir), recursive = FALSE); print(l); quit(status = as.integer(length(l) > 0))'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

check: paper lint test

ci-docker:
	docker run --rm -v "$(PWD):/project" \
		-v "$${DP_DATA_ROOT:-$(abspath ../dp_data)}:/dp_data:ro" \
		-e DP_DATA_ROOT=/dp_data -w /project rocker/verse:4.6.0 \
		bash -lc "Rscript -e 'install.packages(\"renv\", repos = \"https://cloud.r-project.org\")' && make restore check"

clean:
	Rscript -e 'unlink("build", recursive = TRUE)'
