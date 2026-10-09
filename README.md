# Measuring Learning in Informative Processes

Robert C. Luskin, Ariel Helfer, and Gaurav Sood

How well does the gain in knowledge scores measure what people learn from campaigns and deliberative forums?

**[Read the manuscript](ms/main.pdf)** · [Editable source](ms/main.Rmd)

## Findings

- **Post-process knowledge can track learning when observed gain does not.** The general model guarantees nonnegative covariance under its monotonicity and independence assumptions, including conditional on true initial knowledge. Conditioning on noisy observed knowledge does not inherit that guarantee.
- **In the retained proportional-learning simulations**, observed gain's median correlation with true learning is about .01 and post-process knowledge's about .49. The filter retains 1,212 proportional, 240 additive and eight catch-up worlds out of 4,000 of each. These counts depend on the simulation grid; they do not rule out catch-up learning. [Simulations](tabs/simulations.csv)
- **Education comparisons depend on adjustment.** For participants above their poll's education median, the pooled observed-gain difference is .017 [−.005, .041]; the post-process difference controlling for baseline is .066 [.049, .085]. These are associations with observed scores, with 95% posterior intervals. [Estimates](tabs/who_learns.csv)
- **Attitude associations depend on the proxy.** Across 129 indices, observed gain has positive coefficients on 50%, post-process knowledge on 59%, and post-process knowledge controlling for baseline on 53%. These descriptive summaries do not establish an absence of learning effects. [Summary](tabs/attitudes_summary.csv)

![Simulations](figs/simulations.png)

## Data

Participant-level knowledge scores and attitude indices for 21 Deliberative Polls come from the corrected [dp-data](https://github.com/soodoku/dp-data) export, pinned to commit `681d08ad72dfb50fa24eccfb837fd48f3db9a700`. The two bundled inputs are checked by SHA-256 on every analysis run. [Data and snapshot instructions](data/README.md) describe the fixed within-poll median education rule.

## Reproduce

R packages are pinned in `renv.lock`; the paper also needs Pandoc and XeLaTeX.

```sh
make restore
make check
```

`make check` runs the simulations and analyses, draws the figures, renders the manuscript, and runs linting and tests. It uses the bundled snapshot and does not require a sibling checkout or network access after dependencies are installed. `make ci-docker` runs the same checks in the standard Rocker R image.

`make snapshot` reconstructs the pinned inputs from `../dp-data` without changing that checkout. Set `DP_DATA_ROOT` to use another local clone. Changing the upstream revision requires reviewing new checksums and regenerating the analyses.

## Layout

| Folder | Contents |
|---|---|
| `data/raw/`, `data/sources.csv` | Pinned upstream inputs and SHA-256 manifest |
| `R/` | Functions: the simulation model, attitude regressions, learner comparisons, figure style |
| `scripts/` | Analyses, figures and pinned input snapshot |
| `tabs/`, `figs/` | Generated tables and figures |
| `ms/` | Manuscript source, bibliography and PDF |
| `tests/` | Checks on the model and the outputs |
