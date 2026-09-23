# Measuring Learning in Informative Processes

Robert C. Luskin, Ariel Helfer, and Gaurav Sood

How well does the gain in knowledge scores measure what people learn from campaigns and deliberative forums?

**[Read the manuscript](ms/main.pdf)** · [Editable source](ms/main.Rmd)

## Findings

- **Post-process knowledge tracks learning; observed gain may not.** When those who know more learn more, post-process knowledge is always positively correlated with true learning, overall and among people with the same initial knowledge. Observed gain has no such guarantee: questionnaires ask easy items, so the most knowledgeable, who learn the most, have the least room to show it.
- **In processes that look like real Deliberative Polls**, observed gain's median correlation with true learning is about .01 and post-process knowledge's about .49. The polls' observable features rule out worlds in which the less knowledgeable learn more. [Simulations](tabs/simulations.csv)
- **The choice changes answers.** Observed gain says the college-educated learn no more than others; post-process knowledge given initial knowledge says they learn more. [Estimates](tabs/who_learns.csv)
- **No learning proxy predicts moving with the net opinion change** across 128 attitude indices. [Summary](tabs/attitudes_summary.csv)

![Simulations](figs/simulations.png)

## Data

Participant-level knowledge scores and attitude indices for 21 Deliberative Polls, from the replication data for Luskin, Sood, Fishkin and Hahn (2022), [doi:10.7910/DVN/D7G1LO](https://doi.org/10.7910/DVN/D7G1LO), CC0. The files are in `data/raw/` and checked by MD5.

## Reproduce

R packages are pinned in `renv.lock`; the paper also needs Pandoc and XeLaTeX.

```sh
make restore
make check
```

`make check` runs the simulations and analyses, draws the figures, renders the manuscript, and runs linting and tests.

## Layout

| Folder | Contents |
|---|---|
| `data/raw/` | Public inputs |
| `R/` | Functions: the simulation model, attitude regressions, learner comparisons, figure style |
| `scripts/` | `run_all.R` builds `tabs/`; `figures.R` builds `figs/` |
| `tabs/`, `figs/` | Generated tables and figures |
| `ms/` | Manuscript source, bibliography and PDF |
| `tests/` | Checks on the model and the outputs |
