# Data inputs

The analysis uses the maintained 21-poll wide export from
[dp-data at commit f88da7f](https://github.com/soodoku/dp-data/tree/f88da7fa904cde55044dd3dda9fd337ca26cb8bd).
The full commit, upstream paths, schema versions and SHA-256 checksums are in
[`sources.csv`](sources.csv). Inputs are read directly from the exports in `../dp-data`. Set `DP_DATA_ROOT`
to use another local clone. No input files are copied into this repository.
The build verifies every input against the recorded SHA-256 checksum and stops
if a file is missing or changed.

| Source | Path within dp-data |
|---|---|
| Participant-level scores | `output/polardata/polardata.tab` |
| Attitude indices | `output/polardata/attitude-indices.tab` |
| Item responses | `output/analysis/analysis_item_responses.parquet` |
| Participant records | `output/analysis/analysis_participants.parquet` |
| Item catalog | `output/analysis/analysis_items.parquet` |

The upstream [issue register](https://github.com/soodoku/dp-data/blob/f88da7fa904cde55044dd3dda9fd337ca26cb8bd/docs/poll-issues.md)
records resolved and unresolved source questions. Those unresolved questions
are not removed by using these exports.

## Education and analysis samples

`education_above_median` is a descriptive alias for the snapshot's `bettered`
field. Upstream defines it using reviewed, poll-specific ordered education:
strictly above the empirical participant median is true; at or below is false.
Missing education stays missing. The reference population comprises unique
historical-polardata participants with observed education, before this paper's
analysis exclusions. Tied categories remain intact, so the groups are not
necessarily equal. This is neither a bachelor's-degree indicator nor a cutoff
recomputed from `educ3`.

Poll calibration features use the same complete pre/post knowledge sample.
Education regressions additionally require education and a discussion-group
ID. Attitude regressions require both attitude scores, both knowledge scores,
and a group ID; singleton groups in that complete-case sample are excluded.
Their baseline peer mean excludes the focal person and is calculated among
the remaining complete cases. Analyses are unweighted. Regression sample sizes
and education splits are regenerated in `tabs/who_learns.csv`.

## Source revision

Use the dp-data revision recorded in `sources.csv`. GitHub Actions checks out
that revision separately, and `make ci-docker` mounts the local dp-data clone
read-only. Neither build changes dp-data.

To adopt a future correction, update the revision and reviewed hashes in
`sources.csv`, run `make check`, and document changes in samples, estimates and
claims. Do not update checksums just to silence a failed verification.

## Disjoint-item validation

Three Parquet inputs from the same pinned revision contain
canonical item responses, participant identities and membership, and the item
catalog. They are recorded in `sources.csv` and verified on every analysis run.
The reader selects `source_dataset == "historical"`, `participant == TRUE`, and
the original selected `t1`/`t2` occasions. These labels have poll-specific timing;
they are not uniformly arrival and exit. No source families are pooled.

The predictor and validation sets contain disjoint matched items, with at least
two items in each. The analysis requires scored responses on every matched item
at both occasions. Upstream scored DK and reviewed blanks remain zero; absent
questionnaires and unresolved responses stay missing. `item_validation_samples.csv`
records complete participants, exclusions, matched items and eligibility by poll.
These cohorts need not reproduce the aggregate score sample exactly because of
the stricter item completeness rule.

Within each of 100 seeded partitions, five respondent folds keep discussion
groups intact. Unknown group IDs use individual fold units. Easiness is ranked
using training-baseline correctness only, with item ID breaking ties. The
baseline-residualized score is fitted using only predictor items in training
respondents. Validation targets use other items in test respondents. Random
partitions use the same train/test folds as easier-item partitions. Folds with
fewer than four test participants or constant scores retain undefined
correlations with an explicit status.

`item_partitions.csv` identifies the items, seeds and sample sizes for every
fold. `item_validation.csv` retains fold-level Pearson and rank correlations for
all four scores. Within each split, summaries take medians over usable folds;
paired differences are computed before taking medians. The median and
5th--95th percentile over splits characterize sensitivity to partitions, not
population confidence intervals. All polls receive equal weight in the
manuscript's across-poll descriptive median. This checks prediction across the
available questionnaire items, not recovery of topic-wide true learning.
