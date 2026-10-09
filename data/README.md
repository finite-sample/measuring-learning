# Data inputs

The analysis uses the maintained 21-poll wide export from
[dp-data at commit f88da7f](https://github.com/soodoku/dp-data/tree/f88da7fa904cde55044dd3dda9fd337ca26cb8bd).
The full commit, upstream paths, schema versions and SHA-256 checksums are in
[`sources.csv`](sources.csv). The local `.tab` files are exact upstream bytes;
Git does not translate their line endings.

| Local file | Upstream file | Rows |
|---|---|---:|
| `raw/polardata.tab` | `output/polardata/polardata.tab` | 5,869 |
| `raw/poll_indices.tab` | `output/polardata/attitude-indices.tab` | 129 |

The maintained export reconstructs the polls from reviewed sources. It includes
corrections to questionnaire presence, item scoring, attitude definitions,
demographics and group membership. It restores two BTP General Election
participants and retains one record per person, unlike the historical deposit's
217 duplicate Primaries records. Missing questionnaires remain missing.
The upstream [issue register](https://github.com/soodoku/dp-data/blob/f88da7fa904cde55044dd3dda9fd337ca26cb8bd/docs/poll-issues.md)
records resolved and unresolved source questions. Those unresolved questions
are not removed by adopting this snapshot.

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

## Reconstruct the snapshot

```sh
make snapshot
DP_DATA_ROOT=/path/to/dp-data make snapshot
```

The snapshot script reads the pinned Git objects, so a newer or dirty sibling
checkout does not change the input. It stages both files and verifies all
checksums before copying them into `data/raw/`. Ordinary `make check` verifies
and reads the bundled files without consulting the sibling repository.

To adopt a future correction, update the revision and reviewed hashes in
`sources.csv`, run `make snapshot`, then `make check`, and document changes in
samples, estimates and claims. Do not update checksums just to silence a failed
verification.
