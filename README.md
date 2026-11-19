<div align="center">

# Ebola Vaccine Effectiveness, Bulape

Reproducible R pipeline for a test-negative study of Ervebo (rVSV-ZEBOV) effectiveness against laboratory-confirmed Ebola virus disease, Democratic Republic of the Congo.

[![R](https://img.shields.io/badge/R-4.6.x-276DC3)](https://www.r-project.org/)
![Status](https://img.shields.io/badge/status-preliminary%20v2.0-orange)
![Data](https://img.shields.io/badge/participant%20data-not%20included-lightgrey)

</div>

<hr>

## Overview

This repository holds the code for the evaluation. The code turns the linked outbreak data into a clean, de-duplicated and de-identified analysis dataset. It then estimates vaccine effectiveness (VE) by several methods and produces the result tables and figures. It contains no participant data.

The design is a test-negative case-control study. Among suspected patients tested by RT-PCR, cases test positive and controls test negative, and the two groups are compared on prior vaccination.

The report and slides are written by the study team from the result tables. The R code does not write them.

## Main finding

| Analysis | VE, % (95% CI) |
| :--- | :--- |
| Crude, any recorded vaccination (3/27 cases vs 96/290 controls vaccinated) | 74.7 (25.4 to 94.1) |
| Adjusted for age, sex and calendar time | 52.5 (-43.6 to 88.0) |
| Matched on fortnight of symptom onset | -0.7 (-330.1 to 76.4) |
| Protocol definition, onset more than 10 days after vaccination (0/24 vs 23/217) | 83.1 (-29.0 to 99.9) |

Most cases fell ill before vaccination began on 13 September 2025, while most controls were tested later, when coverage was higher. Calendar time therefore explains most of the crude effect. With only 3 vaccinated cases, the evaluation is inconclusive for this outbreak. It neither confirms nor rules out protection.

## Quick start

1. Install R 4.6.x and the packages listed under Requirements.
2. Put the input files in place (see Data handling and privacy).
3. Run the whole pipeline from the repository root:

```bash
Rscript Rscripts/run_all.R
```

The pipeline picks its mode by itself:

| Mode | When | What runs |
| :--- | :--- | :--- |
| Full | On the study team's computer, where the private raw inputs exist | Steps 1 to 4. Rebuilds the de-identified dataset from the 414-record linkage export, then analyses it. |
| Analysis only | Anyone with the de-identified dataset in `data/` | Steps 3 and 4 |

Set the environment variable `EVE_MODE=public` to force analysis-only mode. Both modes give identical results.

## Pipeline

| Step | Script | Produces |
| :--- | :--- | :--- |
| 0 | `Rscripts/00_config.R` | packages, folders, shared helpers, coding maps, run mode |
| 1 | `Rscripts/01_import_validate.R` | reads the linkage export and the laboratory register, checks the schema |
| 2 | `Rscripts/02_build_cohort.R` | one row per participant, derived variables, study flow, de-identified dataset |
| 2a | `Rscripts/02a_apply_site_qc.R` | applies the site QC feedback from the dashboard: deletions, corrected values, duplicate merges |
| 2b | `Rscripts/02b_data_dictionary.R` | builds the data dictionary |
| 3 | `Rscripts/03_main_analysis.R` | main VE models, sensitivity analyses, protocol window, mortality, figures F1 to F5 |
| 4 | `Rscripts/04_extended_analysis.R` | descriptive tables, propensity-score weighting, latent class model, subgroups, death and severe disease, co-infection, fortnight-matched analysis, figures F6 to F14 |

`Rscripts/run_all.R` runs the steps in order. Steps 1 and 2 run only in full mode.

## Study population and linkage

Surveillance case-investigation forms (575) were linked to a laboratory register built from six laboratory datasets (1,135 patients). Matching was by name, in four tiers:

| Tier | Rule | Handling |
| :--- | :--- | :--- |
| 1 | Exact full name | Accepted automatically when sex and age agree |
| 2 | Two shared name tokens | Reviewed |
| 3 | Name, sex, and age within 5 years | Reviewed |
| 4 | Partial name and sex | Reviewed |

Laboratory records left without a surveillance form were then reconciled across records, giving 16 cross-record links. The confirmed export had 414 records. From there:

- site QC feedback was applied: 254 of 374 corrected values, 13 merge decisions and 9 deletions checked;
- duplicates were collapsed on a canonical identity key, leaving 392 distinct participants;
- 391 participants had a definitive RT-PCR result: 46 cases and 345 controls;
- the primary analytic sample is 317 (27 cases, 290 controls), after excluding 74 with unknown vaccination status;
- adjusted models use 299, the participants with a symptom-onset date.

## Model formulations

### Notation

For participant $i$:

- $Y_i = 1$ for a case (RT-PCR positive), $Y_i = 0$ for a test-negative control.
- $V_i = 1$ if vaccinated, $V_i = 0$ if not vaccinated.

Covariates:

| Symbol | Variable | Coding |
| :--- | :--- | :--- |
| $\mathbf{A}_i$ | Age group | 0-5 years (reference), 6-17, 18-44, 45+ |
| $S_i$ | Sex | Female (reference), male |
| $T_i$ | Calendar time | Weeks from the earliest symptom onset in the dataset, linear |
| $\mathbf{C}_i$ | Contact with a confirmed or suspected case, 21 days before onset | No (reference), yes, unknown |
| $\mathbf{F}_i$ | Took part in a funeral, 21 days before onset | No (reference), yes, unknown |

Answers of "Other" to the contact question are treated as missing.

### Outcome and exposure

**Outcome.** The episode-defining RT-PCR result is the first definitive specimen taken on or after symptom onset. Sensitivity analyses replace it with the first result, the final result, or any positive result.

**Main exposure.** Any recorded vaccination: $V_i = 1$ if vaccinated, $V_i = 0$ if not. Unknown status is excluded and never imputed.

**Protocol exposure.** Uses the interval from vaccination to symptom onset, $\Delta_i = \text{onset date} - \text{vaccination date}$:

$$
V^{\text{prot}}_i =
\begin{cases}
1 & \text{vaccinated and } \Delta_i > 10 \text{ days} \\
0 & \text{not vaccinated} \\
\text{excluded} & \text{vaccinated } 0 \text{ to } 10 \text{ days before onset, after onset, or with timing unknown}
\end{cases}
$$

Vaccination 0 to 10 days before onset is analysed as a separate exposure against the unvaccinated. A sensitivity analysis counts people vaccinated after onset as unvaccinated.

### Vaccine effectiveness

Every estimate is converted from an odds ratio, with the confidence limits swapped:

$$
\mathrm{VE} = (1 - \mathrm{OR}) \times 100, \qquad
\mathrm{VE}_{\text{lower}} = (1 - \mathrm{OR}_{\text{upper}}) \times 100, \qquad
\mathrm{VE}_{\text{upper}} = (1 - \mathrm{OR}_{\text{lower}}) \times 100
$$

### 1. Crude logistic regression

$$
\operatorname{logit} \Pr(Y_i = 1) = \beta_0 + \beta_1 V_i, \qquad \mathrm{OR} = e^{\beta_1}
$$

The model is fitted by maximum likelihood, with profile-likelihood confidence intervals.

### 2. Firth penalised logistic regression (main estimator)

With only 3 vaccinated cases, ordinary maximum likelihood is unstable, and it fails completely when a cell is zero. Firth's method maximises the penalised log-likelihood

$$
\ell^{*}(\boldsymbol\beta) = \ell(\boldsymbol\beta) + \tfrac{1}{2} \log \left| \mathbf{I}(\boldsymbol\beta) \right|
$$

where $\mathbf{I}(\boldsymbol\beta)$ is the Fisher information matrix. Confidence intervals use the profile penalised likelihood. The fitting function is `logistf`.

### 3. Exact conditional odds ratio

For the 2 x 2 table of vaccination by case status, this gives the conditional maximum-likelihood odds ratio with an exact confidence interval (Fisher). It checks the crude and protocol estimates.

### 4. Adjusted models (Firth)

$$
\begin{aligned}
\text{M1:}\quad & \operatorname{logit} \Pr(Y_i = 1) = \beta_0 + \beta_1 V_i + \boldsymbol\gamma^\top \mathbf{A}_i + \delta S_i + \tau T_i \\
\text{M2:}\quad & \text{M1} + \boldsymbol\kappa^\top \mathbf{C}_i + \boldsymbol\phi^\top \mathbf{F}_i \\
\text{M3:}\quad & \text{M1} + \boldsymbol\lambda^\top \mathbf{L}_i
\end{aligned}
$$

$\mathbf{L}_i$ is the latent exposure class from model 7, with a separate level for people who could not be classified. All three models are complete-case analyses: 299 participants, 27 cases and 272 controls.

### 5. Inverse probability of treatment weighting (IPTW)

**Propensity score.** The probability of being vaccinated, from a logistic model:

$$
e_i = \Pr(V_i = 1 \mid \mathbf{A}_i, S_i, T_i, \mathbf{C}_i, \mathbf{F}_i)
$$

**Weights.** Stabilised weights, where $\bar{V}$ is the overall proportion vaccinated:

$$
w_i = \frac{\bar{V}}{e_i} \text{ if } V_i = 1, \qquad
w_i = \frac{1 - \bar{V}}{1 - e_i} \text{ if } V_i = 0
$$

The weights are truncated at their 1st and 99th percentiles.

**Outcome model.** A weighted logistic regression with robust (HC0 sandwich) standard errors and Wald confidence intervals:

$$
\operatorname{logit} \Pr(Y_i = 1) = \beta_0 + \beta_1 V_i
$$

**Balance check.** Standardised mean differences before and after weighting:

$$
\mathrm{SMD} = \frac{\bar{x}_1 - \bar{x}_0}{\sqrt{(s_1^2 + s_0^2)/2}}
$$

Values below 0.1 are taken as balanced. Calendar time fell from 0.50 before weighting to 0.15 after.

### 6. Conditional logistic regression matched on fortnight of onset

Cases and controls are compared only within the same two-week period of symptom onset, $s$. Fortnights start on Mondays, and only those containing at least one case and one control are kept (116 participants).

$$
\operatorname{logit} \Pr(Y_{is} = 1) = \alpha_s + \beta_1 V_{is}
$$

The stratum intercepts $\alpha_s$ are removed by conditioning, so each case is compared only with controls tested at the same time. The fitting function is `survival::clogit`, with Wald confidence intervals.

A second approach restricts the analysis to onset on or after 13 September 2025, when vaccination was under way. In that subset it refits the crude Firth model (2) and the adjusted model M1 (4).

### 7. Latent class model of exposure risk

There are seven binary exposure items from the 21 days before onset:

- contact with a case;
- being on a contact list;
- taking part in a funeral;
- attending a burial;
- travel;
- contact with animals;
- visiting a traditional healer.

Each item is coded $y_{ij} = 1$ for yes and $0$ for no or other, and is missing if unknown. With $K$ classes, class sizes $\pi_k$ and item probabilities $\theta_{kj}$, the likelihood over the items a person answered, $O_i$, is:

$$
\Pr(\mathbf{y}_i) = \sum_{k=1}^{K} \pi_k \prod_{j \in O_i} \theta_{kj}^{\,y_{ij}} (1 - \theta_{kj})^{1 - y_{ij}}
$$

**Fitting.**

- Only participants who answered at least 3 items are included (351).
- Estimation is by the EM algorithm, with 30 random starts for each $K = 1, \dots, 4$.
- The chosen model has the lowest $\mathrm{BIC} = -2\ell + p \log n$, with $p = (K-1) + KJ$, among solutions whose smallest class holds at least 5% of participants.
- Each person is assigned to their most probable class.

**Result.** Two classes fitted best (entropy 0.95). The higher-exposure class (16%) is dominated by funeral and burial attendance.

### 8. Subgroup analyses and interaction tests

Within each subgroup $g$, crude VE is estimated by Firth regression. Effect modification is tested with a penalised likelihood-ratio test comparing two models:

$$
\begin{aligned}
\text{without interaction:}\quad & \operatorname{logit} \Pr(Y_i = 1) = \beta_0 + \beta_1 V_i + \boldsymbol\gamma_g \\
\text{with interaction:}\quad & \operatorname{logit} \Pr(Y_i = 1) = \beta_0 + \beta_1 V_i + \boldsymbol\gamma_g + \boldsymbol\delta_g V_i
\end{aligned}
$$

The protocol threshold for effect modification is $p < 0.01$.

**Subgroups:**

- age (0-17, 18-44, 45+);
- sex;
- period of onset (before or from 13 September 2025);
- contact with a case;
- funeral participation;
- hospitalisation;
- latent exposure class;
- linkage tier.

### 9. Death and severe disease

**VE against severe disease (test-negative).** The outcome becomes $Y^{\text{fatal}}_i = 1$ for cases who died and $0$ for test-negative controls; other cases are excluded. Hospitalised disease is defined the same way. Both use Firth regression, and the fatal model is also adjusted for age and sex.

**VE against death among confirmed cases (cohort, protocol Primary 2).** The risk ratio of death in vaccinated against unvaccinated cases, plus a Firth logistic model:

$$
\operatorname{logit} \Pr(\text{death}_i = 1) = \beta_0 + \beta_1 V_i, \qquad
\mathrm{VE}_{\text{death}} = (1 - e^{\beta_1}) \times 100
$$

A multivariable Firth model among confirmed cases adds age group (0-17, 18-44, 45+) and sex.

**Viral load.** The lowest GeneXpert Ct value is compared by vaccination status and by survival, using Wilcoxon rank-sum tests.

### 10. Descriptive statistics and other checks

**Descriptive tables.** Categorical characteristics are compared with chi-squared or Fisher exact tests, and continuous ones with Wilcoxon tests.

**Co-infection.** Malaria is identified from laboratory records (BioFire panel, comments, remarks, treatment and signs) and described. The crude VE is re-estimated without the malaria case.

**Sensitivity analyses.** These exclude three groups in turn:

- cross-record (Tier 3-4) links;
- specimens taken after death;
- malaria-positive participants.

**Reproducibility.** The random seed is 20261118.

## Data handling and privacy

Participant data never goes into this repository.

**Raw inputs.** The raw files identify people. They stay in a private folder outside the repository, by default a folder named `EVE_VE_PRIVATE_identifiers` next to it:

```
EVE_VE_PRIVATE_identifiers/
└── raw_inputs/
    ├── EVE_raw_linkage_export_414_records.csv
    ├── EVE_raw_lab_register_wide.csv
    └── site_qc_feedback/          record_corrections.json, merge_decisions.json, ...
```

These files are:

- the confirmed linkage export (414 records);
- the triangulated laboratory register;
- the site QC decisions.

To use another location, set `EVE_PRIVATE_DIR`, or copy `config.local.R.example` to `config.local.R` and set the paths there. `config.local.R` is gitignored.

**Outputs of a full run.** Every file that names a person goes to the private folder: the identifiable coded dataset, the study-number crosswalk, and the QC log by laboratory ID. The shareable de-identified dataset is written to `data/`. Aggregate tables and figures go to `Outputs/`.

**De-identified dataset.** This file holds no names or IDs. It still contains exact dates, age, sex and health area, so the study team shares it only under the study's data-sharing terms. It is not stored here.

## Requirements

R 4.6.x and the following packages:

```r
install.packages(c("dplyr", "ggplot2", "writexl", "logistf", "sandwich", "tidyr", "survival", "jsonlite"))
```

`jsonlite` is needed only in full mode, to read the site QC feedback.

## Outputs

**`data/`**
- `EVE_VE_analysis_dataset_deidentified.csv` and `.xlsx`: one row per participant, study number only, with the data dictionary on a second sheet.

**`Outputs/`**
- `cohort_flow.csv`: study-flow counts.
- `qc_summary.csv` and `qc_breakdown.csv`: what QC feedback was applied or not, as counts.
- `analysis_tables.xlsx` and `extended_tables.xlsx`: every result table, one per sheet.
- `figures/`: F1 to F14.

**Data dictionary**
- `data_dictionary/DATA_DICTIONARY.md` and `.csv`: every variable, including which ones are kept out of the shared dataset.

## Site QC feedback

In full mode, the corrections, merges and deletions recorded in the dashboard are applied automatically. A correction to a surveillance field is carried to every record of the same person, so it does not create false duplicates. Items that cannot be matched to exactly one surveillance-laboratory pair are listed in the private QC log for manual resolution.

Dates written as `08-Sep-25` are read as 2025. Before this fix they were read as the year 25 AD.

## Status

Preliminary report, version 2.0 (18 November 2026), for internal review. All estimates are provisional.
