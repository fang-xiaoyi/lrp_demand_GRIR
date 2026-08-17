# Assessment of advisor-aligned candidate setups

 All models use the official statutory subsidy schedule and study tier allocation conditional on an active LRP county-month. They do not identify producer entry.

## Setup 1: tier seasonality

Specification: county-by-month fixed effects, county-by-tier fixed effects, and tier-by-calendar-month fixed effects. The treatment is the post-July-2019 indicator interacted with the official tier-specific subsidy increase.

Feeder estimates per 10-percentage-point larger subsidy increase:

| Outcome and weighting | Estimate | SE | p-value |
|---|---:|---:|---:|
| Head share, equal weight | 0.012 | 0.012 | 0.319 |
| Head share, volume weight | 0.033 | 0.022 | 0.137 |
| Endorsement share, equal weight | 0.011 | 0.012 | 0.367 |
| Endorsement share, volume weight | 0.010 | 0.009 | 0.287 |

Interpretation: after absorbing local contemporaneous conditions, persistent county-tier preferences, and tier-specific seasonality, the data do not show a statistically distinguishable shift toward tiers receiving larger subsidy increases.

## Setup 2: seasonality plus tier-specific linear trends

This adds tier-specific linear time trends to Setup 1.

| Outcome and weighting | Estimate | SE | p-value |
|---|---:|---:|---:|
| Head share, equal weight | 0.067 | 0.025 | 0.008 |
| Head share, volume weight | 0.062 | 0.033 | 0.063 |
| Endorsement share, equal weight | 0.066 | 0.024 | 0.008 |
| Endorsement share, volume weight | 0.051 | 0.020 | 0.010 |

Interpretation: the positive result appears only after imposing a linear extrapolation of tier trends. The separate monthly event-study diagnostics show strong nonlinear pre-reform movements, so the linear trend is not an adequate description of the counterfactual. This setup is not suitable as a preferred causal specification.

## Setup 3: year-over-year change relative to a placebo year

This setup requires the same county-month to be active in consecutive years. It compares the tier-specific share change from 2018--19 to 2019--20, absorbs county-by-current-month shocks, county-tier differences, and tier-by-calendar-month patterns, and uses the 2018--19 change as the pre-reform comparison.

The feeder overlap sample contains 114 county-month changes from 52 counties. Estimates range from -0.0054 to -0.0007 and all p-values exceed 0.87. Fed cattle has only three overlapping county-month changes in two counties and is not identified with the required fixed effects.

Interpretation: among repeatedly active feeder county-months, the reform-year differential change across tiers is effectively zero relative to the preceding year. The overlap sample is selected and smaller, so this is a conservative robustness design rather than a standalone population estimate.

## Identification and claim discipline

- The strongest internally credible statement currently supported is a null or inconclusive result for differential tier reallocation after the 2019 reform.
- A positive causal claim is not robust to reasonable choices about pre-reform tier dynamics.
- The significant trend-adjusted estimate may be reported only as a sensitivity result showing dependence on linear trend extrapolation.
- All claims must be limited to contract allocation among active LRP purchases. Head or endorsement shares do not measure new producer participation.
- Fed-cattle evidence is too sparse for a separate causal conclusion.

The next design should be motivated by a distinct source of identifying variation, not by additional functional forms chosen to recover statistical significance.
