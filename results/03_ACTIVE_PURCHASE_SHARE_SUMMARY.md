# Active-purchase county-month tier-share design

## Question and sample

This design asks whether active LRP purchases shifted toward fixed coverage tiers that received larger statutory subsidy increases in July 2019. The sample runs from July 2017 through June 2020, ending before the July 2020 reform. Feeder and fed cattle are estimated separately.

Each active county-month has five fixed tier alternatives, including zero-share alternatives. The outcomes are each tier's share of insured head and endorsements within that county-month.

## Specification

The static model includes county-by-month fixed effects, county-by-tier fixed effects, and tier-specific linear time trends. The treatment is the post-July-2019 indicator interacted with the official 2019 statutory subsidy-rate increase, scaled per 10 percentage points. Standard errors are clustered by county. Equal-county-month estimates are primary; estimates weighted by the corresponding county-month volume are sensitivity checks.

The event study uses monthly reform-exposure coefficients with the same fixed effects and tier-specific linear trends. Its pre-reform coefficients test whether a linear trend adequately captures earlier tier reallocation.

## Static estimates

For feeder cattle, a 10-percentage-point larger statutory subsidy increase is associated with:

- a 4.40-percentage-point larger insured-head share in the equal-weight model (SE 2.07 pp, p = 0.035);
- a 6.09-percentage-point larger insured-head share in the volume-weighted model (SE 3.41 pp, p = 0.076);
- a 4.32-percentage-point larger endorsement share in the equal-weight model (SE 2.03 pp, p = 0.034); and
- a 3.38-percentage-point larger endorsement share in the volume-weighted model (SE 1.52 pp, p = 0.028).

Fed-cattle estimates are small and statistically insignificant. That sample contains only 37 active county-months in 15 counties during the window and is not adequate for a main result.

## Identification diagnostic

The feeder-cattle monthly pre-reform coefficients strongly reject the joint null of no differential pre-trends even after including tier-specific linear trends:

- equal-weight insured-head shares: chi-square(23) = 225.95, p < 0.001;
- volume-weighted insured-head shares: chi-square(23) = 123.17, p < 0.001;
- equal-weight endorsement shares: chi-square(23) = 275.04, p < 0.001; and
- volume-weighted endorsement shares: chi-square(23) = 125.53, p < 0.001.

The significant static feeder coefficients therefore depend on extrapolating a restrictive linear tier trend through a period with clear nonlinear pre-reform reallocation. They are suggestive associations, not yet defensible causal estimates of the 2019 reform.

## Bottom line

Changing the outcome from counts to conditional tier shares produces interpretable coefficient magnitudes and directly addresses contract choice among active purchases. It does not, by itself, resolve identification. The remaining issue is not county-month shocks; those are absorbed. It is differential, nonlinear movement across coverage tiers before July 2019.
