# PPML county-month-tier count models

The outcomes are insured head and endorsement counts in each county-month-tier cell.
The treatment is the official statutory subsidy rate, scaled in 10-percentage-point units.

Base specification:

`E[Y_itb | X] = exp(alpha_it + gamma_ib + beta s_bt)`

where alpha_it is a county-by-month fixed effect and gamma_ib is a county-by-coverage-tier fixed effect.
The tier-seasonality robustness adds coverage-tier by calendar-month fixed effects.
The event-study version interacts event month with the tier-specific July 2019 statutory subsidy-rate change.
Predetermined CME downside-risk controls are not included yet.
