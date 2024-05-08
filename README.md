# transit-equity-methods

This repository contains all of the data and scripts necessary to reproduce the analysis included in the manuscript ["Advances and Pitfalls in Measuring Transportation Equity"](https://link.springer.com/article/10.1007/s11116-023-10460-7) published in the Springer Journal *Transportation*. The work demonstrates shortcomings inherent in traditional methods of analyzing accessibility inequalities and accessibility poverty. It also operationalizes new methods for capturing the same concepts.

Script descriptions:
- **00-inequality-measures.R**: Contains helper functions used to calculate the inequality measures (sourced by 01-inequality.R). Note that all of these helper functions have now been implemented in the R package [accessibiity](https://github.com/ipeaGIT/accessibility).
- **01-inequality.R**: Calculate all inequality meausres and produce Figures 4-6.
- **02-sufficiency.R**: Calculate all sufficiency/poverty meausres and produce Figures 7-9.
- **03-DCDemographics.R**: Produce the demographic map in Figure 2.
- **04-access-change-map.R**: Produce the access change map shown in Figure 3.