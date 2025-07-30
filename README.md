# Community Waiting List Regional Analysis
## NHS South West Intelligence and Insights Team

### About the Project

This study investigates strategies for eliminating community waiting lists exceeding 52 weeks within Community Health Services (CHS) and also exploring the potential for achieving 92% target at 18 weeks as an aspiration to bring community waits in line with more established elective care waiting lists. Utilising data from CHS SitReps, it analyses key metrics of load and pressure at both provider and service levels. Queuing theory is applied to current waiting times to forecast future performance and determine the additional capacity needed to achieve national targets and future aspirations. Furthermore, the analysis quantifies the ongoing capacity required to maintain a steady state on the waiting list, preventing future backlogs. This research provides actionable insights for optimising resource allocation and improving patient access to community health services.

_**Note:** Only public or fake data are shared in this repository._

### Project Stucture

- The main code is found in the root of the repository (see Usage below for more information)

### Built With

[R Studio](RStudio Team (2020). RStudio: Integrated Development for R. RStudio, PBC, Boston, MA URL http://www.rstudio.com/.)  
[R Statistical Software](  R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.)

- library('tidyverse')
- library('janitor')
- library('NHSRwaitinglist')
- library('gt')
- library('gtExtras')
- library('geomtextpath')
- library('forecast')
- library('cli')
- library('quarto')

### Getting Started

#### Installation

To get a local copy up and running follow these simple steps.

Clone the repo

### Usage
Data is drawn from NHSE Udal data warehouse.

This is dataset 456 Community Health Services SitRep

Similar data data for this code is can ve obtained obtain from NHS futures, unfortunately it is in a different format.

https://future.nhs.uk/CommunityHealthServices/view?objectId=16189616

The report runs by NHS England region to show regional and local ICB comparisons.

You can select the region that the report runs against at the start of the code. 

The main engine of the analysis is based on the NHSR  Waiting list project.

Walton N, Dray M, Smith T, Mainey C (2025). NHSRwaitinglist: Waiting List Metrics Using Queuing Theory. R package version 0.1.1, https://nhs-r-community.github.io/NHSRwaitinglist/.


#### Outputs
The report produces an interactive quarto document.

#### Datasets
Data from this code is taken from UDAL NHSE Data warehouse.  If you have access you will need to run report in UDAL enviorment.

It could be altered to run from publically available data.

### Roadmap

See the {LINK TO REPO ISSUES} for a list of proposed features (and known issues).

### Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

_See [CONTRIBUTING.md](./CONTRIBUTING.md) for detailed guidance._

### License

Unless stated otherwise, the codebase is released under [the MIT Licence][mit].
This covers both the codebase and any sample code in the documentation.

_See [LICENSE](./LICENSE) for more information._

The documentation is [© Crown copyright][copyright] and available under the terms
of the [Open Government 3.0][ogl] licence.

[mit]: LICENCE
[copyright]: http://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/
[ogl]: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

### Contact

Simon Wellesley-Miller

To find out more  get in touch at [Simon.Wellesley-Miller](mailto:simon.wellesley-miller@nhs.net).




