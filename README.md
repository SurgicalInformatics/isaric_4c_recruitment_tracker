COVID19 Clinical Information Network (CO-CIN) / ISARIC-4C Recruitment Tracker for UK, England, Scotland and Wales
==========

* [ISARIC COVID-19 Clincial Research Resources](https://isaric.tghn.org/covid-19-clinical-research-resources/)
* [ISARIC4C - Coronavirus Clinical Characterisation Consortium](https://isaric4c.net/)

These are scripts to pull and prepare data from the above REDCap database. This is an active project and scripts will change, so please always update to the latest version.

## Memory issues

Rendering reports for all nations requires a lot of memory. Although individually small files, the amount of memory Pandoc (error message 127) requires to render these files seems to be inoordinately large and is well described as a Pandoc issue. Recommendation is to deploy on a large instance of RStudio Connect or Shiny Server, with access to at least 16GB RAM and a further 16GB of swap (vRAM) space.

## Caution
### Data security

These are patient-level data that contain disclosive information. Only use in a secure environment and do not hold data on a removable device including laptops. 

### Always check the data

It is the end-users responsibility to understand the processes contained in these scripts, the assumptions that are used, and to check the data created conforms to their expectations. 

### Set environment variable with REDCap API token

**Do not store the REDCap API token as plain text.**

``` r
usethis::edit_r_environ()
# this opens up .Renviron, add your token, e.g. ccp_token = 2F3xxxxxxxxxxxxE0111
# Restart R
```

If using RStudio connect, ccp_token will need to be set as an environment variable.

### `WWW Folder`

**Description**: CSS and Visual Branding

Changes bootstrap theme to ISARIC-4C colours and gives logo.

### `render_site.Rmd`

**Description**: R Flexdashboard markdown file. Knit or render this to make site.

This is passed params, data and is rendered from `07_render_reports.R`. Contains statements to allow parameterised generation of reports by UK country.

### `01_data_pull.R`

**Description**: Pulls data and applies labels to data from CCP.

See section above: `Set environment variable with REDCap API token`. Pulls data and applies labels to data from CCP.

### `02_functions.R`

**Description**: Contains functions required to generate scores

### `03_prep.R`

**Description**: Prepares datasets needed in report

### `04_prep_working.R`

**Description**: Prepares variables needed in report

### `05_dag_locator.R`

**Description**: Looks up location of data access groups

### `06_imd_lookup.R`

**Description**: Deprecated. Included in README to explain number jump.

### `07_source_data.R`

**Description**: Sources scripts and to generate data to go into site markdown file.
