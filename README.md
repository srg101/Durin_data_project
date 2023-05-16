
This is the git repository for the DURIN project financed by the
Norwegian Research Council project. This repository is linked to the
DURIN data paper: xxx et al. (not written yet).

# INTRODUCTION

Predicting how the biosphere will respond to the global climate and
environmental changes, and in turn how these responses will feed back to
and influence the climate and earth system, is a grand and urgent
cross-disciplinary scientific challenge. Despite their broad climatic
and geographic ranges and dominant ecosystem roles across boreal, alpine
and arctic vegetation zones, much remains to be understood about the
critical impact of dwarf-shrubs at different organizational levels; from
physiological responses to species interactions and ecosystem
functioning. Exploring climate-responses and feedbacks across biomes and
habitats, these insights will not only further fundamental knowledge,
but through also integrating the under-represented dwarf-shrubs in earth
system models, develop a more realistic parameterization of terrestrial
processes.

DURINs unique multi-pronged empirical approach builds upon existing
distributed observational systems, field experiments, and
controlled-environment experiments. This enables great flexibility in
exploring broad-scale patterns and context-dependencies in dwarf-shrub
biotic interactions, and responses and feedbacks to climate across
multiple sites. It also allows for detailed quantification of the
underlying mechanisms and processes which can then be scaled to an earth
system feedback perspective. To facilitate this, DURIN will develop a
collaborative workflow facilitating cross-disciplinary knowledge
transfer. DURIN is well-placed to conduct critical fundamental and
applied climate change research through establishing a new research
cluster, drawing together a uniquely-qualified team with complementary
skills and expertise from seven world-class research institutions within
Norway. The project leadership and partners have excellent track records
in relevant fields, and DURIN will provide career opportunities for
early-career researchers.

<div class="figure">

<img src="pics/Circle_figure_Sticker.png" alt="Conceptual diagram illustrating the role of dwarf-shrubs in climate responses and feedbacks. Responses to climate operate via physiological processes (assessed in DURIN WP1), interactions with other organisms (WP2), and ecosystem processes (WP3); and processes at all these levels may impact feedbacks to the climate system (WP4)." width="50%" />
<p class="caption">
Conceptual diagram illustrating the role of dwarf-shrubs in climate
responses and feedbacks. Responses to climate operate via physiological
processes (assessed in DURIN WP1), interactions with other organisms
(WP2), and ecosystem processes (WP3); and processes at all these levels
may impact feedbacks to the climate system (WP4).
</p>

</div>

The overarching objective of the DURIN project is to provide critical
new knowledge about an important but underrepresented plant functional
type in climate-biosphere science; the dwarf-shrubs of the boreal,
alpine, and arctic regions, and simultaneously provide a worked example
of an integrated climate response-feedback research workflow.

The primary objective of the DURIN project is to provide critical new
knowledge about an important but underrepresented plant functional type
in climate-biosphere science; the dwarf-shrubs of the boreal, alpine,
and arctic regions, and simultaneously provide a worked example of an
integrated climate response-feedback research workflow. To that end, the
secondary objectives are to:

- Examine variation in dwarf-shrubs physiological and growth responses
  to climate, and their impacts on microclimate (WP1);

- Map important species interactions, focusing on ericoid mycorrhizae
  (ErM) and quantifying key roles in dwarf-shrub carbon dynamics (WP2);

- Assess how environmental factors and vegetation characteristics impact
  ecosystem functioning and carbon dynamics (WP3);

- Use DURIN WP0-3 outcomes to parameterize dwarf-shrubs as a new plant
  functional type within a terrestrial ecosystem model (WP4);

- Create optimized data workflows using transparent, reproducible Open
  Science practices (WP5).

# METHODS

We have extensive prior data and infrastructure in both systems. Using a
nested design, we will set up blocks in forested and open heathland
systems (Fig. 2b), for five focal dwarf-shrub species (*Calluna
vulgaris*, *Empetrum nigrum*, *Vaccinium myrtillus*, *V. vitis-idea*,
and *Betula nana*). These species are selected to be the dominant
dwarf-shrubs in our region, while also representing wide ranges in
dwarf-shrub leaf size, longevity, leaf economic traits, and mycorrhizal
associations (Fig. 2c). These sites will provide a flexible resource for
both broad-scale and targeted field data collection, sites for field
experiments, and plant material for controlled-environment experiments
(Fig. 2d, study designs detailed in WP0, responses in WP1-WP3). Data
workflow will follow Open Science best practices (WP5), to inform both
analysis of plant-climate relations (WP1-3) and CLM-FATES modelling
(WP4) (Fig. 2e). Scientific outputs include community outreach,
conference proceedings and scientific articles (Fig. 2f). The entire
DURIN project design will be shared as a generalized workflow
highlighting how collaborative, Open Science practices facilitate
cross-disciplinary knowledge transfer between ecologists and Terrestrial
Ecosystem Modellers (Fig. 2g, WP0-5).

<div class="figure">

<img src="pics/Picture 1.jpg" alt="Figure 2 Our dwarf-shrub heath observational system based around two biogeographic gradients; a latitudinal gradient from boreal to arctic heathlands along the Norwegian coastline, and an elevational and continentality gradient from boreal to alpine systems." width="483" />
<p class="caption">
Figure 2 Our dwarf-shrub heath observational system based around two
biogeographic gradients; a latitudinal gradient from boreal to arctic
heathlands along the Norwegian coastline, and an elevational and
continentality gradient from boreal to alpine systems.
</p>

</div>

# DATA MANAGEMENT

## Location of data, metadata and code

The **project description**, an overview of all the **datasets**, and
the **data dictionaries** are in this readme file, available on
[GitHub](https://github.com/Durin-project/Durin_data). The draft for the
data paper is available [here](...). (only available for authors)

The raw and clean **datasets** from this project are stored and
available on [OSF](https://osf.io/f4v9t/).

All R code for the cleaning the raw data is available on
[GitHub](https://github.com/Durin-project/Durin_data).

### Naming conventions used for the data

| Files or variable | Naming convention                                                                                      | Example                         |
|:------------------|:-------------------------------------------------------------------------------------------------------|:--------------------------------|
| Project           | Project name                                                                                           | DURIN                           |
| Datasets          | Project_Status\_(Experiment)\_Response_Year(s).Extension                                               | DURIN_clean_cflux_2023-2025.csv |
|                   |                                                                                                        |                                 |
| siteID            | Unique site ID written out fully                                                                       | Lygra                           |
| blockID           | Unique block ID, with 3 first letters of siteID and a number (1-4)                                     | Lyg1                            |
| plotID            | Unique plot ID with blockID and treatment                                                              | Alr1…                           |
| treatment         | …                                                                                                      |                                 |
| species           | Vascular plant taxon names follow for Norway Lid & Lid (Lid J & Lid, 2010). We use full species names. | *Leontopodium nivale*           |
| responses         | Response variables                                                                                     | cover, biomass, Reco            |

### Valid siteID

Here is the list of valid siteIDs.

| siteID |
|:-------|
| Lygra  |
| …      |
| …      |
| …      |

Some useful code…

``` r
# code to clean site names
dat |> 
  mutate(siteID = recode(siteID,
                         # old name (replace) = valid name (do not change)
                         'Gud' = "Gudmedalen",
                         'Lav' = "Lavisdalen",
                         'Ram' = "Rambera",
                         'Ulv' = "Ulvehaugen",
                         'Skj' = "Skjelingahaugen",
                         'Alr' = "Alrust",
                         'Arh' = "Arhelleren",
                         'Fau' = "Fauske",
                         'Hog' = "Hogsete",
                         'Ovs' = "Ovstedalen",
                         'Vik' = "Vikesland",
                         'Ves' = "Veskre"))
```

## Overview of datasets

This is an overview over all the datasets. They are available on
[OSF](https://osf.io/f4v9t/).

| Response                                  | Time period       | Level   | Project       | Filename                       |
|:------------------------------------------|:------------------|:--------|:--------------|:-------------------------------|
| **Site level**                            |                   |         |               |                                |
| Coordinates, elevation                    | \-                | Site    | VCG           | e.g. VCG_clean_coordinates.csv |
| Geology, Land-use history                 | \-                | Site    | VCG           |                                |
|                                           |                   |         |               |                                |
| **1) Vegetation**                         |                   |         |               |                                |
| Vascular plant species cover              | 2015 - 2019, 2022 | Plot    | FunCaB        |                                |
| Vascular plant species presence           | 2015 - 2019, 2022 | Subplot | FunCaB        |                                |
| Vegetation height                         | 2015 - 2019, 2022 | Plot    | FunCaB        |                                |
| Functional group biomass                  | 2015 - 2022       | Plot    | FunCaB/FUNDER |                                |
| Total biomass                             | 2022              | Plot    | FUNDER        |                                |
| Reflectance                               | 2021              | Plot    | FunCaB        |                                |
|                                           |                   |         |               |                                |
| Root biomass                              | 2022              | Plot    | FUNDER        |                                |
| Root productivity                         | 2022              | Plot    | FUNDER        |                                |
| Root traits                               | 2022              | Plot    | FUNDER        |                                |
|                                           |                   |         |               |                                |
| Bryophyte composition                     | 2022              | Plot    | FUNDER        |                                |
| Bryophyte presence?                       | 2022              | Plot    | FUNDER        |                                |
| Bryophyte functional traits               | 2022              | Plot    | FUNDER        |                                |
|                                           |                   |         |               |                                |
| **2) Mesofauna**                          |                   |         |               |                                |
| Mycelia production                        | 2022              | Plot    | FUNDER        |                                |
| Mesofauna functional groups and diversity | 2022              | Plot    | FUNDER        |                                |
| Mesofauna abundance and biomass           | 2022              | Plot    | FUNDER        |                                |
|                                           |                   |         |               |                                |
| **3) Fungi**                              |                   |         |               |                                |
| Fungal functional groups and diversity    | 2022              | Plot    | FUNDER        |                                |
|                                           |                   |         |               |                                |
| **4) Bacteria**                           |                   |         |               |                                |
| Bacteria functional groups and diversity  | 2022              | Plot    | FUNDER        |                                |
| **5) Carbon cycling**                     |                   |         |               |                                |
| Ecosystem carbon fluxes                   | 2015-2018, 2022   | Plot    | FunCaB/FUNDER |                                |
| Litter bag decomposition                  | 2022              | Plot    | FUNDER        |                                |
| Tea bag decomposition                     | 2022              | Plot    | FUNDER        |                                |
|                                           |                   |         |               |                                |
| **6) Nutrient cycling**                   |                   |         |               |                                |
| C and N stocks                            | 2022              | Plot    | FUNDER        |                                |
| Available nutrients                       | 2022              | Plot    | FUNDER        |                                |
| Soil depth                                | 2022              | Plot    | FUNDER        |                                |
|                                           |                   |         |               |                                |
| **7) Climate**                            |                   |         |               |                                |
| Soil temperature and moisture             | 2022              | Plot    | FUNDER        |                                |
| Soil temperature and moisture             | 2015-2017         | Plot    | FunCaB        |                                |
| Climate                                   | 2009-2022         | Site    | VCG           |                                |

## Methods

## Data dictionary

**How to make a data dictionary?**

The R package **dataDocumentation** that will help you to make the data
dictionary. You can install and load the package as follows:

``` r

# if needed install the remotes package
install.packages("remotes")

# then install the dataDocumentation package
remotes::install_github("audhalbritter/dataDocumentation")

# and load it
library(dataDocumentation)
```

*Make data description table*

Find the file *R/data_dic/data_description.xlsx*. Enter all the
variables into that table, including variable name, description,
unit/treatment level and how measured. If the variables are global for
all of Funder, leave TableID blank (e.g. siteID). If the variable is
unique for a specific dataset, create a TableID and use it consistently
for one specific dataset. Make sure you have described all variables.

*Make data dictionary*

Then run the function make_data_dic().

``` r

data_dic <- make_data_dictionary(data = biomass,
                                 description_table = description_table,
                                 table_ID = "biomass",
                                 keep_table_ID = FALSE)
```

Check that the function produces the correct data dictionary.

*Add data dictionary to readme file*

Finally, add the data dictionary below to be displayed in this readme
file. Add a title, and a code chunk using `kable()` to display the data
dictionary.

For more details go to the [dataDocumentation
readme](https://github.com/audhalbritter/dataDocumentation) file.

------------------------------------------------------------------------

------------------------------------------------------------------------

### 1 VEGETATION DATA

## Biomass

``` r
 knitr::kable(biomass_dic)
```

------------------------------------------------------------------------
