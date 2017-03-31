Welcome to the code repository for the [article](https://academic.oup.com/femsec/article-abstract/doi/10.1093/femsec/fix040/3078548/Elucidation-of-the-methanogenic-potential-from).  [![DOI](https://zenodo.org/badge/22930297.svg)](https://zenodo.org/badge/latestdoi/22930297)

## Elucidation of the methanogenic potential from coalbed microbial communities amended with volatile fatty acids

[Christopher N. Lyles](https://sciences.nsula.edu/contact-usfaculty/)<sup>[1](http://mpbio.ou.edu/)</sup>,
[Victoria A. Parisi](https://www.linkedin.com/in/victoria-parisi-4a64437/)<sup>[1](http://mpbio.ou.edu/)</sup>,
[William H. Beasley](http://scholar.google.com/citations?user=ffsJTC0AAAAJ&hl=en)<sup>[3](http://howardliveoak.com/)</sup>
[Joy D. Van Nostrand](https://www.linkedin.com/in/joy-van-nostrand-64310120/)<sup>[1](http://mpbio.ou.edu/),[2](http://ieg.ou.edu/)</sup>
[Jizhong Zhou]( http://www.ou.edu/content/ieg/people/ieg-director.html)<sup>[1](http://mpbio.ou.edu/),[2](http://ieg.ou.edu/)</sup>
[Joseph M. Suflita](http://mpbio.ou.edu/joseph-m-sulfita-ph-d)<sup>[1](http://mpbio.ou.edu/)</sup> (2017). **[Elucidation of the methanogenic potential from coalbed microbial communities amended with volatile fatty acids](https://academic.oup.com/femsec/article-abstract/doi/10.1093/femsec/fix040/3078548/Elucidation-of-the-methanogenic-potential-from?redirectedFrom=fulltext)**. [*FEMS Microbiology Ecology*](https://academic.oup.com/femsec), 93.  doi: https://doi.org/10.1093/femsec/fix040.

Organization 1: [Department of Microbiology and Plant Biology](http://mpbio.ou.edu/), and the [Institute for Energy and the Environment](http://vpr-norman.ou.edu/centers-institutes/list/ou-institute-energy-and-environment), Norman, OK; Organization 2: [Institute for Environmental Genomics](http://ieg.ou.edu/), [University of Oklahoma](http://www.ou.edu/), Norman, OK; Organization 3: [Howard Live Oak, LLC](http://howardliveoak.com/), Norman, OK.

(Note: Christopher Lyles is now an Assistant Professor of Biology at [Northwestern State University](https://biology.nsula.edu/)).


## Article Abstract
>The potential for modern coalfield methanogenesis was assessed using formation water from the Illinois Basin, Powder River Basin, and Cook Inlet gas field as inocula for nutrient-replete incubations amended with C<sub>1</sub>-C<sub>5</sub> fatty acids as presumed intermediates formed during anaerobic coal biodegradation. Instead of the expected rapid mineralization of these substrates, methanogenesis was inordinately slow (∼1 μmol•day<sup>−1</sup>), following long lag periods (>100 day), and methane yields typically did not reach stoichiometrically expected levels. However, a gene microarray confirmed the potential for a wide variety of microbiological functions, including methanogenesis, at all sites. The Cook Inlet incubations produced methane at a relatively rapid rate when amended with butyrate (*r* = 0.98; *p* = 0.001) or valerate (*r* = 0.84; *p* = 0.04), a result that significantly correlated with the number of positive mcr gene sequence probes from the functional gene microarray and was consistent with the in situ detection of C<sub>4</sub>-C<sub>5</sub> alkanoic acids. This finding highlighted the role of syntrophy for the biodegradation of the softer lignite and subbituminous coal in this formation, but methanogenesis from the harder subbituminous and bituminous coals in the other fields was less apparent. We conclude that coal methanogenesis is probably not limited by the inherent lack of metabolic potential, the presence of alternate electron acceptors, or the lack of available nutrients, but more likely restricted by the inherent recalcitrance of the coal itself.

> Keywords:
coalbed methane; coal basin; metabolite analysis; alkanoic acids; microbial communities; functional gene array


## Selected Figures from Manuscript
The following figures can be reproduced from the datasets and analytical code in this repository. These are low-resolution versions; the high-resolution versions can be found in the [article](https://academic.oup.com/femsec/article-abstract/doi/10.1093/femsec/fix040/3078548/Elucidation-of-the-methanogenic-potential-from) or in the [`./Analysis/`](./Analysis/) directory.

#### Figure 4

Venn diagrams showing the genetic overlap of functional genes associated with nine gene categories from three coalfields using the GeoChip functional gene microarray (3.2 and 4.0). The numeric value indicates the number of shared genes between the coalfields. A list of the shared genes between all three coalfields can be found in the supplemental information (Table S3).

<img src="./Analysis/GeneticOverlap/Figures/CarbonCycling.png" alt="figure_2a" width="200" /> <img src="Analysis/GeneticOverlap/Figures/EnergyProcess.png" alt="figure_2b" width="200" /> <img src="Analysis/GeneticOverlap/Figures/MetalResistance.png" alt="figure_2c" width="200" /> <img src="Analysis/GeneticOverlap/Figures/MethaneProduction.png" alt="figure_2d" width="200" /> <img src="Analysis/GeneticOverlap/Figures/Nitrogen.png" alt="figure_2e" width="200" /> <img src="Analysis/GeneticOverlap/Figures/OrganicRemediation.png" alt="figure_2f" width="200" /> <img src="Analysis/GeneticOverlap/Figures/Phosphorus.png" alt="figure_2g" width="200" /> <img src="Analysis/GeneticOverlap/Figures/SulfateReduction.png" alt="figure_2h" width="200" /> <img src="Analysis/GeneticOverlap/Figures/SulfurOxidation.png" alt="figure_2i" width="200" />

#### Figure 5

A scatterplot correlating the total amount of methane (μ mol) to the **number of positive mcr probes** as measured by the GeoChip.  

The boxes associated with the Illinois Basin data represent the range of error between three technical replicates of the GeoChip and the methane produced in duplicate incubations. The vertical bars for the Cook Inlet gas field and the Powder River Basin represent the error between the methane produced in duplicate incubations.


#### Figure 6

A scatterplot correlating the total amount of methane (μ mol) to the **signal intensities of mcr probes** as measured by the GeoChip.


## Directories
The following directories are part of the repository:
 * `Data/Raw`: CSV versions of the initial dataset.  It is readable from GitHub, or any text editor.
 * `Data/Derived`: CSV versions of the groomed/derivative dataset.
 * `Analysis`: The analytical code.
 * `utility/`: R scripts that help automate certain tasks, including reproducing the analysis and figures in this repository.


## Code Repository

The analysis is written primarily in R, under the GPL-2 License.  The DOI of the *repository* (not of the article) is [10.5281/zenodo.439418](https://zenodo.org/badge/latestdoi/22930297).
