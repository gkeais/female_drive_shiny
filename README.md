# Meiotic Drive Model - Shiny App

Interactive population genetics model for exploring X chromosome drive dynamics in *Drosophila*.

**Live App**: https://gkeais.shinyapps.io/drive_model_app/

## Associated Publication

Keais GL, Saad-Roy CM, Gonzalez-Sqalli E, Powell CN, Rieseberg LH, Gawryluk RMR, van den Driessche P, Wei KH-C, Loppin B, Perlman SJ. (2025). A selfish supergene causes meiotic drive through both sexes in *Drosophila*. *PNAS* 122(17): e2421185122. https://doi.org/10.1073/pnas.2421185122

## Quick Start

### Input Parameters

- **Drive strength**: Male (m) and female (n) meiotic drive parameters (0-1)
- **Fitness values**: Relative fitness for each of 5 genotypes (0-1)
- **Starting frequencies**: Initial genotype frequencies (**must sum to 1.0**)
- **Generations**: Simulation duration (0-200)

### Running the Model

1. Adjust parameter sliders
2. Ensure starting frequencies sum to 1.0
3. Click **"Run model"**

### Outputs

- **Genotype frequencies**: XX, X<sup>D</sup>X, X<sup>D</sup>X<sup>D</sup>, XY, X<sup>D</sup>Y over time
- **Sex ratio**: Males vs. females
- **Allele frequencies**: X vs. X<sup>D</sup>
- downloadable data from the current run as a csv
