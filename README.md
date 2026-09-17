<div id="top">

<!-- HEADER STYLE: COMPACT -->
<img src="resources/iVME-taxa_logo.png" width="30%" align="left" style="margin-right: 15px">

# iVME-TAXA
<em></em>

<!-- BADGES -->
<img src="https://img.shields.io/github/license/mparrondo/ivme-taxa?style=for-the-badge&logo=opensourceinitiative&logoColor=white&color=29B7C5" alt="license">
<img src="https://img.shields.io/github/last-commit/mparrondo/ivme-taxa?style=for-the-badge&logo=git&logoColor=white&color=29B7C5" alt="last-commit">
<img src="https://img.shields.io/github/languages/top/mparrondo/ivme-taxa?style=for-the-badge&color=29B7C5" alt="repo-top-language">
<img src="https://img.shields.io/github/languages/count/mparrondo/ivme-taxa?style=for-the-badge&color=29B7C5" alt="repo-language-count">

<em>Built with the tools and technologies:</em>

<img src="https://img.shields.io/badge/R-276DC3.svg?style=for-the-badge&logo=R&logoColor=white" alt="R">

<br clear="left"/>

## Table of Contents

1. [Table of Contents](#table-of-contents)
2. [Overview](#overview)
3. [Data Availability](#data-availability)
4. [Project Structure](#project-structure)
5. [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Installation](#installation)
    - [Usage](#usage)
6. [Contributing](#contributing)
7. [License](#license)
8. [Acknowledgments](#acknowledgments)

---

## Overview

<details open>
<summary><b>🇬🇧 English</b></summary>

**iVME-taxa** contains the analysis code supporting the study *"Integrating DNA-Based and Morphological Approaches Improves Biodiversity Characterization of Vulnerable Marine Ecosystems at Flemish Cap."* This repository provides the workflows used to combine classical taxonomy with DNA barcoding (COI and 16S) for the identification of marine invertebrates collected during a 2022 survey of the Flemish Cap, an isolated seamount hosting several Vulnerable Marine Ecosystem (VME) indicator species that remain poorly documented.

The pipeline integrates morphological classification of 434 specimens with molecular assignment against public reference databases, addressing the persistent gaps and inconsistencies in invertebrate DNA barcode records — particularly for understudied deep-sea taxa such as Porifera and Cnidaria. By reconciling morphological and molecular evidence, this project improved taxonomic resolution for 140 specimens and expanded the formal species-level inventory from 88 to 123 (or 133, including open-nomenclature assignments), generating reference data intended to support non-invasive, DNA-based monitoring and conservation strategies for vulnerable deep-sea habitats.

</details>

<details>
<summary><b>🇪🇸 Español</b></summary>

**iVME-taxa** contiene el código de análisis empleado en el estudio *"Integrating DNA-Based and Morphological Approaches Improves Biodiversity Characterization of Vulnerable Marine Ecosystems at Flemish Cap."* Este repositorio reúne los flujos de trabajo utilizados para combinar taxonomía clásica con barcoding de ADN (COI y 16S) en la identificación de invertebrados marinos recolectados durante una campaña de muestreo en 2022 en Flemish Cap, un monte submarino aislado que alberga varias especies indicadoras de Ecosistemas Marinos Vulnerables (VME) todavía poco documentadas.

El pipeline integra la clasificación morfológica de 434 especímenes con la asignación molecular frente a bases de datos públicas de referencia, abordando las brechas e inconsistencias persistentes en los registros de barcodes de ADN para invertebrados, particularmente en taxones de aguas profundas poco estudiados como Porifera y Cnidaria. Al conciliar la evidencia morfológica y molecular, este proyecto mejoró la resolución taxonómica en 140 especímenes y amplió el inventario formal a nivel de especie de 88 a 123 taxones (o 133, incluyendo asignaciones de nomenclatura abierta), generando datos de referencia orientados a apoyar estrategias de monitoreo y conservación no invasivas basadas en ADN para hábitats vulnerables de aguas profundas.

</details>

---

## Data Availability

All sequences have been deposited in the European Nucleotide Archive (ENA) at EMBL-EBI under accession number [PRJEB81803](https://www.ebi.ac.uk/ena/browser/view/PRJEB81803), and in the Barcode of Life Data Systems (BOLD) under dataset [DS-VMEFC](https://portal.boldsystems.org/recordset/DS-VMEFC) (DOI: 10.5883/DS-VMEFC).

Voucher specimens for all collected taxa are deposited at the Institute of Marine Research (IIM-CSIC) in Vigo, Spain, and are available for further research.

</content>

---

## Project Structure

```sh
└── ivme-taxa/
    ├── LICENSE
    ├── README.md
    ├── data
    │   ├── metadata
    │   ├── processed
    │   └── raw
    ├── flemish_cap_integrative_taxonomy.Rproj
    └── scripts
        └── rarefraction_by_stratum.R
        └── script_flemish_cap.R
```

---

## Getting Started

### Prerequisites

The analysis was developed in R 4.6.1 (2026-06-24; “Happy Hop”) and uses the
[`renv`](https://rstudio.github.io/renv/) package for reproducible dependency
management. The exact R package versions and installation sources required to
run the analysis are recorded in `renv.lock`.

- **R:** version 4.6.1, 64-bit (`x86_64-pc-linux-gnu`).
- **RStudio Desktop:** recommended, but not required. The project can also be
  run from a standard R session or the command line.
- **Internet connection:** required during the initial setup to restore package
  dependencies with `renv`.

The analysis was developed and tested on 64-bit Manjaro Linux
(kernel 7.1.13-2-MANJARO).

### Installation

Build ivme-taxa from the source and install dependencies:

1. **Clone the repository:**

    ```sh
    ❯ git clone https://github.com/mparrondo/ivme-taxa/
    ```

2. **Navigate to the project directory:**

    ```sh
    ❯ cd ivme-taxa
    ```

3. **Install the dependencies:**

This project uses the `renv` package to provide a reproducible R environment.
Package versions and sources are recorded in `renv.lock`. After downloading or
cloning the repository, open `flemish_cap_integrative_taxonomy.Rproj` in RStudio
(or set the working directory to the project root) and restore the required
packages by running:

```r
install.packages("renv")  # Run only if renv is not already installed
renv::restore()
```

This step only needs to be performed once when setting up the project.

### Usage

After restoring the R environment, run the analysis scripts from the project
root directory. Input files are located in the `data/` directory, and the
required R package versions are managed through `renv`.

For example, the rarefaction analysis can be run with:

```r
source("scripts/script_flemish_cap.R")
```

Please ensure that the required input files are available in the expected
subdirectories before running the script.

---

## Contributing

- **💬 [Join the Discussions](https://github.com/mparrondo/ivme-taxa/discussions)**: Share your insights, provide feedback, or ask questions.
- **🐛 [Report Issues](https://github.com/mparrondo/ivme-taxa/issues)**: Submit bugs found or log feature requests for the `ivme-taxa` project.
- **💡 [Submit Pull Requests](https://github.com/mparrondo/ivme-taxa/blob/main/CONTRIBUTING.md)**: Review open PRs, and submit your own PRs.

<details closed>
<summary>Contributing Guidelines</summary>

1. **Fork the Repository**: Start by forking the project repository to your github account.
2. **Clone Locally**: Clone the forked repository to your local machine using a git client.
   ```sh
   git clone https://github.com/mparrondo/ivme-taxa/
   ```
3. **Create a New Branch**: Always work on a new branch, giving it a descriptive name.
   ```sh
   git checkout -b new-feature-x
   ```
4. **Make Your Changes**: Develop and test your changes locally.
5. **Commit Your Changes**: Commit with a clear message describing your updates.
   ```sh
   git commit -m 'Implemented new feature x.'
   ```
6. **Push to github**: Push the changes to your forked repository.
   ```sh
   git push origin new-feature-x
   ```
7. **Submit a Pull Request**: Create a PR against the original project repository. Clearly describe the changes and their motivations.
8. **Review**: Once your PR is reviewed and approved, it will be merged into the main branch. Congratulations on your contribution!
</details>

<details closed>
<summary>Contributor Graph</summary>
<br>
<p align="left">
   <a href="https://github.com{/mparrondo/ivme-taxa/}graphs/contributors">
      <img src="https://contrib.rocks/image?repo=mparrondo/ivme-taxa">
   </a>
</p>
</details>

---

## License

**iVME-taxa** is licensed under the **GNU General Public License v3.0 (GPLv3)**. This means you are free to use, modify, and distribute this code, provided that any derivative work is also distributed under the same license. For the full license text, refer to the [LICENSE](https://github.com/mparrondo/ivme-taxa/blob/main/LICENSE) file.

---

## Acknowledgments

**Funding**

- This work was carried out within the framework of the research project "Fisheries sustainability and protection of the biodiversity of vulnerable marine ecosystems", established under the agreement signed in September 2021 between the Spanish Ministry of Agriculture, Fisheries and Food and the Spanish National Research Council, to promote fisheries research as a basis for sustainable fisheries management, and funded by the European Union – NextGenerationEU.

- The EU Bottom Trawl Survey on Flemish Cap (NAFO Division 3M) was co-financed by the EU through the European Maritime, Fisheries and Aquaculture Fund (EMFAF) within the Spanish National Programme for the collection, management and use of data in the fisheries sector, and to support scientific advice related to the Common Fisheries Policy.

- **MP** was supported by Grant FJC2021-047881-I funded by MCIN/AEI/10.13039/501100011033 and by the European Union NextGenerationEU/PRTR.

- **NVA** was supported by Grant PTA2021-020507-I funded by MCIN/AEI/10.13039/501100011033 and by the ESF+.

**Institutional and Personal Acknowledgments**

- The authors thank the collaboration and work of the Flemish Cap 2022 research survey leader team from the Spanish Institute of Oceanography (IEO-CSIC), the rest of the scientific staff from the Institute of Marine Research (IIM-CSIC), and the Instituto Português do Mar e da Atmosfera (IPMA), as well as the Vizconde de Eza's crew, for their performance during the scientific survey.

- **MP** would like to thank Dr. María López-Acosta for generously sharing her network of contacts, which enabled the establishment of a collaboration that substantially enhanced the quality of this work.

- A preliminary analysis of these data was presented at the ICES Annual Congress 2023; both **MP** and **NVA** thank the feedback received during the conference.
</content>

<div align="right">

[![][back-to-top]](#top)

</div>


[back-to-top]: https://img.shields.io/badge/-BACK_TO_TOP-151515?style=flat-square


---
