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
4. [Features](#features)
5. [Project Structure](#project-structure)
    - [Project Index](#project-index)
6. [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Installation](#installation)
    - [Usage](#usage)
    - [Testing](#testing)
7. [Roadmap](#roadmap)
8. [Contributing](#contributing)
9. [License](#license)
10. [Acknowledgments](#acknowledgments)

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

## Features

<code>❯ REPLACE-ME</code>

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
```

### Project Index

<details open>
	<summary><b><code>IVME-TAXA/</code></b></summary>
	<!-- __root__ Submodule -->
	<details>
		<summary><b>__root__</b></summary>
		<blockquote>
			<div class='directory-path' style='padding: 8px 0; color: #666;'>
				<code><b>⦿ __root__</b></code>
			<table style='width: 100%; border-collapse: collapse;'>
			<thead>
				<tr style='background-color: #f8f9fa;'>
					<th style='width: 30%; text-align: left; padding: 8px;'>File Name</th>
					<th style='text-align: left; padding: 8px;'>Summary</th>
				</tr>
			</thead>
				<tr style='border-bottom: 1px solid #eee;'>
					<td style='padding: 8px;'><b><a href='https://github.com/mparrondo/ivme-taxa/blob/master/flemish_cap_integrative_taxonomy.Rproj'>flemish_cap_integrative_taxonomy.Rproj</a></b></td>
					<td style='padding: 8px;'>Code>❯ REPLACE-ME</code></td>
				</tr>
				<tr style='border-bottom: 1px solid #eee;'>
					<td style='padding: 8px;'><b><a href='https://github.com/mparrondo/ivme-taxa/blob/master/LICENSE'>LICENSE</a></b></td>
					<td style='padding: 8px;'>Code>❯ REPLACE-ME</code></td>
				</tr>
			</table>
		</blockquote>
	</details>
	<!-- scripts Submodule -->
	<details>
		<summary><b>scripts</b></summary>
		<blockquote>
			<div class='directory-path' style='padding: 8px 0; color: #666;'>
				<code><b>⦿ scripts</b></code>
			<table style='width: 100%; border-collapse: collapse;'>
			<thead>
				<tr style='background-color: #f8f9fa;'>
					<th style='width: 30%; text-align: left; padding: 8px;'>File Name</th>
					<th style='text-align: left; padding: 8px;'>Summary</th>
				</tr>
			</thead>
				<tr style='border-bottom: 1px solid #eee;'>
					<td style='padding: 8px;'><b><a href='https://github.com/mparrondo/ivme-taxa/blob/master/scripts/rarefraction_by_stratum.R'>rarefraction_by_stratum.R</a></b></td>
					<td style='padding: 8px;'>Code>❯ REPLACE-ME</code></td>
				</tr>
			</table>
		</blockquote>
	</details>
</details>

---

## Getting Started

### Prerequisites

This project requires the following dependencies:

- **Programming Language:** R

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

echo 'INSERT-INSTALL-COMMAND-HERE'

### Usage

Run the project with:

echo 'INSERT-RUN-COMMAND-HERE'

### Testing

Ivme-taxa uses the {__test_framework__} test framework. Run the test suite with:

echo 'INSERT-TEST-COMMAND-HERE'

---

## Roadmap

- [X] **`Task 1`**: <strike>Implement feature one.</strike>
- [ ] **`Task 2`**: Implement feature two.
- [ ] **`Task 3`**: Implement feature three.

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
