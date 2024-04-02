# Installation Guide

This is an installation guide to set up the epi_predict R project. 

## Requirements

Before getting started, make sure you have the following components installed:

- R (version 4.1.3 or higher)
- [RStudio](https://www.rstudio.com/) (optional, but recommended)
- [Git](https://git-scm.com/downloads)

## Installation Steps

### Clone the Repository

Clone this repository to your computer using the `git clone` command:

```bash
git clone https://github.com/lrodrin/epi_predict
```

### Install Dependencies

Run the following command in your terminal to install the required dependencies:

```bash
Rscript install_packages.R
```

If you are using RStudio, you can also install the dependencies by opening the `install_packages.R` script in RStudio and clicking the **Source** button.

### Configure the Project

Before running the project, you need to ensure that the necessary datasets are available. These datasets are stored in a private Google Drive folder accessible at [this link](https://drive.google.com/drive/folders/1PQ_0qZRyXoPgn_hAAbbpCNcjmfXLsH69?usp=sharing). Permission to access the folder must be requested from [jpujadasmo@uoc.edu](mailto:jpujadasmo@uoc.edu).

Once you have access to the datasets, follow these steps:

1. Create a folder named `data` in the root directory of the project.

2. Download the datasets from the provided Google Drive link and place them in the `data` folder.

With the datasets properly configured, the project will be ready for execution.

## Support

If you encounter any issues during the installation of the project, please open an issue in this repository or contact us via email at [lrodrin@gmail.com](mailto:lrodrin@gmail.com) or [jpujadasmo@uoc.edu](mailto:jpujadasmo@uoc.edu).
