# Learning to be fair: A consequentialist approach to equitable decision-making

This repository contains replication materials for our paper, [Learning to be fair: A consequentialist approach to equitable decision-making](https://arxiv.org/abs/2109.08792).

Users can set up the correct R environment by running `renv::restore()` in the root project directory. This will install the packages necessary to run the scripts in this repository.

Scripts can be opened and run in RStudio, or run from the command line using `Rscript`, e.g., `Rscript src/1_client_locations.R`.

Source code is available in the directory `src/`. The main scripts are:

- `1_client_locations.R`
  - **Overview**: This script generates the client location map and our illustration of the consequences of following the naive optimization approach. 
  - **Data description**: These data are described in Section 3.1 of the paper. 
  - **NOTE**: Client locations have been randomly perturbed a random amount to protect privacy.
  - **Inputs**:
    - `data/scc_client_map_data_masked.csv`
    - `data/scc_no_parity_illustration.csv`
  - **Outputs**:
    - `output/figure_1a.pdf`
    - `output/figure_1b.pdf`
- `2_pareto.R`
  - **Overview**: This script generates the Pareto frontier figure.
  - **Outputs**:
    - `output/figure_2.pdf`
- `3_survey.R`
  - **Overview**: This script parses the survey results and generates the relevant figures.
  - **Data description**: These data are standard survey outputs from Qualtrics.
  - **Inputs**: 
    - `data/LTBF survey December 21, 2023_20.09.csv`
  - **Outputs**:
    - `output/figure_3b.pdf`
    - `output/figure_ec1a.pdf`
    - `output/figure_ec1b.pdf`
- `4_simulation/`
  - **Overview**: This directory contains the scripts necessary to run the simulation. 
  - **Data description**: The data used in the simulation are described in Appendix F of the paper.  
  - The main script is `4_simulation/run_sim.R`. This script is designed to be run from the root project directory, e.g., using `Rscript src/4_simulation/run_sim.R`. 
  - Users will also be required to set up a Python virtual environment using `venv` called `nudge_simulation`. Establish this environment at `src/4_simulation` with the name `nudge_simulation`. Install the necessary Python packages from the `requirements.txt` file before running `run_sim.R`, e.g., by calling `pip install -r requirements.txt` with the environment activated.
  - **Inputs**:
    - `data/scc_simulation_population_masked.csv`
  - **Outputs**:
    - `output/simulation_output/{sim_num_1}_x{export_datetime}.rds`
    - `output/simulation_output/{sim_num_2}_x{export_datetime}.rds`
    - `output/simulation_output/{sim_num_3}_x{export_datetime}.rds`
    - Etc.
- `5_simulation_plots.R`
  - **Overview**: This script takes data generated from the simulation above and draws plots based on this data.
  - **Data description**: The data used here are generated by the scripts above.  
  - **NOTE**: For now the data from our simulation is not stored in this repository, as it is too large for Github. We are waiting for direction from the journal on where the best place to store this data would be.
  - **Inputs**:
    - `output/simulation_output/{sim_num_1}_x{export_datetime}.rds`
    - `output/simulation_output/{sim_num_2}_x{export_datetime}.rds`
    - `output/simulation_output/{sim_num_3}_x{export_datetime}.rds`
    - Etc.
  - **Outputs**:
    - `output/figure_4.pdf`
    - `output/figure_5.pdf`
    - `output/figure_6.pdf`
    - `output/figure_ec2.pdf`
    - `output/figure_ec3.pdf`
    