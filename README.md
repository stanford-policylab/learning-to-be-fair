# Learning to be fair: Replication materials

This repository contains replication materials for our paper, [Learning to be fair: A consequentialist approach to equitable decision-making](https://arxiv.org/abs/2109.08792).

If you are not viewing this README on GitHub, a more recently updated version may be available at our [online paper repository](https://github.com/stanford-policylab/learning-to-be-fair/).

Users can set up the correct R environment by running `renv::restore()` in the root project directory. This will install the packages necessary to run the scripts in this repository.

Scripts can be opened and run in RStudio, or run from the command line using `Rscript`, e.g., `Rscript src/1_client_locations.R`.

Source code is available in the directory `src/`. The main scripts are:

- `1_client_locations.R`
  - **Overview**: This script generates the client location map and our illustration of the consequences of following the naive optimization approach. 
  - **Data description**: These data are described in Section 3.1 of the paper. 
  - **NOTE**: Client locations have been perturbed a random distance to protect privacy.
  - **NOTE**: You will need to sign up for a Google Maps API key [here](https://developers.google.com/maps/get-started) to recreate the first figure. Instructions:
    - Sign in to (or create) your Google account.
    - Agree to the Google Cloud terms of service.
    - Click "Web" under "Platforms" on the left-hand menu.
    - On the new left-hand menu that pops up, click "Keys & Credentials"
    - Click "Create Project" to create your first Maps project.
    - Set a project name, organization, and location, and create the project.
    - Navigate back to the "Keys & Credentials" tab within the new project you created.
    - If needed, set the Country and add billing information.
    - Under "Keys & Credentials", click "Create Credentials", and select "API key"
    - Configure the API key with a name, and restrictions if you desire
    - You should have an API key now. Back in `R`, use `ggmap::register_google()` to allow `ggmap` to access this key.
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
  - **Overview**: This directory contains the scripts necessary to run the simulation in Section 6.1 of the paper. 
  - **Data description**: The data used in the simulation are described in Appendix F of the paper.  
  - The main script is `4_simulation/run_sim_withcosts.R`. This script is designed to be run from the root project directory, e.g., using `Rscript src/4_simulation/run_sim_withcosts.R`. 
  - Users will also be required to set up a Python virtual environment using `venv` called `nudge_simulation`. Establish this environment at `src/4_simulation` with the name `nudge_simulation`. Install the necessary Python packages from the `requirements.txt` file before running `run_sim_withcosts.R`, e.g., by calling `pip install -r requirements.txt` with the environment activated.
  - **Simulation configurations**:
    1. Main simulation (Figures 4, 5, EC2, and EC3):
        - Parameters:
          - `sim_name`: main_sim
          - `budget`: 5
          - `lambda`: 0.0006
          - `experiment_size`: 1000
          - `num_sims`: 2000
          - Population:
            - `param_rideshare_coef`: 4
            - `param_transit_log_distance_coef`: -0.75
            - `param_rideshare_distance_price_permi`: 10
            - `param_transit_price_constant`: 7.5
            - `param_max_distance`: 20
        - Experiments:
          1. `method`: oracle
          1. `method`: random
          1. `method`: random, `stop_early_random`: 50
          1. `method`: random, `stop_early_random`: 100
          1. `method`: random, `stop_early_random`: 150
          1. `method`: UCB
          1. `method`: thompson
          1. `method`: egreedy
        - Note: this combination of parameters and experiments may take substantial computing power to replicate. We generated these across several clusters, with each cluster using 35 cores simultaneously, over the course of several weeks.
    2. Spending disparity comparison (Table 2):
        - Parameters:
          - `sim_name`: spending_disparity
          - All others same as above
        - Experiments:
          1. `method`: UCB, `ignore_lambda`: `T`
          1. `method`: thompson, `ignore_lambda`: `T`
          1. `method`: egreedy, `ignore_lambda`: `T`
    3. Results from Appendix F, paragraph 10:
        - Parameters: 
          - `sim_name`: appendix_f_p_10
          - All others same as above 
        - Experiments:
            1. `method`: UCB
            1. `method`: UCB, `num_sim_samples` = 500
            1. `method`: UCB, `num_sim_samples` = 2000
    4. Spending variation grid search (Figure 6):
        - Parameters: 
          - `sim_name` depends on the set of experiments run:
            1. spending_vars_expensive 
            2. spending_vars_cheaper
          - All others same as above, though `budget` is irrelevant here
        - Experiments:
          - All possible combinations of transit and rideshare allocations of 0%, 2%, 4%, 6%, and 8% (e.g., 2% transit and 6% rideshare)
          - All possible combinations of transit and rideshare allocations between 10% and 100% by every 10% (e.g., 20% transit and 60% rideshare)
        - **NOTE**: To run this grid search, toggle comments on code in `run_sim_withcosts.R` that says "Comment/Uncomment this for grid search."
  - **Inputs**:
    - `data/scc_simulation_population_masked.csv`
  - **Outputs**:
    - `data/simulation_output/{sim_name}/{sim_num_1}_x{export_datetime}.rds`
    - `data/simulation_output/{sim_name}/{sim_num_2}_x{export_datetime}.rds`
    - `data/simulation_output/{sim_name}/{sim_num_3}_x{export_datetime}.rds`
    - Etc.
- `5_simulation_plots.R`
  - **Overview**: This script takes data generated from the simulation(s) above and draws plots based on this data.
  - **Data description**: The data used here are generated by the scripts above.  
  - **Inputs**:
    - `data/simulation_output/{sim_name}/{sim_num_1}_x{export_datetime}.rds`
    - `data/simulation_output/{sim_name}/{sim_num_2}_x{export_datetime}.rds`
    - `data/simulation_output/{sim_name}/{sim_num_3}_x{export_datetime}.rds`
    - Etc.
  - **NOTE**: This script is built assuming simulation configurations #1, #2, and #3 above were all run at once and saved to the same folder, `main_sim`. Simulation configuration #4 were saved to two separate folders, `spending_vars_expensive` and `spending_vars_cheaper`. 
  - **Outputs**:
    - `output/figure_4.pdf`
    - `output/figure_5.pdf`
    - `output/figure_6.pdf`
    - `output/figure_ec2.pdf`
    - `output/figure_ec3.pdf`
    - `Table 2` (output to console)
    