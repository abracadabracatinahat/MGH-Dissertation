# ScanOMetrics Economic Model

This is an economic evaluation model for AI-enhanced dementia diagnosis tool ScanOMetrics, compared to standard care. 
A discrete-event simulation model is used, using a decision tree and Markov model. 

## Key Features

- Compares AI-enhanced vs. standard care pathways
- Accounts for urban vs. rural healthcare access differences
- Reflects population transitions, by modelling aging cohorts
- Calculates ICERs and QALYs 
- Simulates waiting times and bottlenecks
- One-way sensitivity analysis (OWSA) done for parameter uncertainty


### Core Simulation Files

- parameters.R: defines all key inputs for poulation build up, transitions, sensitivity and specifcity, costs, variation, etc. 
- main.R: Main entry point and execution of model, defines scenarios. Simulation pipeline, error handling, validation. 
- simulation.R: runs simulation, yearly cycle progression logic, population state updates.
- cohort_manager.R: cohort initialization and management. Progression, aging, stratification, mortality, dynamics. 
- queue_manager.R: wait times and queuing mechanisms. For GP, MC, and MRI. Applies capacity constraints.
- pathways.R: diagnostic pathway logic, decision tree.
- transitions.R: health state progression modelling, markov chain.
- outcomes.R: outcome calculations, QALY computations, ICER, QALYs, aggregation, scenario comparison.
- owsa.R: one way sensitivty analysis
- generate_urban_rural_table.R: area omaprison table 

