# Light-Biofeedback-Effects-on-Stress

This project is part of my employment as a research assistant. We conducted a study about the effects of light induced breathing intervention on stress responses where I was responsible for study management, data management and analysis. The code shared here was written for preparation and execution of the analysis. As the results of the study do not have published yet, no data or graphs can be shared, the code is only to present my coding abilities.

# Scripts

## Preparation

- **create_study_plan.R** - script for setting up an excel file containing participant IDs, randomized conditions, randomized light calibration sequence.
- **data_man_heartrate.R** - script for preprocessing heartrate data, collected by POLAR device.
- **data_man_inquisit.R** - script for preprocessing results of perfomance tests within the study.
- **data_man_quest.R**  script for preprocessing the questionnaire data.
- **functions.R** - script containing functions to support heartrate preprocessing. It was externalized to keep code of data_man_heartrate.R cleaner
- **semantic_differential.R** - code for plotting semantic differentials. Authored by Justine Leon A. Uro - https://github.com/justineuro

## Analysis

- **analysis.Rmd** - descriptive and inferential analysis written in markdown for enabling easy commented presentation of results.
