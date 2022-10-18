## README
This is an archive including the data of currently seven studies measuring emotions in daily life using the Experience Sampling Methodology (ESM).

### The included studies are:

1. Bringmann, L. F., Vissers, N., Wichers, M., Geschwind, N., Kuppens, P., Peeters, F., ... & Tuerlinckx, F. (2013). A network approach to psychopathology: new insights into clinical longitudinal data. PloS one, 8(4), e60188.
2. Bringmann, L. F., Pe, M. L., Vissers, N., Ceulemans, E., Borsboom, D., Vanpaemel, W., ... & Kuppens, P. (2016). Assessing temporal emotion dynamics using networks. Assessment, 23(4), 425-435.
3. Rowland, Z., & Wenzel, M. (2020). Mindfulness and affect-network density: Does mindfulness facilitate disengagement from affective experiences in daily life?. Mindfulness, 11(5), 1253-1266.
4. Fried, E. I., Papanikolaou, F., & Epskamp, S. (2021). Mental health and social contact during the COVID-19 pandemic: an ecological momentary assessment study. Clinical Psychological Science, 10(2), 340-354.
5. Vrijen, C., Hartman, C. A., Van Roekel, E., De Jonge, P., & Oldehinkel, A. J. (2018). Spread the joy: How high and low bias for happy facial emotions translate into different daily life affect dynamics. Complexity, 2018.
6. Fisher, A. J., Reeves, J. W., Lawyer, G., Medaglia, J. D., & Rubel, J. A. (2017). Exploring the idiographic dynamics of mood and anxiety via network analysis. Journal of abnormal psychology, 126(8), 1044.
7. Wright, A. G., Stepp, S. D., Scott, L. N., Hallquist, M. N., Beeney, J. E., Lazarus, S. A., & Pilkonis, P. A. (2017). The effect of pathological narcissism on interpersonal and affective processes in social interactions. Journal of abnormal psychology, 126(7), 898.

Note 1: Vrijen et. al (2018) ask for a request to reuse their data. For more info see [https://osf.io/4czv3/](https://osf.io/4czv3/).

Note 2: The data of Wright et al. (2017) are shared in the publication: Wendt, L. P., Wright, A. G., Pilkonis, P. A., Woods, W. C., Denissen, J. J., Kühnel, A., & Zimmermann, J. (2020). Indicators of affect dynamics: Structure, reliability, and personality correlates. European Journal of Personality, 34(6), 1060-1072.

We would like to thank these authors for making their data openly available.


### Organization of this archive

The organization of this archive is as follows:

- `DataFromAuthors/`: Includes the data and associated material for each study.
- `ProcessingFiles/`: Includes R files for each original data set and processes them into a standard format; these scripts are called by `ProcessData.R`
- `DataClean/`: Includes the pre-processed data for each study.
- `ProcessData.R`: Runs the processing scripts for each study, generating the cleaned data.
- `ProcessBetween.R`: Uses the cleaned between-person data to create a list of datasets with columns id and between person characteristic for each dataset; this script is called by `ProcessData.R`

