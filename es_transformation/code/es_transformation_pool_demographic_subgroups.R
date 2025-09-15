#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 12/09/2025                                                                           #
# Purpose: combine studies that stratify results by demographic subgroups                    #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read combined data
almp_nma_combined_data_raw_location <- "./data_cleaning/outputs/almp_nma_combined_study_data.RDS"
almp_nma_combined_data_raw <- readRDS(almp_nma_combined_data_raw_location)

#-------------------------------------------------------------------------------
# 1. Filter studies to be pooled
#-------------------------------------------------------------------------------

# subset combined data
almp_nma_combined_data_subset <- almp_nma_combined_data_raw |>
  # exclude study ids that will subsequently be pooled
  filter(
    !str_detect(study_id, "bloom1993nationaljtpastudy"),
    !str_detect(study_id, "caliendo2005employmenteffectsjob"),
    !str_detect(study_id, "caliendo2011fightingyouthunemployment"),
    !str_detect(study_id, "cammeraat2022preventingneetsgreat"),
    !str_detect(study_id, "cronin2020jobsplusevaluation"),
    !str_detect(study_id, "dorsett2022preapprenticeshiptrainingyoung"),
    !str_detect(study_id, "dwp2025sectorbasedworkacademy"),
    !str_detect(study_id, "hollenbeck2006netimpactestimates"),
    !str_detect(study_id, "hollenbeck2016netimpactbenefitcost"),
    !str_detect(study_id, "mamede2015esfsupportingyouth"),
    !str_detect(study_id, "nafilyan2014youthcontractprovision"),
    !str_detect(study_id, "oecd2024impactevaluationtraining"),
    !str_detect(study_id, "oecd2025impactevaluationwage"),
    !str_detect(study_id, "stefanik2024supportingrightworkplace"),
  )

#-------------------------------------------------------------------------------
# 2. Pool studies across subgroups
#-------------------------------------------------------------------------------

# pool studies of 'JPTA (classroom training)' from bloom1993nationaljtpastudy
# note: these results are split by sex
bloom1993nationaljtpastudy_a <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "bloom1993nationaljtpastudy_a",
    "bloom1993nationaljtpastudy_d"
  ),
  output_study_id = "bloom1993nationaljtpastudy_a"
)

# pool studies of 'JPTA (OJT/JSA)' from bloom1993nationaljtpastudy
# note: these results are split by sex
bloom1993nationaljtpastudy_b <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "bloom1993nationaljtpastudy_b",
    "bloom1993nationaljtpastudy_e"
  ),
  output_study_id = "bloom1993nationaljtpastudy_b"
)

# pool studies of 'JPTA (Other services)' from bloom1993nationaljtpastudy
# note: these results are split by sex
bloom1993nationaljtpastudy_c <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "bloom1993nationaljtpastudy_c",
    "bloom1993nationaljtpastudy_f"
  ),
  output_study_id = "bloom1993nationaljtpastudy_c"
)

# pool studies of 'Job Creation Schemes (JCS)' from caliendo2005employmenteffectsjob
# note: these results are split by sex and region (former East vs. West Germany)
caliendo2005employmenteffectsjob <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "caliendo2005employmenteffectsjob_a",
    "caliendo2005employmenteffectsjob_b",
    "caliendo2005employmenteffectsjob_c",
    "caliendo2005employmenteffectsjob_d"
  ),
  output_study_id = "caliendo2005employmenteffectsjob"
)

# pool studies of 'Job Search and Assessment of Employability' from caliendo2011fightingyouthunemployment
# note: these results are split by region (former East vs. West Germany)
caliendo2011fightingyouthunemployment_a <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "caliendo2005employmenteffectsjob_a",
    "caliendo2005employmenteffectsjob_b"
  ),
  output_study_id = "caliendo2011fightingyouthunemployment_a"
)

# pool studies of 'Short-term Training' from caliendo2011fightingyouthunemployment
# note: these results are split by region (former East vs. West Germany)
caliendo2011fightingyouthunemployment_b <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "caliendo2005employmenteffectsjob_c",
    "caliendo2005employmenteffectsjob_d"
  ),
  output_study_id = "caliendo2011fightingyouthunemployment_b"
)

# pool studies of 'JUMP Wage Subsidies' from caliendo2011fightingyouthunemployment
# note: these results are split by region (former East vs. West Germany)
caliendo2011fightingyouthunemployment_c <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "caliendo2005employmenteffectsjob_e",
    "caliendo2005employmenteffectsjob_f"
  ),
  output_study_id = "caliendo2011fightingyouthunemployment_c"
)

# pool studies of 'SGB III Wage Subsidies' from caliendo2011fightingyouthunemployment
# note: these results are split by region (former East vs. West Germany)
caliendo2011fightingyouthunemployment_d <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "caliendo2005employmenteffectsjob_g",
    "caliendo2005employmenteffectsjob_h"
  ),
  output_study_id = "caliendo2011fightingyouthunemployment_d"
)

# pool studies of 'Job Creation Schemes' from caliendo2011fightingyouthunemployment
# note: these results are split by region (former East vs. West Germany)
caliendo2011fightingyouthunemployment_e <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "caliendo2005employmenteffectsjob_i",
    "caliendo2005employmenteffectsjob_j"
  ),
  output_study_id = "caliendo2011fightingyouthunemployment_e"
)

# pool studies of 'Further Training Measures' from caliendo2011fightingyouthunemployment
# note: these results are split by region (former East vs. West Germany)
caliendo2011fightingyouthunemployment_f <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "caliendo2005employmenteffectsjob_k",
    "caliendo2005employmenteffectsjob_l"
  ),
  output_study_id = "caliendo2011fightingyouthunemployment_f"
)

# pool studies of 'Preparatory Training' from caliendo2011fightingyouthunemployment
# note: these results are split by region (former East vs. West Germany)
caliendo2011fightingyouthunemployment_g <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "caliendo2005employmenteffectsjob_m",
    "caliendo2005employmenteffectsjob_n"
  ),
  output_study_id = "caliendo2011fightingyouthunemployment_g"
)

# pool studies of 'WIJ' from cammeraat2022preventingneetsgreat
# note: these results are split by sex
cammeraat2022preventingneetsgreat <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "cammeraat2022preventingneetsgreat_a",
    "cammeraat2022preventingneetsgreat_b",
    "cammeraat2022preventingneetsgreat_c"
  ),
  output_study_id = "cammeraat2022preventingneetsgreat"
)

# pool studies of 'Jobs Plus' from cronin2020jobsplusevaluation
# note: these results are split by age and time since intervention was implemented
cronin2020jobsplusevaluation <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "cronin2020jobsplusevaluation_a",
    "cronin2020jobsplusevaluation_b",
    "cronin2020jobsplusevaluation_c",
    "cronin2020jobsplusevaluation_d",
    "cronin2020jobsplusevaluation_e",
    "cronin2020jobsplusevaluation_f"
  ),
  output_study_id = "cronin2020jobsplusevaluation"
)

# pool studies of 'UK Traineeships program' from dorsett2022preapprenticeshiptrainingyoung
# note: these results are split by age
dorsett2022preapprenticeshiptrainingyoung <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "dorsett2022preapprenticeshiptrainingyoung_a",
    "dorsett2022preapprenticeshiptrainingyoung_b"
  ),
  output_study_id = "dorsett2022preapprenticeshiptrainingyoung"
)

# pool studies of 'Sector-based Work Academy Programme' from dorsett2022preapprenticeshiptrainingyoung
# note: these results are split by age
dwp2025sectorbasedworkacademy <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "dwp2025sectorbasedworkacademy_a",
    "dwp2025sectorbasedworkacademy_b"
  ),
  output_study_id = "dwp2025sectorbasedworkacademy"
)

# pool studies of 'WIA Title 1-B Youth Programs' from hollenbeck2006netimpactestimates
# note: these results are split by cohort/year
hollenbeck2006netimpactestimates <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "hollenbeck2006netimpactestimates_a",
    "hollenbeck2006netimpactestimates_b"
  ),
  output_study_id = "hollenbeck2006netimpactestimates"
)

# pool studies of 'WIA Youth Program' from hollenbeck2016netimpactbenefitcost
# note: these results are split by cohort/year
hollenbeck2016netimpactbenefitcost <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "hollenbeck2016netimpactbenefitcost_a",
    "hollenbeck2016netimpactbenefitcost_b"
  ),
  output_study_id = "hollenbeck2016netimpactbenefitcost"
)

# pool studies of 'The Traineeship Programme' from mamede2015esfsupportingyouth
# note: these results are split by sex
mamede2015esfsupportingyouth <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "mamede2015esfsupportingyouth_a",
    "mamede2015esfsupportingyouth_b"
  ),
  output_study_id = "mamede2015esfsupportingyouth"
)

# 'Youth Contract' from nafilyan2014youthcontractprovision
# note: these results are split by location and sex
nafilyan2014youthcontractprovision <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "nafilyan2014youthcontractprovision_a",
    "nafilyan2014youthcontractprovision_b",
    "nafilyan2014youthcontractprovision_c"
  ),
  output_study_id = "nafilyan2014youthcontractprovision"
)

# pool studies of 'Wage Subsidies' from oecd2024impactevaluationtraining
# note: these results are split by sex
oecd2024impactevaluationtraining_a <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "oecd2024impactevaluationtraining_a",
    "oecd2024impactevaluationtraining_b"
  ),
  output_study_id = "oecd2024impactevaluationtraining_a"
)

# pool studies of 'Training Programs' from oecd2024impactevaluationtraining
# note: these results are split by sex
oecd2024impactevaluationtraining_b <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "oecd2024impactevaluationtraining_c",
    "oecd2024impactevaluationtraining_d"
  ),
  output_study_id = "oecd2024impactevaluationtraining_b"
)

# pool studies of 'Training Programs' from oecd2025impactevaluationwage
# note: these results are split by sex
oecd2025impactevaluationwage <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "oecd2025impactevaluationwage_a",
    "oecd2025impactevaluationwage_b"
  ),
  output_study_id = "oecd2025impactevaluationwage"
)

# pool studies of 'graduate practice' from stefanik2024supportingrightworkplace
# note: these results are split by sex and length of prior unemployment
stefanik2024supportingrightworkplace_a <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "stefanik2024supportingrightworkplace_a",
    "stefanik2024supportingrightworkplace_b",
    "stefanik2024supportingrightworkplace_c",
    "stefanik2024supportingrightworkplace_d",
    "stefanik2024supportingrightworkplace_e",
    "stefanik2024supportingrightworkplace_f"
  ),
  output_study_id = "stefanik2024supportingrightworkplace_a"
)

# pool studies of 'activation works' from stefanik2024supportingrightworkplace
# note: these results are split by sex and length of prior unemployment
stefanik2024supportingrightworkplace_b <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "stefanik2024supportingrightworkplace_g",
    "stefanik2024supportingrightworkplace_h",
    "stefanik2024supportingrightworkplace_i",
    "stefanik2024supportingrightworkplace_j",
    "stefanik2024supportingrightworkplace_k",
    "stefanik2024supportingrightworkplace_l"
  ),
  output_study_id = "stefanik2024supportingrightworkplace_b"
)

# pool studies of 'voluntary activation works' from stefanik2024supportingrightworkplace
# note: these results are split by sex and length of prior unemployment
stefanik2024supportingrightworkplace_c <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "stefanik2024supportingrightworkplace_m",
    "stefanik2024supportingrightworkplace_n",
    "stefanik2024supportingrightworkplace_o",
    "stefanik2024supportingrightworkplace_p",
    "stefanik2024supportingrightworkplace_q",
    "stefanik2024supportingrightworkplace_e"
  ),
  output_study_id = "stefanik2024supportingrightworkplace_c"
)

#-------------------------------------------------------------------------------
# 3. Combine studies again and export
#-------------------------------------------------------------------------------

# combine pooled studies with study data
almp_nma_combined_data_clean <- bind_rows(
  almp_nma_combined_data_subset,
  bloom1993nationaljtpastudy_a,
  bloom1993nationaljtpastudy_b,
  bloom1993nationaljtpastudy_c,
  caliendo2005employmenteffectsjob,
  caliendo2011fightingyouthunemployment_a,
  caliendo2011fightingyouthunemployment_b,
  caliendo2011fightingyouthunemployment_c,
  caliendo2011fightingyouthunemployment_d,
  caliendo2011fightingyouthunemployment_e,
  caliendo2011fightingyouthunemployment_f,
  caliendo2011fightingyouthunemployment_g,
  cammeraat2022preventingneetsgreat,
  cronin2020jobsplusevaluation,
  dorsett2022preapprenticeshiptrainingyoung,
  dwp2025sectorbasedworkacademy,
  hollenbeck2006netimpactestimates,
  hollenbeck2016netimpactbenefitcost,
  mamede2015esfsupportingyouth,
  nafilyan2014youthcontractprovision,
  oecd2024impactevaluationtraining_a,
  oecd2024impactevaluationtraining_b,
  oecd2025impactevaluationwage,
  stefanik2024supportingrightworkplace_a,
  stefanik2024supportingrightworkplace_b,
  stefanik2024supportingrightworkplace_c
)

# export
saveRDS(
  almp_nma_combined_data_clean,
  file = "./data_cleaning/outputs/almp_nma_pooled_analysis_data.rds"
)
