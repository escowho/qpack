#' @title caddat is an example positioning dataset
#'
#' @description Dataset containing attribute associations, importance scores, awareness values,
#' and allocations from a typical positioning study.  A specialty flag is also
#' included.
#'
#' @format A data frame of 362 observations and 247 variables:
#' \describe{
#' \item{respnum}{Respondent ID Number}
#' \item{speclty}{Specialty Indicator where 1=PCP, 2=Pain Spec, 3=Ortho, 4=Rheum, 5=Pod}
#' \item{q19_1}{Awareness: Celebrex}
#' \item{q19_2}{Awareness: Mobic}
#' \item{q19_3}{Awareness: Duexis}
#' \item{q19_4}{Awareness: Vimovo}
#' \item{q19_5}{Awareness: Vivlodex}
#' \item{q19_6}{Awareness: Zorvolex}
#' \item{q21_01}{Allocation: Ibuprofen}
#' \item{q21_02}{Allocation: Naproxen}
#' \item{q21_03}{Allocation: Celecoxib}
#' \item{q21_04}{Allocation: Moloxicam}
#' \item{q21_05}{Allocation: Duexis}
#' \item{q21_06}{Allocation: Vimovo}
#' \item{q21_07}{Allocation: Vivlodex}
#' \item{q21_08}{Allocation: Zorvolex}
#' \item{q21_09}{Allocation: Other}
#' \item{q53_01}{Importance (Unimportant): Provides potent and effective relief against pain and inflammation}
#' \item{q53_02}{Importance (Unimportant): Provides rapid pain relief}
#' \item{q53_03}{Importance (Unimportant): Low risk of major GI issues like ulcers & bleeding}
#' \item{q53_04}{Importance (Unimportant): Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q53_05}{Importance (Unimportant): Low incidence of non-GI-related side effects}
#' \item{q53_06}{Importance (Unimportant): Low cardiovascular risk}
#' \item{q53_07}{Importance (Unimportant): Has an added benefit of GI protection built into the medicine}
#' \item{q53_08}{Importance (Unimportant): Convenient dosing (frequency of doses per day)}
#' \item{q53_09}{Importance (Unimportant): Product samples readily available}
#' \item{q53_10}{Importance (Unimportant): Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q53_11}{Importance (Unimportant): Does not require a prior authorization}
#' \item{q53_12}{Importance (Unimportant): Available through a specialty pharmacy}
#' \item{q53_13}{Importance (Unimportant): Available on most formulary plans}
#' \item{q53_14}{Importance (Unimportant): Available on Medicare/Medicaid}
#' \item{q53_15}{Importance (Unimportant): Cost of the product to the healthcare system}
#' \item{q53_16}{Importance (Unimportant): Has a short half-life}
#' \item{q53_17}{Importance (Unimportant): Low systemic exposure}
#' \item{q53_18}{Importance (Unimportant): Rapid absorption}
#' \item{q53_19}{Importance (Unimportant): Relationship with sales rep}
#' \item{q53_99}{Importance (Unimportant): None}
#' \item{q54_01}{Importance (Critical): Provides potent and effective relief against pain and inflammation}
#' \item{q54_02}{Importance (Critical): Provides rapid pain relief}
#' \item{q54_03}{Importance (Critical): Low risk of major GI issues like ulcers & bleeding}
#' \item{q54_04}{Importance (Critical): Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q54_05}{Importance (Critical): Low incidence of non-GI-related side effects}
#' \item{q54_06}{Importance (Critical): Low cardiovascular risk}
#' \item{q54_07}{Importance (Critical): Has an added benefit of GI protection built into the medicine}
#' \item{q54_08}{Importance (Critical): Convenient dosing (frequency of doses per day)}
#' \item{q54_09}{Importance (Critical): Product samples readily available}
#' \item{q54_10}{Importance (Critical): Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q54_11}{Importance (Critical): Does not require a prior authorization}
#' \item{q54_12}{Importance (Critical): Available through a specialty pharmacy}
#' \item{q54_13}{Importance (Critical): Available on most formulary plans}
#' \item{q54_14}{Importance (Critical): Available on Medicare/Medicaid}
#' \item{q54_15}{Importance (Critical): Cost of the product to the healthcare system}
#' \item{q54_16}{Importance (Critical): Has a short half-life}
#' \item{q54_17}{Importance (Critical): Low systemic exposure}
#' \item{q54_18}{Importance (Critical): Rapid absorption}
#' \item{q54_19}{Importance (Critical): Relationship with sales rep}
#' \item{q54_99}{Importance (Critical): None}
#' \item{q55_01_01}{Attribute Association: Ibuprofen 800mg _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_02}{Attribute Association: Naproxen 500mg _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_03}{Attribute Association: Celecoxib 100mg _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_04}{Attribute Association: Meloxicam 7.5mg _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_05}{Attribute Association: Meloxicam 15mg _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_06}{Attribute Association: Duexis _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_07}{Attribute Association: Vimovo _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_08}{Attribute Association: Vivlodex _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_09}{Attribute Association: All are similar _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_01_10}{Attribute Association: None Perform Well _ Provides potent and effective relief against pain and inflammation}
#' \item{q55_02_01}{Attribute Association: Ibuprofen 800mg _ Provides rapid pain relief}
#' \item{q55_02_02}{Attribute Association: Naproxen 500mg _ Provides rapid pain relief}
#' \item{q55_02_03}{Attribute Association: Celecoxib 100mg _ Provides rapid pain relief}
#' \item{q55_02_04}{Attribute Association: Meloxicam 7.5mg _ Provides rapid pain relief}
#' \item{q55_02_05}{Attribute Association: Meloxicam 15mg _ Provides rapid pain relief}
#' \item{q55_02_06}{Attribute Association: Duexis _ Provides rapid pain relief}
#' \item{q55_02_07}{Attribute Association: Vimovo _ Provides rapid pain relief}
#' \item{q55_02_08}{Attribute Association: Vivlodex _ Provides rapid pain relief}
#' \item{q55_02_09}{Attribute Association: All are similar _ Provides rapid pain relief}
#' \item{q55_02_10}{Attribute Association: None Perform Well _ Provides rapid pain relief}
#' \item{q55_03_01}{Attribute Association: Ibuprofen 800mg _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_02}{Attribute Association: Naproxen 500mg _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_03}{Attribute Association: Celecoxib 100mg _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_04}{Attribute Association: Meloxicam 7.5mg _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_05}{Attribute Association: Meloxicam 15mg _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_06}{Attribute Association: Duexis _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_07}{Attribute Association: Vimovo _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_08}{Attribute Association: Vivlodex _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_09}{Attribute Association: All are similar _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_03_10}{Attribute Association: None Perform Well _ Low risk of major GI issues like ulcers & bleeding}
#' \item{q55_04_01}{Attribute Association: Ibuprofen 800mg _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_02}{Attribute Association: Naproxen 500mg _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_03}{Attribute Association: Celecoxib 100mg _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_04}{Attribute Association: Meloxicam 7.5mg _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_05}{Attribute Association: Meloxicam 15mg _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_06}{Attribute Association: Duexis _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_07}{Attribute Association: Vimovo _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_08}{Attribute Association: Vivlodex _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_09}{Attribute Association: All are similar _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_04_10}{Attribute Association: None Perform Well _ Low incidence of GI-related side effects (e.g., dyspepsia and nausea)}
#' \item{q55_05_01}{Attribute Association: Ibuprofen 800mg _ Low incidence of non-GI-related side effects}
#' \item{q55_05_02}{Attribute Association: Naproxen 500mg _ Low incidence of non-GI-related side effects}
#' \item{q55_05_03}{Attribute Association: Celecoxib 100mg _ Low incidence of non-GI-related side effects}
#' \item{q55_05_04}{Attribute Association: Meloxicam 7.5mg _ Low incidence of non-GI-related side effects}
#' \item{q55_05_05}{Attribute Association: Meloxicam 15mg _ Low incidence of non-GI-related side effects}
#' \item{q55_05_06}{Attribute Association: Duexis _ Low incidence of non-GI-related side effects}
#' \item{q55_05_07}{Attribute Association: Vimovo _ Low incidence of non-GI-related side effects}
#' \item{q55_05_08}{Attribute Association: Vivlodex _ Low incidence of non-GI-related side effects}
#' \item{q55_05_09}{Attribute Association: All are similar _ Low incidence of non-GI-related side effects}
#' \item{q55_05_10}{Attribute Association: None Perform Well _ Low incidence of non-GI-related side effects}
#' \item{q55_06_01}{Attribute Association: Ibuprofen 800mg _ Low cardiovascular risk}
#' \item{q55_06_02}{Attribute Association: Naproxen 500mg _ Low cardiovascular risk}
#' \item{q55_06_03}{Attribute Association: Celecoxib 100mg _ Low cardiovascular risk}
#' \item{q55_06_04}{Attribute Association: Meloxicam 7.5mg _ Low cardiovascular risk}
#' \item{q55_06_05}{Attribute Association: Meloxicam 15mg _ Low cardiovascular risk}
#' \item{q55_06_06}{Attribute Association: Duexis _ Low cardiovascular risk}
#' \item{q55_06_07}{Attribute Association: Vimovo _ Low cardiovascular risk}
#' \item{q55_06_08}{Attribute Association: Vivlodex _ Low cardiovascular risk}
#' \item{q55_06_09}{Attribute Association: All are similar _ Low cardiovascular risk}
#' \item{q55_06_10}{Attribute Association: None Perform Well _ Low cardiovascular risk}
#' \item{q55_07_01}{Attribute Association: Ibuprofen 800mg _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_02}{Attribute Association: Naproxen 500mg _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_03}{Attribute Association: Celecoxib 100mg _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_04}{Attribute Association: Meloxicam 7.5mg _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_05}{Attribute Association: Meloxicam 15mg _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_06}{Attribute Association: Duexis _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_07}{Attribute Association: Vimovo _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_08}{Attribute Association: Vivlodex _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_09}{Attribute Association: All are similar _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_07_10}{Attribute Association: None Perform Well _ Has an added benefit of GI protection built into the medicine}
#' \item{q55_08_01}{Attribute Association: Ibuprofen 800mg _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_02}{Attribute Association: Naproxen 500mg _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_03}{Attribute Association: Celecoxib 100mg _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_04}{Attribute Association: Meloxicam 7.5mg _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_05}{Attribute Association: Meloxicam 15mg _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_06}{Attribute Association: Duexis _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_07}{Attribute Association: Vimovo _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_08}{Attribute Association: Vivlodex _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_09}{Attribute Association: All are similar _ Convenient dosing (frequency of doses per day)}
#' \item{q55_08_10}{Attribute Association: None Perform Well _ Convenient dosing (frequency of doses per day)}
#' \item{q55_09_01}{Attribute Association: Ibuprofen 800mg _ Product samples readily available}
#' \item{q55_09_02}{Attribute Association: Naproxen 500mg _ Product samples readily available}
#' \item{q55_09_03}{Attribute Association: Celecoxib 100mg _ Product samples readily available}
#' \item{q55_09_04}{Attribute Association: Meloxicam 7.5mg _ Product samples readily available}
#' \item{q55_09_05}{Attribute Association: Meloxicam 15mg _ Product samples readily available}
#' \item{q55_09_06}{Attribute Association: Duexis _ Product samples readily available}
#' \item{q55_09_07}{Attribute Association: Vimovo _ Product samples readily available}
#' \item{q55_09_08}{Attribute Association: Vivlodex _ Product samples readily available}
#' \item{q55_09_09}{Attribute Association: All are similar _ Product samples readily available}
#' \item{q55_09_10}{Attribute Association: None Perform Well _ Product samples readily available}
#' \item{q55_10_01}{Attribute Association: Ibuprofen 800mg _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_02}{Attribute Association: Naproxen 500mg _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_03}{Attribute Association: Celecoxib 100mg _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_04}{Attribute Association: Meloxicam 7.5mg _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_05}{Attribute Association: Meloxicam 15mg _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_06}{Attribute Association: Duexis _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_07}{Attribute Association: Vimovo _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_08}{Attribute Association: Vivlodex _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_09}{Attribute Association: All are similar _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_10_10}{Attribute Association: None Perform Well _ Low out-of-pocket cost for the patient ($10 or less)}
#' \item{q55_11_01}{Attribute Association: Ibuprofen 800mg _ Does not require a prior authorization}
#' \item{q55_11_02}{Attribute Association: Naproxen 500mg _ Does not require a prior authorization}
#' \item{q55_11_03}{Attribute Association: Celecoxib 100mg _ Does not require a prior authorization}
#' \item{q55_11_04}{Attribute Association: Meloxicam 7.5mg _ Does not require a prior authorization}
#' \item{q55_11_05}{Attribute Association: Meloxicam 15mg _ Does not require a prior authorization}
#' \item{q55_11_06}{Attribute Association: Duexis _ Does not require a prior authorization}
#' \item{q55_11_07}{Attribute Association: Vimovo _ Does not require a prior authorization}
#' \item{q55_11_08}{Attribute Association: Vivlodex _ Does not require a prior authorization}
#' \item{q55_11_09}{Attribute Association: All are similar _ Does not require a prior authorization}
#' \item{q55_11_10}{Attribute Association: None Perform Well _ Does not require a prior authorization}
#' \item{q55_12_01}{Attribute Association: Ibuprofen 800mg _ Available through a specialty pharmacy}
#' \item{q55_12_02}{Attribute Association: Naproxen 500mg _ Available through a specialty pharmacy}
#' \item{q55_12_03}{Attribute Association: Celecoxib 100mg _ Available through a specialty pharmacy}
#' \item{q55_12_04}{Attribute Association: Meloxicam 7.5mg _ Available through a specialty pharmacy}
#' \item{q55_12_05}{Attribute Association: Meloxicam 15mg _ Available through a specialty pharmacy}
#' \item{q55_12_06}{Attribute Association: Duexis _ Available through a specialty pharmacy}
#' \item{q55_12_07}{Attribute Association: Vimovo _ Available through a specialty pharmacy}
#' \item{q55_12_08}{Attribute Association: Vivlodex _ Available through a specialty pharmacy}
#' \item{q55_12_09}{Attribute Association: All are similar _ Available through a specialty pharmacy}
#' \item{q55_12_10}{Attribute Association: None Perform Well _ Available through a specialty pharmacy}
#' \item{q55_13_01}{Attribute Association: Ibuprofen 800mg _ Available on most formulary plans}
#' \item{q55_13_02}{Attribute Association: Naproxen 500mg _ Available on most formulary plans}
#' \item{q55_13_03}{Attribute Association: Celecoxib 100mg _ Available on most formulary plans}
#' \item{q55_13_04}{Attribute Association: Meloxicam 7.5mg _ Available on most formulary plans}
#' \item{q55_13_05}{Attribute Association: Meloxicam 15mg _ Available on most formulary plans}
#' \item{q55_13_06}{Attribute Association: Duexis _ Available on most formulary plans}
#' \item{q55_13_07}{Attribute Association: Vimovo _ Available on most formulary plans}
#' \item{q55_13_08}{Attribute Association: Vivlodex _ Available on most formulary plans}
#' \item{q55_13_09}{Attribute Association: All are similar _ Available on most formulary plans}
#' \item{q55_13_10}{Attribute Association: None Perform Well _ Available on most formulary plans}
#' \item{q55_14_01}{Attribute Association: Ibuprofen 800mg _ Available on Medicare/Medicaid}
#' \item{q55_14_02}{Attribute Association: Naproxen 500mg _ Available on Medicare/Medicaid}
#' \item{q55_14_03}{Attribute Association: Celecoxib 100mg _ Available on Medicare/Medicaid}
#' \item{q55_14_04}{Attribute Association: Meloxicam 7.5mg _ Available on Medicare/Medicaid}
#' \item{q55_14_05}{Attribute Association: Meloxicam 15mg _ Available on Medicare/Medicaid}
#' \item{q55_14_06}{Attribute Association: Duexis _ Available on Medicare/Medicaid}
#' \item{q55_14_07}{Attribute Association: Vimovo _ Available on Medicare/Medicaid}
#' \item{q55_14_08}{Attribute Association: Vivlodex _ Available on Medicare/Medicaid}
#' \item{q55_14_09}{Attribute Association: All are similar _ Available on Medicare/Medicaid}
#' \item{q55_14_10}{Attribute Association: None Perform Well _ Available on Medicare/Medicaid}
#' \item{q55_15_01}{Attribute Association: Ibuprofen 800mg _ Cost of the product to the healthcare system}
#' \item{q55_15_02}{Attribute Association: Naproxen 500mg _ Cost of the product to the healthcare system}
#' \item{q55_15_03}{Attribute Association: Celecoxib 100mg _ Cost of the product to the healthcare system}
#' \item{q55_15_04}{Attribute Association: Meloxicam 7.5mg _ Cost of the product to the healthcare system}
#' \item{q55_15_05}{Attribute Association: Meloxicam 15mg _ Cost of the product to the healthcare system}
#' \item{q55_15_06}{Attribute Association: Duexis _ Cost of the product to the healthcare system}
#' \item{q55_15_07}{Attribute Association: Vimovo _ Cost of the product to the healthcare system}
#' \item{q55_15_08}{Attribute Association: Vivlodex _ Cost of the product to the healthcare system}
#' \item{q55_15_09}{Attribute Association: All are similar _ Cost of the product to the healthcare system}
#' \item{q55_15_10}{Attribute Association: None Perform Well _ Cost of the product to the healthcare system}
#' \item{q55_16_01}{Attribute Association: Ibuprofen 800mg _ Has a short half-life}
#' \item{q55_16_02}{Attribute Association: Naproxen 500mg _ Has a short half-life}
#' \item{q55_16_03}{Attribute Association: Celecoxib 100mg _ Has a short half-life}
#' \item{q55_16_04}{Attribute Association: Meloxicam 7.5mg _ Has a short half-life}
#' \item{q55_16_05}{Attribute Association: Meloxicam 15mg _ Has a short half-life}
#' \item{q55_16_06}{Attribute Association: Duexis _ Has a short half-life}
#' \item{q55_16_07}{Attribute Association: Vimovo _ Has a short half-life}
#' \item{q55_16_08}{Attribute Association: Vivlodex _ Has a short half-life}
#' \item{q55_16_09}{Attribute Association: All are similar _ Has a short half-life}
#' \item{q55_16_10}{Attribute Association: None Perform Well _ Has a short half-life}
#' \item{q55_17_01}{Attribute Association: Ibuprofen 800mg _ Low systemic exposure}
#' \item{q55_17_02}{Attribute Association: Naproxen 500mg _ Low systemic exposure}
#' \item{q55_17_03}{Attribute Association: Celecoxib 100mg _ Low systemic exposure}
#' \item{q55_17_04}{Attribute Association: Meloxicam 7.5mg _ Low systemic exposure}
#' \item{q55_17_05}{Attribute Association: Meloxicam 15mg _ Low systemic exposure}
#' \item{q55_17_06}{Attribute Association: Duexis _ Low systemic exposure}
#' \item{q55_17_07}{Attribute Association: Vimovo _ Low systemic exposure}
#' \item{q55_17_08}{Attribute Association: Vivlodex _ Low systemic exposure}
#' \item{q55_17_09}{Attribute Association: All are similar _ Low systemic exposure}
#' \item{q55_17_10}{Attribute Association: None Perform Well _ Low systemic exposure}
#' \item{q55_18_01}{Attribute Association: Ibuprofen 800mg _ Rapid absorption}
#' \item{q55_18_02}{Attribute Association: Naproxen 500mg _ Rapid absorption}
#' \item{q55_18_03}{Attribute Association: Celecoxib 100mg _ Rapid absorption}
#' \item{q55_18_04}{Attribute Association: Meloxicam 7.5mg _ Rapid absorption}
#' \item{q55_18_05}{Attribute Association: Meloxicam 15mg _ Rapid absorption}
#' \item{q55_18_06}{Attribute Association: Duexis _ Rapid absorption}
#' \item{q55_18_07}{Attribute Association: Vimovo _ Rapid absorption}
#' \item{q55_18_08}{Attribute Association: Vivlodex _ Rapid absorption}
#' \item{q55_18_09}{Attribute Association: All are similar _ Rapid absorption}
#' \item{q55_18_10}{Attribute Association: None Perform Well _ Rapid absorption}
#' \item{q55_19_01}{Attribute Association: Ibuprofen 800mg _ Relationship with sales rep}
#' \item{q55_19_02}{Attribute Association: Naproxen 500mg _ Relationship with sales rep}
#' \item{q55_19_03}{Attribute Association: Celecoxib 100mg _ Relationship with sales rep}
#' \item{q55_19_04}{Attribute Association: Meloxicam 7.5mg _ Relationship with sales rep}
#' \item{q55_19_05}{Attribute Association: Meloxicam 15mg _ Relationship with sales rep}
#' \item{q55_19_06}{Attribute Association: Duexis _ Relationship with sales rep}
#' \item{q55_19_07}{Attribute Association: Vimovo _ Relationship with sales rep}
#' \item{q55_19_08}{Attribute Association: Vivlodex _ Relationship with sales rep}
#' \item{q55_19_09}{Attribute Association: All are similar _ Relationship with sales rep}
#' \item{q55_19_10}{Attribute Association: None Perform Well _ Relationship with sales rep}
#' }
#' @details HOR-0403
#'
#' q19 Awareness uses a large scale were 1 = Not Aware, 2 & 3 are Aware but Not
#' Used and 4 through 7 are increasing degrees of trial and usage for the following
#' brands: Celebrex, Mobic, Duexis, Vimovo, Vivlodex, Zorvolex.
#'
#' q21 Allocation uses the following brands: Ibuprofen, Naproxen, Celecoxib,
#' Moloxicam, Duexis, Vimovo, Vivlodex, Zorvolex, Other.
#'
#' q53 & q54 Importance Questions, first Unimportant and then Critical.
#'
#' q55 Attribute Associations follow this convention:  q55_XX_YY where
#' XX = Attribute and YY = Brand.  XX follows the same numbering as q54 above.
"caddat"

