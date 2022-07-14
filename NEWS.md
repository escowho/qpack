<!-- README.md is generated from README.Rmd. Please edit that file -->

# Version History

## 0.1.8.9000 2022-0701

-   Deprecating qconfig in place of qkey family of functions

## 0.1.7 - 2022-0518

-   Updated create_dictionary to correctly show frequencies with numeric
    data
-   Updated tests for create_dictionary, create_frequencies,
    create_qconfig to correctly reset or pass during testing
-   Updated pull_labels to account for name problem if variable named
    value

## 0.1.6 - 2202-0423

-   Updated dependencies to correct install problem

## 0.1.5 - 2022-0422

-   Removed data type issued for create_codebook that would sometimes
    cause an error
-   Added a progress bar and beepr:beep to create_frequencies
-   Added create_dictionary

## 0.1.4 - 2022-0314

-   Added filters to archive_project to remove .tmp and .qconfig files

## 0.1.3 - 2022-02-08

-   Added create_frequencies and separated function from create_codebook
-   Cleaned up NEWS and README

## 0.1.2 - 2022-02-03

-   Added additional quartiles to the describe function

## 0.1.1 - 2022-02-01

-   Added the following functions:

    -   create_codebook
    -   create_qconfig, view_qconfig, delete_qconfig
    -   describe
    -   flip
    -   pull_labels
    -   refresh

## 0.1.0 - 2021-09-12

-   Initial release, including the following functions:

    -   archive_project, delete_project
    -   clean_names
    -   create_id
    -   fix_na
    -   freq, crosstab, threeway
    -   rescale
    -   set_colnames
    -   set_up
    -   topbox, botbox
    -   write_xlsx
