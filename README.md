<!-- README.md is generated from README.Rmd. Please edit that file -->

# qpack

An R Package to assist with CS data science projects at Qualtrics

## Functions

### Study Functions

-   archive_project
    -   Archives a project into a zip file
    -   Uses base zip function to archive all the files and non-empty
        folders inside a project folder. Ignores the .git and
        .Rproj.user folder. Creates a zip file names after the study in
        the general study directory.
-   clear_qkey
    -   more to come on qkey
-   create_codebook
    -   Creates a codebook or data dictionary for a dataframe in
        dataframe format that can be exported to Excel. Provides a list
        of the variables (Column), randomly pulls an example value for
        each one (Example), indicates the type of variable (Type),
        identifies the number of unique levels (Unique) as well as the
        percent of records that are missing (Missing). Leaves spaces for
        a Description and a Note in the file. Option to also create
        frequencies for each variable in the dataframe and export to a
        separate Excel file.
-   create_dictionary
    -   wrapper function that uses sjPlot::view_df to create an html
        data dictionary +Convenience wrapper that uses sjPlot::view_df
        to create an html data dictionary. sjPlot::view_df is
        particularly good at pulling out labelled data that is usually
        found in Qualtrics datasets downloaded via the API or from an
        SPSS file. The default output will list the variable name, type,
        label (if found), number of missing, values, value labels (if
        found), and frequencies. Options set to also display limited
        number of character variables. A more basic version is also
        available that just shows variable labels and values labels wit
        the type option. Output is in html format and is best viewed by
        launching outside R/Rstudio.
-   create_frequencies
    -   Creates a set of frequency tables for each variable in a
        dataframe
    -   Creates a set of frequency tables for each variable in a
        dataframe and exports a list of tables that can be exported to
        Excel.
-   create_qconfig
    -   Deprecated in favor of qkey family of functions
-   create_qkey
    -   more to come on qkey
-   delete_project
    -   Wrapper function that uses janitor::clean_names on a data set
    -   Convenience wrapper that uses janitor::clean_names to clean up
        variable names and returns a list where vars = tibble of
        original variable names and data = the original data with
        cleaned variables names.
-   delete_qconfig
    -   Deprecated in favor of qkey family of functions
-   get_qkey
    -   more to come on qkey
-   set_up
    -   Set-up code to create project directories and load needed
        packages
    -   Part of the qpack package of functions, this sets up project
        directories, sets the working directory, loads specified
        packages, and sources a functions file and config file, if
        specified. Option to load qpack package.
-   show_token
    -   more to come on qkey
-   view_qconfig
    -   Deprecated in favor of qkey family of functions

### Data Functions

-   botbox
    -   Function to recode data into Bottom X Box ratings data
    -   Works on vectors to recode the data into 0,1 values based on
        Bot1, Bot2, Bot3, or BotX values. A minimum value can be
        indicated (minval) but the function will find the minimum in the
        data if not provided. The replacena logical allows for replacing
        any NA with 0, if desired.
-   clean_names
    -   Wrapper function that uses janitor::clean_names on a data set
    -   Convenience wrapper that uses janitor::clean_names to clean up
        variable names. Option to return a list instead where vars =
        tibble of original variable names and data = the data with
        cleaned variables names.
-   create_id
    -   Generates a unique ID value for each row in a dataset
    -   Generates a unique ID value for each row in a dataset that’s an
        ordered sequence of numbers ranging from 1 to the number of rows
        in the data. This is to replace a potentially arbitrary and
        non-numerical case ID number found in the original data. The
        dataset is first sorted (ascending) on up to two other variables
        (var1 and var2, if specified) to ensure replicability of
        ordering. If duplicates are detected, a warning is issued.
        There’s an option to keep the original id variable (default) or
        to remove the original id variable (remove = TRUE). The ID value
        will be in the first position.
-   fix_na
    -   Function to fill NA’s with either a specified value or the mean
    -   Works on vectors or data frames. If a value is provided, like 0,
        then each NA will be replaced with the value. If replace=“mean”
        is specified, then all NA’s will be replaced by the mean value
        for that column. This works across the dataframe per column.
-   flip
    -   Transposes a dataframe with t() but outputs a Tibble
    -   Uses t() to transpose a dataframe but outputs a Tibble instead.
        Maintains the row names that a Tibble typically removes.
-   rescale
    -   Function to rescale a vector to a new minimum and maximum range
    -   Rescales a vector so that range conforms to a defined minimum
        and maximum. If no minimum or maximum is provided, then the
        default is to rescale to a minimum of 0 and a maximum of 1. This
        only works on vectors and not dataframes.
-   set_colnames
    -   Wrapper for a naming columns with pipes from
        magrittr::set_colnames
    -   Convenience wrapper for naming columns with pipes from
        magrittr::set_colnames so do not have to write out the function
        name.
-   topbox
    -   Function to recode data into Top X Box data
    -   Works on vectors to recode the data into 0,1 values based on
        Top1, Top2, Top3, or TopX values. A maximum value can be
        indicated (maxval) but the function will find the maximum in the
        data if not provided. The replacena logical allows for replacing
        any NA with 0, if desired.

### Analysis Functions

-   crosstab
    -   Wrapper for a 2-way table from janitor::tabyl
    -   Generates a 2-way frequency table that is outputed as a
        janitor::tabyl object that shows counts, row percents, column
        percents, and total percents. Also uses quasiquotation from
        rlang so that variables need not be quoted.
-   describe
    -   Provides descriptive statistics for a dataframe, similar to
        Python’s Describe
    -   Provides descriptive statistics for a dataframe, similar to
        Python’s Describe. Returns the variable name, mean, standard
        deviation, number of valid responses, number of missing
        responses, minimum value, 10%, 25%, median, 75%, 90%, and
        maximum value quartiles. Currently limited to numeric data only;
        will exclude any non-numeric data from output and listed as NA
        in output.
-   freq
    -   Wrapper for a 1-way table from janitor::tabyl
    -   Generates a frequency table that is outputed as a janitor::tabyl
        object. Also uses quasiquotation from rlang so that variables
        need not be quoted.
-   pull_labels
    -   Generate Variable and Value Labels Exported Data Containing
        Labels
    -   Generates a list of two dataframes containing variable and value
        label information that is found from Qualtrics and exported via
        qualtRics::fetch_survey or imported from an SPSS .sav file using
        haven::read_spss.
-   threeway
    -   Performs 3-way crosstab using qpack::crosstab split on a third
        variable
    -   Generates a 3-way crosstab, or a qpack::crosstab for each value
        of the third variable specified. Built on janitor::tabyl but
        outputs a dataframe to maintain visual elements suitable for
        exporting (but likely not for the console). Object shows counts,
        row percents, column percents, and total percents for each level
        of third variable. Also uses quasiquotation from rlang so that
        variables need not be quoted. var1 and var2 specify the crosstab
        and var3 specifies the split.
-   write_xlsx
    -   Wrapper function for openxlsx that exports a data frame to an
        XLSX file
    -   Takes a dataframe or other table and exports it as an XLSX file.
        Sheet name can be specified to add to an existing sheet. Options
        include ability to overwrite the sheet(oversheet), overwrite the
        file (overfile), or to use a default (over = TRUE) that
        overwrites both the sheet and the file. A keepna option controls
        whether NA values appear in the sheet.

### Support Functions

-   refresh
    -   Refreshes github install of qpack
    -   Convenience function to perform a remotes::install_github for
        qpack.
-   skeleton()
    -   creates complete skeleton syntax printed to the console for
        creating folder structures with all options
