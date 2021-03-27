#keep env cln
rm(list = ls())

#load libs
library(tidyverse) #see output for masks

#script opts
# option(tibble.width = Inf)

# Data Import & Quick Updates ---------------------------------------------

#specify col_names
names(read_csv("../data/proj-data_cln.csv", n_max = 0)) -> col_names

#read in data
read_csv("../data/proj-data_cln.csv", #dir w/ data file
         col_types = cols(Age = "d"), #parse "Age" col as num
         skip = 2, #skip first 2 rows
         col_names = col_names) -> #specify col names
    raw

#list containing processed dfs
list() -> dat_ls

#demographics items
raw %>%
    #convert col names to lowercase
    rename_with(tolower) %>%
    #extract cols of interest
    select(age:race, edu, industry) %>%
    #shorten "High school..." for ease of reading
    mutate(edu = if_else(grepl("High school", edu), "High school/GED", edu)) %>%
    #shorted opt for Edu
    mutate(edu = str_remove_all(edu, "in college ")) ->
    dat_ls$demo

#custom function to convert anchors from chr to nums
unfactorise = function(x) {
    case_when(
        x %in% c(
            "Strongly disagree",
            "Very Slightly",
            "To a very small extent",
            "Never",
            "Very dissatisfied"
        ) ~ 1,
        x %in% c(
            "Somewhat disagree",
            "A Little",
            "To a small extent",
            "Sometimes",
            "Somewhat dissatisfied"
        ) ~ 2,
        x %in% c(
            "Neither agree nor disagree",
            "Moderately",
            "To a moderate extent",
            "About half the time",
            "Neither satisfied nor dissatisfied"
        ) ~ 3,
        x %in% c(
            "Somewhat agree",
            "Quite a Bit",
            "To a large extent",
            "Most of the time",
            "Somewhat satisfied"
        ) ~ 4,
        x %in% c(
            "Strongly agree",
            "Extremely",
            "To a very large extent",
            "Always",
            "Very satisfied"
        ) ~ 5
    )
}

#extract scales
raw %>%
    #convert col names to lowercase
    rename_with(tolower) %>%
    #extract only items used in scales; attention checks previously processed!
    select(!age:industry & !starts_with("att_chk")) %>%
    # #alphabetize col names; chrs don't read double digit nums in order...
    # select(order(colnames(.))) %>%
    #apply custom func across cols - convert chrs to nums
    map_df(., function(.x) {
        unfactorise(.x)
        }) %>%
    #handle reverse coded items
    mutate(
        across(
            ends_with("_r"), ~ 6 - .x
            )
        ) ->
    dat_ls$survey

# End ----


# Scale & Construct Extraction --------------------------------------------

list() -> scales_ls

#custom function
scl_pull_fun = function(prefix = NULL) {

    if (!is.character(prefix)) {
        stop("prefix must be chr class")
    }

    #use updated df to extract scales
    select(dat_ls$survey, starts_with(prefix)) ->
        scl_df

    return(scl_df)
}

#extract respective scales - use custom function w/ df already
#bfi
scl_pull_fun("bfi_c") -> scales_ls$bfi$consci
scl_pull_fun("bfi_a") -> scales_ls$bfi$agree
scl_pull_fun("bfi_n") -> scales_ls$bfi$neuro
scl_pull_fun("bfi_o") -> scales_ls$bfi$open
scl_pull_fun("bfi_e") -> scales_ls$bfi$extro

#hostility
scl_pull_fun("hos_s") -> scales_ls$hos$sus
scl_pull_fun("hos_r") -> scales_ls$hos$resent

#equity sensitivity
scl_pull_fun("eq") -> scales_ls$eq

#affectivity
scl_pull_fun("pa") -> scales_ls$affect$pa
scl_pull_fun("na") -> scales_ls$affect$na

#org justice
scl_pull_fun("jus_p") -> scales_ls$jus$proc
scl_pull_fun("jus_d") -> scales_ls$jus$distr
scl_pull_fun("jus_int") -> scales_ls$jus$int
scl_pull_fun("jus_inf") -> scales_ls$jus$info

#counterproductive work behavior (cwb)
scl_pull_fun("cwb_s") -> scales_ls$cwb$sbtg
scl_pull_fun("cwb_pd") -> scales_ls$cwb$prod_dev
scl_pull_fun("cwb_w") -> scales_ls$cwb$withdrw
scl_pull_fun("cwb_t") -> scales_ls$cwb$theft
scl_pull_fun("cwb_a") -> scales_ls$cwb$abuse

#satisfaction
scl_pull_fun("SAT") -> scales_ls$sat

#combine scales into sorted df
reduce(scales_ls$bfi, cbind) %>%
    cbind(reduce(scales_ls$hos, cbind)) %>%
    cbind(scales_ls$eq) %>%
    cbind(reduce(scales_ls$affect, cbind)) %>%
    cbind(reduce(scales_ls$jus, cbind)) %>%
    cbind(reduce(scales_ls$cwb, cbind)) %>%
    cbind(scales_ls$sat) %>%
    as_tibble() ->
    scales_ls$full

#save relevant dfs as R obj
save(dat_ls, scales_ls, file = "../data/r_objs/feat_eng_cln.rda")
