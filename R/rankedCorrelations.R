rankedCorrelations <- function(data){
  library(tidyverse)
  data %>%
    gather_(data = ., key_col = "key", value_col = "value", gather_cols = names(.)[sapply(., is.character)]) %>%
    rownames_to_column() %>%
    mutate(spreadValue = 1) %>%
    unite(comb, key, value) %>%
    spread(comb, spreadValue, fill = 0) %>%
    select(-rowname) %>%
    cor %>%
    data.frame %>%
    rownames_to_column(var = "Vars1") %>%
    tbl_df %>%
    gather(key = "Vars2", value = "correlation", -Vars1) %>%
    mutate(Vars1 = gsub("_","~",Vars1),
           Vars1 = ifelse(grepl("_", Vars1), Vars1, paste0(Vars1, "_", Vars1)),
           Vars2 = gsub("_","~",Vars2),
           Vars2 = ifelse(grepl("_", Vars2), Vars2, paste0(Vars2, "_", Vars2))) %>%
    separate(Vars1, into = c("Vars1_category", "Vars1"), sep = "_") %>%
    separate(Vars2, into = c("Vars2_category", "Vars2"), sep = "_") %>%
    mutate_at(vars(contains("Var")), funs(gsub("~","_",.))) %>%
    filter(Vars1 != Vars2,
           Vars1_category != Vars2_category,
           !is.na(correlation)) %>%
    mutate(Vars1_category = ifelse(Vars1_category == Vars1, "", Vars1_category),
           Vars2_category = ifelse(Vars2_category == Vars2, "", Vars2_category)) %>%
    #order by the absolute value of correlation
    group_by(abs(correlation)) %>%
    slice(1) %>%
    arrange(desc(abs(correlation))) %>%
    ungroup %>%
    select(-`abs(correlation)`) %>%
    return
}
