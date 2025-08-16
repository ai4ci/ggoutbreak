# get and cache data
raw_lineages = tempfile()
download.file("https://covid-surveillance-data.cog.sanger.ac.uk/download/lineages_by_ltla_and_week.tsv",raw_lineages)
variants = readr::read_tsv(raw_lineages,na = c("Lineage data suppressed","Unassigned"))

lineages = tempfile()
download.file("https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json", lineages)
lineage_keys = jsonlite::read_json(lineages)

# filter out recombinant species
recombinants = lineage_keys %>% magrittr::extract(lapply(.,length)>1)
lineage_keys = lineage_keys %>% magrittr::extract(lapply(.,length)==1)

lineage_key_df = dplyr::tibble(prefix = names(lineage_keys), expanded = unlist(unname(lineage_keys)))
recombinant_key_df = dplyr::tibble(prefix = names(recombinants), expanded = names(recombinants))

lineage_key_df = dplyr::bind_rows(lineage_key_df, recombinant_key_df)

# replace e.g. AY with B.1.617.2
variants2 = variants %>%
  dplyr::mutate(
    prefix = Lineage %>% stringr::str_extract("^[A-Z]+"),
    suffix = Lineage %>% stringr::str_sub(start = stringr::str_length(prefix)+1)
  ) %>%
  dplyr::left_join(lineage_key_df, by="prefix") %>%
  dplyr::mutate(full_tree = dplyr::case_when(
    !is.null(expanded) & expanded != "" ~ paste0(expanded,suffix),
    TRUE ~ paste0(prefix,suffix)
  )) %>%
  dplyr::mutate(
    lineage_class = dplyr::case_when(
      is.na(Lineage) ~ "Unclassified",
      full_tree %>% stringr::str_starts("B.1.1.7") ~ "Alpha (B.1.1.7)",
      full_tree %>% stringr::str_starts("B.1.617.2.4") ~ "Delta (AY.4)",
      full_tree %>% stringr::str_starts("B.1.617.2") ~ "Delta (B.1.617.2)",
      full_tree %>% stringr::str_starts("B.1.1.529.2.86") ~ "Pirola (BA.2.86)",
      full_tree %>% stringr::str_starts("B.1.1.529.2") ~ "Omicron (BA.2)",
      full_tree %>% stringr::str_starts("B.1.1.529.4") ~ "Omicron (BA.4)",
      full_tree %>% stringr::str_starts("B.1.1.529.5") ~ "Omicron (BA.5)",
      full_tree %>% stringr::str_starts("B.1.1.529") ~ "Omicron (Other)",
      full_tree %>% stringr::str_starts("XBB.1.5") ~ "Kraken (XBB.1.5)",
      full_tree %>% stringr::str_starts("XBB.1.16") ~ "Arcturus (XBB.1.16)",
      full_tree %>% stringr::str_starts("XBB.1.9.2.5.1") ~ "Eris (EG.5.1)",
      full_tree %>% stringr::str_starts("XBB") ~ "XBB (Other)",
      TRUE ~ "Other"
    ) %>% factor(c(
      "Other", "Alpha (B.1.1.7)", "Delta (B.1.617.2)", "Delta (AY.4)",
      "Omicron (Other)","Omicron (BA.2)","Omicron (BA.4)","Omicron (BA.5)",
      "XBB (Other)", "Kraken (XBB.1.5)","Arcturus (XBB.1.16)","Eris (EG.5.1)",
      "Pirola (BA.2.86)"
    )),
    who_class = dplyr::case_when(
      is.na(Lineage) ~ "Unclassified",
      full_tree %>% stringr::str_starts("B.1.1.7") ~ "Alpha",
      full_tree %>% stringr::str_starts("B.1.617.2") ~ "Delta",
      full_tree %>% stringr::str_starts("B.1.1.529.2.86") ~ "Pirola",
      full_tree %>% stringr::str_starts("B.1.1.529") ~ "Omicron",
      full_tree %>% stringr::str_starts("XBB.1.5") ~ "Kraken",
      full_tree %>% stringr::str_starts("XBB.1.16") ~ "Arcturus",
      full_tree %>% stringr::str_starts("XBB.1.9.2.5.1") ~ "Eris",
      TRUE ~ "Other"
    ) %>% factor(c(
      "Other", "Alpha", "Delta", "Omicron", "Kraken","Arcturus","Eris","Pirola"
    ))
  ) #%>% dplyr::select(-prefix,-suffix,-expanded)


# The count data is at LTLA level. Firstly lets aggregate to England (LTLAs) that
# start with "E"
england_variants = variants2 %>%
  dplyr::filter(lineage_class != "Unclassified") %>%
  dplyr::filter(stringr::str_starts(LTLA,"E")) %>%
  dplyr::select(
    date = WeekEndDate,
    class = lineage_class,
    who_class,
    count = Count
  ) %>%
  dplyr::mutate(
    time = ggoutbreak::as.time_period(date,unit="1 day",anchor="start")
  ) %>%
  dplyr::group_by(date,time,class,who_class) %>%
  dplyr::summarise(count = sum(count,na.rm = TRUE)) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    denom = sum(count)
  ) %>%
  dplyr::group_by(class)

if(interactive()) interfacer::use_dataframe(england_variants)
