``` r
knitr::opts_chunk$set(echo = TRUE, include = TRUE, message=FALSE, warning=FALSE)

library(tidyverse) # for data manipulation
library(googlesheets4) # Read raw data stored in a google sheet document
library(vcd) # Calculation reliability statistics, e.g. Kappa
library(gt) # This packages supports the creation of tables

fct_label_inorder <- c("Equivalence of meaning; lexical, as well as conceptual",
                       "Equivalence of meaning, but with synonymy.",
                       "Source concept is broader and has a less specific meaning than the target concept",
                       "Source concept is narrower and has a more specific meaning than the target concept",
                       "No map is possible")

# Data preprocessing
# Among other tasks: relabels factors
concepts <- read_csv(file = "./data/mapping-results.csv", col_types = cols(.default = "c")) %>% 
  mutate(kapitel = stringr::str_extract(string = chapter, pattern = "^\\d{2}")) %>% 
  mutate_at("kapitelbezeichnung", stringr::str_to_title) %>% 
  mutate(kapitelnummer = str_extract(string = chapter, pattern = "^\\d\\d")) %>% 
  mutate(kapitelbezeichnung = glue::glue("{kapitelnummer} {kapitelbezeichnung}")) %>% 
  select(-kapitelnummer) %>% 
  mutate_at(.vars = c("equ_jens", "equ_mareike", "equi_final"), 
            .funs = ~ factor(x = .x, levels = fct_label_inorder, labels = fct_label_inorder, ordered = TRUE))
```

Reliabilität Mapping
====================

``` r
# Restructure/ Pivot Data

mapping <- concepts %>% 
  select(id, id_mapper, kapitel, kapitelbezeichnung, bezeichnung, snomed_code, kapitel) %>% 
  pivot_wider(id_cols = c("id", "kapitelbezeichnung", "bezeichnung", "kapitel"),
              names_from = "id_mapper",
              values_from = "snomed_code")

# Caluclate Kappa values

# Kappa Overall
kappa_overall <- irr::kappam.fleiss(mapping[, c("9f6", "83f", "605")])

# Kappa by Chapter
by_chapter <- mapping %>% 
  select(-id, -bezeichnung) %>% 
  mutate_at("kapitelbezeichnung", stringr::str_to_title) %>% 
  nest(data = c(`9f6`, `83f`, `605`)) %>% 
  mutate(kappa = map(data, ~ irr::kappam.fleiss(ratings = .x))) %>% 
  mutate(kappa_val = map_dbl(kappa, pluck, "value")) %>% 
  mutate(kappa_subjects = map_dbl(kappa, pluck, "subjects")) %>% 
  mutate(group = "By Chapter")

overall <- tibble(group = "Overall",
       kappa_val = kappa_overall$value,
       kappa_subjects = kappa_overall$subjects) 

# Calculate Agreement
rater <- mapping %>% 
  set_names(c("id", "kapitelbezeichnung", "bezeichnung", "kapitel", "phil", "lisa", "anne")) 
  
agreement <- rater %>% 
  mutate(phil_anne = phil == anne) %>% 
  mutate(anne_lisa = anne == lisa) %>% 
  mutate(phil_lisa = phil == lisa) %>% 
  mutate(agreement = (phil_anne + anne_lisa + phil_lisa) / 3) 

overall <- agreement %>% 
  summarise(agreement = mean(agreement)) %>% 
  bind_cols(overall)

by_chapter <- agreement %>% 
  mutate_at("kapitelbezeichnung", stringr::str_to_title) %>% 
  group_by(kapitelbezeichnung) %>% 
  summarise(agreement = mean(agreement)) %>% 
  full_join(by_chapter, by = "kapitelbezeichnung") %>% 
  select(kapitelbezeichnung, data, kappa, agreement, everything())

mapping_results <- by_chapter %>% 
  select(-data, -kappa) %>% 
  bind_rows(overall)
  
mapping_results %>% 
  mutate_at("kapitelbezeichnung", stringr::str_to_title) %>% 
  set_names(c("Chapter", "Percent", "Chapter_Nr", "Fleiss-Kappa", "Number of Items", "Group")) %>% 
  mutate(Chapter = glue::glue("{Chapter_Nr} - {Chapter}", .na =  NULL)) %>% 
  select(-Chapter_Nr) %>% 
  arrange(Chapter) %>% 
  mutate_at("Chapter", ~ if_else(condition = is.na(.x), true = "", false = as.character(.x))) %>% 
  gt::gt(rowname_col = "Chapter", groupname_col = "Group") %>%
  gt::fmt_percent(columns = vars("Percent"), decimals = 2) %>% 
  gt::fmt(columns = vars("Fleiss-Kappa"), fns = function(x) sprintf(x, fmt = '%#.3f')) %>% 
  gt::tab_style(
    style = list(
      gt::cell_text(align = "center") 
    ),
    locations = gt::cells_body()  
  ) %>% 
  gt::tab_spanner(label = "Agreement", columns = matches("Percent|Fleiss")) %>% 
  gt::tab_header(
    title = gt::md("Reliability of the Mapping between three Mappers"),
  )
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#sybcntlzko .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#sybcntlzko .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#sybcntlzko .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#sybcntlzko .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#sybcntlzko .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sybcntlzko .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#sybcntlzko .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#sybcntlzko .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#sybcntlzko .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#sybcntlzko .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#sybcntlzko .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#sybcntlzko .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#sybcntlzko .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#sybcntlzko .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#sybcntlzko .gt_from_md > :first-child {
  margin-top: 0;
}

#sybcntlzko .gt_from_md > :last-child {
  margin-bottom: 0;
}

#sybcntlzko .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#sybcntlzko .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#sybcntlzko .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sybcntlzko .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#sybcntlzko .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sybcntlzko .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#sybcntlzko .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sybcntlzko .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#sybcntlzko .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#sybcntlzko .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#sybcntlzko .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#sybcntlzko .gt_left {
  text-align: left;
}

#sybcntlzko .gt_center {
  text-align: center;
}

#sybcntlzko .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#sybcntlzko .gt_font_normal {
  font-weight: normal;
}

#sybcntlzko .gt_font_bold {
  font-weight: bold;
}

#sybcntlzko .gt_font_italic {
  font-style: italic;
}

#sybcntlzko .gt_super {
  font-size: 65%;
}

#sybcntlzko .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="sybcntlzko" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<table class="gt_table">
<thead class="gt_header">
<tr>
<th colspan="4" class="gt_heading gt_title gt_font_normal" style>
Reliability of the Mapping between three Mappers
</th>
</tr>
<tr>
<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>
</th>
</tr>
</thead>
<thead class="gt_col_headings">
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">
</th>
<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">
<span class="gt_column_spanner">Agreement</span>
</th>
<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">
Number of Items
</th>
</tr>
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
Percent
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
Fleiss-Kappa
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="gt_group_heading_row">
<td colspan="4" class="gt_group_heading">
By Chapter
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
01 - 01 01 Stammdaten
</td>
<td class="gt_row gt_right" style="text-align: center;">
60.78%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.575
</td>
<td class="gt_row gt_right" style="text-align: center;">
34
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
02 - 02 02 Allgemeinstatus
</td>
<td class="gt_row gt_right" style="text-align: center;">
75.76%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.754
</td>
<td class="gt_row gt_right" style="text-align: center;">
66
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
03 - 03 03 Wundanamnese
</td>
<td class="gt_row gt_right" style="text-align: center;">
58.33%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.568
</td>
<td class="gt_row gt_right" style="text-align: center;">
24
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
04 - 04 04 Wundstatus
</td>
<td class="gt_row gt_right" style="text-align: center;">
38.60%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.366
</td>
<td class="gt_row gt_right" style="text-align: center;">
57
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
05 - 05 05 Diagnostik
</td>
<td class="gt_row gt_right" style="text-align: center;">
33.33%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.280
</td>
<td class="gt_row gt_right" style="text-align: center;">
14
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
06 - 06 06 Therapie
</td>
<td class="gt_row gt_right" style="text-align: center;">
39.73%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.367
</td>
<td class="gt_row gt_right" style="text-align: center;">
73
</td>
</tr>
<tr class="gt_group_heading_row">
<td colspan="4" class="gt_group_heading">
Overall
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
</td>
<td class="gt_row gt_right" style="text-align: center;">
52.36%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.512
</td>
<td class="gt_row gt_right" style="text-align: center;">
268
</td>
</tr>
</tbody>
</table>
</div>
<!--/html_preserve-->
Reliability of the Equivalence Testing
======================================

``` r
equ <- concepts %>% 
  select(id, kapitelbezeichnung, kapitel, equ_jens, equ_mareike) %>%
  mutate_at("kapitelbezeichnung", stringr::str_to_title) %>% 
  mutate(Chapter = glue::glue("{kapitel} - {kapitelbezeichnung}", .na =  NULL)) %>% 
  select(-kapitelbezeichnung, -kapitel)

overall_equ <- irr::kappam.fleiss(equ[, c("equ_jens", "equ_mareike")])

by_chapter_equ <- equ %>% 
  nest(data = c(id, equ_jens, equ_mareike)) %>% 
  mutate(kappa = map(data, ~ irr::kappam.fleiss(ratings = .x[, c("equ_jens", "equ_mareike")]))) %>% 
  mutate(kappa_val = map_dbl(kappa, pluck, "value")) %>% 
  mutate(kappa_subjects = map_dbl(kappa, pluck, "subjects")) %>% 
  unnest(data) %>% 
  mutate(agreement = equ_jens == equ_mareike) %>% 
  group_by(Chapter) %>% 
  mutate(agreement = sum(agreement) / length(agreement)) %>% 
  summarise_at(c("kappa_val", "kappa_subjects", "agreement"), .funs = mean) %>% 
  mutate(Group = "By Chapter")

overall_equ <- tibble(
  Chapter = "",
  agreement = sum(equ$equ_jens == equ$equ_mareike) / nrow(equ),
  kappa_val = overall_equ$value,
  kappa_subjects = overall_equ$subjects,
  Group = "Overall"
)

by_chapter_equ %>% 
  mutate_at("Chapter", as.character) %>% 
  bind_rows(overall_equ) %>% 
  select(Chapter, Percent = agreement, `Fleiss-Kappa` = kappa_val, `Number of Items` = kappa_subjects, Group) %>% 
  gt::gt(rowname_col = "Chapter", groupname_col = "Group") %>%
  gt::fmt_percent(columns = vars("Percent"), decimals = 2) %>% 
  gt::fmt(columns = vars("Fleiss-Kappa"), fns = function(x) sprintf(x, fmt = '%#.3f')) %>% 
  gt::tab_style(
    style = list(
      gt::cell_text(align = "center") 
    ),
    locations = gt::cells_body()  
  ) %>% 
  gt::tab_spanner(label = "Agreement", columns = matches("Percent|Fleiss")) %>% 
  gt::tab_header(
    title = gt::md("Reliability of the Equivalence Rating between two Rater"),
  )
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#eejhmqhdud .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#eejhmqhdud .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eejhmqhdud .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#eejhmqhdud .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#eejhmqhdud .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eejhmqhdud .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eejhmqhdud .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#eejhmqhdud .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#eejhmqhdud .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#eejhmqhdud .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#eejhmqhdud .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#eejhmqhdud .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#eejhmqhdud .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#eejhmqhdud .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#eejhmqhdud .gt_from_md > :first-child {
  margin-top: 0;
}

#eejhmqhdud .gt_from_md > :last-child {
  margin-bottom: 0;
}

#eejhmqhdud .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#eejhmqhdud .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#eejhmqhdud .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eejhmqhdud .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#eejhmqhdud .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eejhmqhdud .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#eejhmqhdud .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eejhmqhdud .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eejhmqhdud .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#eejhmqhdud .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eejhmqhdud .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#eejhmqhdud .gt_left {
  text-align: left;
}

#eejhmqhdud .gt_center {
  text-align: center;
}

#eejhmqhdud .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#eejhmqhdud .gt_font_normal {
  font-weight: normal;
}

#eejhmqhdud .gt_font_bold {
  font-weight: bold;
}

#eejhmqhdud .gt_font_italic {
  font-style: italic;
}

#eejhmqhdud .gt_super {
  font-size: 65%;
}

#eejhmqhdud .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="eejhmqhdud" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<table class="gt_table">
<thead class="gt_header">
<tr>
<th colspan="4" class="gt_heading gt_title gt_font_normal" style>
Reliability of the Equivalence Rating between two Rater
</th>
</tr>
<tr>
<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>
</th>
</tr>
</thead>
<thead class="gt_col_headings">
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">
</th>
<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">
<span class="gt_column_spanner">Agreement</span>
</th>
<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">
Number of Items
</th>
</tr>
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
Percent
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
Fleiss-Kappa
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="gt_group_heading_row">
<td colspan="4" class="gt_group_heading">
By Chapter
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
01 - 01 01 Stammdaten
</td>
<td class="gt_row gt_right" style="text-align: center;">
83.33%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.772
</td>
<td class="gt_row gt_right" style="text-align: center;">
102
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
02 - 02 02 Allgemeinstatus
</td>
<td class="gt_row gt_right" style="text-align: center;">
89.90%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.835
</td>
<td class="gt_row gt_right" style="text-align: center;">
198
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
03 - 03 03 Wundanamnese
</td>
<td class="gt_row gt_right" style="text-align: center;">
70.83%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.583
</td>
<td class="gt_row gt_right" style="text-align: center;">
72
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
04 - 04 04 Wundstatus
</td>
<td class="gt_row gt_right" style="text-align: center;">
74.27%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.641
</td>
<td class="gt_row gt_right" style="text-align: center;">
171
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
05 - 05 05 Diagnostik
</td>
<td class="gt_row gt_right" style="text-align: center;">
59.52%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.408
</td>
<td class="gt_row gt_right" style="text-align: center;">
42
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
06 - 06 06 Therapie
</td>
<td class="gt_row gt_right" style="text-align: center;">
75.34%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.667
</td>
<td class="gt_row gt_right" style="text-align: center;">
219
</td>
</tr>
<tr class="gt_group_heading_row">
<td colspan="4" class="gt_group_heading">
Overall
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
</td>
<td class="gt_row gt_right" style="text-align: center;">
78.48%
</td>
<td class="gt_row gt_right" style="text-align: center;">
0.702
</td>
<td class="gt_row gt_right" style="text-align: center;">
804
</td>
</tr>
</tbody>
</table>
</div>
<!--/html_preserve-->
Coverage
========

``` r
coverage_results <- concepts %>% 
  group_by(id) %>% 
  filter(finales_konzept == min(finales_konzept)) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(equi_final) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(proportion = n / sum(n)) %>% 
  mutate(map = c("Map", "Map", "Map", "Map", "No Map")) %>% 
  group_by(map) %>% 
  mutate(proportion_map = sum(proportion)) %>% 
  mutate(n_map = sum(n)) 
 
# Vielviel Prozent der Konzepte können insgesamt abgedeckt werden?
coverage_results %>% 
  rename("Number of Items" = n, "Proportion" = proportion) %>% 
  select(-n_map, -proportion_map) %>% 
  gt(rowname_col = "equi_final", groupname_col = "map") %>% 
  fmt_percent(columns = starts_with("proportion"), decimals = 2) %>%
  tab_spanner(label = "Equivalence Rating", columns = vars("Number of Items", "Proportion")) %>% 
  gt::tab_style(
    style = list(
      gt::cell_text(align = "center") 
    ),
    locations = gt::cells_body()  
  )
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hkaboivwyg .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hkaboivwyg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hkaboivwyg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hkaboivwyg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hkaboivwyg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hkaboivwyg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hkaboivwyg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hkaboivwyg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hkaboivwyg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hkaboivwyg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hkaboivwyg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hkaboivwyg .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#hkaboivwyg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hkaboivwyg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hkaboivwyg .gt_from_md > :first-child {
  margin-top: 0;
}

#hkaboivwyg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hkaboivwyg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hkaboivwyg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#hkaboivwyg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hkaboivwyg .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hkaboivwyg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hkaboivwyg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hkaboivwyg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hkaboivwyg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hkaboivwyg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hkaboivwyg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hkaboivwyg .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hkaboivwyg .gt_left {
  text-align: left;
}

#hkaboivwyg .gt_center {
  text-align: center;
}

#hkaboivwyg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hkaboivwyg .gt_font_normal {
  font-weight: normal;
}

#hkaboivwyg .gt_font_bold {
  font-weight: bold;
}

#hkaboivwyg .gt_font_italic {
  font-style: italic;
}

#hkaboivwyg .gt_super {
  font-size: 65%;
}

#hkaboivwyg .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="hkaboivwyg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<table class="gt_table">
<thead class="gt_col_headings">
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">
</th>
<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">
<span class="gt_column_spanner">Equivalence Rating</span>
</th>
</tr>
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
Number of Items
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
Proportion
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="gt_group_heading_row">
<td colspan="3" class="gt_group_heading">
Map
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Equivalence of meaning; lexical, as well as conceptual
</td>
<td class="gt_row gt_center" style="text-align: center;">
117
</td>
<td class="gt_row gt_right" style="text-align: center;">
43.66%
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Equivalence of meaning, but with synonymy.
</td>
<td class="gt_row gt_center" style="text-align: center;">
63
</td>
<td class="gt_row gt_right" style="text-align: center;">
23.51%
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Source concept is broader and has a less specific meaning than the
target concept
</td>
<td class="gt_row gt_center" style="text-align: center;">
9
</td>
<td class="gt_row gt_right" style="text-align: center;">
3.36%
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Source concept is narrower and has a more specific meaning than the
target concept
</td>
<td class="gt_row gt_center" style="text-align: center;">
30
</td>
<td class="gt_row gt_right" style="text-align: center;">
11.19%
</td>
</tr>
<tr class="gt_group_heading_row">
<td colspan="3" class="gt_group_heading">
No Map
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
No map is possible
</td>
<td class="gt_row gt_center" style="text-align: center;">
49
</td>
<td class="gt_row gt_right" style="text-align: center;">
18.28%
</td>
</tr>
</tbody>
</table>
</div>
<!--/html_preserve-->
``` r
results_equivalence <- concepts %>% 
  group_by(id) %>% 
  filter(finales_konzept == min(finales_konzept)) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(equi_final, kapitelbezeichnung) %>% 
  count() %>% 
  group_by(kapitelbezeichnung) %>% 
  mutate(proportion = n / sum(n)) %>% 
  arrange(kapitelbezeichnung, equi_final) %>% 
  ungroup()
  
overall_equ_results <- results_equivalence %>% 
  group_by(equi_final) %>% 
  select(-proportion) %>% 
  summarize(n = sum(n)) %>% 
  ungroup %>% 
  mutate(proportion = n / sum(n)) %>% 
  mutate(kapitelbezeichnung = "Overall") %>% 
  mutate(section = "Overall") %>% 
  mutate_at("proportion", ~ scales::percent(x = .x, accuracy = 0.1)) %>% 
  mutate(n_prop = glue::glue("{proportion} (n={n})")) %>% 
  select(Overall = n_prop)
  

results_equivalence %>% 
  mutate_at("proportion", ~ scales::percent(x = .x, accuracy = 0.1)) %>% 
  mutate(n_prop = glue::glue("{proportion} (n={n})")) %>% 
  select(-n, -proportion) %>% 
  pivot_wider(id_cols = equi_final, names_from = kapitelbezeichnung, values_from = n_prop, values_fill = list(n_prop = "-")) %>%
  mutate_all(~ stringr::str_replace(string = .x, pattern = "\\% \\(", replacement = "%\n(")) %>% 
  bind_cols(overall_equ_results) %>% 
  select(equi_final, Overall, dplyr::everything()) %>% 
  gt::gt(rowname_col = "equi_final") %>% 
  tab_spanner(label = "Chapter", columns = matches("^\\d\\d")) %>% 
  gt::tab_stubhead(label = "Equivalence Categories") %>% 
  gt::tab_style(
    style = list(
      gt::cell_text(align = "center") 
    ),
    locations = gt::cells_body()  
  )
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#yvpzytbmgv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#yvpzytbmgv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yvpzytbmgv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#yvpzytbmgv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#yvpzytbmgv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yvpzytbmgv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yvpzytbmgv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#yvpzytbmgv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#yvpzytbmgv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yvpzytbmgv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yvpzytbmgv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#yvpzytbmgv .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#yvpzytbmgv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#yvpzytbmgv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yvpzytbmgv .gt_from_md > :first-child {
  margin-top: 0;
}

#yvpzytbmgv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yvpzytbmgv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#yvpzytbmgv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#yvpzytbmgv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yvpzytbmgv .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#yvpzytbmgv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yvpzytbmgv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yvpzytbmgv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yvpzytbmgv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yvpzytbmgv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#yvpzytbmgv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yvpzytbmgv .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#yvpzytbmgv .gt_left {
  text-align: left;
}

#yvpzytbmgv .gt_center {
  text-align: center;
}

#yvpzytbmgv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yvpzytbmgv .gt_font_normal {
  font-weight: normal;
}

#yvpzytbmgv .gt_font_bold {
  font-weight: bold;
}

#yvpzytbmgv .gt_font_italic {
  font-style: italic;
}

#yvpzytbmgv .gt_super {
  font-size: 65%;
}

#yvpzytbmgv .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="yvpzytbmgv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<table class="gt_table">
<thead class="gt_col_headings">
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">
Equivalence Categories
</th>
<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">
Overall
</th>
<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6">
<span class="gt_column_spanner">Chapter</span>
</th>
</tr>
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
01 01 Stammdaten
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
02 02 Allgemeinstatus
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
03 03 Wundanamnese
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
04 04 Wundstatus
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
05 05 Diagnostik
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
06 06 Therapie
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_left gt_stub">
Equivalence of meaning; lexical, as well as conceptual
</td>
<td class="gt_row gt_center" style="text-align: center;">
43.7% (n=117)
</td>
<td class="gt_row gt_left" style="text-align: center;">
23.5% (n=8)
</td>
<td class="gt_row gt_left" style="text-align: center;">
59.1% (n=39)
</td>
<td class="gt_row gt_left" style="text-align: center;">
50.0% (n=12)
</td>
<td class="gt_row gt_left" style="text-align: center;">
43.9% (n=25)
</td>
<td class="gt_row gt_left" style="text-align: center;">
35.7% (n=5)
</td>
<td class="gt_row gt_left" style="text-align: center;">
38.4% (n=28)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Equivalence of meaning, but with synonymy.
</td>
<td class="gt_row gt_center" style="text-align: center;">
23.5% (n=63)
</td>
<td class="gt_row gt_left" style="text-align: center;">
26.5% (n=9)
</td>
<td class="gt_row gt_left" style="text-align: center;">
24.2% (n=16)
</td>
<td class="gt_row gt_left" style="text-align: center;">
25.0% (n=6)
</td>
<td class="gt_row gt_left" style="text-align: center;">
21.1% (n=12)
</td>
<td class="gt_row gt_left" style="text-align: center;">
21.4% (n=3)
</td>
<td class="gt_row gt_left" style="text-align: center;">
23.3% (n=17)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Source concept is broader and has a less specific meaning than the
target concept
</td>
<td class="gt_row gt_center" style="text-align: center;">
3.4% (n=9)
</td>
<td class="gt_row gt_left" style="text-align: center;">
2.9% (n=1)
</td>
<td class="gt_row gt_left" style="text-align: center;">
6.1% (n=4)
</td>
<td class="gt_row gt_left" style="text-align: center;">
4.2% (n=1)
</td>
<td class="gt_row gt_left" style="text-align: center;">
1.8% (n=1)
</td>
<td class="gt_row gt_left" style="text-align: center;">
\-
</td>
<td class="gt_row gt_left" style="text-align: center;">
2.7% (n=2)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Source concept is narrower and has a more specific meaning than the
target concept
</td>
<td class="gt_row gt_center" style="text-align: center;">
11.2% (n=30)
</td>
<td class="gt_row gt_left" style="text-align: center;">
11.8% (n=4)
</td>
<td class="gt_row gt_left" style="text-align: center;">
4.5% (n=3)
</td>
<td class="gt_row gt_left" style="text-align: center;">
\-
</td>
<td class="gt_row gt_left" style="text-align: center;">
12.3% (n=7)
</td>
<td class="gt_row gt_left" style="text-align: center;">
21.4% (n=3)
</td>
<td class="gt_row gt_left" style="text-align: center;">
17.8% (n=13)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
No map is possible
</td>
<td class="gt_row gt_center" style="text-align: center;">
18.3% (n=49)
</td>
<td class="gt_row gt_left" style="text-align: center;">
35.3% (n=12)
</td>
<td class="gt_row gt_left" style="text-align: center;">
6.1% (n=4)
</td>
<td class="gt_row gt_left" style="text-align: center;">
20.8% (n=5)
</td>
<td class="gt_row gt_left" style="text-align: center;">
21.1% (n=12)
</td>
<td class="gt_row gt_left" style="text-align: center;">
21.4% (n=3)
</td>
<td class="gt_row gt_left" style="text-align: center;">
17.8% (n=13)
</td>
</tr>
</tbody>
</table>
</div>
<!--/html_preserve-->
``` r
results_equivalence <- results_equivalence %>% 
  mutate(equi_simple = as.integer(equi_final)) %>% 
  mutate(equi_simple = case_when(
    equi_simple == 1 ~ "1",
    equi_simple == 2 ~ "1",
    equi_simple == 3 ~ "2",
    equi_simple == 4 ~ "2",
    equi_simple == 5 ~ "3"
  )) %>% 
  mutate(equi_simple = factor(x = equi_simple,
                              levels = 1:3, 
                              labels = c("Semantic Match present (Degree 1 and 2)",
                                         "Semantic Asymmetry present (Degree 3 and 4)",
                                         "Semantic Match absent (Degree 5)"),
                              ordered = TRUE
                              )) 
overall_equ_results_simple <- results_equivalence %>% 
  group_by(equi_simple) %>% 
  select(-proportion) %>% 
  summarize(n = sum(n)) %>% 
  ungroup %>% 
  mutate(proportion = n / sum(n)) %>% 
  mutate(kapitelbezeichnung = "Overall") %>% 
  mutate(section = "Overall") %>% 
  mutate_at("proportion", ~ scales::percent(x = .x, accuracy = 0.1)) %>% 
  mutate(n_prop = glue::glue("{proportion} (n={n})")) %>% 
  select(Overall = n_prop)
  

results_equivalence %>% 
  group_by(equi_simple, kapitelbezeichnung) %>% 
  summarise(n = sum(n), proportion = sum(proportion)) %>% 
  mutate_at("proportion", ~ scales::percent(x = .x, accuracy = 0.1)) %>% 
  mutate(n_prop = glue::glue("{proportion} (n={n})")) %>% 
  select(-n, -proportion) %>% 
  pivot_wider(id_cols = equi_simple, names_from = kapitelbezeichnung, values_from = n_prop, values_fill = list(n_prop = "-")) %>%
  ungroup() %>% 
  mutate_all(~ stringr::str_replace(string = .x, pattern = "\\% \\(", replacement = "%\n(")) %>% 
  bind_cols(overall_equ_results_simple) %>% 
  select(equi_simple, Overall, dplyr::everything()) %>% 
  gt::gt(rowname_col = "equi_simple") %>% 
  tab_spanner(label = "Chapter", columns = matches("^\\d\\d")) %>% 
  gt::tab_stubhead(label = "Equivalence Categories") %>% 
  gt::tab_style(
    style = list(
      gt::cell_text(align = "center") 
    ),
    locations = gt::cells_body()  
  )
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#bffixxagkd .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#bffixxagkd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bffixxagkd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#bffixxagkd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#bffixxagkd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bffixxagkd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bffixxagkd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#bffixxagkd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#bffixxagkd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bffixxagkd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bffixxagkd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#bffixxagkd .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#bffixxagkd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#bffixxagkd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bffixxagkd .gt_from_md > :first-child {
  margin-top: 0;
}

#bffixxagkd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bffixxagkd .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#bffixxagkd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#bffixxagkd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bffixxagkd .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#bffixxagkd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bffixxagkd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bffixxagkd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bffixxagkd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bffixxagkd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#bffixxagkd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bffixxagkd .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#bffixxagkd .gt_left {
  text-align: left;
}

#bffixxagkd .gt_center {
  text-align: center;
}

#bffixxagkd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bffixxagkd .gt_font_normal {
  font-weight: normal;
}

#bffixxagkd .gt_font_bold {
  font-weight: bold;
}

#bffixxagkd .gt_font_italic {
  font-style: italic;
}

#bffixxagkd .gt_super {
  font-size: 65%;
}

#bffixxagkd .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="bffixxagkd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<table class="gt_table">
<thead class="gt_col_headings">
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">
Equivalence Categories
</th>
<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">
Overall
</th>
<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6">
<span class="gt_column_spanner">Chapter</span>
</th>
</tr>
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
01 01 Stammdaten
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
02 02 Allgemeinstatus
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
03 03 Wundanamnese
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
04 04 Wundstatus
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
05 05 Diagnostik
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
06 06 Therapie
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_left gt_stub">
Semantic Match present (Degree 1 and 2)
</td>
<td class="gt_row gt_center" style="text-align: center;">
67.2% (n=180)
</td>
<td class="gt_row gt_left" style="text-align: center;">
50.0% (n=17)
</td>
<td class="gt_row gt_left" style="text-align: center;">
83.3% (n=55)
</td>
<td class="gt_row gt_left" style="text-align: center;">
75.0% (n=18)
</td>
<td class="gt_row gt_left" style="text-align: center;">
64.9% (n=37)
</td>
<td class="gt_row gt_left" style="text-align: center;">
57.1% (n=8)
</td>
<td class="gt_row gt_left" style="text-align: center;">
61.6% (n=45)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Semantic Asymmetry present (Degree 3 and 4)
</td>
<td class="gt_row gt_center" style="text-align: center;">
14.6% (n=39)
</td>
<td class="gt_row gt_left" style="text-align: center;">
14.7% (n=5)
</td>
<td class="gt_row gt_left" style="text-align: center;">
10.6% (n=7)
</td>
<td class="gt_row gt_left" style="text-align: center;">
4.2% (n=1)
</td>
<td class="gt_row gt_left" style="text-align: center;">
14.0% (n=8)
</td>
<td class="gt_row gt_left" style="text-align: center;">
21.4% (n=3)
</td>
<td class="gt_row gt_left" style="text-align: center;">
20.5% (n=15)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
Semantic Match absent (Degree 5)
</td>
<td class="gt_row gt_center" style="text-align: center;">
18.3% (n=49)
</td>
<td class="gt_row gt_left" style="text-align: center;">
35.3% (n=12)
</td>
<td class="gt_row gt_left" style="text-align: center;">
6.1% (n=4)
</td>
<td class="gt_row gt_left" style="text-align: center;">
20.8% (n=5)
</td>
<td class="gt_row gt_left" style="text-align: center;">
21.1% (n=12)
</td>
<td class="gt_row gt_left" style="text-align: center;">
21.4% (n=3)
</td>
<td class="gt_row gt_left" style="text-align: center;">
17.8% (n=13)
</td>
</tr>
</tbody>
</table>
</div>
<!--/html_preserve-->
``` r
concepts %>% 
  group_by(id) %>% 
  filter(finales_konzept == min(finales_konzept)) %>% 
  distinct(id, .keep_all = TRUE)
```

    ## # A tibble: 268 x 15
    ## # Groups:   id [268]
    ##    id    id_mapper chapter kapitelbezeichn… bezeichnung finaler_beschlu…
    ##    <chr> <chr>     <chr>   <glue>           <chr>       <chr>           
    ##  1 1     9f6       01-01   01 01 Stammdaten Geburtsdat… ddmmyyyy        
    ##  2 2     605       01-02   01 01 Stammdaten Geschlecht  1 „männlich“    
    ##  3 3     9f6       01-02   01 01 Stammdaten Geschlecht  2 „weiblich“    
    ##  4 4     9f6       01-02   01 01 Stammdaten Geschlecht  Geschlecht      
    ##  5 5     605       01-03   01 01 Stammdaten Versichert… 1 „GKV ohne Zus…
    ##  6 6     605       01-03   01 01 Stammdaten Versichert… 2 „GKV mit staa…
    ##  7 7     605       01-03   01 01 Stammdaten Versichert… 3 „PKV“         
    ##  8 8     605       01-03   01 01 Stammdaten Versichert… 4 „nicht krankv…
    ##  9 9     605       01-03   01 01 Stammdaten Versichert… 5 „anders krank…
    ## 10 10    83f       01-03   01 01 Stammdaten Versichert… Versichertensta…
    ## # … with 258 more rows, and 9 more variables: snomed_code <chr>,
    ## #   descriptor <chr>, equ_jens <ord>, equ_mareike <ord>, equi_final <ord>,
    ## #   finales_konzept <chr>, agreement <chr>, map <chr>, kapitel <chr>

``` r
results_equivalence_agg <- results_equivalence %>% 
  mutate_at(.vars = "equi_final", 
            .funs =  ~ if_else(
              condition = .x == "No map is possible",
              true = "No",
              false = "Yes"
            )) 

coverage_overall <- results_equivalence_agg %>% 
  group_by(equi_final) %>% 
  summarise(n = sum(n), proportion = sum(proportion)) %>% 
  mutate(kapitelbezeichnung = "") %>% 
  mutate(group = "Overall") %>% 
  mutate(proportion = n / sum(n)) %>% 
  mutate(proportion = glue::glue("{scales::percent(proportion, accuracy = .01)} (n={n})")) %>% 
  select(-n) %>% 
  pivot_wider(id_cols = c("kapitelbezeichnung", "group"), names_from = equi_final, values_from = proportion)

coverage_by_chapter <- results_equivalence_agg %>% 
  group_by(equi_final, kapitelbezeichnung) %>% 
  summarise(n = sum(n), proportion = sum(proportion)) %>% 
  mutate(group = "By Chapter") %>% 
  arrange(kapitelbezeichnung) %>% 
  mutate(proportion = glue::glue("{scales::percent(proportion, accuracy = .01)} (n={n})")) %>% 
  mutate_at(c("kapitelbezeichnung", "proportion"), as.character) %>% 
  select(-n) %>% 
  pivot_wider(id_cols = c("kapitelbezeichnung", "group"), names_from = equi_final, values_from = proportion)

coverage_by_chapter %>% 
  bind_rows(coverage_overall) %>% 
  gt::gt(rowname_col = "kapitelbezeichnung", groupname_col = "group") %>% 
  gt::tab_spanner(label = "SNOMED CT identified", columns = c("No", "Yes"))
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#pqtithxxlu .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#pqtithxxlu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pqtithxxlu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#pqtithxxlu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#pqtithxxlu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pqtithxxlu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pqtithxxlu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#pqtithxxlu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#pqtithxxlu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pqtithxxlu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pqtithxxlu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#pqtithxxlu .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#pqtithxxlu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#pqtithxxlu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pqtithxxlu .gt_from_md > :first-child {
  margin-top: 0;
}

#pqtithxxlu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pqtithxxlu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#pqtithxxlu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#pqtithxxlu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pqtithxxlu .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#pqtithxxlu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pqtithxxlu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pqtithxxlu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pqtithxxlu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pqtithxxlu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#pqtithxxlu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pqtithxxlu .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#pqtithxxlu .gt_left {
  text-align: left;
}

#pqtithxxlu .gt_center {
  text-align: center;
}

#pqtithxxlu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pqtithxxlu .gt_font_normal {
  font-weight: normal;
}

#pqtithxxlu .gt_font_bold {
  font-weight: bold;
}

#pqtithxxlu .gt_font_italic {
  font-style: italic;
}

#pqtithxxlu .gt_super {
  font-size: 65%;
}

#pqtithxxlu .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="pqtithxxlu" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<table class="gt_table">
<thead class="gt_col_headings">
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">
</th>
<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">
<span class="gt_column_spanner">SNOMED CT identified</span>
</th>
</tr>
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
No
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">
Yes
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="gt_group_heading_row">
<td colspan="3" class="gt_group_heading">
By Chapter
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
01 01 Stammdaten
</td>
<td class="gt_row gt_center">
35.29% (n=12)
</td>
<td class="gt_row gt_center">
64.71% (n=22)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
02 02 Allgemeinstatus
</td>
<td class="gt_row gt_center">
6.06% (n=4)
</td>
<td class="gt_row gt_center">
93.94% (n=62)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
03 03 Wundanamnese
</td>
<td class="gt_row gt_center">
20.83% (n=5)
</td>
<td class="gt_row gt_center">
79.17% (n=19)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
04 04 Wundstatus
</td>
<td class="gt_row gt_center">
21.05% (n=12)
</td>
<td class="gt_row gt_center">
78.95% (n=45)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
05 05 Diagnostik
</td>
<td class="gt_row gt_center">
21.43% (n=3)
</td>
<td class="gt_row gt_center">
78.57% (n=11)
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
06 06 Therapie
</td>
<td class="gt_row gt_center">
17.81% (n=13)
</td>
<td class="gt_row gt_center">
82.19% (n=60)
</td>
</tr>
<tr class="gt_group_heading_row">
<td colspan="3" class="gt_group_heading">
Overall
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_stub">
</td>
<td class="gt_row gt_center">
18.28% (n=49)
</td>
<td class="gt_row gt_center">
81.72% (n=219)
</td>
</tr>
</tbody>
</table>
</div>
<!--/html_preserve-->
