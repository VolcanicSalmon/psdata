wms=read_protti("~/Library/CloudStorage/OneDrive-NorwichBioscienceInstitutes/wang_ms/Samples View Report With Clusters for 241101_At_CmOx.txt",sep='\t')
replicate_names=c('rpw8_non_1','rpw8_non_2','rpw8_non_3','rpw8_pw_1','rpw8_pw_2','rpw8_pw_3','ty_non_1','ty_non_2','ty_non_3','ty_pw_1','ty_pw_2','ty_pw_3')
condition=c('rpw_8_non','rpw_8_non','rpw_8_non','rpw_8_pw','rpw8_pw','rpw8_pw','ty_non','ty_non','ty_non','ty_pw','ty_pw','ty_pw')
anno=data.frame(replicate_names,condition) 

```

```{r}
replicate_names <- c('rpw8_non_1', 'rpw8_non_2', 'rpw8_non_3',
                     'rpw8_pw_1', 'rpw8_pw_2', 'rpw8_pw_3',
                     'ty_non_1', 'ty_non_2', 'ty_non_3',
                     'ty_pw_1', 'ty_pw_2', 'ty_pw_3')

protti_ready <- wms %>%dplyr::select(accession_number, molecular_weight, all_of(replicate_names)) %>%
  pivot_longer(
    cols = all_of(replicate_names),
    names_to = "Sample",
    values_to = "Intensity"
  ) %>%
  mutate(
    Condition = recode(Sample,
      rpw8_non_1 = 'rpw8_non',
      rpw8_non_2 = 'rpw8_non',
      rpw8_non_3 = 'rpw8_non',
      rpw8_pw_1 = 'rpw8_pw',
      rpw8_pw_2 = 'rpw8_pw',
      rpw8_pw_3 = 'rpw8_pw',
      ty_non_1 = 'ty_non',
      ty_non_2 = 'ty_non',
      ty_non_3 = 'ty_non',
      ty_pw_1 = 'ty_pw',
      ty_pw_2 = 'ty_pw',
      ty_pw_3 = 'ty_pw'
    ),
    Replicate = as.integer(gsub(".*_", "", Sample)),
    log2_Intensity = log2(Intensity+0.5)
  ) %>%
  dplyr::select(accession_number, molecular_weight, Condition, Replicate, Sample, Intensity, log2_Intensity)

# View the result
head(protti_ready)
```

```{r}
wms_normalised <- protti_ready%>%
  normalise(
    sample =Sample,
    intensity_log2 = log2_Intensity,
    method = "median"
  )
wms_normalised[1:3,]
```
```{r}
wms_filtered <- wms_normalised %>%
  filter_cv(
    grouping = accession_number,
    condition = Condition,
    log2_intensity = log2_Intensity,
    cv_limit = 0.3,
    min_conditions = 1
  )
wms_filtered[1:3,]
```

```{r}
qc_pca(
  data = wms_normalised,
  sample = Sample,
  grouping = accession_number,
  intensity = normalised_intensity_log2,
  condition = Condition
)
ggsave("~/Library/CloudStorage/OneDrive-NorwichBioscienceInstitutes/wang_ms/qc_pca_norm_3003.pdf")
```
```{r}
wms_miss=wms_normalised%>%assign_missingness(
  
  sample = Sample,
  grouping = accession_number,
  intensity = normalised_intensity_log2,
  condition = Condition,ref_condition = 'ty_pw'
)%>%calculate_diff_abundance(sample=Sample,condition=Condition,
                             grouping=accession_number,
                             intensity_log2 = normalised_intensity_log2,
                             missingness = missingness,
                             comparison=comparison,method='moderated_t-test')
wms_miss_reftynon=wms_normalised%>%assign_missingness(
  
  sample = Sample,
  grouping = accession_number,
  intensity = normalised_intensity_log2,
  condition = Condition,ref_condition = 'ty_non'
)%>%calculate_diff_abundance(sample=Sample,condition=Condition,
                             grouping=accession_number,
                             intensity_log2 = normalised_intensity_log2,
                             missingness = missingness,
                             comparison=comparison,method='moderated_t-test')
wms_miss=wms_normalised%>%assign_missingness(
  
  sample = Sample,
  grouping = accession_number,
  intensity = normalised_intensity_log2,
  condition = Condition,ref_condition = 'ty_pw'
)%>%calculate_diff_abundance(sample=Sample,condition=Condition,
                             grouping=accession_number,
                             intensity_log2 = normalised_intensity_log2,
                             missingness = missingness,
                             comparison=comparison,method='moderated_t-test')
wms_miss_refrpw8non=wms_normalised%>%assign_missingness(
  
  sample = Sample,
  grouping = accession_number,
  intensity = normalised_intensity_log2,
  condition = Condition,ref_condition = 'rpw8_non'
)%>%calculate_diff_abundance(sample=Sample,condition=Condition,
                             grouping=accession_number,
                             intensity_log2 = normalised_intensity_log2,
                             missingness = missingness,
                             comparison=comparison,method='moderated_t-test')
```
```{r}
pval_distribution_plot(
  data = wms_miss,
  grouping = accession_number,
  pval = pval
)
```
```{r}
wms_miss$accession_number <- gsub("\\s.*", "", wms_miss$accession_number)
volcano_plot(data=wms_miss,grouping=accession_number,log2FC=diff,
             significance=pval,significance_cutoff=0.05,method='significant',
             interactive=F,target_column='accession_number',
             target=accession,facet_by=comparison)
wms_miss_reftynon$accession_number <- gsub("\\s.*", "", wms_miss_reftynon$accession_number)
volcano_plot(data=wms_miss_reftynon,grouping=accession_number,log2FC=diff,
             significance=pval,significance_cutoff=0.05,method='significant',
             interactive=F,target_column='accession_number',
             target=accession,facet_by=comparison)
wms_miss_refrpw8non$accession_number <- gsub("\\s.*", "", wms_miss_refrpw8non$accession_number)
volcano_plot(data=wms_miss_refrpw8non,grouping=accession_number,log2FC=diff,
             significance=pval,significance_cutoff=0.05,method='significant',
             interactive=F,target_column='accession_number',
             target=accession,facet_by=comparison)
```
```{r}
wms_miss$significant <- wms_miss$adj_pval < 0.05 & abs(wms_miss$diff) > 1
wms_miss_reftynon$significant <- wms_miss_reftynon$adj_pval < 0.05 & abs(wms_miss_reftynon$diff) > 1

wms_filtered <- wms_miss[wms_miss$comparison %in% c('rpw8_pw_vs_ty_pw'), ]
wms_filtered_reftynon<-wms_miss_reftynon[wms_miss_reftynon$comparison %in%c("rpw8_non_vs_ty_non","ty_pw_vs_ty_non"),]
wms_volcano <- ggplot(wms_filtered, aes(x = diff, y = -log10(adj_pval), color = significant)) +
  geom_point(alpha = 0.6) +  # All points
  geom_text_repel(
    data = subset(wms_filtered, significant),  # Only label significant points in filtered data
    aes(label = accession_number),
    box.padding = 0.5,
    max.overlaps = 10,
    size = 3,
    segment.color = "grey50"
  ) +
  facet_wrap(~ comparison) +
  labs(
    x = "Log2 Fold Change",
    y = "-log10(adjusted p-value)",
    color = "Significant (FDR < 0.05, |FC| > 1)"
  ) +
  theme_minimal() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "seagreen") +
  scale_color_manual(values = c("TRUE" = "seagreen", "FALSE" = "black"))

ggsave(
  "~/Library/CloudStorage/OneDrive-NorwichBioscienceInstitutes/wang_ms/wms_volcano_reftypw_sig005.pdf",
  wms_volcano,
  width = 7.5,
  height = 5
)
```


```{r}
wms_miss$significant <- wms_miss$adj_pval < 0.05 & abs(wms_miss$diff) > 1
wms_miss_reftynon$significant <- wms_miss_reftynon$adj_pval < 0.05 & abs(wms_miss_reftynon$diff) > 1
wms_miss_refrpw8non$significant<-wms_miss_refrpw8non$adj_pval<0.05 & abs(wms_miss_refrpw8non$diff)>1
wms_filtered <- wms_miss[wms_miss$comparison %in% c('rpw8_pw_vs_ty_pw'), ]
wms_filtered_reftynon<-wms_miss_reftynon[wms_miss_reftynon$comparison %in%c("rpw8_non_vs_ty_non","ty_pw_vs_ty_non"),]
wms_filtered_refrpw8non<-wms_miss_refrpw8non[wms_miss_refrpw8non$comparison %in%c("rpw8_pw_vs_rpw8_non"),]
volcano1 <- ggplot(wms_filtered, aes(x = diff, y = -log10(adj_pval), color = significant)) +
  geom_point(alpha = 0.6) +
  geom_text_repel(
    data = subset(wms_filtered, significant),
    aes(label = accession_number),
    box.padding = 0.5,
    max.overlaps = 10,
    size = 3,
    segment.color = "grey50"
  ) +
  facet_wrap(~comparison) +
  labs(
    x = "Log2 Fold Change",
    y = "-log10(adjusted p-value)",
    color = "Significant",
    title = "rpw8_pw_vs_ty_pw"
  ) +
  theme_minimal() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "seagreen") +
  scale_color_manual(values = c("TRUE" = "seagreen", "FALSE" = "lightgrey"))


volcano2 <- ggplot(
  subset(wms_filtered_reftynon, comparison == "rpw8_non_vs_ty_non"),
  aes(x = diff, y = -log10(adj_pval), color = significant)
) +
  geom_point(alpha = 0.6) +
  geom_text_repel(
    data = subset(wms_filtered_reftynon, significant & comparison == "rpw8_non_vs_ty_non"),
    aes(label = accession_number),
    box.padding = 0.5,
    max.overlaps = 10,
    size = 3,
    segment.color = "grey50"
  ) +
  labs(
    x = "Log2 Fold Change",
    y = "-log10(adjusted p-value)",
    color = "Significant",
    title = "rpw8_non_vs_ty_non"
  ) +
  theme_minimal() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "seagreen") +
  scale_color_manual(values = c("TRUE" = "seagreen", "FALSE" = "lightgrey"))


volcano3 <- ggplot(
  subset(wms_filtered_reftynon, comparison == "ty_pw_vs_ty_non"),
  aes(x = diff, y = -log10(adj_pval), color = significant)
) +
  geom_point(alpha = 0.6) +
  geom_text_repel(
    data = subset(wms_filtered_reftynon, significant & comparison == "ty_pw_vs_ty_non"),
    aes(label = accession_number),
    box.padding = 0.5,
    max.overlaps = 10,
    size = 3,
    segment.color = "grey50"
  ) +
  labs(
    x = "Log2 Fold Change",
    y = "-log10(adjusted p-value)",
    color = "Significant",
    title = "ty_pw_vs_ty_non"
  ) +
  theme_minimal() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "seagreen") +
  scale_color_manual(values = c("TRUE" = "seagreen", "FALSE" = "lightgrey"))
volcano4 <- ggplot(
  subset(wms_filtered_refrpw8non, comparison == "rpw8_pw_vs_rpw8_non"),
  aes(x = diff, y = -log10(adj_pval), color = significant)
) +
  geom_point(alpha = 0.6) +
  geom_text_repel(
    data = subset(wms_filtered_refrpw8non, significant & comparison == "rpw8_pw_vs_rpw8_non"),
    aes(label = accession_number),
    box.padding = 0.5,
    max.overlaps = 10,
    size = 3,
    segment.color = "grey50"
  ) +
  labs(
    x = "Log2 Fold Change",
    y = "-log10(adjusted p-value)",
    color = "Significant",
    title = "rpw8_pw_vs_rpw8_non"
  ) +
  theme_minimal() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "seagreen") +
  scale_color_manual(values = c("TRUE" = "seagreen", "FALSE" = "lightgrey"))


combined_volcano <- grid.arrange(
  volcano1, volcano2, volcano3,volcano4,
  ncol = 4
)
ggsave("~/Library/CloudStorage/OneDrive-NorwichBioscienceInstitutes/wang_ms/volcano_4comparisons_3003.pdf",combined_volcano,width=24,height=5)
```


```{r}
tairgo=read.csv('/Users/vef25hok/Downloads/ATH_GO_GOSLIM.txt',sep='\t',header=F)
go_data <- tairgo %>%
  transmute(
    TAIR_ID = V1,  # Assuming TAIR locus ID is in the 1st column
    GO_term = V7,  # Assuming GO term is in the 7th column (e.g., "regulation of DNA-templated transcription GO:0006355")
    Aspect = V8   # 8th column: F, C, or P
  )

# Clean GO terms: Extract only the GO:XXXXXXX part
go_data$GO_term <- gsub(".*(GO:\\d+).*", "\\1", go_data$GO_term)

# Remove rows with missing GO terms or aspects
go_data <- go_data %>%
  filter(!is.na(GO_term), !is.na(Aspect), GO_term != "")

go_f <- go_data %>% filter(Aspect == "F")  # Molecular Function
go_c <- go_data %>% filter(Aspect == "C")  # Cellular Component
go_p <- go_data %>% filter(Aspect == "P")  # Biological Process
```

```{r}
uniprot_data <- fetch_uniprot_proteome(
  organism_id = 3702,  # Arabidopsis thaliana
  columns = c("accession", "gene_names", "go_f","go_c","go_p"),  # Include GO annotations
  reviewed = TRUE,     # Only reviewed (Swiss-Prot) entries
  timeout = 120,       # Timeout in seconds
  max_tries = 5        # Max retries
)
uniprot_data
```

```{r}
uniprot_data <- uniprot_data %>%
  mutate(
    tair_id = str_extract(gene_names, "[Aa][Tt][1-5][Gg]\\d+") %>%  # Match At1g08910 or AT1G08910
      toupper()  # Convert to uppercase for consistency
  )
uniprot_data[1:3,]
```

```{r}
wms_miss2 <- wms_miss %>%
  mutate(tair_id = str_replace(accession_number, "\\.\\d+$", ""))
wms_miss_annotated <- wms_miss2 %>%
  left_join(uniprot_data, by = "tair_id")
wms_miss_annotated
wms_miss2_reftynon <- wms_miss_reftynon %>%
  mutate(tair_id = str_replace(accession_number, "\\.\\d+$", ""))
wms_miss_annotated_reftynon <- wms_miss2_reftynon %>%
  left_join(uniprot_data, by = "tair_id")
wms_miss_annotated_reftynon
wms_miss2_refrpw8non <- wms_miss_refrpw8non %>%
  mutate(tair_id = str_replace(accession_number, "\\.\\d+$", ""))
wms_miss_annotated_refrpw8non <- wms_miss2_refrpw8non %>%
  left_join(uniprot_data, by = "tair_id")
wms_miss_annotated_refrpw8non
```
```{r}
go_enrichment_bp <- calculate_go_enrichment(
  data = wms_miss_annotated,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_p,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
go_enrichment_bp_reftynon <- calculate_go_enrichment(
  data = wms_miss_annotated_reftynon,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_p,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
go_enrichment_bp_refrpw8non <- calculate_go_enrichment(
  data = wms_miss_annotated_refrpw8non,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_p,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
#ggsave("~/Library/CloudStorage/OneDrive-NorwichBioscienceInstitutes/wang_ms/GO_enrichment_BP_ref_typw.pdf", width = 24, height = 12, units = "in")
```

```{r}
go_enrichment_mf <- calculate_go_enrichment(
  data = wms_miss_annotated,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_f,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
go_enrichment_mf_reftynon <- calculate_go_enrichment(
  data = wms_miss_annotated_reftynon,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_f,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
go_enrichment_mf_refrpw8non <- calculate_go_enrichment(
  data = wms_miss_annotated_refrpw8non,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_f,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
#ggsave("~/Library/CloudStorage/OneDrive-NorwichBioscienceInstitutes/wang_ms/GO_enrichment_MF_ref_tynon.pdf", width = 24, height = 12, units = "in")
```

```{r}
go_enrichment_cp <- calculate_go_enrichment(
  data = wms_miss_annotated,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_c,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
go_enrichment_cp_reftynon <- calculate_go_enrichment(
  data = wms_miss_annotated_reftynon,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_c,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
go_enrichment_cp_refrpw8non <- calculate_go_enrichment(
  data = wms_miss_annotated_refrpw8non,
  protein_id = accession,  # Use UniProt accessions
  go_annotations_uniprot = go_c,  # Use go_p for BP
  is_significant = significant,  # Your significance column
  plot = F,group=comparison,
  plot_cutoff = "pval 0.05",facet_n_col=1
)
