#nicoabund is the differential abundance results
nicorich=nicoabund%>%left_join(nicologprotti[,c('accession','peptype')],by=c('accession'))
nicorich2=nicorich%>%filter(abs(diff)>=1.5 & adj_pval<0.05)%>%distinct(accession,peptype,.keep_all=T)
peptype_abundance=nicorich2%>%
  group_by(peptype) %>%
  summarise(
    total_abundance = sum(avg_abundance),
    mean_abundance = mean(avg_abundance),
    n_proteins = n_distinct(accession),
    .groups = "drop"
  ) %>%
  arrange(desc(total_abundance))
ggplot(peptype_abundance, 
       aes(x = total_abundance, 
           y = reorder(peptype, total_abundance))) +
  geom_bar(stat = "identity", aes(fill = mean_diff)) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = 0,
                       name = 'Mean\nFold Change') +
  labs(title = 'Peptype Total Abundance',
       x = 'Total Abundance',
       y = 'Peptype') +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
ggsave('nico_enrichbar.pdf',height=16,width=13,dpi=300)
