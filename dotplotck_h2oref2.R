pval_cv_thres <- function(data, cvthres, pthres) {
  pcols <- grep("Adj\\. P-Value", colnames(data))
  cvcols <- grep("CV", colnames(data))
  pfilt <- data[apply(data[, pcols], 1, function(x) any(x < pthres, na.rm = T)),]
  cvfilt <- pfilt[apply(pfilt[, cvcols], 1, function(x) any(x < cvthres, na.rm = T)),]
  return(cvfilt)
}
list.files()
library(readxl)
xf=read_excel('filtered-3609xf_4r_250820_09_20250822145903_format.xlsx')
xf[1:3,]
psthres=pval_cv_thres(xf,50,0.1)
psthres[1:3,]
names(psthres)
psthresnorm=psthres[,c(5:13,52:63)]
psthresnorm[1:3,]
names(psthresnorm)
psthreslog=cbind(psthresnorm[,c(1:9)],log(psthresnorm[,c(10:21)]))
library(protti)
write.csv(psthreslog,'psthreslog.csv')
psthreslogprotti=read_protti('psthreslog.csv')
psthreslogprotti=psthreslogprotti%>%pivot_longer(cols=starts_with('Abundances'),names_to='sample',values_to='log_abundance')
names(psthreslogprotti)
library(dplyr)
psthreslogprotti=psthreslogprotti%>%pivot_longer(cols=starts_with('Abundances'),names_to='sample',values_to='log_abundance')
names(psthreslogprotti)
library(tidyverse)
psthreslogprotti=psthreslogprotti%>%pivot_longer(cols=starts_with('Abundances'),names_to='sample',values_to='log_abundance')
names(psthreslogprotti)
psthreslogprotti=psthreslogprotti%>%pivot_longer(cols=starts_with('abundances'),names_to='sample',values_to='log_abundance')
psthreslogprotti=read_protti('psthreslog.csv')
psthreslogprotti=psthreslogprotti%>%pivot_longer(cols=starts_with('abundances'),names_to='sample',values_to='log_abundance')
psthreslogprotti$sample=sub('abundances_normalized','',psthreslogprotti$sample)
psthreslogprotti$sample=sub('sample_','',psthreslogprotti$sample)
psthreslogprotti$condition=sub('^_f\\d+_','',psthreslogprotti$sample)
psthreslogprotti$condition
p=qc_pca(data=psthreslogprotti,sample=sample,grouping=accession,intensity=log_abundance,condition=condition)+theme(text=element_text(size=16),axis.title = element_text(size = 15),  # axis titles
  axis.text = element_text(size = 13),   # axis tick labels
  legend.title = element_text(size = 13),
  legend.text = element_text(size = 12))
p
ggsave('ppthreslogprotti_ggpca.png',plot=p,width=6,height=5,dpi=300)
psthreslogprotti_diffabund=psthreslogprotti%>%assign_missingness(sample=sample,condition=condition,grouping=accession,intensity=log_abundance,ref_condition='PSR2',retain_columns=c('accession','description','coverage_percent','number_ps_ms'))%>%calculate_diff_abundance(sample=sample,condition=condition,grouping=accession,intensity_log2=log_abundance,missingness=missingness,comparison=comparison,method='moderated_t-test',retain_columns=c('accession','description','coverage_percent','number_ps_ms'))
qc_ranked_intensities(data=psthreslogprotti_diffabund,sample=sample,grouping=accession,intensity_log2=log_abund)
psthreslogprotti_diffabund=psthreslogprotti%>%assign_missingness(sample=sample,condition=condition,grouping=accession,intensity=log_abundance,ref_condition='PSR2',retain_columns=c('accession','description','coverage_percent','number_ps_ms'))%>%calculate_diff_abundance(sample=sample,condition=condition,grouping=accession,intensity_log2=log_abundance,missingness=missingness,comparison=comparison,method='moderated_t-test',retain_columns=c('accession','description','coverage_percent','number_ps_ms',condition','sample'))
psthreslogprotti_diffabund=psthreslogprotti%>%assign_missingness(sample=sample,condition=condition,grouping=accession,intensity=log_abundance,ref_condition='PSR2',retain_columns=c('accession','description','coverage_percent','number_ps_ms'))%>%calculate_diff_abundance(sample=sample,condition=condition,grouping=accession,intensity_log2=log_abundance,missingness=missingness,comparison=comparison,method='moderated_t-test')
psthreslogprotti
psthreslogprotti$condition
psthreslogprotti_diffabund=psthreslogprotti%>%assign_missingness(sample=sample,condition=condition,grouping=accession,intensity=log_abundance,ref_condition='gfp_chitin',retain_columns=c('accession','description','coverage_percent','number_ps_ms'))%>%calculate_diff_abundance(sample=sample,condition=condition,grouping=accession,intensity_log2=log_abundance,missingness=missingness,comparison=comparison,method='moderated_t-test')
qc_ranked_intensities(data=psthreslogprotti,sample=sample,grouping=accession,intensity_log2=log_abundance,plot=T)
qcrank=qc_ranked_intensities(data=psthreslogprotti,sample=sample,grouping=accession,intensity_log2=log_abundance,plot=T)
ggsave('ppthreslogprotti_qcrank.png',plot=qcrank,width=6,height=5,dpi=300)
pval_distribution_plot(data=psthreslogprotti_diffabundall,grouping=accession,pval=pval,facet_by=comparison)
pval_distribution_plot(data=psthreslogprotti_diffabund,grouping=accession,pval=pval,facet_by=comparison)
pval_distr2=pval_distribution_plot(data=psthreslogprotti_diffabundall,grouping=accession,pval=pval,facet_by=comparison)
ggsave('ppthreslogprotti_pval_all.png',plot=pval_distr2,width=6,height=5,dpi=300)
pval_distr2=pval_distribution_plot(data=psthreslogprotti_diffabund,grouping=accession,pval=pval,facet_by=comparison)
ggsave('ppthreslogprotti_pval_all.png',plot=pval_distr2,width=6,height=5,dpi=300)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession')
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession')
psthreslogprotti_diffabund[1:3,]
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',label_column='accession',interactive=F)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',label_column='accession')
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',label=accession)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',label=accession)++ ggrepel::geom_text_repel(
  data = subset(psthreslogprotti_diffabund, adj_pval < 0.1),
  aes(x = diff, y = adj_pval, label = accession),
  size = 3,
  max.overlaps = 20
)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession')+ ggrepel::geom_text_repel(
  data = subset(psthreslogprotti_diffabund, adj_pval < 0.1),
  aes(x = diff, y = adj_pval, label = accession),
  size = 3,
  max.overlaps = 20
)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',label=accession,target=T)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=T)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession')+ ggrepel::geom_text_repel(
  data = subset(psthreslogprotti_diffabund, adj_pval < 0.1),
  aes(x = diff, y = adj_pval, label = accession),
  size = 3,
  max.overlaps = 20
)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession')+ ggrepel::geom_text_repel(
  data = subset(psthreslogprotti_diffabund, adj_pval < 0.1),
  aes(x = diff, y = adj_pval, label = accession),
  size = 3,box.padding=0.1,
  max.overlaps = 20
)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession')+ ggrepel::geom_text_repel(
  data = subset(psthreslogprotti_diffabund, adj_pval < 0.1),
  aes(x = diff, y = adj_pval, label = accession),
  size = 3,box.padding=0.1,point.padding=0.05,segment.length=0.05,
  max.overlaps = 20
)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+labs()
lab_data <- psthreslogprotti_diffabund %>%
  filter(adj_pval < 0.1) %>%
  mutate(yplot = -log10(adj_pval))
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3,
  max.overlaps = 25,
  box.padding = 0.15,
  point.padding = 0.05,
  force = 0.5,
  min.segment.length = 0
)
psthreslogprotti_diffabund[psthreslogprotti_diffabund=='AT5G59870.1',]
psthreslogprotti_diffabund[psthreslogprotti_diffabund$accession=='AT5G59870.1',]
v=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3,
  max.overlaps = 25,
  box.padding = 0.15,
  point.padding = 0.05,
  force = 0.5,
  min.segment.length = 0
)
ggsave('ppvlogprotti_volcano.png',plot=v,width=15,height=5,dpi=300)
pval_distr2=pval_distribution_plot(data=psthreslogprotti_diffabund,grouping=accession,pval=pval,facet_by=comparison)
ggsave('ppthreslogprotti_pval_all.png',plot=pval_distr2,width=15,height=5,dpi=300)
library(clusterProfiler)
BiocManager::install('clusterProfiler')
library(clusterProfiler)
lab_data
v
v
v
psthreslogprotti_diffabund=psthreslogprotti%>%assign_missingness(sample=sample,condition=condition,grouping=accession,intensity=log_abundance,ref_condition='gfp_dd_h2o',retain_columns=c('accession','description','coverage_percent','number_ps_ms'))%>%calculate_diff_abundance(sample=sample,condition=condition,grouping=accession,intensity_log2=log_abundance,missingness=missingness,comparison=comparison,method='moderated_t-test',retain_columns=c('accession','description','coverage_percent','number_ps_ms'))
pval_distribution_plot(data=psthreslogprotti_diffabund,grouping=accession,pval=pval,facet_by=comparison)
pval_distr3=pval_distribution_plot(data=psthreslogprotti_diffabund,grouping=accession,pval=pval,facet_by=comparison)
ggsave('ppthreslogprotti_pval_h2oref.png',plot=pval_distr3,width=15,height=5,dpi=300)
pval_distr3
ggsave('ppthreslogprotti_pval_h2oref.png',plot=pval_distr3,width=15,height=5,dpi=300)
volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=T)
lab_data <- psthreslogprotti_diffabund %>%
  filter(adj_pval < 0.1) %>%
  mutate(yplot = -log10(adj_pval))
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3,
  max.overlaps = 25,
  box.padding = 0.15,
  point.padding = 0.05,
  force = 0.5,
  min.segment.length = 0
)
v2
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3,
  max.overlaps = 40,
  box.padding = 0.15,
  point.padding = 0.05,
  force = 0.5,
  min.segment.length = 0
)
v2
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 5,
  max.overlaps = 40,
  box.padding = 0.15,
  point.padding = 0.05,
  force = 0.5,
  min.segment.length = 0
)
v2
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3,
  max.overlaps = 40,
  box.padding = 0.15,
  point.padding = 0.05,
  force = 0.5,
  min.segment.length = 0
)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=24,height=8,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 4,
  max.overlaps = 40,
  force = 0.5,
  min.segment.length = 0
)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=24,height=8,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,
  max.overlaps = 40,
  box.padding = 0.2,
  point.padding = 0.1,
  force = 0.5,
  min.segment.length = 0
)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=24,height=8,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,
  max.overlaps = 80,
  box.padding = 0.2,
  point.padding = 0.1,
  force = 0.5,
  min.segment.length = 0
)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=24,height=8,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,
  max.overlaps = 60,
  box.padding = 0.2,
  point.padding = 0.1,
  force = 0.5,
  min.segment.length = 0
)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=24,height=8,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,
  max.overlaps = 60,
  box.padding = 0.05,segment.length=0.02,min.segment.length=0,
  point.padding = 0.1,
  force = 1,force_pull=0.2,max.overlaps=Inf)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,
  max.overlaps = 70,
  box.padding = 0.05,segment.length=0.02,min.segment.length=0,
  point.padding = 0.1,
  force = 1,force_pull=0.2)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=21,height=7,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,
  max.overlaps = 60,
  force = 0.5,force_pull=0.2)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=21,height=7,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,box.padding = 0.05,segment.length=0.02,
  max.overlaps = 60,
  force = 0.5,force_pull=0.2)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,box.padding = 0.05,
  max.overlaps = 60,
  force = 0.5,force_pull=0.2)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=21,height=7,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,box.padding = 0.15,point.padding=0.05,min.segment.length=0,
  max.overlaps = 60,
  force = 0.5,force_pull=0.2)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=21,height=7,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,box.padding = 0.15,point.padding=0.05,min.segment.length=0,
  max.overlaps = 50,
  force = 0.5,force_pull=0.2)
ggsave('ppthreslogprotti_volcano_h2oref.png',plot=v2,width=21,height=7,dpi=300)
ggsave('ppthreslogprotti_volcano_h2oref.pdf',plot=v2,width=24,height=8,dpi=300)
ggsave('ppthreslogprotti_volcano_h2oref.pdf',plot=v2,width=27,height=9,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,box.padding = 0.15,point.padding=0.05,min.segment.length=0,
  max.overlaps = 40,
  force = 0.5,force_pull=0.2)
ggsave('ppthreslogprotti_volcano_h2oref.pdf',plot=v2,width=27,height=9,dpi=300)
v2=volcano_plot(psthreslogprotti_diffabund,accession,diff,adj_pval,x_axis_label='log abundance',y_axis_label='q-value',significance_cutoff=0.1,interactive=F,facet_by=comparison,method='significant',target_column='accession',target=accession)+ggrepel::geom_text_repel(
  data = lab_data,
  inherit.aes = FALSE,
  aes(x = diff, y = yplot, label = accession),
  size = 3.5,box.padding = 0.15,point.padding=0.05,min.segment.length=0.05,
  max.overlaps = 40,
  force = 0.5)
ggsave('ppthreslogprotti_volcano_h2oref.pdf',plot=v2,width=27,height=9,dpi=300)
lab_data[1:3,]
range(lab_data$diff)
uph2oref=enrichKEGG(labdata[labdata$diff>0,],organism='ath')
uph2oref=enrichKEGG(labdata[lab_data$diff>0,],organism='ath')
uph2oref=enrichKEGG(lab_data[lab_data$diff>0,],organism='ath')
uph2oref=lab_data[lab_data$diff>0,]$accession
upids=sub('\\..*$','',uph2oref)%>%unique()
upkeg=enrichKEGG(upids,organism='ath')
upkeg
wide <- psthreslogprotti_diffabund %>%
  mutate(accession = sub("\\..*$","", accession)) %>%            # drop .1 transcript suffixes
  group_by(accession, comparison) %>%
  summarise(diff = mean(diff, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = comparison, values_from = diff) %>%
  drop_na()
wide[1:3,]
mat <- as.matrix(wide[,-1])
rownames(mat) <- wide$accession
# (optional) keep only significant genes overall
sig_ids <- psthreslogprotti_diffabund %>%
  mutate(accession = sub("\\..*$","", accession)) %>%
  filter(adj_pval < sig_cutoff) %>% pull(accession) %>% unique()
mat <- mat[rownames(mat) %in% sig_ids, , drop = FALSE]
sig_cutoff=0.1
mat <- as.matrix(wide[,-1])
rownames(mat) <- wide$accession
# (optional) keep only significant genes overall
sig_ids <- psthreslogprotti_diffabund %>%
  mutate(accession = sub("\\..*$","", accession)) %>%
  filter(adj_pval < sig_cutoff) %>% pull(accession) %>% unique()
mat <- mat[rownames(mat) %in% sig_ids, , drop = FALSE]
mat <- mat[rownames(mat) %in% sig_ids, , drop = FALSE]
# 2) Row-scale (z-scores) for clustering
mat_z <- t(scale(t(mat)))
mat_z <- mat_z[complete.cases(mat_z), , drop = FALSE]
# 3) Choose k (quick elbow)
set.seed(1)
wss <- sapply(2:10, function(k) kmeans(mat_z, k, nstart = 50)$tot.withinss)
plot(2:10, wss, type = "b", xlab = "k", ylab = "Total within-cluster SS")
k=7
set.seed(42)
km <- kmeans(mat_z, centers = k, nstart = 100)
clusters <- km$cluster
ann_row <- data.frame(Cluster = factor(clusters))
row.names(ann_row) <- rownames(mat_z)
pheatmap(
  mat_z,
  scale = "none",
  clustering_distance_rows = "correlation",
  clustering_method = "ward.D2",
  annotation_row = ann_row,
  show_rownames = FALSE,
  main = "Clusters of accessions by differential pattern"
)
library(pheatmap)
set.seed(42)
km <- kmeans(mat_z, centers = k, nstart = 100)
clusters <- km$cluster
ann_row <- data.frame(Cluster = factor(clusters))
row.names(ann_row) <- rownames(mat_z)
pheatmap(
  mat_z,
  scale = "none",
  clustering_distance_rows = "correlation",
  clustering_method = "ward.D2",
  annotation_row = ann_row,
  show_rownames = FALSE,
  main = "Clusters of accessions by differential pattern"
)
k=3
set.seed(42)
km <- kmeans(mat_z, centers = k, nstart = 100)
clusters <- km$cluster
ann_row <- data.frame(Cluster = factor(clusters))
row.names(ann_row) <- rownames(mat_z)
pheatmap(
  mat_z,
  scale = "none",
  clustering_distance_rows = "correlation",
  clustering_method = "ward.D2",
  annotation_row = ann_row,
  show_rownames = FALSE,
  main = "Clusters of accessions by differential pattern"
)
pheat=pheatmap(
  mat_z,
  scale = "none",
  clustering_distance_rows = "correlation",
  clustering_method = "ward.D2",
  annotation_row = ann_row,
  show_rownames = FALSE,
  main = "Clusters of accessions by differential pattern"
)
ggsave('ppheat_h2oref.pdf',pheat,dpi=300,width=12,height=8)
gene_clusters <- split(sub("\\..*$","", rownames(mat_z)), clusters)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichGO",
  OrgDb          = org.At.tair.db,
  keyType        = "TAIR",
  ont            = "BP",
  pAdjustMethod  = "BH",
  qvalueCutoff   = 0.2
)
dotplot(ck, showCategory = 3)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichGKEGG",
  organism='ath',  keyType        = "TAIR",
  ont            = "BP",
  pAdjustMethod  = "BH",
  qvalueCutoff   = 0.2
)
dotplot(ck, showCategory = 3)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichKEGG",
  organism='ath',  keyType        = "TAIR",
  ont            = "BP",
  pAdjustMethod  = "BH",
  qvalueCutoff   = 0.2
)
dotplot(ck, showCategory = 3)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichKEGG",
  organism='ath',  keyType        = "TAIR",
   pAdjustMethod  = "BH",
  qvalueCutoff   = 0.2
)
dotplot(ck, showCategory = 3)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichKEGG",
  organism='ath', 
  qvalueCutoff   = 0.1
)
dotplot(ck, showCategory = 3)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichKEGG",
  organism='ath', 
  qvalueCutoff   = 0.2
)
dotplot(ck, showCategory = 3)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichGO",
  organism='ath', 
  qvalueCutoff   = 0.2
)
dotplot(ck, showCategory = 3)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichGO",
  OrgDB='org.At.tair.db',keyType='TAIR', 
  qvalueCutoff   = 0.2
)
dotplot(ck, showCategory = 3)
gene_clusters[1:3]
library(org.At.tair.db)
BiocManager::install('org.At.tair.db')
library(org.At.tair.db)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichGO",
  OrgDB='org.At.tair.db',keyType='TAIR', 
  qvalueCutoff   = 0.2
)
dotplot(ck, showCategory = 3)
ck <- compareCluster(
  geneClusters   = gene_clusters,
  fun            = "enrichGO",
  OrgDb          = org.At.tair.db,
  keyType        = "TAIR",
  ont            = "BP",
  pAdjustMethod  = "BH",
  qvalueCutoff   = 0.2
)
ck
dotplot(ck)
dot=dotplot(ck)
ggsave('dot_h2oref.pdf',dot,dpi=300,width=10,height=10)
save.image('dotplotck_h2oref.RDS')
savehistory('dotplotck_h2oref.Rhistory')
q()
savehistory('dotplotck_h2oref2.Rhistory')
