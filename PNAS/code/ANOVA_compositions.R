library(ggplot2)
library(readxl)
setwd("/Users/git/Si_biomineralization_ANME_SRB/PNAS/data")
# FeSi_data <- read.csv('FeSi_shell_comp_for_R_stats.csv')
# resFe.aov <- aov(FeSi ~ Source, data = FeSi_data)
# summary(resFe.aov)
# AlSi_data <- read.csv('AlSi_shell_comp_for_R_stats.csv')
# resAl.aov <- aov(AlSi ~ Source, data = AlSi_data)
# summary(resAl.aov)

#Read data file for composition ((Mg+Al+Fe)/Si) of sed-free ANME-SRB consortia,
# ANME-SRB consortia in sediments and sediments without ANME-SRB consortia
octtet_data <- read_excel('Dataset S3.xlsx')
#Filter by categories
#Sediments and ANME-SRB consortia-attached silicates from Jaco Scar
octtet_data_Jaco <- subset(octtet_data, Basin == "Jaco Scar", select = c("octtet","Source","Basin","source_order","basin_order"))
#Sediments and ANME-SRB consortia-attached silicates from the Santa Monica Basin
octtet_data_SMB <- subset(octtet_data, Basin == "Santa Monica", select = c("octtet","Source","Basin","source_order","basin_order"))
#Si-rich phase attached to sed-free ANME-SRB consortia from the Santa Monica Basin in incubations
octtet_data_SMB_sedfree <- subset(octtet_data_SMB,Source ==  "Aggregate-attached, Sediment-free" | Source ==  "Sediment", select = c("octtet","Source","Basin","source_order","basin_order"))
#Silicates attached to ANME-SRB consortia from the Santa Monica Basin in sediments
octtet_data_SMB_fromsed <- subset(octtet_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Sediment", select = c("octtet","Source","Basin","source_order","basin_order"))
#Silicates attached to ANME-SRB consortia from the Santa Monica Basin in sediments and 
#Si-rich phase attached to sed-free ANME-SRB consortia from the Santa Monica Basin in incubations
octtet_data_SMB_freevsfromsed<- subset(octtet_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Aggregate-attached, Sediment-free", select = c("octtet","Source","Basin","source_order","basin_order"))
##Sediments and ANME-SRB consortia-attached silicates from Eel River Basin
octtet_data_ERB <- subset(octtet_data, Basin == "Eel River", select = c("octtet","Source","Basin","source_order","basin_order"))

#Perform one-way ANOVA test on the Jaco Scar sediments and ANME-SRB consortia-attached silicates
resot1.aov <- aov(mg_al_fe_to_si ~ Source, data = octtet_data_Jaco)
summary(resot1.aov)

#Perform one-way ANOVA test on the Si-rich phase attached to sed-free ANME-SRB consortia from the Santa Monica Basin in incubations
resot2.aov <- aov(mg_al_fe_to_si ~ Source, data = octtet_data_SMB_sedfree)
summary(resot2.aov)

#Perform one-way ANOVA test on the silicates attached to ANME-SRB consortia from the Santa Monica Basin in sediments
resot3.aov <- aov(mg_al_fe_to_si ~ Source, data = octtet_data_SMB_fromsed)
summary(resot3.aov)

#Perform one-way ANOVA test on the silicates attached to ANME-SRB consortia from the Santa Monica Basin in sediments and 
#Si-rich phase attached to sed-free ANME-SRB consortia from the Santa Monica Basin in incubations
resot4.aov <- aov(mg_al_fe_to_si ~ Source, data = octtet_data_SMB_freevsfromsed)
summary(resot4.aov)

#Perform one-way ANOVA test on the sediments and ANME-SRB consortia-attached silicates from Eel River Basin
resot5.aov <- aov(mg_al_fe_to_si ~ Source, data = octtet_data_ERB)
summary(resot5.aov)

#Reorganize data
octtet_data$new_source_order <- reorder(octtet_data$Source,octtet_data$source_order)
octtet_data$new_basin_order <- reorder(octtet_data$Basin,octtet_data$basin_order)
#Make violin plots
ot<-ggplot(octtet_data,aes(new_source_order,mg_al_fe_to_si))
ot+geom_violin()+geom_boxplot(width=0.6)+facet_grid(~new_basin_order, scales = "free", space = "free")#+scale_color_brewer(palette = "Blues")#+geom_violin(aes(new_source_order,octtet))+geom_point(aes(octtet_data$ID))+geom_jitter(width=0.3,aes(color=octtet_data$ID), size=2.5)#

#Read data file for composition (Al/Si) of sed-free ANME-SRB consortia,
# ANME-SRB consortia in sediments and sediments without ANME-SRB consortia
AlSi_data <- read_excel('Dataset S2.xlsx')

#Filter by categories
#Sediments and ANME-SRB consortia-attached silicates from Jaco Scar
AlSi_data_Jaco <- subset(AlSi_data, Basin == "Jaco Scar", select = c("Al.per.Si","Source","Basin","source_order","basin_order"))
#Sediments and ANME-SRB consortia-attached silicates from the Santa Monica Basin
AlSi_data_SMB <- subset(AlSi_data, Basin == "Santa Monica", select = c("Al.per.Si","Source","Basin","source_order","basin_order"))
#Si-rich phase attached to sed-free ANME-SRB consortia from the Santa Monica Basin in incubations
AlSi_data_SMB_sedfree <- subset(AlSi_data_SMB,Source ==  "Aggregate-attached, Sediment-free" | Source ==  "Sediment", select = c("Al.per.Si","Source","Basin","source_order","basin_order"))
#Silicates attached to ANME-SRB consortia from the Santa Monica Basin in sediments
AlSi_data_SMB_fromsed <- subset(AlSi_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Sediment", select = c("Al.per.Si","Source","Basin","source_order","basin_order"))
#Silicates attached to ANME-SRB consortia from the Santa Monica Basin in sediments and 
#Si-rich phase attached to sed-free ANME-SRB consortia from the Santa Monica Basin in incubations
AlSi_data_SMB_freevsfromsed<- subset(AlSi_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Aggregate-attached, Sediment-free", select = c("Al.per.Si","Source","Basin","source_order","basin_order"))
##Sediments and ANME-SRB consortia-attached silicates from Eel River Basin
AlSi_data_ERB <- subset(AlSi_data, Basin == "Eel River", select = c("Al.per.Si","Source","Basin","source_order","basin_order"))

#Reorganize data
AlSi_data$new_source_order <- reorder(AlSi_data$Source,AlSi_data$source_order)
AlSi_data$new_basin_order <- reorder(AlSi_data$Basin,AlSi_data$basin_order)

#Make violin plot
ot<-ggplot(AlSi_data,aes(new_source_order,Al.per.Si))
#ot+geom_violin(aes())+geom_boxplot(width=0.07)+facet_grid(~new_basin_order, scales = "free", space = "free")+scale_color_brewer(palette = "Blues")#+geom_point(aes(AlSi_data$ID))+geom_jitter(width=0.3,aes(color=AlSi_data$ID), size=2.5)#
ot+geom_violin()+geom_boxplot(width=0.6)+facet_grid(~new_basin_order, scales = "free", space = "free")#+scale_color_brewer(palette = "Blues")#+geom_violin(aes(new_source_order,octtet))+geom_point(aes(octtet_data$ID))+geom_jitter(width=0.3,aes(color=octtet_data$ID), size=2.5)#

#Perform one-way ANOVA test on the Jaco Scar sediments and ANME-SRB consortia-attached silicates
resot6.aov <- aov(Al.per.Si ~ Source, data = AlSi_data_Jaco)
summary(resot6.aov)

#Perform one-way ANOVA test on the Si-rich phase attached to sed-free ANME-SRB consortia from the Santa Monica Basin in incubations
resot7.aov <- aov(Al.per.Si ~ Source, data = AlSi_data_SMB_sedfree)
summary(resot7.aov)

#Perform one-way ANOVA test on the silicates attached to ANME-SRB consortia from the Santa Monica Basin in sediments
resot8.aov <- aov(Al.per.Si ~ Source, data = AlSi_data_SMB_fromsed)
summary(resot8.aov)

#Perform one-way ANOVA test on the silicates attached to ANME-SRB consortia from the Santa Monica Basin in sediments and 
#Si-rich phase attached to sed-free ANME-SRB consortia from the Santa Monica Basin in incubations
resot9.aov <- aov(Al.per.Si ~ Source, data = AlSi_data_SMB_freevsfromsed)
summary(resot9.aov)

#Perform one-way ANOVA test on the sediments and ANME-SRB consortia-attached silicates from Eel River Basin
resot10.aov <- aov(Al.per.Si ~ Source, data = AlSi_data_ERB)
summary(resot10.aov)

# FeSi_data <- read.csv('Fe_Si_for_R_stats_all.csv')
# FeSi_data_Jaco <- subset(FeSi_data, Basin == "Jaco Scar", select = c("Fe.per.Si","Source","Basin","source_order","basin_order"))
# FeSi_data_SMB <- subset(FeSi_data, Basin == "Santa Monica", select = c("Fe.per.Si","Source","Basin","source_order","basin_order"))
# FeSi_data_SMB_sedfree <- subset(FeSi_data_SMB,Source ==  "Aggregate-attached, Sediment-free" | Source ==  "Sediment", select = c("Fe.per.Si","Source","Basin","source_order","basin_order"))
# FeSi_data_SMB_fromsed <- subset(FeSi_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Sediment", select = c("Fe.per.Si","Source","Basin","source_order","basin_order"))
# FeSi_data_SMB_freevsfromsed<- subset(FeSi_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Aggregate-attached, Sediment-free", select = c("Fe.per.Si","Source","Basin","source_order","basin_order"))
# FeSi_data_ERB <- subset(FeSi_data, Basin == "Eel River", select = c("Fe.per.Si","Source","Basin","source_order","basin_order"))
# FeSi_data$new_source_order <- reorder(FeSi_data$Source,FeSi_data$source_order)
# FeSi_data$new_basin_order <- reorder(FeSi_data$Basin,FeSi_data$basin_order)
# ot<-ggplot(FeSi_data,aes(new_source_order,Fe.per.Si))
# #ot+geom_violin(aes())+geom_boxplot(width=0.07)+facet_grid(~new_basin_order, scales = "free", space = "free")+scale_color_brewer(palette = "Blues")#+geom_point(aes(FeSi_data$ID))+geom_jitter(width=0.3,aes(color=FeSi_data$ID), size=2.5)#
# ot+geom_violin()+geom_boxplot(width=0.6)+facet_grid(~new_basin_order, scales = "free", space = "free")#+scale_color_brewer(palette = "Blues")#+geom_violin(aes(new_source_order,octtet))+geom_point(aes(octtet_data$ID))+geom_jitter(width=0.3,aes(color=octtet_data$ID), size=2.5)#
# 
# 
# 
# Al_data <- read.csv('Al_for_R_stats_all.csv')
# Al_data_Jaco <- subset(Al_data, Basin == "Jaco Scar", select = c("Al","Source","Basin","source_order","basin_order"))
# Al_data_SMB <- subset(Al_data, Basin == "Santa Monica", select = c("Al","Source","Basin","source_order","basin_order"))
# Al_data_SMB_sedfree <- subset(Al_data_SMB,Source ==  "Aggregate-attached, Sediment-free" | Source ==  "Sediment", select = c("Al","Source","Basin","source_order","basin_order"))
# Al_data_SMB_fromsed <- subset(Al_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Sediment", select = c("Al","Source","Basin","source_order","basin_order"))
# Al_data_SMB_freevsfromsed<- subset(Al_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Aggregate-attached, Sediment-free", select = c("Al","Source","Basin","source_order","basin_order"))
# Al_data_ERB <- subset(Al_data, Basin == "Eel River", select = c("Al","Source","Basin","source_order","basin_order"))
# Al_data_attached_only <-  subset(Al_data, Source == "Aggregate-attached, Sediment-free"| Source == "Aggregate-attached", select = c("Al","Source","Basin","source_order","basin_order"))
# 
# 
# resot1.aov <- aov(Al ~ Source, data = Al_data_Jaco)
# summary(resot1.aov)
# 
# resot2.aov <- aov(Al ~ Source, data = Al_data_SMB_sedfree)
# summary(resot2.aov)
# 
# resot3.aov <- aov(Al ~ Source, data = Al_data_SMB_fromsed)
# summary(resot3.aov)
# 
# resot4.aov <- aov(Al ~ Source, data = Al_data_SMB_freevsfromsed)
# summary(resot4.aov)
# 
# resot5.aov <- aov(Al ~ Source, data = Al_data_ERB)
# summary(resot5.aov)
# 
# Al_data$new_source_order <- reorder(Al_data$Source,Al_data$source_order)
# Al_data$new_basin_order <- reorder(Al_data$Basin,Al_data$basin_order)
# ot<-ggplot(Al_data,aes(new_source_order,Al))
# ot+geom_boxplot(width=0.6)+facet_grid(~new_basin_order, scales = "free", space = "free")#+scale_color_brewer(palette = "Blues")#+geom_violin(aes(new_source_order,Al))+geom_point(aes(Al_data$ID))+geom_jitter(width=0.3,aes(color=Al_data$ID), size=2.5)#
# 
# Al_att_only<-ggplot(Al_data_attached_only,aes(Source,Al))
# Al_att_only+geom_boxplot(width=0.6)+facet_grid(~Basin, scales = "free", space = "free")
# 
# 
# Si_data <- read.csv('Si_for_R_stats_all.csv')
# Si_data_Jaco <- subset(Si_data, Basin == "Jaco Scar", select = c("Si","Source","Basin","source_order","basin_order"))
# Si_data_SMB <- subset(Si_data, Basin == "Santa Monica", select = c("Si","Source","Basin","source_order","basin_order"))
# Si_data_SMB_sedfree <- subset(Si_data_SMB,Source ==  "Aggregate-attached, Sediment-free" | Source ==  "Sediment", select = c("Si","Source","Basin","source_order","basin_order"))
# Si_data_SMB_fromsed <- subset(Si_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Sediment", select = c("Si","Source","Basin","source_order","basin_order"))
# Si_data_SMB_freevsfromsed<- subset(Si_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Aggregate-attached, Sediment-free", select = c("Si","Source","Basin","source_order","basin_order"))
# Si_data_ERB <- subset(Si_data, Basin == "Eel River", select = c("Si","Source","Basin","source_order","basin_order"))
# Si_data_attached_only <-  subset(Si_data, Source == "Aggregate-attached, Sediment-free"| Source == "Aggregate-attached", select = c("Si","Source","Basin","source_order","basin_order"))
# 
# resot1.aov <- aov(Si ~ Source, data = Si_data_Jaco)
# summary(resot1.aov)
# 
# resot2.aov <- aov(Si ~ Source, data = Si_data_SMB_sedfree)
# summary(resot2.aov)
# 
# resot3.aov <- aov(Si ~ Source, data = Si_data_SMB_fromsed)
# summary(resot3.aov)
# 
# resot4.aov <- aov(Si ~ Source, data = Si_data_SMB_freevsfromsed)
# summary(resot4.aov)
# 
# resot5.aov <- aov(Si ~ Source, data = Si_data_ERB)
# summary(resot5.aov)
# 
# Si_data$new_source_order <- reorder(Si_data$Source,Si_data$source_order)
# Si_data$new_basin_order <- reorder(Si_data$Basin,Si_data$basin_order)
# ot<-ggplot(Si_data,aes(new_source_order,Si))
# ot+geom_boxplot(width=0.6)+facet_grid(~new_basin_order, scales = "free", space = "free")#+scSie_color_brewer(pSiette = "Blues")#+geom_violin(aes(new_source_order,Si))+geom_point(aes(Si_data$ID))+geom_jitter(width=0.3,aes(color=Si_data$ID), size=2.5)#
# 
# Si_att_only<-ggplot(Si_data_attached_only,aes(Source,Si))
# Si_att_only+geom_boxplot(width=0.6)+facet_grid(~Basin, scales = "free", space = "free")
# 
# 
# Fe_data <- read.csv('Fe_for_R_stats_all.csv')
# Fe_data_Jaco <- subset(Fe_data, Basin == "Jaco Scar", select = c("Fe","Source","Basin","source_order","basin_order"))
# Fe_data_SMB <- subset(Fe_data, Basin == "Santa Monica", select = c("Fe","Source","Basin","source_order","basin_order"))
# Fe_data_SMB_sedfree <- subset(Fe_data_SMB,Source ==  "Aggregate-attached, Sediment-free" | Source ==  "Sediment", select = c("Fe","Source","Basin","source_order","basin_order"))
# Fe_data_SMB_fromsed <- subset(Fe_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Sediment", select = c("Fe","Source","Basin","source_order","basin_order"))
# Fe_data_SMB_freevsfromsed<- subset(Fe_data_SMB,Source ==  "Aggregate-attached" | Source ==  "Aggregate-attached, Sediment-free", select = c("Fe","Source","Basin","source_order","basin_order"))
# Fe_data_ERB <- subset(Fe_data, Basin == "Eel River", select = c("Fe","Source","Basin","source_order","basin_order"))
# Fe_data_attached_only <-  subset(Fe_data, Source == "Aggregate-attached, Sediment-free"| Source == "Aggregate-attached", select = c("Fe","Source","Basin","source_order","basin_order"))
# 
# 
# resot1.aov <- aov(Fe ~ Source, data = Fe_data_Jaco)
# summary(resot1.aov)
# 
# resot2.aov <- aov(Fe ~ Source, data = Fe_data_SMB_sedfree)
# summary(resot2.aov)
# 
# resot3.aov <- aov(Fe ~ Source, data = Fe_data_SMB_fromsed)
# summary(resot3.aov)
# 
# resot4.aov <- aov(Fe ~ Source, data = Fe_data_SMB_freevsfromsed)
# summary(resot4.aov)
# 
# resot5.aov <- aov(Fe ~ Source, data = Fe_data_ERB)
# summary(resot5.aov)
# 
# Fe_data$new_source_order <- reorder(Fe_data$Source,Fe_data$source_order)
# Fe_data$new_basin_order <- reorder(Fe_data$Basin,Fe_data$basin_order)
# ot<-ggplot(Fe_data,aes(new_source_order,Fe))
# ot+geom_boxplot(width=0.6)+facet_grid(~new_basin_order, scales = "free", space = "free")#+scSie_color_brewer(pSiette = "Blues")#+geom_violin(aes(new_source_order,Si))+geom_point(aes(Fe_data$ID))+geom_jitter(width=0.3,aes(color=Fe_data$ID), size=2.5)#
# 
# Fe_att_only<-ggplot(Fe_data_attached_only,aes(Source,Fe))
# Fe_att_only+geom_boxplot(width=0.6)+facet_grid(~Basin, scales = "free", space = "free")




