## Taxon accumulation / rarefaction-extrapolation curves by depth stratum (Hill q=0)
## Author: Laura Casas (lauracasas@iim.csic.es)
## Two input files:
## Supplementary_Table_S1.csv - Trawl, Lowest_Taxon, Weight, Number
## Supplementary_Table_S4.csv - Trawl, Depth (m)
## Two sectors: (Shallow strata = Traditional NAFO <=732 m vs Extended Deep-Water/VME >732 m).

library(dplyr)
library(tidyr)
library(iNEXT)
library(ggplot2)
library(grid)     
library(gtable)   

## ---- USER SETTINGS --------------------------------------------------
data_dir <- "."       # folder containing the two input files
out_dir  <- "."        # folder where the figure/tables will be written
depth_cutoff_m <- 732
## -----------------------------------------------------------------------
df    <- read.csv(file.path(data_dir, "Supplementary_Table_S1.csv"), stringsAsFactors = FALSE)
depth <- read.csv(file.path(data_dir, "Supplementary_Table_S4.csv"), stringsAsFactors = FALSE)
names(depth) <- c("Trawl", "Depth")   

col_zone <- c("#4292C6", "#08306B")

## ---------------------------------------------------------------------------
## 1) Depth stratum assignment (Supplementary_Table_S4 lists all 183 trawls)
## ---------------------------------------------------------------------------
depth <- depth %>%
  mutate(stratum = cut(Depth, breaks = c(0, depth_cutoff_m, Inf),
                        labels = c("Shallow strata (<=732m)",
                                   "Deep-Water strata (>732m)")))

cat("Trawls per stratum:\n")
print(table(depth$stratum))

df_z <- df %>% left_join(depth %>% select(Trawl, stratum), by = "Trawl")
strata <- levels(depth$stratum)

## -------------------------------------------------------------------
## 2) Incidence data by stratum (taxon x trawl presence/absence)
## -------------------------------------------------------------------
inc_list <- list()
for (z in strata) {
  trawls_z <- depth$Trawl[depth$stratum == z]
  taxa_z   <- sort(unique(df_z$Lowest_Taxon[df_z$stratum == z]))
  m <- matrix(0L, nrow = length(taxa_z), ncol = length(trawls_z),
              dimnames = list(taxa_z, trawls_z))
  sub <- df_z[df_z$stratum == z & !is.na(df_z$stratum), ]
  for (i in seq_len(nrow(sub))) {
    m[sub$Lowest_Taxon[i], as.character(sub$Trawl[i])] <- 1L
  }
  inc_list[[z]] <- m
}

out_inc <- iNEXT(inc_list, q = 0, datatype = "incidence_raw")

## -------------------------------------------------------------------
## 3) Abundance data by stratum (individual counts per taxon)
##    Records with NA in 'Number' are excluded (no countable individuals).
## -------------------------------------------------------------------
abund_list <- list()
for (z in strata) {
  v <- df_z %>%
    filter(stratum == z, !is.na(Number)) %>%
    group_by(Lowest_Taxon) %>%
    summarise(N = sum(Number), .groups = "drop") %>%
    filter(N > 0)
  abund_list[[z]] <- setNames(v$N, v$Lowest_Taxon)
}

out_abund <- iNEXT(abund_list, q = 0, datatype = "abundance")

## -------------------------------------------------------------------
## 4) Combined figure, single shared legend 
## -------------------------------------------------------------------
common_theme <- theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        legend.key = element_blank(),
        legend.key.width = unit(1.6, "cm"),
        panel.grid.minor = element_blank())

p_inc <- ggiNEXT(out_inc, type = 1) +
  scale_colour_manual(values = col_zone, name = "Stratum") +
  scale_fill_manual(values = col_zone, guide = "none") +
  scale_shape_manual(values = c(16, 16), guide = "none") +
  scale_linetype_manual(values = c("Rarefaction" = "solid", "Extrapolation" = "22"),
                         name = "Method") +
  guides(linetype = guide_legend(override.aes = list(linewidth = 1))) +
  labs(title = "Incidence-based (trawls)",
       x = "Number of trawls", y = "Taxonomic richness") +
  common_theme

p_abund <- ggiNEXT(out_abund, type = 1) +
  scale_colour_manual(values = col_zone, name = "Stratum") +
  scale_fill_manual(values = col_zone, guide = "none") +
  scale_shape_manual(values = c(16, 16), guide = "none") +
  scale_linetype_manual(values = c("Rarefaction" = "solid", "Extrapolation" = "22"),
                         name = "Method") +
  guides(linetype = guide_legend(override.aes = list(linewidth = 1))) +
  labs(title = "Abundance-based (individuals)",
       x = "Number of individuals", y = "Taxonomic richness") +
  common_theme

## Extract the legend grob from one plot (both use identical scales, so
## either would do), then strip the legends from the two panels.
extract_legend <- function(gplot) {
  g <- ggplotGrob(gplot)
  idx <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
  g$grobs[[idx]]
}

shared_legend <- extract_legend(p_inc)

p_inc_nl   <- p_inc   + theme(legend.position = "none")
p_abund_nl <- p_abund + theme(legend.position = "none")

g1 <- ggplotGrob(p_inc_nl)
g2 <- ggplotGrob(p_abund_nl)
g_side_by_side <- cbind(g1, g2, size = "first")

g_final <- gtable_add_rows(g_side_by_side, unit(1.2, "cm"), pos = -1)
g_final <- gtable_add_grob(g_final, shared_legend,
                            t = nrow(g_final), l = 1, r = ncol(g_final))

png(file.path(out_dir, "curves_by_stratum_combined.png"),
    width = 12, height = 6.5, units = "in", res = 300)
grid.newpage()
grid.draw(g_final)
dev.off()

## -------------------------------------------------------------------
## 5) Summary table: incidence- and abundance-based results
## -------------------------------------------------------------------
asy_richness <- function(asyest) {
  asyest[asyest$Diversity == "Species richness", c("Assemblage", "Estimator", "s.e.")]
}

summary_incidence <- merge(out_inc$DataInfo, asy_richness(out_inc$AsyEst), by = "Assemblage") %>%
  transmute(Stratum = Assemblage, Trawls = T,
            Observed_richness_incidence = S.obs,
            Uniques_Q1 = Q1, Duplicates_Q2 = Q2,
            Chao2 = round(Estimator, 1), Chao2_SE = round(`s.e.`, 1),
            Coverage_pct_incidence = round(SC * 100, 1))

summary_abundance <- merge(out_abund$DataInfo, asy_richness(out_abund$AsyEst), by = "Assemblage") %>%
  transmute(Stratum = Assemblage, Individuals = n,
            Observed_richness_abundance = S.obs,
            Singletons_f1 = f1, Doubletons_f2 = f2,
            Chao1 = round(Estimator, 1), Chao1_SE = round(`s.e.`, 1),
            Coverage_pct_abundance = round(SC * 100, 1))

summary_combined <- summary_incidence %>%
  left_join(summary_abundance, by = "Stratum") %>%
  select(Stratum, Trawls, Individuals,
         Observed_richness_incidence, Uniques_Q1, Duplicates_Q2,
         Chao2, Chao2_SE, Coverage_pct_incidence,
         Observed_richness_abundance, Singletons_f1, Doubletons_f2,
         Chao1, Chao1_SE, Coverage_pct_abundance)

cat("\n=== Combined incidence + abundance summary by stratum ===\n")
print(summary_combined)

write.csv(summary_combined, file.path(out_dir, "summary_by_stratum_combined.csv"), row.names = FALSE)

cat("\nDone.\n")
