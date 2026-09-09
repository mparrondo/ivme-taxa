# ---------------------------------------------------------------------------- #
# Integrating DNA-Based and Morphological Approaches Improves Biodiversity
# Charactization of Vulnerable Marine Ecosystems at Flemish Cap
# 
# Data Analysis
# Author: Marina Parrondo Lombardía (parrondomarina@proton.me)
#
# Input: Supplementary Table S1 + curated reference-sequence count table
# Entrada: Tabla suplementaria S1 + tabla curada de conteos de referencias
# ---------------------------------------------------------------------------- #

# Install and load packages ----------------------------------------------------
# Instalar y cargar paquetes ------------------------------------------------- #
required_packages <- c("tidyverse", 
                       "treemapify", 
                       "patchwork",
                       "ggokabeito",
                       "scales",
                       "DiagrammeR",
                       "DiagrammeRsvg",
                       "rsvg")

packages_to_install <- required_packages[!required_packages %in% 
                                           installed.packages()[, "Package"]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install,
                   repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
library(tidyverse)
library(treemapify)
library(patchwork)
library(ggokabeito)
library(scales)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
})

# Paths ------------------------------------------------------------------------
# Rutas ---------------------------------------------------------------------- #
# Run from the project root / Ejecutar desde el directorio raíz del proyecto.
input_taxonomy <- "data/raw/suppl_table_s1.csv"
input_reference_counts <- "data/processed/reference_sequence_counts.csv"
tables_dir <- "results/tables"
figures_dir <- "results/figures"

dir.create(tables_dir,
           recursive = TRUE,
           showWarnings = FALSE)

dir.create(figures_dir,
           recursive = TRUE,
           showWarnings = FALSE)

# Thresholds -------------------------------------------------------------------
# Umbrales ------------------------------------------------------------------- #
assignment_threshold <- 97

# Minimum sample size for comparative heatmap.
# Tamaño mínimo de muestra para el heatmap comparativo.
minimum_n_heatmap <- 6

# Functions --------------------------------------------------------------------
# Funciones ------------------------------------------------------------------ #

# Helper for explicit denominators / Función para denominadores explícitos
make_summary_row <- function(metric, value, analysis_unit, denominator, denominator_label) {
  tibble(metric = metric,
         value = as.integer(value),
         analysis_unit = analysis_unit,
         denominator = as.integer(denominator),
         denominator_label = denominator_label,
         percent = if_else(denominator > 0, 100 * value / denominator, NA_real_))
}

# Labeller to add an asterisk and edit only those four labels on the panels
# Añadir un asterisco y modificar únicamente esas cuatro etiquetas de las figs
phylum_labeller <- function(x) {
  ifelse(x %in% c("Annelida",
                  "Brachiopoda",
                  "Chaetognatha",
                  "Chordata"),
         paste0(x, " (*)"),
         x)
}

# Import data ------------------------------------------------------------------
# Importación de datos ------------------------------------------------------- #
# Comma decimals are converted to numeric values / Convierte comas decimales.
dataset <- read_csv(input_taxonomy,
                    na = c("",
                           "NA"),
                    locale = locale(decimal_mark = ","),
                    show_col_types = FALSE) %>%
  mutate(across(c(gb_coi,
                  bold_coi,
                  gb_16s),
                as.character),
         across(c(blast_coi_ap,
                  bold_coi_ap,
                  blast_16s_ap),
                as.numeric))

reference_counts <- read_csv(input_reference_counts,
                             na = c("",
                                    "NA"),
                             show_col_types = FALSE) %>%
  mutate(across(c(n_gb_coi,
                  n_bold_coi,
                  n_gb_16s),
                as.numeric))

# Derived variables ------------------------------------------------------------
# Variables derivadas -------------------------------------------------------- #
# Classify the finest available initial taxonomic level.
# Clasificar el nivel taxonómico inicial más fino disponible.
# Taxonomic resolution is assigned from the most specific populated rank.
# La resolución se asigna desde el rango taxonómico más específico disponible.
dataset <- dataset %>%
  mutate(initial_taxonomic_level = case_when(!is.na(tax_species) & tax_species != "" ~ "Species",
                                             !is.na(tax_genus) & tax_genus != "" ~ "Genus",
                                             !is.na(tax_family) & tax_family != "" ~ "Family",
                                             !is.na(tax_order) & tax_order != "" ~ "Order",
                                             !is.na(tax_class) & tax_class != "" ~ "Class",
                                             !is.na(tax_phylum) & tax_phylum != "" ~ "Phylum",
                                             TRUE ~ "Unassigned"),
         ltr_summary_category = case_when(initial_taxonomic_level == "Genus" ~ "Genus",
                                          initial_taxonomic_level %in% c("Family",
                                                                         "Order",
                                                                         "Class",
                                                                         "Phylum") ~ "Family_or_higher",
                                          initial_taxonomic_level == "Unassigned" ~ "Unassigned",
                                          TRUE ~ NA_character_),
         technical_coi = if_else(!is.na(accession_coi), "Success", "Failure"),
         technical_16s = if_else(!is.na(accession_16s), "Success", "Failure"))

# One row per initial taxon / Una fila por taxón inicial.
# Available has priority, followed by LTR / Disponible tiene prioridad, luego LTR.
taxon_dataset <- dataset %>%
  group_by(tax) %>%
  summarise(tax_phylum = first(na.omit(tax_phylum),
                               default = "Unclassified"),
            tax_class = first(na.omit(tax_class),
                              default = "Low Taxonomic Resolution"),
            initial_taxonomic_level = first(initial_taxonomic_level),
            ltr_summary_category = first(na.omit(ltr_summary_category),
                                         default = NA_character_),
            gb_coi = case_when(any(gb_coi == "1",
                                   na.rm = TRUE) ~ "1",
                               any(gb_coi == "LTR",
                                   na.rm = TRUE) ~ "LTR",
                               TRUE ~ "0"),
            bold_coi = case_when(any(bold_coi == "1",
                                     na.rm = TRUE) ~ "1",
                                 any(bold_coi == "LTR",
                                     na.rm = TRUE) ~ "LTR",
                                 TRUE ~ "0"),
            gb_16s = case_when(any(gb_16s == "1",
                                   na.rm = TRUE) ~ "1",
                               any(gb_16s == "LTR",
                                   na.rm = TRUE) ~ "LTR",
                               TRUE ~ "0"),
            .groups = "drop")

# Join reference-sequence counts / Unir los conteos de secuencias de referencia
# reference_counts has repeated taxa because it contains one row per voucher.
# reference_counts contiene taxones repetidos porque tiene una fila por voucher.
reference_counts_taxon <- reference_counts %>%
  group_by(tax) %>%
  summarise(across(c(n_gb_coi,
                     n_bold_coi,
                     n_gb_16s),
                   ~ first(na.omit(.x),
                           default = NA_real_)),
            .groups = "drop")

# Keep all initial taxa, even if no reference-count record is available.
# Mantener todos los taxones iniciales, incluso sin conteo de referencias.
taxon_dataset <- taxon_dataset %>%
  left_join(reference_counts_taxon,
            by = "tax")

rm(reference_counts_taxon,
   reference_counts)

# Taxa eligible for database searches / Taxones elegibles para búsquedas
# Only initial species are included; LTR categories are excluded.
# Solo se incluyen especies iniciales; se excluyen las categorías LTR.
species_dataset <- taxon_dataset %>%
  filter(initial_taxonomic_level == "Species") %>%
  select(-ltr_summary_category)

# Summary table ----------------------------------------------------------------
# Tabla resumen
# Taxon-level availability percentages use unique initial taxa as denominator.
# Los porcentajes de disponibilidad usan taxones iniciales únicos como denominador.
n_specimens <- n_distinct(dataset$voucher_id)
n_initial_taxa <- n_distinct(taxon_dataset$tax)
n_initial_species <- sum(taxon_dataset$initial_taxonomic_level == "Species")
n_initial_ltr_genus <- sum(taxon_dataset$ltr_summary_category == "Genus",
                           na.rm = TRUE)
n_initial_ltr_family_higher <- sum(taxon_dataset$ltr_summary_category == "Family_or_higher",
                                   na.rm = TRUE)
n_initial_unassigned <- sum(taxon_dataset$ltr_summary_category == "Unassigned",
                            na.rm = TRUE)
# Total low-taxonomic-resolution categories.
# Total de categorías con baja resolución taxonómica.
n_initial_ltr_total <- n_initial_ltr_genus +
  n_initial_ltr_family_higher +
  n_initial_unassigned

n_final_taxa <- n_distinct(na.omit(dataset$final_tax))
n_reviewed <- sum(!is.na(dataset$rev_tax) & dataset$rev_tax != "")
n_coi_success <- sum(dataset$technical_coi == "Success")
n_16s_success <- sum(dataset$technical_16s == "Success")

# "Available" means code 1 / "Disponible" significa código 1.
# Availability is calculated only among initial species.
# La disponibilidad se calcula solo sobre especies iniciales.
n_initial_species <- nrow(species_dataset)

n_gb_coi_available <- sum(species_dataset$gb_coi == "1")
n_bold_coi_available <- sum(species_dataset$bold_coi == "1")
n_gb_16s_available <- sum(species_dataset$gb_16s == "1")

n_gb_and_bold_coi_available <- sum(species_dataset$gb_coi == "1" &
                                     species_dataset$bold_coi == "1")

# Combined COI availability across BOLD and GenBank.
# Disponibilidad combinada de COI en BOLD y GenBank.
n_coi_available_any_database <- sum(species_dataset$gb_coi == "1" |
                                      species_dataset$bold_coi == "1")

n_coi_no_reference_any_database <- sum(species_dataset$gb_coi == "0" &
                                         species_dataset$bold_coi == "0")

n_16s_no_reference_genbank <- sum(species_dataset$gb_16s == "0")

summary_table <- bind_rows(make_summary_row("specimens",
                                            n_specimens,
                                            "specimen",
                                            n_specimens,
                                            "All specimens"),
                           make_summary_row("unique_vouchers",
                                            n_specimens,
                                            "specimen",
                                            n_specimens,
                                            "All specimens"),
                           make_summary_row("unique_initial_taxa",
                                            n_initial_taxa,
                                            "taxon",
                                            n_initial_taxa,
                                            "Unique initial taxa"),
                           make_summary_row("initial_species_taxa",
                                            n_initial_species,
                                            "taxon",
                                            n_initial_taxa,
                                            "Unique initial taxa"),
                           make_summary_row("initial_LTR_taxa_total",
                                            n_initial_ltr_total,
                                            "taxon",
                                            n_initial_taxa,
                                            "Unique initial taxa"),
                           make_summary_row("initial_LTR_genus_taxa",
                                            n_initial_ltr_genus,
                                            "taxon",
                                            n_initial_taxa,
                                            "Unique initial taxa"),
                           make_summary_row("initial_LTR_family_or_higher_taxa",
                                            n_initial_ltr_family_higher,
                                            "taxon",
                                            n_initial_taxa,
                                            "Unique initial taxa"),
                           make_summary_row("initial_unassigned_taxa",
                                            n_initial_unassigned,
                                            "taxon",
                                            n_initial_taxa,
                                            "Unique initial taxa"),
                           make_summary_row("unique_final_taxa",
                                            n_final_taxa,
                                            "taxon",
                                            n_initial_taxa,
                                            "Unique initial taxa"),
                           make_summary_row("reviewed_specimens",
                                            n_reviewed,
                                            "specimen",
                                            n_specimens,
                                            "All specimens"),
                           make_summary_row("COI_technical_success",
                                            n_coi_success,
                                            "specimen",
                                            n_specimens,
                                            "All specimens"),
                           make_summary_row("16S_technical_success",
                                            n_16s_success,
                                            "specimen",
                                            n_specimens,
                                            "All specimens"),
                           make_summary_row("gb_coi_available_species",
                                            n_gb_coi_available,
                                            "taxon",
                                            n_initial_species,
                                            "Unique initial species taxa"),
                           make_summary_row("bold_coi_available_species",
                                            n_bold_coi_available,
                                            "taxon",
                                            n_initial_species,
                                            "Unique initial species taxa"),
                           make_summary_row("gb_16s_available_species",
                                            n_gb_16s_available,
                                            "taxon",
                                            n_initial_species,
                                            "Unique initial species taxa"),
                           make_summary_row("gb_coi_and_bold_coi_available_species",
                                            n_gb_and_bold_coi_available,
                                            "taxon",
                                            n_initial_species,
                                            "Unique initial species taxa"),
                           make_summary_row("coi_available_in_at_least_one_public_database_species",
                                            n_coi_available_any_database,
                                            "taxon",
                                            n_initial_species,
                                            "Unique initial species taxa"),
                           make_summary_row("coi_no_reference_in_BOLD_or_GenBank_species",
                                            n_coi_no_reference_any_database,
                                            "taxon",
                                            n_initial_species,
                                            "Unique initial species taxa"),
                           make_summary_row("gb_16s_no_reference_species",
                                            n_16s_no_reference_genbank,
                                            "taxon",
                                            n_initial_species,
                                            "Unique initial species taxa")) %>%
  mutate(percent = round(percent, 1))

# write_csv(summary_table,
#           file.path(tables_dir,
#                     "summary_counts_percentages.csv"))

print(summary_table, n = Inf)

rm(species_dataset,
   n_specimens,
   n_initial_taxa,
   n_initial_ltr_genus,
   n_initial_ltr_family_higher,
   n_initial_unassigned,
   n_initial_ltr_total,
   n_final_taxa,
   n_reviewed,
   n_coi_success,
   n_16s_success,
   n_initial_species,
   n_gb_coi_available,
   n_bold_coi_available,
   n_gb_16s_available,
   n_gb_and_bold_coi_available,
   n_coi_available_any_database,
   n_coi_no_reference_any_database,
   n_16s_no_reference_genbank)
   
# Figure 2: initial taxonomy ---------------------------------------------------
# Figura 2: taxonomía inicial ------------------------------------------------ #
taxa_by_phylum_class <- taxon_dataset %>%
  mutate(tax_phylum = replace_na(tax_phylum, "Unclassified"),
         tax_class = replace_na(tax_class, "Low Taxonomic Resolution")) %>%
  count(tax_phylum,
        tax_class,
        name = "n")

taxa_by_phylum_class

fig_02 <- ggplot(taxa_by_phylum_class,
                 aes(area = n,
                     fill = tax_phylum,
                     label = tax_class,
                     subgroup = tax_phylum)) +
  geom_treemap(colour = "white") +
  geom_treemap_subgroup_border(colour = "white",
                               size = 1.2) +
  geom_treemap_subgroup_text(place = "centre",
                             grow = TRUE,
                             alpha = 0.5,
                             colour = "black",
                             fontface = "bold",
                             min.size = 5,
                             padding.x = grid::unit(4, "mm"),
                             padding.y = grid::unit(4, "mm")) +
  geom_treemap_text(place = "bottomright",
                    grow = FALSE,
                    reflow = TRUE,
                    colour = "white",
                    min.size = 3,
                    padding.x = grid::unit(2, "mm"),
                    padding.y = grid::unit(2, "mm")) +
  scale_fill_okabe_ito(name = "Phylum",
                       order = c(1, 8 , 5 , 3, 4, 5, 2, 6, 7)) +
  labs(x = NULL,
       y = NULL) +
  theme_classic() +
  theme(legend.position = "none")

print(fig_02)

ggsave(filename = file.path(figures_dir,
                            "Figure_02_treemap.png"),
       plot = fig_02,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 300,
       bg = "white")

rm(taxa_by_phylum_class)

# Singleton analysis -----------------------------------------------------------
# Análisis de singletons ----------------------------------------------------- #
# A singleton has exactly one sequence in the database (count == 1).
# Un singleton tiene exactamente una secuencia en la base de datos (conteo == 1).
singletons_long <- taxon_dataset %>%
  select(tax,
         tax_phylum,
         n_gb_coi,
         n_bold_coi,
         n_gb_16s) %>%
  pivot_longer(c(n_gb_coi,
                 n_bold_coi,
                 n_gb_16s),
               names_to = "database",
               values_to = "reference_count") %>%
  mutate(database = recode(database,
                           n_gb_coi = "GenBank COI",
                           n_bold_coi = "BOLD COI",
                           n_gb_16s = "GenBank 16S"),
         database = factor(database, 
                           levels = c("GenBank COI", 
                                      "BOLD COI", 
                                      "GenBank 16S")),
         tax_phylum = replace_na(tax_phylum, "Unclassified"),
         reference_category = case_when(is.na(reference_count) ~ "Low Taxonomic Resolution",
                                        reference_count == 0 ~ "No reference sequences",
                                        reference_count == 1 ~ "Singleton",
                                        reference_count >= 2 ~ "Two or more sequences"))

# Denominator for singleton percent excludes NA/LTR taxa.
# El denominador excluye taxones NA/LTR porque no hubo consulta a nivel adecuado.
singletons_summary <- singletons_long %>%
  group_by(database) %>%
  summarise(eligible_taxa = sum(!is.na(reference_count)),
            singleton_taxa = sum(reference_count == 1,
                                 na.rm = TRUE),
            no_reference_taxa = sum(reference_count == 0,
                                    na.rm = TRUE),
            multiple_reference_taxa = sum(reference_count >= 2,
                                          na.rm = TRUE),
            LTR_taxa = sum(is.na(reference_count)),
            .groups = "drop") %>%
  mutate(singleton_percent = if_else(eligible_taxa > 0,
                                     round(100 * singleton_taxa / eligible_taxa, 1),
                                     NA_real_))

singletons_summary

singleton_rows <- singletons_summary %>%
  transmute(metric = paste0("singletons_",
                            str_to_lower(str_replace_all(database, " ", "_"))),
            value = singleton_taxa,
            analysis_unit = "taxon",
            denominator = eligible_taxa,
            denominator_label = "Taxa with a numeric reference-sequence count",
            percent = singleton_percent)

singleton_rows

summary_table <- bind_rows(summary_table,
                           singleton_rows) %>%
  mutate(percent = round(percent, 1))

# write_csv(summary_table,
#           file.path(tables_dir,
#                     "summary_counts_percentages.csv"))

rm(singletons_summary,
   singleton_rows)

# Figure 3: singleton frequency ------------------------------------------------
# Figura 3: frecuencia de secuencias únicas ---------------------------------- #
singletons_by_phylum <- singletons_long %>%
  filter(reference_count == 1) %>%
  count(tax_phylum,
        database,
        name = "n_singletons")

singletons_by_phylum

fig_03 <- ggplot(singletons_by_phylum,
                 aes(x = tax_phylum,
                     y = n_singletons,
                     fill = database)) +
  geom_col(position = position_dodge2(width = 0.85,
                                      preserve = "single",
                                      padding = 0.08),
           width = 0.70,
           colour = "white",
           linewidth = 0.25) +
  scale_fill_okabe_ito(name = "Sequence database",
                       drop = FALSE,
                       order = c(5, 2, 7)) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Phylum",
       y = "Number of singleton taxa") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1),
        legend.position = "bottom")

print(fig_03)

ggsave(file.path(figures_dir,
                 "Figure_03_singletons.png"),
       fig_03,
       width = 210,
       height = 148,
       units = "mm",
       dpi = 300)

rm(singletons_by_phylum,
   singletons_long)

# Figure 4: database availability ----------------------------------------------
# Figura 4: disponibilidad en bases de datos --------------------------------- #
availability_long <- taxon_dataset %>%
  pivot_longer(c(bold_coi,
                 gb_coi,
                 gb_16s),
               names_to = "database",
               values_to = "availability_code") %>%
  mutate(database = recode(database,
                           bold_coi = "BOLD_COI",
                           gb_coi = "GenBank_COI",
                           gb_16s = "GenBank_16S"),
         database = factor(database,
                           levels = c("BOLD_COI",
                                      "GenBank_COI",
                                      "GenBank_16S")),
         availability = recode(availability_code,
                               "0" = "Not available",
                               "1" = "Available",
                               "LTR" = "Low Taxonomic Resolution"),
         availability = factor(availability,
                               levels = c("Not available",
                                          "Available",
                                          "Low Taxonomic Resolution")),
         tax_phylum = replace_na(tax_phylum,
                                 "Unclassified"))

# Reference availability by phylum.
# Disponibilidad de referencias por filo.
availability_by_phylum_long <- availability_long %>%
  count(tax_phylum,
        database,
        availability,
        name = "value") %>%
  complete(tax_phylum,
           database,
           availability,
           fill = list(value = 0)) %>%
  group_by(tax_phylum,
           database) %>%
  mutate(denominator = sum(value),
         denominator_label = "Unique initial taxa within phylum",
         percent = if_else(denominator > 0,
                           round(100 * value / denominator, 1),
                           NA_real_)) %>%
  ungroup()

# Taxonomic resolution by phylum.
# Resolución taxonómica inicial por filo.
taxonomic_resolution_by_phylum <- taxon_dataset %>%
  mutate(tax_phylum = replace_na(tax_phylum,
                                 "Unclassified"),
         ltr_category = case_when(initial_taxonomic_level == "Species" ~ "Species",
                                  initial_taxonomic_level %in% c("Genus",
                                                                 "Family",
                                                                 "Order",
                                                                 "Class",
                                                                 "Phylum") ~ "LTR",
                                  initial_taxonomic_level == "Unassigned" ~ "Unassigned")) %>%
  group_by(tax_phylum) %>%
  summarise(n_initial_taxa = n(),
            n_species_taxa = sum(ltr_category == "Species",
                                 na.rm = TRUE),
            n_ltr = sum(ltr_category == "LTR",
                        na.rm = TRUE),
            n_unassigned_taxa = sum(ltr_category == "Unassigned",
                                    na.rm = TRUE),
            n_ltr_taxa = sum(ltr_category != "Species",
                             na.rm = TRUE),
            ltr_percent = round(100 * n_ltr_taxa / n_initial_taxa, 1),
            .groups = "drop") %>%
  arrange(desc(ltr_percent),
          tax_phylum)

taxonomic_resolution_by_phylum

# Wide table for reporting.
# Tabla ancha para resultados.
reference_availability_by_phylum <- availability_by_phylum_long %>%
  filter(availability == "Available") %>%
  transmute(tax_phylum,
            database,
            n = value,
            percent) %>%
  pivot_wider(names_from = database,
              values_from = c(n,
                              percent),
              names_glue = "{database}_available_{.value}") %>%
  left_join(taxonomic_resolution_by_phylum,
            by = "tax_phylum") %>%
  select(tax_phylum,
         n_initial_taxa,
         n_species_taxa,
         n_ltr_taxa,
         ltr_percent,
         BOLD_COI_available_n,
         BOLD_COI_available_percent,
         GenBank_COI_available_n,
         GenBank_COI_available_percent,
         GenBank_16S_available_n,
         GenBank_16S_available_percent) %>%
  arrange(desc(n_initial_taxa),
          tax_phylum)

# write_csv(reference_availability_by_phylum,
#           file.path(tables_dir,
#                     "reference_availability_by_phylum.csv"))

print(reference_availability_by_phylum, width = Inf)

# Phyla with more than 50% LTR taxa.
# Filos con más de un 50% de taxones LTR.
high_ltr_phyla <- taxonomic_resolution_by_phylum %>%
  filter(ltr_percent > 50) %>%
  select(tax_phylum,
         n_initial_taxa,
         n_ltr_taxa,
         ltr_percent)

# write_csv(high_ltr_phyla,
#           file.path(tables_dir,
#                     "phyla_with_more_than_50_percent_LTR.csv"))

print(high_ltr_phyla)

fig_04 <- ggplot(availability_long,
                 aes(database,
                     fill = availability)) +
  geom_bar(position = "fill",
           colour = "white",
           linewidth = 0.25) +
  facet_wrap(~tax_phylum,
             scales = "free_y",
             ncol = 3,
             labeller = labeller(tax_phylum = phylum_labeller)) +
  scale_x_discrete(labels = c(BOLD_COI = "BOLD COI",
                              GenBank_COI = "GenBank COI",
                              GenBank_16S = "GenBank 16S")) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_okabe_ito(name = "Reference availability",
                       order = c(6, 3, 8)) +
  labs(x = "Sequence database",
       y = "Percentage of initial taxa",
       fill = "Reference availability",
       caption = "(*) Phyla represented by a small number of specimens; results should be interpreted descriptively.") +
  theme_classic() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = "bottom",
        strip.background = element_blank())

print(fig_04)

ggsave(file.path(figures_dir,
                 "Figure_04_reference_availability.png"),
       fig_04,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 300)

# BINs -------------------------------------------------------------------------
bins <- dataset %>%
  filter(!is.na(tax_species)) %>%
  distinct(tax)

bins <- bins %>%
  mutate(bin = case_when(tax == "Anthoptilum grandiflorum" ~ "multiple",     
                         tax == "Mediaster bairdi" ~ NA,             
                         tax == "Phormosoma placenta" ~ NA,
                         tax == "Gnathophausia zoea" ~ "multiple",          
                         tax == "Stereomastis nana" ~ "concordant",         
                         tax == "Balticina finmarchica" ~ NA,        
                         tax == "Acanthephyra purpurea" ~ "multiple",       
                         tax == "Acanthephyra pelagica" ~ "multiple",        
                         tax == "Pasiphaea tarda" ~ "multiple",             
                         tax == "Zoroaster fulgens" ~ NA,            
                         tax == "Bathybiaster vexillifer" ~ "concordant",      
                         tax == "Neolithodes grimaldii" ~ "concordant",        
                         tax == "Cirrothauma murrayi" ~ "concordant",          
                         tax == "Taonius pavo" ~ "concordant",                 
                         tax == "Gonatus fabricii" ~ "concordant",             
                         tax == "Periphylla periphylla" ~ "concordant",        
                         tax == "Arcoscalpellum michelottianum" ~ "concordant",
                         tax == "Colus islandicus" ~ "multiple",             
                         tax == "Chiroteuthis veranii" ~ "multiple",         
                         tax == "Distichoptilum gracile" ~ "concordant",       
                         tax == "Duva florida" ~ NA,                 
                         tax == "Geodia barretti" ~ "concordant",              
                         tax == "Atlantopandalus propinqvus" ~ "multiple",   
                         tax == "Ophiomusa lymani" ~ NA,             
                         tax == "Geodia macandrewii" ~ NA,           
                         tax == "Colossendeis colossea" ~ "multiple",        
                         tax == "Hippasteria phrygiana" ~ "concordant",        
                         tax == "Stereomastis sculpta" ~ "concordant",         
                         tax == "Stauroteuthis syrtensis" ~ "concordant",      
                         tax == "Stephanauge nexilis" ~ "multiple",          
                         tax == "Radicipes gracilis" ~ "concordant",           
                         tax == "Funiculina quadrangularis" ~ "concordant",    
                         tax == "Pontophilus norvegicus" ~ "concordant",       
                         tax == "Psilaster andromeda" ~ "multiple",          
                         tax == "Flabellum alabastrum" ~ NA,         
                         tax == "Parapasiphae sulcatifrons" ~ "multiple",    
                         tax == "Geodia phlegraei" ~ "multiple",             
                         tax == "Histioteuthis bonnellii" ~ NA,      
                         tax == "Sabinea hystrix" ~ "concordant",              
                         tax == "Stauropathes arctica" ~ NA,         
                         tax == "Acanella arbuscula" ~ "concordant",           
                         tax == "Terebratulina septentrionalis" ~ NA,
                         tax == "Ophiopholis aculeata" ~ "multiple",         
                         tax == "Ceramaster granularis" ~ NA,        
                         tax == "Stomphia coccinea" ~ "multiple",            
                         tax == "Iophon piceum" ~ NA,                
                         tax == "Aristaeopsis edwardsiana" ~ "concordant",     
                         tax == "Colossendeis proboscidea" ~ NA,     
                         tax == "Eusergestes arcticus" ~ "concordant",         
                         tax == "Chaceon quinquedens" ~ "concordant",          
                         tax == "Geodia atlantica" ~ NA,             
                         tax == "Ophioplinthus tessellata" ~ NA,     
                         tax == "Arrhoges occidentalis" ~ "concordant",        
                         tax == "Poraniomorpha hispida" ~ NA,        
                         tax == "Lophaster furcifer" ~ "concordant",           
                         tax == "Leptychaster arcticus" ~ "multiple",        
                         tax == "Novodinia americana" ~ NA,          
                         tax == "Actinernus nobilis" ~ NA,           
                         tax == "Tremaster mirabilis" ~ "multiple",          
                         tax == "Weltnerium stroemii" ~ NA,          
                         tax == "Neptunea vinlandica" ~ NA,          
                         tax == "Ctenodiscus crispatus" ~ "concordant",        
                         tax == "Ophiura sarsii" ~ "multiple",               
                         tax == "Pontaster tenuispinus" ~ "multiple",        
                         tax == "Tritonia newfoundlandica" ~ NA,     
                         tax == "Asconema foliatum" ~ NA,            
                         tax == "Stelletta rhaphidiophora" ~ NA,     
                         tax == "Benthoecetes bartletti" ~ "concordant",       
                         tax == "Graneledone verrucosa" ~ "concordant",        
                         tax == "Desmophyllum dianthus" ~ "concordant",        
                         tax == "Diplopteraster multipes" ~ "concordant",      
                         tax == "Pennatula aculeata" ~ "concordant",           
                         tax == "Pasiphaea multidentata" ~ "concordant",       
                         tax == "Ophiopleura inermis" ~ "multiple",          
                         tax == "Lithodes maja" ~ "concordant",                
                         tax == "Hyas coarctatus" ~ "concordant",              
                         tax == "Colus holboelli" ~ "concordant",              
                         tax == "Neptunea despecta" ~ "multiple",            
                         tax == "Polymastia hemisphaerica" ~ NA,     
                         tax == "Todarodes sagittatus" ~ "concordant",         
                         tax == "Tentorium semisuberites" ~ "concordant",      
                         tax == "Neognathophausia gigas" ~ "concordant",       
                         tax == "Meningodora mollis" ~ "concordant",           
                         tax == "Myxaster sol" ~ "concordant",                 
                         tax == "Histodermella kagigunensis" ~ NA,   
                         tax == "Stephanasterias albula" ~ "concordant",       
                         tax == "Suberites ficus" ~ "concordant",              
                         tax == "Eucopia australis" ~ "concordant"))

bins %>%
  mutate(bin = replace_na(bin,
                          "no_bin")) %>%
  count(bin,
        name = "n_taxa") %>%
  mutate(proportion = n_taxa / sum(n_taxa),
         percentage = round(100 * proportion, 2))

rm(bins)

# Assignment summaries ---------------------------------------------------------
# Resúmenes de asignación ---------------------------------------------------- #
# Match percentages use only successful specimens as denominator.
# Los porcentajes de match usan solo especímenes con éxito técnico.
assignment_long <- dataset %>%
  transmute(voucher_id,
            tax_phylum = replace_na(tax_phylum,
                                    "Unclassified"),
            tax_order = replace_na(tax_order,
                                   "Unclassified"),
            sequence_COI = technical_coi == "Success",
            sequence_16S = technical_16s == "Success",
            BOLD_COI = bold_coi_ap,
            GenBank_COI = blast_coi_ap,
            GenBank_16S = blast_16s_ap) %>%
  pivot_longer(c(BOLD_COI,
                 GenBank_COI,
                 GenBank_16S),
               names_to = "database",
               values_to = "assignment_percent") %>%
  mutate(sequence_available = if_else(database == "GenBank_16S",
                                      sequence_16S,
                                      sequence_COI),
         database = factor(database,
                           levels = c("BOLD_COI",
                                      "GenBank_COI",
                                      "GenBank_16S")))

# Overall assignment outcomes -----------------------------------------------
# Resultados globales de asignación ----------------------------------------- #
assignment_overall <- assignment_long %>%
  group_by(voucher_id) %>%
            # Match >= 97% in COI (BOLD or GenBank)
  summarise(match_coi = any(database %in% c("BOLD_COI",
                                            "GenBank_COI") &
                              sequence_available &
                              !is.na(assignment_percent) &
                              assignment_percent >= assignment_threshold,
                            na.rm = TRUE),
            # Match >= 97% in 16S (GenBank)
            match_16s = any(database == "GenBank_16S" &
                              sequence_available &
                              !is.na(assignment_percent) &
                              assignment_percent >= assignment_threshold,
                            na.rm = TRUE),
            # At least one technically successful sequence
            sequence_any = any(sequence_available,
                               na.rm = TRUE),
            .groups = "drop") %>%
         # Match in at least one marker
  mutate(match_any = match_coi | match_16s,
         # Match in both markers
         both_markers = match_coi & match_16s,
         # One mutually exclusive category per specimen
         assignment_category = case_when(both_markers ~ "Match across both markers",
                                         match_coi ~ "Match in COI only",
                                         match_16s ~ "Match in 16S only",
                                         sequence_any ~ "Below threshold",
                                         TRUE ~ "No sequence"),
         assignment_category = factor(assignment_category,
                                      levels = c("No sequence",
                                                 "Below threshold",
                                                 "Match in 16S only",
                                                 "Match in COI only",
                                                 "Match across both markers")))

assignment_successful_summary <- assignment_overall %>%
  filter(sequence_any) %>%
  count(assignment_category,
        name = "n_specimens") %>%
  mutate(percent_successful_specimens = round(100 * n_specimens /
                                                sum(n_specimens), 2))

print(assignment_successful_summary, n = Inf)

# Figure S1: assignment outcomes -----------------------------------------------
# Figura S1: resultados de asignación ---------------------------------------- #
# Panel A: overall assignment outcomes
assignment_overall_plot_data <- assignment_overall %>%
  count(assignment_category,
        name = "n") %>%
  mutate(percent = n / sum(n))

fig_s01_a <- ggplot(assignment_overall_plot_data,
                    aes(x = "Overall",
                        y = percent,
                        fill = assignment_category)) +
  geom_col(colour = "white",
           linewidth = 0.25,
           width = 0.7) +
  # geom_text(aes(label = paste0(round(100 * percent, 1), "%")),
  #           position = position_stack(vjust = 0.5),
  #           size = 3.2) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_okabe_ito(name = "Assignment result",
                       drop = FALSE,
                       order = c(8, 6, 7, 5, 3),
                       labels = c("No sequence",
                                  "Below threshold < 97%",
                                  "Match ≥ 97% in 16S",
                                  "Match ≥ 97% in COI",
                                  "Match ≥ 97% in both")) +
  labs(x = NULL,
       y = "Proportion of all specimens") +
  theme_classic() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 2,
                             byrow = TRUE))

fig_s01_a 

# Panel B: assignment outcomes by reference database
assignment_plot_data <- crossing(assignment_long,
                                 threshold = assignment_threshold) %>%
  mutate(assignment_result = case_when(!sequence_available ~ "No sequence",
                                       is.na(assignment_percent) ~ "No assignment",
                                       assignment_percent >= threshold ~ "Match",
                                       TRUE ~ "Below threshold"),
         assignment_result = factor(assignment_result, 
                                    levels = c("No sequence",
                                               "No assignment",
                                               "Below threshold",
                                               "Match")),
         threshold_label = paste0("Assignment threshold: ", threshold, "%"))

fig_s01_b <- ggplot(assignment_plot_data,
                    aes(database,
                        fill = assignment_result)) +
  geom_bar(position = "fill",
           colour = "white",
           linewidth = 0.25) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_discrete(labels = c("BOLD_COI" = "BOLD COI",
                              "GenBank_COI" = "GenBank COI",
                              "GenBank_16S" = "GenBank 16S")) +
  scale_fill_okabe_ito(name = "Assignment result",
                       labels = c("No sequence",
                                  "No assignment",
                                  "Below threshold < 97%",
                                  "Match ≥ 97%"),
                       drop = FALSE,
                       order = c(8, 1, 6, 3)) +
  labs(x = "Reference database",
       y = "Proportion of all specimens") +
  theme_classic() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 2,
                             byrow = TRUE))

fig_s01_b

# Combine panels A and B
fig_s01 <- (fig_s01_a + fig_s01_b) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold",
                                size = 11))

print(fig_s01)

# Save Figure S1
ggsave(file.path(figures_dir,
                 "Figure_S01_assignment_outcomes.png"),
       fig_s01,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 300 )

# Table 1: Overall technical success summary -----------------------------------
# Tabla 1: Resumen global de éxito técnico ----------------------------------- #

success_overall <- dataset %>%
  group_by(technical_coi,
           technical_16s) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  mutate(denominator = sum(n),
         denominator_label = "All specimens",
         proportion = n / denominator,
         percentage = 100 * proportion,
         percentage_label = paste0(round(percentage, 1), "%"),
         outcome = case_when(technical_coi == "Success" &
                               technical_16s == "Success" ~ "COI ✓ / 16S ✓",
                             technical_coi == "Success" &
                               technical_16s == "Failure" ~ "COI ✓ / 16S ✗",
                             technical_coi == "Failure" &
                               technical_16s == "Success" ~ "COI ✗ / 16S ✓",
                             technical_coi == "Failure" &
                               technical_16s == "Failure" ~ "COI ✗ / 16S ✗"),
         outcome = factor(outcome,
                          levels = c("COI ✓ / 16S ✓",
                                     "COI ✓ / 16S ✗",
                                     "COI ✗ / 16S ✓",
                                     "COI ✗ / 16S ✗")))

success_overall

# Save overall table
# Guardar tabla global
write_csv(success_overall,
          file.path(tables_dir,
                    "Table_01_amplification_sequencing_success_overall.csv"))

# Figure 5: Amplification and sequencing success -------------------------------
# Figura 5: Exito de amplificación y secuenciación --------------------------- #

# Amplification and sequencing success by phylum
success_by_group <- dataset %>%
  mutate(tax_phylum = replace_na(tax_phylum,
                                 "Unclassified")) %>%
  group_by(tax_phylum,
           technical_coi,
           technical_16s) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  group_by(tax_phylum) %>%
  mutate(proportion = n / sum(n),
         percentage = 100 * proportion,
         percentage_label = paste0(round(percentage, 1), "%")) %>%
  ungroup() %>%
  mutate(outcome = case_when(technical_coi == "Success" &
                               technical_16s == "Success" ~ "COI ✓ / 16S ✓",
                             technical_coi == "Success" &
                               technical_16s == "Failure" ~ "COI ✓ / 16S ✗",
                             technical_coi == "Failure" &
                               technical_16s == "Success" ~ "COI ✗ / 16S ✓",
                             technical_coi == "Failure" &
                               technical_16s == "Failure" ~ "COI ✗ / 16S ✗"))

# Set order of amplification outcomes
success_by_group$outcome <- factor(success_by_group$outcome,
                                   levels = c("COI ✓ / 16S ✓",
                                              "COI ✓ / 16S ✗",
                                              "COI ✗ / 16S ✓",
                                              "COI ✗ / 16S ✗"))

print(success_by_group,
      n = Inf)

# Save table
# write_csv(success_by_group,
#           file.path(tables_dir,
#                     "amplification_sequencing_success_by_phylum.csv"))

# Plot
fig_05 <- ggplot(success_by_group,
                 aes(x = tax_phylum,
                     y = percentage,
                     fill = outcome)) +
  geom_col(position = "fill",
           colour = "white") +
  scale_x_discrete(labels = phylum_labeller) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = seq(0,
                                  1,
                                  0.2)) +
  scale_fill_okabe_ito(name = "Amplification outcome",
                       labels = c("COI ✓ / 16S ✓", 
                                  "COI ✓ / 16S ✗", 
                                  "COI ✗ / 16S ✓",
                                  "COI ✗ / 16S ✗"), 
                       drop = FALSE,
                       order = c(3, 5, 7, 6)) +
  labs(x = "Phylum",
       y = "Percentage of specimens",
       caption = "(*) Phyla represented by a small number of specimens; results should be interpreted descriptively.") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = "bottom")

print(fig_05)

# Save figure
ggsave(filename = file.path(figures_dir,
                            "Figure_05_amplification_sequencing_success.png"),
       plot = fig_05,
       width = 210,
       height = 148,
       units = "mm",
       dpi = 300,
       bg = "white")

# Figure 6 ---------------------------------------------------------------------

# Technical and assignment dataset.
# Dataset para el análisis técnico y de asignación.
taxonomic_success_ds <- dataset %>%
  mutate(final_tax_phylum = na_if(final_tax_phylum, ""),
         final_tax_order = na_if(final_tax_order, ""),
         analysis_phylum = coalesce(final_tax_phylum,
                                    tax_phylum,
                                    "Unclassified"),
         analysis_order = coalesce(final_tax_order,
                                   tax_order,
                                   "Unclassified"),
         # Technical success from public accession availability.
         # Éxito técnico según disponibilidad de accesión pública.
         success_coi = technical_coi == "Success",
         success_16s = technical_16s == "Success",
         # Assignment matches at the selected identity threshold.
         # Matches de asignación con el umbral de identidad seleccionado.
         bold_coi_match = success_coi & !is.na(bold_coi_ap) &
           bold_coi_ap >= assignment_threshold,
         gb_coi_match = success_coi & !is.na(blast_coi_ap) &
           blast_coi_ap >= assignment_threshold,
         gb_16s_match = success_16s & !is.na(blast_16s_ap) &
           blast_16s_ap >= assignment_threshold,
         # Successful sequencing but no database assignment percentage.
         # Secuenciación exitosa, pero sin porcentaje de asignación.
         bold_coi_no_assignment = success_coi & is.na(bold_coi_ap),
         gb_coi_no_assignment = success_coi & is.na(blast_coi_ap),
         gb_16s_no_assignment = success_16s & is.na(blast_16s_ap)) %>%
  select(voucher_id,
         analysis_phylum,
         analysis_order,
         technical_coi,
         technical_16s,
         success_coi,
         success_16s,
         bold_coi_match,
         gb_coi_match,
         gb_16s_match,
         bold_coi_no_assignment,
         gb_coi_no_assignment,
         gb_16s_no_assignment)

glimpse(taxonomic_success_ds)

# Technical and assignment success by phylum.
# Éxito técnico y de asignación por filo.
taxonomic_success_by_phylum <- taxonomic_success_ds %>%
  group_by(analysis_phylum) %>%
  summarise(n_total = n(),
            # Technical success: denominator = all specimens in the phylum.
            # Éxito técnico: denominador = todos los especímenes del filo.
            n_success_coi = sum(success_coi,
                                na.rm = TRUE),
            n_success_16s = sum(success_16s,
                                na.rm = TRUE),
            technical_success_percent_coi = 100 * n_success_coi / n_total,
            technical_success_percent_16s = 100 * n_success_16s / n_total,
            # BOLD COI assignment: denominator = successful COI specimens.
            # Asignación BOLD COI: denominador = especímenes con COI exitoso.
            n_bold_coi_match = sum(bold_coi_match,
                                   na.rm = TRUE),
            n_bold_coi_no_assignment = sum(bold_coi_no_assignment,
                                           na.rm = TRUE),
            bold_coi_match_percent = if_else(n_success_coi > 0,
                                             100 * n_bold_coi_match / n_success_coi,
                                             NA_real_),
            # GenBank COI assignment: denominator = successful COI specimens.
            # Asignación GenBank COI: denominador = especímenes con COI exitoso.
            n_gb_coi_match = sum(gb_coi_match,
                                 na.rm = TRUE),
            n_gb_coi_no_assignment = sum(gb_coi_no_assignment,
                                         na.rm = TRUE),
            gb_coi_match_percent = if_else(n_success_coi > 0,
                                           100 * n_gb_coi_match / n_success_coi,
                                           NA_real_),
            # GenBank 16S assignment: denominator = successful 16S specimens.
            # Asignación GenBank 16S: denominador = especímenes con 16S exitoso.
            n_gb_16s_match = sum(gb_16s_match,
                                 na.rm = TRUE),
            n_gb_16s_no_assignment = sum(gb_16s_no_assignment,
                                         na.rm = TRUE),
            gb_16s_match_percent = if_else(n_success_16s > 0,
                                           100 * n_gb_16s_match / n_success_16s,
                                           NA_real_),
            .groups = "drop") %>%
  mutate(across(ends_with("_percent"),
                ~ round(.x, 1))) %>%
  arrange(desc(n_total),
          analysis_phylum)

print(taxonomic_success_by_phylum,
      width = Inf)

# write_csv(taxonomic_success_by_phylum,
#   file.path(tables_dir,
#             "technical_and_assignment_success_by_phylum.csv"))

# Technical and assignment success by order.
# Éxito técnico y de asignación por orden.
taxonomic_success_by_order <- taxonomic_success_ds %>%
  group_by(analysis_phylum,
           analysis_order) %>%
  summarise(n_total = n(),
            n_success_coi = sum(success_coi,
                                na.rm = TRUE),
            n_success_16s = sum(success_16s,
                                na.rm = TRUE),
            technical_success_percent_coi = 100 * n_success_coi / n_total,
            technical_success_percent_16s = 100 * n_success_16s / n_total,
            n_bold_coi_match = sum(bold_coi_match,
                                   na.rm = TRUE),
            n_gb_coi_match = sum(gb_coi_match,
                                 na.rm = TRUE),
            n_gb_16s_match = sum(gb_16s_match,
                                 na.rm = TRUE),
            bold_coi_match_percent = if_else(n_success_coi > 0,
                                             100 * n_bold_coi_match / n_success_coi,
                                             NA_real_),
            gb_coi_match_percent = if_else(n_success_coi > 0,
                                           100 * n_gb_coi_match / n_success_coi,
                                           NA_real_),
            gb_16s_match_percent = if_else(n_success_16s > 0,
                                           100 * n_gb_16s_match / n_success_16s,
                                           NA_real_),
            .groups = "drop") %>%
  mutate(across(ends_with("_percent"),
                ~ round(.x, 1))) %>%
  arrange(analysis_phylum,
          desc(n_total),
          analysis_order)

print(taxonomic_success_by_order,
      width = Inf, n = Inf)

# write_csv(taxonomic_success_by_order,
#           file.path(tables_dir,
#                     "technical_and_assignment_success_by_order.csv"))

# Small orders retained for descriptive reporting only.
# Órdenes pequeños conservados solo para reporte descriptivo.

small_orders <- taxonomic_success_by_order %>%
  filter(n_total < minimum_n_heatmap) %>%
  mutate(reporting_note = "Small sample size: descriptive only") %>%
  arrange(analysis_phylum,
          desc(n_total),
          analysis_order)

print(small_orders)

# write_csv(small_orders,
#           file.path(tables_dir,
#                     "technical_and_assignment_success_small_orders.csv"))

# Reversed sequential Okabe-Ito-inspired gradient.
# Gradiente secuencial invertido inspirado en Okabe-Ito.

okabe_ito_heatmap_colours <- c("#F0E442",  # Yellow: low success / éxito bajo
                               "#E69F00",  # Orange: intermediate-low success
                               "#009E73")  # Bluish green: high success / éxito alto

# Heatmap data for orders with n >= 6.
# Datos del heatmap para órdenes con n >= 6.
order_heatmap_data <- taxonomic_success_by_order %>%
  filter(n_total >= minimum_n_heatmap,
         analysis_order != "Unclassified") %>%
  pivot_longer(cols = c(technical_success_percent_coi,
                        bold_coi_match_percent,
                        gb_coi_match_percent,
                        technical_success_percent_16s,
                        gb_16s_match_percent),
               names_to = "process_step",
               values_to = "success_percent") %>%
  mutate(process_step = recode(process_step,
                               technical_success_percent_coi = "COI amplification",
                               bold_coi_match_percent = "BOLD COI\nMatch ≥97%",
                               gb_coi_match_percent = "GenBank COI\nMatch ≥97%",
                               technical_success_percent_16s = "16S amplification",
                               gb_16s_match_percent = "GenBank 16S\nMatch ≥97%"),
         process_step = factor(process_step,
                               levels = c("COI amplification",
                                          "BOLD COI\nMatch ≥97%",
                                          "GenBank COI\nMatch ≥97%",
                                          " ",
                                          "16S amplification",
                                          "GenBank 16S\nMatch ≥97%")),
         order_label = paste0(analysis_order, "\n(n = ", n_total, ")"),
         text_colour = case_when(is.na(success_percent) ~ "black",
                                 success_percent >= 65 ~ "white",
                                 TRUE ~ "black"))

fig_06 <- ggplot(order_heatmap_data,
                       aes(x = process_step,
                           y = forcats::fct_reorder(order_label,
                                                    n_total,
                                                    .desc = TRUE),
                           fill = success_percent)) +
  geom_tile(colour = "white",
            linewidth = 0.5,
            height = 0.85) +
  geom_text(aes(label = if_else(is.na(success_percent), "NA",
                                paste0(round(success_percent, 0), "%")),
                colour = text_colour),
            size = 2.7,
            fontface = "bold",
            show.legend = FALSE) +
  scale_colour_identity() +
  scale_x_discrete(drop = FALSE,
                   labels = c("COI amplification" = "COI\namplification",
                              "BOLD COI\nMatch ≥97%" = "BOLD COI\nMatch ≥97%",
                              "GenBank COI\nMatch ≥97%" = "GenBank COI\nMatch ≥97%",
                              " " = "",
                              "16S amplification" = "16S\namplification",
                              "GenBank 16S\nMatch ≥97%" = "GenBank 16S\nMatch ≥97%")) +
  scale_fill_gradientn(colours = okabe_ito_heatmap_colours,
                       values = scales::rescale(c(0,
                                                  45,
                                                  100)),
                       limits = c(0,
                                  100),
                       breaks = seq(0,
                                    100,
                                    25),
                       na.value = "grey90",
                       name = "Success\nrate (%)") +
  facet_grid(analysis_phylum ~ .,
             scales = "free_y",
             space = "free_y") +
  labs(x = "Workflow stage",
       y = "Order (number of specimens)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text.y = element_text(angle = 0,
                                    hjust = 0,
                                    face = "bold"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    plot.caption = element_text(hjust = 0,
                                face = "italic",
                                size = 8))

print(fig_06)

ggsave(filename = file.path(figures_dir,
                            "Figure_06_technical_assignment_heatmap_by_order.png"),
       plot = fig_06,
       device = "png",
       width = 297,
       height = 210,
       units = "mm",
       dpi = 300,
       bg = "white")

# Figure 7: --------------------------------------------------------------------
taxonomic_resolution_audit <- dataset %>%
  mutate(any_success = technical_coi == "Success" |
           technical_16s == "Success",
         initial_rank = stringr::str_to_title(initial_taxonomic_level),
         final_rank = case_when(!is.na(final_tax) &
                                  final_tax != "" ~ "Species",
                                !is.na(final_tax_genus) &
                                  final_tax_genus != "" ~ "Genus",
                                !is.na(final_tax_family) &
                                  final_tax_family != "" ~ "Family",
                                !is.na(final_tax_order) &
                                  final_tax_order != "" ~ "Order",
                                !is.na(final_tax_class) &
                                  final_tax_class != "" ~ "Class",
                                !is.na(final_tax_phylum) &
                                  final_tax_phylum != "" ~ "Phylum",
                                TRUE ~ NA_character_),
         analysis_status = case_when(!any_success ~ "Excluded: no successful marker",
                                     is.na(initial_rank) ~ "Excluded: missing initial resolution",
                                     is.na(final_rank) ~ "Excluded: no final assignment",
                                     is.na(tax_phylum) ~ "Excluded: missing initial phylum",
                                     TRUE ~ "Included in Figure 7")) %>%
  count(analysis_status, 
        name = "n_specimens") %>%
  mutate(percentage_total = round(100 * n_specimens / sum(n_specimens), 1))

print(taxonomic_resolution_audit)

exclusion_audit_by_phylum <- dataset %>%
  mutate(any_success = technical_coi == "Success" |
           technical_16s == "Success",
         analysis_status = case_when(!any_success ~ "No successful marker",
                                     TRUE ~ "At least one successful marker")) %>%
  count(tax_phylum,
        analysis_status,
        name = "n_specimens") %>%
  group_by(tax_phylum) %>%
  mutate(total_phylum = sum(n_specimens),
         percentage = round(100 * n_specimens / total_phylum, 1)) %>%
  ungroup() %>%
  arrange(tax_phylum,
          desc(analysis_status))

print(exclusion_audit_by_phylum)

write_csv(exclusion_audit_by_phylum,
  "results/tables/technical_exclusion_audit_by_phylum.csv")

tax_levels <- c("Phylum",
                "Class",
                "Order",
                "Family",
                "Genus",
                "Species")

rank_to_number <- c("Phylum"  = 1,
                    "Class"   = 2,
                    "Order"   = 3,
                    "Family"  = 4,
                    "Genus"   = 5,
                    "Species" = 6)

taxonomic_resolution_data <- dataset %>%
  mutate(initial_rank_check = case_when(!is.na(tax_species) &
                                          tax_species != "" ~ "Species",
                                        !is.na(tax_genus) &
                                          tax_genus != "" ~ "Genus",
                                        !is.na(tax_family) &
                                          tax_family != "" ~ "Family",
                                        !is.na(tax_order) &
                                          tax_order != "" ~ "Order",
                                        !is.na(tax_class) &
                                          tax_class != "" ~ "Class",
                                        !is.na(tax_phylum) &
                                          tax_phylum != "" ~ "Phylum",
                                        TRUE ~ NA_character_),
         initial_rank = stringr::str_to_title(initial_taxonomic_level),
         # ------------------------------------------------------------------- #
         # Ojo! Identificar etiquetas finales que son "sp."
         #
         # Ejemplos que deben excluirse de Species:
         #   "Geodia sp."
         #
         # Ejemplos que siguen contando como Species:
         #   "Henricia cf. lisa ingolfi"
         #   "Colus aff. islandicus"
         # ------------------------------------------------------------------- #
         final_is_sp = stringr::str_detect(final_tax,
                                           stringr::regex("\\bsp\\.?($|\\s)|\\bspp\\.?($|\\s)",
                                                          ignore_case = TRUE)),
         # Una asignación a especie debe contener, como mínimo, dos componentes:
         # género + epíteto específico. Esta regla también admite cf. y aff.
         final_has_multiple_terms = stringr::str_detect(stringr::str_trim(final_tax),
                                                        "\\S+\\s+\\S+"),
         final_is_species = !is.na(final_tax) &
           final_tax != "" &
           !final_is_sp &
           final_has_multiple_terms,
         # Resultado de la evaluación taxonómica integrativa.
         final_rank = case_when(final_is_species ~ "Species",
                                !is.na(final_tax_genus) &
                                  final_tax_genus != "" ~ "Genus",
                                !is.na(final_tax_family) & 
                                  final_tax_family != "" ~ "Family",
                                !is.na(final_tax_order) &
                                  final_tax_order != "" ~ "Order",
                                !is.na(final_tax_class) &
                                  final_tax_class != "" ~ "Class",
                                !is.na(final_tax_phylum) &
                                  final_tax_phylum != "" ~ "Phylum",
                                TRUE ~ NA_character_),
         initial_rank_num = dplyr::recode(initial_rank,
                                          "Phylum" = 1,
                                          "Class" = 2,
                                          "Order" = 3,
                                          "Family" = 4,
                                          "Genus" = 5,
                                          "Species" = 6,
                                          .default = NA_real_),
         final_rank_num = dplyr::recode(final_rank,
                                        "Phylum" = 1,
                                        "Class" = 2,
                                        "Order" = 3,
                                        "Family" = 4,
                                        "Genus" = 5,
                                        "Species" = 6,
                                        .default = NA_real_),
         resolution_change = final_rank_num - initial_rank_num,
         resolution_outcome = case_when(resolution_change > 0 ~ "Improved",
                                        resolution_change == 0 ~ "Unchanged",
                                        resolution_change < 0 ~ "Assigned to a higher taxonomic rank",
                                        TRUE ~ NA_character_),
         initial_phylum = tax_phylum) %>%
  filter(technical_coi == "Success" | technical_16s == "Success",
         !is.na(initial_rank),
         !is.na(final_rank),
         !is.na(initial_phylum)) %>%
  mutate(initial_rank = factor(initial_rank,
                               levels = tax_levels),
         final_rank = factor(final_rank,
                             levels = tax_levels),
         initial_phylum = factor(initial_phylum))

dataset %>%
  filter(!is.na(final_tax),
         stringr::str_detect(final_tax,
                             stringr::regex("\\bsp\\.?($|\\s)|\\bspp\\.?($|\\s)",
                                            ignore_case = TRUE))) %>%
  select(voucher_id,
         tax,
         initial_taxonomic_level,
         final_tax,
         final_tax_genus,
         final_tax_family,
         final_tax_order,
         final_tax_class,
         final_tax_phylum) %>%
  arrange(final_tax) %>%
  print(n = Inf,
        width = Inf)

# Comprobación del tamaño de muestra analítico:
nrow(taxonomic_resolution_data)

# La identificación inicial calculada desde las columnas tax_* debe coincidir
# con initial_taxonomic_level, salvo casos justificados o valores faltantes.
initial_rank_qc <- taxonomic_resolution_data %>%
  count(initial_rank,
        initial_rank_check,
        name = "n") %>%
  arrange(initial_rank,
          initial_rank_check)

print(initial_rank_qc)

# Revisar qué tipos de final_tax se están clasificando como Species.
# Si final_tax puede contener identificaciones de género, familia u orden,
# habrá que crear una variable final_taxonomic_level explícita antes de continuar.
final_tax_qc <- taxonomic_resolution_data %>%
  count(final_rank,
        name = "n") %>%
  arrange(match(final_rank,
                tax_levels))

print(final_tax_qc)

# Tabla resumen
resolution_summary_overall <- taxonomic_resolution_data %>%
  summarise(n_specimens = n(),
            improved = sum(resolution_change > 0),
            unchanged = sum(resolution_change == 0),
            assigned_higher_rank = sum(resolution_change < 0),
            pct_improved = 100 * improved / n_specimens,
            pct_unchanged = 100 * unchanged / n_specimens,
            pct_assigned_higher_rank = 100 * assigned_higher_rank / n_specimens,
            mean_change = mean(resolution_change),
            median_change = median(resolution_change)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

print(resolution_summary_overall)

# write_csv(
#   resolution_summary_overall,
#   "results/tables/taxonomic_resolution_summary_overall.csv"
# )

# Tabla resumen por filo
resolution_summary_by_phylum <- taxonomic_resolution_data %>%
  group_by(initial_phylum) %>%
  summarise(n_specimens = n(),
            improved = sum(resolution_change > 0),
            unchanged = sum(resolution_change == 0),
            assigned_higher_rank = sum(resolution_change < 0),
            pct_improved = 100 * improved / n_specimens,
            pct_unchanged = 100 * unchanged / n_specimens,
            pct_assigned_higher_rank = 100 * assigned_higher_rank / n_specimens,
            mean_change = mean(resolution_change),
            median_change = median(resolution_change),
            .groups = "drop") %>%
  mutate(across(c(pct_improved,
                  pct_unchanged,
                  pct_assigned_higher_rank,
                  mean_change,
                  median_change),
                ~ round(.x, 1))) %>%
  arrange(desc(pct_improved))

print(resolution_summary_by_phylum,
      width = Inf)

# write_csv(resolution_summary_by_phylum,
#           "results/tables/taxonomic_resolution_summary_by_phylum.csv")

# Matriz general (transicion)
# Cada fila corresponde a un nivel de identificación morfológica inicial.
# Los porcentajes se calculan dentro de cada fila; por tanto, cada fila suma 100%.
transition_matrix_overall <- taxonomic_resolution_data %>%
  count(initial_rank,
        final_rank,
        name = "n_specimens") %>%
  complete(initial_rank = factor(tax_levels,
                                 levels = tax_levels),
           final_rank = factor(tax_levels,
                               levels = tax_levels),
           fill = list(n_specimens = 0)) %>%
  group_by(initial_rank) %>%
  mutate(total_initial_rank = sum(n_specimens),
         percentage = if_else(total_initial_rank > 0,
                              100 * n_specimens / total_initial_rank, 0),
         cell_label = if_else(n_specimens > 0,
                              paste0(n_specimens, "\n(",
                                     round(percentage, 1), "%)"), "0")) %>%
  ungroup() %>%
  mutate(initial_rank = factor(initial_rank,
                               levels = tax_levels),
         final_rank = factor(final_rank,
                             levels = tax_levels))

# write_csv(transition_matrix_overall,
#           "results/tables/taxonomic_resolution_transition_matrix_overall.csv")

## Figure 7a -------------------------------------------------------------------
fig_07a <- ggplot(transition_matrix_overall,
                  aes(x = final_rank,
                      y = fct_rev(initial_rank),
                      fill = percentage)) +
  geom_tile(colour = "white",
            linewidth = 0.7) +
  geom_text(aes(label = if_else(n_specimens > 0, cell_label, ""),
                colour = percentage >= 55 & n_specimens > 0),
            size = 3.1,
            fontface = "bold",
            lineheight = 0.9) +
  scale_fill_gradientn(colours = okabe_ito_heatmap_colours,
                       values = scales::rescale(c(0, 50, 100)),
                       limits = c(0, 100),
                       breaks = seq(0, 100, 25),
                       labels = label_percent(scale = 1),
                       name = "Specimens within\ninitial rank") +
  scale_colour_manual(values = c("FALSE" = "black",
                                 "TRUE" = "white"),
                      guide = "none") +
  labs(x = "Final taxonomic resolution",
       y = "Initial taxonomic resolution") +
  coord_fixed() +
  theme_classic(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right",
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0,
                                    size = 8,
                                    margin = margin(t = 10)))

print(fig_07a)

ggsave(plot = fig_07a,
       filename = "Figure_07A_taxonomic_resolution_overall.png",
       path = "results/figures",
       units = "mm",
       width = 180,
       height = 155,
       dpi = 300)

# Funcion para las matrices por filo
make_phylum_transition_plot <- function(data, phylum_name) {
  phylum_matrix <- data %>%
    filter(initial_phylum == phylum_name) %>%
    count(initial_rank,
          final_rank,
          name = "n_specimens") %>%
    complete(initial_rank = factor(tax_levels,
                                   levels = tax_levels),
             final_rank = factor(tax_levels,
                                 levels = tax_levels),
             fill = list(n_specimens = 0)) %>%
    group_by(initial_rank) %>%
    mutate(total_initial_rank = sum(n_specimens),
           percentage = if_else(total_initial_rank > 0,
                                100 * n_specimens / total_initial_rank, 0),
           cell_label = if_else(n_specimens > 0,
                                paste0(n_specimens, "\n(",
                                       round(percentage, 0), "%)"), "")) %>%
    ungroup() %>%
    mutate(initial_rank = factor(initial_rank,
                                 levels = tax_levels),
           final_rank = factor(final_rank,
                               levels = tax_levels))
  
  n_phylum <- sum(phylum_matrix$n_specimens)
  
  phylum_plot <- ggplot(phylum_matrix,
                        aes(x = final_rank,
                            y = fct_rev(initial_rank),
                            fill = percentage)) +
    geom_tile(colour = "white",
              linewidth = 0.5) +
    geom_text(aes(label = cell_label,
                  colour = percentage >= 55 &
                    n_specimens > 0),
              size = 2.3,
              fontface = "bold",
              lineheight = 0.85) +
    scale_fill_gradientn(colours = okabe_ito_heatmap_colours,
                         values = scales::rescale(c(0, 50, 100)),
                         limits = c(0, 100),
                         breaks = c(0, 50, 100),
                         labels = label_percent(scale = 1),
                         name = "Specimens within\ninitial rank") +
    scale_colour_manual(values = c("FALSE" = "black",
                                   "TRUE" = "white"),
                        guide = "none") +
    labs(title = paste0(phylum_name, " (n = ", n_phylum, ")"),
         x = NULL,
         y = NULL) +
    coord_fixed() +
    theme_classic(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1,
                                     size = 7),
          axis.text.y = element_text(size = 7),
          axis.ticks = element_blank(),
          legend.position = "none",
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    face = "bold",
                                    size = 10),
          plot.margin = margin(3, 3, 3, 3))
  
  list(plot = phylum_plot,
       data = phylum_matrix)
}

## Figure 7b -------------------------------------------------------------------

phyla <- taxonomic_resolution_data %>%
  distinct(initial_phylum) %>%
  pull(initial_phylum) %>%
  sort()

phylum_results <- map(phyla,
                      ~ make_phylum_transition_plot(data = taxonomic_resolution_data,
                                                    phylum_name = .x))

names(phylum_results) <- phyla

phylum_plots <- map(phylum_results, "plot")

phylum_plots <- purrr::imap(phylum_plots,
                            function(plot, phylum_name) {
                              n_phylum <- resolution_summary_by_phylum %>%
                                filter(as.character(initial_phylum) == phylum_name) %>%
                                pull(n_specimens)
                              plot +
                                labs(title = paste0(phylum_labeller(phylum_name),
                                                    " (n = ", n_phylum, ")"))})

fig_07b <- wrap_plots(phylum_plots,
                      ncol = 3) +
  plot_annotation(title = "Taxonomic resolution transitions by phylum",
                  caption = paste0("(*) Phyla represented by a small number of specimens; results should be interpreted descriptively."),
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          face = "bold",
                                                          size = 14),
                  plot.caption = element_text(hjust = 0,
                                              size = 9,
                                              margin = margin(t = 10))))

print(fig_07b)

ggsave(plot = fig_07b,
       filename = "Figure_07B_taxonomic_resolution_by_phylum.png",
       path = "results/figures",
       units = "mm",
       width = 245,
       height = 230,
       dpi = 300)

# Export
walk2(phylum_results,
      names(phylum_results),
      ~ write_csv(.x$data,
                  paste0("results/tables/taxonomic_resolution_transition_",
                         stringr::str_to_lower(.y), ".csv")))

fig_07b_single <- patchwork::wrap_elements(full = patchwork::patchworkGrob(fig_07b))

fig_07 <- (fig_07a | fig_07b_single) +
  plot_layout(heights = c(1, 2.5)) +
  plot_annotation(tag_levels = "A",
                  theme = theme(plot.tag = element_text(face = "bold",
                                                        size = 16)))

print(fig_07)

ggsave(plot = fig_07,
       filename = "Figure_07_taxonomic_resolution_transitions.png",
       path = "results/figures",
       device = "png",
       units = "mm",
       width = 420,
       height = 220,
       dpi = 300)

# Integrative Taxonomy ---------------------------------------------------------

# Parámetros
reference_columns <- c("gb_coi",
                       "bold_coi",
                       "gb_16s")

# Funciones auxiliares ------------------------------------------------------- #
# Estandariza etiquetas taxonómicas para comparaciones de especie.
# Elimina diferencias de mayúsculas, espacios y los calificadores cf./aff.
#
# Ejemplos:
# "Anthoptilum grandiflorum"     -> "anthoptilum grandiflorum"
# "Anthoptilum cf. grandiflorum" -> "anthoptilum grandiflorum"
# "Colus aff. islandicus"        -> "colus islandicus"
#
# No elimina "sp.", porque "Henricia sp." no equivale a una especie concreta.
# ---------------------------------------------------------------------------- #
normalise_taxon_label <- function(x) {
  x %>%
    str_to_lower() %>%
    str_squish() %>%
    str_replace_all("\\b(cf|aff)\\.?", "") %>%
    str_squish()
}

# Extrae el primer término de una etiqueta taxonómica, interpretado como género.
#
# Ejemplos:
# "Henricia sp."           -> "henricia"
# "Henricia sanguinolenta" -> "henricia"
# "Stryphnus fortis"       -> "stryphnus"
# ---------------------------------------------------------------------------- #
extract_genus <- function(x) {
  x %>%
    normalise_taxon_label() %>%
    str_extract("^[[:alpha:]-]+")
}

# Preparar el dataset de congruencia
congruence_data <- dataset %>%
  # ------------------------------------------------------------------------ #
  # Estado técnico:
  # Se considera que un espécimen tiene información molecular utilizable si
  # COI, 16S o ambos marcadores tuvieron éxito técnico.
  # ------------------------------------------------------------------------ #
  mutate(any_marker_success = technical_coi == "Success" | technical_16s == "Success",
         # ------------------------------------------------------------------------ #
         # Estandarización de etiquetas taxonómicas:
         # Se mantienen las etiquetas originales y se crean versiones comparables.
         # ------------------------------------------------------------------------ #
         tax_norm = normalise_taxon_label(tax),
         blast_coi_norm = normalise_taxon_label(blast_coi_id),
         bold_coi_norm = normalise_taxon_label(bold_coi_id),
         blast_16s_norm = normalise_taxon_label(blast_16s_id),
         tax_genus_norm = extract_genus(tax),
         blast_coi_genus = extract_genus(blast_coi_id),
         bold_coi_genus = extract_genus(bold_coi_id),
         blast_16s_genus = extract_genus(blast_16s_id),
         # ------------------------------------------------------------------------ #
         # Identificación de hits moleculares que cumplen el umbral ≥97%.
         # Se exige tanto el porcentaje de identidad como un identificador taxonómico
         # no vacío. Esto evita contar un porcentaje sin asignación como un hit útil.
         # ------------------------------------------------------------------------ #
         blast_coi_high_hit = !is.na(blast_coi_ap) &
           blast_coi_ap >= assignment_threshold & 
           !is.na(blast_coi_id) &
           blast_coi_id != "",
         bold_coi_high_hit = !is.na(bold_coi_ap) &
           bold_coi_ap >= assignment_threshold &
           !is.na(bold_coi_id) &
           bold_coi_id != "",
         blast_16s_high_hit = !is.na(blast_16s_ap) &
           blast_16s_ap >= assignment_threshold &
           !is.na(blast_16s_id) &
           blast_16s_id != "",
         any_high_hit = blast_coi_high_hit |
           bold_coi_high_hit |
           blast_16s_high_hit,
         # ------------------------------------------------------------------------ #
         # Disponibilidad de referencias:
         #
         # "1"   = existe referencia para ese marcador/base de datos.
         # "0"   = no existe referencia.
         # "LTR" = no se pudo evaluar a especie debido a la baja resolución inicial.
         #
         # Esta disponibilidad se interpreta únicamente para identificaciones
         # iniciales a nivel de especie.
         # ------------------------------------------------------------------------ #
         gb_coi_available = gb_coi == "1",
         bold_coi_available = bold_coi == "1",
         gb_16s_available = gb_16s == "1",
         any_reference_available = coalesce(gb_coi_available, FALSE) |
           coalesce(bold_coi_available, FALSE) |
           coalesce(gb_16s_available, FALSE),
         all_references_unavailable = coalesce(gb_coi == "0", FALSE) &
           coalesce(bold_coi == "0", FALSE) &
           coalesce(gb_16s == "0", FALSE),
         initial_rank = initial_taxonomic_level,
         reference_status = case_when(initial_rank != "Species" ~
                                        "Not assessed: initial identification below species level",
                                      any_reference_available ~
                                        "Reference available for at least one marker",
                                      all_references_unavailable ~
                                        "No reference available for any marker",
                                      TRUE ~
                                        "Reference availability unknown"),
         # ------------------------------------------------------------------------ #
         # Congruencia a nivel de especie:
         # Solo se evalúa si la identificación morfológica inicial fue a especie.
         # Una coincidencia con cualquiera de los marcadores/bases de datos es
         # suficiente para considerar el registro congruente.
         # ------------------------------------------------------------------------ #
         species_match_blast_coi = blast_coi_high_hit &
           coalesce(tax_norm == blast_coi_norm, FALSE),
         species_match_bold_coi = bold_coi_high_hit &
           coalesce(tax_norm == bold_coi_norm, FALSE),
         species_match_blast_16s = blast_16s_high_hit &
           coalesce(tax_norm == blast_16s_norm, FALSE),
         species_congruent = species_match_blast_coi |
           species_match_bold_coi |
           species_match_blast_16s,
         # ------------------------------------------------------------------------ #
         # Congruencia a nivel de género:
         # Solo se evalúa si la identificación inicial está a género. Por ejemplo:
         #
         # Inicial:      Henricia sp.
         # Molecular:    Henricia sanguinolenta
         # Resultado:    congruente a género.
         #
         # Esto no significa que la identificación esté confirmada a especie.
         # ------------------------------------------------------------------------ #
         genus_match_blast_coi = blast_coi_high_hit &
           coalesce(tax_genus_norm == blast_coi_genus, FALSE),
         genus_match_bold_coi = bold_coi_high_hit &
           coalesce(tax_genus_norm == bold_coi_genus, FALSE),
         genus_match_blast_16s = blast_16s_high_hit &
           coalesce(tax_genus_norm == blast_16s_genus, FALSE),
         genus_congruent = genus_match_blast_coi |
           genus_match_bold_coi |
           genus_match_blast_16s,
         # ------------------------------------------------------------------------ #
         # Estado de congruencia inicial-molecular:
         # Para identificaciones a familia, orden, clase o filo, la congruencia no
         # se evalúa automáticamente porque el dataset no contiene la taxonomía
         # jerárquica de cada hit molecular recuperado de GenBank/BOLD.
         # ------------------------------------------------------------------------ #
         congruence_status = case_when(!any_marker_success ~
                                         "Excluded: no successful marker",
                                       initial_rank == "Species" &
                                         species_congruent ~
                                         "Congruent at species level",
                                       initial_rank == "Genus" &
                                         genus_congruent ~
                                         "Congruent at genus level",
                                       initial_rank %in% c("Species",
                                                           "Genus") &
                                         any_high_hit ~
                                         "Discordant molecular assignment",
                                       initial_rank %in% c("Species",
                                                           "Genus") &
                                         !any_high_hit ~
                                         "No molecular match at ≥97% identity",
                                       initial_rank %in% c("Family",
                                                           "Order",
                                                           "Class",
                                                           "Phylum") &
                                         any_high_hit ~
                                         "Molecular match; congruence not assessed above genus level",
                                       initial_rank %in% c("Family",
                                                           "Order",
                                                           "Class",
                                                           "Phylum") &
                                         !any_high_hit ~
                                         "LTR; no molecular match at ≥97% identity",
                                       TRUE ~ "Not assessed"),
         # ------------------------------------------------------------------------ #
         # Categoría para revisión taxonómica:
         #
         # "Potential new genetic record" se asigna solo cuando:
         # - La identificación inicial fue a especie.
         # - No existía referencia en BOLD COI, GenBank COI ni GenBank 16S.
         # - Ningún marcador produjo un hit ≥97%.
         #
         # Un LTR no se clasifica automáticamente como potencial registro genético
         # nuevo, porque la disponibilidad de referencia no pudo evaluarse a especie.
         # ------------------------------------------------------------------------ #
         review_category = case_when(!any_marker_success ~
                                       "Excluded: no successful marker",
                                     congruence_status == "Congruent at species level" ~ "Congruent",
                                     congruence_status == "Congruent at genus level" ~
                                       "Genus-level concordance: requires expert assessment",
                                     congruence_status == "Discordant molecular assignment" ~ "Pending verification",
                                     congruence_status == "No molecular match at ≥97% identity" &
                                       reference_status == "No reference available for any marker" ~ "Potential new genetic record",
                                     congruence_status == "No molecular match at ≥97% identity" &
                                       reference_status %in% c("Reference available for at least one marker",
                                                               "Reference availability unknown") ~ "Pending verification",
                                     initial_rank %in% c("Family",
                                                         "Order",
                                                         "Class",
                                                         "Phylum") ~
                                       "Low taxonomic resolution: requires expert assessment",
                                     TRUE ~ "Pending verification"),
         
         # ------------------------------------------------------------------------ #
         # Estado de asignación final:
         # Esta variable es solo para auditoría y revisión manual. No debe emplearse
         # para definir congruencia, porque final_tax incorpora la evidencia molecular.
         # ------------------------------------------------------------------------ #
         final_assignment_available = !is.na(final_tax) &
           final_tax != "") %>%
  dplyr::select(voucher_id,
                haul,
                strata,
                tax,
                tax_phylum,
                initial_taxonomic_level,
                ltr_summary_category,
                technical_coi,
                technical_16s,
                any_marker_success,
                gb_coi,
                bold_coi,
                gb_16s,
                reference_status,
                blast_coi_id,
                blast_coi_ap,
                bold_coi_id,
                bold_coi_ap,
                blast_16s_id,
                blast_16s_ap,
                any_high_hit,
                congruence_status,
                review_category,
                rev_tax,
                final_tax,
                final_tax_phylum,
                final_tax_class,
                final_tax_order,
                final_tax_family,
                final_tax_genus,
                final_taxid,
                evidence,
                bin,
                bin_congruence,
                final_assignment_available)

write.csv(congruence_data,
          "results/tables/congruence_data.csv",
          row.names = FALSE)

congruence_summary <- congruence_data %>%
  count(review_category,
        name = "n_specimens") %>%
  mutate(percentage_all_specimens = round(100 * n_specimens / sum(n_specimens), 1)) %>%
  arrange(desc(n_specimens))

congruence_summary

congruence_summary_molecular <- congruence_data %>%
  filter(any_marker_success) %>%
  count(review_category, name = "n_specimens") %>%
  mutate(percentage_molecular_subset = round(100 * n_specimens / sum(n_specimens), 1)) %>%
  arrange(desc(n_specimens))

congruence_summary_molecular

congruence_by_phylum <- congruence_data %>%
  filter(any_marker_success) %>%
  count(tax_phylum,
        review_category,
        name = "n_specimens") %>%
  group_by(tax_phylum) %>%
  mutate(
    total_phylum = sum(n_specimens),
    percentage = round(100 * n_specimens / total_phylum, 1)) %>%
  ungroup() %>%
  arrange(tax_phylum,
          desc(n_specimens))

print(congruence_by_phylum, n = Inf)

potential_new_records <- congruence_data %>%
  filter(review_category == "Potential new genetic record") %>%
  select(voucher_id,
         tax,
         tax_phylum,
         initial_taxonomic_level,
         gb_coi,
         bold_coi,
         gb_16s,
         blast_coi_id,
         blast_coi_ap,
         bold_coi_id,
         bold_coi_ap,
         blast_16s_id,
         blast_16s_ap,
         final_tax,
         evidence) %>%
  arrange(tax_phylum, tax)

potential_new_records

rm(congruence_summary,
   congruence_summary_molecular,
   congruence_by_phylum,
   potential_new_records)

# Figure S2: Decision-Tree -----------------------------------------------------
fig_s02 <- grViz("digraph decision_tree {

  graph [
    layout = dot,
    rankdir = TB,
    bgcolor = white,
    nodesep = 0.35,
    ranksep = 0.45
  ]

  node [
    fontname = Helvetica,
    fontsize = 11,
    color = '#4D4D4D',
    penwidth = 1.1,
    style = filled,
    fillcolor = '#F7F7F7',
    margin = '0.15,0.08'
  ]

  edge [
    fontname = Helvetica,
    fontsize = 10,
    color = '#4D4D4D',
    penwidth = 1.1,
    arrowsize = 0.7
  ]

  start [
    label = 'Specimen with at least one\\nsuccessfully amplified marker',
    shape = box,
    fillcolor = '#CC79A7'
  ]

  rank_question [
    label = 'Initial morphological\\ntaxonomic resolution?',
    shape = box,
    fillcolor = '#F0E442'
  ]

  species [
    label = 'Species-level\\nidentification',
    shape = box,
    fillcolor = '#56B4E9'
  ]

  genus [
    label = 'Genus-level\\nidentification',
    shape = box,
    fillcolor = '#56B4E9'
  ]

  ltr [
    label = 'Family, Order, Class,\\nor Phylum identification (LTR)',
    shape = box,
    fillcolor = '#0072B2',
    fontcolor = 'white'
  ]

  species_question [
    label = 'At least one molecular\\nmatch at ≥97% identity?',
    shape = box,
    fillcolor = '#F0E442'
  ]

  species_congruent [
    label = 'Does at least one molecular\\nassignment match the initial\\nspecies-level identification?',
    shape = box,
    fillcolor = '#F0E442'
  ]

  genus_question [
    label = 'At least one molecular\\nmatch at ≥97% identity?',
    shape = box,
    fillcolor = '#F0E442'
  ]

  genus_congruent [
    label = 'Does at least one molecular\\nassignment belong to the same genus?',
    shape = box,
    fillcolor = '#F0E442'
  ]

  reference_question [
    label = 'Reference sequence available\\nfor at least one marker?',
    shape = box,
    fillcolor = '#F0E442'
  ]

  congruent_species [
    label = 'Congruent at\\nspecies level',
    shape = box,
    fillcolor = '#009E73',
    fontcolor = white
  ]

  congruent_genus [
    label = 'Genus-level concordance:\\nrequires expert assessment',
    shape = box,
    fillcolor = '#009E73',
    fontcolor = white
  ]

  pending [
    label = 'Pending verification',
    shape = box,
    fillcolor = '#E69F00'
  ]

  potential_new [
    label = 'Potential new\\ngenetic record',
    shape = box,
    fillcolor = '#E69F00'
  ]

  ltr_expert [
    label = 'Low taxonomic resolution:\\nrequires expert assessment',
    shape = box,
    fillcolor = '#0072B2',
    fontcolor = 'white'
  ]

  expert_review [
    label = 'Expert taxonomic reassessment',
    shape = box,
    fillcolor = '#CC79A7'
  ]

  final_assignment [
    label = 'Final integrative\\ntaxonomic assignment',
    shape = box,
    fillcolor = '#CC79A7',
    fontcolor = white
  ]

  start -> rank_question

  rank_question -> species [label = 'Species']
  rank_question -> genus [label = 'Genus']
  rank_question -> ltr [label = 'Family/Order/Class/Phylum']

  species -> species_question

  species_question -> species_congruent [label = 'Yes']
  species_question -> reference_question [label = 'No']

  species_congruent -> congruent_species [label = 'Yes']
  species_congruent -> pending [label = 'No']

  reference_question -> pending [label = 'Yes']
  reference_question -> potential_new [label = 'No']

  genus -> genus_question

  genus_question -> genus_congruent [label = 'Yes']
  genus_question -> pending [label = 'No']

  genus_congruent -> congruent_genus [label = 'Yes']
  genus_congruent -> pending [label = 'No']

  ltr -> ltr_expert

  pending -> expert_review
  potential_new -> expert_review
  ltr_expert -> expert_review
  congruent_genus -> expert_review
  congruent_species -> final_assignment
  expert_review -> final_assignment
}
")

fig_s02

fig_s02_svg <- DiagrammeRsvg::export_svg(fig_s02)

rsvg::rsvg_png(charToRaw(fig_s02_svg),
               file = "results/figures/Figure_S2_decision_tree.png",
               width = 3600,
               height = 3000)

taxa_inventory <- dataset %>%
  mutate(# Normalizar etiquetas taxonómicas para evitar diferencias por espacios.
        initial_taxon = na_if(str_squish(tax), ""),
        final_taxon = na_if(str_squish(final_tax), ""),
        # Detecta expresiones como "Genus sp.", "Genus spp." o "Genus sp. CLADE A"
        final_is_sp = str_detect(coalesce(final_tax, ""),
                                      regex("\\bsp\\.?($|\\s)|\\bspp\\.?($|\\s)",
                                            ignore_case = TRUE)),
         # Una identificación de especie debe contener al menos dos términos.
         # Ejemplos válidos:
         # Anthoptilum grandiflorum
         # Henricia cf. lisa ingolfi
         # Colus aff. islandicus
         final_has_multiple_terms = str_detect(str_squish(coalesce(final_tax, "")),
                                               "\\S+\\s+\\S+"),
         # Clasificación de nomenclatura abierta.
         final_open_nomenclature = str_detect(coalesce(final_tax, ""),
                                              regex("\\b(cf|aff)\\.?(\\s|$)",
                                                    ignore_case = TRUE)),
         # Nomenclatura abierta en la identificación inicial.
         initial_open_nomenclature = str_detect(coalesce(tax, ""),
                                                regex("\\b(cf|aff)\\.?(\\s|$)",
                                                      ignore_case = TRUE)),
         # Rango final corregido.
         final_taxonomic_level = case_when(!is.na(final_tax) &
                                             final_tax != "" &
                                             !final_is_sp &
                                             final_has_multiple_terms ~ "Species",
                                           !is.na(final_tax_genus) &
                                             final_tax_genus != "" ~ "Genus",
                                           !is.na(final_tax_family) &
                                             final_tax_family != "" ~ "Family",
                                           !is.na(final_tax_order) &
                                             final_tax_order != "" ~ "Order",
                                           !is.na(final_tax_class) &
                                             final_tax_class != "" ~ "Class",
                                           !is.na(final_tax_phylum) &
                                             final_tax_phylum != "" ~ "Phylum",
                                           TRUE ~ NA_character_),
         # LTR incluye todas las identificaciones iniciales inferiores a especie:
         # género, familia, orden, clase o filo.
         initial_ltr = initial_taxonomic_level %in% c("Genus",
                                                      "Family",
                                                      "Order",
                                                      "Class",
                                                      "Phylum"),
        # Éxito técnico: al menos un marcador secuenciado y depositado.
        technical_success = coalesce(!is.na(accession_coi) | !is.na(accession_16s),
                                     FALSE),
        # Éxito técnico separado por marcador.
        technical_success_coi = coalesce(!is.na(accession_coi),
                                         FALSE),
        technical_success_16s = coalesce(!is.na(accession_16s),
                                         FALSE),
        # Resolución taxonómica, incluyendo nomenclatura abierta.
        final_resolved_to_species = coalesce(final_taxonomic_level == "Species",
                                             FALSE),
        # Resolución taxonómica estricta, excluyendo cf. y aff.
        final_resolved_to_strict_species = coalesce(final_taxonomic_level == "Species" &
                                                      !final_open_nomenclature,
                                                    FALSE),
        # Resultado integrado: éxito técnico frente a resolución taxonómica.
        integrative_outcome = case_when(technical_success &
                                          final_resolved_to_strict_species ~
                                          "Technical success and formal species-level resolution",
                                        technical_success &
                                          final_resolved_to_species ~
                                          "Technical success and open-nomenclature species-level interpretation",
                                        technical_success &
                                          !final_resolved_to_species ~
                                          "Technical success without species-level resolution",
                                        !technical_success &
                                          final_resolved_to_species ~
                                          "No study sequence but species-level interpretation",
                                        !technical_success &
                                          !final_resolved_to_species ~
                                          "No study sequence and no species-level resolution",
                                        TRUE ~ "Other / check record"),
         # Existencia de, como mínimo, una secuencia producida en este estudio.
         has_study_sequence = !is.na(accession_coi) | !is.na(accession_16s),
         # Existencia de referencias públicas para la identificación morfológica inicial.
         initial_reference_available = gb_coi == "1" |
           bold_coi == "1" |
           gb_16s == "1")

technical_taxonomic_summary <- taxa_inventory %>%
  summarise(total_specimens = n(),
            technical_success_any_marker = sum(technical_success),
            technical_success_coi = sum(technical_success_coi),
            technical_success_16s = sum(technical_success_16s),
            technical_failure = sum(!technical_success),
            resolved_to_species_including_open = sum(final_resolved_to_species),
            resolved_to_formal_species = sum(final_resolved_to_strict_species),
            technical_success_and_species = sum(technical_success & final_resolved_to_species),
            technical_success_without_species = sum(technical_success & !final_resolved_to_species),
            no_sequence_but_species = sum(!technical_success & final_resolved_to_species),
            no_sequence_and_no_species = sum(!technical_success & !final_resolved_to_species)) %>%
  mutate(technical_success_percentage = round(100 * technical_success_any_marker / total_specimens, 1),
         species_resolution_percentage = round(100 * resolved_to_species_including_open / total_specimens, 1),
         formal_species_resolution_percentage = round(100 * resolved_to_formal_species / total_specimens, 1))

print(technical_taxonomic_summary,
      width = Inf)

technical_taxonomic_outcomes <- taxa_inventory %>%
  count(integrative_outcome,
        name = "n_specimens") %>%
  mutate(percentage = round(100 * n_specimens / sum(n_specimens), 1)) %>%
  arrange(desc(n_specimens))

print(technical_taxonomic_outcomes,
      n = Inf)

ltr_technical_taxonomic_summary <- taxa_inventory %>%
  filter(initial_ltr) %>%
  summarise(total_ltr_specimens = n(),
            total_ltr_categories = n_distinct(initial_taxon),
            ltr_technical_success_any_marker = sum(technical_success),
            ltr_technical_failure = sum(!technical_success),
            ltr_resolved_to_species_including_open = sum(final_resolved_to_species),
            ltr_resolved_to_formal_species = sum(final_resolved_to_strict_species),
            ltr_technical_success_and_species = sum(technical_success & final_resolved_to_species),
            ltr_technical_success_without_species = sum(technical_success & !final_resolved_to_species),
            ltr_no_sequence_but_species = sum(!technical_success & final_resolved_to_species),
            ltr_no_sequence_and_no_species = sum(!technical_success & !final_resolved_to_species)) %>%
  mutate(ltr_technical_success_percentage = round(100 * ltr_technical_success_any_marker / total_ltr_specimens,
                                                  1),
         ltr_species_resolution_percentage = round(100 * ltr_resolved_to_species_including_open /
                                                     total_ltr_specimens, 1),
         ltr_formal_species_resolution_percentage = round(100 * ltr_resolved_to_formal_species /
                                                            total_ltr_specimens, 1))

print(ltr_technical_taxonomic_summary,
      width = Inf)

ltr_taxon_resolution <- taxa_inventory %>%
  filter(initial_ltr,
         !is.na(initial_taxon)) %>%
  group_by(initial_taxon,
           initial_taxonomic_level) %>%
  summarise(n_specimens = n(),
            n_technically_successful = sum(technical_success),
            n_technical_failures = sum(!technical_success),
            n_specimens_resolved_to_species = sum(technical_success &
                                                    final_resolved_to_species),
            n_specimens_resolved_to_strict_species = sum(technical_success &
                                                           final_resolved_to_strict_species),
            n_final_species_taxa = n_distinct(final_taxon[technical_success &
                                                            final_resolved_to_species &
                                                            !is.na(final_taxon)]),
            n_final_strict_species_taxa = n_distinct(final_taxon[technical_success &
                                                                   final_resolved_to_strict_species &
                                                                   !is.na(final_taxon)]),
            final_species_taxa = paste(sort(unique(final_taxon[technical_success &
                                                                 final_resolved_to_species &
                                                                 !is.na(final_taxon)])),
                                       collapse = "; "),
            .groups = "drop") %>%
  # Resultado incluyendo cf. y aff. No hubo ningún espécimen técnicamente evaluable
  mutate(ltr_resolution_status = case_when(n_technically_successful == 0 ~
                                             "Not assessable due to technical failure",
                                           # Todos los especímenes técnicamente evaluables llegaron a especie
                                           n_specimens_resolved_to_species == n_technically_successful &
                                             n_technical_failures == 0 ~ "Fully resolved to species level",
                                           # Todos los especímenes evaluables llegaron a especie,
                                           # pero hubo fallos técnicos
                                           n_specimens_resolved_to_species == n_technically_successful &
                                             n_technical_failures > 0 ~
                                             "Fully resolved among technically successful specimens; technical failures present",
                                           # Algunos evaluables llegaron a especie y otros no
                                           n_specimens_resolved_to_species > 0 ~
                                             "Partially resolved to species level",
                                           # Hubo evaluación técnica pero ninguno llegó a especie
                                           TRUE ~ "Unresolved among technically successful specimens"),
         ltr_strict_resolution_status = case_when(n_technically_successful == 0 ~
                                                    "Not assessable due to technical failure",
                                                  n_specimens_resolved_to_strict_species == n_technically_successful &
                                                    n_technical_failures == 0 ~
                                                    "Fully resolved to formal species level",
                                                  n_specimens_resolved_to_strict_species == n_technically_successful &
                                                    n_technical_failures > 0 ~
                                                    "Fully resolved among technically successful specimens; technical failures present",
                                                  n_specimens_resolved_to_strict_species > 0 ~
                                                    "Partially resolved to formal species level",
                                                  TRUE ~
                                                    "No formal species-level resolution among technically successful specimens")) %>%
  arrange(initial_taxonomic_level,
          initial_taxon)

print(ltr_taxon_resolution, n = Inf)

ltr_taxon_resolution_summary <- ltr_taxon_resolution %>%
  count(ltr_resolution_status,
        name = "n_ltr_categories") %>%
  mutate(percentage = round(100 * n_ltr_categories / sum(n_ltr_categories),
                            1))

print(ltr_taxon_resolution_summary, n = Inf)

ltr_taxon_strict_resolution_summary <- ltr_taxon_resolution %>%
  count(ltr_strict_resolution_status,
        name = "n_ltr_categories") %>%
  mutate(percentage = round(100 * n_ltr_categories / sum(n_ltr_categories), 1))

print(ltr_taxon_strict_resolution_summary, n = Inf)

# Inventario inicial
initial_taxa <- taxa_inventory %>%
  filter(!is.na(initial_taxon)) %>%
  group_by(initial_taxon) %>%
  summarise(pre_taxid = first(pre_taxid),
            initial_taxonomic_level = first(initial_taxonomic_level),
            initial_open_nomenclature = first(initial_open_nomenclature),
            initial_ltr = first(initial_ltr),
            initial_reference_available = any(initial_reference_available,
                                              na.rm = TRUE),
            n_specimens = n(),
            .groups = "drop")

# Taxones inicialmente identificados a especie
initial_species_taxa <- initial_taxa %>%
  filter(initial_taxonomic_level == "Species")

initial_species_summary <- initial_species_taxa %>%
  summarise(initial_species_strict = sum(!initial_open_nomenclature),
            initial_species_including_open = n(),
            initial_species_without_reference = sum(!initial_reference_available))

print(initial_species_summary)

# Inventario final
final_species_taxa <- taxa_inventory %>%
  filter(final_taxonomic_level == "Species",
         !is.na(final_taxon)) %>%
  group_by(final_taxon) %>%
  summarise(final_taxid = first(final_taxid),
            final_open_nomenclature = first(final_open_nomenclature),
            has_study_sequence = any(has_study_sequence,
                                     na.rm = TRUE),
            has_coi_sequence = any(!is.na(accession_coi),
                                   na.rm = TRUE),
            has_16s_sequence = any(!is.na(accession_16s),
                                   na.rm = TRUE),
            n_specimens = n(),
            .groups = "drop")

final_species_summary <- final_species_taxa %>%
  summarise(final_species_strict = sum(!final_open_nomenclature),
            final_species_including_open = n(),
            final_species_with_study_sequence = sum(has_study_sequence),
            final_species_with_coi_sequence = sum(has_coi_sequence),
            final_species_with_16s_sequence = sum(has_16s_sequence))

print(final_species_summary)

# Cambio en el inventario a nivel de especie
taxonomic_gain_summary <- bind_cols(initial_species_summary,
                                    final_species_summary) %>%
  mutate(net_gain_strict_species = final_species_strict - initial_species_strict,
         net_gain_including_open = final_species_including_open -
           initial_species_including_open)

print(taxonomic_gain_summary,
      width = Inf)

# Disponibilidad inicial de referencias y clasificacion LTR
initial_reference_gap <- initial_taxa %>%
  mutate(reference_gap_status = case_when(initial_ltr ~
                                            "LTR: not assessable at species level",
                                          initial_taxonomic_level == "Species" &
                                            !initial_reference_available ~ 
                                            "Species-level taxon without public reference",
                                          initial_taxonomic_level == "Species" &
                                            initial_reference_available ~
                                            "Species-level taxon with public reference",
                                          TRUE ~ "Other / unresolved"))

initial_reference_gap_summary <- initial_reference_gap %>%
  count(reference_gap_status,
        name = "n_taxonomic_categories") %>%
  mutate(percentage = round(100 * n_taxonomic_categories /
                              sum(n_taxonomic_categories), 1))

print(initial_reference_gap_summary, n = Inf)

# Desglose de LTR por rango taxonomico
ltr_category_summary <- initial_taxa %>%
  filter(initial_ltr) %>%
  count(initial_taxonomic_level,
        name = "n_ltr_categories") %>%
  mutate(percentage_of_ltr_categories = round(100 * n_ltr_categories /
                                                sum(n_ltr_categories), 1))

print(ltr_category_summary, n = Inf)

# Resolucion de casos LTR a nivel de especimen
ltr_resolution_summary <- taxa_inventory %>%
  filter(initial_ltr) %>%
  # Número de especímenes inicialmente LTR.
  summarise(ltr_specimens = n(),
            # Número de categorías iniciales únicas clasificadas como LTR.
            ltr_initial_categories = n_distinct(initial_taxon),
            # Resolución a especie incluyendo cf. y aff.
            ltr_specimens_resolved_to_species = sum(final_resolved_to_species,
                                                    na.rm = TRUE),
            # Resolución estricta a especie, excluyendo cf. y aff.
            ltr_specimens_resolved_to_strict_species = sum(final_resolved_to_strict_species,
                                                           na.rm = TRUE),
            percentage_resolved_to_species = round(100 * ltr_specimens_resolved_to_species /
                                                     ltr_specimens, 1),
            percentage_resolved_to_strict_species = round(100 * ltr_specimens_resolved_to_strict_species /
                                                            ltr_specimens, 1))

print(ltr_resolution_summary)

# Taxones finales a especie derivados de casos iniciales LTR
final_species_from_ltr <- taxa_inventory %>%
  filter(initial_ltr,
         final_taxonomic_level == "Species",
         !is.na(final_taxon)) %>%
  distinct(initial_taxon,
           voucher_id,
           final_taxon,
           final_taxid,
           final_open_nomenclature,
           accession_coi,
           accession_16s) %>%
  arrange(initial_taxon,
          final_taxon)

print(final_species_from_ltr, n = Inf)

final_species_from_ltr_summary <- final_species_from_ltr %>%
  distinct(final_taxon,
           final_open_nomenclature) %>%
  summarise(final_species_taxa_from_ltr = n(),
            final_strict_species_taxa_from_ltr = sum(!final_open_nomenclature),
            final_open_nomenclature_taxa_from_ltr = sum(final_open_nomenclature))

print(final_species_from_ltr_summary)

# Reproducibility record / Registro de reproducibilidad ------------------------
write_lines(capture.output(sessionInfo()),
            file.path(tables_dir, "session_info.txt"))
