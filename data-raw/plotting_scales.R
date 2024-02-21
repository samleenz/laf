# Wed Jul 26 14:19:12 2023 ------------------------------
# Sam Lee

# Plotting colours
library(RColorBrewer)
library(ggplot2)

pal_tissue <- setNames(
  brewer.pal(6, "Set2"),
  c("Bone", "Brain", "Liver", "Lungs", "Tumour", "LymphNode")
)
colour_tissue <- scale_colour_manual(values = pal_tissue)
fill_tissue <- scale_fill_manual(values = pal_tissue)
# fill_tissue <- colour_tissue
# fill_tissue$aesthetics <- "fill"


pal_mouse <- setNames(
  brewer.pal(7, "Set1"),
  c("Ms941", "Ms1097", "Ms1098", "Ms1100", "Ms1295", "Ms1297", "Ms1300")
)
colour_mouse <- scale_colour_manual(values = pal_mouse)
fill_mouse <- scale_fill_manual(values = pal_mouse)

pal_clone <- setNames(
  scico::scico(6, palette = "tokyo", categorical = TRUE),
  c("12", "2",  "4",  "1", "16", "6")
)
colour_clone <- scale_colour_manual(values = pal_clone)
fill_clone <- scale_fill_manual(values = pal_clone)

pal_batch <- setNames(
  # brewer.pal(3, "Paired"),
  scico::scico(4, palette = "navia", categorical = TRUE),
  c("B1", "B2", "B3", "B4")
)
colour_batch <- scale_colour_manual(values = pal_batch[-2])
fill_batch <- scale_fill_manual(values = pal_batch[-2])

updn_cols_dark <-  c(brewer.pal(6, 'Greens')[4], brewer.pal(6, 'Purples')[4]) # pinched from dhamresh :-)
tble_cols = c(brewer.pal(6, 'Greens')[2], brewer.pal(6, 'Purples')[2]) # 2023-08-11, for the signif results in DataTables

pal_all <- c(pal_tissue, pal_mouse, pal_clone, pal_batch[-2])
colour_all <- scale_colour_manual(values = pal_all)
fill_all <- scale_fill_manual(values = pal_all)

plotting_scales <- list(
  colour_tissue = colour_tissue,
  fill_tissue = fill_tissue,
  colour_mouse = colour_mouse,
  fill_mouse = fill_mouse,
  colour_clone = colour_clone,
  fill_clone = fill_clone,
  colour_batch = colour_batch,
  fill_batch = fill_batch,
  colour_all = colour_all,
  fill_all = fill_all,
  pal_all = pal_all,
  updn_cols_dark = updn_cols_dark,
  tble_cols = tble_cols
)

usethis::use_data(plotting_scales, overwrite = TRUE)
