# Read in and prepare data
# (European Regional Database by Cambridge Econometrics)
#
# Author: LV
# Editor: NK

# Helper function ---------------------------------------------------------

fix_data <- function(x, col_string) {
  
  x <- x[x$"Nuts level" != "nuts 3", ]
  x <- as.data.frame(x)
  for(i in 1:nrow(x)) {
    if(nchar(x[i, 2]) == 3) {
      x[i, 2] <- paste(x[i, 2], "0", sep = "")
    } else if(nchar(x[i, 2]) == 2) {
      x[i, 2] <- paste(x[i, 2], "00", sep = "")
    } 
  }
  
  x["Nuts level"] <- NULL
  names(x)[1] <- "NUTS_ID"
  x$NUTS_ID <- toupper(x$NUTS_ID)
  x <- x[, colSums(is.na(x)) != nrow(x)]
  x <- x[!apply(is.na(x) | x == "", 1, all), ]
  for(i in 2:ncol(x)){
    names(x)[i] <- paste(col_string, names(x)[i], sep = "_")
  }
  return(x)
}


# Data --------------------------------------------------------------------

library(dplyr)
library(readr)


# GDP, GVA & POP ----------------------------------------------------------

gva_tot <- read_delim("data/CSV/GVA_total.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
gva_tot <- fix_data(gva_tot, "GVA_tot")

gdp_tot <- read_delim("data/CSV/GDP_total.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
gdp_tot <- fix_data(gdp_tot, "GDP_tot")

pop_tot <- read_delim("data/CSV/POP_total.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
pop_tot <- fix_data(pop_tot, "POP_tot")

# Per capita
gva_pc <- gva_tot
gva_pc[, 2:ncol(gva_pc)] <- gva_tot[, 2:ncol(gva_tot)] / pop_tot[, 2:ncol(pop_tot)] * 10^6
names(gva_pc)[2:ncol(gva_pc)] <- paste0("GVA_pc_", seq(1980, 2015))

gdp_pc <- gdp_tot
gdp_pc[, 2:ncol(gdp_pc)] <- gdp_tot[, 2:ncol(gdp_tot)] / pop_tot[, 2:ncol(pop_tot)] * 10 ^ 6
names(gdp_pc)[2:ncol(gdp_pc)] <- paste0("GDP_pc_", seq(1980, 2015))


# Splitting variables -----------------------------------------------------

inv_tot <- read_delim("data/CSV/GFCF_total.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
inv_tot <- fix_data(inv_tot, "INV_tot")

inv_agr <- read_delim("data/CSV/GFCF_agriculture.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
inv_agr <- fix_data(inv_agr, "INV_agr")

inv_ind <- read_delim("data/CSV/GFCF_industry.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
inv_ind <- fix_data(inv_ind, "INV_ind")

inv_con <- read_delim("data/CSV/GFCF_construction.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
inv_con <- fix_data(inv_con, "INV_con")

inv_nms <- read_delim("data/CSV/GFCF_NMS.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
inv_nms <- fix_data(inv_nms, "INV_nms")

inv_mar <- inv_tot
inv_mar[, 2:ncol(inv_mar)] <- inv_tot[, 2:ncol(inv_tot)] - inv_nms[, 2:ncol(inv_nms)]
names(inv_mar)[2:ncol(inv_mar)] <- paste0("INV_mar_", seq(1980, 2015))


emp_tot <- read_delim("data/CSV/EMP_total.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
emp_tot <- fix_data(emp_tot, "EMP_tot")

emp_agr <- read_delim("data/CSV/EMP_agriculture.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
emp_agr <- fix_data(emp_agr, "EMP_agr")

emp_ind <- read_delim("data/CSV/EMP_industry.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
emp_ind <- fix_data(emp_ind, "EMP_ind")

emp_con <- read_delim("data/CSV/EMP_construction.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
emp_con <- fix_data(emp_con, "EMP_con")

emp_nms <- read_delim("data/CSV/EMP_NMS.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
emp_nms <- fix_data(emp_nms, "EMP_nms")

emp_mar <- emp_tot
emp_mar[, 2:ncol(emp_mar)] <- emp_tot[, 2:ncol(emp_tot)] - emp_nms[, 2:ncol(emp_nms)]
names(emp_mar)[2:ncol(emp_mar)] <- paste0("EMP_mar_", seq(1980, 2015))


thw_tot <- read_delim("data/CSV/THW_total.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
thw_tot <- fix_data(thw_tot, "THW_tot")


thw_agr <- read_delim("data/CSV/THW_agriculture.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
thw_agr <- fix_data(thw_agr, "THW_agr")


thw_ind <- read_delim("data/CSV/THW_industry.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
thw_ind <- fix_data(thw_ind, "THW_ind")


thw_con <- read_delim("data/CSV/THW_construction.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
thw_con <- fix_data(thw_con, "THW_con")


thw_nms <- read_delim("data/CSV/THW_NMS.csv", 
                      ";", escape_double = FALSE, na = ".NaN", 
                      trim_ws = TRUE, skip = 1)
thw_nms <- fix_data(thw_nms, "THW_nms")

thw_mar <- thw_tot
thw_mar[, 2:ncol(thw_mar)] <- thw_tot[, 2:ncol(thw_tot)] - thw_nms[, 2:ncol(thw_nms)]
names(thw_mar)[2:ncol(thw_mar)] <- paste0("THW_mar_", seq(1980, 2015))


# Spatial -----------------------------------------------------------------

library(rgdal)
library(raster)
library(geospacom)


poly <- readOGR(dsn = "./data/SHP/NUTS_RG_03M_2013_4326_LEVL_2.shp")
poly <- poly[substring(poly$NUTS_ID, 1, 2) != "LI",]
#poly <- poly[substring(poly$NUTS_ID, 1, 2) != "CY", ]
poly <- poly[substring(poly$NUTS_ID, 1, 2) != "MK",]
poly <- poly[substring(poly$NUTS_ID, 1, 2) != "ME",]
#poly <- poly[substring(poly$NUTS_ID, 1, 2) != "MT", ]
poly <- poly[substring(poly$NUTS_ID, 1, 2) != "TR",]
poly <- poly[substring(poly$NUTS_ID, 1, 2) != "CH",]
poly <- poly[substring(poly$NUTS_ID, 1, 2) != "HR",]
poly <- poly[poly$NUTS_ID != "IS00", ]
poly <- poly[poly$NUTS_ID != "PT20", ]
poly <- poly[poly$NUTS_ID != "PT30", ]
poly <- poly[poly$NUTS_ID != "FRA1", ]
poly <- poly[poly$NUTS_ID != "FRA2", ]
poly <- poly[poly$NUTS_ID != "FRA3", ]
poly <- poly[poly$NUTS_ID != "FRA4", ]
poly <- poly[poly$NUTS_ID != "FRA5", ]
poly <- poly[poly$NUTS_ID != "ES70", ]
poly$AREA <- area(poly) / 1000000

dist <- DistanceMatrix(poly, id = "NUTS_ID")

poly_nuts <- poly$NUTS_ID

# Calculate population density
pop_tot <- pop_tot[match(poly_nuts, pop_tot$NUTS_ID),]
pop_den <- inner_join(pop_tot, poly@data[, c("NUTS_ID", "AREA")], by = "NUTS_ID")
pop_den[, 2:(ncol(pop_den) - 1)] <- pop_den[, 2:(ncol(pop_den) - 1)] / pop_den$AREA * 1000
pop_den$AREA <- NULL
names(pop_den)[2:ncol(pop_den)] <- paste0("POP_den_", seq(1980, 2015))


# Transformations-----------------------------------------------------------

split_list <- list(pop_den = pop_den,
                   inv_agr = inv_agr, inv_con = inv_con, inv_ind = inv_ind, 
                   inv_mar = inv_mar, inv_nms = inv_nms, 
                   emp_agr = emp_agr, emp_con = emp_con, emp_ind = emp_ind, 
                   emp_mar = emp_mar, emp_nms = emp_nms, 
                   thw_agr = thw_agr, thw_con = thw_con, thw_ind = thw_ind, 
                   thw_mar = thw_mar, thw_nms = thw_nms)
split_list <- lapply(split_list, function(x) {
  x[match(poly_nuts, x$NUTS_ID),]
  })

for(i in 1:length(split_list)) {
  split_list[[i]][, 2:ncol(split_list[[i]])] <- 
    apply(split_list[[i]][, 2:ncol(split_list[[i]])], 2, function(x) {
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
      })
}

depvar_list <- list(gva_tot = gva_tot, gdp_tot = gdp_tot, 
                    gva_pc = gva_pc, gdp_pc = gdp_pc)
depvar_list <- lapply(depvar_list, function(x) {
    x[match(poly_nuts, x$NUTS_ID),]
  })

for(i in 1:length(depvar_list)) {
  depvar_list[[i]][, 2:ncol(depvar_list[[i]])] <-
    apply(depvar_list[[i]][, 2:ncol(depvar_list[[i]])], 2, log)
  
}

gdp_pc_gr <- depvar_list$gdp_pc
gdp_pc_gr$GDP_gr_8015 <- gdp_pc_gr$GDP_pc_2015 - gdp_pc_gr$GDP_pc_1980
gdp_pc_gr$GDP_gr_9115 <- gdp_pc_gr$GDP_pc_2015 - gdp_pc_gr$GDP_pc_1991
gdp_pc_gr$GDP_gr_9515 <- gdp_pc_gr$GDP_pc_2015 - gdp_pc_gr$GDP_pc_1995
gdp_pc_gr$GDP_gr_9505 <- gdp_pc_gr$GDP_pc_2005 - gdp_pc_gr$GDP_pc_1995
gdp_pc_gr$GDP_gr_0515 <- gdp_pc_gr$GDP_pc_2015 - gdp_pc_gr$GDP_pc_2005
gdp_pc_gr$GDP_gr_0015 <- gdp_pc_gr$GDP_pc_2015 - gdp_pc_gr$GDP_pc_2000
gdp_pc_gr$GDP_gr_0007 <- gdp_pc_gr$GDP_pc_2007 - gdp_pc_gr$GDP_pc_2000
gdp_pc_gr$GDP_gr_0815 <- gdp_pc_gr$GDP_pc_2015 - gdp_pc_gr$GDP_pc_2008

gdp_pc_gr <- gdp_pc_gr[, c(1, 38:ncol(gdp_pc_gr))]

gva_pc_gr <- depvar_list$gva_pc
gva_pc_gr$GVA_gr_8015 <- gva_pc_gr$GVA_pc_2015 - gva_pc_gr$GVA_pc_1980
gva_pc_gr$GVA_gr_9115 <- gva_pc_gr$GVA_pc_2015 - gva_pc_gr$GVA_pc_1991
gva_pc_gr$GVA_gr_9515 <- gva_pc_gr$GVA_pc_2015 - gva_pc_gr$GVA_pc_1995
gva_pc_gr$GVA_gr_9505 <- gva_pc_gr$GVA_pc_2005 - gva_pc_gr$GVA_pc_1995
gva_pc_gr$GVA_gr_0515 <- gva_pc_gr$GVA_pc_2015 - gva_pc_gr$GVA_pc_2005
gva_pc_gr$GVA_gr_0015 <- gva_pc_gr$GVA_pc_2015 - gva_pc_gr$GVA_pc_2000
gva_pc_gr$GVA_gr_0007 <- gva_pc_gr$GVA_pc_2007 - gva_pc_gr$GVA_pc_2000
gva_pc_gr$GVA_gr_0815 <- gva_pc_gr$GVA_pc_2015 - gva_pc_gr$GVA_pc_2008

gva_pc_gr <- gva_pc_gr[, c(1, 38:ncol(gva_pc_gr))]

depvar_list[["gva_pc_gr"]] <- gva_pc_gr
depvar_list[["gdp_pc_gr"]] <- gdp_pc_gr

# split_names <- names(split_list)
# 
# for(i in split_names) {
#   assign(i, split_list[[i]])
# }

# depvar_names <- names(depvar_list)
# 
# for(i in depvar_names) {
#   assign(i, depvar_list[[i]])
# }


# Save --------------------------------------------------------------------

save(poly, split_list, depvar_list, dist, file = "./data/processed_data.Rdata")
