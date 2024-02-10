conditions <- expand.grid(bedingung = c("kurz", "lang"), geschw = c("schnell", "langsam"), blick = c("vr", "desktop"))

outlier_lists <- list()
outlier_lists_u <- list()
dsa_cleaned <- NULL

for (i in 1:nrow(conditions)) {
  subset_dsa <- dsa[dsa$bedingung == conditions[i, "bedingung"] & dsa$geschw == conditions[i, "geschw"] & dsa$Blick == conditions[i, "blick"], ]
  
  outlier_list <- list()
  outlier_list_u <- list()
  
  for (j in 1:53) {
    outliers <- outliers_mad(subset_dsa[subset_dsa$ID == j, "distWalked"], threshold = 3)
    outlier_limits <- outliers[["limits"]]
    outlier_list[[j]] <- outlier_limits[[2]]
    outlier_list_u[[j]] <- outlier_limits[[1]]
  }
  
  outlier_lists[[i]] <- outlier_list
  outlier_lists_u[[i]] <- outlier_list_u
  
  for (j in 1:53) {
    outlier_indices_s <- which(subset_dsa$ID == j & subset_dsa$distWalked > outlier_list[[j]])
    if (length(outlier_indices_s) > 0) {
      subset_dsa[outlier_indices_s, ] <- NA
    }
    
    outlier_indices_u <- which(subset_dsa$ID == j & subset_dsa$distWalked < outlier_list_u[[j]])
    if (length(outlier_indices_u) > 0) {
      subset_dsa[outlier_indices_u, ] <- NA
    }
  }
  
  subset_dsa <- na.omit(subset_dsa)
  
  dsa_cleaned <- rbind(dsa_cleaned, subset_dsa)
}


dsa_k <- dsa_cleaned[dsa_cleaned$bedingung=="kurz",]
dsa_l <- dsa_cleaned[dsa_cleaned$bedingung=="lang",]