# This function performs the global hypothetical extraction method 
# for sector (either product or industry) s in region r
# It nullifies s-r and computes the thereby derived new MRIO

# A test case:
# s <- 123 (motor vehicles)
# r <-31 (China)

Run_GEM <- function(s,r)
{
  
  # Read row/column indices for different sector region combinations that are needed 
  
  ind <- list("s" = MRIO$Code$Product %>% filter(ProductCode == s) %>% 
                pull(Index),
              "r" = MRIO$Code$Product %>% filter(RegionCode == r) %>% 
                pull(Index),
              "s_r" = MRIO$Code$Product %>% filter(ProductCode == s & RegionCode == r) %>% 
                pull(Index),
              "s_without_r" = MRIO$Code$Product %>% filter(ProductCode == s & RegionCode != r) %>% 
                pull(Index),
              "r_without_s" = MRIO$Code$Product %>% filter(ProductCode != s & RegionCode == r) %>% 
                pull(Index) )
  
  A <- MRIO$A   # Create new inter-industry matrix
  y <- MRIO$Y   # Create new final demand block
  
  # Global/total sector s intput into each sector (without s) in region r
  All_s_for_r <- colSums( A[ ind$s , ind$r_without_s ] )               
  
  # Foreign sector s input into each sector (without s) in region r
  Foreign_s_for_r <- colSums( A[ ind$s_without_r , ind$r_without_s ] )
  
  # s import shares for all sectors (that are not s) in region r  
  IM_Share_s <- 100 * Foreign_s_for_r / All_s_for_r
  
  # Sectoral scaling factors 
  Scale_IM_s <- 100/IM_Share_s
  
  # Offset infinite and NA values with zero
  Scale_IM_s[is.na(Scale_IM_s)] <- 0
  Scale_IM_s[Scale_IM_s == Inf] <- 0
  
  # Store numbers for report
  test <- data.frame("before" = colSums( A[,ind$r_without_s] ) )
  
  A[ind$s_r,] <- 0   # Nullify intermediate output of s-r (row)
  A[,ind$s_r] <- 0   # Nullify intermediate inputs of s-r (column)
  
  # Calculate intermediate inputs before scaling
  test["without_scaling"] <- colSums( A[,ind$r_without_s] )
  
  # Apply scaling factor
  A[ind$s_without_r,ind$r_without_s] <- A[ind$s_without_r,ind$r_without_s] %*% diag(Scale_IM_s) 
  
  # Compute other test metrics
  test["after"] <- colSums( A[,ind$r_without_s] )
  test["diff"] <- test$before - test$after
  

  y[ind$s_r,] <- 0   # Nullify s-r deliveries to final demand
  
  
    
  
}