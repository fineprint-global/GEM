# This function performs the global hypothetical extraction method 
# for sector (either product or industry) s in region r
# It nullifies s-r and computes the thereby derived new MRIO

# A test case:
# s <- 123 (motor vehicles)
# r <-31 (China)
# r <- 1

Run_GEM <- function(s,r)
{
  
  # Read row/column indices for different sector region combinations that are needed 
  
  ind <- list("s" = MRIO$Code$Product %>% filter(ProductCode == s) %>% pull(Index),
              "r" = MRIO$Code$Product %>% filter(RegionCode == r) %>% pull(Index),
              "s_r" = MRIO$Code$Product %>% filter(ProductCode == s & RegionCode == r) %>% pull(Index),
              "s_without_r" = MRIO$Code$Product %>% filter(ProductCode == s & RegionCode != r) %>% pull(Index),
              "r_without_s" = MRIO$Code$Product %>% filter(ProductCode != s & RegionCode == r) %>% pull(Index) )
  
  A <- MRIO$A   # Create new inter-industry matrix for post-extraction scenario
  y <- MRIO$Y   # Create new final demand block for post-extraction scenario
  
  A[ind$s_r,] <- 0   # Nullify outputs of sector s from region r (row)
  A[,ind$s_r] <- 0   # Nullify inputs of sector s in region r (column)
  y[ind$s_r,] <- 0   # Nullify global final demand for products from sector s 
  
  # Global inputs of sector s into any sector (but s) in region r
  Global_s_for_r <- colSums( MRIO$A[ ind$s , ind$r_without_s ] )               
  
  # Domestic inputs of sector s into any sector (but s) in region r
  Domestic_s_for_r <- MRIO$A[ ind$s_r , ind$r_without_s ]
  
  # Foreign sector s input into any sector (but s) in region r
  Foreign_s_for_r <- colSums( MRIO$A[ ind$s_without_r , ind$r_without_s ] )
  
  
  ## 1. Offset sector s-R inputs within region r through scalling of imports of s
  
  # Sectoral scaling factors for imports of s into region r
  Scale <- 1 + Domestic_s_for_r / Foreign_s_for_r
  
  # Offset infinite and NA values with zero
  Scale[ is.na(Scale) ] <- 0
  Scale[ Scale == Inf ] <- 0
  
  # Store numbers for report
  test <- data.frame("before" = colSums( MRIO$A[,ind$r_without_s] ) )
  
  # Calculate intermediate inputs before scaling
  test["without_scaling"] <- colSums( A[,ind$r_without_s] )
  
  # Apply scaling factor
  A[ind$s_without_r,ind$r_without_s] <- A[ind$s_without_r,ind$r_without_s] %*% diag(Scale) 
  
  # Compute other test metrics
  test["after"] <- colSums( A[,ind$r_without_s] )
  test["diff"] <- test$before - test$after
  
  # Remove all obsolete objects 
  remove(Foreign_s_for_r, Domestic_s_for_r, Global_s_for_r, Scale, test)
  
  
  ## 2. Offset sector s-R inputs with all other regions except R by scalling imports of s
  
  not_r <- setdiff(1:49,r)  # All regions but r
  
  # Loop over all other regions for stepwise scalling of imports
  
  for(j in not_r)
  {
    # Read indices of products in region j
    ind[["j"]] <- MRIO$Code$Product %>% filter(RegionCode == j) %>% pull(Index)
    
    # Inputs of sector s from region r for sectors in region j
    s_from_r <- MRIO$A[ ind$s_r , ind$j ]
  
    # Global imports of sector s sourced from regions other than r and j 
    s_from_not_rj <- colSums( MRIO$A[ ind$s[ setdiff(not_r,j) ] , ind$j ] )
    
    
    # Sectoral scaling factors for imports of s into region j
    Scale <- 1 + s_from_r / s_from_not_rj
    
    # Offset infinite and NA values with zero
    Scale[ is.na(Scale) ] <- 0
    Scale[ Scale == Inf ] <- 0
    
     # Store original numbers for report
    test <- data.frame( "before" = colSums( MRIO$A[ , ind$j] ) )
    
    # Calculate intermediate inputs before scaling
    test["without_scaling"] <- colSums( A[, ind$j] )
    
    # Apply scaling factor
    A[ ind$s[ setdiff(not_r,j) ] , ind$j ] <-  A[ ind$s[ setdiff(not_r,j) ] , ind$j ] %*% diag(Scale)
  
    # Compute other test metrics
    test["after"] <- colSums( A[, ind$j] )
    test["diff"] <- test$before - test$after
    
    print( paste("Maximum of sectoral difference for region", j,":",max(test$diff) ) )
    print( paste("Minimum of sectoral difference for region", j,":",min(test$diff) ) )
      
  }

}