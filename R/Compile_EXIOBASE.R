# Loading and parsing EXIOBASE 3.6 variables

Compile_EXIOBASE <- function(year,type)
{
  
  ### 1. Load and clean objects ###
  
  Code <- list("Product" = read.csv(paste0(path$exio,"IO_product_codes.csv")),
               "Industry" = read.csv(paste0(path$exio,"IO_industry_codes.csv")),
               "Demand" = read.csv(paste0(path$exio,"IO_finaldemand_codes.csv")) ) 
  
  Uni <- list("Product" = Code$Product[1:200,c("Index","ProductEXIOCode")],
              "Industry" = Code$Industry[1:163,c("Index","IndustryEXIOCode")],
              "Region" = read.csv(paste0(path$exio,"region_codes.csv"))
              )
  
  # Change colnames for ease of access
  colnames(Uni$Product)[1] <- c("ProIndex")    
  colnames(Uni$Industry)[1] <- c("IndIndex")
  
  # Supply table (products by industry):
  S <- read.csv(paste0(path$exio,"MRSUT_",year,"/supply.csv"),header = FALSE)
  S <- as.matrix(S) # Transform into matrix
  
  # Use table (products by industry):
  U <- read.csv(paste0(path$exio,"MRSUT_",year,"/use.csv"),header = FALSE)
  U <- as.matrix(U)  # Transform into matrix
  
  # Final demand block (product by final demand category)
  Y <- read.csv(paste0(path$exio,"MRSUT_",year,"/final_demand.csv"),header = FALSE)
  Y <- as.matrix(Y)  # Transform into matrix
  
  # Value added by industry block
  v <- read.csv(paste0(path$exio,"MRSUT_",year,"/value_added.csv"),header = FALSE)
  v <- colSums(v)
  
  # Concordance for agrgegating materials in four main categories
  Conco <- read.xlsx(paste0(path$exio,"extensions/material/Concordance_material.xlsx"),sheet = 1)
  
  # Raw material extraction extension
  Raw <- read.xlsx(paste0(path$exio,"extensions/material/Material_extensions_used_",year,".xlsx"),sheet = 2)
  
  # Aggregating material extraction into four categories and transform codes to numeric:
  E_raw <- Raw %>% 
    left_join(Conco, by = c("PhysicalTypeName" = "Child")) %>% 
    select(ProductTypeCode, CountryCode, Amount, MotherCode) %>% 
    group_by(ProductTypeCode, CountryCode, MotherCode) %>% 
    summarise(Amount = sum(Amount)) %>% 
    ungroup(ProductTypeCode, CountryCode, MotherCode) %>% 
    left_join(Uni$Product, by = c("ProductTypeCode" = "ProductEXIOCode")) %>%
    select(CountryCode, MotherCode, Amount, ProIndex) %>% 
    left_join(Uni$Region, by = c("CountryCode" = "Region") ) %>% 
    select(MotherCode, Amount, ProIndex, RegIndex) %>% 
    mutate(Index = (RegIndex - 1) * 200 +  ProIndex) 
  
  E <- as.data.frame( matrix(0, nrow = 4, ncol = 9800))
  rownames(E) <- c("Biomass","Metal","Mineral","Fossil")
  
  E[ as.matrix(E_raw[,c("MotherCode","Index")]) ] <- E_raw$Amount
  
  remove(E_raw,Raw)  # Object not needed anymore
  
  
  ### 2. Calculating general variables ###
  
  x <- colSums(S)                   # Gross production of all industry
  q <- rowSums(S)                   # Gross production of all products
  
  D <- t(S/q)                       # Commodity output proportions matrix (industry by product)
  D[is.na(D)] <- 0                  # Set NaN (due to zero gross output) to zero
  
  C <- t(S)/x                       # Industry output proportions matrix (industry by product)
  C[is.na(C)] <- 0                  # Set NaN (due to zero gross output) to zero
  
  B <- t(t(U)/x)                    # All commodity by industry coefficient matrix
  B[is.na(B)] <- 0                  # Set NaN (due to zero gross output) to zero
  
  

  ### 3. Compile MRIO depending on type (industry vs. product classification) ###
  
  if(type == "IxI")
  {
    A <- D %*% B                    # Ind-by-Ind technology matrix
    
    I <- diag( rep( 1,nrow(A) ) )   # Identity matrix
    L <- solve(I - A)               # Inverse
    
    Z <- A %*% diag(x)              # IxI transaction matrix
    
    Y <- D %*% Y                    # Transform to industry classification
  }
  
  if(type == "PxP")
  {
    A <- B %*% D                    # Pro-by-Pro technology matrix
    
    I <- diag( rep( 1,nrow(A) ) )   # Identity matrix
    L <- solve(I - A)               # Inverse
    
    Z <- A %*% diag(q)              # IxI transaction matrix
    
    v <- colSums( C * v )           # value added by products
  }
  
  obj <- list("Z" = Z,
              "Y" = Y,
              "v" = v,
              "A" = A,
              "L" = L,
              "x" = x,
              "E" = E
              )
  
  return(obj)
}
