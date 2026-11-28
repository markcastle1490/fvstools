################################################################################
#This file contains a suite of R functions that can be used to derive competition
#and density attributes. The general expectation when using these functions is 
#that the user is working with a tree-level dataset that contains attributes 
#including DBH, expansion factors, species, total tree height, and others 
#relevant as described in the documentation for each function. Many of the 
#attributes in this file can be calculated for custom size ranges (DBH and 
#total tree height) and desired species.
#
#Usage notes:
#Although these functions can be called within loops for subsets of data, they 
#are best used in dplyr or data.table calculation sequences. Below are examples
#of how one could calculate the following variables in a dplyr and data.table
#summarize statement for each stand and year in an input dataframe:
#
#Variables
#BA: basal area of all trees in stand
#BA_GE5: basal area of trees with DBH >= 5" in stand
#BA_PINE: basal area of trees with DBH >= 5" and are from a user defined set of
#pine species in stand.
#
#dplyr example
# fvs_sum = tree %>%
#   group_by(StandID, Year) %>%
#   summarize(BA_ = ba(dbh = DBH, expf = TPA),
#             BA_GE5 = ba(dbh = DBH, expf = TPA, dbhmin = 5),
#             BA_PINE = ba(dbh = DBH, expf = TPA, species = SpeciesFVS, 
#             dbhmin = 5, select_species = c('PP', 'LP')))
#
#data.table example
# fvs_sum = tree[, .(
#   BA_ = ba(dbh = DBH, expf = TPA),
#   BA_GE5 = ba(dbh = DBH, expf = TPA, dbhmin = 5),
#   BA_PINE = ba(dbh = DBH, expf = TPA, species = SpeciesFVS, dbhmin = 5, 
#   select_species = c('PP', 'LP')),
#   by = .(StandID, Year)]
#
#Tree is an input dataframe or data.table. StandID, Year, DBH, TPA, Ht, and 
#SpeciesFVS are variables within the tree dataframe.
################################################################################

################################################################################
#' ba
#' @name ba
#' @description
#' This function calculates a basal area per acre given input vectors 
#' containing diameter and expansion factor values. This attribute can be 
#' calculated for user defined size ranges and for select species.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric basal area per acre value
#' @export
################################################################################

#'@export
ba = function(dbh,
              expf,
              ht = NULL,
              species = NULL,
              dbhmin = 0,
              dbhmax = 999,
              htmin = 0,
              htmax = 999,
              select_species = NULL)
{
  
  # Identify records to include by DBH
  include <- dbh >= dbhmin & dbh < dbhmax
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate BA over DBH, HT, and species
  ba_ <- sum((dbh * dbh * expf * f_con)[include], na.rm = TRUE)
  
  # Capture bad values
  if (is.na(ba_)) ba_ <- 0
  
  # Return BA
  return(ba_)
}

################################################################################
#' tpa
#' @name tpa
#' @description
#' This function calculates a trees/stems per acre given an input vector 
#' containing expansion factors. This attribute can be calculated for user 
#' defined size ranges and for select species.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric trees per acre value
#' @export
################################################################################

tpa = function(expf,
               dbh = NULL,
               ht = NULL,
               species = NULL,
               dbhmin = 0,
               dbhmax = 999,
               htmin = 0,
               htmax = 999,
               select_species = NULL)
{
  
  # Include all records unless a filter is provided
  include <- TRUE
  
  # Apply DBH filter if provided
  if (!is.null(dbh)) {
    include <- include & (dbh >= dbhmin & dbh < dbhmax)
  }
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate TPA over DBH, HT, and species
  tpa_ <- sum(expf[include], na.rm = TRUE)
  
  # Capture bad values
  if (is.na(tpa_)) tpa_ <- 0
  
  # Return TPA
  return(tpa_)
}

################################################################################
#' qmd
#' @name qmd
#' @description
#' This function calculates quadratic mean diameter given vectors containing 
#' DBH and expansion factors. This attribute can be calculated for user 
#' defined size ranges and for select species.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric quadratic mean diameter value
#' @export
################################################################################

qmd = function(dbh,
               expf,
               ht = NULL,
               species = NULL,
               dbhmin = 0,
               dbhmax = 999,
               htmin = 0,
               htmax = 999,
               select_species = NULL)
{
  
  qmd_ <- 0
  
  # Identify records to include by DBH
  include <- dbh >= dbhmin & dbh < dbhmax
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate QMD over DBH, HT, and species
  dbhsq <- sum((dbh * dbh * expf)[include], na.rm = TRUE)
  tpa_ <- sum(expf[include], na.rm = TRUE)
  if (tpa_ > 0) qmd_ <- sqrt(dbhsq / tpa_)
  
  # Capture bad values
  if (is.na(qmd_)) qmd_ <- 0
  
  # Return QMD
  return(qmd_)
}

################################################################################
#' gmd
#' @name gmd
#' @description
#' This function calculates generalized mean diameter (Reineke diameter) given 
#' vectors containing diameter and expansion factors. This attribute can be 
#' calculated for user defined size ranges and for select species.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param ht
#' Vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric GMD value
#' @export
################################################################################

#'@export
gmd = function(dbh,
               expf,
               ht = NULL,
               species = NULL,
               dbhmin = 0,
               dbhmax = 999,
               htmin = 0,
               htmax = 999,
               select_species = NULL)
{
  
  gmd_ <- 0
  
  # Identify records to include by DBH
  include <- dbh >= dbhmin & dbh < dbhmax
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate Reineke diameter over DBH, HT, and species
  gmd_sum <- sum((expf * dbh^r_slope)[include], na.rm = TRUE)
  tpa_ <- sum(expf[include], na.rm = TRUE)
  if (tpa_ > 0) gmd_ <- (gmd_sum / tpa_)^(1 / r_slope)
  
  # Capture bad values
  if (is.na(gmd_)) gmd_ <- 0
  
  # Return GMD
  return(gmd_)
}

################################################################################
#' top_dia
#' @name top_dia
#' @description
#' This function is used to calculate QMD or average diameter weighted by TPA 
#' for the largest trees by DBH within a specified percentage of TPA or an 
#' explicit TPA value. This value is calculated from a set of input vectors 
#' containing diameter values and expansion factors.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors values.
#' 
#' @param top_tpa
#' Numeric value corresponding to amount of TPA to include in top diameter 
#' calculation. Largest 20 TPA, Largest 40 TPA, etc.
#' 
#' @param top_per
#' Numeric value corresponding to percentage of trees to include in the top 
#' diameter calculation. If this value is greater than 0 then it will take 
#' precedence over the value in top_tpa argument.
#' 
#' @param dia_type
#' Integer value used to specify what type of diameter should be calculated. 
#' 1 = QMD 
#' 2 = average diameter weighted by trees per acre 
#' 3 = GMD (Reineke diameter)
#' 
#' @param sorted
#' Logical variable used to indicate if values in dbh are already in descending
#' order.
#' 
#' @return
#' Numeric top diameter value.
#' @export
################################################################################

top_dia = function(dbh,
                   expf,
                   top_tpa = 40,
                   top_per = 0,
                   dia_type = 1,
                   sorted = FALSE)
{
  #Initialize top_dia_
  top_dia_ <- 0
  
  #Validate top_tpa
  if(is.null(top_tpa) || top_tpa < 0) top_tpa <- 40
  
  #Validate top_per. Default to 20 if value is invalid.
  if (top_per < 0 || top_per > 100) {
    top_per <- 20
  }
  
  #Validate dia_type
  if(!dia_type %in% c(1, 2, 3)) dia_type <- 1
  
  #Calculate TPA for the entire stand
  tpa_ <- sum(expf, na.rm = TRUE)
  
  #Do calculations if tpa is > 0
  if(tpa_ > 0)
  {
    # Determine target top TPA
    if (top_per > 0) {
      top <- tpa_ * (top_per / 100)
    } else {
      top <- top_tpa
    }
    
    # Constrain target top TPA to not exceed total stand TPA
    if(top > tpa_) top <- tpa_
    
    #Sort data if needed
    if(!sorted)
    {
      #Get order of DBH values in descending order
      dbh_order <- order(-dbh)
      
      #Sort vectors
      dbh  <- dbh[dbh_order]
      expf <- expf[dbh_order]
    }
    
    #Find the index where top is exceeded
    top_exceed <- match(TRUE, cumsum(expf) >= top)
    
    #Fallback if the threshold is never reached (include all trees)
    if (is.na(top_exceed)) {
      top_exceed <- length(expf)
    }
    
    #Sum expf up to this index
    tpa_sum <- sum(expf[1:top_exceed], na.rm = TRUE)
    
    #Calculate tpa_dif and adjust tpa_sum
    tpa_dif <- tpa_sum - top
    tpa_sum <- tpa_sum - tpa_dif
    
    # Isolate the exact scaled remainder weight for the boundary tree
    remainder_expf <- expf[top_exceed] - tpa_dif
    
    #Quadratic Mean Diameter (QMD)
    if(dia_type == 1) {
      dbh_sum <- if(top_exceed > 1) 
        sum((dbh*dbh * expf)[1:(top_exceed - 1)], na.rm = TRUE) else 0
      dbh_sum <- dbh_sum + (dbh[top_exceed]^2 * remainder_expf)
      if(tpa_sum > 0) top_dia_ <- sqrt(dbh_sum / tpa_sum)
    } 
    
    #Arithmetic Mean Diameter (Weighted by TPA)
    else if (dia_type == 2) {
      dbh_sum <- if(top_exceed > 1) 
        sum((dbh * expf)[1:(top_exceed - 1)], na.rm = TRUE) else 0
      dbh_sum <- dbh_sum + (dbh[top_exceed] * remainder_expf)
      if(tpa_sum > 0) top_dia_ <- dbh_sum / tpa_sum
    } 
    
    #Reineke's Diameter (GMD)
    else {
      dbh_sum <- if(top_exceed > 1) 
        sum((dbh^r_slope * expf)[1:(top_exceed - 1)], na.rm = TRUE) else 0
      dbh_sum <- dbh_sum + (dbh[top_exceed]^r_slope * remainder_expf)
      if(tpa_sum > 0) top_dia_ <- (dbh_sum / tpa_sum)^(1 / r_slope)
    }
  }
  
  return(top_dia_)
}

################################################################################
#' lorey_dia
#' @name lorey_dia
#' @description
#' This function calculates Lorey diameter (basal area weighted average 
#' diameter). This attribute can be calculated for user defined size ranges and 
#' for select species.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric basal area weighted (lorey) diameter value
#' @export
################################################################################

lorey_dia = function(dbh,
                     expf,
                     ht = NULL,
                     species = NULL,
                     dbhmin = 0,
                     dbhmax = 999,
                     htmin = 0,
                     htmax = 999,
                     select_species = NULL)
{
  lorey_dia_ <- 0
  
  # Identify records to include by DBH
  include <- dbh >= dbhmin & dbh < dbhmax
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate expanded tree basal area
  treeba <- dbh * dbh * expf * f_con
  
  # Calculate Lorey diameter over DBH, HT, and species
  dbhsum <- sum((dbh * treeba)[include], na.rm = TRUE)
  ba_ <- sum(treeba[include], na.rm = TRUE)
  if (ba_ > 0) lorey_dia_ <- dbhsum / ba_
  
  # Capture bad values
  if (is.na(lorey_dia_)) lorey_dia_ <- 0
  
  # Return Lorey diameter
  return(lorey_dia_)
}

################################################################################
#' rsdi
#' @name rsdi
#' @description
#' This function calculates Reineke SDI using input vectors containing diameter 
#' and expansion factor values.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @return
#' Numeric Reineke SDI value
#' @export
################################################################################

rsdi = function(dbh,
                expf)
{
  #Calculate TPA
  tpa_ <- tpa(dbh = dbh, expf = expf)
  
  #Calculate qmd
  qmd_ <- qmd(dbh = dbh, expf = expf)
  
  #Calculate rsdi
  rsdi <- tpa_ * (qmd_/10)^r_slope
  
  #Return rsdi
  return(rsdi)
}

################################################################################
#' rsdi_stage
#' @name rsdi_stage
#' @description
#' This function is used to calculate Reineke SDI used the methodology proposed 
#' by Stage 1968. From Section 7.3.2.1 of EFVS using input vectors containing 
#' DBH and expansion factors. This attribute can be calculated for user defined 
#' size ranges and for select species.
#' 
#' SDI = sum(a * TPAi + b * DBHi^2 * TPA) 
#' a = 10^(-1.605) * (1-(1.605/2)) * qmd^1.605 
#' b = 10^(−1.605) * (1.605/2) * QMD^(1.605-2)
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric Reineke SDI calculated using stage formulation.
#' @export
################################################################################

rsdi_stage = function(dbh,
                     expf,
                     ht = NULL,
                     species = NULL,
                     dbhmin = 0,
                     dbhmax = 999,
                     htmin = 0,
                     htmax = 999,
                     select_species = NULL)
{
  rsdi_ <- 0
  
  # Calculate stand-level TPA and squared diameter sum
  stand_tpa <- sum(expf)
  dbhsq <- sum(dbh * dbh * expf)
  
  # Do calculation if stand_tpa > 0
  if (stand_tpa > 0)
  {
    # Initialize a and b parameters
    a <- 10^(-r_slope) * (1 - (r_slope / 2)) * (dbhsq / stand_tpa)^(r_slope / 2)
    
    b <- 10^(-r_slope) * (r_slope / 2) * (dbhsq / stand_tpa)^(r_slope / 2 - 1)
    
    # Identify records to include by DBH
    include <- dbh >= dbhmin & dbh < dbhmax
    
    # Apply height filter if provided
    if (!is.null(ht)) {
      include <- include & (ht >= htmin & ht < htmax)
    }
    
    # Apply species filter if provided
    if (!is.null(select_species) && !is.null(species)) {
      include <- include & (species %in% select_species)
    }
    
    # Calculate RSDI over DBH, HT, and species
    rsdi_ <- sum(
      (a * expf + b * dbh * dbh * expf)[include],
      na.rm = TRUE
    )
    
    # Capture bad values
    if (is.na(rsdi_)) rsdi_ <- 0
  }
  
  # Return RSDI
  return(rsdi_)
}

################################################################################
#' zsdi
#' @name zsdi
#' @description
#' This function calculates Zeide SDI using input vectors containing diameter and 
#' expansion factor values. This attribute can be calculated for user defined 
#' size ranges and for select species.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. This 
#' value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This 
#' value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric Zeide SDI value
#' @export
################################################################################

zsdi = function(dbh,
                expf,
                ht = NULL,
                species = NULL,
                dbhmin = 0,
                dbhmax = 999,
                htmin = 0,
                htmax = 999,
                select_species = NULL)
{
  # Identify records to include by DBH
  include <- dbh >= dbhmin & dbh < dbhmax
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate ZSDI over DBH, HT, and species
  zsdi_ <- sum(((dbh / 10)^r_slope * expf)[include], na.rm = TRUE)
  
  # Capture bad values
  if (is.na(zsdi_)) zsdi_ <- 0
  
  # Return ZSDI
  return(zsdi_)
}

################################################################################
#' cc
#' @name cc
#' @description
#' This function calculates a percent canopy cover value corrected for overlap 
#' using input vectors containing crown width values and expansion factors. This 
#' attribute can be calculated for user defined size ranges and for select 
#' species.
#' 
#' @param crwidth
#' Numeric vector containing crown width values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#'
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. This 
#' value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This 
#' value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all
#' species.
#' 
#' @return
#' Numeric percent canopy cover value
#' @export
################################################################################

cc = function(crwidth,
              expf,
              dbh = NULL,
              ht = NULL,
              species = NULL,
              dbhmin = 0,
              dbhmax = 999,
              htmin = 0,
              htmax = 999,
              select_species = NULL)
{
  # Include all records unless a filter is provided
  include <- TRUE
  
  # Apply DBH filter if provided
  if (!is.null(dbh)) {
    include <- include & (dbh >= dbhmin & dbh < dbhmax)
  }
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate CC over DBH, HT, and species
  cc_ <- sum(
    (((crwidth / 2)^2) * (expf / 43560) * pi * 100)[include],
    na.rm = TRUE
  )
  
  # Capture bad values
  if (is.na(cc_)) cc_ <- 0
  
  # Correct for overlap
  cc_ <- correct_cc(cc_)
  
  return(cc_)
}

################################################################################
#' correct_cc
#' @name correct_cc
#' @description
#' This function takes in an uncorrected percent canopy cover value and returns 
#' a corrected value using the relationship described on page 2 of Crookston, 
#' Nicholas L.; Stage, Albert R. 1999. Percent canopy cover and stand structure 
#' statistics from the Forest Vegetation Simulator. Gen. Tech. Rep. RMRS-GTR-24. 
#' Ogden, UT: U. S. Department of Agriculture, Forest Service, Rocky Mountain 
#' Research Station. 11 p.
# 
#' @param cc
#' cc: Numeric uncorrected CC value
#' 
#' @return
#' Numeric corrected canopy cover value
#' @export
################################################################################

correct_cc = function(cc = 0)
{
  cor_cc <- 0
  
  if(!is.null(cc) && length(cc) > 0)
    cor_cc <- 100 * (1 - exp ( - 0.01* cc))
  
  #Capture bad values
  if(is.na(cor_cc)) cor_cc <- 0
  
  return(cor_cc)
}

################################################################################
#' bal
#' @name bal
#' @description
#' This function calculates basal area in trees larger than subject tree (BAL) 
#' from input vectors containing diameter and expansion factor values.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param no_ties
#' Logical variable used to determine if dbh values with equivalent values get 
#' the same BAL return. If this value is TRUE, then trees with equivalent DBH 
#' values will have a different BAL (3 trees with 10 inch DBH will each have a 
#' different BAL). If this value is FALSE, then trees with equivalent DBH 
#' values will have the same BAL value (e.g. 3 trees with 10 inch DBH will all 
#' have the same BAL).
#' 
#' @param sorted
#' Logical variable used to indicate if values in dbh are already in descending
#' order.
#' 
#' @return
#' Numeric vector containing BAL values
#' @export
################################################################################

bal = function(dbh,
               expf,
               no_ties = TRUE,
               sorted = FALSE)
{
  
  #Get tree basal area
  tree_ba <- (dbh*dbh) * expf * f_con
  
  #Identify initial order
  dbh_order <- seq_along(dbh)
  
  #Determine if sorting should be done
  if(!sorted)
  {
    #Get indices of sorted DBH in descending order
    dbh_order <- order(-dbh)
    
    #Sort vectors
    tree_ba <- tree_ba[dbh_order]
    dbh <- dbh[dbh_order]
  }

  #Don't handle ties.
  if(no_ties)
  {
    #Do a cumulative sum of basal area and then subtract ba of tree from each 
    #record.
    bal <- cumsum(tree_ba) - tree_ba
  }
  
  #Handle ties.
  else
  {
    #Find the index of the first tree (the largest tree) in each tied block.
    first_occurr_idx <- match(dbh, dbh)
      
    #Calculate bal
    bal <- (cumsum(tree_ba) - tree_ba)[first_occurr_idx]
  }
  
  #Reset order to match input
  bal <- bal[match(seq_along(dbh_order), dbh_order)]
  
  return(bal)
}

################################################################################
#' lorey_ht
#' @name lorey_ht
#' @description
#' This function calculates Lorey height (basal area weighted average height). 
#' This attribute can be calculated for user defined size ranges and for select 
#' species.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#
#' @param ht
#' Numeric vector containing total tree height values.
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric Lorey height value
#' @export
################################################################################

#'@export
lorey_ht = function(dbh,
                    ht,
                    expf,
                    species = NULL,
                    dbhmin = 0,
                    dbhmax = 999,
                    htmin = 0,
                    htmax = 999,
                    select_species = NULL)
{
  
  lorey_ht_ <- 0
  
  # Identify records to include by DBH and height
  include <- (dbh >= dbhmin & dbh < dbhmax) &
    (ht >= htmin & ht < htmax)
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate expanded tree basal area
  treeba <- dbh * dbh * expf * f_con
  
  # Calculate Lorey height over DBH, HT, and species
  htsum <- sum((ht * treeba)[include], na.rm = TRUE)
  ba_ <- sum(treeba[include], na.rm = TRUE)
  if (ba_ > 0) lorey_ht_ <- htsum / ba_
  
  # Capture bad values
  if (is.na(lorey_ht_)) lorey_ht_ <- 0
  
  # Return Lorey height
  return(lorey_ht_)
}

################################################################################
#' top_ht
#' @name top_ht
#' @description
#' This function is used to calculate top height for a specified percentage of 
#' trees in the stand or and explicit number of trees (trees per acre) value.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#'
#' @param expf
#' Numeric vector containing expansion factors values.
#' 
#' @param ht
#' Numeric vector containing total tree height values.
#' 
#' @param top_tpa
#' Numeric value corresponding to amount of TPA to include in top height 
#' calculation. Top 20 TPA, top 40 TPA, etc.
#' 
#' @param top_per
#' Numeric value corresponding to percentage of trees to include in the top 
#' height calculation. Largest 20% of trees, largest 40% of trees etc. If this 
#' value is greater than 0 then it will take precedence over the value in 
#' top_tpa argument.
#' 
#' @param sorted
#' Logical variable used to indicate if values in dbh are already in descending
#' order.
#' 
#' @return
#' Top height value.
#' @export
################################################################################

top_ht = function(dbh,
                  expf,
                  ht,
                  top_tpa = 40,
                  top_per = 0,
                  sorted = FALSE)
{
  #Initialize top_ht_
  top_ht_ <- 0
  
  #Validate top_tpa
  if(is.null(top_tpa) || top_tpa < 0) top_tpa <- 40
  
  #Validate top_per. Default to 20 if value is invalid.
  if (top_per < 0 || top_per > 100) {
    top_per <- 20
  }
  
  #Calculate TPA for the entire stand
  tpa_ <- sum(expf, na.rm = TRUE)
  
  #Do calculations if tpa > 0
  if(tpa_ > 0)
  {
    #Determine target top TPA. 
    if (top_per > 0) {
      top <- tpa_ * (top_per / 100)
    } else {
      top <- top_tpa
    }
    
    # Constrain target top TPA to not exceed total stand TPA
    if(top > tpa_) top <- tpa_
    
    if(!sorted)
    {
      #Get order of DBH values in descending order
      dbh_order <- order(-dbh)
      
      #Sort vectors
      expf <- expf[dbh_order]
      ht <- ht[dbh_order]
    }
    
    #Find the index where top is exceeded
    top_exceed <- match(TRUE, cumsum(expf) >= top)
    
    #Fallback if the threshold is never reached (include all trees)
    if (is.na(top_exceed)) {
      top_exceed <- length(expf)
    }
    
    #Sum expf up to this index
    tpa_sum <- sum(expf[1:top_exceed], na.rm = TRUE)
    
    #Calculate tpa_dif and adjust tpa_sum
    tpa_dif <- tpa_sum - top
    tpa_sum <- tpa_sum - tpa_dif
    
    #Handle situations where ht_sum may only include 1 tree
    if (top_exceed > 1) {
      ht_sum <- sum((ht * expf)[1:(top_exceed - 1)], na.rm = TRUE)
    } else {
      ht_sum <- 0
    }
    
    #Top height
    remainder_expf <- expf[top_exceed] - tpa_dif
    ht_sum <- ht_sum + (ht[top_exceed] * remainder_expf)
    
    if(tpa_sum > 0) top_ht_ <- ht_sum / tpa_sum
  }
  
  return(top_ht_)
}

################################################################################
#' mean_attr
#' @name mean_attr
#' @description
#' This function is used to calculate the arithmetic or weighted mean (average) 
#' of an attribute. The weighted mean will only be calculated if weights are 
#' provided as an input argument. These mean values can be calculated within 
#' custom size ranges and for select species.
#' 
#' @param attr
#' Numeric vector containing numeric attribute
#
#' @param weight
#' Optional numeric vector containing a weighting value. This could be an 
#' expansion factor, tree basal area, or other user defined weight. If this 
#' argument is left as NULL, then the arithmetic average will be returned..
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Average or weighted average of attribute.
#' @export
################################################################################

mean_attr = function(attr,
                     weight = NULL,
                     dbh = NULL,
                     ht = NULL,
                     species = NULL,
                     dbhmin = 0,
                     dbhmax = 999,
                     htmin = 0,
                     htmax = 999,
                     select_species = NULL)
{
  # Get number of records
  ntree <- length(attr)
  
  # Assign weight of 1 if NULL or length differs from attr
  if (is.null(weight) || length(weight) != ntree) {
    weight <- rep(1.0, ntree)
  }
  
  # Include all records unless a filter is provided
  include <- TRUE
  
  # Apply DBH filter if provided
  if (!is.null(dbh)) {
    include <- include & (dbh >= dbhmin & dbh < dbhmax)
  }
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Calculate mean
  mean_attr_ <- weighted.mean(
    x = attr[include],
    w = weight[include],
    na.rm = TRUE
  )
  
  # Capture bad values
  if (is.na(mean_attr_)) mean_attr_ <- 0
  
  return(mean_attr_)
}

################################################################################
#' expand_attr
#' @name expand_attr
#' @description
#' This function sums and expands an input numeric attribute to a per unit area 
#' basis using numeric vectors containing diameter, attribute of interest, and 
#' expansion factors. The numeric attribute could be a tree-level volume, 
#' biomass, carbon, etc.This attribute can be calculated for user defined size 
#' ranges and for select species.
#' 
#' @param attr
#' Numeric vector containing numeric attribute
#' 
#' @param expf
#' Numeric vector containing expansion factors.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#'
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This 
#' value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Sum of attribute expanded to a per unit area
#' @export
################################################################################

#'@export
expand_attr = function(attr,
                       expf,
                       dbh = NULL,
                       ht = NULL,
                       species = NULL,
                       dbhmin = 0,
                       dbhmax = 999,
                       htmin = 0,
                       htmax = 999,
                       select_species = NULL)
{
  
  # Include all records unless a filter is provided
  include <- TRUE
  
  # Apply DBH filter if provided
  if (!is.null(dbh)) {
    include <- include & (dbh >= dbhmin & dbh < dbhmax)
  }
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Expand attr over DBH, HT, and species
  expand_attr_ <- sum((attr * expf)[include], na.rm = TRUE)
  
  # If expand_attr_ is NaN or NA, set to 0
  if (is.na(expand_attr_)) expand_attr_ <- 0
  
  return(expand_attr_)
}

################################################################################
#' min_attr
#' @name min_attr
#' @description
#' This function determines the minimum value for an input attribute. This can 
#' be calculated for custom size ranges and for select species.
#' 
#' @param attr
#' Numeric vector containing numeric attribute
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This 
#' value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Minimum value of attribute.
#' @export
################################################################################

min_attr = function(attr,
                    dbh = NULL,
                    ht = NULL,
                    species = NULL,
                    dbhmin = 0,
                    dbhmax = 999,
                    htmin = 0,
                    htmax = 999,
                    select_species = NULL)
{
  
  min_attr_ <- 0
  
  # Include records with nonmissing attribute values
  include <- !is.na(attr)
  
  # Apply DBH filter if provided
  if (!is.null(dbh)) {
    include <- include & (dbh >= dbhmin & dbh < dbhmax)
  }
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Find minimum over DBH, HT, and species
  if (any(include, na.rm = TRUE)) {
    min_attr_ <- min(attr[include], na.rm = TRUE)
  }
  
  # Capture bad values
  if (is.na(min_attr_)) min_attr_ <- 0
  
  return(min_attr_)
}

################################################################################
#' max_attr
#' @name max_attr
#' @description
#' This function determines the maximum value for an input attribute. This can 
#' be calculated for custom size ranges and for select species.
#' 
#' @param attr
#' Numeric vector containing numeric attribute
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#' 
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This 
#' value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#'
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Maximum value of attribute.
#' @export
################################################################################

max_attr = function(attr,
                    dbh = NULL,
                    ht = NULL,
                    species = NULL,
                    dbhmin = 0,
                    dbhmax = 999,
                    htmin = 0,
                    htmax = 999,
                    select_species = NULL)
{
  
  max_attr_ <- 0
  
  # Include records with nonmissing attribute values
  include <- !is.na(attr)
  
  # Apply DBH filter if provided
  if (!is.null(dbh)) {
    include <- include & (dbh >= dbhmin & dbh < dbhmax)
  }
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Find maximum over DBH, HT, and species
  if (any(include, na.rm = TRUE)) {
    max_attr_ <- max(attr[include], na.rm = TRUE)
  }
  
  # Capture bad values
  if (is.na(max_attr_)) max_attr_ <- 0
  
  return(max_attr_)
}

################################################################################
#' count_rec
#' @name count_rec
#' @description
#' This function counts the number of tree records between specified DBH and HT 
#' ranges and for select species.
#' 
#' @param dbh
#' Numeric vector containing diameter values.
#' 
#' @param ht
#' Optional numeric vector containing total tree height values.
#' 
#' @param species
#' Optional vector containing species codes. Will be used when select_species is
#' not NULL.
#'
#' @param dbhmin
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#' 
#' @param dbhmax
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This 
#' value is exclusive (<).
#' 
#' @param htmin
#' Numeric value corresponding to lower tree height bound to calculate attribute 
#' in. This value is inclusive (>=).
#' 
#' @param htmax
#' Numeric value corresponding to upper tree height bound to calculate attribute 
#' in. This value is exclusive (<).
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to 
#' select which species get included in calculation of attribute. If left as 
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric count of records.
#' @export
################################################################################

count_rec = function(dbh,
                     ht = NULL,
                     species = NULL,
                     dbhmin = 0,
                     dbhmax = 999,
                     htmin = 0,
                     htmax = 999,
                     select_species = NULL)
{
  # Identify records to include by DBH
  include <- dbh >= dbhmin & dbh < dbhmax
  
  # Apply height filter if provided
  if (!is.null(ht)) {
    include <- include & (ht >= htmin & ht < htmax)
  }
  
  # Apply species filter if provided
  if (!is.null(select_species) && !is.null(species)) {
    include <- include & (species %in% select_species)
  }
  
  # Count records over DBH, HT, and species
  count_ <- sum(include, na.rm = TRUE)
  
  return(count_)
}
