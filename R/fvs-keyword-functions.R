################################################################################
#' fvs_keyfile
#' @name fvs_keyfile
#' @description This function creates a keyword file based on user defined set 
#' of keywords for a set of stands IDs and inventory years.
#' 
#' @param keyfile
#' Character string corresponding to directory path and file name of keyword file 
#' being created.
#' 
#' @param standid
#' Character vector of stand identification numbers.
#' 
#' @param invyear
#' Optional numeric vector of inventory years for each stand in standid and or 
#' standcn arguments.
#' 
#' @param standcn
#' Optional character vector of stand control numbers. If values are not defined 
#' for this argument, then the value from standid will be used as the stand 
#' control number in the output keyword file. Defaults to NULL.
#' 
#' @param keywords
#' Optional character vector containing properly formatted FVS keywords or 
#' variables to define with event monitor function call that will be included in 
#' keyword file for each stand. Defaults to NULL.
#' 
#' @param stand_keys
#' Optional character vector containing a set of properly formatted keywords or 
#' variables to define with event monitor function call for specific stands in 
#' the standid argument. These keywords will be added for the given stand in the 
#' output keyword file. If this argument is used, it should match the length of 
#' the standid argument. Defaults to NULL.
#' 
#' @param start_year
#' Common start year for simulation. If start_year and end_year are not NULL, then 
#' function will attempt to add time keyword sequence (see time_keys function). 
#' Defaults to NULL.
#' 
#' @param end_year
#' Common end year for simulation. If start_year and end_year are not NULL, then 
#' function will attempt to add TIMEIMT and NUMCYCLE keyword sequence (see 
#' time_keys function). Defaults to NULL.
#' 
#' @param cycle_length
#' Default cycle length assumed for simulation. Only used when start_year and 
#' end_year are not NULL. Defaults to 10.
#' 
#' @param cycle_at
#' Numeric vector containing additional reporting years (those that do not occur 
#' on default cycle boundaries based on start_year, end_year and cycle_length 
#' arguments). Only used when start_year and end_year are not NULL. Defaults to NULL.
#' 
#' @param dbin
#' Character string for name of FVS input database that includes file extension 
#' (.db, .sqlite). If this value is not NULL, the function will attempt to add a 
#' DSNIN keyword sequence for reading stand and tree data. Defaults to NULL. 
#' 
#' Note: This argument should not be used if DATASCRN keyword is included in 
#' keywords or stand_keys arguments.
#' 
#' @param standinit
#' Character string corresponding to name table where stand information will be 
#' read from. Typically this would be "FVS_standinit" of "FVS_PlotInit" but could 
#' be "FVS_standinit_Plot", "FVS_standinit_Cond", etc. when FIA data is in use. 
#' This is an optional argument and will only be used if dbin is not NULL. 
#' Defaults to "FVS_StandInit".
#' 
#' @param treeinit
#' Character string corresponding to name table where tree information will be 
#' read from. Typically this would be "FVS_treeinit" but could be 
#' "FVS_treeinit_Plot", "FVS_treeinit_Cond", etc. when FIA data is in use. This 
#' is an optional argument and will only be used if dbin is not NULL. 
#' Defaults to "FVS_TreeInit".
#' 
#' @param stand_type
#' Integer value that determines what stand ID will be used to read data from 
#' dbin when dbin is not NULL.
#' 
#' 0 = Stand_CN
#' 
#' 1 = Stand_ID
#' 
#' Defaults to 1.
#' 
#' @param dbout
#' Name of FVS output database that includes file extension (.db or .sqlite). 
#' If this value is not NULL, the function will attempt to add a DSNOUT keyword 
#' sequence. Defaults to NULL.
#' 
#' @param delotab
#' Optional vector of numeric values that contain any values from 1-4. If values 
#' are provided in this argument, then DELOTAB keywords will be included in 
#' keyword file to suppress creation of text outputs. By default this argument is 
#' set to c(1, 2) so the stand composition table and sample tree data tables will 
#' be suppressed in the output keyword file. Defaults to c(1, 2).
#' 
#' @param create_batch
#' Logical value where if TRUE, a batch of keywords is created instead of a 
#' single keyword file. The number of stands included in a keyword file within a 
#' batch is determined by the nstands argument. The name of the keyword 
#' file in a batch will be keyword file name appended to _BATCHX.key where X is 
#' the number corresponding to the keyword batch (e.g. _BATCH2 would correspond 
#' to keyword file containing the second batch of stands). Defaults to FALSE.
#' 
#' @param unique_dbout
#' Logical value where if TRUE, a unique output database will be specified for 
#' the keyword file within the batch. This argument will only have an effect when 
#' value in dbout argument is not NULL and create_batch is TRUE. Defaults to FALSE.
#' 
#' @param nstands
#' Numeric value corresponding to number of stands to include in a keyword file 
#' when create_batch argument is TRUE. This value will be ignored if create_batch 
#' is FALSE. Defaults to 10000.
#' 
#' @param nbatches
#' Numeric value corresponding to number of batches to create when create_batch 
#' argument is TRUE. The value in this argument will override the value in 
#' nstands when it is not NULL. This value will be ignored if create_batch is 
#' FALSE. Defaults to NULL.
#' 
#' @return
#' None
#' @export
################################################################################

fvs_keyfile <- function(keyfile,
                        standid,
                        invyear = NULL,
                        standcn = NULL,
                        keywords = NULL,
                        stand_keys = NULL,
                        start_year = NULL,
                        end_year = NULL,
                        cycle_length = 10,
                        cycle_at = NULL,
                        dbin = NULL,
                        standinit = "FVS_StandInit",
                        treeinit = "FVS_TreeInit",
                        stand_type = 1,
                        dbout = NULL,
                        delotab = c(1, 2),
                        create_batch = FALSE,
                        unique_dbout = FALSE,
                        nstands = 10000,
                        nbatches = NULL)
{
  
  #Switch \\\\ to / in keyfile
  keyfile <- chartr("\\", "/", keyfile)
  
  #Extract path, filename, and extension
  keydir <- dirname(keyfile)
  key_name <- basename(keyfile)
  key_ext <- tools::file_ext(keyfile)
  
  #Validate output directory
  if (keydir == ".") {
    keydir <- getwd()
  } 
  else if (!file.exists(keydir)) {
    stop(paste("Path to output:", keydir, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }
  
  #Validate .key extension
  if(key_ext != "key")
    stop(paste("Output argument does not have a valid file extension. File",
               "extension must be .key."))
  
  # Validate standid
  if(length(standid) <= 0)
    stop(paste("Values are required for standid."))
  
  #Fill in defaults when NULL
  n <- length(standid)
  if(is.null(invyear)) invyear <- rep(-1, times = n)
  if(is.null(standcn)) standcn <- standid
  if(is.null(stand_keys)) stand_keys <- rep("", times = n)
  
  #Validate keywords
  if(!is.null(keywords))
  {
    #Stop if keywords is a list or not a character vector
    if(is.list(keywords) || !is.character(keywords))
      stop("keywords argument must be a character vector.")
  }
  
  #Validate vector lengths and types
  if(unequal_vector(standid, standcn, invyear, stand_keys))
    stop("standid, standcn, invyear, and stand_keys must all be the same length.")
  
  if(!is.numeric(invyear))
    stop("invyear argument must be a numeric vector.")
  
  if(!is.character(stand_keys))
    stop("stand_keys argument must be a character vector.")
  
  #Capture bad nstands values
  if(is.na(nstands) || nstands <= 0) nstands <- 10000
  
  #Process nbatches if not null.
  if(!is.null(nbatches))
  {
    #Set nbatches = 1 if value is less than or equal to 0 or NA.
    if(is.na(nbatches) || nbatches <= 0) nbatches <- 1
    nstands <- ceiling(max(length(standid) / nbatches, 1))
  }
  
  #Create list of vectors containing number of stands to process in each batch.
  #If create_batch is FALSE, only one batch and keyword file is made.
  if(!create_batch) nstands <- length(standid)
  batch_list <-  split(1:length(standid), 
                       ceiling(seq_along(1:length(standid)) / nstands))
  
  #Initialize stand_count
  stand_count <- 1
  
  #Create keyword strings that never change across stands
  #DSNIN key string
  dsnin_str <- if (!is.null(dbin)) {
    paste(dsnin_keys(dbin = dbin, stand_type = stand_type,
                     standinit = standinit, treeinit = treeinit),
          collapse = "\n\n")
  } else NULL
  
  #DELOTAB string
  delo_str <- if (length(delotab) > 0) {
    paste(delotab_keys(delotab = delotab), collapse = "\n\n")
  } else NULL
  
  #keywords string
  keywords_str <- if (length(keywords) > 0) {
    paste(keywords, collapse = "\n\n")
  } else NULL
  
  #INVYEAR and timing keyword lookups
  u_inv <- unique(invyear)
  inv_map <- match(invyear, u_inv)
  
  #INVYEAR string
  u_inv_str <- lapply(u_inv, function(yr) {
    if (!is.na(yr) && yr >= 0) {
      paste(fvs_keyword(params = list("INVYEAR", yr), type = 1), collapse = "\n\n")
    } else NULL
  })
  
  #Timing keyword string
  u_time_str <- if (!is.null(start_year) && !is.null(end_year)) {
    lapply(u_inv, function(yr) {
      paste(time_keys(invyear = yr,
                      start_year = start_year,
                      end_year = end_year,
                      cycle_length = cycle_length,
                      cycle_at = cycle_at), collapse = "\n\n")
    })
  } else vector("list", length(u_inv))
  
  #Start loop across batch list
  for(i in 1:length(batch_list))
  {
    #Get stands in batch
    stands <- batch_list[[i]]
    
    #Create keyword file path and create unique output database if needed
    if(!create_batch)
      key_path <- file.path(keydir, key_name)
    
    else
    {
      key_name_ <- gsub(pattern = ".key", 
                        replacement = paste0("_BATCH", i, ".key"),
                        x = key_name)
      
      key_path <- file.path(keydir, key_name_)
    }
    
    # Open file connection
    con <- file(description = key_path, open = "wt")
    
    # Create dsnout_str once per batch
    dsnout_str <- if (!is.null(dbout)) {
      dbout_ <- dbout
      if (create_batch && unique_dbout) {
        db_ext <- tools::file_ext(dbout_)
        dbout_ <- gsub(pattern = paste0(".", db_ext),
                       replacement = paste0("_BATCH", i, paste0(".", db_ext)),
                       x = dbout_)
      }
      paste(dsnout_keys(dbout = dbout_), collapse = "\n\n")
    } else NULL
    
    # Start loop across stands
    tryCatch({
      for (stand in stands) {
        stand_id <- standid[[stand_count]]
        stand_cn <- standcn[[stand_count]]
        s_key    <- stand_keys[[stand_count]]
        idx      <- inv_map[[stand_count]]
        
        stand_block <- c(
          paste("STDIDENT", stand_id, "STANDCN", stand_cn, sep = "\n"),
          u_inv_str[[idx]],
          dsnout_str,
          dsnin_str,
          u_time_str[[idx]],
          delo_str,
          keywords_str,
          if (!is.na(s_key) && s_key != "") s_key,
          "PROCESS"
        )
        
        writeLines(text = stand_block, con = con, sep = "\n\n")
        stand_count <- stand_count + 1L
        #End of stand loop
      }
      
      # Add STOP keyword before closing connection
      writeLines(text = "STOP", con = con, sep = "\n\n")
    }, finally = {
      if (isOpen(con)) close(con = con)
    })
    #End of batch loop
  }
  invisible()
}

################################################################################
#' fvs_kcpfile
#' @name fvs_kcpfile
#' @description This function creates a kcp file based on user defined set of 
#' #keywords.
#' 
#' @param kcpfile
#' Character string corresponding to file path of keyword component file (.kcp) 
#' being created.
#' 
#' @param keywords
#' Optional character vector containing properly formatted FVS keywords or 
#' variables to define with event monitor function call that will be included in 
#' kcp file for each stand. Defaults to NULL.
#' 
#' @return
#' None
#' @export
################################################################################

fvs_kcpfile <- function(kcpfile,
                        keywords = NULL)
{
  
  # Switch \ to / in kcpfile
  kcpfile <- chartr("\\", "/", kcpfile)
  
  #Extract path and extension
  keydir <- dirname(kcpfile)
  key_ext <- tools::file_ext(kcpfile)
  
  # Validate output directory
  if (keydir == ".") {
    keydir <- getwd()
  } else if (!file.exists(keydir)) {
    stop(paste("Path to output:", keydir, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }

  #Validate .kcp extension
  if(!key_ext %in% c("kcp"))
  {
    stop(paste("Output argument does not have a valid file extension. File",
               "extension must be .kcp."))
  }
  
  # Validate keywords argument if not NULL
  if(!is.null(keywords))
  {
    #Stop if keywords is a list or not a character vector
    if(is.list(keywords) || !is.character(keywords))
      stop("keywords argument must be a character vector.")
  }
  
  #Open file connection
  con <- file(description = kcpfile, open = "wt")
  on.exit(try(if(isOpen(con)) close(con = con), silent = TRUE))
    
  #===========================================================================
  #Write any keywords provided in keywords argument
  #===========================================================================
    
  if(length(keywords) > 0)
    writeLines(text = keywords, con = con, sep = "\n\n")
  
  close(con = con)
  invisible()
}

################################################################################
#' fvs_keyword
#' @name fvs_keyword
#' @description 
#' Formats one or more sets of parameters into FVS keyword records. Supports 
#' standard fixed-width keywords, Parms format keywords, Event Monitor 
#' functions, and freeform text records.
#' 
#' @param params
#' A list of parameters for a single FVS keyword, or a list of lists when 
#' formatting multiple keywords at once.
#' 
#' 1: List corresponds to standard FVS keyword (not in parms format). Value in 
#' index 1 should be name of FVS keyword. All following indices should contain 
#' keyword parameters.
#' 
#' 2: List corresponds to FVS keyword in parms format. Value in index 1 should be 
#' name of FVS keyword. All following indices should contain keyword parameters.
#' 
#' 3: List corresponds to variable defined by event monitor function. Value in 
#' index 1, should be name of event monitor variable to define. Index 2 should be 
#' name of event monitor function being used. All following indices should 
#' contain event monitor function arguments.
#' 
#' 4: List corresponds to a collection of values that will be returned as single 
#' string. All indices in list contain values that will be concatenated into a 
#' string where each value is separated by a space and returned without any 
#' additional formatting.
#' 
#' @param type
#' An integer (or integer vector) specifying the keyword formatting rule (1,
#' 2, 3, or 4). Must be length 1 or the same length as params when
#' passing multiple parameter sets. Invalid values default to 1.
#' 
#' 1 = Standard
#' 
#' 2 = Parms format
#' 
#' 3 = event monitor function call
#' 
#' 4 = freeform, could be standard keyword, parms formatted keyword, or event 
#' monitor function call.
#' 
#' @return
#' A character vector of formatted FVS keyword strings (length 1 for a single
#' parameter set, or matching the length of params for a list of lists).
#' Returns "" if params is empty.
#' 
#' @export
################################################################################

fvs_keyword <- function(params = list(),
                        type = 1)
{
  # Return empty string if params is empty
  if (length(params) <= 0) return("")
  
  # Wrap a single parameter set in a list so all inputs are processed uniformly
  if (!is.list(params) || !any(vapply(params, is.list, logical(1)))) {
    params <- list(params)
  }
  
  # Reject lists nested more than two levels deep
  if (any(vapply(unlist(params, recursive = FALSE), is.list, logical(1)))) {
    stop("params cannot be nested more than two levels deep.")
  }
  
  n <- length(params)
  
  # Validate type values up front (default invalid values to 1)
  if (length(type) <= 0) type <- 1
  type[!type %in% c(1, 2, 3, 4)] <- 1
  
  # Stop if multiple types are supplied and don't match the number of parameter sets
  if (length(type) > 1 && length(type) != n) {
    stop("type must be length 1 or the same length as params.")
  }
  
  # Recycle a single type across all parameter sets
  if (length(type) == 1L) {
    type <- rep.int(type, n)
  }
  
  # Format each parameter set into a character vector
  keywords <- vapply(seq_len(n), function(i) {
    
    keyword <- ""
    p <- params[[i]]
    t <- type[i]
    
    # If no values are in p, return empty keyword
    if (length(p) <= 0) return(keyword)
    
    # Drop NAs and convert to character vector
    flat_params <- unlist(p)
    char_params <- as.character(flat_params[!is.na(flat_params)])
    
    # Enforce length limit if type isn't 4
    if (t != 4) {
      char_params <- substring(char_params, 1, 10)
    }
    
    # Clean up whitespace
    char_params <- trimws(char_params)
    
    # If length of char_params is still 0, return
    if (length(char_params) <= 0) return(keyword)
    
    # Handle standard keyword formatting
    if (t == 1) {
      keyword <- paste0(sprintf("%-10s", char_params),
                        collapse = "")
    }
    
    # Handle Parms keyword formatting
    else if (t == 2 && length(char_params) >= 3) {
      parms <- paste0("Parms",
                      "(",
                      paste0(char_params[3:length(char_params)],
                             collapse = ","),
                      ")")
      
      keyword <- paste0(sprintf("%-10s", char_params[1]),
                        sprintf("%-10s", char_params[2]),
                        parms)
    }
    
    # Handle event monitor function
    else if (t == 3 && length(char_params) >= 3) {
      event_var <- paste0(char_params[1], "=", char_params[2])
      
      function_args <- paste0("(",
                              paste0(char_params[3:length(char_params)],
                                     collapse = ","),
                              ")")
      
      keyword <- paste0(event_var, function_args)
    }
    
    # Handle freeform option
    else if (t == 4) {
      keyword <- paste(char_params, collapse = " ")
    }
    
    return(keyword)
    
  }, character(1))
  
  return(keywords)
}


################################################################################
#' time_keys
#' @name time_keys
#' @description This function creates a set of keywords for controlling the 
#' length of a FVS simulation for a given inventory year, common start year, 
#' common end year, default cycle length, and optional cycle_at keywords.
#' 
#' @param invyear
#' Inventory year of stand. Defaults to NULL.
#' 
#' @param start_year
#' Common start year for simulation. Defaults to NULL.
#' 
#' @param end_year
#' End year for simulation. Defaults to NULL.
#' 
#' @param cycle_length
#' Default cycle length assumed for simulation. Defaults to NULL.
#' 
#' @param cycle_at
#' Vector of years corresponding to additional reporting years that don't 
#' coincide with default reporting years determined by start_year, end_year, and 
#' cycle_length. Defaults to NULL.
#' 
#' @param max_cycles
#' Numeric value corresponding to the maximum number of cycles that can be 
#' accommodated in FVS simulation. Defaults to 40.
#' 
#' @param verbose
#' Logical variable where if TRUE, debug output will be printed in console. 
#' Defaults to FALSE.
#' 
#' @return
#' Character string of keywords for controlling the length of a FVS simulation 
#' for a given inventory year, common start year, common end year, default cycle 
#' length, and additional reporting years.
#' @export
################################################################################

time_keys <- function(invyear = NULL,
                      start_year = NULL,
                      end_year = NULL,
                      cycle_length = NULL,
                      cycle_at = NULL,
                      max_cycles = 40,
                      verbose = F)
{
  time_keys_ <- ""
  
  #If any required inputs are NULL or NA, return
  if(null_vector(invyear, start_year, end_year, cycle_length) ||
     na_vector(invyear, start_year, end_year, cycle_length)) return(time_keys_)
  
  #If start_year is greater than or equal to end_year, then set end_year to 
  #start_year + 1 default cycle length
  if(start_year >= end_year) end_year <- start_year + cycle_length
  
  #If invyear is greater than start_year, reset it to start_year
  if(invyear > start_year) invyear <- start_year
  
  #Remove any factional part of cycle_length
  cycle_length <- floor(cycle_length)

  #Catch bad max cycles values
  if(is.na(max_cycles) || max_cycles <= 0) max_cycles <- 40
  
  #Obtain years from start year to end year of simulation and cycle at values
  to_end <- sort(unique(c(seq(from = start_year, to = end_year, by = cycle_length),
                   end_year,
                   cycle_at)))
  
  #Filter years to ensure they sit strictly within our valid simulation bounds
  to_end <- to_end[to_end >= invyear & to_end <= end_year]
  
  #Now get get years from inventory year to first value in to_end
  years <- sort(unique(c(
    seq(from = invyear, to = to_end[1], by = cycle_length),
    to_end)))
  
  #Do debug if requested.
  if(verbose)
  {
    cat("Inventory year:", invyear, "\n")
    cat("Start year:", start_year, "\n")
    cat("End year:", end_year, "\n")
    cat("Default cycle length:", cycle_length, "\n")
    cat("Years to consider:", "\n")
    for(year in years)
      cat(year, "\n")
  }
  
  #Create initial TIMEINT for all cycles with a common cycle length.
  timeint_all <- paste(fvs_keyword(params = list("TIMEINT",
                                                "0", 
                                                as.character(cycle_length))))
  
  #Create add_timeint and cycle_num counter
  add_timeint <- character(max_cycles)
  cycle_num <- 0
  
  #Generate difference between years vector
  year_difs <- diff(years)
  
  #Loop across years and create additional TIMEINT keywords for cycles whose 
  #length in years differs from the default cycle length.
  for(i in seq_along(year_difs))
  {
    #Get dif
    dif <- year_difs[i]
    
    #If max_cycles cycles has been exceeded break out of loop
    if(cycle_num >= max_cycles) break
    else cycle_num <- cycle_num + 1
    
    #If dif != cycle_length, then add another timeInt keyword to add_timeint
    if(dif != cycle_length)
    {
      add_timeint[cycle_num] <- paste(fvs_keyword(params = list("TIMEINT",
                                      as.character(cycle_num), 
                                      as.character(dif))))
    }
  }
  
  #Drop empty entries from add_timeint
  add_timeint <- add_timeint[add_timeint != ""]

  #Combine keywords
  time_keys_ <- paste(
    c(timeint_all,
      add_timeint,
      fvs_keyword(params = list("NUMCYCLE", 
                                cycle_num))),
    collapse = "\n")
  
  return(time_keys_)
}

################################################################################
#' dsnin_keys
#' @name dsnin_keys
#' @description This function creates a set of keywords for reading in data from
#' an input FVS database.
#' 
#' @param dbin
#' Name of FVS input database that includes file extension (.db or .sqlite). 
#' Defaults to "FVS_Data.db".
#' 
#' @param stand_type
#' Integer value that determine what stand ID will be used to read data from.
#' 
#' 0 = Stand_CN
#' 
#' 1 = Stand_ID
#' 
#' Defaults to 1.
#' 
#' @param standinit
#' Character string corresponding to name table where stand information will be 
#' read from. Typically this would be "FVS_standinit" or "FVS_PlotInit" but could 
#' be "FVS_standinit_Plot", "FVS_standinit_Cond", etc. 
#' Defaults to "FVS_StandInit".
#' 
#' @param treeinit
#' Character string corresponding to name table where tree information will be 
#' read from. Typically this would be "FVS_treeinit" but could be 
#' "FVS_treeinit_Plot", "FVS_treeinit_Cond", etc. 
#' Defaults to "FVS_TreeInit".
#' 
#' @return
#' Character string of keywords with SQL instructions to read data in from 
#' FVS_standinit and FVS_treeinit tables.
#' @export
################################################################################

dsnin_keys <- function(dbin = 'FVS_Data.db',
                       stand_type = 1,
                       standinit = "FVS_StandInit",
                       treeinit = "FVS_TreeInit")
{

  #Catch bad stand_type values
  if(!stand_type %in% c(0, 1)) stand_type <- 1
     
  #If standinit contains "stand", assume we are dealing with a StandInit table
  if(grepl(pattern = "stand", x = standinit, ignore.case = TRUE))
  {
    if(stand_type == 1) stand_read <- "WHERE Stand_ID = '%StandID%'"
    else stand_read <- "WHERE Stand_CN = '%Stand_CN%'"
  }
  
  #Otherwise assume a PlotInit table
  else 
  {
    if(stand_type == 1) stand_read <- "WHERE StandPlot_ID = '%StandID%'"
    else stand_read <- "WHERE StandPlot_CN = '%Stand_CN%'"
  }
  
  #Build the db input keywords
  dsnin_keys_ <- paste("DATABASE", 
                       "DSNIN",
                       dbin, 
                       "STANDSQL",
                       "SELECT *",
                       paste("FROM", standinit),
                       stand_read,
                       "ENDSQL",
                       "TREESQL", 
                       "SELECT *",
                       paste("FROM", treeinit),
                       stand_read,
                      "ENDSQL",
                      "END", 
                      sep = "\n")
  
  return(dsnin_keys_)
}

################################################################################
#' dsnout_keys
#' @name dsnout_keys
#' @description This function creates a set of keywords for setting an FVS 
#' output database.
#' 
#' @param dbout
#' Name of FVS output database that includes file extension (.db or .sqlite). 
#' Defaults to "FVSOut.db".
#' 
#' @return
#' Character string containing keywords for setting an FVS output database.
#' @export
################################################################################

dsnout_keys <- function(dbout = 'FVSOut.db')
  
{
  #Build the db output keywords
  dsnout_keys <- paste("DATABASE", 
                       "DSNOUT",
                       dbout, 
                       "END", 
                       sep = "\n")
  
  return(dsnout_keys)
}

################################################################################
#' delotab_keys
#' @name delotab_keys
#' @description This function creates a set of delotab keywords based on input 
#' numeric vector.
#' 
#' @param delotab
#' Vector of integer values that contain any values from 1-4. Defaults to NULL.
#' 
#' @return
#' Character string containing set of DELOTAB keywords or empty string.
#' @export
################################################################################

#'@export
delotab_keys <- function(delotab = NULL)
{
  delokeys <- ""
  
  #Create DELOTAB keys
  if(length(delotab) > 0)
  {
    delotab <- ifelse(delotab < 1 | delotab > 4, 1, delotab)
    delokeys <- vapply(seq_along(delotab), function(i) {
        delokey <- fvs_keyword(list("DELOTAB", delotab[i]), type = 1)
    }, character(1))
  }
  
  #Concatenate DELOTAB keys
  keys <- paste(delokeys,
                collapse = "\n")
  
  return(keys)
}

################################################################################
#' open_keys
#' @name open_keys
#' @description This function creates a set of keywords for opening and closing
#' an FVS addfile or kcp file.
#' 
#' @param file
#' Vector of character strings corresponding to a file path to a addfile (.kcp) 
#' file.
#' 
#' @param ref_num
#' Vector of integer file reference numbers. Values should be greater than 50
#' (this is based on default value from FVS-GUI).
#' 
#' @param blanks
#' Integer value pertaining to how blank values are treated in numeric fields 
#' from contents of file that is opened (OPEN keyword).
#' 
#' 0 = treat blanks as zeros (default)
#' 
#' gt 0 = treat blanks as null characters
#' 
#' @param status
#' Integer value corresponding to file status code (OPEN keyword).
#' 
#' 0 = Unknown (default, file may or may not currently exist)
#' 
#' 1 = New (file does not currently exist)
#' 
#' 2 = Old (file currently exists)
#' 
#' @param record_length
#' Integer value corresponding to maximum record length in the file (number of 
#' characters per line; OPEN keyword). Default value is 80 characters.
#' 
#' @return
#' Character string containing keywords for opening and closing an addfile (.kcp) 
#' file.
#' @export
################################################################################

open_keys <- function(file = "FVS_Addfile.kcp",
                      ref_num = 50,
                      blanks = 0,
                      status = 0,
                      record_length = 80)
  
{
  #Validate ref_num
  ref_num <- ifelse(is.na(ref_num) | ref_num < 50, 50, ref_num)
  
  #Validate blanks, status and record_length
  blanks <- blanks[1L]
  if (is.na(blanks) || blanks < 0) blanks <- 0
  
  status <- status[1L]
  if (is.na(status) || !status %in% c(0, 1, 2)) status <- 0
  
  record_length <- record_length[1L]
  if (is.na(record_length) || record_length < 80 || record_length > 150) {
    record_length <- 80
  }
  
  #Build the keyword sequence
  open_key <- mapply(
    function(f, r) {
      paste(
        fvs_keyword(params = list("OPEN", r, blanks, status, record_length, 0)),
        f,
        fvs_keyword(params = list("ADDFILE", r)),
        fvs_keyword(params = list("CLOSE", r)),
        sep = "\n"
      )
    },
    f = file,
    r = ref_num,
    USE.NAMES = FALSE)
  
  return(open_key)
}

################################################################################
#' valid_vectors
#' 
#' @name valid_vectors
#' 
#' @description
#' This function takes a set of vectors and checks if any are NULL, have 
#' unequal lengths, or contain NA values.
#' 
#' @param ... 
#' A set of vectors to be evaluated.
#' 
#' @return 
#' A logical value: `FALSE` if any vector is NULL, has unequal lengths, or 
#' contains NA values; `TRUE` otherwise.
#' 
#' @noRd
################################################################################

valid_vectors <- function(...) {
  
  return(!(null_vector(...) || unequal_vector(...) || na_vector(...)))
}

################################################################################
#' null_vector
#' @name null_vector
#' 
#' @description
#' This function takes a set of vectors and checks if any are NULL.
#' 
#' @param ... 
#' A set of vectors to be evaluated.
#' 
#' @return 
#' A logical value: `TRUE` if any vector is NULL, `FALSE` otherwise.
#' 
#' @noRd
################################################################################

null_vector = function(...)
{
  #Determine value of null vector
  null_vector_ <- any(sapply(list(...), is.null, USE.NAMES = FALSE))
  
  return(null_vector_)
}

################################################################################
#' unequal_vector
#' @name unequal_vector
#' @description
#' 
#' This function takes in a set of vectors and checks if they are the same 
#' length. If any of the vectors are not the same length then a TRUE value is
#' returned. This a helper function that is used inside many of functions in 
#' this file. 
#
#' @param ...
#' The ... should be a set of vectors that will be checked if any are of unequal
#' length.
#
#' @return
#' Logical TRUE or FALSE value.
#' 
#' @noRd
################################################################################

unequal_vector <- function(...)
{
  #Initialize return value
  unequal_vector_ <- FALSE
  
  #Store the vectors in a list
  vectors <- list(...)
  
  #Check if any vectors are not equal to length of first vector
  if(length(vectors) > 0)
  {
    vec_lengths <- lengths(vectors)
    unequal_vector_ <- any(vec_lengths != vec_lengths[1])
  }
  
  return(unequal_vector_)
}

################################################################################
#' na_vector
#' @name na_vector
#' 
#' @description
#' This function takes a set of vectors and checks if any contain NA values.
#' 
#' @param ... 
#' A set of vectors to be evaluated.
#' 
#' @return 
#' A logical value: `TRUE` if any vector contains an NA, `FALSE` otherwise.
#' 
#' @noRd
################################################################################

na_vector = function(...)
{
  # Determine if any vector contains an NA value
  na_vector_ <- any(sapply(list(...), 
                           function(x) any(is.na(x)),
                           USE.NAMES = FALSE))
  
  return(na_vector_)
}
