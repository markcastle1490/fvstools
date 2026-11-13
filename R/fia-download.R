################################################################################
#' download_fia
#' @name download_fia
#' @description This function is used to download one of the following:
#'
#' 1) State specific SQLite FIA database(s)
#'
#' 2) All .csv files for state(s).
#'
#' 3) FIA Reference tables ZIP archive.
#' 
#' 4) Specific .csv files for state. AK_TREE.csv, AK_PLOT.csv, etc.
#'
#' The downloaded files are extracted and stored on a specified local directory.
#' 
#' @param output
#' Character string pertaining to file path to output directory where FIA 
#' databases will be stored. Defaults to NULL.
#' 
#' @param type 
#' Numeric value specifying the type of data to download.
#' 
#' 1 = FIADB SQLite database for state(s)
#'
#' 2 = All csv files for state(s)
#' 
#' 3 = FIA Reference tables ZIP archive.
#'
#' 4 = Specific .csv files for state. AK_TREE.csv, AK_PLOT.csv, etc.
#' 
#' @param states
#' Vector of two-character state abbreviations. This argument is required
#' for types 1, 2, and 4, and is ignored for type 3.
#' 
#' "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
#' "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
#' "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
#' "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
#' "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
#' "ENTIRE" (all states)
#' 
#' Defaults to NULL. This argument will be ignored when type is 3. 
#' 
#' @param tables 
#' Character vector containing valid, FIA table names (e.g. 'TREE', PLOT'). Only
#' used when type is 4.
#' 
#' @param timeout
#' Numeric value specifying the download timeout in seconds. Only the first
#' value is used. This argument is required and must be greater than zero.
#'
#' @param max_tries
#' Numeric value specifying the maximum number of download attempts. Only the
#' first value is used. This argument is required and must be at least 1.
#' 
#' @return
#' None
#' @export
################################################################################

download_fia <- function(output = NULL,
                         type = 1,
                         states = NULL,
                         tables = NULL,
                         verbose = FALSE,
                         timeout = 600,
                         max_tries = 3)
{
  #Check output
  if (is.null(output) || !dir.exists(output)) {
    stop(paste("Output directory does not exist or was not found:", output))
  }
  
  #Check for bad type values
  if(!type %in% 1:4)
    stop("Invalid type value entered.")
  
  #Check for input states options
  if (is.null(states) && type %in% c(1, 2, 4)) {
    stop("No states specified for download. Enter values in states argument.")
  }
  
  #Check for input tables options
  if (is.null(tables) && type == 4) {
    stop("Type is 4 and no tables were specified to download.")
  }
  
  #Check timeout value
  timeout <- as.numeric(timeout[1L])
  
  if (is.na(timeout) || timeout <= 0) {
    stop(
      "timeout must be a positive number.",
      call. = FALSE
    )
  }
  
  #Check max_tries
  max_tries <- as.numeric(max_tries[1])
  
  if (is.na(max_tries) || max_tries < 1) {
    stop(
      "max_tries must be a positive number.",
      call. = FALSE
    )
  }
  
  #Upper case states/tables and get unique values
  states <- unique(toupper(as.character(states)))
  tables <- unique(toupper(as.character(tables)))
  
  #Set timeout values
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)
  
  #Vector of state abbreviations
  state_abbrev <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                    "ENTIRE")
  
  #Vector of FIA tables
  fia_tables <- c("GRND_LYR_FNCTL_GRP",
                  "GRND_LYR_MICROQUAD",
                  "COND",
                  "COND_DWM_CALC",
                  "COUNTY",
                  "DWM_COARSE_WOODY_DEBRIS",
                  "DWM_DUFF_LITTER_FUEL",
                  "DWM_FINE_WOODY_DEBRIS",
                  "DWM_MICROPLOT_FUEL",
                  "DWM_RESIDUAL_PILE",
                  "DWM_TRANSECT_SEGMENT",
                  "DWM_VISIT",
                  "FVS_GROUPADDFILESANDKEYWORDS",
                  "FVS_PLOTINIT_PLOT",
                  "FVS_STANDINIT_COND",
                  "FVS_STANDINIT_PLOT",
                  "FVS_TREEINIT_COND",
                  "FVS_TREEINIT_PLOT",
                  "GRND_CVR",
                  "INVASIVE_SUBPLOT_SPP",
                  "LICHEN_LAB",
                  "LICHEN_PLOT_SUMMARY",
                  "LICHEN_SPECIES_SUMMARY",
                  "LICHEN_VISIT",
                  "OZONE_BIOSITE_SUMMARY",
                  "OZONE_PLOT",
                  "OZONE_PLOT_SUMMARY",
                  "OZONE_SPECIES_SUMMARY",
                  "OZONE_VALIDATION",
                  "OZONE_VISIT",
                  "P2VEG_SUBP_STRUCTURE",
                  "P2VEG_SUBPLOT_SPP",
                  "PLOT",
                  "PLOT_REGEN",
                  "PLOTGEOM",
                  "PLOTSNAP",
                  "POP_ESTN_UNIT",
                  "POP_EVAL",
                  "POP_EVAL_ATTRIBUTE",
                  "POP_EVAL_GRP",
                  "POP_EVAL_TYP",
                  "POP_PLOT_STRATUM_ASSGN",
                  "POP_STRATUM",
                  "PROJECT",
                  "SEEDLING",
                  "SEEDLING_REGEN",
                  "SITETREE",
                  "SOILS_EROSION",
                  "SOILS_LAB",
                  "SOILS_SAMPLE_LOC",
                  "SOILS_VISIT",
                  "SUBP_COND",
                  "SUBP_COND_CHNG_MTRX",
                  "SUBP_SOIL_SAMPLE_LAYER",
                  "SUBP_SOIL_SAMPLE_LOC",
                  "SUBPLOT",
                  "SUBPLOT_REGEN",
                  "SURVEY",
                  "TREE",
                  "TREE_GRM_BEGIN",
                  "TREE_GRM_COMPONENT",
                  "TREE_GRM_ESTN",
                  "TREE_GRM_MIDPT",
                  "TREE_GRM_THRESHOLD",
                  "TREE_WOODLAND_STEMS",
                  "VEG_PLOT_SPECIES",
                  "VEG_QUADRAT",
                  "VEG_SUBPLOT",
                  "VEG_SUBPLOT_SPP",
                  "VEG_VISIT")
  
  #Check input states
  if(type %in% c(1, 2, 4))
  {
    invalid <- setdiff(states, state_abbrev)
    if(length(invalid) > 0)
      stop(paste("The following states are not valid:",
                 paste(invalid, collapse = ", ")))
  }
  
  #Check input tables
  if(type == 4)
  {
    tables <- toupper(tables)
    invalid <- setdiff(tables, fia_tables)
    if(length(invalid) > 0)
      stop(paste("The following tables are not valid:",
                 paste(invalid, collapse = ", ")))
  }
  
  #Determine url based on type
  url <- if(type == 1) "https://apps.fs.usda.gov/fia/datamart/Databases/" else
    "https://apps.fs.usda.gov/fia/datamart/CSV/"
  
  # --------------------------------------------------------------------------
  # Build a single download plan
  # --------------------------------------------------------------------------
  
  # SQLite databases
  if (type == 1) {
    
    download_plan <- paste0(
      "SQLite_FIADB_",
      states,
      ".zip")
    
    names(download_plan) <- states
    
    # CSV ZIP archives
  } else if (type == 2) {
    
    download_plan <- paste0(
      states,
      "_CSV.zip")
    
    names(download_plan) <- states
    
    # FIA reference ZIP archive
  } else if (type == 3) {
    
    download_plan <- "FIADB_REFERENCE.zip"
    names(download_plan) <- "REF"
    
    # Individual CSV files
  } else if (type == 4) {
    
    download_plan <- paste0(
      rep(states, each = length(tables)),
      "_",
      rep(tables, times = length(states)),
      ".csv")
    
    names(download_plan) <- rep(
      states,
      each = length(tables)
    )
    
  } else {
    
    stop(
      "Invalid type passed: ",
      type,
      call. = FALSE
    )
  }
  
  # --------------------------------------------------------------------------
  # Download each file
  # --------------------------------------------------------------------------
  
  for (k in seq_along(download_plan)) {
    
    target <- names(download_plan)[k]
    file <- download_plan[[k]]
    
    filename_url <- paste0(url, file)
    filename_disk <- file.path(output, file)
    
    if (verbose) {
      cat(
        "Processing:",
        target,
        "\n",
        "Downloading file:",
        filename_url,
        "to",
        filename_disk,
        "\n")
    }
    
    # Remove a preexisting local file
    if (file.exists(filename_disk)) {
      
      if (verbose) {
        cat(
          "Removing preexisting local file:",
          filename_disk,
          "\n")
      }
      
      unlink(filename_disk, force = TRUE)
      
      if (file.exists(filename_disk)) {
        stop(
          "Unable to remove preexisting file: ",
          filename_disk,
          call. = FALSE
        )
      }
    }
    
    # Download with retries
    download_success <- FALSE
    last_error <- NULL
    download_status <- NA_integer_
    
    for (attempt in seq_len(max_tries)) {
      
      if (verbose) {
        cat(
          "Download attempt",
          attempt,
          "of",
          max_tries,
          "\n"
        )
      }
      
      download_status <- tryCatch(
        utils::download.file(
          url = filename_url,
          destfile = filename_disk,
          mode = "wb",
          method = "libcurl",
          quiet = !verbose
        ),
        error = function(e) {
          last_error <<- conditionMessage(e)
          NA_integer_
        }
      )
      
      # download.file() returns zero on success
      if (identical(as.integer(download_status), 0L) &&
          file.exists(filename_disk) &&
          isTRUE(file.info(filename_disk)$size > 0)) {
        
        download_success <- TRUE
        break
      }
      
      # Remove partial or failed downloads before retrying
      if (file.exists(filename_disk)) {
        unlink(filename_disk, force = TRUE)
      }
      
      if (attempt < max_tries) {
        wait_seconds <- min(60, 2^(attempt - 1L))
        
        if (verbose) {
          cat(
            "Download failed. Retrying in",
            wait_seconds,
            "seconds.\n"
          )
        }
        
        Sys.sleep(wait_seconds)
      }
    }
    
    if (!download_success) {
      
      if (is.null(last_error)) {
        last_error <- paste(
          "download.file() returned status",
          download_status
        )
      }
      
      stop(
        "Network transaction failed or data was not accessible: ",
        last_error,
        call. = FALSE
      )
    }
    
    if (verbose) {
      cat("Download completed successfully.\n")
    }
    
    # Extract ZIP archives for types 1 through 3
    if (type %in% 1:3) {
      
      if (verbose) {
        cat("Extracting files from:", filename_disk, "\n")
      }
      
      tryCatch(
        utils::unzip(
          zipfile = filename_disk,
          exdir = output
        ),
        error = function(e) {
          stop(
            "Unable to extract downloaded archive ",
            filename_disk,
            ": ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
      
      if (verbose) {
        cat("Files extracted from:", filename_disk, "\n")
        cat("Deleting file:", filename_disk, "\n")
      }
      
      delete_status <- unlink(filename_disk, force = TRUE)
      
      if (delete_status != 0L || file.exists(filename_disk)) {
        warning(
          filename_disk,
          " was not deleted successfully.",
          call. = FALSE
        )
      }
    }
    
    if (verbose) {
      cat("Finished processing:", file, "\n\n")
    }
  }
  
  if (verbose) {
    cat("All files processed.\n")
  }
  
  invisible(NULL)
}
