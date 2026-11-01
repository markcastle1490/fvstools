################################################################################
#' fvs_standint
#'
#' @description
#' Constructs an empty or pre-allocated data frame template initialized with the
#' variable names and column classes required for an FVS Stand Initialization 
#' (`STANDINIT`) table.
#'
#' @param nstand:
#' An integer specifying the number of rows to include in dataframe. Defaults to
#' 0 to build an empty schema blueprint.
#'
#' @return
#' A data frame containing 67 columns structurally typed for FVS processing 
#' with a row count matching nstand.
#'
#' @export
################################################################################

fvs_standinit <- function(nstand = 0) {
  
  df <- data.frame(
    STAND_CN           = rep(NA_character_, nstand),
    STANDPLOT_CN       = rep(NA_character_, nstand),
    STAND_ID           = rep(NA_character_, nstand),
    STANDPLOT_ID       = rep(NA_character_, nstand),
    VARIANT            = rep(NA_character_, nstand),
    INV_YEAR           = rep(NA_integer_, nstand),
    GROUPS             = rep(NA_character_, nstand),
    ADDFILES           = rep(NA_character_, nstand),
    FVSKEYWORDS        = rep(NA_character_, nstand),
    LATITUDE           = rep(NA_real_, nstand),
    LONGITUDE          = rep(NA_real_, nstand),
    REGION             = rep(NA_integer_, nstand),
    FOREST             = rep(NA_integer_, nstand),
    DISTRICT           = rep(NA_integer_, nstand),
    COMPARTMENT        = rep(NA_integer_, nstand),
    LOCATION           = rep(NA_integer_, nstand),
    ECOREGION          = rep(NA_character_, nstand),
    PV_CODE            = rep(NA_character_, nstand),
    PV_REF_CODE        = rep(NA_integer_, nstand),
    AGE                = rep(NA_integer_, nstand),   
    ASPECT             = rep(NA_real_, nstand),
    SLOPE              = rep(NA_real_, nstand),
    ELEVATION          = rep(NA_real_, nstand),
    ELEVFT             = rep(NA_real_, nstand),
    BASAL_AREA_FACTOR  = rep(NA_real_, nstand),
    INV_PLOT_SIZE      = rep(NA_real_, nstand),
    BRK_DBH            = rep(NA_real_, nstand),
    NUM_PLOTS          = rep(NA_integer_, nstand),   
    NONSTK_PLOTS       = rep(NA_integer_, nstand), 
    SAM_WT             = rep(NA_real_, nstand),
    STK_PCNT           = rep(NA_real_, nstand),
    DG_TRANS           = rep(NA_integer_, nstand), 
    DG_MEASURE         = rep(NA_real_, nstand),
    HTG_TRANS          = rep(NA_integer_, nstand),
    HTG_MEASURE        = rep(NA_real_, nstand),
    MORT_MEASURE       = rep(NA_real_, nstand), 
    MAX_BA             = rep(NA_real_, nstand),
    MAX_SDI            = rep(NA_real_, nstand),
    SITE_SPECIES       = rep(NA_character_, nstand),
    SITE_INDEX         = rep(NA_integer_, nstand),
    MODEL_TYPE         = rep(NA_integer_, nstand),
    PHYSIO_REGION      = rep(NA_integer_, nstand),
    FOREST_TYPE        = rep(NA_integer_, nstand),
    STATE              = rep(NA_integer_, nstand),
    COUNTY             = rep(NA_integer_, nstand),
    FUEL_MODEL         = rep(NA_integer_, nstand),
    FUEL_0_25_H        = rep(NA_real_, nstand),
    FUEL_25_1_H        = rep(NA_real_, nstand),
    FUEL_1_3_H         = rep(NA_real_, nstand),
    FUEL_3_6_H         = rep(NA_real_, nstand),
    FUEL_6_12_H        = rep(NA_real_, nstand),
    FUEL_12_20_H       = rep(NA_real_, nstand),
    FUEL_20_35_H       = rep(NA_real_, nstand),
    FUEL_35_50_H       = rep(NA_real_, nstand),
    FUEL_GT_50_H       = rep(NA_real_, nstand),
    FUEL_0_25_S        = rep(NA_real_, nstand),
    FUEL_25_1_S        = rep(NA_real_, nstand),
    FUEL_1_3_S         = rep(NA_real_, nstand),
    FUEL_3_6_S         = rep(NA_real_, nstand),
    FUEL_6_12_S        = rep(NA_real_, nstand),
    FUEL_12_20_S       = rep(NA_real_, nstand),
    FUEL_20_35_S       = rep(NA_real_, nstand),
    FUEL_35_50_S       = rep(NA_real_, nstand),
    FUEL_GT_50_S       = rep(NA_real_, nstand),
    FUEL_LITTER        = rep(NA_real_, nstand),
    FUEL_DUFF          = rep(NA_real_, nstand),
    PHOTO_REF          = rep(NA_integer_, nstand),
    PHOTO_CODE         = rep(NA_character_, nstand),
    STDORGCD           = rep(NA_integer_, nstand),
    stringsAsFactors   = FALSE
  )
  
  return(df)
}

################################################################################
#' fvs_treeinit
#'
#' @description
#' Constructs an empty or pre-allocated data frame template initialized with the
#' variable names and column classes required for an FVS Tree Initialization 
#' (`TREEINIT`) table.
#'
#' @param ntree:
#' An integer specifying the number of rows to pre-allocate. Defaults to 0 to 
#' build an empty schema blueprint.
#'
#' @return 
#' A data frame containing 30 columns structurally typed for FVS tree data
#' processing with a row count matching ntree.
#'
#' @export
################################################################################

fvs_treeinit <- function(ntree = 0) {
  
  df <- data.frame(
    STAND_CN         = rep(NA_character_, ntree),
    STAND_ID         = rep(NA_character_, ntree),
    STANDPLOT_CN     = rep(NA_character_, ntree),
    STANDPLOT_ID     = rep(NA_character_, ntree),
    PLOT_ID          = rep(NA_real_, ntree),
    TREE_ID          = rep(NA_character_, ntree),
    TREE_COUNT       = rep(NA_real_, ntree),
    HISTORY          = rep(NA_integer_, ntree),
    SPECIES          = rep(NA_character_, ntree),
    DBH              = rep(NA_real_, ntree),
    DG               = rep(NA_real_, ntree),
    HT               = rep(NA_real_, ntree),
    HTG              = rep(NA_real_, ntree),
    HTTOPK           = rep(NA_real_, ntree),
    CRRATIO          = rep(NA_real_, ntree),
    DAMAGE1          = rep(NA_real_, ntree),
    SEVERITY1        = rep(NA_real_, ntree),
    DAMAGE2          = rep(NA_real_, ntree),
    SEVERITY2        = rep(NA_real_, ntree),
    DAMAGE3          = rep(NA_real_, ntree),
    SEVERITY3        = rep(NA_real_, ntree),
    TREEVALUE        = rep(NA_real_, ntree),
    PRESCRIPTION     = rep(NA_real_, ntree),
    AGE              = rep(NA_real_, ntree),
    SLOPE            = rep(NA_integer_, ntree),
    ASPECT           = rep(NA_integer_, ntree),
    PV_CODE          = rep(NA_character_, ntree),
    TOPOCODE         = rep(NA_real_, ntree),
    SITEPREP         = rep(NA_real_, ntree),
    CULL             = rep(NA_real_, ntree),
    DECAYCD          = rep(NA_integer_, ntree),
    WDLND_STEMS      = rep(NA_integer_, ntree),
    stringsAsFactors = FALSE
  )
  
  return(df)
}
