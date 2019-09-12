#' Metalimnion finder
#'
#' Determines the upper and lower bounds of the metalimnion in a water column from a temperature profile of a stratified lake.
#' This function was borrowed heavily from the rLakeAnalyzer package on CRAN.
#'
#' @param temps Vector of water temperature in degrees Celsius
#' @param depths Vector of depths corresponding to the temps values
#' @param slope
#' @param seasonal Logical value indicating if seasonal thermocline should be returned.
#' The seasonal thermocline is defined as the deepest density gradient found in the profile.
#' @param mixed_cutoff Cutoff point where the thermocline is not calculated; defaults to 1 degree C
#' @keywords thermocline
#' @export
#' @examples
#'
#' depths <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
#' temps <- c(25, 25, 25, 20, 15, 12, 10, 8, 8, 8)
#'
#' metalimnion_finder(temps, depths)


metalimnion_finder <- function(temps, depths, slope = 0.1, seasonal = TRUE, mixed_cutoff = 1){

  if(any(is.na(temps))){
    return(rep(NaN, 2))
  }

  # We can't determine anything with less than 3 measurements
  # just return lake bottom

  if(length(temps) < 3){
    return(c(max(depths), max(depths)))
  }

  depths <-  sort.int(depths, index.return = TRUE)
  temps <-  temps[depths$ix]
  depths <- depths$x

  thermoD <- thermo_finder(temps, depths, seasonal = seasonal, mixed_cutoff = mixed_cutoff)

  # if no thermo depth, then there can be no meta depths
  if(is.na(thermoD)){
    return(c(NaN, NaN))
  }

  #We need water density, not temperature to do this
  rhoVar <- water_density(temps)

  dRhoPerc <- 0.15; #in percentage max for unique thermocline step
  numDepths <- length(depths)
  drho_dz <- vector(mode="double", length = numDepths-1)

  #Calculate the first derivative of density
  for(i in 1:(numDepths-1)){
    drho_dz[i] <- (rhoVar[i+1] - rhoVar[i]) / (depths[i+1] - depths[i])
  }

  #initiate metalimnion bottom as last depth, this is returned if we can't
  # find a bottom
  metaBot_depth <- depths[numDepths]
  metaTop_depth <- depths[1]
  Tdepth <- rep(NaN, numDepths-1)

  for(i in 1:(numDepths-1)){
    Tdepth[i] <- mean(depths[ i:(i+1) ]);
  }

  tmp <- sort.int(unique(c(Tdepth, thermoD)), index.return = TRUE)
  sortDepth <- tmp$x
  sortInd <- tmp$ix
  numDepths <- length(sortDepth) #set numDepths again, it could have changed above
  drho_dz <- stats::approx(Tdepth, drho_dz, sortDepth)
  drho_dz <- drho_dz$y

  #find the thermocline index
  # this is where we will start our search for meta depths
  thermo_index <- which(sortDepth == thermoD)

  for (i in thermo_index:numDepths){ # moving down from thermocline index
    if (!is.na(drho_dz[i]) && drho_dz[i] < slope){ #top of metalimnion
      metaBot_depth <- sortDepth[i];
      break
    }
  }

  if (i-thermo_index >= 1 && (!is.na(drho_dz[thermo_index]) && drho_dz[thermo_index] > slope)){
    metaBot_depth <- stats::approx(drho_dz[thermo_index:i],
                                   sortDepth[thermo_index:i],slope)
    metaBot_depth <- metaBot_depth$y
  }

  if(is.na(metaBot_depth)){
    metaBot_depth <- depths[numDepths]
  }

  for(i in seq(thermo_index,1)){
    if(!is.na(drho_dz[i]) && drho_dz[i] < slope){
      metaTop_depth <- sortDepth[i];
      break;
    }
  }

  if(thermo_index - i >= 1 && (!is.na(drho_dz[thermo_index]) && drho_dz[thermo_index] > slope)){
    metaTop_depth <- stats::approx(drho_dz[i:thermo_index], sortDepth[i:thermo_index], slope);
    metaTop_depth <- metaTop_depth$y
  }

  if(is.na(metaTop_depth)){
    metaTop_depth <- depths[i]
  }

  return(c(metaTop_depth, metaBot_depth))
}
