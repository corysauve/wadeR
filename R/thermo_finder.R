#' Thermocline finder
#'
#' Determines the depth of the thermocline in a water column from a temperature profile
#' This function was borrowed heavily from the rLakeAnalyzer package on CRAN.
#' @param temps Vector of water temperature in degrees Celsius
#' @param depths Vector of depths corresponding to the temps values
#' @param Smin Optional parameter defining minimum density gradient for thermocline; defaults to 0.1
#' @param seasonal Logical value indicating if seasonal thermocline should be returned.
#' The seasonal thermocline is defined as the deepest density gradient found in the profile.
#' @param index Boolean value indicating if index of thermocline depth should be returned
#' @param mixed_cutoff Cutoff point where the thermoclien is not calculated; defaults to 1 degree C
#' @keywords thermocline
#' @export
#' @examples
#'
#' depths <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
#' temps <- c(25, 25, 25, 20, 15, 12, 10, 8, 8, 8)
#'
#' thermo_finder(temps, depths)

thermo_finder <- function(temps, depths, Smin = 0.1, seasonal = TRUE, index = FALSE, mixed_cutoff = 1){

  # Missing value catcher
  if(any(is.na(temps))){
    return("Missing values")
  }

  # Indicates range is below cutoff
  if(diff(range(temps, na.rm = TRUE)) < mixed_cutoff){
    return("Temps below cutoff")
  }

  # Throws error if there are not at least 3 measurements
  if(length(temps) < 3){
    return("Depth is less than 3 measurements")
  }

  # Throws error if profile depths are not unique
  if(length(depths) != length(unique(depths))){
    stop("Depths are not unique")
  }

  rhoVar <- water_density(temps)

  dRhoPerc = 0.15; # in percentage max for unique thermocline step
  numDepths = length(depths);
  drho_dz = rep(NaN, numDepths - 1);

  # Calculating the first derivative of density
  for(i in 1:(numDepths-1)){
    drho_dz[i] = (rhoVar[i+1] - rhoVar[i]) / (depths[i+1] - depths[i]);
  }

  # Looking for two distinct max slopes; lower one is seasonal
  thermoInd = which.max(drho_dz)  #Find max slope
  mDrhoZ = drho_dz[thermoInd]
  thermoD = mean( depths[thermoInd:(thermoInd+1)] )

  if(thermoInd > 1 && thermoInd < (numDepths-1)){  # if within range
    Sdn = -(depths[thermoInd + 1] - depths[thermoInd]) /
      (drho_dz[thermoInd + 1] - drho_dz[thermoInd])

    Sup <- (depths[thermoInd] - depths[thermoInd - 1]) /
      (drho_dz[thermoInd] - drho_dz[thermoInd - 1])

    upD <-  depths[thermoInd]
    dnD <-  depths[thermoInd+1]

    if(!is.infinite(Sup) & !is.infinite(Sdn)){
      thermoD = dnD * (Sdn / (Sdn + Sup)) + upD * (Sup / (Sdn + Sup))
    }
  }

  dRhoCut = max(c(dRhoPerc * mDrhoZ, Smin))
  locs = findPeaks(drho_dz, dRhoCut)
  pks = drho_dz[locs]

  if(length(pks) == 0){
    SthermoD = thermoD
    SthermoInd = thermoInd
  } else {
    mDrhoZ = pks[length(pks)]
    SthermoInd = locs[length(pks)]

    if(SthermoInd > (thermoInd + 1)){
      SthermoD = mean(depths[SthermoInd:(SthermoInd + 1)])

      if(SthermoInd > 1 && SthermoInd < (numDepths - 1)){
        Sdn = -(depths[SthermoInd + 1] - depths[SthermoInd]) /
          (drho_dz[SthermoInd + 1] - drho_dz[SthermoInd])

        Sup = (depths[SthermoInd] - depths[SthermoInd - 1])/
          (drho_dz[SthermoInd] - drho_dz[SthermoInd - 1])

        upD  = depths[SthermoInd]
        dnD  = depths[SthermoInd + 1]

        if(!is.infinite(Sup) & !is.infinite(Sdn)){
          SthermoD = dnD * (Sdn / (Sdn + Sup)) +upD * (Sup / (Sdn + Sup))
        }
      }
    } else {
      SthermoD = thermoD
      SthermoInd = thermoInd
    }
  }

  if(SthermoD < thermoD){
    SthermoD = thermoD
    SthermoInd = thermoInd
  }

  # Determines which output was requested. Index or value
  # seasonal or non-seasonal

  if(index){
    if(seasonal){
      return(SthermoInd)
    } else {
      return(thermoInd)
    }
  } else {
    if(seasonal){
      return(SthermoD)
    } else {
      return(thermoD)
    }
  }

}

# Finds the local peaks in a vector. Checks the optionally supplied threshold
#  for minimum height.

findPeaks <- function(dataIn, thresh = 0){

  varL = length(dataIn)
  locs = rep(FALSE, varL)
  peaks= rep(NaN, varL)

  for(i in 2:varL-1){
    pkI = which.max(dataIn[(i-1):(i+1)])
    posPeak = max(dataIn[(i-1):(i+1)]);

    if(pkI == 2) {
      peaks[i] = posPeak
      locs[i]  = TRUE
    }
  }

  inds = 1:varL
  locs = inds[locs]
  peaks= peaks[locs]

  # remove all below threshold value

  useI = peaks > thresh
  locs = locs[useI]

  return(locs)
}
