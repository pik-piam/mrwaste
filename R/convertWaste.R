#' convertWaste
#'
#' @description Converts readWaste output to complete MAgPIE object containing Waste data on country level (kg/cap)
#' @param subtype type of waste data, generation composition treatment or special
#' @return Waste data as complete MAgPIE object on country level
#' @author David Chen
#' @seealso \code{\link{readSource}}
#' @export

convertWaste <- function(subtype){
  if (subtype=="Generation"){
  x<-readSource("Waste", subtype="Generation", convert=F)

  }
  else if (subtype=="Composition"){
  x<-readSource("Waste", subtype="Composition", convert=F)
  x[is.na(x)] <- 0

  x[,,"food"] <- x[,,"food"] + x[,,"yard"]
  x <- x[,,"yard", inv=T]

      }
  else if (subtype=="Treatment"){
    x<-readSource("Waste", subtype="Treatment", convert=F)
    x[is.na(x)] <- 0

    x[,,"landfill_unspecified"] <- x[,,"landfill_unspecified"] + x[,,"sanitary_landfill_landfill_gas_system"] + x[,,"controlled_landfill"]
    x[,,"open_dump"] <- x[,,"open_dump"] + x[,,"other"] + x[,,"waterways_marine"] + x[,,"unaccounted_for"]
    x[,,"compost"] <- x[,,"compost"] + x[,,"anaerobic_digestion"]
    x <- x[,,c("anaerobic_digestion","controlled_landfill", "other", "sanitary_landfill_landfill_gas_system", "unaccounted_for", "waterways_marine"),inv=T]

    x <- x[,,c("compost", "recycling", "incineration", "landfill_unspecified", "open_dump")]


    }
  else if (subtype=="Special"){
    x<-readSource("Waste", subtype="Special", convert=F)
  }
 x <- toolCountryFill(x, fill=0)

 return(x)
}
