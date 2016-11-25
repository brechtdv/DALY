## Sets LE table to 'Standard Life Expectancy'
## Update 'LE' and '.LE'

setStdLE <-
function() {
  if (DALYtclvalue("stdLEtab") == "GBD1990") {
    stdM <- DALYget("stdM")
    stdF <- DALYget("stdF")

  } else if (DALYtclvalue("stdLEtab") == "GBD2010") {
    stdM <- stdF <- DALYget("stdGBD")

  } else if (DALYtclvalue("stdLEtab") == "WHO/GHE") {
    stdM <- stdF <- DALYget("stdWHO")
  }

  for (i in seq(21)) {
    DALYassign("LE", stdM[i], i, 1)
    DALYassign("LE", stdF[i], i, 2)
  }
  DALYupdate(".LE")
}