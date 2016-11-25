## Sets LE table to 'Standard Life Expectancy'
## Update 'LE' and '.LE'

setStdLE <-
function(table = NULL) {
  if (is.null(table)) {
    table <- DALYtclvalue("stdLEtab")
  }

  table <- match.arg(table, c("GBD2010", "GBD1990", "WHO/GHE"))

  if (table == "GBD1990") {
    stdM <- DALYget("stdM")
    stdF <- DALYget("stdF")

  } else if (table == "GBD2010") {
    stdM <- stdF <- DALYget("stdGBD")

  } else if (table == "WHO/GHE") {
    stdM <- stdF <- DALYget("stdWHO")
  }

  for (i in seq(21)) {
    DALYassign("LE", stdM[i], i, 1)
    DALYassign("LE", stdF[i], i, 2)
  }
  DALYupdate(".LE")
  DALYupdate("stdLEtab", table)
}
