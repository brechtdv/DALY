## (1) Startup & Exit functions
## (2) Assign & Get functions (adapted from John Fox's Rcommander)
## (3) Window handlers

.onAttach <-
function(...){
  ## Return if not interactive
  if (!interactive())
    return()
  
  ## Startup messages
  packageStartupMessage("\nWelcome to DALY Calculator 1.2.0 (2013-07-05)")
  packageStartupMessage(paste("\nType 'DALYmanual()' for help on using",
                              "the DALY Calculator"))
  packageStartupMessage(paste("Type 'DALYcalculator()' for re-initializing",
                              "the DALY Calculator\n"))

  ## Initiate Tcl/Tk widgets
  if(.Platform$OS.type == "unix"){
    addTclPath("/usr/local/lib/Tktable2.9")
	addTclPath("/usr/local/lib/Tktable2.10")  # latest version
  }
  if (as.numeric(tcl("info", "tclversion")) < 8.5)
    stop(paste("Loading the DALY Calculator requires",
               "Tcl/Tk version 8.5 or greater.\n",
               " -> Please download it from",
               "www.activestate.com/activetcl/downloads",
               sep = " "))
  if (class(tclRequire("Tktable", warn = FALSE)) == "logical"){
    stop(paste("Loading the DALY Calculator requires",
               "the 'Tktable' toolkit.\n",
               " -> Please download it from",
               "packages.ubuntu.com/search?keywords=tk-table\n",
               " -> Ubuntu/Debian users may use\n",
               "     'sudo apt-get install tk-table' or\n",
			   "     'sudo apt-get install libtktable2.9'"))
  } else {
    tclRequire("Tktable")
  }
  
  ## Launch DALY Calculator
  DALYcalculator.startup()
}

.onLoad <-
function(...){
  ## Create 'DALY' database
  if (! "DALY" %in% search())
    attach(NULL, pos = length(search()) - 1, name = "DALY")

  ## Create list of active windows
  assign("active.windows", list(), pos = "DALY")
}

.onUnload <-
function(...){
  ## Remove 'DALY' database
  detach(DALY)
}

##===========================================================================

DALYassign <-
function(x, val, row = NULL, col = NULL, item = NULL, ...){
  if (is.null(row) & is.null(col) & is.null(item)){
    assign(x, value = val, pos = "DALY", ...)
  } else if (is.null(row) & is.null(col)) {
    eval(parse(text = paste(x, "[['", item, "']] <- ", val, sep = "")),
         envir = as.environment("DALY"))
  } else {
    eval(parse(text = paste(x, "[", row, ",", col, "] <- ", val, sep = "")),
         envir = as.environment("DALY"))
  }
}

DALYget <-
function(x, row = NULL, col = NULL, ...){
  if (is.null(row) & is.null(col)){
    get(x, pos = "DALY", inherits = FALSE, ...)
  } else {
    get(x, pos = "DALY", inherits = FALSE, ...)[[row, col]]
  }
}

DALYtclvalue <-
function(x){
  ## tclVar versus tclArray
  if (!any(class(DALYget(x)) == "tclArray")){
    out <- tclvalue(DALYget(x))
    if (grepl("^[[:digit:]]*\\.?[[:digit:]]+$", out))
	  out <- as.numeric(out)
  } else {
    dim <- names(DALYget(x))[names(DALYget(x)) != "active"]
    dim <- strsplit(dim, ",")
    row <- max(c(as.numeric(unlist(lapply(dim, head, 1L))), 1))  # data win !
    col <- max(as.numeric(unlist(lapply(dim, tail, 1L))))
    out <- matrix(nrow = row, ncol = col)
    for (i in seq(row)){
      for (j in seq(col)){
        out[i, j] <- ifelse(is.null(DALYget(x, i, j)),
                            NA, as.numeric(DALYget(x, i, j)))
      }
    }
  }
  return(out)
}

DALYupdate <-  # from R:tmp --> Tcl:x
function(x, tmp = DALYget(substr(x, 2, nchar(x)))){
  row <- nrow(tmp)
  col <- ncol(tmp)

  ## tclVar versus tclArray
  if (is.null(row) & is.null(col)){
    eval(parse(text = paste("tcltk::tclvalue(", x, ") <- '",
                            tmp, "'", sep = "")),
         envir = as.environment("DALY"))
  } else {
    for (i in seq(row)){
      for (j in seq(col)){
        new <- ifelse(is.na(tmp[i, j]), "NULL", tmp[i, j])
        eval(parse(text = paste(x, "[[", i, ",", j, "]] <- ", new, sep = "")),
             envir = as.environment("DALY"))
      }
    }
  }
}

DALYsave <-  # from Tcl:x --> R:tmp
function(x){
  DALYassign(substr(x, 2, nchar(x)), DALYtclvalue(x))
}

DALYexists <-
function(x, ...){
  return(exists(x, envir = as.environment("DALY"), ...))
}

DALYeval <-
function(x, ...){
  eval(x, envir = as.environment("DALY"), ...)
}

DALYcheck <-
function(x, allow.null){
  ## tclVar versus tclArray
  if (!any(class(DALYget(x)) == "tclArray")){
    out <- is.numeric(DALYtclvalue(x))
  } else {
    dim <- names(DALYget(x))[names(DALYget(x)) != "active"]
    dim <- strsplit(dim, ",")
    row <- max(c(as.numeric(unlist(lapply(dim, head, 1L))), 1))  # data win !
    col <- max(as.numeric(unlist(lapply(dim, tail, 1L))))
    out <- matrix(nrow = row, ncol = col)
    for (i in seq(row)){
      for (j in seq(col)){
        new <- grepl("^[[:digit:]]*\\.?[[:digit:]]+$", DALYget(x, i, j))
        out[i, j] <- ifelse(length(new) == 0, allow.null, new)
      }
    }
    out <- all(out == TRUE)
  }
  return(invisible(out))
}

DALYtxt <-
function(x){
  if (x == ".it") return("Iterations")
  if (x == ".pop") return("Population table")
  if (x == ".LE") return("Life Expectancy table")
  if (grepl("inc", x)) return("Incidence table")
  if (grepl("trt", x)) return("Treatment table")
  if (grepl("ons", x)) return("Onset table")
  if (grepl("dur", x)) return("Duration table")
  if (grepl("DWt", x)) return("DW-treated table")
  if (grepl("DWn", x)) return("DW-untreated table")
  if (grepl("mrt", x)) return("Mortality table")
  if (grepl("lxp", x)) return("Average age at death table")
}

##===========================================================================

## not implemented yet...
closeWindow <-
function(win){
  tkmessageBox(message = "Exit without saving?", title = "ok",
               type = "okcancel")
}

saveWindow <-
function(win, save, check, allow.null = TRUE){
  checklist <- unlist(lapply(check, DALYcheck, allow.null))
  if (any(checklist == FALSE)){
    select <- min(which(checklist == FALSE))
	error <- ifelse(allow.null,
	                  "Non-numeric value in ",
                      "Non-numeric or NULL value in ")
    tkmessageBox(message = paste(error, DALYtxt(check[[select]]), sep = ""),
                 title = "Error", icon = "error", type = "ok")
  } else {
    lapply(save, DALYsave)
    tkdestroy(win)
  }
}

saveOptWin <-
function(save, check){
  if (!grepl("^[[:digit:]]+$", DALYtclvalue(".it"))){
    tkmessageBox(message = "'Iterations' must be a positive integer value",
                 title = "Error", icon = "error", type = "ok")
  } else if (grepl("^0+$", DALYtclvalue(".it"))){
    tkmessageBox(message = "'Iterations' must be larger than zero",
                 title = "Error", icon = "error", type = "ok")
  } else {
    saveWindow(DALYget("opt.win"), save, check)
  }
}

cancelWindow <-
function(win){
#  x <- tkmessageBox(message = "Exit without saving?", type = "okcancel")
#  if (tclvalue(x) == "ok") tkdestroy(win)
#  if (tclvalue(x) == "cancel") return(0)
  tkdestroy(win)
}

DALYfocus <-
function(win){
  tkfocus("-force", DALYget(win))
}

DALYdestroy <-
function(win){
  DALYassign("active.windows", FALSE, item = win)
  tkdestroy(DALYget(win))
}

DALYdestroy.main <-
function(){
  DALYdestroy("main")
  if (DALYexists("LE.win")) DALYdestroy("LE.win")
  if (DALYexists("pop.win")) DALYdestroy("pop.win")
  if (DALYexists("opt.win")) DALYdestroy("opt.win")
  for (i in seq(8)){
    win <- paste("data", i, sep = "")
    if (DALYexists(win)) DALYdestroy(win)	
  }
}

is.open <- function(win){
  open <- ifelse(is.null(DALYget("active.windows")[[win]]),
                 FALSE,
                 DALYget("active.windows")[[win]])
  return(open)
}