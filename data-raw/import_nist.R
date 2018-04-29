

#' Read optical spectral lines data from file for single element
#'
#' @author K. Juraic
#' @param file_name spectral data file name
#' @return data.frame with optical spectral line data
#' @example \dontrun{read_nist_oes(oes_name)}
read_nist_oes <- function(file_name) {
  if (file.exists(file_name)){
    pos <- c(1, 10, 19, 30, 41, 52, 63, 74, 76, 90, 101, 112, 123, 134, 145,
             173, 185, 201, 211, 240)
    col_names <- stringi::stri_trim_both(strsplit(x = readLines(file_name,
                                                                n = 2)[2],
                                                  split = "|",
                                                  fixed = TRUE)[[1]])
    col_names <- stringi::stri_replace(col_names, fixed = " ", "_")
    col_names[1] <- "Element"
    col_names <- stringi::stri_trans_tolower(col_names)
    dat <- suppressWarnings(readr::read_fwf(file = file_name,
                                            readr::fwf_widths(diff(pos),
                                                              col_names),
                                            skip = 5,
                                    col_types = readr::cols(wavelength = "d",
                                                            rel. = "d",
                                                            aki = "d",
                                                            fik = "d",
                                                            s = "d",
                                                            log_gf = "d",
                                                            ei = "d",
                                                            ek = "d",
                                                            ji = "d",
                                                            jk = "d",
                                                            gi = "d",
                                                            gk = "d")))
    cat("[", file_name, "] Read OK!\n")
  } else {
    dat <- data.frame()
    cat("[", file_name, "] File not exist!\n")
  }
  as.data.frame(dat)
}


#' read multiple oes file
#'
#' @author K. Juraic
#' @param element_list  list of elements to read data
#' @param db_path path to directory with oes files
#' @return data.frame with spectral data
#' @examples
#'       \dontrun{read_nist_oes_multi(c("AR_I","O_I"))}
read_nist_oes_multi <- function(element_list, db_path = "extdata/Database/"){
  fnms <- paste0(db_path, element_list, ".txt")
  df <- plyr::adply(.data = fnms, .margins = 1, .fun = read_nist_oes)
  df
}



#' Read optical spectral lines data from file for single molecule
#'
#' @author K. Juraic
#' @param file_name spectral data file name
#' @return data.frame with optical spectral line data
#' @example \dontrun{read_nist_oes_molecule(oes_name)}
read_nist_oes_molecule <- function(file_name) {
  if (file.exists(file_name)){
    pos <- c(1, 12, 23, 34, 45, 56, 67, 78, 100)
    col_names <- stringi::stri_trim_both(strsplit(x = readLines(file_name,
                                                                n = 2)[2],
                                                  split = "|",
                                                  fixed = TRUE)[[1]])
    col_names <- stringi::stri_replace(col_names, fixed = "'", "c", mode = "all")
    #col_names[1] <- "Element"
    col_names <- stringi::stri_trans_tolower(col_names)
    dat <- suppressWarnings(readr::read_fwf(file = file_name,
                                            readr::fwf_widths(diff(pos),
                                                              col_names),
                                            skip = 4,
                                            col_types = readr::cols(wavelength = "d",
                                                                    frequency = "d",
                                                                    intensity = "d",
                                                                    vc = "d",
                                                                    vcc = "d",
                                                                    s = "d",
                                                                    ec = "d",
                                                                    note = "c")))
    cat("[", file_name, "] Read OK!\n")
  } else {
    dat <- data.frame()
    cat("[", file_name, "] File not exist!\n")
  }
  as.data.frame(dat)
}


#' read multiple oes molecule file
#'
#' @author K. Juraic
#' @param molecule_list  list of molecule to read data
#' @param db_path path to directory with oes files
#' @return data.frame with spectral data
#' @examples
#'       \dontrun{read_nist_oes_molecule_multi(c("AR_I","O_I"))}
read_nist_oes_molecule_multi <- function(molecule_list, db_path = "~/Desktop//Database/"){
  fnms <- paste0(db_path, molecule_list, ".txt")
  df <- plyr::adply(.data = fnms, .margins = 1, .fun = read_nist_oes_molecule)
  df
}



#' list content of NIST OES database
#'
#' @author K. Juraic
#' @return list of files in NIST database folder
#' @examples \dontrun{nist_db_content()}
nist_db_content <- function(){
  tmp <- list.files("~/Desktop/Database/")
  tmp <- stringi::stri_replace(tmp, "", fixed = ".txt")
  return(tmp)
}


#nm <- nist_db_content()
#nm_atom <- nm[-(73:88)]
#db_atoms <- read_nist_oes_multi(element_list = nm_atom, db_path = "~/Desktop/Database/")
#save(db_atoms, file = "~/Job/R/Rpackages/roes/data/db_atoms.RData")

#nm <- nist_db_content()
#nm_molecules <- nm[73:88]
#nm_molecules
#setwd("~/Desktop/Database/")
#db_molecules <- read_nist_oes_molecule_multi(molecule_list = nm_molecules)
#save(db_molecules, file = "~/Job/R/Rpackages/roes/data/db_molecules.RData")
