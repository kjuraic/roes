

#' Read optical spectral lines data from file for single element
#'
#' @author K. Juraic
#' @param file_name spectral data file name
#' @return data.frame with optical spectral line data
#' @example \dontrun{read_nist_oes('inst/extdata/Database/AR_I.txt')}
#' @export
read_nist_oes <- function(file_name) {
  if(file.exists(file_name)){
    pos <- c(1,10,19,30,41,52,63,74,76,90,1/01,112,123,134,145,173,185,201,211,240)
    col_names <- stringi::stri_trim_both(strsplit(x = readLines(file_name, n = 2)[2], split = "|", fixed = TRUE)[[1]])
    col_names <- stringi::stri_replace(col_names, fixed = " ", "_")
    col_names[1] <- 'Element'
    col_names <- stringi::stri_trans_tolower(col_names)
    #col_names <- stri_replace_all(str = col_names, '_', fixed=' ')
    #dat <- read.fwf(file = file, widths = diff(pos),skip = 5)
    #names(dat) <- col_names
    dat = suppressWarnings(readr::read_fwf(file = file_name, readr::fwf_widths(diff(pos), col_names), skip = 5,
                                    col_types = readr::cols(wavelength = 'd', rel. = 'd', aki = 'd',  fik = 'd', s = 'd',
                                                     log_gf = 'd', ei = 'd', ek = 'd', ji = 'd', jk = 'd', gi = 'd', gk = 'd')))
    cat("[",file_name, "] Read OK!\n")
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
#' @export
read_nist_oes_multi <- function(element_list, db_path = 'extdata/Database/'){
  fnms <- paste0(db_path,element_list,'.txt')
  df <- plyr::adply(.data = fnms, .margins = 1, .fun = read_nist_oes)
  df
}


#' list content of NIST OES database
#'
#' @author K. Juraic
#' @return list of files in NIST database folder
#' @examples \dontrun{nist_db_content()}
#' @export
nist_db_content <- function(){
  tmp = list.files("inst/extdata/Database/")
  tmp = stringi::stri_replace(tmp, "", fixed = ".txt")
  return(tmp)
}
