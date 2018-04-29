
#' Read Ocean optics spectra from file
#' @author K. Juraic
#' @description Read ocean optics spectra from tab delimited file with header
#' @param file_name File name for stored ocean optics spectra
#' @return list(name, data, scan_time, int_time, spectra_avg, boxcar, n_pixels)
#' @examples
#'     \dontrun{read_ocean_tab("ocean_file.txt")}
#' @export
read_ocean_tab <- function(file_name) {
  #citanje spektra iz Ocean datoteke
  datoteka <- file_name

  #ime spektra
  name <- basename(file_name)
  name <- strsplit(name, "\\.")[[1]][1]

  # scan time
  podaci <- scan(datoteka, list(""), skip = 2, nlines = 1, quiet = TRUE)[[1]]
  podaci <- paste(podaci[7], podaci[3], podaci[4], podaci[5])
  scan_time <- strptime(podaci, "%Y %b %d  %H:%M:%S")
  # za prebaciti natrax u neki string format koristiti funkciju strftime()

  #integration time
  podaci <- scan(datoteka, list(""), skip = 8, nlines = 1, quiet = TRUE)[[1]]
  int_time <- as.numeric(podaci[4])

  #spectra average
  podaci <- scan(datoteka, list(""), skip = 9, nlines = 1, quiet = TRUE)[[1]]
  spectra_avg <- as.numeric(podaci[3])

  #Boxcar smoothing
  podaci <- scan(datoteka, list(""), skip = 10, nlines = 1, quiet = TRUE)[[1]]
  boxcar <- as.numeric(podaci[3])
  #boxcar

  #number of pixels in file
  podaci <- scan(datoteka, list(""), skip = 15, nlines = 1, quiet = TRUE)[[1]]
  n_pixels <- as.integer(podaci[7])

  #citanje spektra
  col_names <- c("lambda", "I")
  data <- utils::read.table(datoteka,
                     skip = 17,
                     nrows = n_pixels,
                     row.names = NULL,
                     col.names = col_names)

  #objekt koji sadrzi sve podatke procitane iz OCEAN datoteke
  ocean_spektar <- list(name = name,
                       data = data,
                       scan_time = scan_time,
                       int_time = int_time,
                       spectraAvg = spectra_avg,
                       boxcar = boxcar,
                       nPixels = n_pixels)

  cat("Read from file:", file_name, "\n")

  ocean_spektar

}


#' Read data from multiple files to list
#' @author K. Juraic
#' @description Read data from multiple files to list by using specified function.
#'              If the folder name is not specified GUI dialog will be opened to
#'              select folder path. Can be read all files in folder or selected by
#'              dialog.
#' @param oes_folder Folder path from where to read
#' @param read_function Functio tpo be used for single file read
#' @param all_files TRUE all files satisfieing filePattern in folder will be read
#' @param file_pattern File pattern
#' @return list of read data
#' @examples \dontrun{dat = read_file_2_list()}
#' @export
read_file_2_list <- function(oes_folder =
                               tcltk::tk_choose.dir(caption =
                                                      "Select OES folder:"),
                             read_function = read_ocean_tab,
                             all_files = FALSE,
                             file_pattern = "*.*") {
  wd <- getwd()
  setwd(oes_folder)
  if (all_files == TRUE) {
    file_lst <- list.files(path = oes_folder, pattern = file_pattern)
  } else {
    file_filter <- matrix(c("Ocena Optics Tab", ".txt"), nrow = 1, ncol = 2)
    file_lst <- tcltk::tk_choose.files(caption = "Select spectra files:",
                                       filters = file_filter,
                                       multi = TRUE)
  }
  data <- plyr::alply(.data = file_lst, .margins = 1, .fun = read_function)
  cat("Total number of files read =", length(file_lst))
  setwd(wd)
  return(data)
}


#' Convert OES list to matrix
#' @author K. Juraic
#' @param oes_lst list of oes data
#' @param plot TRUE plot matrix
#' @examples
#'        \dontrun{oes_lst_2_mat(oes_lst)}
#' @export
oes_lst_2_mat <- function(oes_lst, plot = TRUE) {
  n_sp <- length(oes_lst)
  n_point <- nrow(oes_lst[[1]]$data)
  oes_mat <- matrix(0, nrow = n_sp, ncol = n_point)
  for (i in 1:n_sp) {
    oes_mat[i, ] <- oes_lst[[i]]$data[, 2]
  }
  if (plot == TRUE) {
    graphics::image(x = 1:n_sp, y = oes_lst[[1]]$data[, 1], z = log(oes_mat),
          col = grDevices::rainbow(255),
          xlab = "Spectra No.",
          ylab = "Wavelength / nm")
  }
  return(oes_mat)
}


#' OES time scale from list
#' @author  Juraic
#' @description Extract time scale from OES data list
#' @param oes_lst List with OES data
#' @return OES time scale in array
#' @examples
#'      \dontrun{oes_lst_2_time(oes_lst)}
#' @export
oes_lst_2_time <- function(oes_lst){
  oes_time <- array()
  for (i in 1:length(oes_lst))
    oes_time[i] <- strftime(oes_lst[[i]]$scan_time)
  return(oes_time)
}


#' Calculate peak area for set of ocena optics spectras
#' @author K. Juraic
#' @param oes_mat matrix with data
#' @param x.min range min for peak area calculation
#' @param x.max range max for peak area calculation
#' @param sub_min TRUE subtract const background
#' @return peak-area array
#' @examples
#'         \dontrun{oes_peak_area(oes_mat, xmin, xmax)}
#' @export
oes_peak_area <- function(oes_mat, x.min, x.max, sub_min = TRUE){
  mat_crop <- oes_mat[, x.min:x.max]
  mat_crop
  bg <- apply(X = mat_crop, MARGIN = 1, FUN = min)
  if (sub_min)
    peak_area <- rowMeans(mat_crop - bg, na.rm = TRUE)
  else
    peak_area <- rowMeans(mat_crop, na.rm = TRUE)
  return(peak_area)
}

#' plot oes data section
#'
#' @author K. Juraic
#' @param oes_lst oes list
#' @export
plot_oes_section <- function(oes_lst) {
  x.min <- 0
  x.max <- 0
  wavelength_id <- 1
  wavelength_id_2 <- 1
  frame <- 1
  oes_mat <- oes_lst_2_mat(oes_lst)
  wavelength <- oes_lst[[1]]$data[, 1]
  manipulate::manipulate({
      graphics::par(mfrow = c(1, 2))
      graphics::plot(wavelength, oes_mat[frame, ],
           xlim = sort(c(wavelength[x.min], wavelength[x.max])),
           type = "l", ceh = .5, pch = 16)
      graphics::abline(v = wavelength[wavelength_id], col = 2)
      graphics::abline(v = wavelength[wavelength_id_2], col = 3)
      graphics::abline(v = wavelength[x.min], col = 4)
      graphics::abline(v = wavelength[x.max], col = 4)
      graphics::plot(1:dim(oes_mat)[1],
                     oes_mat[, wavelength_id] / max(oes_mat[, wavelength_id],
                                                  na.rm = TRUE),
                     type = "l", col = 2, ylim = c(.6, 1))
      graphics::points(1:dim(oes_mat)[1], oes_mat[, wavelength_id_2] /
                         max(oes_mat[, wavelength_id_2],
                             na.rm = TRUE), type = "l", col = 3)
      graphics::points(1:dim(oes_mat)[1],
                       oes_mat[, wavelength_id] / oes_mat[, wavelength_id_2] /
                         max(oes_mat[, wavelength_id] /
                               oes_mat[, wavelength_id_2]),
                       type = "l", col = 4, ceh = .5, pch = 16)
    },
    frame = manipulate::slider(min = 1,
                               max = dim(oes_mat)[1],
                               initial = 1),
    wavelength_id = manipulate::slider(min = 1,
                                       max = dim(oes_mat)[2],
                                       initial = 1),
    wavelength_id_2 = manipulate::slider(min = 1,
                                         max = dim(oes_mat)[2],
                                         initial = 1),
    x.min = manipulate::slider(1, dim(oes_mat)[2]),
    x.max = manipulate::slider(1, dim(oes_mat)[2], initial = dim(oes_mat)[2])
  )
}
