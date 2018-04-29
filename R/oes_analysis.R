

#' Compare experimental OES data to NIST literature
#'
#' @author K. Juraic
#' @param nist data.frame with NIST data(imported with read_nist_oes_multi)
#' @param oes_lst list with experimental data (imported with read_file_2_list())
#' @return NA
#' @examples \dontrun{oes_plot_compare(nist, oes_lst)}
#' @export
oes_plot_compare <- function(nist, oes_lst){
  w_min <- 200
  w_max <- 1000
  fac <- 1
  fac_I <- 1
  w_shift <- 0
  id <- 1
  lambda <- array()
  wavelength <- array()
  element <- factor()
  rel <- array()
  nist$rel <- log10(nist$rel)
  nist$rel <- nist$rel / max(nist$rel, na.rm = TRUE)
  manipulate::manipulate({
    ggplot2::ggplot(data = nist) +
      ggplot2::geom_segment(ggplot2::aes(x = wavelength, xend = wavelength,
                                         y = 0, yend = rel * fac,
                                         color = element)) +
      ggplot2::theme_bw() + ggplot2::ylim(0, 1) +
      ggplot2::xlim(w_min, w_max) +
      ggplot2::geom_line(data = oes_lst[[id]]$data,
                         ggplot2::aes(x = lambda + w_shift,
                                      y = I / max(I) * fac_I))
    },
    w_min = manipulate::slider(min = 200, max = 1000, initial = 200),
    w_max = manipulate::slider(min = 200, max = 1000, initial = 1000),
    fac = manipulate::slider(min = 0, max = 1, initial = .5),
    fac_I = manipulate::slider(min = 0, max = 100, initial = 1),
    w_shift = manipulate::slider(min = -1, max = 1, step = .001, initial = 0),
    id = manipulate::slider(min = 1, max = length(oes_lst),
                            initial = 1, step = 1)
  )
}
