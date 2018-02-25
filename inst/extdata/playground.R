library(roes)

# load eeperimental data
oes_folder <- "~/Job/experiments/OES/OES_2018-02-14_Ti_DC/scan/"
oes_folder <- "~/Job/experiments/OES/OES_2018-02-12_TiO2_RF_R/scan/"
oes_folder <- "~/Job/experiments/OES/OES_2017-11-20_Ti/scan/"
oes_lst <- read_file_2_list(oes_folder = oes_folder, read_function = read_ocean_tab, allFiles = TRUE, filePattern = "OES_")

# plot experimental data
oes_mat <- oes_lst_2_mat(oes_lst = oes_dat, plot = TRUE)
plot_oes_section(oes_lst = oes_lst)

# read NIST data
nist_db_content()
nist <- read_nist_oes_multi(c("AR_I", "TI_I", "N_I", "O_I"))

#usporedba expeprimentalnih podata i NIST-a
oes_plot_compare(nist = nist, oes_lst = oes_lst)


