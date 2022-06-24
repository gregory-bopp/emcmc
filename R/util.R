zero_pad_strd <- function(x, width = 10){
  stringr::str_pad(formatC(x, format = 'd'), width = width, pad = '0')
}
