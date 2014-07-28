#' Function to generate a number of sequence segments of a particular length from an input sequence
#' @param seq Sequence to draw segments from. Can be a character vector or \code{\link{DNAString}} object of length 1
#' @param len Length of the segments to be generated
#' @param num Number of segment to generate
#' @return A \code{\link{DNAStringSet}} object containing the generated sequence segmenta
#' @export
gen_segs <- function(seq, len = 90, num = 1000) {
  seqlen <- nchar(seq)
  ## choose random start and stop positions for segments within the original sequence
  pos_seq <- runif(num, 0, seqlen - len) %>% data.frame %>% set_names("lower") %>% mutate(upper=lower + len - 1)
  ## pull out new sequences using positions
  newseqs <- pos_seq %>% rowwise %>% summarise(Sequence = substr(seq, start=lower, stop=upper)) 
  return(DNAStringSet(newseqs$Sequence))
}