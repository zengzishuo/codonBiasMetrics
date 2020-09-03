
generate_a_sequence = function(n_codon){
  if(n_codon <3){
    stop('number of codons must be greater than or equal to 3')}
  middle = sample(codons_except_stop, n_codon-2, replace = T)
  start_middle_end = c('ATG', middle, sample(c('TAG','TAA','TGA'), 1))
  res = paste(start_middle_end, collapse = '')
  return(res)
}
