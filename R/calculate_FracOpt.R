
calculate_FracOpt = function(input_sequence){
  codon_counts = count_codon(input_sequence)
  res = sum(codon_counts[index_for_opt_codon])/sum(codon_counts)
  return(res)
}
