calculate_CUB = function(input_sequence, ref_a = fa_ref){
  codon_counts = count_codon(input_sequence)
  freqs64 = synonymous_usage_ratio(input_sequence)
  fa = sapply(1:20, function(x) freqs64[pointers[[x]]])
  dist_20aa = sapply(1:20, function(x) sum(abs(ref_a[[x]] - fa[[x]])))[-c(11,19)]
  Fa = numerate_aa(input_sequence)/sum(numerate_aa(input_sequence))
  Fa = Fa[-c(11,19)]
  output = sum(Fa*dist_20aa)
  return(output)
}

