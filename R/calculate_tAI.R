
calculate_tAI = function(input_sequence, ref_w = wi){
  codon_counts = count_codon(input_sequence)
  return(exp(sum(codon_counts*log(ref_w))/sum(codon_counts)))
}

