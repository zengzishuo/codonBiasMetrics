calculate_CF = function(substituted_nucleotide, substituted_position, original_sequence){
  substituted_sequence = original_sequence
  substr(substituted_sequence, start =substituted_position, stop=substituted_position) <- substituted_nucleotide
  ###
  n = nchar(original_sequence)
  original_codons_seq = substring(original_sequence, seq(1, n-2, 3), seq(3, n, 3))
  substituted_codons_seq = substring(substituted_sequence, seq(1, n-2, 3), seq(3, n, 3))
  ###
  OC = original_codons_seq[ceiling(substituted_position/3)]
  SC = substituted_codons_seq[ceiling(substituted_position/3)]
  ###
  original_codon_counts = count_codon(original_sequence)
  substituted_codon_counts = count_codon(substituted_sequence)
  tf = sapply(1:21, function(x) which(codons == OC) %in% pointers[[x]])
  IND = which(tf == T)
  syn_codons = codons[pointers[[IND]]]
  total_n = as.numeric(sum(original_codon_counts[which(codons %in% syn_codons)]))
  freq_oc_before = as.numeric(original_codon_counts[which(codons == OC)])/total_n
  freq_sc_before = as.numeric(original_codon_counts[which(codons == SC)])/total_n
  freq_oc_after = as.numeric(substituted_codon_counts[which(codons == OC)])/total_n
  freq_sc_after = as.numeric(substituted_codon_counts[which(codons == SC)])/total_n
  out = abs(freq_oc_before - freq_oc_after) + abs(freq_sc_before - freq_sc_after)
  return(out)
}# CF:change of codon frequency between original and substitute codon


