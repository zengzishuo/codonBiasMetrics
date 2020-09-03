
calculate_CAI = function(input_sequence, ref_W = W){
  codon_counts = count_codon(input_sequence)
  exp(sum(sapply(1:64, function(x) codon_counts[x]*log(ref_W[x]) ))/sum(codon_counts))
}
