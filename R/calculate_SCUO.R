
calculate_SCUO = function(input_sequence){
  ha = function(a_set_freqs){ #freq of codons for the same aa
    out = -sum(a_set_freqs*log(a_set_freqs))
    return(out)
  }
  codon_counts = count_codon(input_sequence)
  tmp2 = synonymous_usage_ratio(input_sequence)
  tmp2[which(tmp2==0)] = 0.000001
  Has = sapply(1:20, function(x) ha(tmp2[pointers[[x]]]))
  ka = c(4,2,2,2,2,4,2,3,2,6,1,2,4,2,6,6,4,4,1,2)
  max_Ha = log(ka)
  Ea = sapply(1:20, function(x) (max_Ha[x]-Has[x])/max_Ha[x])[-c(11,19)]
  Fa = (numerate_aa(input_sequence)/sum(numerate_aa(input_sequence)))[-c(11,19)]
  return(sum(Ea*Fa))
}

