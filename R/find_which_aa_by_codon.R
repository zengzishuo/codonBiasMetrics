
find_which_aa_by_codon = function(CODON){
  `%notin%` <- Negate(`%in%`)
  if(CODON %notin% codons){
    stop('input should be a DNA codon')
  }
  tf = sapply(1:21, function(x) CODON %in% aa_codon_list21[[x]])
  ind = which(tf == T)
  return(attributes(aa_codon_list21)[[1]][ind])
}

