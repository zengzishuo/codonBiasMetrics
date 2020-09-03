
count_codon = function(input_sequence){
  input_sequence = toupper(input_sequence)
  unique_char = sort(unique(strsplit(input_sequence, "")[[1]]))
  if(all(unique_char == c("A", "C", "G", "T")) != T){
    stop('sequence contains characters other than A, C, G, T')}
  n = nchar(input_sequence)
  if(n %%3 != 0){
    warning('sequence length is not divisible by 3')}
  tmp = substring(input_sequence, seq(1, n-2, 3), seq(3, n, 3))
  if(tmp[1] != 'ATG'){
    warning('first codon is not ATG')}
  if(all(tmp[length(tmp)] %in% c("TAA", "TGA", "TAG")) != T){
    warning('last codon is not stop codon')}
  return(c(sum(str_count(tmp, pattern = "AAA")),
           sum(str_count(tmp, pattern = "AAC")),
           sum(str_count(tmp, pattern = "AAG")),
           sum(str_count(tmp, pattern = "AAT")),
           sum(str_count(tmp, pattern = "ACA")),
           sum(str_count(tmp, pattern = "ACC")),
           sum(str_count(tmp, pattern = "ACG")),
           sum(str_count(tmp, pattern = "ACT")),
           sum(str_count(tmp, pattern = "AGA")),
           sum(str_count(tmp, pattern = "AGC")),
           sum(str_count(tmp, pattern = "AGG")),
           sum(str_count(tmp, pattern = "AGT")),
           sum(str_count(tmp, pattern = "ATA")),
           sum(str_count(tmp, pattern = "ATC")),
           sum(str_count(tmp, pattern = "ATG")),
           sum(str_count(tmp, pattern = "ATT")),
           sum(str_count(tmp, pattern = "CAA")),
           sum(str_count(tmp, pattern = "CAC")),
           sum(str_count(tmp, pattern = "CAG")),
           sum(str_count(tmp, pattern = "CAT")),
           sum(str_count(tmp, pattern = "CCA")),
           sum(str_count(tmp, pattern = "CCC")),
           sum(str_count(tmp, pattern = "CCG")),
           sum(str_count(tmp, pattern = "CCT")),
           sum(str_count(tmp, pattern = "CGA")),
           sum(str_count(tmp, pattern = "CGC")),
           sum(str_count(tmp, pattern = "CGG")),
           sum(str_count(tmp, pattern = "CGT")),
           sum(str_count(tmp, pattern = "CTA")),
           sum(str_count(tmp, pattern = "CTC")),
           sum(str_count(tmp, pattern = "CTG")),
           sum(str_count(tmp, pattern = "CTT")),
           sum(str_count(tmp, pattern = "GAA")),
           sum(str_count(tmp, pattern = "GAC")),
           sum(str_count(tmp, pattern = "GAG")),
           sum(str_count(tmp, pattern = "GAT")),
           sum(str_count(tmp, pattern = "GCA")),
           sum(str_count(tmp, pattern = "GCC")),
           sum(str_count(tmp, pattern = "GCG")),
           sum(str_count(tmp, pattern = "GCT")),
           sum(str_count(tmp, pattern = "GGA")),
           sum(str_count(tmp, pattern = "GGC")),
           sum(str_count(tmp, pattern = "GGG")),
           sum(str_count(tmp, pattern = "GGT")),
           sum(str_count(tmp, pattern = "GTA")),
           sum(str_count(tmp, pattern = "GTC")),
           sum(str_count(tmp, pattern = "GTG")),
           sum(str_count(tmp, pattern = "GTT")),
           sum(str_count(tmp, pattern = "TAA")),
           sum(str_count(tmp, pattern = "TAC")),
           sum(str_count(tmp, pattern = "TAG")),
           sum(str_count(tmp, pattern = "TAT")),
           sum(str_count(tmp, pattern = "TCA")),
           sum(str_count(tmp, pattern = "TCC")),
           sum(str_count(tmp, pattern = "TCG")),
           sum(str_count(tmp, pattern = "TCT")),
           sum(str_count(tmp, pattern = "TGA")),
           sum(str_count(tmp, pattern = "TGC")),
           sum(str_count(tmp, pattern = "TGG")),
           sum(str_count(tmp, pattern = "TGT")),
           sum(str_count(tmp, pattern = "TTA")),
           sum(str_count(tmp, pattern = "TTC")),
           sum(str_count(tmp, pattern = "TTG")),
           sum(str_count(tmp, pattern = "TTT"))
  ))
}

