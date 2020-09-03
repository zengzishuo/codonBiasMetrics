
numerate_aa = function(input_sequence){
  codon_counts = count_codon(input_sequence)
  res = rep(0,20)
  # A
  res[1] = codon_counts[37]+codon_counts[40]+codon_counts[38]+codon_counts[39]
  # C
  res[2] = codon_counts[58]+codon_counts[60]
  # D
  res[3] = codon_counts[34]+codon_counts[36]
  # E
  res[4] = codon_counts[33]+codon_counts[35]
  # F
  res[5] = codon_counts[62]+codon_counts[64]
  # G
  res[6] = codon_counts[41]+codon_counts[42]+codon_counts[43]+codon_counts[44]
  # H
  res[7] = codon_counts[18]+codon_counts[20]
  # I
  res[8] = codon_counts[13]+codon_counts[14]+codon_counts[16]
  # K
  res[9] = codon_counts[1]+codon_counts[3]
  # L
  res[10] = codon_counts[29]+codon_counts[32]+codon_counts[30]+codon_counts[31]+codon_counts[61]+codon_counts[63]
  # M
  res[11] = codon_counts[15]
  # N
  res[12] = codon_counts[2]+codon_counts[4]
  # P
  res[13] = codon_counts[21]+codon_counts[23]+codon_counts[22]+codon_counts[24]
  # Q
  res[14] = codon_counts[17]+codon_counts[19]
  # R
  res[15] = codon_counts[9]+codon_counts[11]+codon_counts[28]+codon_counts[26]+codon_counts[25]+codon_counts[27]
  # S
  res[16] = codon_counts[10]+codon_counts[12]+codon_counts[55]+codon_counts[56]+codon_counts[54]+codon_counts[53]
  # T
  res[17] = codon_counts[5]+codon_counts[6]+codon_counts[7]+codon_counts[8]
  # V
  res[18] = codon_counts[45]+codon_counts[46]+codon_counts[47]+codon_counts[48]
  # W
  res[19] = codon_counts[59]
  # Y
  res[20] = codon_counts[50]+codon_counts[52]
  #stop codon
  #res[49] = codon_counts[49]+codon_counts[51]+codon_counts[57]
  return(res)
} #number of occurrences of aa in a sequence, length of 20

