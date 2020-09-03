
synonymous_usage_ratio = function(input_sequence){
  codon_counts = count_codon(input_sequence)
  res = rep(1,64)
  res[1] = codon_counts[1]/(codon_counts[1]+codon_counts[3])
  res[3] = codon_counts[3]/(codon_counts[1]+codon_counts[3])

  res[2] = codon_counts[2]/(codon_counts[2]+codon_counts[4])
  res[4] = codon_counts[4]/(codon_counts[2]+codon_counts[4])

  res[5] = codon_counts[5]/(codon_counts[5]+codon_counts[6]+codon_counts[7]+codon_counts[8])
  res[6] = codon_counts[6]/(codon_counts[5]+codon_counts[6]+codon_counts[7]+codon_counts[8])
  res[7] = codon_counts[7]/(codon_counts[5]+codon_counts[6]+codon_counts[7]+codon_counts[8])
  res[8] = codon_counts[8]/(codon_counts[5]+codon_counts[6]+codon_counts[7]+codon_counts[8])

  res[9] = codon_counts[9]/(codon_counts[9]+codon_counts[11]+codon_counts[28]+codon_counts[26]+codon_counts[25]+codon_counts[27])
  res[11] = codon_counts[11]/(codon_counts[9]+codon_counts[11]+codon_counts[28]+codon_counts[26]+codon_counts[25]+codon_counts[27])
  res[25] = codon_counts[25]/(codon_counts[9]+codon_counts[11]+codon_counts[28]+codon_counts[26]+codon_counts[25]+codon_counts[27])
  res[26] = codon_counts[26]/(codon_counts[9]+codon_counts[11]+codon_counts[28]+codon_counts[26]+codon_counts[25]+codon_counts[27])
  res[27] = codon_counts[27]/(codon_counts[9]+codon_counts[11]+codon_counts[28]+codon_counts[26]+codon_counts[25]+codon_counts[27])
  res[28] = codon_counts[28]/(codon_counts[9]+codon_counts[11]+codon_counts[28]+codon_counts[26]+codon_counts[25]+codon_counts[27])

  res[10] = codon_counts[10]/(codon_counts[10]+codon_counts[12]+codon_counts[55]+codon_counts[56]+codon_counts[54]+codon_counts[53])
  res[12] = codon_counts[12]/(codon_counts[10]+codon_counts[12]+codon_counts[55]+codon_counts[56]+codon_counts[54]+codon_counts[53])
  res[53] = codon_counts[53]/(codon_counts[10]+codon_counts[12]+codon_counts[55]+codon_counts[56]+codon_counts[54]+codon_counts[53])
  res[54] = codon_counts[54]/(codon_counts[10]+codon_counts[12]+codon_counts[55]+codon_counts[56]+codon_counts[54]+codon_counts[53])
  res[55] = codon_counts[55]/(codon_counts[10]+codon_counts[12]+codon_counts[55]+codon_counts[56]+codon_counts[54]+codon_counts[53])
  res[56] = codon_counts[56]/(codon_counts[10]+codon_counts[12]+codon_counts[55]+codon_counts[56]+codon_counts[54]+codon_counts[53])

  res[13] = codon_counts[13]/(codon_counts[13]+codon_counts[14]+codon_counts[16])
  res[14] = codon_counts[14]/(codon_counts[13]+codon_counts[14]+codon_counts[16])
  res[16] = codon_counts[16]/(codon_counts[13]+codon_counts[14]+codon_counts[16])

  res[17] = codon_counts[17]/(codon_counts[17]+codon_counts[19])
  res[19] = codon_counts[19]/(codon_counts[17]+codon_counts[19])

  res[29] = codon_counts[29]/(codon_counts[29]+codon_counts[32]+codon_counts[30]+codon_counts[31]+codon_counts[61]+codon_counts[63])
  res[32] = codon_counts[32]/(codon_counts[29]+codon_counts[32]+codon_counts[30]+codon_counts[31]+codon_counts[61]+codon_counts[63])
  res[30] = codon_counts[30]/(codon_counts[29]+codon_counts[32]+codon_counts[30]+codon_counts[31]+codon_counts[61]+codon_counts[63])
  res[31] = codon_counts[31]/(codon_counts[29]+codon_counts[32]+codon_counts[30]+codon_counts[31]+codon_counts[61]+codon_counts[63])
  res[61] = codon_counts[61]/(codon_counts[29]+codon_counts[32]+codon_counts[30]+codon_counts[31]+codon_counts[61]+codon_counts[63])
  res[63] = codon_counts[63]/(codon_counts[29]+codon_counts[32]+codon_counts[30]+codon_counts[31]+codon_counts[61]+codon_counts[63])

  res[21] = codon_counts[21]/(codon_counts[21]+codon_counts[23]+codon_counts[22]+codon_counts[24])
  res[23] = codon_counts[23]/(codon_counts[21]+codon_counts[23]+codon_counts[22]+codon_counts[24])
  res[22] = codon_counts[22]/(codon_counts[21]+codon_counts[23]+codon_counts[22]+codon_counts[24])
  res[24] = codon_counts[24]/(codon_counts[21]+codon_counts[23]+codon_counts[22]+codon_counts[24])

  res[18] = codon_counts[18]/(codon_counts[18]+codon_counts[20])
  res[20] = codon_counts[20]/(codon_counts[18]+codon_counts[20])

  res[33] = codon_counts[33]/(codon_counts[33]+codon_counts[35])
  res[35] = codon_counts[35]/(codon_counts[33]+codon_counts[35])

  res[34] = codon_counts[34]/(codon_counts[34]+codon_counts[36])
  res[36] = codon_counts[36]/(codon_counts[34]+codon_counts[36])

  res[37] = codon_counts[37]/(codon_counts[37]+codon_counts[40]+codon_counts[38]+codon_counts[39])
  res[40] = codon_counts[40]/(codon_counts[37]+codon_counts[40]+codon_counts[38]+codon_counts[39])
  res[38] = codon_counts[38]/(codon_counts[37]+codon_counts[40]+codon_counts[38]+codon_counts[39])
  res[39] = codon_counts[39]/(codon_counts[37]+codon_counts[40]+codon_counts[38]+codon_counts[39])

  res[41] = codon_counts[41]/(codon_counts[41]+codon_counts[42]+codon_counts[43]+codon_counts[44])
  res[42] = codon_counts[42]/(codon_counts[41]+codon_counts[42]+codon_counts[43]+codon_counts[44])
  res[43] = codon_counts[43]/(codon_counts[41]+codon_counts[42]+codon_counts[43]+codon_counts[44])
  res[44] = codon_counts[44]/(codon_counts[41]+codon_counts[42]+codon_counts[43]+codon_counts[44])

  res[45] = codon_counts[45]/(codon_counts[45]+codon_counts[46]+codon_counts[47]+codon_counts[48])
  res[46] = codon_counts[46]/(codon_counts[45]+codon_counts[46]+codon_counts[47]+codon_counts[48])
  res[47] = codon_counts[47]/(codon_counts[45]+codon_counts[46]+codon_counts[47]+codon_counts[48])
  res[48] = codon_counts[48]/(codon_counts[45]+codon_counts[46]+codon_counts[47]+codon_counts[48])

  res[49] = codon_counts[49]/(codon_counts[49]+codon_counts[51]+codon_counts[57])
  res[51] = codon_counts[51]/(codon_counts[49]+codon_counts[51]+codon_counts[57])
  res[57] = codon_counts[57]/(codon_counts[49]+codon_counts[51]+codon_counts[57])

  res[50] = codon_counts[50]/(codon_counts[50]+codon_counts[52])
  res[52] = codon_counts[52]/(codon_counts[50]+codon_counts[52])

  res[58] = codon_counts[58]/(codon_counts[58]+codon_counts[60])
  res[60] = codon_counts[60]/(codon_counts[58]+codon_counts[60])

  res[62] = codon_counts[62]/(codon_counts[62]+codon_counts[64])
  res[64] = codon_counts[64]/(codon_counts[62]+codon_counts[64])

  res[which(is.na(res) == T)] = 0
  return(res)
}
