calculate_CAM = function(substituted_nucleotide, substituted_position, original_sequence){
  calc_one_part = function(x, y, pos_x, pos_y){
    res = ifelse(x == y, 1, 0)/abs(pos_x - pos_y)
    return(res)
  }
  ###
  substituted_sequence = original_sequence
  substr(substituted_sequence, start =substituted_position, stop=substituted_position) <- substituted_nucleotide
  n_th_codon_sub = ceiling(substituted_position/3)
  n = nchar(original_sequence)
  orig_seq_split = substring(original_sequence, seq(1, n-2, 3), seq(3, n, 3))
  sub_seq_split = substring(substituted_sequence, seq(1, n-2, 3), seq(3, n, 3))
  if(orig_seq_split[n_th_codon_sub] %in% c("TAA","TAG","TGA")){
    return(10)
  } else {
    if(find_which_aa_by_codon(orig_seq_split[n_th_codon_sub]) != find_which_aa_by_codon(sub_seq_split[n_th_codon_sub])){
      warning('it is not a synonymous mutation')
    }
    aa_seq = sapply(1:length(orig_seq_split),
                    function(x) find_which_aa_by_codon(orig_seq_split[x]))
    the_aa = find_which_aa_by_codon(orig_seq_split[n_th_codon_sub])
    pos_with_same_aa = which(aa_seq == the_aa)
    if (length(which(aa_seq == the_aa)) ==1) {
      return(0)
    } else if(n_th_codon_sub == min(pos_with_same_aa)){
      pos_after = sort(pos_with_same_aa[which(pos_with_same_aa > n_th_codon_sub)])
      nearest_pos_after = pos_after[1]
      res = calc_one_part(sub_seq_split[n_th_codon_sub], sub_seq_split[nearest_pos_after],
                          n_th_codon_sub, nearest_pos_after)-
        calc_one_part(orig_seq_split[n_th_codon_sub], orig_seq_split[nearest_pos_after],
                      n_th_codon_sub, nearest_pos_after)
      return(res)
    } else if (n_th_codon_sub == max(pos_with_same_aa)) {
      pos_previous = sort(pos_with_same_aa[which(pos_with_same_aa < n_th_codon_sub)])
      nearest_pos_previous = pos_previous[length(pos_previous)]
      res = calc_one_part(sub_seq_split[n_th_codon_sub], sub_seq_split[nearest_pos_previous],
                          n_th_codon_sub, nearest_pos_previous)-
        calc_one_part(orig_seq_split[n_th_codon_sub], orig_seq_split[nearest_pos_previous],
                      n_th_codon_sub, nearest_pos_previous)
      return(res)
    } else {
      pos_previous = sort(pos_with_same_aa[which(pos_with_same_aa < n_th_codon_sub)])
      nearest_pos_previous = pos_previous[length(pos_previous)]
      pos_after = sort(pos_with_same_aa[which(pos_with_same_aa > n_th_codon_sub)])
      nearest_pos_after = pos_after[1]
      res = calc_one_part(sub_seq_split[n_th_codon_sub], sub_seq_split[nearest_pos_after],
                          n_th_codon_sub, nearest_pos_after) +
        calc_one_part(sub_seq_split[n_th_codon_sub], sub_seq_split[nearest_pos_previous],
                      n_th_codon_sub, nearest_pos_previous) -
        calc_one_part(orig_seq_split[n_th_codon_sub], orig_seq_split[nearest_pos_after],
                      n_th_codon_sub, nearest_pos_after) -
        calc_one_part(orig_seq_split[n_th_codon_sub], orig_seq_split[nearest_pos_previous],
                      n_th_codon_sub, nearest_pos_previous)
      return(res)
    }
  }
}


