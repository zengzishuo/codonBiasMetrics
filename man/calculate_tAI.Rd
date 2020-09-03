\name{calculate_tAI}
\alias{calculate_tAI}
\title{calculate tAI}
\description{
Given a DNA coding sequence, calculate CAI (Codon Adaptation Index) of the sequence.

Dos Reis, M., et al. (2003). "Unexpected correlations between gene expression and codon usage bias from microarray data for the whole Escherichia coli K‐12 genome." Nucleic acids research 31(23): 6976-6985.
}
\usage{
calculate_tAI(input_sequence, ref_W)
}
\arguments{
  \item{input_sequence}{a DNA coding sequence}

  \item{ref_w}{A reference set of translational efficiency. The default s derived from predictions from genomic tRNA database (GtRNAdb).}
}
\examples{
my_sequence = generate_a_sequence(20)
calculate_tAI(input_sequence = my_sequence)

}
