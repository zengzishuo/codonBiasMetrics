\name{calculate_CAI}
\alias{calculate_CAI}
\title{calculate CAI}
\description{
Given a DNA coding sequence, calculate CAI (Codon Adaptation Index) of the sequence.

CAI is from the publication here: Sharp, P. M. and W.-H. Li (1987). "The codon adaptation index-a measure of directional synonymous codon usage bias, and its potential applications." Nucleic acids research 15(3): 1281-1295.
}
\usage{
calculate_CAI(input_sequence, ref_W)
}
\arguments{
  \item{input_sequence}{a DNA coding sequence}

  \item{ref_W}{A reference set of w (frequency of a codon divided by the most frequently used codon for that amino acid). Default input is derived from the sequences of top one percent expressed transcripts in GTEx database.}
}
\examples{
my_sequence = generate_a_sequence(20)
calculate_CAI(input_sequence = my_sequence)

}
