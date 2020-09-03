\name{calculate_CAM}
\alias{calculate_CAM}
\title{calculate CAM}
\description{
Given a DNA coding sequence, calculate CAM (Codon Autocorrelation Measure) of the sequence due to substitution of nucleotide.
}
\usage{
calculate_CAM(substituted_nucleotide, substituted_position, original_sequence)
}
\arguments{
  \item{substituted_nucleotide}{the nucleotide at a position in the original sequence that is substituted to}

  \item{substituted_position}{the position in the original sequence where subsitution occurs}
  \item{original_sequence}{the original sequence before substitution}
}
\examples{
my_sequence = 'ATGCCGCCAACGGCATAT'
calculate_CAM('A', 5, my_sequence)

}
