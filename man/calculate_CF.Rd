\name{calculate_CF}
\alias{calculate_CF}
\title{calculate CF}
\description{
Given a DNA coding sequence, calculate CF (change of frequency) of the sequence due to a substituted nucleotide.
}
\usage{
calculate_CF(substituted_nucleotide, substituted_position, original_sequence)
}
\arguments{
  \item{substituted_nucleotide}{the nucleotide at a position in the original sequence that is substituted to}

  \item{substituted_position}{the position in the original sequence where subsitution occurs}
  \item{original_sequence}{the original sequence before substitution}
}
\examples{
my_sequence = 'ATGCCGCCAACGGCATTCCTACTAATCTAG'
calculate_CF('A', 5, my_sequence)

}
