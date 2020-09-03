\name{calculate_CUB}
\alias{calculate_CUB}
\title{calculate CUB}
\description{
Given a DNA coding sequence, calculate CUB (Codon Usage Bias) of the sequence.

CUB is from the publication here: Karlin, S. and J. Mrázek (1996). "What drives codon choices in human genes?" Journal of molecular biology 262(4): 459-472.
}
\usage{
calculate_CUB(input_sequence, ref_a)
}
\arguments{
  \item{input_sequence}{a DNA coding sequence}

  \item{ref_a}{A reference set of frequency of a codon for an amino acid. The default is derived from the top one percent of transcript expressions in GTEx data.}
}
\examples{
my_sequence = generate_a_sequence(20)
calculate_CUB(input_sequence = my_sequence)

}
