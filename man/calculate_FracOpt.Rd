\name{calculate_FracOpt}
\alias{calculate_FracOpt}
\title{calculate FracOpt}
\description{
Given a DNA coding sequence, calculate FracOpt (fraction of optimal codons) of the sequence.

Ikemura, T. (1981). "Correlation between the abundance of Escherichia coli transfer RNAs and the occurrence of the respective codons in its protein genes: a proposal for a synonymous codon choice that is optimal for the E. coli translational system." Journal of molecular biology 151(3): 389-409.
}
\usage{
calculate_FracOpt(input_sequence)
}
\arguments{
  \item{input_sequence}{a DNA coding sequence}
}
\examples{
my_sequence = generate_a_sequence(20)
calculate_FracOpt(input_sequence = my_sequence)

}
