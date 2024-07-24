#!/bin/bash

################################################################################
# Running R scripts
################################################################################

echo ""
echo "#-------------------------------------------------"
echo "Running R scripts..."

cd Code

# Produce the Credit Scores Time Series plot used for Presentation 1 and Outline
Rscript CreditScoresTimeSeries.R > CreditScoresTimeSeries.out
# Produce the Prototype plots used for the Results section
Rscript CCPortfolioPrototype.R > CCPortfolioPrototype.out
# Run the Monte Carlo Simulation for M = 100,000
# Warning: the following RunMonteCarlo script takes > 2 hours to complete,
# uncomment at your own risk:
### Rscript CCPortfolioRunMonteCarlo.R > CCPortfolioRunMonteCarlo.out
# Analyze the (saved) Monte Carlo output
Rscript CCPortfolioAnalyzeMonteCarlo.R > CCPortfolioAnalyzeMonteCarlo.out

cd ..

echo "Finished running R scripts."
echo "#-------------------------------------------------"
echo ""

################################################################################
# Build the final Paper.pdf with LaTeX
################################################################################

echo ""
echo "#-------------------------------------------------"
echo "Building the Paper.pdf document with LaTeX..."

cd Paper

# Use pdflatex to compile the LaTeX file
pdflatex Paper
# Run LaTeX twice to fix references in the document
pdflatex Paper
# Use bibtex to compile the references
bibtex Paper
# Run LaTeX twice to fix references in the document
pdflatex Paper
pdflatex Paper

cd ..

echo "Finished building the Paper.pdf document with LaTeX."
echo "#-------------------------------------------------"
echo ""

echo ""
echo "#-------------------------------------------------"
echo "Copying Paper/Paper.pdf to RScheepmakerPaper.pdf..."

cp Paper/Paper.pdf RScheepmakerPaper.pdf

echo "Done."
echo "#-------------------------------------------------"

################################################################################
# End
################################################################################
