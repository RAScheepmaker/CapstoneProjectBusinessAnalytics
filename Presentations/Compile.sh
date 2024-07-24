#!/bin/bash

#########################################################
# Build the pdf Document with LaTeX
#########################################################

echo "#-------------------------------------------------"
echo ""
echo "Building the presentation with LaTeX..."
echo ""

#cd Paper

# Use pdflatex to compile the LaTeX file
# pdflatex ScheepmakerPresentation1
pdflatex ScheepmakerPresentation4
# Run LaTeX twice to fix references in the document
# pdflatex ScheepmakerPresentation1
pdflatex ScheepmakerPresentation4
# Use bibtex to compile the references
# bibtex ScheepmakerPresentation1
bibtex ScheepmakerPresentation4
# pdflatex ScheepmakerPresentation1
# pdflatex ScheepmakerPresentation1
pdflatex ScheepmakerPresentation4
pdflatex ScheepmakerPresentation4

#cd ..

echo ""
echo "Finished building the pdf document with LaTeX."
echo ""
echo "#-------------------------------------------------"
echo ""

#########################################################
# End
#########################################################
