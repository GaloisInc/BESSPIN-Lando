pandoc --toc --standalone --number-sections --metadata title="README" --include-in-header="resources/doc-header.html" ../README.md -o README.html
pandoc --toc --standalone --number-sections --metadata title="Lobot User's Guide" --include-in-header="resources/doc-header.html" UserGuide.md -o UserGuide.html
pandoc --toc --standalone --number-sections --metadata title="Lobot User's Guide" --metadata author="Ben Selfridge, Galois Inc." UserGuide.md -o UserGuide.pdf
