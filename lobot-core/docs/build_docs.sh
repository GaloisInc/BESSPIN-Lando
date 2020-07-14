pandoc --toc --standalone --metadata title="README" --include-in-header="resources/doc-header.html" ../README.md >README.html
pandoc --toc --standalone --number-sections --metadata title="Lobot User's Guide" --include-in-header="resources/doc-header.html" UserGuide.md >UserGuide.html
