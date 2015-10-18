cd /tmp
rm -rf oii-r
git clone git@github.com:oxfordinternetinstitute/oii-r.git
cd oii-r
git checkout --orphan gh-pages
git rm --cached -r *
git rm --cached .Rbuildignore .gitignore
rm -rf *.tar.gz
echo "devtools::document()" | R --no-save
R CMD build .
mv oii_*.tar.gz oii.tar.gz
R CMD check oii.tar.gz
git add oii.tar.gz
git commit -m "Generated on `date`"
git push -f origin gh-pages
