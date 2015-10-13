cd /tmp
rm -rf oii-r
git clone git@github.com:oxfordinternetinstitute/oii-r.git
cd oii-r
git checkout --orphan gh-pages
git rm --cached -r *
git rm --cached .Rbuildignore .gitignore
#TODO: generate documentation
R CMD build .
mv oii_*.tar.gz oii.tar.gz
R CMD check oii.tar.gz
git add oii.tar.gz
git commit -m "v1.0"
git push -f origin gh-pages
