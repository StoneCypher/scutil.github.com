majorver=`head major.version`
minorver=`head minor.version`
ver=`head version.counter`
next=`expr $ver + 1`
`echo $next > version.counter`
gver=$majorver.$minorver.$next

blue='\e[1;34m'
gray='\e[0m'

echo "${blue}Updating $majorver.$minorver.$ver to $next (git $gver)$gray"

`git tag -a $gver -m 'Version $gver'`
echo Created `git describe --tags --long`
git commit -a -m "$1"
git push
