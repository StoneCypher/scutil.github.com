majorver=`head major.version`
minorver=`head minor.version`
ver=`head version.counter`
next=`expr $ver + 1`
`echo $next > version.counter`
gver=$majorver.$minorver.$next

echo "Updating $majorver.$minorver.$ver to $next (git $gver)"

`git tag -a $gver -m 'Version $gver'`
echo Created `git describe --tags --long`
git commit -m "$1"
git push
