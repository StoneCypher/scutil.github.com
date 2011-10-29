majorver=`head major.version`
ver=`head version.counter`
next=`expr $ver + 1`
`echo $next > version.counter`
gver=$majorver.$next

echo "Updating $majorver $ver to $next (git $gver)"

`git tag -a $gver -m 'Version $gver'`
echo Created `git describe --tags --long`
git commit -a -m "$1"
git push