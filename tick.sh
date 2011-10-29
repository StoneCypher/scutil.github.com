ver=`head version.counter`
next=`expr $ver + 1`
`echo $next > version.counter`
gver=2.5.$next

echo "Updating $ver to $next (git $gver)"

`git tag -a $gver -m 'Version $gver'`
echo Created `git describe --tags --long`
git commit -a -m "$1"
git push
