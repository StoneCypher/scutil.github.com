
// $Revision: 323 $
// $Date: 2009-04-05 17:01:30 -0600 (Sun, 05 Apr 2009) $
// since April 3, 2009
// $Id: scutil.js 323 2009-04-05 23:01:30Z john $
// $HeadURL: svn://crunchyd.com/scutil/js/scutil.js $





function AddClassIfMissing(Tag, tclass) {

    // depending on the class name
    switch (Tag.className) {

        // if it exists and is empty, set to the class and leave
        case '' :
            Tag.className = tclass;
            break;

        // if it doesn't exist, set to the class and leave
        case undefined :
            Tag.className = tclass;
            break;

        // if there is/are class(es),
        default :

            // tokenise on spaces
            var Classes = Tag.className.split(' ');

            // iterate the resulting classes
            for (i in Classes) { 

                // if one matches the to-be-added, do nothing and return
                if (Classes[i] == tclass) {
                    return;
                }
            }

            // otherwise stuff the class and a space on the front and return
            Tag.className += (' ' + tclass);

    }

}





// only removes the first instance of the class if repeated

function RemoveClassOnceIfPresent(Tag, tclass) {

    // take the current class list and tokenize on spaces
    var Classes = Tag.className.split(' ');
    
    // iterate the resulting classes
    for (i in Classes) {

        // remove any matching element
        if (Classes[i] == tclass) {

            Classes.splice(i, 1);
            
            // then immediately recombine and return
            Tag.className = Classes.join(' ');
            return;

        }
    }
}





function RemoveClassRepeatedlyIfPresent(Tag, tclass) {

    // Take the current class list and tokenise it on spaces.
    var Classes = Tag.className.split(' ');

    // iterate the resulting classes
    for (i in Classes) {

        // even though the non-match of a tclass would get skipped, the repeated joins can cause combinatoric
        // space expansion if there are extra 0-length strings getting .join()ed back together to make more,
        // so we discard them as a memory space safety issue

        if (Classes[i] == '') {
            Classes.splice(i, 1);
        }

        // remove any matching element

        if (Classes[i] == tclass) {
            Classes.splice(i, 1);
        }

    }

    // Recombine the result

    Tag.className = Classes.join(' ');

}





function Zebrafy(Tag, RowType, ClassList) {


    // get the first child of the container to start the tag iteration
    var kid = Tag.firstChild;

    // cc is the class count - how many classes there are.  cached because object inspection could have a changing value (no mutability control in javascript :( )
    var cc = ClassList.length;

    // ci is the class index - the current class.  started at -1 so that when the counter is immediately incremented, it's to the correct default of 0, to skip uptick safety logic.
    var ci = -1;


    // loop will terminate when out of children
    while (kid != undefined) {

        // if it isn't the kind of child counted, skip to the next without incrementing the class counter
        if (kid.nodeName != RowType.toUpperCase()) {
            kid = kid.nextSibling;
            continue;
        }

        // increment the class counter, and zero on tick-over
        if ((++ci) >= cc) {
            ci = 0;
        }

        // set the target row's class
        AddClassIfMissing(kid, ClassList[ci]);

        // next tag
        kid = kid.nextSibling;

    }

}
