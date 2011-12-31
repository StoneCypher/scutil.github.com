
// $Revision$
// $Date$
// since April 3, 2009
// $Id$
// $HeadURL$





var scutil_LastUniqueIdFound = 0;

function CreateUniqueId(Prefix, Suffix) {

    'use strict';

    var uPref = (Prefix === undefined) ? 'id_' : Prefix,
        uSuff = (Suffix === undefined) ? ''    : Suffix,
        Curr;

    while (true) {

        ++scutil_LastUniqueIdFound;

        Curr = Prefix + scutil_LastUniqueIdFound.toString() + Suffix;
        if (document.getElementById(Curr) === undefined) {
            return Curr;
        }
    }

}




function emptyish(X) {

    'use strict';

    // there are a variety of reasons not to add empty objects

    switch (X) {

        case 0:
        case 0.0:
        case false:
        case null:
        case '':
        case []:
        case undefined:
        case Number.NAN:
            return true;

    }

    return false;

}






function AddClassIfMissing(Tag, tclass) {

    'use strict';
    
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
            var Classes = Tag.className.split(' '), 
                i;

            // iterate the resulting classes
            for (i in Classes) {

                // if one matches the to-be-added, do nothing and return
                if (Classes[i] === tclass) {
                    return;
                }
            }

            // otherwise stuff the class and a space on the front and return
            Tag.className += (' ' + tclass);

    }

}





// only removes the first instance of the class if repeated

function RemoveClassOnceIfPresent(Tag, tclass) {

    'use strict';

    // take the current class list and tokenize on spaces
    var Classes = Tag.className.split(' '),
        i;

    // iterate the resulting classes
    for (i in Classes) {

        // remove any matching element
        if (Classes[i] === tclass) {

            Classes.splice(i, 1);

            // then immediately recombine and return
            Tag.className = Classes.join(' ');
            return;

        }
    }
}





function RemoveClassRepeatedlyIfPresent(Tag, tclass) {

    'use strict';

    // Take the current class list and tokenise it on spaces.
    var Classes = Tag.className.split(' '),
        i;

    // iterate the resulting classes
    for (i in Classes) {

        // even though the non-match of a tclass would get skipped, the repeated joins can cause combinatoric
        // space expansion if there are extra 0-length strings getting .join()ed back together to make more,
        // so we discard them as a memory space safety issue

        if (Classes[i] === '') {
            Classes.splice(i, 1);
        }

        // remove any matching element

        if (Classes[i] === tclass) {
            Classes.splice(i, 1);
        }

    }

    // Recombine the result

    Tag.className = Classes.join(' ');

}





function Zebrafy(Tag, RowType, ClassList) {

    'use strict';


    var kid = Tag.firstChild,   // get the first child of the container to start the tag iteration
        cc = ClassList.length,  // cc is the class count - how many classes there are.  cached because object inspection could have a changing value (no mutability control in javascript :( )
        ci = -1;                // ci is the class index - the current class.  started at -1 so that when the counter is immediately incremented, it's to the correct default of 0, to skip uptick safety logic.


    // loop will terminate when out of children
    while (kid !== undefined) {

        // if it isn't the kind of child counted, skip to the next without incrementing the class counter
        if (kid.nodeName === RowType.toUpperCase()) {

            // increment the class counter, and zero on tick-over
            if ((++ci) >= cc) {
                ci = 0;
            }

            // set the target row's class
            AddClassIfMissing(kid, ClassList[ci]);

        }

        // next tag
        kid = kid.nextSibling;

    }

}





function IsLeapYear(Year) {

    'use strict';

    return (
        ((Year % 400) === 0)? true  : (
        ((Year % 100) === 0)? false : (
        ((Year % 4)   === 0)? true  :
                              false
        ))
    );

}





function MonthLength(Month, Year) {

    'use strict';

    switch (Month) {
        case  1: return 31;
        case  2: return (IsLeapYear(Year))? 29 : 28;
        case  3: return 31;
        case  4: return 30;
        case  5: return 31;
        case  6: return 30;
        case  7: return 31;
        case  8: return 31;
        case  9: return 30;
        case 10: return 31;
        case 11: return 30;
        case 12: return 31;
    }

    return false;

}





function DateToOrdinalDate(Month, Day, Year) {

    'use strict';

    var Base = 0,
        i;

    for (i=1; i<Month; ++i) {
        Base += MonthLength(i, Year);
    }

    return { 'year':Year, 'ord':Base+Day };

}
