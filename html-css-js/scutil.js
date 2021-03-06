
// since April 3, 2009





var scutil_LastUniqueIdFound = 0;

function createUniqueId(Prefix, Suffix) {

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
//      case 0.0:   // not needed, JS promotes numbers to match in either direction in a case
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






function addClassIfMissing(Tag, tclass) {

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

                if (Classes.hasOwnProperty(i)) {

                    // if one matches the to-be-added, do nothing and return
                    if (Classes[i] === tclass) {
                        return;
                    }
                
                }

            }

            // otherwise stuff the class and a space on the front and return
            Tag.className += (' ' + tclass);

    }

}





// only removes the first instance of the class if repeated

function removeClassOnceIfPresent(Tag, tclass) {

    'use strict';

    // take the current class list and tokenize on spaces
    var Classes = Tag.className.split(' '),
        i;

    // iterate the resulting classes
    for (i in Classes) {

        if (Classes.hasOwnProperty(i)) {

            // remove any matching element
            if (Classes[i] === tclass) {

                Classes.splice(i, 1);

                // then immediately recombine and return
                Tag.className = Classes.join(' ');
                return;

            }
        }
    }
}





function removeClassRepeatedlyIfPresent(Tag, tclass) {

    'use strict';

    // Take the current class list and tokenise it on spaces.
    var Classes = Tag.className.split(' '),
        i;

    // iterate the resulting classes
    for (i in Classes) {

        if (Classes.hasOwnProperty(i)) {
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

    }

    // Recombine the result

    Tag.className = Classes.join(' ');

}





function zebrafy(Tag, RowType, ClassList) {

    'use strict';

    var kid = Tag.firstChild,                          // get the first child of the container to start the tag iteration
        cc  = ClassList.length,                        // cc is the class count - how many classes there are.  cached because object inspection could have a changing value (no mutability control in javascript :( )
        ci  = -1;                                      // ci is the class index - the current class.  started at -1 so that when the counter is immediately incremented, it's to the correct default of 0, to skip uptick safety logic.

    while (kid !== undefined) {

        if (kid.nodeName === RowType.toUpperCase()) {  // if it isn't the kind of child counted, skip to the next without incrementing the class counter

            if ((++ci) >= cc) {
                ci = 0;
            }

            addClassIfMissing(kid, ClassList[ci]);     // set the target row's class

        }

        kid = kid.nextSibling;

    }

}





function isLeapYear(Year) {

    'use strict';

    return (
        ((Year % 400) === 0)? true  : (
        ((Year % 100) === 0)? false : (
        ((Year % 4)   === 0)? true  :
                              false
        ))
    );

}





function monthLength(Month, Year) {

    'use strict';

    switch (Month) {

        case  1: return 31;
        case  2: return (isLeapYear(Year) === true)? 29 : 28;
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





function dateToOrdinalDate(Month, Day, Year) {

    'use strict';

    var Base = 0,
        i;

    for (i=1; i<Month; ++i) {
        Base += monthLength(i, Year);
    }

    return { 'year':Year, 'ord':Base+Day };

}
