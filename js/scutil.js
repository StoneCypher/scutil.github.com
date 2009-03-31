
// $Revision$

function AddClassIfMissing(Tag, tclass) {

  switch (Tag.className) {
    case ''        : Tag.className = tclass; break;
    case undefined : Tag.className = tclass; break;
    default :
      var Classes = Tag.className.split(' ');
      for (i in Classes) { if (Classes[i] == tclass) { return; } }
      Tag.className += (' ' + tclass);
  }
}





// only removes the first instance of the class if repeated

function RemoveClassOnceIfPresent(Tag, tclass) {
  var Classes = Tag.className.split(' ');
  for (i in Classes) {
    if (Classes[i] == tclass) {
      Classes.splice(i, 1);
      Tag.className = Classes.join(' ');
      return;
    }
  }
}





function RemoveClassRepeatedlyIfPresent(Tag, tclass) {
  var Classes = Tag.className.split(' ');
  for (i in Classes) {
    if (Classes[i] == tclass) {
      Classes.splice(i, 1);
      Tag.className = Classes.join(' ');
    }
  }
}





// Todo: change this from two arguments to a single list argument, so that N zebra colors may be supported

//function Zebrafy(Tag, RowType, class1, class2) {
//
//  var kid = Tag.firstChild;
//  var cl1 = true;
//
//  while (kid != undefined) {
//    if (kid.nodeName != RowType.toUpperCase()) { kid = kid.nextSibling; continue; }
//    AddClassIfMissing(kid, cl1? class1 : class2);
//    cl1 = cl1? false:true;
//    kid = kid.nextSibling;
//  }
//
//}
