
% add_to_path('c:\\projects\\private\\xword\\').
% [somewordlist].
% [xword].

word(abalone,a,b,a,l,o,n,e).
word(abandon,a,b,a,n,d,o,n).
word(enhance,e,n,h,a,n,c,e).
word(anagram,a,n,a,g,r,a,m).
word(connect,c,o,n,n,e,c,t).
word(elegant,e,l,e,g,a,n,t).

make_3by7_hash(HA,HB,HC,VA,VB,VC) :-

    word(HA,_,At11,_,At12,_,At13,_),
    word(HB,_,At21,_,At22,_,At23,_),
    word(HC,_,At31,_,At32,_,At33,_),

    word(VA,_,At11,_,At21,_,At31,_),
    word(VB,_,At12,_,At22,_,At32,_),
    word(VC,_,At13,_,At23,_,At33,_).
