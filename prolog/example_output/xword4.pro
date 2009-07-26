
% add_to_path('c:\\projects\\private\\xword\\').
% [xword4].

word(cur,c,u,r).
word(cat,c,a,t).
word(ran,r,a,n).
word(tan,t,a,n).

make(HA,HB,VA,VB) :-

    word(HA,At11,u,At12),
    word(HB,At21,_,At22),

    word(VA,At11,_,At21),
    word(VB,At12,_,At22),

    HA \= HB,
    HA \= VA,
    HA \= VB,

    HB \= VA,
    HB \= VB,

    VA \= VB.
