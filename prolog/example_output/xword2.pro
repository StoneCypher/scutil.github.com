
% add_to_path('c:\\projects\\private\\xword\\').
% [xword2].

word(ax,a,x).
word(by,b,y).
word(abc,a,b,c).
word(xyz,x,y,z).
word(cz,c,z).

make_2by3(HA,HB,HC,VA,VB) :-

    word(HA,At11,At12),
    word(HB,At21,At22),
    word(HC,At31,At32),

    word(VA,At11,At21,At31),
    word(VB,At12,At22,At32).
