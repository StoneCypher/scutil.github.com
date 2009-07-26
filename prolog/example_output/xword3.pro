
% add_to_path('c:\\projects\\private\\xword\\').
% [xword3].

word(pots, p, o, t, s).
word(oop, o, o, p).
word(lom, l, o, m).
word(epic, e, p, i, c).
word(apple, a, p, p, l, e).
word(tmi, t, m, i).
word(car, c, a, r).





make(A1, A3, A6, A7, D2, D4, D5) :-

    word(A1,            _, VV_0002_0001,            _),
    word(A3, VV_0002_0003, VV_0003_0003, VV_0004_0003,            _),
    word(A6, VV_0002_0004, VV_0003_0004, VV_0004_0004),
    word(A7, VV_0002_0005, VV_0003_0005, VV_0004_0005,            _),
    word(D2, VV_0002_0001,            _, VV_0002_0003, VV_0002_0004, VV_0002_0005),
    word(D4, VV_0003_0003, VV_0003_0004, VV_0003_0005),
    word(D5, VV_0004_0003, VV_0004_0004, VV_0004_0005).





go({A1, A3, A6, A7, D2, D4, D5}) :- X = make(A1, A3, A6, A7, D2, D4, D5).
