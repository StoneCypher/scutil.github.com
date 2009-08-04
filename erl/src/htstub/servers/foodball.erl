
-module(foodball).





-export([

    where_food/0,
      where_food/1,
      where_food/2,

    foodlist/0,
      foodlist/1     % in foodball_foodlist.erl_frag

]).





-include("foodball_foodlist.erl_frag").





foodlist() ->

    case erlang:time() of

        { Y,  _, _ } when Y >= 11 -> foodlist(11);
        { 10, X, _ } when X >= 50 -> foodlist(11);

        { 10, X, _ } when X >= 20 -> foodlist(10.5);

        { 10, _, _ }              -> foodlist(10);
        { 9,  X, _ } when X >= 50 -> foodlist(10);

        _                         -> foodlist(9)

    end.





where_food()                                -> where_food(80,   foodlist()).
where_food(Port)     when is_integer(Port)  -> where_food(Port, foodlist());
where_food(FoodList) when is_list(FoodList) -> where_food(80,   FoodList).



where_food(Port, FoodList) ->

    HtmlStart = "<html><head><title>WHERE FOOD</title><style>body{margin:0;padding:2em 3em;background-color:#f0f8ff}</style></head><body><p>And the magic 8-ball says...</p><h1>",
    HtmlMid   = "</h1><div style=\"position:absolute;bottom:2em;right:3em;\">Viewed ",
    HtmlStop  = " times since boot</div></body></html>",

    sc_counter:reset(wherefood),

    Server = fun(_,_,_) ->
        {A,B,C} = now(),
        random:seed(A,B,C),
        HtmlStart ++ sc_random:from(FoodList) ++ HtmlMid ++ integer_to_list(sc_counter:inc(wherefood)) ++ HtmlStop
    end,

    htstub:serve(Port, Server).
