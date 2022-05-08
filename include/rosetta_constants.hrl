
-define(RE_WHITE_SPACE, [<<"\\s+">>, <<"\\\\n">>] ).

-define(RE_STATE_REMOVE, [<<"state\"">>, <<">>">>] ).

-define(RE_NOTE_REMOVE, [<<"note\"">>, <<"fsmname:">>] ).

-define(RE_STATE_REPLACE, [<<"\"as">>, <<"<<package:">>, <<"<<class:">>] ).

-define(RE_NODE_REPLACE, [<<"<<([^<]*)>">>, <<"-([^-]*)->">>, <<":">>] ).

-define(RE_NOTE_REPLACE, [<<"\"as">>, <<"fsmtemplate:">>, <<"fsmpackage:">>, <<"fsmclass:">>] ).

-define(NOTHING, <<"">> ).

-define(COMMA, <<",">> ).