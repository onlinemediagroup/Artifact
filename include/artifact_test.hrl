-define(NODE1, {{127,0,0,1}, 11011}).
-define(NODE2, {{127,0,0,1}, 11012}).
-define(NODE3, {{127,0,0,1}, 11013}).
-define(NODE4, {{127,0,0,1}, 11014}).

-define(INFO, [{number_of_virtual_nodes, 2}]).

-define(assert(BoolExpr),
        ((fun() ->
            case (BoolExpr) of
                true -> ok;
                __V -> .erlang:error({assertion_failed,
                                      [{module, ?MODULE},
                                       {line, ?LINE},
                                       {expression, (??BoolExpr)},
                                       {expected, true},
                                       {value, case __V of
                                                   false -> __V;
                                                   _     -> {not_a_boolean,__V}
                                               end}]})
            end
          end)())).

-define(assertNot(BoolExpr), ?assert(not (BoolExpr))).

-define(assertEqual(Expect, Expr),
        ((fun(__X) ->
            case (Expr) of
                __X -> ok;
                __V -> .erlang:error({assertEqual_failed,
                                      [{module, ?MODULE},
                                       {line, ?LINE},
                                       {expression, (??Expr)},
                                       {expected, __X},
                                       {value, __V}]})
            end
          end)(Expect))).
