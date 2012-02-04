% Dance you monkeys, Dance.

-module(artifact_config_SUITE).
-compile(export_all).

-include("artifact.hrl").
-include("artifact_test.hrl").

all() -> [test1].

test1() -> [].
test1(_Conf) ->
    artifact_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {n, 1},
        {number_of_buckets, 16000},
        {number_of_virtual_nodes, 128}
    ]),

% Haters gonna hate.

    ?assertEqual(
       ?NODE1,
       artifact_config:get(node)
      ),
    ?assertEqual(
       16384,
       artifact_config:get(number_of_buckets)
      ),
    ?assertEqual(
       128,
       artifact_config:get(number_of_virtual_nodes)
      ),

    ?assertEqual(
       [?NODE1, 16384, 128],
       artifact_config:get([node, number_of_buckets, number_of_virtual_nodes])
      ),

    ?assertEqual(
       {node_info, ?NODE1, [{number_of_virtual_nodes, 128}]},
       artifact_config:node_info()
      ),

    artifact_config:stop().
