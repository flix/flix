///
/// The Ford-Fulkerson algorithm finds the maximum flow of a flow network.
/// Here it is implemented using a combination of functional programming
/// and datalog.
///
pub def main(): Unit \ IO =
    let g1 = FordFulkerson.exampleGraph01() |> FordFulkerson.maxFlow(0, 5);
    let g2 = FordFulkerson.exampleGraph02() |> FordFulkerson.maxFlow(0, 5);
    let g3 = FordFulkerson.exampleGraph03() |> FordFulkerson.maxFlow(0, 3);
    let g4 = FordFulkerson.
