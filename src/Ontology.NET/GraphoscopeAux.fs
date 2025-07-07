module GraphoscopeAux


open System.Collections.Generic

open Graphoscope


module Algorithms =

    module DFS =

        let ofFGraph (starting : 'NodeKey) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let stack = Stack<'NodeKey>()

            stack.Push(starting)
            visited.Add(starting) |> ignore

            seq {
                while stack.Count > 0 do
                    let nodeKey = stack.Pop()
                    let (_, nd, s) = graph.[nodeKey]
                    yield (nodeKey, nd)

                    for kv in s do
                        if not(visited.Contains(kv.Key)) then
                            stack.Push(kv.Key)
                            visited.Add(kv.Key) |> ignore
            }


        let ofFGraphBy (starting : 'NodeKey) (predicate : 'NodeKey -> 'NodeData -> 'EdgeData -> bool) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let stack = Stack<'NodeKey>()

            stack.Push(starting)
            visited.Add(starting) |> ignore

            seq {
                while stack.Count > 0 do
                    let nodeKey = stack.Pop()
                    let (_, nd, s) = graph.[nodeKey]
                    yield (nodeKey, nd)

                    for kv in s do
                        let _, ndSuccessor, _ = graph[kv.Key]
                        if not (visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                            stack.Push(kv.Key)
                            visited.Add(kv.Key) |> ignore
            }


        let ofFGraphWithDepth (starting : 'NodeKey) depth (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let stack = Stack<'NodeKey * int>()

            stack.Push(starting,0)
            visited.Add(starting) |> ignore

            seq {
                while stack.Count > 0 do
                    let nodeKey, currDepth = stack.Pop()
                    let (_, nd, s) = graph.[nodeKey]
                    yield (nodeKey, nd)

                    if currDepth < depth then
                        for kv in s do
                            if not (visited.Contains(kv.Key)) then
                                stack.Push(kv.Key, currDepth + 1)
                                visited.Add(kv.Key) |> ignore
            }


    module BFS =

        let ofFGraphBy (starting : 'NodeKey) (predicate : 'NodeKey -> 'NodeData -> 'EdgeData -> bool) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let queue = Queue<'NodeKey>()

            queue.Enqueue(starting)
            visited.Add(starting) |> ignore
            seq {
                while queue.Count > 0 do
                    let nodeKey = queue.Dequeue()
                    let (_,nd,s) = graph.[nodeKey]
                    yield (nodeKey, nd)
                    for kv in s do
                        let _, ndSucc, _ = graph[kv.Key]
                        if not(visited.Contains(kv.Key)) && predicate kv.Key ndSucc s[kv.Key] then
                            queue.Enqueue(kv.Key)
                            visited.Add(kv.Key) |> ignore
            }


        let ofFGraphWithDepth (starting : 'NodeKey) depth (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let queue = Queue<'NodeKey * int>()

            queue.Enqueue(starting, 0)
            visited.Add(starting) |> ignore
            seq {
                while queue.Count > 0 do
                    let nodeKey, currDepth = queue.Dequeue()
                    let (_,nd,s) = graph.[nodeKey]
                    yield (nodeKey, nd)

                    if currDepth < depth then
                        for kv in s do
                            if not(visited.Contains(kv.Key)) then
                                queue.Enqueue(kv.Key, currDepth + 1)
            }


        let ofFGraphWithDepthBy (starting : 'NodeKey) depth (predicate : 'NodeKey -> 'NodeData -> 'EdgeData -> bool) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let queue = Queue<'NodeKey * int>()

            queue.Enqueue(starting, 0)
            visited.Add(starting) |> ignore
            seq {
                while queue.Count > 0 do
                    let nodeKey, currDepth = queue.Dequeue()
                    let (_,nd,s) = graph.[nodeKey]
                    yield (nodeKey, nd)

                    if currDepth < depth then
                        for kv in s do
                            let _, ndSucc, _ = graph[kv.Key]
                            if not(visited.Contains(kv.Key)) && predicate kv.Key ndSucc s[kv.Key] then
                                queue.Enqueue(kv.Key, currDepth + 1)
            }