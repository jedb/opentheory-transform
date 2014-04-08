multiCommands :: PGraph -> UsageMap -> [Node] -> PGraph
multiCommands graph usemap nodeList =
    let multiNodes = filter (\x -> nodeOutput graph x > 1) (Graph.nodes graph)
        umap = Map.filterWithKey (\n _ -> n `elem` multiNodes) usemap

        before = (\gr node edgemap arg indexList ->
                let edges = filter (\x -> snd . thd3 . fst $ x < arg) edgemap

                    -- sorts and groups by which output of the command each edge is using
                    sorted = sortBy (\(a,b) (c,d) -> compare (snd . thd3 $ a) (snd . thd3 $ c)) edges
                    grouped = groupBy (\x y -> snd . thd3 . fst $ x == snd . thd3 . fst $ y) sorted

                    -- makes a list of pairs of (maximum, restOfList)
                    maxSplit = map (\x -> partition (\y -> y == maximumBy useSort x)) grouped

                    refNodeEdges = map (fst . snd) maxSplit
                    removeNodeEdges = concat (map (fst . fst) maxSplit)

                    usedArgs = filter (\x -> x `elem` (map (snd . thd3) removeNodeEdges)) [1..(arg-1)]

                    -- creates a graphpart to define and pop all the initial outputs to get to the used one in the middle
                    defGen = (\num -> 
                            if (num == arg)
                            then []
                            else if (index!!num `elem` usedArgs)
                                 then [index!!num, "def", "pop"] ++ defGen (num+1)
                                 else ["pop"] ++ defGen (num+1))
                    defPart = genPart (defGen 0) True

                    -- creates graphparts for removing all the items stored in the dictionary, including node attachments
                    removeList = zip usedArgs removeNodeEdges
                    removePart = map (\(x,y) -> (genPart [index!!(x-1), "remove"] False, Nothing, [edgeToNode y])) removeList

                    -- creates graphparts to reference all the items stored in the dictionary, including node attachments
                    refList = zip usedArgs refNodeEdges
                    refPart = map (\(x,y) -> (genPart [index!!(x-1), "ref"] False, Nothing, (map edgeToNode y))) refList

                in (defPart, refPart ++ removePart))


        after = (\gr node edgemap arg indexList ->
                let -- obtain edges after the cutoff argument
                    edges = filter (\x -> snd . thd3 . fst $ x > arg) edgemap

                    -- sort and group by which output of the command each edge is using
                    sorted = sortBy (\(a,b) (c,d) -> compare (snd . thd3 $ a) (snd . thd3 $ c)) edges
                    grouped = groupBy (\x y -> snd . thd3 . fst $ x == snd . thd3 . fst $ y) sorted

                    mins = map (minimumBy useSort) grouped
                    initEdge = minimumBy useSort (Map.toList edgemap)

                    usedArgs = filter (\x -> x `elem` (map (snd . thd3 . fst) mins)) [(arg+1)..(nodeOutput gr node)]
                    edgeToNode = (\x -> (fst3 x, fst . thd3 $ x))

                    -- finds the argument where you have to pop everything and store it all in the dictionary before
                    -- proceeding
                    findAttach = (\x y ->
                            if (x == [])
                            then nodeOutput gr node
                            else let allZero = all (=0) (snd . snd . head $ x)
                                     headIsMin = (head x) == (minimumBy useSort x)
                                     headThmLowestStrict = let testList = map (fst . snd) x
                                                           in all (> head testList) (tail testList)
                                     nextUsedArg = snd . thd3 . fst . head . tail $ x
                                 in if (allZero && headIsMin && headThmLowestStrict)
                                    then findAttach (tail x) nextUsedArg
                                    else y)

                    argToAttach = findAttach initEdge:mins arg

                    process = (\attach curArg modp ordp ->
                        case (compare arg argToAttach) of
                            LT ->
                            EQ ->
                            GT ->)

                    (modParts, ordinaryParts) = process argToAttach arg [] []

                    -- calculate the def/pop/ref defPart
                    afterPartInit = 
                    afterPart = 
                        if (argToAttach == arg)
                        then
                        else

                    -- calculate def nodes/parts for outputs before the argToAttach
                    defs =
                    makeDefList =
                    defPart = map (\(x,y) -> (genPart [index!!(x-1), "def"] False, Nothing, [edgeToNode y])) makeDefList

                    -- calculate ref and remove nodes/parts
                    maxes = map (maximumBy useSort) grouped
                    refs = map (filter (\x -> x `notElem` maxes && x `notElem` defs)) grouped

                    removeList = zip usedArgs maxes
                    removePart = map (\(x,y) -> (genPart [index!!(x-1), "remove"] False, Nothing, [edgeToNode y])) removeList

                    -- creates graphparts to reference all the items stored in the dictionary, including node attachments
                    refList = zip usedArgs refs
                    refPart = map (\(x,y) -> (genPart [index!!(x-1), "ref"] False, Nothing, (map edgeToNode y))) refList

                in (modParts, ordinaryParts))

        addPreserveNodeParts = (\partList graph ->
            )

        f = (\gr node edgemap ->
                let edgeList = Map.toList edgemap

                    out = nodeOutput gr node
                    index = next (out + 1) gr

                    initEdge = fst (minimumBy useSort edgeList)
                    initArg = snd . thd3 $ initEdge
                    (defBefore, beforeParts) = before gr node edgemap initArg (take (initArg-1) index)
                    (defAfter, afterParts) = after gr node edgemap initArg (drop initArg index)
                    edgesToRemove = filter (\x -> x /= initEdge) (map fst edgeList)

                    gr' = addPreserveNodeParts defAfter gr

                    edgesRemoved = foldl' (\x y -> Graph.delLEdge y x) gr' edgesToRemove
                    partsAdded = graphAddList partList edgesRemoved
                in partsAdded)

    in foldl' (\g n -> f g n (fromJust (Map.lookup n umap))) graph multiNodes













multiCommands :: PGraph -> PGraph -> UsageMap -> [Node] -> PGraph
multiCommands graph orig usemap nodeList =
    let process = (\gr stack dict workmap edge ->
                let node = snd3 edge
                    label = fromJust (Graph.lab gr node)
                in if (label == "def" || label == "ref" || label == "remove" || isNumber label)
                   then dictProcess gr stack dict workmap edge
                   else regProcess gr stack dict workmap edge


        dictProcess = (\gr stack dict workmap edge ->
                let node = snd3 edge
                    label = fromJust (Graph.lab gr node)
                    index = fromJust (Graph.lab gr (head (Graph.suc gr node)))

                in if (label == "def")
                   then let dict' = Map.insert index (head stack) dict
                        in (gr, stack, dict', workmap)

                   else if (label == "ref")
                        then let stack' = (fromJust (Map.lookup index dict)):stack
                             in (gr, stack', dict, workmap)

                        else if (label == "remove")
                             then let stack' = (fromJust (Map.lookup index dict)):stack
                                      dict' = Map.delete index dict
                                  in (gr, stack', dict', workmap)
                             else -- isNumber label
                                  (gr, stack, dict, workmap)


        regProcess = (\gr stack dict workmap edge ->
                let node = snd3 edge
                    label = fromJust (Graph.lab gr node)

                    io = argMap label
                    sortedIns = sortBy (\x y -> compare (fst . thd3 $ x) (fst . thd3 $ y)) (Graph.out orig node)
                    expectedInput = map (\(a,b,(c,d)) -> (b,d)) sortedIns

                    consume = (\(g,s,d,w) inList ->
                        if (inList == [])
                        then if (nodeOutput == 1)
                             then (g, (node,1):s, d, w)
                             else initial (g,s,d,w) 
                        else let i = head inList
                             in if (head s == i)
                                then consume (g, tail s, d, w) (tail inList)
                                else store (g, s, d, w) inList)

                    initial = (\(g,s,d,w) inList ->
                        let edgemap = Map.toList (fromJust (Map.lookup node usemap))
                            sorted = sortBy (\(a,b) (c,d) -> compare (snd . thd3 $ a) (snd . thd3 $ b)) edgemap
                            grouped = groupBy (\x y -> (snd . thd3 . fst $ x) == (snd . thd3 . fst $ y)) sorted
                            minimals = map (minimumBy useSort) grouped
                            usedArgs = filter (\x -> x `elem` (map (snd . thd3 . fst) minimals)) [1..nodeOutput]
                            atArg = snd . thd3 $ edge
                            atArgReuse = length (filter (\x -> (snd . thd3 . fst $ x) == atArg) edgemap)
                            fromStart = fst . snd $ (head (filter (\x -> ((snd . thd3 . fst $ x) == atArg)) minimals))
                            edgesToRemove = filter (\x -> (snd . thd3 $ x) < upTo) (map fst edgemap)

                            upTo = let shortList = filter (\x -> (snd . thd3 . fst $ x) > atArg && (fst . snd $ x) > fromStart) minimals
                                   in if (shortList == [])
                                      then nodeOutput + 1
                                      else let shortNum = snd . thd3 . fst . head $ shortList
                                               calc = (\num ->
                                                    if (filter (\x -> (snd . thd3 . fst $ x) == num - 1) edgemap == [])
                                                    then calc (num - 1)
                                                    else num)
                                               in calc shortNum
                            index = next upTo g

                            defPartGen = (\num ->
                                if (num == upTo)
                                then if (atArg + 1 < upTo)
                                     then if (atArgReuse > 1)
                                          then [index!!atArg, "ref"]
                                          else [index!!atArg, "remove"]
                                     else []
                                else if (num `elem` usedArgs)
                                     then if (num + 1 == atArg &&) 
                                     else if (num == atArg)
                                          then if (atArgReuse <= 1 && atArg + 1 == upTo)
                                               then defPartGen (num+1)
                                               else if (atArg + 1 < upTo)
                                                    then [index!!num, "def", "pop"] ++ (defPartGen (num+1))
                                                    else [index!!num, "def"] ++ (defPartGen (num+1))
                                          else [index!!num, "def", "pop"] ++ (defPartGen (num+1))
                                     else ["pop"] ++ (defPartGen (num+1)))
                            defPart = genPart (defPartGen 1) True

                            maxSplit = map (\x -> partition (\y -> y == maximumBy useSort x)) grouped
                            refNodeEdges = map (fst . snd) maxSplit
                            removeNodeEdges = concat (map (fst . fst) maxSplit)

                            removeList = zip usedArgs removeNodeEdges
                            removePart = map (\(x,y) -> (genPart [index!!(x-1), "remove"] False, Nothing, [edgeToNode y])) removeList

                            refList = zip usedArgs refNodeEdges
                            refPart = map (\(x,y) -> (genPart [index!!(x-1), "ref"] False, Nothing, (map edgeToNode y))) refPart

                            workingEdge =
                                let atArgEdges = filter (\x -> (snd . thd3 . fst $ x) == atArg) edgemap
                                    initEdge = fst . head $ (filter (\x -> (snd . thd3 $ x) == atArg &&
                                                                           (x `notElem` (delete (minimumBy useSort atArgEdges)
                                                                                                atArgEdges))) (Graph.inn g' node))
                                    calc = (\e ->
                                        if (fst3 e == fst3 edge)
                                        then e
                                        else calc (head (Graph.inn g' (fst3 e))))
                                in calc initEdge
                            w' = Map.insert node workingEdge

                            storedArgs = if (atArgReuse > 1 || atArg + 1 < upTo)
                                         then filter (< upTo) usedArgs
                                         else delete atArg (filter (< upTo) usedArgs)
                            dictAddList = map (\x -> (index!!(x-1), (node,x))) storedArgs
                            d' = foldl' (\x (y,z) -> Map.insert y z x) d dictAddList

                            stackArgs = atArg:(filter (>= upTo) usedArgs)
                            stackAddList = map (\x -> (node,x)) stackArgs
                            s' = stackArgs ++ s

                            edgesRemoved = foldl' (\x (y,z) -> Graph.delLEdge y x) g edgesToRemove
                            g' = graphAddList (defPart:(refPart ++ removePart)) edgesRemoved

                        in (g', s', d', w'))


                    store = (\(g,s,d,w) inList ->
                        let s' = tail s
                            (node, arg) = head s -- the thing on the stack that shouldnt be there
                            workEdge = Map.lookup node w
                            (reqNode, reqArg) = head inList -- what we want on the stack instead

                            index = head (next 1 g)
                            edgemap = Map.toList (Map.lookup node usemap) -- map of the edges leading into the node
                            edgesOfArg = filter (\(x,y) -> (snd . thd3 $ x) == arg) edgemap -- edges using the arg we want to get rid of
                            
                            removeEdge = maximumBy useSort edgesOfArg
                            refEdgeList = delete removeEdge edgesOfArg

                            defPart = genPart [index, "def"] True
                            refPart = genPart [index, "ref"] False
                            removePart = genPart [index, "remove" False
                            popPart = genPart ["pop"] True

                        in consume (g', s', d', w') inList

                in consume (gr,stack,dict,workmap) expectedInput)


        h = (\gr st di ma edge ->
                let node = snd3 edge
                    (gr',st',di',ma') = f gr st di ma node
                in process gr' st' di' ma' edge)

        f = (\gr st di ma no ->
                let args = reverse [1..(nodeOutput gr no)]
                    func = (\(g,s,d,m) a ->
                        let edge = filter (\x -> fst . thd3 $ x == a) (Graph.out g no)
                        in if (edge == [])
                           then (g,s,d,m)
                           else h gr st di ma (head edge)
                in foldl' func (gr,st,di,ma) args

        stack = []
        dictionary = Map.empty
        workmap = Map.empty

        (graph',stack',dictionary',workmap') =
            foldl' (\(g,s,d,m) n -> f g s d m n) (graph, stack, dictionary, workmap) nodeList

    in graph'



    