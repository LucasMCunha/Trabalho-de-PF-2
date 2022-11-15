data Command w = LOD w | STO w | ADD w | SUB w | JMP w | JMZ w | CPE w | NOP w | HTL w deriving Show
data Registrador y = ACC y | PC y  deriving Show

starter :: ([(Int,Int)],[(Registrador y,Int)]) -> ([(Int,Int)],[(Registrador y,Int)])
starter (x,[(ACC y,z),(PC a, b)]) | b =< -1 = sum (0,1)
                                  | otherwise = starter (x, [(ACC y,z), (PC a, b)]) | (find(==(b,_)) x) <- fst (cpu (find(==(b,_)) x), ([(ACC y,z),(PC a, b)]))
                     --             | otherwise = ((takeWhile == b) x  cpu((i,k),[(ACC y,z),(PC a, b)]) | (i,k) <- (takeWhile == b) x)


cpu :: ((Int,Int),[(Registrador y,Int)]) -> ((Int,Int),[(Registrador y,Int)])
cpu ((2,x),[(ACC y,z),(PC a,b)]) = cpu ula ((LOD w,x),[(ACC y,z), (PC a, b)])
cpu ((4,x),[(ACC y,z),(PC a,b)]) = cpu ula ((STO w,x),[(ACC y,z), (PC a, b)])
cpu ((6,x),[(ACC y,z),(PC y,z)]) = cpu ula ((JMP w,x),[(ACC y,z), (PC y,z)])
cpu ((8,x),[(ACC y,z),(PC a,b)]) = cpu ula ((JMZ w,x),[(ACC y,z), (PC a,b)])
cpu ((10,x),[(ACC y,z),(PC a,b)]) = cpu ula ((CPE w,x),[(ACC y,z), (PC a, b)])
cpu ((14,x),[(ACC y,z),(PC a,b)]) = cpu ula ((ADD w,x),[(ACC y,z), (PC a, b)])
cpu ((16,x),[(ACC y,z),(PC a,b)]) = cpu ula ((SUB w,x),[(ACC y,z), (PC a, b)])
cpu ((18,x),[(ACC y,z),(PC y,z)]) = cpu ula ((NOP w,x),[(ACC y,z), (PC y,z)])
cpu ((20,x),[(ACC y,z),(PC y,z)]) = cpu ula ((HTL w,x),[(ACC y,z), (PC y,z)])

ula :: ((Command x,Int),[(Registrador y,Int)]) -> ((Int,Int),[(Registrador y,Int)]) --arrumar saida da ula para coincidir com a saida do cpu
ula ((ADD w,x),[(ACC y,z), (PC a, b)]) =  [(ACC y,h) | h <- snd (x,_)] --LOD
ula ((STO w,x),[(ACC y,z),(PC a, b)]) =  [(x,h) | h <- snd (ACC,_) ]--STO
ula ((JMP w,x),[(ACC y,z),(PC a, b)]) =  (PC y,x) --JMP
ula ((JMZ,x),[(ACC y,z),(PC a,b)]) | z == 0 = (PC y,x) --JMZ
                                   | otherwise NOP
ula ((CPE,x),[(ACC y,z), (PC a, b)]) | z == snd(x,_) = (ACC y, 0) --CPE                   
                                     | otherwise = (ACC y,1)  
ula ((ADD,x),[(ACC y,z), (PC a, b)]) =  [(ACC y,z+h) | h <- snd (x,_)] --ADD
ula ((SUB,x),[(ACC y,z), (PC a, b)]) =  [(ACC y,z-h) | h <- snd (x,_)] --SUB
ula ((NOP,x),[(PC y,z), (PC a, b)]) =  (ACC y,0) --NOP
ula ((HTL,x),[(PC y,z), (PC a, b)]) =  (PC y, -1) --HTL

--cpu :: ([(Int,Int)],[(Registrador y,Int)]) -> ([(Int,Int)],[(Registrador y,Int)])
--cpu ([(2,x)],(ACC y,z)) =  [(ACC y,h) | h <- snd (x,_)] --LOD
--cpu ([(4,x)],(ACC y,z)) =  [(x,h) | h <- snd (ACC,_) ]--STO
--cpu ([(6,x)],(ACC y,z)) =  (PC y,x) --JMP
--cpu ([(8,x)],(ACC y,z)) | snd(ACC y,_) == 0 = (PC y,x) --JMZ
--                        | otherwise NOP
--cpu ([(10,x)],(ACC y,z)) | z == snd(x,_) = (ACC y, 0) --CPE                   
--                         | otherwise = (ACC y,1)  
--cpu ([(14,x)],(ACC y,z)) =  [(ACC y,z+h) | h <- snd (x,_)] --ADD
--cpu ([(16,x)],(ACC y,z)) =  [(ACC y,z-h) | h <- snd (x,_)] --SUB
--cpu ([(18,x)],(ACC y,z)) =  (ACC y,0) --NOP
--cpu ([(20,x)],(ACC y,z)) =  (PC y, -1) --HTL
