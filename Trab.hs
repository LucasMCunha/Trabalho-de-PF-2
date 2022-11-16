data Command w = LOD w | STO w | ADD w | SUB w | JMP w | JMZ w | CPE w | NOP w | HTL w deriving Show
data Registrador y = ACC y | PC y  deriving Show

starter :: ([(Int,Int)],[(Registrador y,Int)]) -> ([(Int,Int)],[(Registrador y,Int)])
starter (x,[(ACC y,z),(PC a, b)]) | b <= -1 = sum (0,1)
                                  | otherwise = starter ((replaceNth b h x), regs)
								    where (h, regs) = cpu ((x !! b), [(ACC y, z), (PC a, b)])

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

cpu :: ((Int,Int),[(Registrador y,Int)]) -> ((Int,Int),[(Registrador y,Int)])
cpu ((2,x),[(ACC y,z),(PC a,b)]) = ula ((LOD x,0),[(ACC y,z), (PC a, b)])
cpu ((4,x),[(ACC y,z),(PC a,b)]) = ula ((STO x,0),[(ACC y,z), (PC a, b)])
cpu ((6,x),[(ACC y,z),(PC a,b)]) = ula ((JMP x,0),[(ACC y,z), (PC a,b)])
cpu ((8,x),[(ACC y,z),(PC a,b)]) = ula ((JMZ x,0),[(ACC y,z), (PC a,b)])
cpu ((10,x),[(ACC y,z),(PC a,b)]) = ula ((CPE x,0),[(ACC y,z), (PC a, b)])
cpu ((14,x),[(ACC y,z),(PC a,b)]) = ula ((ADD x,0),[(ACC y,z), (PC a, b)])
cpu ((16,x),[(ACC y,z),(PC a,b)]) = ula ((SUB x,0),[(ACC y,z), (PC a, b)])
cpu ((18,x),[(ACC y,z),(PC a,b)]) = ula ((NOP x,0),[(ACC y,z), (PC a,b)])
cpu ((20,x),[(ACC y,z),(PC a,b)]) = ula ((HTL x,0),[(ACC y,z), (PC a,b)])
ula :: ((Command x,Int),[(Registrador y,Int)]) -> ((Int,Int),[(Registrador y,Int)]) --arrumar saida da ula para coincidir com a saida do cpu
ula ((LOD x,_),[(ACC y,z), (PC a, b)]) =  [(ACC y,h) | h <- snd (x,_)] --LOD
ula ((STO x,_),[(ACC y,z),(PC a, b)]) =  [(x,h) | h <- snd (ACC,_) ]--STO
ula ((JMP x,_),[(ACC y,z),(PC a, b)]) =  (PC y,x) --JMP
ula ((JMZ x,_),[(ACC y,z),(PC a,b)]) | z == 0 = (PC y,x) --JMZ
                                   | otherwise = NOP
ula ((CPE x,_),[(ACC y,z), (PC a, b)]) | z == snd(x,_) = (ACC y, 0) --CPE
                                     | otherwise = (ACC y,1)
ula ((ADD x,_),[(ACC y,z), (PC a, b)]) =  [(ACC y,z+h) | h <- snd (x,_)] --ADD
ula ((SUB x,_),[(ACC y,z), (PC a, b)]) =  [(ACC y,z-h) | h <- snd (x,_)] --SUB
ula ((NOP x,_),[(ACC y,z), (PC a, b)]) =  (ACC y,0) --NOP
ula ((HTL x,_),[(ACC y,z), (PC a, b)]) =  (PC y, -1) --HTL

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
