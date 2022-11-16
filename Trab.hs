data Command w = LOD w | STO w | ADD w | SUB w | JMP w | JMZ w | CPE w | NOP w | HTL w deriving Show
data Registrador y = ACC y | PC y  deriving Show

starter :: ([(Int,Int)],[Registrador y]) -> ([(Int,Int)],[Registrador y])
starter (x,[(ACC y),(PC z)]) | z <= -1 = sum (0,1)
                             | otherwise = starter ((replaceNth z h x), regs)
						 where (h, regs) = cpu ((x !! z), [(ACC y), (PC z)])

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

cpu :: ((Int,Int),[Registrador y]) -> ((Int,Int),[Registrador y])
cpu ((2,a),[(ACC y),(PC z)]) = ula ((LOD a),[(ACC y),(PC z)])
cpu ((4,a),[(ACC y),(PC z)]) = ula ((STO a),[(ACC y),(PC z)])
cpu ((6,a),[(ACC y),(PC z)]) = ula ((JMP a),[(ACC y),(PC z)])
cpu ((8,a),[(ACC y),(PC z)]) = ula ((JMZ a),[(ACC y),(PC z)])
cpu ((10,a),[(ACC y),(PC z)]) = ula ((CPE a),[(ACC y),(PC z)])
cpu ((14,a),[(ACC y),(PC z)]) = ula ((ADD a),[(ACC y),(PC z)])
cpu ((16,a),[(ACC y),(PC z)]) = ula ((SUB a),[(ACC y),(PC z)])
cpu ((18,a),[(ACC y),(PC z)]) = ula ((NOP a),[(ACC y),(PC z)])
cpu ((20,a),[(ACC y),(PC z)]) = ula ((HTL a),[(ACC y),(PC z)])


ula :: ((Command w),[Registrador y]) -> ((Int,Int),[Registrador y]) --arrumar saida da ula para coincidir com a saida do cpu
ula ((LOD w),[(ACC y),(PC z)]) =  [(ACC h) | h <- snd (w,_)] --LOD
ula ((STO w),[(ACC y),(PC z)]) =  [(w,h) | h <- (ACC _) ]--STO
ula ((JMP w),[(ACC y),(PC z)])) =  (PC w) --JMP
ula ((JMZ w),[(ACC y),(PC z)]) | z == 0 = (PC w) --JMZ
                               | otherwise = NOP
ula ((CPE w),[(ACC y),(PC z)]) | y == snd(w,_) = (ACC 0) --CPE
                               | otherwise = (ACC 1)
ula ((ADD w),[(ACC y),(PC z)]) =  [(ACC y+h) | h <- snd (w,_)] --ADD
ula ((SUB w),[(ACC y),(PC z)]) =  [(ACC y-h) | h <- snd (w,_)] --SUB
ula ((NOP w),[(ACC y),(PC z)]) =  (ACC 0) --NOP
ula ((HTL w),[(ACC y),(PC z)]) =  (PC -1) --HTL

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
