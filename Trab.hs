data Command w = LOD Int | STO Int | ADD Int | SUB Int | JMP Int | JMZ Int | CPE Int | NOP | HTL deriving Show
data Registrador y = ACC Int | PC Int | VAL Int  deriving Show

starter :: ([(Int,Int)],[Registrador y]) -> ([(Int,Int)],[Registrador y])
starter (x,[(ACC y),(PC z),(VAL v)]) | z <= -1 = sum (0,1)
                                     | otherwise = starter ((replaceNth z h x), regs)
						 where (h, regs) = cpu ((x !! z), [(ACC y), (PC z), (VAL (snd(x !! snd (x !! z)))])

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

cpu :: ((Int,Int),[Registrador y]) -> ((Int,Int),[Registrador y])
cpu ((2,a),[(ACC y),(PC z),(VAL v)]) = ula ((LOD a),[(ACC y),(PC z),(VAL v)])
cpu ((4,a),[(ACC y),(PC z),(VAL v)]) = ula ((STO a),[(ACC y),(PC z),(VAL v)])
cpu ((6,a),[(ACC y),(PC z),(VAL v)]) = ula ((JMP a),[(ACC y),(PC z),(VAL v)])
cpu ((8,a),[(ACC y),(PC z),(VAL v)]) = ula ((JMZ a),[(ACC y),(PC z),(VAL v)])
cpu ((10,a),[(ACC y),(PC z),(VAL v)]) = ula ((CPE a),[(ACC y),(PC z),(VAL v)])
cpu ((14,a),[(ACC y),(PC z),(VAL v)]) = ula ((ADD a),[(ACC y),(PC z),(VAL v)])
cpu ((16,a),[(ACC y),(PC z),(VAL v)]) = ula ((SUB a),[(ACC y),(PC z),(VAL v)])
cpu ((18,a),[(ACC y),(PC z),(VAL v)]) = ula ((NOP a),[(ACC y),(PC z),(VAL v)])
cpu ((20,a),[(ACC y),(PC z),(VAL v)]) = ula ((HTL a),[(ACC y),(PC z),(VAL v)])


ula :: ((Command w),[Registrador y]) -> ((Int,Int),[Registrador y]) --arrumar saida da ula para coincidir com a saida do cpu
ula ((LOD w),[(ACC y),(PC z),(VAL v)]) =  [(ACC h) | h <- v] --LOD
ula ((STO w),[(ACC y),(PC z),(VAL v)]) =  [(w,h) | h <- (ACC _) ]--STO
ula ((JMP w),[(ACC y),(PC z),(VAL v)])) =  (PC w) --JMP
ula ((JMZ w),[(ACC y),(PC z),(VAL v)]) | z == 0 = (PC w) --JMZ
                                       | otherwise = NOP
ula ((CPE w),[(ACC y),(PC z),(VAL v)]) | y == v = (ACC 0) --CPE
                                       | otherwise = (ACC 1)
ula ((ADD w),[(ACC y),(PC z),(VAL v)]) =  [(ACC y+h) | h <- v] --ADD
ula ((SUB w),[(ACC y),(PC z),(VAL v)]) =  [(ACC y-h) | h <- v] --SUB
ula ((NOP w),[(ACC y),(PC z),(VAL v)]) =  (ACC 0) --NOP
ula ((HTL w),[(ACC y),(PC z),(VAL v)]) =  (PC -1) --HTL

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
