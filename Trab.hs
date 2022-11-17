data Command w = LOD Int | STO Int | ADD Int | SUB Int | JMP Int | JMZ Int | CPE Int | NOP | HTL deriving Show
data Registrador y = ACC Int | PC Int   deriving Show

starter :: ([(Int,Int)],[Registrador y]) -> ([(Int,Int)],[Registrador y])
starter (x,[(ACC y),(PC z)]) | z <= -1 = (x,[(ACC y),(PC z)])
                                     | otherwise = starter cpu ((x !! z), x, [(ACC y), (PC z)])

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

removemaybe :: Maybe (Int,Int) -> (Int,Int)
removemaybe Nothing = (0,0)
removemaybe Just (i,j) = (i,j)

cpu :: ((Int,Int),[(Int,Int)],[Registrador y]) -> ([(Int,Int)],[Registrador y])
cpu ((2,a),x,[(ACC y),(PC z)]) = ((replaceNth (z fst(ula ((LOD a),[(ACC y),(PC z)]))) x), snd(ula ((LOD a),[(ACC y),(PC z)])))
cpu ((4,a),x,[(ACC y),(PC z)]) = ((replaceNth (removemaybe(findIndex ((a ==) . fst)) fst(ula ((STO a),[(ACC y),(PC z)]))) x), snd(ula ((STO a),[(ACC y),(PC z)])))
cpu ((6,a),x,[(ACC y),(PC z)]) = ((replaceNth (z fst(ula ((JMP a),[(ACC y),(PC z)]))) x), snd(ula ((JMP a),[(ACC y),(PC z)])))
cpu ((8,a),x,[(ACC y),(PC z)]) = ((replaceNth (z fst(ula ((JMZ a),[(ACC y),(PC z)]))) x), snd(ula ((JMZ a),[(ACC y),(PC z)])))
cpu ((10,a),x,[(ACC y),(PC z)]) = ((replaceNth (z fst(ula ((CPE a),[(ACC y),(PC z)]))) x), snd(ula ((CPE a),[(ACC y),(PC z)])))
cpu ((14,a),x,[(ACC y),(PC z)]) = ((replaceNth (z fst(ula ((ADD a),[(ACC y),(PC z)]))) x), snd(ula ((ADD a),[(ACC y),(PC z)])))
cpu ((16,a),x,[(ACC y),(PC z)]) = ((replaceNth (z fst(ula ((SUB a),[(ACC y),(PC z)]))) x), snd(ula ((SUB a),[(ACC y),(PC z)])))
cpu ((18,a),x,[(ACC y),(PC z)]) = ((replaceNth (z fst(ula ((NOP a),[(ACC y),(PC z)]))) x), snd(ula ((NOP a),[(ACC y),(PC z)])))
cpu ((20,a),x,[(ACC y),(PC z)]) = ((replaceNth (z fst(ula ((HTL a),[(ACC y),(PC z)]))) x), snd(ula ((HTL a),[(ACC y),(PC z)])))


ula :: ((Command w),[Registrador y]) -> ((Int,Int),[Registrador y]) --arrumar saida da ula para coincidir com a saida do cpu
ula ((LOD w),[(ACC y),(PC z)]) = ((2,w), [(ACC v), (PC (z+1))]) --LOD
ula ((STO w),[(ACC y),(PC z)]) =  ((w,y), [(ACC y), (PC (z+1))])  --STO
ula ((JMP w),[(ACC y),(PC z)]) =  ((6,w),[(ACC y),(PC w)]) --JMP
ula ((JMZ w),[(ACC y),(PC z)]) | y == 0 = ((8,w),[(ACC y),(PC w)]) --JMZ
                                       | otherwise = ula ((NOP),[(ACC y),(PC z)])
ula ((CPE w),[(ACC y),(PC z)]) | y == v = ((10,w), [(ACC 0), (PC (z+1))]) --CPE
                                       | otherwise = ((10,w), [(ACC 1), (PC (z+1))])
ula ((ADD w),[(ACC y),(PC z)]) =  ((14,w), [(ACC (y+v)), (PC (z+1))]) --ADD
ula ((SUB w),[(ACC y),(PC z)]) =  ((16,w), [(ACC (y-v)), (PC (z+1))])  --SUB
ula ((NOP),[(ACC y),(PC z)]) =  ((18,0), [(ACC 0), (PC (z+1))]) --NOP
ula ((HTL),[(ACC y),(PC z)]) =  ((20,0),[(ACC y),(PC (-1))]) --HTL

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
