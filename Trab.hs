
data Command x = LOD x | STO x | ADD x | SUB x | JMP x | JMZ x| CPE x| NOP x| HTL x deriving Show
data Registrador y = ACC y | PC y | EQZ y | RDM y | REM y deriving Show


cpu :: ([(Int,Int)],[(Registrador y,Int)]) -> ([(Int,Int)],[(Registrador y,Int)])
cpu ([(2,x)],(ACC y,z)) =  [(ACC y,h) | h <- snd (x,_)] --LOD
cpu ([(4,x)],(ACC y,z)) =  [(x,h) | h <- snd (ACC,_) ]--STO
cpu ([(6,x)],(ACC y,z)) =  Main ([(snd(x,_),snd(x,_))] (ACC,z)) --JMP
cpu ([(8,x)],(ACC y,z)) | snd(ACC y,_) == 0 = (PC y,x) --JMZ
                        | otherwise NOP
cpu ([(10,x)],(ACC y,z)) | z == snd(x,_) = (ACC y, 0) --CPE                   
                         | otherwise = (ACC y,1)  
cpu ([(14,x)],(ACC y,z)) =  [(ACC y,z+h) | h <- snd (x,_)] --ADD
cpu ([(16,x)],(ACC y,z)) =  [(ACC y,z-h) | h <- snd (x,_)] --SUB
cpu ([(18,x)],(ACC y,z)) =  (ACC y,0) --NOP
cpu ([(20,x)],(ACC y,z)) =  (PC y, -1) --HTL
