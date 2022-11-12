
data Command x = LOD x | STO x | ADD x | SUB x | JMP x | JMZ x| CPE x| NOP x| HTL x deriving Show
data Registrador y = ACC y | PC y | EQZ y | RDM y | REM y 


Main :: ([(Int,Int)],(Registrador y,Int) -> ([(Int,Int)],(Registrador y,Int))
Main ([(2,x)]),(y,z)) =  Main Ula ACC y LOD x
Main ([(4,x)]),(y,z)) =  Main Ula ACC y STO x 
Main ([(6,x)]),(y,z)) =  Main Ula ACC y JMP x 
Main ([(8,x)]),(y,z)) =  Main Ula ACC y JMZ x 
Main ([(10,x)]),(y,z)) =  Main Ula ACC y CPE x 
Main ([(14,x)]),(ACC,z)) =  (ACC,z+h) | h <- snd (x,_)
Main ([(16,x)]),(y,z)) =  Main Ula ACC y SUB x 
Main ([(18,x)]),(y,z)) =  Main Ula ACC y NOP x 
Main ([(20,x)]),(y,z)) =  Main Ula ACC HLT
