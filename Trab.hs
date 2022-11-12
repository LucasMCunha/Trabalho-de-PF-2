import Data.Word
data Command x = LOD x | STO x | ADD x | SUB x | JMP x | JMZ x| CPE x| NOP x| HTL x deriving Show
data Registrador y = ACC y | PC y | EQZ y | RDM y | REM y  
--newtype Inst = Word8 deriving Show


-- Recebe a Lista de espaços de memória.
-- Chama a ula um número de vezes igual ao número
-- de instruções e passa o acumulador junto.
-- Devolve o resultado
cpu :: ([(Int,Int)],(Int,Registrador y)) -> ([(Int,Int)],(Int,Registrador y))
--cpu ([(2,2)], (5,y)) = ([(2,2)], (5,y))
cpu ([(2,x)],(5,y)) =  cpu ACC y LOD x
cpu ([(4,x)],(5,y)) =  cpu ACC y STO x 
cpu ([(6,x)],(5,y)) =  cpu ACC y JMP x 
cpu ([(8,x)],(5,y)) =  cpu ACC y JMZ x 
cpu ([(10,x)],(5,y)) =  cpu ACC y CPE x 
cpu ([(14,x)],(5,y)) =  cpu ACC y ADD x 
cpu ([(16,x)],(5,y)) =  cpu ACC y SUB x 
cpu ([(18,x)],(5,y)) =  cpu ACC y NOP x 
cpu ([(20,x)],(5,y)) =  cpu ACC HTL