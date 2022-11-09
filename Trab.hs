//import Data.Word
data Command x = LOD x | STO x | ADD x | SUB x | JMP x | JMZ x| CPE x| NOP x| HTL x deriving Show
data Registrador y = ACC y | PC y | EQZ y | RDM y | REM y | 
//newtype Inst = Word8 deriving Show


// Recebe a Lista de espaços de memória.
// Chama a ula um número de vezes igual ao número
// de instruções e passa o acumulador junto.
// Devolve o resultado.
Main :: ([(Int,Int)],(5,Registrador y)) -> ([(Int,Int)],(5,Registrador y))
Main ([(2,x)]),(5,y)) =  Main Ula ACC y LOD x
Main ([(4,x)]),(5,y)) =  Main Ula ACC y STO x 
Main ([(6,x)]),(5,y)) =  Main Ula ACC y JMP x 
Main ([(8,x)]),(5,y)) =  Main Ula ACC y JMZ x 
Main ([(10,x)]),(5,y)) =  Main Ula ACC y CPE x 
Main ([(14,x)]),(5,y)) =  Main Ula ACC y ADD x 
Main ([(16,x)]),(5,y)) =  Main Ula ACC y SUB x 
Main ([(18,x)]),(5,y)) =  Main Ula ACC y NOP x 
Main ([(20,x)]),(5,y)) =  Main Ula ACC HLT
