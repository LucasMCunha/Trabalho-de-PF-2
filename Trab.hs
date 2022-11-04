//import Data.Word
data Command x = LOD x | STO x | ADD x | SUB x | JMP x | JMZ x| CPE x| NOP x| HTL x deriving Show
data Registrador y = ACC y | PC y | EQZ y | RDM y | REM y | 
//newtype Inst = Word8 deriving Show

bin2dec :: [Int] -> Int
bin2dec [0] = 0
bin2dec [1] = 1
bin2dec (x:xs) = (x * 2^(length xs)) + bin2dec xs

Conversor :: ([Int],[Int]) -> [(Int,Int)]


// Recebe a Lista de espaços de memória.
// Chama a ula um número de vezes igual ao número
// de instruções e passa o acumulador junto.
// Devolve o resultado.
Main :: [(Int,Int)] -> [(Int,Int)]
Main (2,x) =  Main Ula ACC y LOD x
Main (4,x) =  Main Ula ACC y STO x 
Main (6,x) =  Main Ula ACC y JMP x 
Main (8,x) =  Main Ula ACC y JMZ x 
Main (10,x) =  Main Ula ACC y CPE x 
Main (14,x) =  Main Ula ACC y ADD x 
Main (16,x) =  Main Ula ACC y SUB x 
Main (18,x) =  Main Ula ACC y NOP x 
Main (20,x) =  Main Ula ACC HLT

Ula :: Int -> Command x -> Int -> [(Int,Int)]
