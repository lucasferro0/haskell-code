import Data.List
import System.Exit (exitSuccess)
condition :: [String] -> IO()
condition lista = do
    if length(lista) > 0 then do
        putStrLn(head(lista))
        condition (tail(lista))
    else exibe lista

loop :: Int -> [String] -> IO()
loop n lista = do
    if n > 0 then do
        nome <- getLine
        let listaNova = addInLista nome lista
        loop (n-1) listaNova
    else do
        let listaOrdenada = sort(lista)
        condition listaOrdenada

addInLista :: String -> [String] -> [String]
addInLista nome lista = do
    lista ++ [nome] 

exibe :: [String] -> IO()
exibe lista = do
    number <- getLine
    let n = read number :: Int

    if n > 0 then do
        loop n lista
    else do
        exitSuccess

listaConvidados = []
main :: IO ()
main = do
    exibe listaConvidados
