import System.Exit (exitSuccess)
import Data.List (sort)
import Data.Char (toUpper, toLower)

format :: Float -> Int -> Float
format x n = 
    (fromIntegral (floor (x * t))) / t
    where t = 10^n


split :: Eq a => [a] -> [a] -> [[a]]
split x y = func x y [[]]
    where
        func x [] z = reverse $ map (reverse) z
        func x (y:ys) (z:zs) = if (take (length x) (y:ys)) == x then
            func x (drop (length x) (y:ys)) ([]:(z:zs))
        else
            func x ys ((y:z):zs)


mostrarLista :: [String] -> IO()
mostrarLista frase = do
    if length frase > 0
        then do
            putStrLn(map toUpper (head(frase)))
            mostrarLista (tail(frase))
    else
        exitSuccess

retorno :: String -> String
retorno string = string

imprimir :: String -> String
imprimir frase = do
    let produto = split " " frase
    let nomeProduto = head(tail(produto))
    let desconto = head(produto)
    let cadeia = nomeProduto ++ " " ++ desconto
    retorno cadeia


pegarValor :: [String] -> [String] -> [String] -> Int -> Int -> [String] -> IO()
pegarValor listaProdutos listaDescontoDin listaDesconto tamanhoListaProdutos tamanhoListaDesconto listaMostrar = do
    if tamanhoListaDesconto > 0
        then do
            if tamanhoListaProdutos > 0 
                then do
                    let produto = split "," (head(listaProdutos))
                    let produtoDesconto = split "," (head(listaDescontoDin))
                    if map toLower (head(produto)) == map toLower (head(produtoDesconto))
                        then do
                            let valor = head(tail(produto))
                            let valorDesconto = head(tail(produtoDesconto))
                            let valorInt = read valor :: Float
                            let valorDescontoInt = read valorDesconto :: Float
                            let porcentagem = (valorDescontoInt * 100) / valorInt
                            let porcentagemDesconto = 100 - porcentagem
                            if porcentagemDesconto >= 30
                                then do
                                let listaExibir = addInLista (show(format porcentagemDesconto 2)++"%" ++ " " ++ head(produto)) listaMostrar
                                --putStrLn(head(produto) ++ " " ++ show(format porcentagemDesconto 2)++"%")
                                pegarValor listaProdutos (tail(listaDescontoDin)) listaDesconto (length(listaProdutos)) (tamanhoListaDesconto-1) listaExibir
                            else do
                                pegarValor listaProdutos (tail(listaDescontoDin)) listaDesconto (length(listaProdutos)) (tamanhoListaDesconto-1) listaMostrar
                    else do
                        pegarValor listaProdutos (tail(listaDescontoDin)) listaDesconto (length(listaProdutos)) (tamanhoListaDesconto-1) listaMostrar
            else do
               let listaOrdenada = sort listaMostrar
               let listaDesc = reverse listaOrdenada
               
               let listaPronta = map imprimir listaDesc

               mostrarLista listaPronta
               

    else
        pegarValor (tail(listaProdutos)) listaDesconto listaDesconto (tamanhoListaProdutos-1) (length(listaDesconto)) listaMostrar


loopDesconto :: String -> [String] -> [String] -> IO()
loopDesconto produto lista listaProdutos = do
    if produto /= "*" 
        then do
            let listaNova = addInLista produto lista
            nome <- getLine
            loopDesconto nome listaNova listaProdutos
    else do
        pegarValor listaProdutos lista lista (length (listaProdutos)) (length (lista)) []

loop :: String -> [String] -> IO()
loop produto lista = do
    if produto /= "*" 
        then do
            let listaNova = addInLista produto lista
            nome <- getLine
            loop nome listaNova
    else do
        produtoDesconto <- getLine
        loopDesconto produtoDesconto [] lista


addInLista :: String -> [String] -> [String]
addInLista nome lista = do
    lista ++ [nome] 

listaProdutos = []
main :: IO()
main = do
    produto <- getLine
    loop produto listaProdutos
