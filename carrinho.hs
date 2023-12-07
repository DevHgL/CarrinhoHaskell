type Nome = String
type Produto = (Int, Nome, Double)
type Carrinho = [Produto]
type Estoque = [Produto]

estoque :: Estoque
estoque = [(1,"teclado",50.00),(2,"monitor",450.00),(3,"tv 54", 4600.00)]

encontrarProduto :: Int -> Estoque -> Maybe Produto
encontrarProduto _ [] = Nothing
encontrarProduto codigo ((c, nome, preco):resto)
  | codigo == c = Just (c, nome, preco)
  | otherwise = encontrarProduto codigo resto

adicionarAoCarrinho :: Produto -> Carrinho -> Carrinho
adicionarAoCarrinho produto carrinho = produto : carrinho

valorTotal :: Carrinho -> Double
valorTotal [] = 0.0
valorTotal ((codigo, nome, preco):restoCarrinho) =
  preco + valorTotal restoCarrinho

buscarNoEstoque :: Nome -> Estoque -> Maybe Produto
buscarNoEstoque _ [] = Nothing
buscarNoEstoque nome ((c, nomeProduto, preco):resto)
  | nome == nomeProduto = Just (c, nomeProduto, preco)
  | otherwise = buscarNoEstoque nome resto

main :: IO ()
main = do
    let item = "teclado"
    let produto = buscarNoEstoque item estoque
    case produto of
      Nothing -> putStrLn "Produto não encontrado no estoque."
      Just p -> do
        let carrinho = adicionarAoCarrinho p []
        let total = valorTotal carrinho
        putStrLn ("Total do carrinho: " ++ show total)

