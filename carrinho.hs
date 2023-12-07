type Nome = String
type Preco = Double
type Produto = (Nome, Preco)
type Carrinho = [Produto]
type Estoque = [Produto]

adicionarAoCarrinho :: Produto -> Carrinho -> Carrinho
adicionarAoCarrinho produto carrinho = produto : carrinho

removerDoCarrinho :: Nome -> Carrinho -> Carrinho
removerDoCarrinho nome carrinho = filter (\(n, _) -> n /= nome) carrinho

calcularTotal :: Carrinho -> Preco
calcularTotal carrinho = sum (map snd carrinho)

main :: IO ()
main = do
    let estoque = [("Maçã", 1.0), ("Banana", 0.5), ("Laranja", 0.75)]
    let carrinho = adicionarAoCarrinho (estoque !! 0) []
    let carrinho' = adicionarAoCarrinho (estoque !! 1) carrinho
    let carrinho = adicionarAoCarrinho (estoque !! 0) []
    let carrinho' = adicionarAoCarrinho (estoque !! 1) carrinho
    let carrinho'' = adicionarAoCarrinho (estoque !! 2) carrinho'
    putStrLn ("Total do carrinho: " ++ show total)
