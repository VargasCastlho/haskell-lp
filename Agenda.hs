module Main where
import Data.Char
import System.IO

data Tipocontato = Contato { contNome :: String
                       , contTelefone :: Int
                       , contEndereco :: String
                       , contRelacao :: String
                       } deriving (Show) 

--Agenda :: [Tipocontato] (perguntar para o Baz)

type Agenda = [Tipocontato]
--Verifica inicio do nome com o nome completo
verificanome :: String -> String -> Bool
verificanome [] completoNome = True
verificanome comecoNome [] = False
verificanome comecoNome completoNome
 | toLower (head comecoNome) == toLower (head completoNome) = verificanome (tail comecoNome) (tail completoNome)
 | otherwise = False

--Verifica se existe o contato
verificaContato :: String -> Agenda -> Bool 
verificaContato nomeContato [] = False
verificaContato nomeContato agenda
 | nomeContato == contNome (head agenda) = True
 | otherwise = verificaContato nomeContato (tail agenda)

-- Busca contato pelo nome na agenda 
buscaContato :: String -> Agenda -> Tipocontato
buscaContato nomeContato agenda
 | verificanome nomeContato (contNome (head agenda)) = head agenda
 | otherwise = buscaContato nomeContato (tail agenda)

-- Altera o contato
alterar :: Tipocontato -> Agenda -> Agenda
alterar contato [] = []
alterar contato agenda
 | contNome contato == contNome (head agenda) = contato : tail agenda
 | otherwise = head agenda : alterar contato (tail agenda)

inserir :: Tipocontato -> Agenda -> Agenda
inserir contato [] = [contato]
inserir contato agenda
 | verificaContato (contNome contato) agenda = alterar contato agenda
 | otherwise = agenda ++ [contato]

remover :: String -> Agenda -> Agenda
remover nomeContato [] = []
remover nomeContato agenda
 | nomeContato == contNome (head agenda) = tail agenda
 | otherwise = head agenda : remover nomeContato (tail agenda)

strContato :: Tipocontato -> String
strContato (Contato nome telefone endereco relacao) = nome ++ " | "++ show telefone ++" | " ++ endereco ++ " | " ++ relacao

listar :: Agenda -> [Char]
listar [] = []
listar agenda = strContato(head agenda) ++ "\n" ++ listar (tail agenda)

--strContato (Agenda Contato)
criaArq :: Agenda -> IO()
criaArq a = do
 arq <- openFile "arquivo.txt" WriteMode
 hPutStr arq (show a)
 putStrLn "Arquivo criado com sucesso"
 hFlush arq
 hClose arq

mostrarArq :: IO()
mostrarArq = do
  conteudo <- readFile "arquivo.txt"
  putStrLn conteudo


main :: IO ()
main = do

 let agenda1 = inserir (Contato "Fulano" 99999999 "Rua A" "UFF") []
 let agenda2 = inserir (Contato "Ciclano" 88888888 "Rua B" "Cederj") agenda1
 let agenda3 = inserir (Contato "Beltrano" 88889999 "Rua C" "Infancia") agenda2
 let agenda4 = inserir (Contato "Fulano" 77777777 "Rua D" "UFF") agenda3
 let agenda5 = remover "Ciclano" agenda4
 
 putStrLn ("\n")
 putStrLn ("------------> Agenda telefonica em Haskell <------------")
 putStrLn ("\n")
 putStrLn (" Nome | Telefone | Endereco | Relacao")
 putStrLn ("_____________________________________")
 --putStrLn ("\n") 
 putStrLn (listar agenda4)

 putStrLn ("\nCriando o Arquivo")
 criaArq agenda4
 putStrLn ("\nExibindo conteudo o Arquivo")
 mostrarArq
