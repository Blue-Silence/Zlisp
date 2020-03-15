module Analyzer (
    AST(..)
    ,construct
)where


import Tool
import Parser

data AST = I32 Int
         | Sym String
         | Nul
         --| Err
         | Lst [AST]
         | Boo Bool
        -- | Nod AST [AST]
         | Apply AST [AST]
         | Declare AST [AST] 
         deriving (Eq, Show)
---------------------------------------------------------------------------------------

---GRAMMAR PART

handlerList :: [(Not->Bool,[Not]->AST)]
handlerList=[]


transfer :: Not->AST
transfer (constant x) =I32 x 
transfer (Boolen x)=Boo x
transfer (Not ((Spec x):xs))=(select x (handlerList)) xs
transfer (Not (x:xs))=Apply x xs

select :: Not->[(Not->Bool,[Not]->AST)]->([Not]->AST))
select x ((fTest,f):fs)=if fTest x=f else select x fs

construct :: [Not]->[AST]
construct x=map transfer (chunkP x) 

---------------------------------------------------------------
chunkP :: [Not]->[Not] --去括号，形成Not []
chunkP []=[]
chunkP (LP:xs)=let (ys',xs') =spitP 1 xs in (Not (chunkP ys')):(chunkP xs')
chunkP (x:xs)=x:(chunkP xs)

spitP :: Int->[Not]->([Not],[Not])
spitP 0 xs=([],xs)
spitP n (x:xs)=case x of
                LP->spitP (n+1) xs
                RP->spitP (n-1) xs
                _->attach x (spitP n xs)