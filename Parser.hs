module Parser ( 
    Not(..)
    ,parser

)where

import Tool

data Not=LP
        |RP
        |Id String
        |Constant Int
        |Boolean Bool
        |Not [Not]
        |Spec String

getFunLst :: [(([Char]->Bool),([Char]->Not))]
getFunLst=[]
spaceList :: [Char] --empty chars
spaceList=[]
--Grammar Part
--------------------------

parser :: String->[Not]
parser []= []
parser xs= map proc (genChunk xs)





------------------------------------------------
proc :: String->Not --整合
proc x=pick x getFunLst 

pick x ((fTest,f):xs)
    |fTest x =f x
    |otherwise=pick x xs

-------------------------------------------------
genChunk :: [Char]->[String]
genChunk []=[]
genChunk xs=let (x',xs')=getSingle (mvHeadSpc xs) in x':(genChunk (mvHeadSpc xs'))

mvHeadSpc []=[] --remove space in the head
mvHeadSpc (x:xs)
    |elem x spaceList = mvHeadSpc xs
    |otherwise=(x:xs)

getSingle :: [Char]->([Char],[Char])
getSingle []=([],[])
getSingle (x:xs)
    |elem x spaceList=([],xs)
    |otherwise=attach x (getSingle xs)
