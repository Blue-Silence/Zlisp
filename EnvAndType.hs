module EnvAndType(
    getVal
    ,empty
    ,unDefLst
    ,define
    ,DEF(..)
    ,mvDEF
    ,EVATree(..)
    ,Constant(..) 
    ,ENV

) where


----------------------------------------------------------------------
data Constant =Bo Bool|Int32 Int
data EVATree = ID String ENV
              |Const Constant
              |AppLam EVATree [EVATree]
--EVA PART


----------------------------------------------------------------------
data DEF =DEF String EVATree|UDEF String 


getVal :: ENV->EVATree->EVATree
empty ::  ENV
unDefLst :: ENV->[DEF]
define :: ENV->[DEF]->ENV
mvDEF :: [DEF]->DEF->[DEF]
-------------------------------------------------------------
--临时实现.可换用性能更好实现


data ENV = ENV [DEF] [DEF] 
unDefLst (ENV _ x)=x
empty=ENV [] []
define (ENV def undef) deflst=(ENV (deflst++def) (foldl mvDEF undef deflst))

mvDEF [] _=[]
mvDEF ((UDEF x):xs) a@(DEF y _)=if x==y then xs else (UDEF x):(mvDEF xs a)

getVal (ENV lt _) (ID x _)=getVal' lt x 
getVal' ((DEF x' val):xs) x=if x==x' then val else getVal' xs x