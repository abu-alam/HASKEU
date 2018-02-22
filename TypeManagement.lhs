> module TypeManagement where

> import ModelType

> import Language.Haskell.TypeCheck.TypeCheck
> import Language.Haskell.Exts.Annotated
> import Language.Haskell.Exts.Pretty

> import SyntaxTreeManagement


> class (TreeManager t a b) => TypeManager t t1 a b where
>	setType 		:: TypeInfo -> t1 (a, b) -> t (a,b) -> t (a,b)
>	setType	_ _ t		= t



