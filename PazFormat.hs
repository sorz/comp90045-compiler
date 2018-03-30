module PazFormat where

import PazParser (
    ASTStartSymbol
    )

-- Generate formatted source code from AST.
formatAST :: ASTStartSymbol -> String
formatAST ast =
    show ast
    -- TODO
