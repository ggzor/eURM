{-# LANGUAGE QuasiQuotes #-}
module URM.TH where

import Control.Lens
import Control.Monad
import Language.Haskell.TH

getConstructors :: Info -> Q [(Name, [Type])]
getConstructors (TyConI tyCon) = 
  case tyCon of
    DataD _ _ _ _ constructors _ -> forM constructors $ \case
      RecC name vars -> pure (name, vars <&> view _3)
      NormalC name vars -> pure (name, vars <&> view _2)
      _ -> fail "getConstructors only works with non-polymorphic normal or simple record constructors"
    _ -> fail "getConstructors only works with data constructors"
getConstructors _ = fail "getConstructors only works with type constructors"

makeFieldNames :: Name -> Q [Dec]
makeFieldNames ty =
  do constructors <- getConstructors =<< reify ty
     forM constructors $ \(name, types) -> 
        do tySynName <- newName (nameBase name ++ "Fields")
           return $ TySynD tySynName [] $ foldl AppT (TupleT (length types)) types

makeTags :: Name -> Q [Dec]
makeTags ty =
  do tyCon@(TyConI (DataD _ tyName _ _ _ _)) <- reify ty
     constructors <- getConstructors tyCon
     tagsDataName <- newName (nameBase tyName ++ "Tag")
     tagsConstructorNames <- forM constructors $ \(name, _) ->
                               newName (nameBase name ++ "Tag")
     let tagsConstructors = flip NormalC [] <$> tagsConstructorNames
     let tagsConstructor = DataD [] tagsDataName [] Nothing tagsConstructors []
     tagsGetterName <- newName ("get" ++ nameBase tyName ++ "Tag")
     let constructorPatterns = constructors <&> \(name, types) -> ConP name $ replicate (length types) WildP
     let tagsGetterPatterns = zip constructorPatterns tagsConstructorNames <&> \(pattern, tagName) ->
                                Clause [pattern] (NormalB (ConE tagName)) []
     tagProjectorSignature <- SigD tagsGetterName <$> [t| $(pure $ ConT tyName) -> $(pure $ ConT tagsDataName) |]
     let tagProjector = FunD tagsGetterName tagsGetterPatterns
     pure [tagsConstructor, tagProjectorSignature, tagProjector]
