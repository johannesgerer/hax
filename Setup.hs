import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Haddock
main = defaultMainWithHooks simpleUserHooks{
  haddockHook = \p l h f -> haddockHook simpleUserHooks p l h f{
     haddockHoogle       = Flag True,
     haddockHtml         = Flag True,
     haddockExecutables  = Flag True,
     haddockHscolour     = Flag True
     }
  }

