module AccountSystemSpec (testesAccountSystem) where

import AccountSystem
import AccountTypes
import Test.HUnit

testesAccountSystem :: Test
testesAccountSystem =
  TestLabel "Local accounts" $
    test
      [ "empty names are rejected" ~: nomeVazio,
        "names compare case-insensitively" ~: nomesNormalizados,
        "renaming preserves account id" ~: renomearPreservaId,
        "two accounts start independently" ~: contasIndependentes
      ]

nomeVazio :: Assertion
nomeVazio = case nomeContaValido "   " of
  Left _ -> return ()
  Right _ -> assertFailure "empty account name should be rejected"

nomesNormalizados :: Assertion
nomesNormalizados = nomeJaUsado "  TOMAS " [AccountSummary (AccountId "account-1") "tomas" 1 Nothing] @?= True

renomearPreservaId :: Assertion
renomearPreservaId =
  let original = novaConta (AccountId "account-1") "Tomas"
   in case renomearConta "Outro" original of
        Right renamed -> accountId renamed @?= accountId original
        Left erro -> assertFailure erro

contasIndependentes :: Assertion
contasIndependentes =
  let primeira = novaConta (AccountId "account-1") "A"
      segunda = novaConta (AccountId "account-2") "B"
   in do
        assertBool "ids must differ" (accountId primeira /= accountId segunda)
        assertBool "names must differ" (nomeConta primeira /= nomeConta segunda)
