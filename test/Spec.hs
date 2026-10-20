module Main (main) where

import qualified Data.Text as Text
import Payment.Idempotency
import Payment.Transition (transition)
import Payment.Types
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "transition" $ do
    it "authorizes a created payment" $
      transition createdPayment Authorize
        `shouldBe` Right (createdPayment { paymentState = Authorized })

    it "captures an authorized payment" $
      transition authorizedPayment Capture
        `shouldBe`
          Right
            ( authorizedPayment
                { paymentState = Captured
                , capturedAmount = Just (Money 1000)
                }
            )

    it "allows a partial refund" $
      transition capturedPayment (Refund (Money 400))
        `shouldBe`
          Right
            ( capturedPayment
                { refundedAmount = Money 400
                }
            )

    it "marks a fully refunded payment as refunded" $
      transition capturedPayment (Refund (Money 1000))
        `shouldBe`
          Right
            ( capturedPayment
                { paymentState = Refunded
                , refundedAmount = Money 1000
                }
            )

    it "voids an authorized payment" $
      transition authorizedPayment Void
        `shouldBe` Right (authorizedPayment { paymentState = Voided })

    it "fails a created payment" $
      transition createdPayment (Fail (Text.pack "timeout"))
        `shouldBe` Right (createdPayment { paymentState = Failed })

    it "fails a captured payment" $
      transition capturedPayment (Fail (Text.pack "processor error"))
        `shouldBe` Right (capturedPayment { paymentState = Failed })

    it "rejects refunds larger than the captured amount" $
      transition capturedPayment (Refund (Money 1001))
        `shouldBe` Left RefundExceedsCapturedAmount

    it "rejects non-positive refunds" $ do
      transition capturedPayment (Refund (Money 0))
        `shouldBe` Left InvalidRefundAmount
      transition capturedPayment (Refund (Money (-1)))
        `shouldBe` Left InvalidRefundAmount

    it "rejects every invalid non-terminal transition" $
      mapM_ assertInvalidTransition invalidTransitions

    it "rejects every event from terminal states" $ do
      mapM_ assertTerminalState [Failed, Voided, Refunded]

  describe "idempotency" $ do
    it "returns the original result for a reused key" $ do
      let key = IdempotencyKey (Text.pack "request-1")
          (storeAfterFirst, firstResult) =
            applyIdempotent emptyStore createdPayment key Authorize
          (storeAfterSecond, secondResult) =
            applyIdempotent storeAfterFirst createdPayment key Authorize
      storeAfterSecond `shouldBe` storeAfterFirst
      secondResult `shouldBe` firstResult

  describe "properties" $ do
    it "never refunds more than the captured amount" $
      property prop_refundNeverExceedsCaptured

    it "terminal states reject all events" $
      property prop_terminalStatesRejectEvents

    it "reusing an idempotency key equals applying it once" $
      property prop_idempotencyIsStable

createdPayment :: Payment
createdPayment =
  Payment
    { paymentId = Text.pack "payment-1"
    , paymentAmount = Money 1000
    , paymentState = Created
    , capturedAmount = Nothing
    , refundedAmount = Money 0
    }

authorizedPayment :: Payment
authorizedPayment = createdPayment { paymentState = Authorized }

capturedPayment :: Payment
capturedPayment =
  authorizedPayment
    { paymentState = Captured
    , capturedAmount = Just (Money 1000)
    }

allEvents :: [Event]
allEvents =
  [ Authorize
  , Capture
  , Refund (Money 1)
  , Void
  , Fail (Text.pack "failure")
  ]

invalidTransitions :: [(Payment, Event)]
invalidTransitions =
  [ (createdPayment, Capture)
  , (createdPayment, Void)
  , (authorizedPayment, Authorize)
  , (authorizedPayment, Refund (Money 100))
  , (capturedPayment, Authorize)
  , (capturedPayment, Capture)
  , (capturedPayment, Void)
  ]

assertInvalidTransition :: (Payment, Event) -> Expectation
assertInvalidTransition (payment, event) =
  transition payment event
    `shouldBe` Left (InvalidTransition (paymentState payment) event)

assertTerminalState :: PaymentState -> Expectation
assertTerminalState state =
  let payment = createdPayment { paymentState = state }
   in mapM_
        (\event ->
           transition payment event
             `shouldBe` Left (InvalidTransition state event)
        )
        allEvents

prop_refundNeverExceedsCaptured :: Positive Integer -> Property
prop_refundNeverExceedsCaptured (Positive requested) =
  let result = transition capturedPayment (Refund (Money requested))
   in case result of
        Left RefundExceedsCapturedAmount -> property True
        Left InvalidRefundAmount -> property False
        Right payment ->
          counterexample
            "a successful refund must not exceed the captured amount"
            (minorUnits (refundedAmount payment) <= 1000)
        Left _ -> property False

prop_terminalStatesRejectEvents :: Property
prop_terminalStatesRejectEvents =
  conjoin
    [ transition (createdPayment { paymentState = state }) event
        === Left (InvalidTransition state event)
    | state <- [Failed, Voided, Refunded]
    , event <- allEvents
    ]

prop_idempotencyIsStable :: Positive Integer -> Property
prop_idempotencyIsStable (Positive suffix) =
  let key = IdempotencyKey (Text.pack ("request-" ++ show suffix))
      once = applyIdempotent emptyStore createdPayment key Authorize
      twice = applyIdempotent (fst once) createdPayment key Authorize
   in twice === once
