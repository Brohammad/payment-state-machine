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
                , capturedAmount = Just (mkMoney 1000)
                }
            )

    it "allows a partial refund" $
      transition capturedPayment (Refund (mkMoney 400))
        `shouldBe`
          Right
            ( capturedPayment
                { refundedAmount = mkMoney 400
                }
            )

    it "marks a fully refunded payment as refunded" $
      transition capturedPayment (Refund (mkMoney 1000))
        `shouldBe`
          Right
            ( capturedPayment
                { paymentState = Refunded
                , refundedAmount = mkMoney 1000
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
      transition capturedPayment (Refund (mkMoney 1001))
        `shouldBe` Left RefundExceedsCapturedAmount

    it "rejects non-positive refunds" $ do
      transition capturedPayment (Refund (mkMoney 0))
        `shouldBe` Left InvalidRefundAmount
      transition capturedPayment (Refund (mkMoney (-1)))
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

    it "rejects a reused key with a different event" $ do
      let key = IdempotencyKey (Text.pack "request-2")
          (storeAfterFirst, _) =
            applyIdempotent emptyStore createdPayment key Authorize
          (storeAfterSecond, result) =
            applyIdempotent storeAfterFirst createdPayment key Capture
      storeAfterSecond `shouldBe` storeAfterFirst
      result `shouldBe` Left (IdempotencyKeyConflict Authorize Capture)

  describe "properties" $ do
    it "never refunds more than the captured amount" $
      property prop_refundNeverExceedsCaptured

    it "terminal states reject all events" $
      property prop_terminalStatesRejectEvents

    it "reusing an idempotency key equals applying it once" $
      property prop_idempotencyIsStable

    it "never refunds more than captured across multiple refunds" $
      property prop_multipleRefundsNeverExceedCaptured

createdPayment :: Payment
createdPayment =
  case newPayment (Text.pack "payment-1") (mkMoney 1000) of
    Right payment -> payment
    Left errorMessage -> error (show errorMessage)

authorizedPayment :: Payment
authorizedPayment = createdPayment { paymentState = Authorized }

capturedPayment :: Payment
capturedPayment =
  authorizedPayment
    { paymentState = Captured
    , capturedAmount = Just (mkMoney 1000)
    }

allEvents :: [Event]
allEvents =
  [ Authorize
  , Capture
  , Refund (mkMoney 1)
  , Void
  , Fail (Text.pack "failure")
  ]

invalidTransitions :: [(Payment, Event)]
invalidTransitions =
  [ (createdPayment, Capture)
  , (createdPayment, Void)
  , (authorizedPayment, Authorize)
  , (authorizedPayment, Refund (mkMoney 100))
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
  let result = transition capturedPayment (Refund (mkMoney requested))
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

prop_multipleRefundsNeverExceedCaptured :: [Positive Integer] -> Property
prop_multipleRefundsNeverExceedCaptured requests =
  let refunds = map (Refund . mkMoney . getPositive) requests
      results = scanl applyRefundEvent (Right capturedPayment) refunds
   in conjoin
        [ case result of
            Left RefundExceedsCapturedAmount -> property True
            Left InvalidRefundAmount -> property False
            Left (InvalidTransition Refunded (Refund _)) -> property True
            Right payment ->
              property (minorUnits (refundedAmount payment) <= 1000)
            Left _ -> property False
        | result <- results
        ]

applyRefundEvent
  :: Either TransitionError Payment
  -> Event
  -> Either TransitionError Payment
applyRefundEvent (Left errorMessage) _ = Left errorMessage
applyRefundEvent (Right payment) event = transition payment event
