module Main (main) where

import qualified Data.Text as Text
import Payment.Transition (transition)
import Payment.Types

main :: IO ()
main = do
  putStrLn "Payment state machine demo"
  replayEvents initialPayment demoEvents

initialPayment :: Payment
initialPayment =
  Payment
    { paymentId = Text.pack "demo-payment"
    , paymentAmount = Money 1000
    , paymentState = Created
    , capturedAmount = Nothing
    , refundedAmount = Money 0
    }

demoEvents :: [Event]
demoEvents =
  [ Authorize
  , Capture
  , Refund (Money 250)
  , Refund (Money 750)
  ]

replayEvents :: Payment -> [Event] -> IO ()
replayEvents _ [] = pure ()
replayEvents payment (event : remainingEvents) =
  case transition payment event of
    Left errorMessage -> do
      putStrLn ("Rejected " ++ show event ++ ": " ++ show errorMessage)
      replayEvents payment remainingEvents
    Right nextPayment -> do
      putStrLn
        ( show (paymentState payment)
            ++ " --"
            ++ show event
            ++ "--> "
            ++ show (paymentState nextPayment)
        )
      replayEvents nextPayment remainingEvents
