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
    case newPayment (Text.pack "demo-payment") 1000 of
        Right payment -> payment
        Left errorMessage -> error (show errorMessage)

demoEvents :: [Event]
demoEvents =
    [ Authorize
    , Capture
    , Refund (money 250)
    , Refund (money 750)
    ]

money :: Integer -> Money
money value =
    case mkMoney value of
        Just amount -> amount
        Nothing -> error "demo money must be positive"

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
