module Payment.Types (
    Money,
    minorUnits,
    mkMoney,
    addMoney,
    subtractMoney,
    PaymentState (..),
    Event (..),
    PaymentEvent (..),
    Payment,
    paymentId,
    paymentAmount,
    paymentState,
    capturedAmount,
    refundedAmount,
    failureReason,
    paymentHistory,
    newPayment,
    TransitionError (..),
) where

import Data.Text (Text)

-- Money is stored as integer minor units, such as paise.
newtype Money = Money {minorUnits :: Integer}
    deriving (Eq, Ord, Show)

-- Construct money from integer minor units.
mkMoney :: Integer -> Maybe Money
mkMoney value
    | value <= 0 = Nothing
    | otherwise = Just (Money value)

addMoney :: Money -> Money -> Money
addMoney (Money first) (Money second) = Money (first + second)

subtractMoney :: Money -> Money -> Maybe Money
subtractMoney (Money first) (Money second)
    | first <= second = Nothing
    | otherwise = Just (Money (first - second))

-- PaymentState describes where a payment is in its lifecycle.
data PaymentState
    = Created
    | Authorized
    | Captured
    | Refunded
    | Failed
    | Voided
    deriving (Eq, Show)

-- Event is an action someone can attempt on a payment.
data Event
    = Authorize
    | Capture
    | Refund Money
    | Void
    | Fail Text
    deriving (Eq, Show)

-- PaymentEvent records a successful event and the state it produced.
data PaymentEvent = PaymentEvent
    { appliedEvent :: Event
    , resultingState :: PaymentState
    }
    deriving (Eq, Show)

-- Payment contains the lifecycle state and the amounts needed for refunds.
data Payment = Payment
    { paymentId :: Text
    , paymentAmount :: Money
    , paymentState :: PaymentState
    , capturedAmount :: Maybe Money
    , refundedAmount :: Money
    , failureReason :: Maybe Text
    , paymentHistory :: [PaymentEvent]
    }
    deriving (Eq, Show)

-- TransitionError explains why an event cannot be applied.
data TransitionError
    = InvalidTransition PaymentState Event
    | RefundExceedsCapturedAmount Money
    | InvalidPaymentAmount Integer
    | IdempotencyKeyConflict Event Event
    deriving (Eq, Show)

-- Build a payment while enforcing the positive amount invariant.
newPayment :: Text -> Integer -> Either TransitionError Payment
newPayment identifier value
    | value <= 0 = Left (InvalidPaymentAmount value)
    | otherwise =
        Right
            Payment
                { paymentId = identifier
                , paymentAmount = Money value
                , paymentState = Created
                , capturedAmount = Nothing
                , refundedAmount = Money 0
                , failureReason = Nothing
                , paymentHistory = []
                }
