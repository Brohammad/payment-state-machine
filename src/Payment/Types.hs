module Payment.Types
  ( Money
  , minorUnits
  , mkMoney
  , PaymentState (..)
  , Event (..)
  , Payment
  , paymentId
  , paymentAmount
  , paymentState
  , capturedAmount
  , refundedAmount
  , newPayment
  , TransitionError (..)
  ) where

import Data.Text (Text)

-- Money is stored as integer minor units, such as paise.
newtype Money = Money { minorUnits :: Integer }
  deriving (Eq, Ord, Show)

-- Construct money from integer minor units.
mkMoney :: Integer -> Money
mkMoney = Money

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

-- Payment contains the lifecycle state and the amounts needed for refunds.
data Payment = Payment
  { paymentId :: Text
  , paymentAmount :: Money
  , paymentState :: PaymentState
  , capturedAmount :: Maybe Money
  , refundedAmount :: Money
  }
  deriving (Eq, Show)

-- TransitionError explains why an event cannot be applied.
data TransitionError
  = InvalidTransition PaymentState Event
  | RefundExceedsCapturedAmount
  | InvalidRefundAmount
  | InvalidPaymentAmount
  | IdempotencyKeyConflict Event Event
  deriving (Eq, Show)

-- Build a payment while enforcing the positive amount invariant.
newPayment :: Text -> Money -> Either TransitionError Payment
newPayment identifier amount@(Money value)
  | value <= 0 = Left InvalidPaymentAmount
  | otherwise =
      Right
        Payment
          { paymentId = identifier
          , paymentAmount = amount
          , paymentState = Created
          , capturedAmount = Nothing
          , refundedAmount = Money 0
          }
