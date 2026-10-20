module Payment.Transition
  ( transition
  ) where

import Payment.Types

-- Apply one event without performing IO or mutating the original payment.
transition :: Payment -> Event -> Either TransitionError Payment
transition payment Authorize =
  case paymentState payment of
    Created -> Right payment { paymentState = Authorized }
    Authorized -> Left (InvalidTransition Authorized Authorize)
    Captured -> Left (InvalidTransition Captured Authorize)
    Refunded -> Left (InvalidTransition Refunded Authorize)
    Failed -> Left (InvalidTransition Failed Authorize)
    Voided -> Left (InvalidTransition Voided Authorize)
transition payment Capture =
  case paymentState payment of
    Created -> Left (InvalidTransition Created Capture)
    Authorized ->
      Right payment
        { paymentState = Captured
        , capturedAmount = Just (paymentAmount payment)
        }
    Captured -> Left (InvalidTransition Captured Capture)
    Refunded -> Left (InvalidTransition Refunded Capture)
    Failed -> Left (InvalidTransition Failed Capture)
    Voided -> Left (InvalidTransition Voided Capture)
transition payment (Refund amount) =
  case paymentState payment of
    Created -> Left (InvalidTransition Created (Refund amount))
    Authorized -> Left (InvalidTransition Authorized (Refund amount))
    Captured -> applyRefund payment amount
    Refunded -> Left (InvalidTransition Refunded (Refund amount))
    Failed -> Left (InvalidTransition Failed (Refund amount))
    Voided -> Left (InvalidTransition Voided (Refund amount))
transition payment Void =
  case paymentState payment of
    Created -> Left (InvalidTransition Created Void)
    Authorized -> Right payment { paymentState = Voided }
    Captured -> Left (InvalidTransition Captured Void)
    Refunded -> Left (InvalidTransition Refunded Void)
    Failed -> Left (InvalidTransition Failed Void)
    Voided -> Left (InvalidTransition Voided Void)
transition payment (Fail reason) =
  case paymentState payment of
    Created -> Right payment { paymentState = Failed }
    Authorized -> Right payment { paymentState = Failed }
    Captured -> Right payment { paymentState = Failed }
    Refunded -> Left (InvalidTransition Refunded (Fail reason))
    Failed -> Left (InvalidTransition Failed (Fail reason))
    Voided -> Left (InvalidTransition Voided (Fail reason))

applyRefund :: Payment -> Money -> Either TransitionError Payment
applyRefund payment amount =
  case capturedAmount payment of
    Nothing -> Left (InvalidTransition Captured (Refund amount))
    Just capturedAmountValue ->
      let captured = minorUnits capturedAmountValue
          refunded = minorUnits (refundedAmount payment)
          requested = minorUnits amount
          remaining = captured - refunded
       in if requested <= 0
            then Left InvalidRefundAmount
            else
              if requested > remaining
                then Left RefundExceedsCapturedAmount
                else
                  if requested == remaining
                    then
                      Right payment
                        { paymentState = Refunded
                        , refundedAmount = mkMoney (refunded + requested)
                        }
                    else
                      Right payment
                        { refundedAmount = mkMoney (refunded + requested)
                        }
