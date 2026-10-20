module Payment.Transition (
    transition,
) where

import Data.Text (Text)
import Payment.Types

-- Apply one event without performing IO or mutating the original payment.
transition :: Payment -> Event -> Either TransitionError Payment
transition payment Authorize =
    case paymentState payment of
        Created -> success payment Authorize Authorized id
        Authorized -> Left (InvalidTransition Authorized Authorize)
        Captured -> Left (InvalidTransition Captured Authorize)
        Refunded -> Left (InvalidTransition Refunded Authorize)
        Failed -> Left (InvalidTransition Failed Authorize)
        Voided -> Left (InvalidTransition Voided Authorize)
transition payment Capture =
    case paymentState payment of
        Created -> Left (InvalidTransition Created Capture)
        Authorized ->
            success
                payment
                Capture
                Captured
                (\updated -> updated{capturedAmount = Just (paymentAmount payment)})
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
        Authorized -> success payment Void Voided id
        Captured -> Left (InvalidTransition Captured Void)
        Refunded -> Left (InvalidTransition Refunded Void)
        Failed -> Left (InvalidTransition Failed Void)
        Voided -> Left (InvalidTransition Voided Void)
transition payment (Fail reason) =
    case paymentState payment of
        Created -> successWithReason payment (Fail reason) Failed reason
        Authorized -> successWithReason payment (Fail reason) Failed reason
        Captured -> Left (InvalidTransition Captured (Fail reason))
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
                    then Left (InvalidRefundAmount amount)
                    else
                        if requested > remaining
                            then
                                Left
                                    (RefundExceedsCapturedAmount amount (positiveMoney remaining))
                            else
                                if requested == remaining
                                    then
                                        success
                                            payment
                                            (Refund amount)
                                            Refunded
                                            ( \updated ->
                                                updated
                                                    { refundedAmount = positiveMoney (refunded + requested)
                                                    }
                                            )
                                    else
                                        success
                                            payment
                                            (Refund amount)
                                            Captured
                                            ( \updated ->
                                                updated
                                                    { refundedAmount = positiveMoney (refunded + requested)
                                                    }
                                            )

success ::
    Payment ->
    Event ->
    PaymentState ->
    (Payment -> Payment) ->
    Either TransitionError Payment
success payment event state updatePayment =
    Right
        (updatePayment payment)
            { paymentState = state
            , paymentHistory =
                paymentHistory payment
                    ++ [PaymentEvent event state]
            }

successWithReason :: Payment -> Event -> PaymentState -> Text -> Either TransitionError Payment
successWithReason payment event state reason =
    success
        payment
        event
        state
        (\updated -> updated{failureReason = Just reason})

positiveMoney :: Integer -> Money
positiveMoney value =
    case mkMoney value of
        Just amount -> amount
        Nothing -> error "positiveMoney called with a non-positive value"
