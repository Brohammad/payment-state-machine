# payment-state-machine

[![Haskell CI](https://github.com/Brohammad/payment-state-machine/actions/workflows/ci.yml/badge.svg)](https://github.com/Brohammad/payment-state-machine/actions/workflows/ci.yml)

A small Haskell project that models a payment lifecycle with explicit state
transitions and idempotent event processing.

The project is intentionally simple: money is represented as integer minor
units, the transition logic is pure, and invalid operations return typed
errors instead of throwing exceptions.

## Payment lifecycle

The payment states are:

- `Created`
- `Authorized`
- `Captured`
- `Refunded`
- `Failed`
- `Voided`

The supported events are:

- `Authorize`
- `Capture`
- `Refund Money`
- `Void`
- `Fail Text`

The main rules are:

- A payment can be captured only after authorization.
- Captures are always for the full payment amount; partial capture is
  intentionally out of scope for this educational model.
- A payment can be refunded only after capture.
- Partial refunds are allowed.
- After a partial refund, the payment stays `Captured` with a reduced
  refundable balance; it becomes `Refunded` only when the full captured amount
  has been refunded.
- The total refunded amount cannot exceed the captured amount.
- A payment can be voided only while authorized.
- A payment can fail only before capture; captured payments require a refund,
  reversal, or chargeback flow instead.
- Failed, voided, and fully refunded payments are terminal.

## Why types help prevent illegal states

The domain is represented with algebraic data types. `PaymentState` and
`Event` have a finite set of constructors, so the transition function can
handle each combination explicitly. `Money` wraps an integer rather than
using `Double`, avoiding floating-point rounding for paise or other minor
currency units. `mkMoney` returns `Nothing` for zero or negative values, so
invalid `Money` values cannot enter events through the public API.

The transition function has this type:

```haskell
transition :: Payment -> Event -> Either TransitionError Payment
```

`Right` contains the updated payment, while `Left` contains a domain error.
This makes callers handle failure explicitly instead of relying on exceptions.
In Python, the closest equivalent would be a dataclass-based model with enums
and a function returning either a result object or a typed exception value.

## Idempotent processing

`Payment.Idempotency` provides a pure in-memory store:

```haskell
type Store = Map IdempotencyKey CachedResult
```

When a key is new, the event is applied and its complete result is stored.
When the same key is reused, the stored result is returned without applying
the event again. The store is passed in and returned explicitly, which keeps
the implementation deterministic and easy to test.

This educational implementation compares the original event on a reused key.
A key reused with a different event returns `IdempotencyKeyConflict`. A
production implementation should store a hash of a canonical request
fingerprint in a database with a unique key constraint, or in Redis with a
TTL for bounded retention.

`Payment` values are created through `newPayment`, which rejects non-positive
payment amounts. The `Payment` constructor is kept private to the module, so
callers cannot bypass that initial validation.

## Project layout

```text
src/Payment/Types.hs       Domain types
src/Payment/Transition.hs  Pure lifecycle rules
src/Payment/Idempotency.hs In-memory idempotency store
app/Main.hs                CLI demonstration
test/Spec.hs               Hspec and QuickCheck tests
payment-state-machine.cabal Cabal package definition
cabal.project              Enables the test suite for `cabal test`
README.md                 Project documentation
```

`Payment` also keeps the failure reason and a successful transition history,
which makes the model easier to inspect and audit.

## Build and run

This project requires GHC 9.4 or newer and Cabal.

Build the library and executable:

```bash
cabal build
```

Run the test suite:

```bash
cabal test
```

Run the CLI demo:

```bash
cabal run payment-state-machine
```

The demo replays authorization, capture, a partial refund, and a final refund,
printing each state change.

Expected demo output:

```text
Payment state machine demo
Created --Authorize--> Authorized
Authorized --Capture--> Captured
Captured --Refund (Money {minorUnits = 250})--> Captured
Captured --Refund (Money {minorUnits = 750})--> Refunded
```

The expected output above follows the hard-coded event list in
`app/Main.hs`. Run `cabal run payment-state-machine` locally to verify it
against your installed GHC and Cabal versions.

## Testing

The Hspec suite covers valid and invalid transitions, including terminal
states, invalid refund amounts, and idempotency behavior. QuickCheck also
checks that:

- A payment never refunds more than it captured.
- A sequence of partial refunds never causes the refunded amount to exceed
  the captured amount.
- Terminal states reject every event.
- Applying the same idempotency key twice equals applying it once.

## Design decisions

- **Integer money:** `Money` stores minor units as `Integer`, so calculations
  do not introduce floating-point rounding.
- **Full capture only:** partial capture is deliberately excluded to keep the
  lifecycle small; production support would track captured and remaining
  authorizable amounts separately.
- **Pure transitions:** state changes are represented as returned values; the
  transition function performs no IO and does not mutate the input payment.
- **Explicit errors:** `Either TransitionError Payment` makes invalid events
  visible to every caller.
- **Simple idempotency:** `Data.Map` is enough for an educational in-memory
  implementation and keeps the state-passing model clear.
- **Request consistency:** reusing a key with a different event is rejected;
  a production service would persist a stronger request fingerprint.
- **Audit information:** failure reasons and successful event/state pairs are
  retained on the payment for debugging and replay-oriented examples.
- **No advanced extensions:** the code uses standard Haskell 2010 features,
  algebraic data types, pattern matching, recursion, and common libraries.

## What I would add next

For production use, I would add:

1. **Persistence**, storing payments and idempotency results in a database so
   retries remain safe across process restarts.
2. **Concurrency with STM**, coordinating concurrent requests safely while
   keeping the state transition itself pure.
3. A database-backed request fingerprint and expiration policy for
   idempotency records.
4. Structured logging, API integration, and an HTTP service layer.
