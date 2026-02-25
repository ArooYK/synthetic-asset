;; synthetic-asset.clar
;; ------------------------------------------------------------
;; Collateralized Synthetic Asset (sToken)
;; - Users lock STX as collateral
;; - Mint synthetic asset against collateral
;; - Requires minimum collateral ratio
;; - Supports liquidation
;; ------------------------------------------------------------

(define-constant ERR_INVALID_AMOUNT (err u100))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u101))
(define-constant ERR_NOT_ADMIN (err u102))
(define-constant ERR_UNDERCOLLATERALIZED (err u103))
(define-constant ERR_NO_POSITION (err u104))

(define-constant PRECISION u1000000)
(define-constant MIN_COLLATERAL_RATIO u1500000) ;; 150% (scaled 1e6)

;; -------------------------
;; Price and supply
;; -------------------------
;; Price of synthetic asset in microSTX (scaled 1e6)
;; Example: 1 synthetic = 2 STX -> price = 2_000000
(define-data-var price uint u1000000)

;; Total supply
(define-data-var total-supply uint u0)

;; User positions
(define-map positions
  { user: principal }
  {
    collateral: uint,
    debt: uint
  })

;; Synthetic balances
(define-map balances
  { user: principal }
  { amount: uint })

;; -------------------------
;; Events
;; -------------------------

(define-private (ev-deposit (user principal) (amount uint))
  (print { event: "collateral-deposit", user: user, amount: amount }))

(define-private (ev-mint (user principal) (amount uint))
  (print { event: "mint", user: user, amount: amount }))

(define-private (ev-burn (user principal) (amount uint))
  (print { event: "burn", user: user, amount: amount }))

;; -------------------------
;; Admin
;; -------------------------

(define-data-var admin principal tx-sender)

(define-public (set-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (asserts! (> new-price u0) (err ERR_INVALID_AMOUNT))
    (var-set price new-price)
    (ok true)
  )
)

(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (asserts! (not (is-eq new-admin tx-sender)) (err ERR_INVALID_AMOUNT))
    (var-set admin new-admin)
    (ok true)
  )
)

;; -------------------------
;; Collateral Deposit
;; -------------------------

(define-public (deposit-collateral (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))

    (let ((pos (map-get? positions { user: tx-sender })))
      (if (is-some pos)
          (let ((p (unwrap-panic pos)))
            (map-set positions
              { user: tx-sender }
              { collateral: (+ (get collateral p) amount),
                debt: (get debt p) }))
          (map-set positions
            { user: tx-sender }
            { collateral: amount, debt: u0 })))

    (ev-deposit tx-sender amount)
    (ok true)
  )
)

;; -------------------------
;; Mint Synthetic Asset
;; -------------------------

(define-public (mint (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))

    (let ((pos? (map-get? positions { user: tx-sender })))
      (asserts! (is-some pos?) (err ERR_NO_POSITION))

      (let (
            (pos (unwrap-panic pos?))
            (new-debt (+ (get debt pos) amount))
            (collateral-value (* (get collateral pos) PRECISION))
            (debt-value (* new-debt (var-get price)))
           )

        ;; Check collateral ratio
        (asserts!
          (>= (/ collateral-value debt-value) MIN_COLLATERAL_RATIO)
          (err ERR_INSUFFICIENT_COLLATERAL))

        ;; Update position
        (map-set positions
          { user: tx-sender }
          { collateral: (get collateral pos), debt: new-debt })

        ;; Mint synthetic balance
        (let ((bal (default-to u0 (get amount (map-get? balances { user: tx-sender })))))
          (map-set balances
            { user: tx-sender }
            { amount: (+ bal amount) }))

        (var-set total-supply (+ (var-get total-supply) amount))
        (ev-mint tx-sender amount)
        (ok true)
      )
    )
  )
)

;; -------------------------
;; Burn Synthetic Asset
;; -------------------------

(define-public (burn (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))

    (let (
          (pos? (map-get? positions { user: tx-sender }))
          (bal (default-to u0 (get amount (map-get? balances { user: tx-sender }))))
         )

      (asserts! (is-some pos?) (err ERR_NO_POSITION))
      (asserts! (>= bal amount) (err ERR_INVALID_AMOUNT))

      (let ((pos (unwrap-panic pos?)))
        (map-set positions
          { user: tx-sender }
          { collateral: (get collateral pos),
            debt: (- (get debt pos) amount) })

        (map-set balances
          { user: tx-sender }
          { amount: (- bal amount) })

        (var-set total-supply (- (var-get total-supply) amount))

        (ev-burn tx-sender amount)
        (ok true)
      )
    )
  )
)

;; -------------------------
;; Liquidation
;; -------------------------

(define-public (liquidate (target principal))
  (begin
    (asserts! (not (is-eq target tx-sender)) (err ERR_INVALID_AMOUNT))
    (let ((pos? (map-get? positions { user: target })))
      (asserts! (is-some pos?) (err ERR_NO_POSITION))

      (let (
            (pos (unwrap-panic pos?))
            (collateral-value (* (get collateral pos) PRECISION))
            (debt-value (* (get debt pos) (var-get price)))
           )

        ;; If collateral ratio < minimum -> liquidatable
        (asserts!
          (< (/ collateral-value debt-value) MIN_COLLATERAL_RATIO)
          (err ERR_UNDERCOLLATERALIZED))

        ;; Remove position
        (begin
          (map-delete positions { user: target })
          (ok true)
        )
      )
    )
  )
)

;; -------------------------
;; Views
;; -------------------------

(define-read-only (get-position (user principal))
  (ok (map-get? positions { user: user })))

(define-read-only (get-balance (user principal))
  (ok (default-to u0 (get amount (map-get? balances { user: user })))))

(define-read-only (get-price)
  (ok (var-get price)))

(define-read-only (get-total-supply)
  (ok (var-get total-supply)))