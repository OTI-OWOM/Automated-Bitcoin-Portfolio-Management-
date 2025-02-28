;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-asset-id (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-asset-exists (err u104))
(define-constant err-asset-not-exists (err u105))
(define-constant err-invalid-threshold (err u106))
(define-constant err-invalid-allocation (err u107))
(define-constant err-invalid-risk-level (err u108))
(define-constant err-rebalance-in-progress (err u109))
(define-constant err-no-liquidity (err u110))


;; Define risk levels (conservative, moderate, aggressive)
(define-constant risk-conservative u1)
(define-constant risk-moderate u2)
(define-constant risk-aggressive u3)

;; Data maps and variables
(define-data-var rebalancing-in-progress bool false)
(define-data-var rebalance-frequency uint u30) ;; Default 30 days
(define-data-var last-rebalance-block uint u0)
(define-data-var default-risk-level uint risk-moderate)
(define-data-var max-slippage-percentage uint u2) ;; Default 2%
(define-data-var minimum-rebalance-threshold uint u5) ;; Only rebalance if drift > 5%
(define-data-var performance-fee-percentage uint u2) ;; 2% fee on profits
