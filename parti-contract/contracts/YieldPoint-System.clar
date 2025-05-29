;; title: YieldPoint-System
;; version:
;; summary:
;; description:

;; YieldPoints System - Tracks and rewards contributions based on impact and effort
;; Implements SIP-010 Fungible Token Standard for the YieldPoints token

(define-fungible-token yield-points)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-contribution (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-proposal-not-found (err u104))
(define-constant err-proposal-expired (err u105))
(define-constant err-proposal-already-executed (err u106))
(define-constant err-insufficient-voting-power (err u107))

;; Contribution types and their weights
(define-map contribution-weights
  {contribution-type: (string-utf8 50)}
  {weight: uint, description: (string-utf8 200)}
)

;; Authorized evaluators who can assign points
(define-map authorized-evaluators principal bool)

;; User contribution history
(define-map user-contributions
  {user: principal, contribution-id: uint}
  {
    contribution-type: (string-utf8 50),
    description: (string-utf8 500),
    timestamp: uint,
    points-awarded: uint,
    evaluator: principal
  }
)

;; Track contribution count per user
(define-map user-contribution-count principal uint)

;; Total contribution count
(define-data-var total-contribution-count uint u0)

;; Governance proposals for weight adjustments
(define-map weight-adjustment-proposals
  uint
  {
    proposer: principal,
    contribution-type: (string-utf8 50),
    new-weight: uint,
    description: (string-utf8 500),
    deadline: uint,
    executed: bool,
    total-votes: uint
  }
)

;; Votes on proposals
(define-map proposal-votes
  {proposal-id: uint, voter: principal}
  {vote-amount: uint, support: bool}
)

;; Proposal counter
(define-data-var proposal-count uint u0)

;; SIP-010 Fungible Token Standard Functions

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) err-not-authorized)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (ft-transfer? yield-points amount sender recipient))
    (ok true)
  )
)

(define-read-only (get-name)
  (ok "YieldPoints")
)

(define-read-only (get-symbol)
  (ok "YLD")
)

(define-read-only (get-decimals)
  (ok u6)
)

(define-read-only (get-balance (who principal))
  (ok (ft-get-balance yield-points who))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply yield-points))
)

(define-read-only (get-token-uri)
  (ok none)
)

;; Admin Functions

;; Set an authorized evaluator
(define-public (set-authorized-evaluator (evaluator principal) (authorized bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-evaluators evaluator authorized))
  )
)

;; Initialize or update a contribution type and its weight
(define-public (set-contribution-weight (contribution-type (string-utf8 50)) (weight uint) (description (string-utf8 200)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set contribution-weights {contribution-type: contribution-type} {weight: weight, description: description}))
  )
)
