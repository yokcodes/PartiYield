;; Staking and Slashing Contract
;; Ensures contribution authenticity and prevents Sybil attacks

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_STAKE (err u101))
(define-constant ERR_CONTRIBUTION_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_VERIFIED (err u103))
(define-constant ERR_INVALID_STATUS (err u104))
(define-constant ERR_INSUFFICIENT_BALANCE (err u105))
(define-constant ERR_ALREADY_STAKED (err u106))

;; Minimum stake required for contributions
(define-constant MIN_STAKE_AMOUNT u1000000) ;; 1 STX in microSTX

;; Data Variables
(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var total-staked uint u0)
(define-data-var total-slashed uint u0)

;; Data Maps
;; Track user stakes
(define-map user-stakes 
  { user: principal }
  { 
    amount: uint,
    locked: bool,
    contribution-id: (optional uint)
  }
)

;; Track contributions
(define-map contributions
  { contribution-id: uint }
  {
    contributor: principal,
    stake-amount: uint,
    content-hash: (buff 32),
    timestamp: uint,
    status: (string-ascii 20), ;; "pending", "verified", "slashed"
    verifier: (optional principal)
  }
)

;; Track contribution counter
(define-data-var next-contribution-id uint u1)

;; Track verifiers (authorized to verify contributions)
(define-map authorized-verifiers principal bool)

;; Events
(define-map contribution-events
  { event-id: uint }
  {
    event-type: (string-ascii 20),
    contribution-id: uint,
    user: principal,
    amount: uint,
    timestamp: uint
  }
)

(define-data-var next-event-id uint u1)

;; Private Functions

;; Emit event
(define-private (emit-event (event-type (string-ascii 20)) (contribution-id uint) (user principal) (amount uint))
  (let ((event-id (var-get next-event-id)))
    (map-set contribution-events
      { event-id: event-id }
      {
        event-type: event-type,
        contribution-id: contribution-id,
        user: user,
        amount: amount,
        timestamp: stacks-block-height
      }
    )
    (var-set next-event-id (+ event-id u1))
    event-id
  )
)

;; Check if user is authorized verifier
(define-private (is-authorized-verifier (user principal))
  (default-to false (map-get? authorized-verifiers user))
)

;; Public Functions

;; Initialize contract (only owner)
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    ;; Add contract owner as initial verifier
    (map-set authorized-verifiers (var-get contract-owner) true)
    (ok true)
  )
)

;; Add authorized verifier (only owner)
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set authorized-verifiers verifier true)
    (ok true)
  )
)

;; Remove authorized verifier (only owner)
(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-delete authorized-verifiers verifier)
    (ok true)
  )
)

;; Stake tokens for contribution
(define-public (stake-for-contribution (stake-amount uint) (content-hash (buff 32)))
  (let (
    (current-stake (map-get? user-stakes { user: tx-sender }))
    (contribution-id (var-get next-contribution-id))
  )
    ;; Check minimum stake amount
    (asserts! (>= stake-amount MIN_STAKE_AMOUNT) ERR_INSUFFICIENT_STAKE)
    
    ;; Check user doesn't already have an active stake
    (asserts! (is-none current-stake) ERR_ALREADY_STAKED)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Record user stake
    (map-set user-stakes
      { user: tx-sender }
      {
        amount: stake-amount,
        locked: true,
        contribution-id: (some contribution-id)
      }
    )
    
    ;; Record contribution
    (map-set contributions
      { contribution-id: contribution-id }
      {
        contributor: tx-sender,
        stake-amount: stake-amount,
        content-hash: content-hash,
        timestamp: stacks-block-height,
        status: "pending",
        verifier: none
      }
    )
    
    ;; Update counters
    (var-set next-contribution-id (+ contribution-id u1))
    (var-set total-staked (+ (var-get total-staked) stake-amount))
    
    ;; Emit event
    (emit-event "staked" contribution-id tx-sender stake-amount)
    
    (ok contribution-id)
  )
)

;; Verify contribution (only authorized verifiers)
(define-public (verify-contribution (contribution-id uint))
  (let (
    (contribution (unwrap! (map-get? contributions { contribution-id: contribution-id }) ERR_CONTRIBUTION_NOT_FOUND))
    (contributor (get contributor contribution))
    (stake-amount (get stake-amount contribution))
    (current-status (get status contribution))
  )
    ;; Check verifier authorization
    (asserts! (is-authorized-verifier tx-sender) ERR_UNAUTHORIZED)
    
    ;; Check contribution is pending
    (asserts! (is-eq current-status "pending") ERR_ALREADY_VERIFIED)
    
    ;; Update contribution status
    (map-set contributions
      { contribution-id: contribution-id }
      (merge contribution { 
        status: "verified",
        verifier: (some tx-sender)
      })
    )
    
    ;; Return staked tokens to contributor
    (try! (as-contract (stx-transfer? stake-amount tx-sender contributor)))
    
    ;; Clear user stake
    (map-delete user-stakes { user: contributor })
    
    ;; Update total staked
    (var-set total-staked (- (var-get total-staked) stake-amount))
    
    ;; Emit event
    (emit-event "verified" contribution-id contributor stake-amount)
    
    (ok true)
  )
)

;; Slash contribution for fraud/low quality (only authorized verifiers)
(define-public (slash-contribution (contribution-id uint))
  (let (
    (contribution (unwrap! (map-get? contributions { contribution-id: contribution-id }) ERR_CONTRIBUTION_NOT_FOUND))
    (contributor (get contributor contribution))
    (stake-amount (get stake-amount contribution))
    (current-status (get status contribution))
  )
    ;; Check verifier authorization
    (asserts! (is-authorized-verifier tx-sender) ERR_UNAUTHORIZED)
    
    ;; Check contribution is pending
    (asserts! (is-eq current-status "pending") ERR_INVALID_STATUS)
    
    ;; Update contribution status
    (map-set contributions
      { contribution-id: contribution-id }
      (merge contribution { 
        status: "slashed",
        verifier: (some tx-sender)
      })
    )
    
    ;; Clear user stake (tokens remain in contract as penalty)
    (map-delete user-stakes { user: contributor })
    
    ;; Update counters
    (var-set total-staked (- (var-get total-staked) stake-amount))
    (var-set total-slashed (+ (var-get total-slashed) stake-amount))
    
    ;; Emit event
    (emit-event "slashed" contribution-id contributor stake-amount)
    
    (ok true)
  )
)

;; Emergency withdraw stake (only if contribution is still pending and user wants to cancel)
(define-public (withdraw-stake)
  (let (
    (user-stake (unwrap! (map-get? user-stakes { user: tx-sender }) ERR_CONTRIBUTION_NOT_FOUND))
    (stake-amount (get amount user-stake))
    (contribution-id-opt (get contribution-id user-stake))
    (contribution-id (unwrap! contribution-id-opt ERR_CONTRIBUTION_NOT_FOUND))
    (contribution (unwrap! (map-get? contributions { contribution-id: contribution-id }) ERR_CONTRIBUTION_NOT_FOUND))
  )
    ;; Check contribution is still pending
    (asserts! (is-eq (get status contribution) "pending") ERR_INVALID_STATUS)
    
    ;; Return tokens
    (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
    
    ;; Clear stake and contribution
    (map-delete user-stakes { user: tx-sender })
    (map-delete contributions { contribution-id: contribution-id })
    
    ;; Update total staked
    (var-set total-staked (- (var-get total-staked) stake-amount))
    
    ;; Emit event
    (emit-event "withdrawn" contribution-id tx-sender stake-amount)
    
    (ok true)
  )
)

;; Withdraw slashed funds (only owner)
(define-public (withdraw-slashed-funds (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= amount (var-get total-slashed)) ERR_INSUFFICIENT_BALANCE)
    
    (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))
    (var-set total-slashed (- (var-get total-slashed) amount))
    
    (ok true)
  )
)

;; Read-only Functions

;; Get user stake info
(define-read-only (get-user-stake (user principal))
  (map-get? user-stakes { user: user })
)

;; Get contribution info
(define-read-only (get-contribution (contribution-id uint))
  (map-get? contributions { contribution-id: contribution-id })
)

;; Get contract stats
(define-read-only (get-contract-stats)
  {
    total-staked: (var-get total-staked),
    total-slashed: (var-get total-slashed),
    next-contribution-id: (var-get next-contribution-id)
  }
)

;; Check if user is verifier
(define-read-only (is-verifier (user principal))
  (is-authorized-verifier user)
)

;; Get contribution event
(define-read-only (get-event (event-id uint))
  (map-get? contribution-events { event-id: event-id })
)

;; Get minimum stake amount
(define-read-only (get-min-stake-amount)
  MIN_STAKE_AMOUNT
)