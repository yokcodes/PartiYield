;; Contribution Tracker Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-owner (err u100))
(define-constant err-already-submitted (err u101))
(define-constant err-not-found (err u102))
(define-constant err-already-verified (err u103))

;; Define data maps
(define-map contributions
  { id: uint }
  {
    contributor: principal,
    metadata: (string-ascii 256),
    verified: bool,
    votes: uint
  }
)

(define-map user-votes
  { user: principal, contribution-id: uint }
  { voted: bool }
)

;; Define variables
(define-data-var contribution-counter uint u0)

;; Submit a contribution
(define-public (submit-contribution (metadata (string-ascii 256)))
  (let
    (
      (contribution-id (+ (var-get contribution-counter) u1))
    )
    (map-set contributions
      { id: contribution-id }
      {
        contributor: tx-sender,
        metadata: metadata,
        verified: false,
        votes: u0
      }
    )
    (var-set contribution-counter contribution-id)
    (ok contribution-id)
  )
)

;; Vote for a contribution
(define-public (vote-for-contribution (contribution-id uint))
  (let
    (
      (contribution (unwrap! (map-get? contributions { id: contribution-id }) (err err-not-found)))
      (user-vote (default-to { voted: false } (map-get? user-votes { user: tx-sender, contribution-id: contribution-id })))
    )
    (asserts! (not (get verified contribution)) (err err-already-verified))
    (asserts! (not (get voted user-vote)) (err err-already-submitted))
    
    (map-set user-votes
      { user: tx-sender, contribution-id: contribution-id }
      { voted: true }
    )
    
    (map-set contributions
      { id: contribution-id }
      (merge contribution { votes: (+ (get votes contribution) u1) })
    )
    
    (ok true)
  )
)

;; Verify a contribution (only contract owner can do this)
(define-public (verify-contribution (contribution-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-not-owner))
    (match (map-get? contributions { id: contribution-id })
      contribution
        (ok (map-set contributions
          { id: contribution-id }
          (merge contribution { verified: true })
        ))
      (err err-not-found)
    )
  )
)

;; Get contribution details
(define-read-only (get-contribution (contribution-id uint))
  (map-get? contributions { id: contribution-id })
)

;; Get the total number of contributions
(define-read-only (get-contribution-count)
  (var-get contribution-counter)
)

;; Check if a user has voted for a specific contribution
(define-read-only (has-voted (user principal) (contribution-id uint))
  (default-to
    false
    (get voted (map-get? user-votes { user: user, contribution-id: contribution-id }))
  )
)