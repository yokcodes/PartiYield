;; Retroactive Rewards Contract
;; Allows users to claim YieldPoints for past contributions with community validation

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u200))
(define-constant ERR-INVALID-CLAIM (err u201))
(define-constant ERR-CLAIM-NOT-FOUND (err u202))
(define-constant ERR-ALREADY-CLAIMED (err u203))
(define-constant ERR-ALREADY-VALIDATED (err u204))
(define-constant ERR-INSUFFICIENT-VALIDATORS (err u205))
(define-constant ERR-VALIDATION-ENDED (err u206))
(define-constant ERR-NOT-VALIDATOR (err u207))
(define-constant ERR-INVALID-PROOF (err u208))
(define-constant ERR-CONTRACT-PAUSED (err u209))
(define-constant ERR-INVALID-CONTRIBUTION-TYPE (err u210))
(define-constant ERR-DUPLICATE-PROOF (err u211))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-PROOF-LENGTH u500)
(define-constant MAX-DESCRIPTION-LENGTH u300)
(define-constant VALIDATION-PERIOD u1008) ;; ~7 days in blocks

;; Data variables
(define-data-var contract-admin principal CONTRACT-OWNER)
(define-data-var yield-points-contract (optional principal) none)
(define-data-var is-paused bool false)
(define-data-var next-claim-id uint u1)

;; Validation parameters
(define-data-var min-validators-required uint u3)
(define-data-var approval-threshold uint u66) ;; 66% approval required
(define-data-var validator-reward uint u10) ;; YieldPoints reward for validators
(define-data-var max-retroactive-amount uint u10000) ;; Max YieldPoints per claim

;; Contribution types and their multipliers
(define-map contribution-types
  { type-name: (string-ascii 30) }
  { 
    multiplier: uint, ;; Base multiplier (100 = 1x, 150 = 1.5x)
    max-amount: uint, ;; Max YieldPoints for this type
    enabled: bool,
    description: (string-ascii 100)
  }
)

;; Validator registry
(define-map validators
  { validator: principal }
  { 
    is-active: bool,
    reputation-score: uint,
    total-validations: uint,
    correct-validations: uint,
    registered-at: uint
  }
)

;; Retroactive claims
(define-map retroactive-claims
  { claim-id: uint }
  {
    claimant: principal,
    contribution-type: (string-ascii 30),
    description: (string-ascii 300),
    proof-url: (string-ascii 200),
    proof-hash: (buff 32),
    requested-amount: uint,
    submission-block: uint,
    validation-end-block: uint,
    status: (string-ascii 20), ;; "pending", "approved", "rejected", "expired"
    approvals: uint,
    rejections: uint,
    total-validators: uint,
    final-amount: uint,
    claimed: bool
  }
)

;; Validator votes on claims
(define-map claim-validations
  { claim-id: uint, validator: principal }
  { 
    vote: bool, ;; true = approve, false = reject
    comments: (string-ascii 200),
    vote-weight: uint,
    voted-at: uint
  }
)

;; Track proof hashes to prevent duplicates
(define-map proof-registry
  { proof-hash: (buff 32) }
  { 
    claim-id: uint,
    claimant: principal,
    used-at: uint
  }
)

;; User claim history
(define-map user-claim-stats
  { user: principal }
  { 
    total-claims: uint,
    approved-claims: uint,
    total-awarded: uint,
    last-claim-block: uint
  }
)

;; Read-only functions

(define-read-only (get-contract-info)
  {
    admin: (var-get contract-admin),
    yield-points-contract: (var-get yield-points-contract),
    is-paused: (var-get is-paused),
    next-claim-id: (var-get next-claim-id),
    min-validators: (var-get min-validators-required),
    approval-threshold: (var-get approval-threshold),
    validator-reward: (var-get validator-reward),
    max-retroactive-amount: (var-get max-retroactive-amount)
  }
)

(define-read-only (get-contribution-type (type-name (string-ascii 30)))
  (map-get? contribution-types { type-name: type-name })
)

(define-read-only (get-validator-info (validator principal))
  (map-get? validators { validator: validator })
)

(define-read-only (get-claim (claim-id uint))
  (map-get? retroactive-claims { claim-id: claim-id })
)

(define-read-only (get-claim-validation (claim-id uint) (validator principal))
  (map-get? claim-validations { claim-id: claim-id, validator: validator })
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { total-claims: u0, approved-claims: u0, total-awarded: u0, last-claim-block: u0 }
    (map-get? user-claim-stats { user: user })
  )
)

(define-read-only (is-validator (user principal))
  (default-to false (get is-active (map-get? validators { validator: user })))
)

(define-read-only (is-proof-used (proof-hash (buff 32)))
  (is-some (map-get? proof-registry { proof-hash: proof-hash }))
)

(define-read-only (calculate-final-reward (claim-id uint))
  (match (get-claim claim-id)
    claim
    (let 
      (
        (contribution-type-name (get contribution-type claim))
        (requested-amount (get requested-amount claim))
        (approvals (get approvals claim))
        (total-validators (get total-validators claim))
        (approval-rate (if (> total-validators u0) 
                         (/ (* approvals u100) total-validators) 
                         u0))
      )
      (match (get-contribution-type contribution-type-name)
        type-info
        (let 
          (
            (base-multiplier (get multiplier type-info))
            (max-for-type (get max-amount type-info))
            ;; Apply approval rate as confidence multiplier
            (confidence-multiplier (/ approval-rate u100))
            (adjusted-amount (/ (* requested-amount base-multiplier confidence-multiplier) u10000))
            (final-amount (if (> adjusted-amount max-for-type) max-for-type adjusted-amount))
          )
          (ok final-amount)
        )
        ERR-INVALID-CONTRIBUTION-TYPE
      )
    )
    ERR-CLAIM-NOT-FOUND
  )
)

(define-read-only (get-claim-status (claim-id uint))
  (match (get-claim claim-id)
    claim
    (let 
      (
        (current-block stacks-block-height)
        (end-block (get validation-end-block claim))
        (approvals (get approvals claim))
        (rejections (get rejections claim))
        (total-votes (+ approvals rejections))
        (min-validators (var-get min-validators-required))
        (threshold (var-get approval-threshold))
        (approval-needed (/ (* total-votes threshold) u100))
      )
      {
        status: (get status claim),
        approvals: approvals,
        rejections: rejections,
        total-votes: total-votes,
        validation-complete: (>= current-block end-block),
        min-validators-met: (>= total-votes min-validators),
        approval-threshold-met: (>= approvals approval-needed),
        blocks-remaining: (if (>= current-block end-block) u0 (- end-block current-block))
      }
    )
    { status: "not-found", approvals: u0, rejections: u0, total-votes: u0, 
      validation-complete: false, min-validators-met: false, 
      approval-threshold-met: false, blocks-remaining: u0 }
  )
)

;; Admin functions

(define-public (set-yield-points-contract (contract-address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    (var-set yield-points-contract (some contract-address))
    (ok true)
  )
)

(define-public (update-validation-params 
  (min-validators uint) 
  (threshold uint) 
  (validator-reward-amount uint)
  (max-amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    (asserts! (and (>= threshold u1) (<= threshold u100)) ERR-INVALID-CLAIM)
    (asserts! (>= min-validators u1) ERR-INVALID-CLAIM)
    
    (var-set min-validators-required min-validators)
    (var-set approval-threshold threshold)
    (var-set validator-reward validator-reward-amount)
    (var-set max-retroactive-amount max-amount)
    
    (ok true)
  )
)

(define-public (add-contribution-type 
  (type-name (string-ascii 30)) 
  (multiplier uint) 
  (max-amount uint)
  (description (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (> multiplier u0) ERR-INVALID-CLAIM)
    
    (map-set contribution-types
      { type-name: type-name }
      { 
        multiplier: multiplier,
        max-amount: max-amount,
        enabled: true,
        description: description
      }
    )
    
    (ok true)
  )
)

(define-public (update-contribution-type 
  (type-name (string-ascii 30)) 
  (multiplier uint) 
  (max-amount uint)
  (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    (asserts! (is-some (get-contribution-type type-name)) ERR-INVALID-CONTRIBUTION-TYPE)
    
    (map-set contribution-types
      { type-name: type-name }
      (merge (unwrap-panic (get-contribution-type type-name))
             { multiplier: multiplier, max-amount: max-amount, enabled: enabled })
    )
    
    (ok true)
  )
)

(define-public (register-validator (validator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
    
    (map-set validators
      { validator: validator }
      { 
        is-active: true,
        reputation-score: u100, ;; Starting reputation
        total-validations: u0,
        correct-validations: u0,
        registered-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (deactivate-validator (validator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    
    (match (get-validator-info validator)
      validator-info
      (begin
        (map-set validators
          { validator: validator }
          (merge validator-info { is-active: false })
        )
        (ok true)
      )
      ERR-NOT-VALIDATOR
    )
  )
)

;; User functions

(define-public (submit-retroactive-claim 
  (contribution-type (string-ascii 30))
  (description (string-ascii 300))
  (proof-url (string-ascii 200))
  (proof-hash (buff 32))
  (requested-amount uint))
  (let 
    (
      (claim-id (var-get next-claim-id))
      (user-stats (get-user-stats tx-sender))
    )
    (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (is-some (get-contribution-type contribution-type)) ERR-INVALID-CONTRIBUTION-TYPE)
    (asserts! (not (is-proof-used proof-hash)) ERR-DUPLICATE-PROOF)
    (asserts! (> requested-amount u0) ERR-INVALID-CLAIM)
    (asserts! (<= requested-amount (var-get max-retroactive-amount)) ERR-INVALID-CLAIM)
    
    ;; Verify contribution type is enabled
    (let ((type-info (unwrap! (get-contribution-type contribution-type) ERR-INVALID-CONTRIBUTION-TYPE)))
      (asserts! (get enabled type-info) ERR-INVALID-CONTRIBUTION-TYPE)
      (asserts! (<= requested-amount (get max-amount type-info)) ERR-INVALID-CLAIM)
    )
    
    ;; Create the claim
    (map-set retroactive-claims
      { claim-id: claim-id }
      {
        claimant: tx-sender,
        contribution-type: contribution-type,
        description: description,
        proof-url: proof-url,
        proof-hash: proof-hash,
        requested-amount: requested-amount,
        submission-block: stacks-block-height,
        validation-end-block: (+ stacks-block-height VALIDATION-PERIOD),
        status: "pending",
        approvals: u0,
        rejections: u0,
        total-validators: u0,
        final-amount: u0,
        claimed: false
      }
    )
    
    ;; Register proof hash
    (map-set proof-registry
      { proof-hash: proof-hash }
      { 
        claim-id: claim-id,
        claimant: tx-sender,
        used-at: stacks-block-height
      }
    )
    
    ;; Update user stats
    (map-set user-claim-stats
      { user: tx-sender }
      (merge user-stats 
             { total-claims: (+ (get total-claims user-stats) u1),
               last-claim-block: stacks-block-height })
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (validate-claim (claim-id uint) (approve bool) (comments (string-ascii 200)))
  (let 
    (
      (claim (unwrap! (get-claim claim-id) ERR-CLAIM-NOT-FOUND))
      (validator-info (unwrap! (get-validator-info tx-sender) ERR-NOT-VALIDATOR))
      (existing-vote (get-claim-validation claim-id tx-sender))
    )
    (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (get is-active validator-info) ERR-NOT-VALIDATOR)
    (asserts! (is-none existing-vote) ERR-ALREADY-VALIDATED)
    (asserts! (<= stacks-block-height (get validation-end-block claim)) ERR-VALIDATION-ENDED)
    (asserts! (is-eq (get status claim) "pending") ERR-INVALID-CLAIM)
    
    (let 
      (
        (vote-weight (get reputation-score validator-info))
        (updated-approvals (if approve (+ (get approvals claim) vote-weight) (get approvals claim)))
        (updated-rejections (if approve (get rejections claim) (+ (get rejections claim) vote-weight)))
        (updated-total (+ (get total-validators claim) u1))
      )
      
      ;; Record the validation
      (map-set claim-validations
        { claim-id: claim-id, validator: tx-sender }
        { 
          vote: approve,
          comments: comments,
          vote-weight: vote-weight,
          voted-at: stacks-block-height
        }
      )
      
      ;; Update claim with new vote counts
      (map-set retroactive-claims
        { claim-id: claim-id }
        (merge claim 
               { approvals: updated-approvals,
                 rejections: updated-rejections,
                 total-validators: updated-total })
      )
      
      ;; Update validator stats
      (map-set validators
        { validator: tx-sender }
        (merge validator-info 
               { total-validations: (+ (get total-validations validator-info) u1) })
      )
      
      (ok true)
    )
  )
)

(define-public (finalize-claim (claim-id uint))
  (let 
    (
      (claim (unwrap! (get-claim claim-id) ERR-CLAIM-NOT-FOUND))
      (claim-status (get-claim-status claim-id))
    )
    (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (get validation-complete claim-status) ERR-VALIDATION-ENDED)
    (asserts! (is-eq (get status claim) "pending") ERR-INVALID-CLAIM)
    (asserts! (get min-validators-met claim-status) ERR-INSUFFICIENT-VALIDATORS)
    
    (let 
      (
        (approved (get approval-threshold-met claim-status))
        (new-status (if approved "approved" "rejected"))
        (final-reward-result (if approved (calculate-final-reward claim-id) (ok u0)))
        (final-reward (unwrap! final-reward-result ERR-INVALID-CLAIM))
      )
      
      ;; Update claim status
      (map-set retroactive-claims
        { claim-id: claim-id }
        (merge claim 
               { status: new-status,
                 final-amount: final-reward })
      )
      
      ;; Update user stats if approved
      (if approved
        (let ((user-stats (get-user-stats (get claimant claim))))
          (map-set user-claim-stats
            { user: (get claimant claim) }
            (merge user-stats 
                   { approved-claims: (+ (get approved-claims user-stats) u1),
                     total-awarded: (+ (get total-awarded user-stats) final-reward) })
          )
        )
        true
      )
      
      (ok { status: new-status, final-amount: final-reward })
    )
  )
)

(define-public (claim-rewards (claim-id uint))
  (let 
    (
      (claim (unwrap! (get-claim claim-id) ERR-CLAIM-NOT-FOUND))
      (yield-contract (unwrap! (var-get yield-points-contract) ERR-UNAUTHORIZED))
    )
    (asserts! (not (var-get is-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (is-eq tx-sender (get claimant claim)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status claim) "approved") ERR-INVALID-CLAIM)
    (asserts! (not (get claimed claim)) ERR-ALREADY-CLAIMED)
    (asserts! (> (get final-amount claim) u0) ERR-INVALID-CLAIM)
    
    ;; Mark as claimed
    (map-set retroactive-claims
      { claim-id: claim-id }
      (merge claim { claimed: true })
    )
    
    ;; TODO: Integrate with YieldPoints contract to mint tokens
    ;; For now, we'll just return success
    ;; (contract-call? yield-contract mint (get claimant claim) (get final-amount claim))
    
    (ok (get final-amount claim))
  )
)

;; Emergency functions

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    (var-set is-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    (var-set is-paused false)
    (ok true)
  )
)

(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-UNAUTHORIZED)
    (var-set contract-admin new-admin)
    (ok true)
  )
)