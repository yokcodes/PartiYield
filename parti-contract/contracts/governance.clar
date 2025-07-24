;; Governance Contract
;; Enables users to create, vote on, and execute governance proposals

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-STAKE (err u102))
(define-constant ERR-VOTING-PERIOD-ENDED (err u103))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-PROPOSAL-NOT-APPROVED (err u106))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u107))
(define-constant ERR-QUORUM-NOT-MET (err u108))
(define-constant ERR-INVALID-VOTING-POWER (err u109))

;; Configuration constants
(define-constant PROPOSAL-STAKE-AMOUNT u1000000) ;; 1 STX in microSTX
(define-constant VOTING-PERIOD-BLOCKS u1440) ;; ~10 days (assuming 10 min blocks)
(define-constant QUORUM-THRESHOLD u20) ;; 20% of total voting power required
(define-constant APPROVAL-THRESHOLD u50) ;; 50% of votes cast must be "yes"

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var total-voting-power uint u0)

;; Proposal structure
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    contract-call: (optional { contract: principal, function: (string-ascii 50), args: (list 10 (buff 32)) }),
    stake-amount: uint,
    created-at: uint,
    voting-end: uint,
    yes-votes: uint,
    no-votes: uint,
    total-votes: uint,
    executed: bool,
    approved: bool
  }
)

;; Track user votes for each proposal
(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

;; Track user voting power (could be based on token holdings, reputation, etc.)
(define-map voting-power
  { user: principal }
  { power: uint }
)

;; Track staked amounts for proposals
(define-map proposal-stakes
  { proposal-id: uint }
  { amount: uint, claimed: bool }
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voting-power (user principal))
  (default-to u0 (get power (map-get? voting-power { user: user })))
)

(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

(define-read-only (get-total-voting-power)
  (var-get total-voting-power)
)

(define-read-only (is-proposal-approved (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal
    (let
      (
        (total-votes (get total-votes proposal))
        (yes-votes (get yes-votes proposal))
        (voting-power-threshold (/ (* (var-get total-voting-power) QUORUM-THRESHOLD) u100))
        (approval-votes-needed (/ (* total-votes APPROVAL-THRESHOLD) u100))
      )
      (and
        (>= total-votes voting-power-threshold) ;; Quorum met
        (>= yes-votes approval-votes-needed) ;; Approval threshold met
        (> (get voting-end proposal) stacks-block-height) ;; Voting period ended
      )
    )
    false
  )
)

(define-read-only (can-execute-proposal (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal
    (and
      (is-proposal-approved proposal-id)
      (not (get executed proposal))
      (<= (get voting-end proposal) stacks-block-height)
    )
    false
  )
)

;; Public functions

;; Set voting power for a user (in a real implementation, this might be automated based on token holdings)
(define-public (set-voting-power (user principal) (power uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (let
      (
        (old-power (get-voting-power user))
        (new-total (+ (- (var-get total-voting-power) old-power) power))
      )
      (map-set voting-power { user: user } { power: power })
      (var-set total-voting-power new-total)
      (ok true)
    )
  )
)

;; Create a new governance proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (contract-call (optional { contract: principal, function: (string-ascii 50), args: (list 10 (buff 32)) }))
)
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (voting-end (+ stacks-block-height VOTING-PERIOD-BLOCKS))
    )
    ;; Check if user has sufficient stake
    (asserts! (>= (stx-get-balance tx-sender) PROPOSAL-STAKE-AMOUNT) ERR-INSUFFICIENT-STAKE)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? PROPOSAL-STAKE-AMOUNT tx-sender (as-contract tx-sender)))
    
    ;; Create proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        contract-call: contract-call,
        stake-amount: PROPOSAL-STAKE-AMOUNT,
        created-at: stacks-block-height,
        voting-end: voting-end,
        yes-votes: u0,
        no-votes: u0,
        total-votes: u0,
        executed: false,
        approved: false
      }
    )
    
    ;; Track stake
    (map-set proposal-stakes
      { proposal-id: proposal-id }
      { amount: PROPOSAL-STAKE-AMOUNT, claimed: false }
    )
    
    ;; Update counter
    (var-set proposal-counter proposal-id)
    
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (voter-power (get-voting-power tx-sender))
      (current-vote (get-vote proposal-id tx-sender))
    )
    ;; Check voting period is active
    (asserts! (< stacks-block-height (get voting-end proposal)) ERR-VOTING-PERIOD-ENDED)
    
    ;; Check user hasn't already voted
    (asserts! (is-none current-vote) ERR-ALREADY-VOTED)
    
    ;; Check user has voting power
    (asserts! (> voter-power u0) ERR-INVALID-VOTING-POWER)
    
    ;; Record vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-yes, voting-power: voter-power }
    )
    
    ;; Update proposal vote counts
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal
        {
          yes-votes: (if vote-yes (+ (get yes-votes proposal) voter-power) (get yes-votes proposal)),
          no-votes: (if vote-yes (get no-votes proposal) (+ (get no-votes proposal) voter-power)),
          total-votes: (+ (get total-votes proposal) voter-power)
        }
      )
    )
    
    (ok true)
  )
)

;; Execute an approved proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
    ;; Check if proposal can be executed
    (asserts! (can-execute-proposal proposal-id) ERR-PROPOSAL-NOT-APPROVED)
    
    ;; Mark as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, approved: true })
    )
    
    ;; Execute contract call if specified
    (match (get contract-call proposal)
      call-data
      (begin
        ;; In a real implementation, you would use contract-call? here
        ;; This is a placeholder for the actual execution logic
        (print { event: "proposal-executed", proposal-id: proposal-id, contract-call: call-data })
        (ok true)
      )
      (begin
        (print { event: "proposal-executed", proposal-id: proposal-id, type: "governance-only" })
        (ok true)
      )
    )
  )
)

;; Claim stake back for approved proposals or after voting period ends
(define-public (claim-proposal-stake (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (stake-info (unwrap! (map-get? proposal-stakes { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    )
    ;; Check if caller is the proposer
    (asserts! (is-eq tx-sender (get proposer proposal)) ERR-UNAUTHORIZED)
    
    ;; Check if stake hasn't been claimed
    (asserts! (not (get claimed stake-info)) ERR-PROPOSAL-NOT-FOUND)
    
    ;; Check if voting period has ended
    (asserts! (>= stacks-block-height (get voting-end proposal)) ERR-VOTING-PERIOD-ACTIVE)
    
    ;; Mark stake as claimed
    (map-set proposal-stakes
      { proposal-id: proposal-id }
      (merge stake-info { claimed: true })
    )
    
    ;; Return stake to proposer
    (try! (as-contract (stx-transfer? (get amount stake-info) tx-sender (get proposer proposal))))
    
    (ok true)
  )
)

;; Emergency functions (only contract owner)

(define-public (emergency-cancel-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Mark proposal as executed to prevent further voting/execution
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, approved: false })
    )
    
    (ok true)
  )
)

;; Initialize contract with some default voting power for testing
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (try! (set-voting-power CONTRACT-OWNER u1000))
    (ok true)
  )
)

;; Events for off-chain indexing
(define-private (emit-proposal-created (proposal-id uint))
  (print { event: "proposal-created", proposal-id: proposal-id, proposer: tx-sender })
)

(define-private (emit-vote-cast (proposal-id uint) (vote bool) (power uint))
  (print { event: "vote-cast", proposal-id: proposal-id, voter: tx-sender, vote: vote, power: power })
)