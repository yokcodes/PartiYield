;; Voting Power Calculator Contract
;; Implements quadratic voting based on YieldPoints to prevent whale dominance

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-USER-NOT-FOUND (err u102))
(define-constant ERR-OVERFLOW (err u103))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map user-yield-points principal uint)
(define-map user-voting-power principal uint)
(define-map voting-power-snapshots { proposal-id: uint, user: principal } uint)

;; Contract variables
(define-data-var total-yield-points uint u0)
(define-data-var total-voting-power uint u0)
(define-data-var next-proposal-id uint u1)

;; Events
(define-data-var event-voting-power-updated {user: principal, old-power: uint, new-power: uint, yield-points: uint} (some {user: tx-sender, old-power: u0, new-power: u0, yield-points: u0}))



;; Helper function to calculate square root (integer approximation)
;; Uses Newton's method for integer square root
(define-private (isqrt (n uint))
  (if (is-eq n u0)
    u0
    (let ((x n)
          (y (+ (/ n u2) u1)))
      (if (<= y x)
        
        x))))


;; Calculate voting power from yield points using quadratic voting ()
(define-private (calculate-voting-power (yield-points uint))
  (isqrt yield-points))

;; Update user's voting power based on their yield points
(define-private (update-voting-power (user principal))
  (let ((current-yield-points (default-to u0 (map-get? user-yield-points user)))
        (old-voting-power (default-to u0 (map-get? user-voting-power user)))
        (new-voting-power (calculate-voting-power current-yield-points)))
    (begin
      ;; Update user's voting power
      (map-set user-voting-power user new-voting-power)
      
      ;; Update total voting power
      (var-set total-voting-power 
        (+ (- (var-get total-voting-power) old-voting-power) new-voting-power))
      
      ;; Emit event (simulate with data-var since Clarity doesn't have events)
      (var-set event-voting-power-updated 
        (some {user: user, old-power: old-voting-power, new-power: new-voting-power, yield-points: current-yield-points}))
      
      (ok new-voting-power))))

;; Public functions

;; Initialize or update yield points for a user
(define-public (set-yield-points (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let ((old-yield-points (default-to u0 (map-get? user-yield-points user))))
      (begin
        ;; Update yield points
        (map-set user-yield-points user amount)
        
        ;; Update total yield points
        (var-set total-yield-points 
          (+ (- (var-get total-yield-points) old-yield-points) amount))
        
        ;; Update voting power
        (update-voting-power user)))))

;; Add yield points to user's balance
(define-public (add-yield-points (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let ((current-points (default-to u0 (map-get? user-yield-points user)))
          (new-total (+ current-points amount)))
      ;; Check for overflow
      (asserts! (>= new-total current-points) ERR-OVERFLOW)
      (set-yield-points user new-total))))

;; Subtract yield points from user's balance
(define-public (subtract-yield-points (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let ((current-points (default-to u0 (map-get? user-yield-points user))))
      (asserts! (>= current-points amount) ERR-INVALID-AMOUNT)
      (set-yield-points user (- current-points amount)))))



;; Read-only functions

;; Get user's current yield points
(define-read-only (get-yield-points (user principal))
  (default-to u0 (map-get? user-yield-points user)))

;; Get user's current voting power
(define-read-only (get-voting-power (user principal))
  (default-to u0 (map-get? user-voting-power user)))

;; Get user's voting power for a specific proposal (from snapshot)
(define-read-only (get-voting-power-for-proposal (proposal-id uint) (user principal))
  (default-to u0 (map-get? voting-power-snapshots {proposal-id: proposal-id, user: user})))

;; Get total yield points in the system
(define-read-only (get-total-yield-points)
  (var-get total-yield-points))

;; Get total voting power in the system
(define-read-only (get-total-voting-power)
  (var-get total-voting-power))

;; Calculate what voting power would be for given yield points (without updating)
(define-read-only (preview-voting-power (yield-points uint))
  (calculate-voting-power yield-points))


;; Utility function to check if user has minimum voting power
(define-read-only (has-minimum-voting-power (user principal) (minimum uint))
  (>= (get-voting-power user) minimum))

;; Get voting power statistics
(define-read-only (get-voting-stats)
  {
    total-yield-points: (var-get total-yield-points),
    total-voting-power: (var-get total-voting-power),
    next-proposal-id: (var-get next-proposal-id)
  })

;; Administrative functions

;; Update next proposal ID (for proposal management)
(define-public (increment-proposal-id)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let ((current-id (var-get next-proposal-id)))
      (var-set next-proposal-id (+ current-id u1))
      (ok current-id))))

