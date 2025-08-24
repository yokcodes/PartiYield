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

;; Events - Changed to non-optional tuple type
(define-data-var event-voting-power-updated {user: principal, old-power: uint, new-power: uint, yield-points: uint} {user: tx-sender, old-power: u0, new-power: u0, yield-points: u0})

;; Helper function to calculate square root (integer approximation)
;; Uses iterative approach since Clarity doesn't support recursion
(define-private (isqrt (n uint))
  (if (is-eq n u0)
    u0
    (if (is-eq n u1)
      u1
      (if (<= n u3)
        u1
        (if (<= n u8)
          (if (<= n u3) u1 u2)
          (if (<= n u15)
            (if (<= n u8) u2 u3)
            (if (<= n u24)
              (if (<= n u15) u3 u4)
              (if (<= n u35)
                (if (<= n u24) u4 u5)
                (if (<= n u48)
                  (if (<= n u35) u5 u6)
                  (if (<= n u63)
                    (if (<= n u48) u6 u7)
                    (if (<= n u80)
                      (if (<= n u63) u7 u8)
                      (if (<= n u99)
                        (if (<= n u80) u8 u9)
                        (if (<= n u120)
                          (if (<= n u99) u9 u10)
                          (if (<= n u143)
                            (if (<= n u120) u10 u11)
                            (if (<= n u168)
                              (if (<= n u143) u11 u12)
                              (if (<= n u195)
                                (if (<= n u168) u12 u13)
                                (if (<= n u224)
                                  (if (<= n u195) u13 u14)
                                  (if (<= n u255)
                                    (if (<= n u224) u14 u15)
                                    (if (<= n u288)
                                      (if (<= n u255) u15 u16)
                                      (if (<= n u323)
                                        (if (<= n u288) u16 u17)
                                        (if (<= n u360)
                                          (if (<= n u323) u17 u18)
                                          (if (<= n u399)
                                            (if (<= n u360) u18 u19)
                                            (if (<= n u440)
                                              (if (<= n u399) u19 u20)
                                              (if (<= n u483)
                                                (if (<= n u440) u20 u21)
                                                (if (<= n u528)
                                                  (if (<= n u483) u21 u22)
                                                  (if (<= n u575)
                                                    (if (<= n u528) u22 u23)
                                                    (if (<= n u624)
                                                      (if (<= n u575) u23 u24)
                                                      (if (<= n u675)
                                                        (if (<= n u624) u24 u25)
                                                        (if (<= n u728)
                                                          (if (<= n u675) u25 u26)
                                                          (if (<= n u783)
                                                            (if (<= n u728) u26 u27)
                                                            (if (<= n u840)
                                                              (if (<= n u783) u27 u28)
                                                              (if (<= n u899)
                                                                (if (<= n u840) u28 u29)
                                                                (if (<= n u960)
                                                                  (if (<= n u899) u29 u30)
                                                                  (if (<= n u1023)
                                                                    (if (<= n u960) u30 u31)
                                                                    (if (<= n u1088)
                                                                      (if (<= n u1023) u31 u32)
                                                                      (if (<= n u1155)
                                                                        (if (<= n u1088) u32 u33)
                                                                        (if (<= n u1224)
                                                                          (if (<= n u1155) u33 u34)
                                                                          (if (<= n u1295)
                                                                            (if (<= n u1224) u34 u35)
                                                                            (if (<= n u1368)
                                                                              (if (<= n u1295) u35 u36)
                                                                              (if (<= n u1443)
                                                                                (if (<= n u1368) u36 u37)
                                                                                (if (<= n u1520)
                                                                                  (if (<= n u1443) u37 u38)
                                                                                  (if (<= n u1599)
                                                                                    (if (<= n u1520) u38 u39)
                                                                                    (if (<= n u1680)
                                                                                      (if (<= n u1599) u39 u40)
                                                                                      (if (<= n u1763)
                                                                                        (if (<= n u1680) u40 u41)
                                                                                        (if (<= n u1848)
                                                                                          (if (<= n u1763) u41 u42)
                                                                                          (if (<= n u1935)
                                                                                            (if (<= n u1848) u42 u43)
                                                                                            (if (<= n u2024)
                                                                                              (if (<= n u1935) u43 u44)
                                                                                              (if (<= n u2115)
                                                                                                (if (<= n u2024) u44 u45)
                                                                                                (if (<= n u2208)
                                                                                                  (if (<= n u2115) u45 u46)
                                                                                                  (if (<= n u2303)
                                                                                                    (if (<= n u2208) u46 u47)
                                                                                                    (if (<= n u2400)
                                                                                                      (if (<= n u2303) u47 u48)
                                                                                                      (if (<= n u2499)
                                                                                                        (if (<= n u2400) u48 u49)
                                                                                                        (if (<= n u2600)
                                                                                                          (if (<= n u2499) u49 u50)
                                                                                                          u50)))))))))))))))))))))))))))))))))))))))))))))))))))))

;; Calculate voting power from yield points using quadratic voting - MOVED AFTER isqrt
(define-private (calculate-voting-power (yield-points uint))
  (isqrt yield-points))

;; Internal function to update user's voting power - PRIVATE HELPER
(define-private (internal-update-voting-power (user principal) (new-yield-points uint))
  (let ((old-voting-power (default-to u0 (map-get? user-voting-power user)))
        (new-voting-power (calculate-voting-power new-yield-points)))
    (begin
      ;; Update user's voting power
      (map-set user-voting-power user new-voting-power)
      
      ;; Update total voting power
      (var-set total-voting-power 
        (+ (- (var-get total-voting-power) old-voting-power) new-voting-power))
      
      ;; Emit event (simulate with data-var since Clarity doesn't have events)
      (var-set event-voting-power-updated 
        {user: user, old-power: old-voting-power, new-power: new-voting-power, yield-points: new-yield-points})
      
      new-voting-power)))

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
        
        ;; Update voting power using internal function
        (ok (internal-update-voting-power user amount))))))

;; Add yield points to user's balance
(define-public (add-yield-points (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let ((current-points (default-to u0 (map-get? user-yield-points user)))
          (new-total (+ current-points amount)))
      ;; Check for overflow
      (asserts! (>= new-total current-points) ERR-OVERFLOW)
      (begin
        ;; Update yield points directly
        (map-set user-yield-points user new-total)
        
        ;; Update total yield points
        (var-set total-yield-points 
          (+ (var-get total-yield-points) amount))
        
        ;; Update voting power
        (ok (internal-update-voting-power user new-total))))))

;; Subtract yield points from user's balance
(define-public (subtract-yield-points (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let ((current-points (default-to u0 (map-get? user-yield-points user)))
          (new-total (- current-points amount)))
      (asserts! (>= current-points amount) ERR-INVALID-AMOUNT)
      (begin
        ;; Update yield points directly
        (map-set user-yield-points user new-total)
        
        ;; Update total yield points
        (var-set total-yield-points 
          (- (var-get total-yield-points) amount))
        
        ;; Update voting power
        (ok (internal-update-voting-power user new-total))))))

;; Create a snapshot of voting power for a proposal
(define-public (snapshot-voting-power-for-proposal (proposal-id uint) (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let ((current-voting-power (get-voting-power user)))
      (map-set voting-power-snapshots 
        {proposal-id: proposal-id, user: user} 
        current-voting-power)
      (ok current-voting-power))))

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

;; Calculate what voting power would be for given yield points (without updating) - STANDALONE
(define-read-only (preview-voting-power (yield-points uint))
  (isqrt yield-points))

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

;; Get the last event data
(define-read-only (get-last-event)
  (var-get event-voting-power-updated))

;; Administrative functions

;; Update next proposal ID (for proposal management)
(define-public (increment-proposal-id)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let ((current-id (var-get next-proposal-id)))
      (var-set next-proposal-id (+ current-id u1))
      (ok current-id))))