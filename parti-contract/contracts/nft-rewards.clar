;; NFT Rewards Contract for Backers
;; Manages tiered NFT rewards based on backing contributions

;; Define the NFT
(define-non-fungible-token backer-reward uint)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-CLAIMED (err u101))
(define-constant ERR-INVALID-TIER (err u102))
(define-constant ERR-INSUFFICIENT-BACKING (err u103))
(define-constant ERR-NFT-NOT-FOUND (err u104))
(define-constant ERR-CAMPAIGN-NOT-ACTIVE (err u105))
(define-constant ERR-INVALID-AMOUNT (err u106))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data variables
(define-data-var next-token-id uint u1)
(define-data-var campaign-active bool true)
(define-data-var base-uri (string-ascii 256) "https://api.rewards.com/metadata/")

;; Reward tiers (backing amount in microSTX -> tier level)
(define-map reward-tiers uint {
  name: (string-ascii 50),
  min-backing: uint,
  max-supply: uint,
  current-supply: uint,
  metadata-suffix: (string-ascii 50)
})

;; Backer information
(define-map backers principal {
  total-backed: uint,
  nfts-claimed: (list 10 uint),
  tier-claimed: uint
})

;; NFT metadata
(define-map token-metadata uint {
  tier: uint,
  backer: principal,
  mint-timestamp: uint,
  backing-amount: uint
})

;; Initialize reward tiers
(map-set reward-tiers u1 {
  name: "Bronze Supporter",
  min-backing: u1000000, ;; 1 STX
  max-supply: u1000,
  current-supply: u0,
  metadata-suffix: "bronze"
})

(map-set reward-tiers u2 {
  name: "Silver Supporter", 
  min-backing: u5000000, ;; 5 STX
  max-supply: u500,
  current-supply: u0,
  metadata-suffix: "silver"
})

(map-set reward-tiers u3 {
  name: "Gold Supporter",
  min-backing: u10000000, ;; 10 STX
  max-supply: u200,
  current-supply: u0,
  metadata-suffix: "gold"
})

(map-set reward-tiers u4 {
  name: "Platinum Supporter",
  min-backing: u25000000, ;; 25 STX
  max-supply: u50,
  current-supply: u0,
  metadata-suffix: "platinum"
})

(map-set reward-tiers u5 {
  name: "Diamond Supporter",
  min-backing: u50000000, ;; 50 STX
  max-supply: u10,
  current-supply: u0,
  metadata-suffix: "diamond"
})

;; Read-only functions

(define-read-only (get-last-token-id)
  (ok (- (var-get next-token-id) u1))
)

(define-read-only (get-token-uri (token-id uint))
  (match (map-get? token-metadata token-id)
    metadata (ok (some (concat (concat (var-get base-uri) (get metadata-suffix (unwrap! (map-get? reward-tiers (get tier metadata)) (err ERR-INVALID-TIER)))) (concat "/" (uint-to-ascii token-id)))))
    (err ERR-NFT-NOT-FOUND)
  )
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? backer-reward token-id))
)

(define-read-only (get-backer-info (backer principal))
  (ok (map-get? backers backer))
)

(define-read-only (get-tier-info (tier uint))
  (ok (map-get? reward-tiers tier))
)

(define-read-only (get-token-metadata (token-id uint))
  (ok (map-get? token-metadata token-id))
)

(define-read-only (get-eligible-tier (backing-amount uint))
  (if (>= backing-amount u50000000) (ok u5)
  (if (>= backing-amount u25000000) (ok u4)
  (if (>= backing-amount u10000000) (ok u3)
  (if (>= backing-amount u5000000) (ok u2)
  (if (>= backing-amount u1000000) (ok u1)
  (ok u0))))))
)

;; Fixed return type consistency - now returns (response bool uint) instead of mixed types
(define-read-only (can-claim-tier (backer principal) (tier uint))
  (let (
    (backer-data (default-to {total-backed: u0, nfts-claimed: (list), tier-claimed: u0} (map-get? backers backer)))
    (tier-data (map-get? reward-tiers tier))
  )
    (match tier-data
      tier-info (ok (and 
        (>= (get total-backed backer-data) (get min-backing tier-info))
        (< (get tier-claimed backer-data) tier)
        (< (get current-supply tier-info) (get max-supply tier-info))
      ))
      ERR-INVALID-TIER
    )
  )
)

;; Private functions

(define-private (uint-to-ascii (value uint))
  (if (<= value u9)
    (unwrap-panic (element-at "0123456789" value))
    (get r (fold uint-to-ascii-fold 
      (list u1 u1 u1 u1 u1 u1 u1 u1 u1 u1) 
      {v: value, r: ""}))
  )
)

(define-private (uint-to-ascii-fold (i uint) (acc {v: uint, r: (string-ascii 10)}))
  (if (> (get v acc) u0)
    {
      v: (/ (get v acc) u10),
      r: (unwrap-panic (as-max-len? (concat (unwrap-panic (element-at "0123456789" (mod (get v acc) u10))) (get r acc)) u10))
    }
    acc
  )
)

;; Public functions

(define-public (record-backing (backer principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (var-get campaign-active) ERR-CAMPAIGN-NOT-ACTIVE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (let (
      (current-data (default-to {total-backed: u0, nfts-claimed: (list), tier-claimed: u0} (map-get? backers backer)))
      (new-total (+ (get total-backed current-data) amount))
    )
      (map-set backers backer (merge current-data {total-backed: new-total}))
      (ok new-total)
    )
  )
)

(define-public (claim-nft-reward (tier uint))
  (let (
    (token-id (var-get next-token-id))
    (backer tx-sender)
    (backer-data (unwrap! (map-get? backers backer) ERR-INSUFFICIENT-BACKING))
    (tier-data (unwrap! (map-get? reward-tiers tier) ERR-INVALID-TIER))
    ;; Updated to handle the new response type from can-claim-tier
    (can-claim (unwrap! (can-claim-tier backer tier) ERR-INVALID-TIER))
  )
    (asserts! (var-get campaign-active) ERR-CAMPAIGN-NOT-ACTIVE)
    (asserts! can-claim ERR-INSUFFICIENT-BACKING)
    
    ;; Mint the NFT
    (try! (nft-mint? backer-reward token-id backer))
    
    ;; Update token metadata
    (map-set token-metadata token-id {
      tier: tier,
      backer: backer,
      mint-timestamp: block-height,
      backing-amount: (get total-backed backer-data)
    })
    
    ;; Update backer data
    (map-set backers backer (merge backer-data {
      nfts-claimed: (unwrap! (as-max-len? (append (get nfts-claimed backer-data) token-id) u10) ERR-NOT-AUTHORIZED),
      tier-claimed: tier
    }))
    
    ;; Update tier supply
    (map-set reward-tiers tier (merge tier-data {
      current-supply: (+ (get current-supply tier-data) u1)
    }))
    
    ;; Increment next token ID
    (var-set next-token-id (+ token-id u1))
    
    (ok token-id)
  )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (try! (nft-transfer? backer-reward token-id sender recipient))
    (ok true)
  )
)

;; Admin functions

(define-public (set-campaign-active (active bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set campaign-active active)
    (ok active)
  )
)

(define-public (set-base-uri (new-uri (string-ascii 256)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set base-uri new-uri)
    (ok new-uri)
  )
)

(define-public (update-tier (tier uint) (name (string-ascii 50)) (min-backing uint) (max-supply uint) (metadata-suffix (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let (
      (current-tier (unwrap! (map-get? reward-tiers tier) ERR-INVALID-TIER))
    )
      (map-set reward-tiers tier {
        name: name,
        min-backing: min-backing,
        max-supply: max-supply,
        current-supply: (get current-supply current-tier),
        metadata-suffix: metadata-suffix
      })
      (ok true)
    )
  )
)

(define-public (emergency-mint (recipient principal) (tier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let (
      (token-id (var-get next-token-id))
      (tier-data (unwrap! (map-get? reward-tiers tier) ERR-INVALID-TIER))
    )
      (asserts! (< (get current-supply tier-data) (get max-supply tier-data)) ERR-INVALID-TIER)
      
      (try! (nft-mint? backer-reward token-id recipient))
      
      (map-set token-metadata token-id {
        tier: tier,
        backer: recipient,
        mint-timestamp: block-height,
        backing-amount: u0
      })
      
      (map-set reward-tiers tier (merge tier-data {
        current-supply: (+ (get current-supply tier-data) u1)
      }))
      
      (var-set next-token-id (+ token-id u1))
      (ok token-id)
    )
  )
)
