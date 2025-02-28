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

;; Keep track of all registered assets
(define-map assets 
  { asset-id: uint } 
  { 
    name: (string-ascii 32),
    token-contract: principal,
    token-id: (optional uint),
    is-yield-bearing: bool,
    yield-source: (optional principal),
    current-yield-percentage: uint,
    last-yield-claim-block: uint
  }
)

;; Keep track of portfolio composition/allocations by risk level
(define-map risk-allocations
  { risk-level: uint, asset-id: uint }
  { target-percentage: uint }
)

;; User portfolios
(define-map user-portfolios
  { user: principal }
  {
    risk-level: uint,
    total-btc-value: uint,
    last-rebalance-block: uint,
    custom-allocations: bool,
    active: bool
  }
)

;; User asset balances
(define-map user-asset-balances
  { user: principal, asset-id: uint }
  { amount: uint }
)

;; Historical performance tracking
(define-map portfolio-performance
  { user: principal, timestamp: uint }
  { 
    btc-value: uint,
    percentage-change: int
  }
)

;; Public functions

;; Initialize or update a user's portfolio
(define-public (initialize-portfolio (risk-level uint))
  (begin
    (asserts! (or (is-eq risk-level risk-conservative) 
                (is-eq risk-level risk-moderate) 
                (is-eq risk-level risk-aggressive)) 
            err-invalid-risk-level)
    (map-set user-portfolios 
      { user: tx-sender }
      {
        risk-level: risk-level,
        total-btc-value: u0,
        last-rebalance-block: stacks-block-height,
        custom-allocations: false,
        active: true
      })
    (ok true)))

;; Deposit an asset into the portfolio
(define-public (deposit-asset (asset-id uint) (amount uint))
  (let (
    (asset-info (unwrap! (map-get? assets { asset-id: asset-id }) err-asset-not-exists))
    (portfolio (unwrap! (map-get? user-portfolios { user: tx-sender }) err-invalid-risk-level))
    (token-contract (get token-contract asset-info))
    (token-id (get token-id asset-info))
    (current-balance (default-to u0 (get amount (map-get? user-asset-balances { user: tx-sender, asset-id: asset-id }))))
  )
    (asserts! (> amount u0) err-invalid-amount)
    
    
    ;; Update user's balance
    (map-set user-asset-balances 
      { user: tx-sender, asset-id: asset-id }
      { amount: (+ current-balance amount) })
    
    ;; Update portfolio total value - in a real implementation, you'd calculate BTC value
    (map-set user-portfolios
      { user: tx-sender }
      (merge portfolio { total-btc-value: (+ (get total-btc-value portfolio) amount) }))
    
    (ok true)))


;; Withdraw an asset from the portfolio
(define-public (withdraw-asset (asset-id uint) (amount uint))
  (let (
    (asset-info (unwrap! (map-get? assets { asset-id: asset-id }) err-asset-not-exists))
    (portfolio (unwrap! (map-get? user-portfolios { user: tx-sender }) err-invalid-risk-level))
    (token-contract (get token-contract asset-info))
    (token-id (get token-id asset-info))
    (current-balance (default-to u0 (get amount (map-get? user-asset-balances { user: tx-sender, asset-id: asset-id }))))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    
    ;; Update user's balance
    (map-set user-asset-balances 
      { user: tx-sender, asset-id: asset-id }
      { amount: (- current-balance amount) })
    
    ;; Update portfolio total value
    (map-set user-portfolios
      { user: tx-sender }
      (merge portfolio { total-btc-value: (- (get total-btc-value portfolio) amount) }))
    
    (ok true)))

;; Claim yield from yield-bearing assets
(define-public (claim-yield (asset-id uint))
  (let (
    (asset-info (unwrap! (map-get? assets { asset-id: asset-id }) err-asset-not-exists))
    (user-balance (unwrap! (map-get? user-asset-balances { user: tx-sender, asset-id: asset-id }) err-insufficient-balance))
  )
    (asserts! (get is-yield-bearing asset-info) (err u113))
    (asserts! (is-some (get yield-source asset-info)) (err u114))
    
    
    (let (
      (blocks-since-last-claim (- stacks-block-height (get last-yield-claim-block asset-info)))
      (yield-rate (get current-yield-percentage asset-info))
      (yield-amount (/ (* (get amount user-balance) yield-rate blocks-since-last-claim) u10000))
    )
      ;; Update asset info with new claim block
      (map-set assets
        { asset-id: asset-id }
        (merge asset-info { last-yield-claim-block: stacks-block-height }))
      
      ;; Update user balance with yield
      (map-set user-asset-balances
        { user: tx-sender, asset-id: asset-id }
        { amount: (+ (get amount user-balance) yield-amount) })
      
      (ok yield-amount))))

;; Get a user's portfolio composition
(define-read-only (get-portfolio-composition (user principal))
  (let (
    (portfolio (map-get? user-portfolios { user: user }))
  )
    (if (is-some portfolio)
      (ok (some portfolio))
      (err err-invalid-risk-level))))

;; Get current asset allocation for user
(define-read-only (get-current-allocation (user principal))
  (ok (map-get? user-portfolios { user: user })))

;; Helper to set individual allocation
(define-private (set-allocation (allocation {asset-id: uint, percentage: uint}))
  (let (
    (asset-id (get asset-id allocation))
    (percentage (get percentage allocation))
    (portfolio (unwrap-panic (map-get? user-portfolios { user: tx-sender })))
    (risk-level (get risk-level portfolio))
  )
    (map-set risk-allocations
      { risk-level: risk-level, asset-id: asset-id }
      { target-percentage: percentage })
    true))

;; Calculate current portfolio drift compared to target allocation
(define-private (calculate-portfolio-drift (user principal))
  (let (
    (portfolio (unwrap-panic (map-get? user-portfolios { user: user })))
    (risk-level (get risk-level portfolio))

  )
    u10)) ;; 10% drift from targets

;; Check if a portfolio is active
(define-private (active-portfolio (user principal))
  (let (
    (portfolio (map-get? user-portfolios { user: user }))
  )
    (if (is-some portfolio)
      (get active (unwrap-panic portfolio))
      false)))

;; Record portfolio performance for historical tracking
(define-private (record-performance (user principal))
  (let (
    (portfolio (unwrap-panic (map-get? user-portfolios { user: user })))
    (current-value (get total-btc-value portfolio))
    ;; In a real implementation, you'd calculate actual performance metrics
    (percentage-change 10) ;; Placeholder for 10% increase
  )
    (map-set portfolio-performance
      { user: user, timestamp: stacks-block-height }
      { 
        btc-value: current-value,
        percentage-change: percentage-change
      })
    (ok true)))



;; Admin functions

;; Register a new asset that can be managed in portfolios
(define-public (register-asset (asset-id uint) 
                             (name (string-ascii 32)) 
                             (token-contract principal)
                             (token-id (optional uint))
                             (is-yield-bearing bool)
                             (yield-source (optional principal))
                             (initial-yield-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-some (map-get? assets { asset-id: asset-id }))) err-asset-exists)
    
    (map-set assets
      { asset-id: asset-id }
      {
        name: name,
        token-contract: token-contract,
        token-id: token-id,
        is-yield-bearing: is-yield-bearing,
        yield-source: yield-source,
        current-yield-percentage: initial-yield-percentage,
        last-yield-claim-block: stacks-block-height
      })
    
    (ok true)))

;; Update risk allocation for a specific asset
(define-public (set-risk-allocation (risk-level uint) (asset-id uint) (percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (or (is-eq risk-level risk-conservative) 
                (is-eq risk-level risk-moderate) 
                (is-eq risk-level risk-aggressive)) 
            err-invalid-risk-level)
    (asserts! (is-some (map-get? assets { asset-id: asset-id })) err-asset-not-exists)
    (asserts! (and (>= percentage u0) (<= percentage u100)) err-invalid-threshold)
    
    (map-set risk-allocations
      { risk-level: risk-level, asset-id: asset-id }
      { target-percentage: percentage })
    
    (ok true)))

;; Update the rebalance frequency (in blocks)
(define-public (set-rebalance-frequency (blocks uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> blocks u0) err-invalid-threshold)
    (var-set rebalance-frequency blocks)
    (ok true)))

;; Update the minimum rebalance threshold
(define-public (set-minimum-rebalance-threshold (percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (> percentage u0) (< percentage u100)) err-invalid-threshold)
    (var-set minimum-rebalance-threshold percentage)
    (ok true)))

;; Update maximum allowed slippage
(define-public (set-max-slippage (percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= percentage u0) (< percentage u50)) err-invalid-threshold)
    (var-set max-slippage-percentage percentage)
    (ok true)))

;; Set the performance fee percentage
(define-public (set-performance-fee (percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= percentage u0) (< percentage u30)) err-invalid-threshold)
    (var-set performance-fee-percentage percentage)
    (ok true)))

;; Run a simulation of different allocation strategies
(define-public (simulate-strategy (risk-level uint) (scenario uint))
  (begin
    (asserts! (or (is-eq risk-level risk-conservative) 
                (is-eq risk-level risk-moderate) 
                (is-eq risk-level risk-aggressive)) 
            err-invalid-risk-level)
    
    ;; Placeholder for simulation logic
    ;; In a real implementation, this would run different scenarios and
    ;; return projected performance metrics
    
    (ok u10) ;; Placeholder return for 10% projected growth
  ))

  ;; Get a simulation result
(define-read-only (get-simulation-result (simulation-id uint))
  ;; Placeholder for retrieving simulation results
  (ok {
    projected-growth: u10,
    max-drawdown: u5,
    sharpe-ratio: u200,
    risk-level: risk-moderate
  }))

;; DCA configuration for users
(define-map dca-configurations
  { user: principal }
  {
    active: bool,
    frequency-blocks: uint,
    amount-per-period: uint,
    target-asset-id: uint,
    last-execution-block: uint,
    source-asset-id: uint
  }
)

;; ==================== NEW FEATURE: PORTFOLIO MANAGER DELEGATION ====================
;; Allow users to delegate portfolio management
(define-map delegated-managers
  { user: principal, manager: principal }
  {
    active: bool,
    expiration-height: uint,
    fee-percentage: uint,
    can-withdraw: bool
  }
)

;; Manager performance tracking
(define-map manager-performance
  { manager: principal }
  {
    total-users: uint,
    average-return: int,
    assets-under-management: uint
  }
)

;; Emergency controls
(define-data-var emergency-mode bool false)
(define-data-var emergency-admin principal contract-owner)

;; Predefined strategy templates
(define-map strategy-templates
  { strategy-id: uint }
  {
    name: (string-ascii 32),
    description: (string-utf8 256),
    risk-level: uint,
    allocations: (list 10 {asset-id: uint, percentage: uint}),
    minimum-investment: uint,
    rebalance-frequency: uint
  }
)


;; Follow other users' strategies
(define-map strategy-followers
  { follower: principal, leader: principal }
  {
    active: bool,
    auto-rebalance: bool,
    allocation-percentage: uint
  }
)

;; Platform user limits
(define-data-var max-users uint u1000)
(define-data-var current-users uint u0)

;; Stop loss and take profit settings
(define-map risk-controls
  { user: principal }
  {
    stop-loss-percentage: uint,
    take-profit-percentage: uint,
    stop-loss-active: bool,
    take-profit-active: bool
  }
)

;; Set up dollar-cost averaging
(define-public (configure-dca (frequency-blocks uint) (amount uint) (target-asset-id uint) (source-asset-id uint))
  (begin
    (asserts! (> frequency-blocks u0) err-invalid-threshold)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-some (map-get? assets { asset-id: target-asset-id })) err-asset-not-exists)
    (asserts! (is-some (map-get? assets { asset-id: source-asset-id })) err-asset-not-exists)
    
    
    (map-set dca-configurations
      { user: tx-sender }
      {
        active: true,
        frequency-blocks: frequency-blocks,
        amount-per-period: amount,
        target-asset-id: target-asset-id,
        last-execution-block: stacks-block-height,
        source-asset-id: source-asset-id
      })
    
    (ok true)))

;; Delegate portfolio management to another user
(define-public (delegate-management (manager principal) (expiration-blocks uint) (fee-percentage uint) (can-withdraw bool))
  (begin
    (asserts! (not (is-eq tx-sender manager)) (err u124))
    (asserts! (< fee-percentage u30) err-invalid-threshold)
    
    (map-set delegated-managers
      { user: tx-sender, manager: manager }
      {
        active: true,
        expiration-height: (+ stacks-block-height expiration-blocks),
        fee-percentage: fee-percentage,
        can-withdraw: can-withdraw
      })
    
    ;; Update manager's stats
    (let (
      (manager-stats (default-to 
                      { total-users: u0, average-return: 0, assets-under-management: u0 }
                      (map-get? manager-performance { manager: manager })))
      (portfolio (unwrap! (map-get? user-portfolios { user: tx-sender }) err-invalid-risk-level))
    )
      (map-set manager-performance
        { manager: manager }
        { 
          total-users: (+ (get total-users manager-stats) u1),
          average-return: (get average-return manager-stats),
          assets-under-management: (+ (get assets-under-management manager-stats) (get total-btc-value portfolio))
        })
    )
    
    (ok true)))

;; Stop following a strategy
(define-public (unfollow-strategy (leader principal))
  (let (
    (follow-data (unwrap! (map-get? strategy-followers { follower: tx-sender, leader: leader }) (err u126)))
  )
    (map-set strategy-followers
      { follower: tx-sender, leader: leader }
      (merge follow-data { active: false }))
    
    (ok true)))

;; Copy leader's allocations to follower (proportionally to allocation percentage)
(define-private (copy-leader-allocations (follower principal) (leader principal) (allocation-percentage uint))
  (let (
    (leader-portfolio (unwrap! (map-get? user-portfolios { user: leader }) err-invalid-risk-level))
    (follower-portfolio (unwrap! (map-get? user-portfolios { user: follower }) err-invalid-risk-level))
    ;; In real implementation, copy the asset allocations proportionally
  )
    ;; Placeholder for copying allocations
    (ok true)))

;; Rebalance all followers of a leader
(define-private (rebalance-followers (leader principal))
  (begin
    ;; In a real implementation, you would:
    ;; 1. Query all followers of this leader
    ;; 2. For each follower with auto-rebalance=true, update their allocations
    
    ;; Placeholder for rebalancing followers
    (ok true)))

;; Enable emergency mode (admin only)
(define-public (enable-emergency-mode)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set emergency-mode true)
    (ok true)))

