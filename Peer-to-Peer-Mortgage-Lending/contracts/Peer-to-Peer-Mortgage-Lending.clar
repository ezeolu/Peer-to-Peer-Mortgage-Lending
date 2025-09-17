;; Peer-to-Peer Mortgage Lending Contract with Fractionalized Risk
;; Enables direct lending for real estate with distributed risk among multiple lenders

;; Contract constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVALID_AMOUNT (err u402))
(define-constant ERR_LOAN_NOT_FOUND (err u403))
(define-constant ERR_LOAN_FUNDED (err u404))
(define-constant ERR_LOAN_NOT_FUNDED (err u405))
(define-constant ERR_PAYMENT_FAILED (err u406))
(define-constant ERR_LOAN_DEFAULTED (err u407))
(define-constant ERR_INVALID_TERM (err u408))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u409))
(define-constant MIN_LOAN_AMOUNT u100000) ;; 1000 STX minimum
(define-constant MAX_LOAN_AMOUNT u50000000) ;; 500,000 STX maximum
(define-constant MIN_COLLATERAL_RATIO u150) ;; 150% collateral requirement

;; Data structures
(define-map loans
    { loan-id: uint }
    {
        borrower: principal,
        loan-amount: uint,
        interest-rate: uint, ;; basis points (e.g., 500 = 5%)
        term-months: uint,
        collateral-value: uint,
        property-address: (string-ascii 256),
        monthly-payment: uint,
        total-funded: uint,
        payments-made: uint,
        status: (string-ascii 20), ;; "active", "funding", "defaulted", "completed"
        created-at: uint,
        funded-at: (optional uint)
    }
)

(define-map lender-contributions
    { loan-id: uint, lender: principal }
    {
        amount: uint,
        share-percentage: uint,
        earnings-withdrawn: uint
    }
)

(define-map loan-payments
    { loan-id: uint, payment-number: uint }
    {
        amount: uint,
        principal-portion: uint,
        interest-portion: uint,
        paid-at: uint,
        paid-by: principal
    }
)

;; Global variables
(define-data-var loan-counter uint u0)
(define-data-var platform-fee-rate uint u50) ;; 0.5% platform fee

;; Read-only functions
(define-read-only (get-loan (loan-id uint))
    (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-lender-contribution (loan-id uint) (lender principal))
    (map-get? lender-contributions { loan-id: loan-id, lender: lender })
)

(define-read-only (get-loan-payment (loan-id uint) (payment-number uint))
    (map-get? loan-payments { loan-id: loan-id, payment-number: payment-number })
)

(define-read-only (calculate-monthly-payment (principal uint) (annual-rate uint) (months uint))
    (let
        (
            (monthly-rate (/ annual-rate u1200)) ;; Convert annual rate to monthly decimal
            (rate-factor (pow (+ u100 monthly-rate) months))
        )
        (if (> monthly-rate u0)
            (/ (* principal (* monthly-rate rate-factor)) (- rate-factor u100))
            (/ principal months)
        )
    )
)

(define-read-only (get-loan-status (loan-id uint))
    (match (get-loan loan-id)
        loan-data (get status loan-data)
        "not-found"
    )
)

;; Private functions
(define-private (validate-loan-terms (amount uint) (rate uint) (term uint) (collateral uint))
    (and
        (>= amount MIN_LOAN_AMOUNT)
        (<= amount MAX_LOAN_AMOUNT)
        (> rate u0)
        (<= rate u2000) ;; Max 20% annual rate
        (>= term u12) ;; Min 1 year
        (<= term u360) ;; Max 30 years
        (>= (* collateral u100) (* amount MIN_COLLATERAL_RATIO))
    )
)

(define-private (distribute-payment (loan-id uint) (payment-amount uint))
    (let
        (
            (loan-data (unwrap! (get-loan loan-id) (err u404)))
            (platform-fee (/ (* payment-amount (var-get platform-fee-rate)) u10000))
            (net-payment (- payment-amount platform-fee))
        )
        ;; Transfer platform fee to contract owner
        (try! (stx-transfer? platform-fee tx-sender CONTRACT_OWNER))
        
        ;; The actual distribution to lenders would require iterating through all lenders
        ;; For simplicity, this returns the net payment amount
        (ok net-payment)
    )
)

;; Public functions
(define-public (create-loan 
    (loan-amount uint) 
    (interest-rate uint) 
    (term-months uint) 
    (collateral-value uint)
    (property-address (string-ascii 256))
)
    (let
        (
            (loan-id (+ (var-get loan-counter) u1))
            (monthly-payment (calculate-monthly-payment loan-amount interest-rate term-months))
        )
        
        ;; Validate loan terms
        (asserts! (validate-loan-terms loan-amount interest-rate term-months collateral-value) ERR_INVALID_TERM)
        
        ;; Create loan record
        (map-set loans
            { loan-id: loan-id }
            {
                borrower: tx-sender,
                loan-amount: loan-amount,
                interest-rate: interest-rate,
                term-months: term-months,
                collateral-value: collateral-value,
                property-address: property-address,
                monthly-payment: monthly-payment,
                total-funded: u0,
                payments-made: u0,
                status: "funding",
                created-at: block-height,
                funded-at: none
            }
        )
        
        ;; Update loan counter
        (var-set loan-counter loan-id)
        
        (ok loan-id)
    )
)

(define-public (fund-loan (loan-id uint) (amount uint))
    (let
        (
            (loan-data (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
            (current-funded (get total-funded loan-data))
            (loan-amount (get loan-amount loan-data))
            (new-total (+ current-funded amount))
        )
        
        ;; Validate funding conditions
        (asserts! (is-eq (get status loan-data) "funding") ERR_LOAN_FUNDED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= new-total loan-amount) ERR_INVALID_AMOUNT)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Calculate lender's share percentage
        (let
            (
                (share-percentage (/ (* amount u10000) loan-amount))
            )
            
            ;; Record lender contribution
            (map-set lender-contributions
                { loan-id: loan-id, lender: tx-sender }
                {
                    amount: amount,
                    share-percentage: share-percentage,
                    earnings-withdrawn: u0
                }
            )
            
            ;; Update loan funding status
            (map-set loans
                { loan-id: loan-id }
                (merge loan-data { total-funded: new-total })
            )
            
            ;; If fully funded, activate loan and transfer to borrower
            (if (is-eq new-total loan-amount)
                (begin
                    (map-set loans
                        { loan-id: loan-id }
                        (merge loan-data { 
                            total-funded: new-total,
                            status: "active",
                            funded-at: (some block-height)
                        })
                    )
                    ;; Transfer loan amount to borrower
                    (try! (as-contract (stx-transfer? loan-amount tx-sender (get borrower loan-data))))
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-public (make-payment (loan-id uint) (payment-amount uint))
    (let
        (
            (loan-data (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
            (monthly-payment (get monthly-payment loan-data))
            (payments-made (get payments-made loan-data))
            (new-payments-made (+ payments-made u1))
        )
        
        ;; Validate payment conditions
        (asserts! (is-eq (get status loan-data) "active") ERR_LOAN_NOT_FUNDED)
        (asserts! (is-eq tx-sender (get borrower loan-data)) ERR_UNAUTHORIZED)
        (asserts! (>= payment-amount monthly-payment) ERR_INVALID_AMOUNT)
        
        ;; Process payment distribution
        (try! (distribute-payment loan-id payment-amount))
        
        ;; Calculate principal and interest portions (simplified)
        (let
            (
                (interest-portion (/ (* (get loan-amount loan-data) (get interest-rate loan-data)) u1200))
                (principal-portion (- payment-amount interest-portion))
            )
            
            ;; Record payment
            (map-set loan-payments
                { loan-id: loan-id, payment-number: new-payments-made }
                {
                    amount: payment-amount,
                    principal-portion: principal-portion,
                    interest-portion: interest-portion,
                    paid-at: block-height,
                    paid-by: tx-sender
                }
            )
            
            ;; Update loan status
            (map-set loans
                { loan-id: loan-id }
                (merge loan-data { payments-made: new-payments-made })
            )
            
            ;; Check if loan is completed
            (if (is-eq new-payments-made (get term-months loan-data))
                (begin
                    (map-set loans
                        { loan-id: loan-id }
                        (merge loan-data { 
                            payments-made: new-payments-made,
                            status: "completed"
                        })
                    )
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-public (withdraw-earnings (loan-id uint))
    (let
        (
            (contribution (unwrap! (get-lender-contribution loan-id tx-sender) ERR_UNAUTHORIZED))
            (loan-data (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
        )
        
        ;; Calculate earnings based on payments made and lender's share
        ;; This is a simplified calculation - in practice would need more complex logic
        (let
            (
                (total-payments (* (get payments-made loan-data) (get monthly-payment loan-data)))
                (lender-share (/ (* total-payments (get share-percentage contribution)) u10000))
                (available-earnings (- lender-share (get earnings-withdrawn contribution)))
            )
            
            (asserts! (> available-earnings u0) ERR_INVALID_AMOUNT)
            
            ;; Transfer earnings to lender
            (try! (as-contract (stx-transfer? available-earnings tx-sender tx-sender)))
            
            ;; Update withdrawn amount
            (map-set lender-contributions
                { loan-id: loan-id, lender: tx-sender }
                (merge contribution { 
                    earnings-withdrawn: (+ (get earnings-withdrawn contribution) available-earnings)
                })
            )
            
            (ok available-earnings)
        )
    )
)

(define-public (mark-default (loan-id uint))
    (let
        (
            (loan-data (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
        )
        
        ;; Only contract owner can mark defaults (in practice, would have more sophisticated logic)
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status loan-data) "active") ERR_LOAN_NOT_FUNDED)
        
        ;; Mark loan as defaulted
        (map-set loans
            { loan-id: loan-id }
            (merge loan-data { status: "defaulted" })
        )
        
        (ok true)
    )
)

;; Administrative functions
(define-public (update-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= new-fee u1000) ERR_INVALID_AMOUNT) ;; Max 10% fee
        (var-set platform-fee-rate new-fee)
        (ok true)
    )
)