;; ResearchChain: Academic Research Collaboration & Publication Protocol
;; Version: 1.0.0

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-STUDY-NOT-FOUND (err u2))
(define-constant ERR-INVALID-FUNDING (err u3))
(define-constant ERR-INVALID-DURATION (err u4))
(define-constant ERR-INVALID-TITLE (err u5))
(define-constant ERR-INVALID-ABSTRACT (err u6))
(define-constant ERR-STUDY-INACTIVE (err u7))
(define-constant ERR-ALREADY-JOINED (err u8))
(define-constant ERR-NOT-JOINED (err u9))
(define-constant ERR-INSUFFICIENT-FUNDS (err u10))
(define-constant ERR-STUDY-NOT-COMPLETED (err u11))
(define-constant ERR-ALREADY-PUBLISHED (err u12))
(define-constant ERR-INVALID-FIELD (err u13))
(define-constant ERR-INVALID-METHODOLOGY (err u14))
(define-constant ERR-COLLABORATION-EXPIRED (err u15))
(define-constant ERR-INVALID-PROGRESS (err u16))

;; Constants
(define-constant MIN-FUNDING u5000000) ;; 5 STX minimum
(define-constant MAX-FUNDING u500000000000) ;; 500k STX maximum
(define-constant MIN-DURATION u604800) ;; 1 week minimum
(define-constant MAX-DURATION u31536000) ;; 1 year maximum
(define-constant PLATFORM-FEE-PERCENT u3) ;; 3% platform fee
(define-constant COMPLETION-THRESHOLD u90) ;; 90% minimum progress for publication

;; Data variables
(define-data-var next-study-id uint u1)
(define-data-var next-collaboration-id uint u1)
(define-data-var research-treasury principal tx-sender)
(define-data-var total-research-fees uint u0)

;; Academic study structure
(define-map academic-studies
  uint
  {
    lead-researcher: principal,
    study-title: (string-utf8 100),
    study-abstract: (string-utf8 500),
    research-field: (string-utf8 20),
    methodology: (string-utf8 15),
    collaboration-funding: uint,
    participation-bond: uint,
    study-duration: uint,
    is-active: bool,
    total-collaborators: uint,
    total-publications: uint,
    created-at: uint
  })

;; Research collaboration structure
(define-map research-collaborations
  uint
  {
    collaborator: principal,
    study-id: uint,
    joined-at: uint,
    expires-at: uint,
    progress-score: uint,
    is-completed: bool,
    is-published: bool,
    bond-secured: uint
  })

;; Collaborator study access mapping
(define-map collaborator-study-access
  { collaborator: principal, study-id: uint }
  uint)

;; Research publication records
(define-map research-publications
  { collaborator: principal, study-id: uint }
  {
    published-at: uint,
    final-progress: uint,
    publication-hash: (string-utf8 64)
  })

;; Private validation functions
(define-private (validate-field (research-field (string-utf8 20)))
  (or 
    (is-eq research-field u"Biology")
    (is-eq research-field u"Chemistry")
    (is-eq research-field u"Physics")
    (is-eq research-field u"Mathematics")
    (is-eq research-field u"Computer Science")
    (is-eq research-field u"Psychology")
    (is-eq research-field u"Sociology")
    (is-eq research-field u"Economics")
  ))

(define-private (validate-methodology (methodology (string-utf8 15)))
  (or 
    (is-eq methodology u"Experimental")
    (is-eq methodology u"Observational")
    (is-eq methodology u"Theoretical")
    (is-eq methodology u"Computational")
    (is-eq methodology u"Mixed Methods")
  ))

(define-private (validate-text-length (text (string-utf8 500)) (min-length uint) (max-length uint))
  (let 
    (
      (text-length (len text))
    )
    (and 
      (>= text-length min-length)
      (<= text-length max-length)
    )
  ))

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount PLATFORM-FEE-PERCENT) u100))

(define-private (calculate-researcher-amount (amount uint))
  (- amount (calculate-platform-fee amount)))

(define-private (validate-bond-amount (bond-amount uint))
  (and (>= bond-amount u0) (<= bond-amount u50000000000))) ;; Max 50k STX bond

(define-private (validate-publication-hash (publication-hash (string-utf8 64)))
  (and (>= (len publication-hash) u32) (<= (len publication-hash) u64)))

;; Public functions

;; Create a new academic study
(define-public (create-academic-study 
  (study-title (string-utf8 100))
  (study-abstract (string-utf8 500))
  (research-field (string-utf8 20))
  (methodology (string-utf8 15))
  (collaboration-funding uint)
  (participation-bond uint)
  (study-duration uint))
  (let
    (
      (study-id (var-get next-study-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    ;; Validate inputs
    (asserts! (validate-text-length study-title u10 u100) ERR-INVALID-TITLE)
    (asserts! (validate-text-length study-abstract u50 u500) ERR-INVALID-ABSTRACT)
    (asserts! (validate-field research-field) ERR-INVALID-FIELD)
    (asserts! (validate-methodology methodology) ERR-INVALID-METHODOLOGY)
    (asserts! (and (>= collaboration-funding MIN-FUNDING) (<= collaboration-funding MAX-FUNDING)) ERR-INVALID-FUNDING)
    (asserts! (and (>= study-duration MIN-DURATION) (<= study-duration MAX-DURATION)) ERR-INVALID-DURATION)
    (asserts! (validate-bond-amount participation-bond) ERR-INVALID-FUNDING)
    
    ;; Create study
    (map-set academic-studies study-id {
      lead-researcher: tx-sender,
      study-title: study-title,
      study-abstract: study-abstract,
      research-field: research-field,
      methodology: methodology,
      collaboration-funding: collaboration-funding,
      participation-bond: participation-bond,
      study-duration: study-duration,
      is-active: true,
      total-collaborators: u0,
      total-publications: u0,
      created-at: current-time
    })
    
    (var-set next-study-id (+ study-id u1))
    (ok study-id)
  ))

;; Join academic study with participation bond
(define-public (join-study (study-id uint))
  (let
    (
      (study (unwrap! (map-get? academic-studies study-id) ERR-STUDY-NOT-FOUND))
      (collaboration-id (var-get next-collaboration-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (expires-at (+ current-time (get study-duration study)))
      (total-cost (+ (get collaboration-funding study) (get participation-bond study)))
      (platform-fee (calculate-platform-fee (get collaboration-funding study)))
      (researcher-amount (calculate-researcher-amount (get collaboration-funding study)))
    )
    ;; Validate study is active
    (asserts! (get is-active study) ERR-STUDY-INACTIVE)
    
    ;; Check if already joined
    (asserts! (is-none (map-get? collaborator-study-access { collaborator: tx-sender, study-id: study-id })) ERR-ALREADY-JOINED)
    
    ;; Transfer payment to researcher and platform fee
    (try! (stx-transfer? researcher-amount tx-sender (get lead-researcher study)))
    (try! (stx-transfer? platform-fee tx-sender (var-get research-treasury)))
    
    ;; Lock participation bond (simulated by requiring balance)
    (asserts! (>= (stx-get-balance tx-sender) (get participation-bond study)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Create collaboration record
    (map-set research-collaborations collaboration-id {
      collaborator: tx-sender,
      study-id: study-id,
      joined-at: current-time,
      expires-at: expires-at,
      progress-score: u0,
      is-completed: false,
      is-published: false,
      bond-secured: (get participation-bond study)
    })
    
    ;; Map collaborator to access
    (map-set collaborator-study-access { collaborator: tx-sender, study-id: study-id } collaboration-id)
    
    ;; Update study stats
    (map-set academic-studies study-id (merge study { total-collaborators: (+ (get total-collaborators study) u1) }))
    
    ;; Update research fees
    (var-set total-research-fees (+ (var-get total-research-fees) platform-fee))
    (var-set next-collaboration-id (+ collaboration-id u1))
    
    (ok collaboration-id)
  ))

;; Update research progress
(define-public (update-progress (study-id uint) (progress-score uint))
  (let
    (
      (collaboration-id (unwrap! (map-get? collaborator-study-access { collaborator: tx-sender, study-id: study-id }) ERR-NOT-JOINED))
      (collaboration-record (unwrap! (map-get? research-collaborations collaboration-id) ERR-NOT-JOINED))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    ;; Validate collaboration is active
    (asserts! (< current-time (get expires-at collaboration-record)) ERR-COLLABORATION-EXPIRED)
    (asserts! (<= progress-score u100) ERR-INVALID-PROGRESS)
    (asserts! (>= progress-score (get progress-score collaboration-record)) ERR-INVALID-PROGRESS)
    
    ;; Update progress
    (map-set research-collaborations collaboration-id (merge collaboration-record { 
      progress-score: progress-score,
      is-completed: (>= progress-score u100)
    }))
    
    (ok true)
  ))

;; Publish research results
(define-public (publish-research (study-id uint) (publication-hash (string-utf8 64)))
  (let
    (
      (collaboration-id (unwrap! (map-get? collaborator-study-access { collaborator: tx-sender, study-id: study-id }) ERR-NOT-JOINED))
      (collaboration-record (unwrap! (map-get? research-collaborations collaboration-id) ERR-NOT-JOINED))
      (study (unwrap! (map-get? academic-studies study-id) ERR-STUDY-NOT-FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (validated-study-id (get study-id collaboration-record))
      (validated-hash publication-hash)
    )
    ;; Additional validations
    (asserts! (validate-publication-hash publication-hash) ERR-INVALID-ABSTRACT)
    (asserts! (is-eq study-id validated-study-id) ERR-STUDY-NOT-FOUND)
    
    ;; Validate completion and score
    (asserts! (get is-completed collaboration-record) ERR-STUDY-NOT-COMPLETED)
    (asserts! (>= (get progress-score collaboration-record) COMPLETION-THRESHOLD) ERR-STUDY-NOT-COMPLETED)
    (asserts! (not (get is-published collaboration-record)) ERR-ALREADY-PUBLISHED)
    
    ;; Publish research
    (map-set research-publications { collaborator: tx-sender, study-id: validated-study-id } {
      published-at: current-time,
      final-progress: (get progress-score collaboration-record),
      publication-hash: validated-hash
    })
    
    ;; Update collaboration record
    (map-set research-collaborations collaboration-id (merge collaboration-record { is-published: true }))
    
    ;; Update study stats
    (map-set academic-studies validated-study-id (merge study { total-publications: (+ (get total-publications study) u1) }))
    
    ;; Return bond to collaborator (simulated)
    (ok true)
  ))

;; Deactivate study (lead researcher only)
(define-public (deactivate-study (study-id uint))
  (let
    (
      (study (unwrap! (map-get? academic-studies study-id) ERR-STUDY-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get lead-researcher study)) ERR-NOT-AUTHORIZED)
    (map-set academic-studies study-id (merge study { is-active: false }))
    (ok true)
  ))

;; Read-only functions
(define-read-only (get-academic-study (study-id uint))
  (map-get? academic-studies study-id))

(define-read-only (get-research-collaboration (collaboration-id uint))
  (map-get? research-collaborations collaboration-id))

(define-read-only (get-collaborator-access (collaborator principal) (study-id uint))
  (match (map-get? collaborator-study-access { collaborator: collaborator, study-id: study-id })
    collaboration-id (map-get? research-collaborations collaboration-id)
    none
  ))

(define-read-only (get-research-publication (collaborator principal) (study-id uint))
  (map-get? research-publications { collaborator: collaborator, study-id: study-id }))

(define-read-only (is-collaborator-published (collaborator principal) (study-id uint))
  (is-some (map-get? research-publications { collaborator: collaborator, study-id: study-id })))

(define-read-only (get-study-stats (study-id uint))
  (match (map-get? academic-studies study-id)
    study {
      total-collaborators: (get total-collaborators study),
      total-publications: (get total-publications study),
      publication-rate: (if (> (get total-collaborators study) u0)
        (/ (* (get total-publications study) u100) (get total-collaborators study))
        u0
      )
    }
    { total-collaborators: u0, total-publications: u0, publication-rate: u0 }
  ))

(define-read-only (get-platform-stats)
  {
    total-studies: (- (var-get next-study-id) u1),
    total-collaborations: (- (var-get next-collaboration-id) u1),
    total-research-fees: (var-get total-research-fees),
    research-treasury: (var-get research-treasury)
  })

(define-read-only (calculate-study-cost (study-id uint))
  (match (map-get? academic-studies study-id)
    study {
      funding: (get collaboration-funding study),
      bond: (get participation-bond study),
      total: (+ (get collaboration-funding study) (get participation-bond study)),
      platform-fee: (calculate-platform-fee (get collaboration-funding study)),
      researcher-amount: (calculate-researcher-amount (get collaboration-funding study))
    }
    { funding: u0, bond: u0, total: u0, platform-fee: u0, researcher-amount: u0 }
  ))