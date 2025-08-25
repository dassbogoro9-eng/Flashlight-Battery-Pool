;; Simple Flashlight Battery Pool
;; Emergency supply sharing system for power outages

(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-BATTERY (err u400))
(define-constant ERR-INSUFFICIENT-QUANTITY (err u402))

(define-map batteries
  { battery-id: uint }
  {
    owner: principal,
    battery-type: (string-ascii 20),
    quantity: uint,
    last-tested: uint,
    status: (string-ascii 10)
  }
)

(define-map user-batteries
  { user: principal }
  { battery-ids: (list 50 uint) }
)

(define-data-var next-battery-id uint u1)

(define-read-only (get-battery (battery-id uint))
  (map-get? batteries { battery-id: battery-id })
)

(define-read-only (get-user-batteries (user principal))
  (default-to { battery-ids: (list) } (map-get? user-batteries { user: user }))
)

(define-public (add-battery (battery-type (string-ascii 20)) (quantity uint))
  (let ((battery-id (var-get next-battery-id)))
    (asserts! (> quantity u0) ERR-INVALID-BATTERY)
    (map-set batteries
      { battery-id: battery-id }
      {
        owner: tx-sender,
        battery-type: battery-type,
        quantity: quantity,
        last-tested: stacks-block-height,
        status: "available"
      }
    )
    (let ((current-batteries (get battery-ids (get-user-batteries tx-sender))))
      (map-set user-batteries
        { user: tx-sender }
        { battery-ids: (unwrap! (as-max-len? (append current-batteries battery-id) u50) ERR-INVALID-BATTERY) }
      )
    )
    (var-set next-battery-id (+ battery-id u1))
    (ok battery-id)
  )
)

(define-public (update-battery-test (battery-id uint))
  (let ((battery (unwrap! (get-battery battery-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get owner battery) tx-sender) ERR-UNAUTHORIZED)
    (map-set batteries
      { battery-id: battery-id }
      (merge battery { last-tested: stacks-block-height })
    )
    (ok true)
  )
)

(define-public (transfer-battery (battery-id uint) (recipient principal) (transfer-quantity uint))
  (let ((battery (unwrap! (get-battery battery-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get owner battery) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (<= transfer-quantity (get quantity battery)) ERR-INSUFFICIENT-QUANTITY)
    (asserts! (> transfer-quantity u0) ERR-INVALID-BATTERY)

    (if (is-eq transfer-quantity (get quantity battery))
      ;; Transfer entire battery
      (begin
        (map-set batteries
          { battery-id: battery-id }
          (merge battery { owner: recipient })
        )
        (let ((recipient-batteries (get battery-ids (get-user-batteries recipient))))
          (map-set user-batteries
            { user: recipient }
            { battery-ids: (unwrap! (as-max-len? (append recipient-batteries battery-id) u50) ERR-INVALID-BATTERY) }
          )
        )
        true
      )
      ;; Partial transfer - reduce original and create new entry
      (begin
        (map-set batteries
          { battery-id: battery-id }
          (merge battery { quantity: (- (get quantity battery) transfer-quantity) })
        )
        (unwrap! (add-battery-for-user recipient (get battery-type battery) transfer-quantity) ERR-INVALID-BATTERY)
        true
      )
    )
    (ok true)
  )
)

(define-private (add-battery-for-user (user principal) (battery-type (string-ascii 20)) (quantity uint))
  (let ((battery-id (var-get next-battery-id)))
    (map-set batteries
      { battery-id: battery-id }
      {
        owner: user,
        battery-type: battery-type,
        quantity: quantity,
        last-tested: stacks-block-height,
        status: "available"
      }
    )
    (let ((current-batteries (get battery-ids (get-user-batteries user))))
      (map-set user-batteries
        { user: user }
        { battery-ids: (unwrap! (as-max-len? (append current-batteries battery-id) u50) ERR-INVALID-BATTERY) }
      )
    )
    (var-set next-battery-id (+ battery-id u1))
    (ok battery-id)
  )
)
