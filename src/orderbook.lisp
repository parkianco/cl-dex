;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; orderbook.lisp - Order Book Matching Engine Implementation

(in-package #:cl-dex)

;;; ===================================================================
;;; Order Book Creation
;;; ===================================================================

(defun create-order-book (pair)
  "Create a new order book for a trading pair."
  (let ((book (make-order-book :pair pair)))
    (register-order-book book)
    book))

;;; ===================================================================
;;; Order Priority
;;; ===================================================================

(defun bid-priority> (order1 order2)
  "Compare bid priority: higher price wins, then earlier time (price-time priority)."
  (let ((p1 (limit-order-price order1))
        (p2 (limit-order-price order2)))
    (cond
      ((> p1 p2) t)
      ((< p1 p2) nil)
      (t (< (limit-order-created-at order1) (limit-order-created-at order2))))))

(defun ask-priority> (order1 order2)
  "Compare ask priority: lower price wins, then earlier time (price-time priority)."
  (let ((p1 (limit-order-price order1))
        (p2 (limit-order-price order2)))
    (cond
      ((< p1 p2) t)
      ((> p1 p2) nil)
      (t (< (limit-order-created-at order1) (limit-order-created-at order2))))))

(defun insert-sorted (order list comparator)
  "Insert order into sorted list maintaining order."
  (if (null list)
      (list order)
      (if (funcall comparator order (car list))
          (cons order list)
          (cons (car list) (insert-sorted order (cdr list) comparator)))))

;;; ===================================================================
;;; Order Creation
;;; ===================================================================

(defun validate-order (order)
  "Validate an order. Signals invalid-order-error if invalid."
  (let ((id (limit-order-id order)))
    (when (<= (limit-order-price order) 0)
      (error 'invalid-order-error :order-id id :reason "Price must be positive"))
    (when (<= (limit-order-amount order) 0)
      (error 'invalid-order-error :order-id id :reason "Amount must be positive"))
    (when (not (member (limit-order-side order) (list +side-buy+ +side-sell+)))
      (error 'invalid-order-error :order-id id :reason "Invalid side"))
    (when (not (member (limit-order-time-in-force order)
                       (list +tif-gtc+ +tif-ioc+ +tif-fok+ +tif-gtt+)))
      (error 'invalid-order-error :order-id id :reason "Invalid time-in-force"))
    (when (and (eq (limit-order-time-in-force order) +tif-gtt+)
               (null (limit-order-expiry order)))
      (error 'invalid-order-error :order-id id :reason "GTT orders require expiry"))
    t))

(defun create-limit-order (pair side price amount &key
                                                    (trader "anon")
                                                    (time-in-force +tif-gtc+)
                                                    (expiry nil))
  "Create and submit a limit order. Returns (values order trades).
   The order is matched against the book immediately if possible."
  (let* ((now (get-current-time))
         (order (make-limit-order
                 :id (generate-order-id)
                 :trader trader
                 :pair pair
                 :side side
                 :price price
                 :amount amount
                 :filled-amount 0
                 :status +status-open+
                 :time-in-force time-in-force
                 :expiry expiry
                 :created-at now
                 :updated-at now)))

    ;; Validate order
    (validate-order order)

    ;; Check expiry for GTT
    (when (and (eq time-in-force +tif-gtt+) expiry (<= expiry now))
      (error 'order-expired-error :order-id (limit-order-id order) :expiry expiry
             :message "Order already expired"))

    ;; Register order
    (setf (gethash (limit-order-id order) *order-registry*) order)

    ;; Get or create order book
    (let ((book (handler-case (get-order-book pair)
                  (order-book-not-found-error () (create-order-book pair)))))

      ;; Match against existing orders
      (let ((trades (match-order order book)))

        ;; Handle time-in-force
        (cond
          ;; IOC: cancel any unfilled portion
          ((eq time-in-force +tif-ioc+)
           (when (> (limit-order-remaining-amount order) 0)
             (setf (limit-order-status order) +status-cancelled+)
             (setf (limit-order-updated-at order) (get-current-time))))

          ;; FOK: if not fully filled, cancel and reverse trades
          ((eq time-in-force +tif-fok+)
           (when (> (limit-order-remaining-amount order) 0)
             ;; Cancel order
             (setf (limit-order-status order) +status-cancelled+)
             (setf (limit-order-updated-at order) (get-current-time))
             ;; Note: In production, trades would need to be reversed
             ;; For simplicity, FOK is checked before matching
             (setf trades nil)))

          ;; GTC/GTT: add remaining to book
          (t
           (when (and (> (limit-order-remaining-amount order) 0)
                      (member (limit-order-status order) (list +status-open+ +status-partial+)))
             (add-order-to-book order book))))

        (values order trades)))))

(defun add-order-to-book (order book)
  "Add an order to the appropriate side of the order book."
  (if (eq (limit-order-side order) +side-buy+)
      (setf (order-book-bids book)
            (insert-sorted order (order-book-bids book) #'bid-priority>))
      (setf (order-book-asks book)
            (insert-sorted order (order-book-asks book) #'ask-priority>))))

(defun remove-order-from-book (order book)
  "Remove an order from the order book."
  (if (eq (limit-order-side order) +side-buy+)
      (setf (order-book-bids book)
            (remove order (order-book-bids book) :test #'eq))
      (setf (order-book-asks book)
            (remove order (order-book-asks book) :test #'eq))))

;;; ===================================================================
;;; Order Matching
;;; ===================================================================

(defun match-order (order book)
  "Match an incoming order against the book. Returns list of trades."
  (let ((trades nil)
        (remaining (limit-order-remaining-amount order)))

    ;; FOK check: verify full fill is possible before matching
    (when (eq (limit-order-time-in-force order) +tif-fok+)
      (let ((available (calculate-available-liquidity book order)))
        (when (< available remaining)
          (return-from match-order nil))))

    ;; Get opposite side
    (let ((opposite-orders (if (eq (limit-order-side order) +side-buy+)
                               (order-book-asks book)
                               (order-book-bids book))))

      ;; Match against orders
      (loop while (and (> remaining 0) opposite-orders)
            for match = (car opposite-orders)
            do
               ;; Check price compatibility
               (unless (price-crosses-p order match)
                 (return))

               ;; Check if match is expired
               (when (order-expired-p match)
                 (setf (limit-order-status match) +status-expired+)
                 (pop opposite-orders)
                 (if (eq (limit-order-side order) +side-buy+)
                     (setf (order-book-asks book) opposite-orders)
                     (setf (order-book-bids book) opposite-orders))
                 ;; Continue to next order
                 (go next-iteration))

               ;; Execute fill
               (let* ((match-remaining (limit-order-remaining-amount match))
                      (fill-amount (min remaining match-remaining))
                      (fill-price (limit-order-price match)) ; Maker price
                      (trade (execute-fill order match fill-amount fill-price book)))

                 (push trade trades)
                 (decf remaining fill-amount)

                 ;; Update match order
                 (incf (limit-order-filled-amount match) fill-amount)
                 (setf (limit-order-updated-at match) (get-current-time))
                 (if (zerop (limit-order-remaining-amount match))
                     (progn
                       (setf (limit-order-status match) +status-filled+)
                       (pop opposite-orders)
                       (if (eq (limit-order-side order) +side-buy+)
                           (setf (order-book-asks book) opposite-orders)
                           (setf (order-book-bids book) opposite-orders)))
                     (setf (limit-order-status match) +status-partial+)))

            next-iteration)

      ;; Update incoming order
      (let ((filled (- (limit-order-amount order) remaining)))
        (setf (limit-order-filled-amount order) filled)
        (setf (limit-order-updated-at order) (get-current-time))
        (cond
          ((zerop remaining)
           (setf (limit-order-status order) +status-filled+))
          ((> filled 0)
           (setf (limit-order-status order) +status-partial+))))

      (nreverse trades))))

(defun price-crosses-p (taker maker)
  "Check if taker order's price crosses maker's price."
  (if (eq (limit-order-side taker) +side-buy+)
      (>= (limit-order-price taker) (limit-order-price maker))
      (<= (limit-order-price taker) (limit-order-price maker))))

(defun order-expired-p (order)
  "Check if an order has expired."
  (and (eq (limit-order-time-in-force order) +tif-gtt+)
       (limit-order-expiry order)
       (>= (get-current-time) (limit-order-expiry order))))

(defun calculate-available-liquidity (book order)
  "Calculate available liquidity on opposite side at compatible prices."
  (let ((opposite-orders (if (eq (limit-order-side order) +side-buy+)
                             (order-book-asks book)
                             (order-book-bids book)))
        (available 0))
    (dolist (match opposite-orders)
      (when (and (price-crosses-p order match)
                 (not (order-expired-p match)))
        (incf available (limit-order-remaining-amount match))))
    available))

(defun execute-fill (taker maker amount price book)
  "Execute a fill between two orders. Returns the trade."
  (let* ((buyer (if (eq (limit-order-side taker) +side-buy+) taker maker))
         (seller (if (eq (limit-order-side taker) +side-sell+) taker maker))
         (now (get-current-time))
         (trade (make-trade
                 :id (generate-trade-id)
                 :buy-order-id (limit-order-id buyer)
                 :sell-order-id (limit-order-id seller)
                 :pair (limit-order-pair taker)
                 :price price
                 :amount amount
                 :buyer (limit-order-trader buyer)
                 :seller (limit-order-trader seller)
                 :timestamp now)))

    ;; Update order book state
    (setf (order-book-last-trade-price book) price)
    (incf (order-book-volume-24h book) amount)

    ;; Add to trade history
    (push trade *trade-history*)

    trade))

;;; ===================================================================
;;; Market Orders
;;; ===================================================================

(defun process-market-order (pair side amount &key (trader "anon") (max-slippage-bps nil))
  "Execute a market order. Returns (values trades total-filled avg-price).
   Market orders match at the best available prices."
  (let ((book (get-order-book pair))
        (trades nil)
        (total-filled 0)
        (total-cost 0)
        (remaining amount))

    ;; Get opposite side
    (let ((opposite-orders (if (eq side +side-buy+)
                               (copy-list (order-book-asks book))
                               (copy-list (order-book-bids book)))))

      (when (null opposite-orders)
        (error 'insufficient-liquidity-error
               :pair pair :side side :required amount :available 0
               :message "No liquidity available"))

      ;; Calculate reference price for slippage check
      (let ((reference-price (limit-order-price (car opposite-orders))))

        ;; Match against orders
        (loop while (and (> remaining 0) opposite-orders)
              for match = (car opposite-orders)
              do
                 ;; Skip expired orders
                 (when (order-expired-p match)
                   (setf (limit-order-status match) +status-expired+)
                   (remove-order-from-book match book)
                   (pop opposite-orders)
                   (go next-iteration))

                 ;; Check slippage if specified
                 (when max-slippage-bps
                   (let* ((match-price (limit-order-price match))
                          (slippage-bps (if (eq side +side-buy+)
                                            (floor (* (- match-price reference-price) 10000) reference-price)
                                            (floor (* (- reference-price match-price) 10000) reference-price))))
                     (when (> slippage-bps max-slippage-bps)
                       (return))))

                 ;; Execute fill
                 (let* ((match-remaining (limit-order-remaining-amount match))
                        (fill-amount (min remaining match-remaining))
                        (fill-price (limit-order-price match)))

                   ;; Create a temporary taker order for the trade
                   (let ((taker-order (make-limit-order
                                       :id (generate-order-id)
                                       :trader trader
                                       :pair pair
                                       :side side
                                       :price fill-price
                                       :amount fill-amount
                                       :filled-amount 0
                                       :status +status-open+
                                       :created-at (get-current-time))))

                     (let ((trade (execute-fill taker-order match fill-amount fill-price book)))
                       (push trade trades)
                       (incf total-filled fill-amount)
                       (incf total-cost (* fill-amount fill-price))
                       (decf remaining fill-amount)

                       ;; Update match order
                       (incf (limit-order-filled-amount match) fill-amount)
                       (setf (limit-order-updated-at match) (get-current-time))
                       (if (zerop (limit-order-remaining-amount match))
                           (progn
                             (setf (limit-order-status match) +status-filled+)
                             (remove-order-from-book match book)
                             (pop opposite-orders))
                           (setf (limit-order-status match) +status-partial+)))))

              next-iteration))

      (when (and (> remaining 0) (zerop total-filled))
        (error 'insufficient-liquidity-error
               :pair pair :side side :required amount :available 0
               :message "Could not fill any amount"))

      (values (nreverse trades)
              total-filled
              (if (> total-filled 0)
                  (floor total-cost total-filled)
                  0)))))

;;; ===================================================================
;;; Order Cancellation
;;; ===================================================================

(defun cancel-order (order-id)
  "Cancel an open order. Returns the cancelled order."
  (let ((order (gethash order-id *order-registry*)))
    (unless order
      (error 'order-not-found-error :order-id order-id :message "Order not found"))

    (unless (member (limit-order-status order) (list +status-open+ +status-partial+))
      (error 'invalid-order-error :order-id order-id
             :reason (format nil "Cannot cancel order with status ~A"
                             (limit-order-status order))))

    ;; Remove from book
    (let ((book (handler-case (get-order-book (limit-order-pair order))
                  (order-book-not-found-error () nil))))
      (when book
        (remove-order-from-book order book)))

    ;; Update status
    (setf (limit-order-status order) +status-cancelled+)
    (setf (limit-order-updated-at order) (get-current-time))

    order))

;;; ===================================================================
;;; Order Book Queries
;;; ===================================================================

(defun get-best-bid (pair)
  "Get the best bid (highest buy price) for a pair. Returns the order or nil."
  (let ((book (get-order-book pair)))
    (car (order-book-bids book))))

(defun get-best-ask (pair)
  "Get the best ask (lowest sell price) for a pair. Returns the order or nil."
  (let ((book (get-order-book pair)))
    (car (order-book-asks book))))

(defun get-spread (pair)
  "Get the bid-ask spread. Returns (values spread-absolute spread-bps best-bid best-ask)."
  (let* ((best-bid (get-best-bid pair))
         (best-ask (get-best-ask pair))
         (bid-price (if best-bid (limit-order-price best-bid) 0))
         (ask-price (if best-ask (limit-order-price best-ask) 0)))
    (if (and best-bid best-ask)
        (let* ((spread (- ask-price bid-price))
               (mid-price (floor (+ bid-price ask-price) 2))
               (spread-bps (if (> mid-price 0)
                               (floor (* spread 10000) mid-price)
                               0)))
          (values spread spread-bps bid-price ask-price))
        (values 0 0 bid-price ask-price))))

(defun get-order-book-depth (pair &key (levels 10))
  "Get order book depth. Returns (values bids asks) as lists of (price . total-amount)."
  (let ((book (get-order-book pair))
        (bid-levels (make-hash-table))
        (ask-levels (make-hash-table)))

    ;; Aggregate bids by price
    (dolist (order (order-book-bids book))
      (let ((price (limit-order-price order)))
        (incf (gethash price bid-levels 0) (limit-order-remaining-amount order))))

    ;; Aggregate asks by price
    (dolist (order (order-book-asks book))
      (let ((price (limit-order-price order)))
        (incf (gethash price ask-levels 0) (limit-order-remaining-amount order))))

    ;; Convert to sorted lists
    (let ((bids nil) (asks nil))
      (maphash (lambda (price amount) (push (cons price amount) bids)) bid-levels)
      (maphash (lambda (price amount) (push (cons price amount) asks)) ask-levels)

      ;; Sort and limit
      (setf bids (sort bids #'> :key #'car))
      (setf asks (sort asks #'< :key #'car))

      (values (subseq bids 0 (min levels (length bids)))
              (subseq asks 0 (min levels (length asks)))))))

(defun get-open-orders (pair &key (trader nil))
  "Get all open orders for a pair, optionally filtered by trader."
  (let ((book (get-order-book pair))
        (orders nil))
    (dolist (order (append (order-book-bids book) (order-book-asks book)))
      (when (and (member (limit-order-status order) (list +status-open+ +status-partial+))
                 (or (null trader) (string= trader (limit-order-trader order))))
        (push order orders)))
    (nreverse orders)))

(defun get-order-by-id (order-id)
  "Get an order by its ID."
  (or (gethash order-id *order-registry*)
      (error 'order-not-found-error :order-id order-id :message "Order not found")))

;;; ===================================================================
;;; Order Expiry
;;; ===================================================================

(defun expire-orders (pair)
  "Expire all GTT orders that have passed their expiry time. Returns count expired."
  (let ((book (get-order-book pair))
        (count 0)
        (now (get-current-time)))

    ;; Check bids
    (dolist (order (copy-list (order-book-bids book)))
      (when (and (eq (limit-order-time-in-force order) +tif-gtt+)
                 (limit-order-expiry order)
                 (>= now (limit-order-expiry order)))
        (setf (limit-order-status order) +status-expired+)
        (setf (limit-order-updated-at order) now)
        (remove-order-from-book order book)
        (incf count)))

    ;; Check asks
    (dolist (order (copy-list (order-book-asks book)))
      (when (and (eq (limit-order-time-in-force order) +tif-gtt+)
                 (limit-order-expiry order)
                 (>= now (limit-order-expiry order)))
        (setf (limit-order-status order) +status-expired+)
        (setf (limit-order-updated-at order) now)
        (remove-order-from-book order book)
        (incf count)))

    count))

;;; ===================================================================
;;; Batch Matching
;;; ===================================================================

(defun match-orders (pair)
  "Match all crossing orders in the book. Returns list of trades.
   Useful after batch order submission."
  (let ((book (get-order-book pair))
        (trades nil))

    (loop while t do
      (let ((best-bid (car (order-book-bids book)))
            (best-ask (car (order-book-asks book))))

        ;; Exit if no orders on either side
        (unless (and best-bid best-ask)
          (return))

        ;; Exit if prices don't cross
        (unless (>= (limit-order-price best-bid) (limit-order-price best-ask))
          (return))

        ;; Skip expired orders
        (when (order-expired-p best-bid)
          (setf (limit-order-status best-bid) +status-expired+)
          (pop (order-book-bids book))
          (go continue))

        (when (order-expired-p best-ask)
          (setf (limit-order-status best-ask) +status-expired+)
          (pop (order-book-asks book))
          (go continue))

        ;; Execute match
        (let* ((fill-amount (min (limit-order-remaining-amount best-bid)
                                 (limit-order-remaining-amount best-ask)))
               (fill-price (limit-order-price best-ask)) ; Earlier order's price
               (trade (execute-fill best-bid best-ask fill-amount fill-price book)))

          (push trade trades)

          ;; Update orders
          (incf (limit-order-filled-amount best-bid) fill-amount)
          (incf (limit-order-filled-amount best-ask) fill-amount)

          (let ((now (get-current-time)))
            (setf (limit-order-updated-at best-bid) now)
            (setf (limit-order-updated-at best-ask) now))

          ;; Update statuses and remove filled orders
          (if (zerop (limit-order-remaining-amount best-bid))
              (progn
                (setf (limit-order-status best-bid) +status-filled+)
                (pop (order-book-bids book)))
              (setf (limit-order-status best-bid) +status-partial+))

          (if (zerop (limit-order-remaining-amount best-ask))
              (progn
                (setf (limit-order-status best-ask) +status-filled+)
                (pop (order-book-asks book)))
              (setf (limit-order-status best-ask) +status-partial+))))

      continue)

    (nreverse trades)))

;;; End of orderbook.lisp
