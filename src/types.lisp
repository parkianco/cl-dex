;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; types.lisp - Core data structures for cl-dex

(in-package #:cl-dex)

;;; ===================================================================
;;; Constants
;;; ===================================================================

;; Order sides
(defconstant +side-buy+ :buy)
(defconstant +side-sell+ :sell)

;; Order status
(defconstant +status-open+ :open)
(defconstant +status-partial+ :partial)
(defconstant +status-filled+ :filled)
(defconstant +status-cancelled+ :cancelled)
(defconstant +status-expired+ :expired)

;; Time in force
(defconstant +tif-gtc+ :gtc)   ; Good Till Cancelled
(defconstant +tif-ioc+ :ioc)   ; Immediate Or Cancel
(defconstant +tif-fok+ :fok)   ; Fill Or Kill
(defconstant +tif-gtt+ :gtt)   ; Good Till Time

;;; ===================================================================
;;; Error Conditions
;;; ===================================================================

(define-condition dex-error (error)
  ((message :initarg :message :reader dex-error-message))
  (:report (lambda (c s) (format s "DEX Error: ~A" (dex-error-message c)))))

(define-condition order-not-found-error (dex-error)
  ((order-id :initarg :order-id :reader order-not-found-id))
  (:report (lambda (c s)
             (format s "Order not found: ~A" (order-not-found-id c)))))

(define-condition order-book-not-found-error (dex-error)
  ((pair :initarg :pair :reader order-book-not-found-pair))
  (:report (lambda (c s)
             (format s "Order book not found: ~A" (order-book-not-found-pair c)))))

(define-condition invalid-order-error (dex-error)
  ((order-id :initarg :order-id :reader invalid-order-id)
   (reason :initarg :reason :reader invalid-order-reason))
  (:report (lambda (c s)
             (format s "Invalid order ~A: ~A"
                     (invalid-order-id c)
                     (invalid-order-reason c)))))

(define-condition insufficient-liquidity-error (dex-error)
  ((pair :initarg :pair :reader insufficient-liquidity-pair)
   (side :initarg :side :reader insufficient-liquidity-side)
   (required :initarg :required :reader insufficient-liquidity-required)
   (available :initarg :available :reader insufficient-liquidity-available))
  (:report (lambda (c s)
             (format s "Insufficient liquidity for ~A ~A: required ~A, available ~A"
                     (insufficient-liquidity-side c)
                     (insufficient-liquidity-pair c)
                     (insufficient-liquidity-required c)
                     (insufficient-liquidity-available c)))))

(define-condition order-expired-error (dex-error)
  ((order-id :initarg :order-id :reader order-expired-id)
   (expiry :initarg :expiry :reader order-expired-expiry))
  (:report (lambda (c s)
             (format s "Order ~A expired at ~A"
                     (order-expired-id c)
                     (order-expired-expiry c)))))

;;; ===================================================================
;;; Core Structures
;;; ===================================================================

(defstruct limit-order
  "A limit order in the order book."
  (id nil :type (or string null))
  (trader nil :type (or string null))
  (pair nil :type (or string null))
  (side +side-buy+ :type keyword)
  (price 0 :type integer)
  (amount 0 :type integer)
  (filled-amount 0 :type integer)
  (status +status-open+ :type keyword)
  (time-in-force +tif-gtc+ :type keyword)
  (expiry nil :type (or integer null))
  (created-at 0 :type integer)
  (updated-at 0 :type integer))

(defun limit-order-remaining-amount (order)
  "Calculate remaining unfilled amount."
  (- (limit-order-amount order) (limit-order-filled-amount order)))

(defstruct trade
  "A trade (fill) between two orders."
  (id nil :type (or string null))
  (buy-order-id nil :type (or string null))
  (sell-order-id nil :type (or string null))
  (pair nil :type (or string null))
  (price 0 :type integer)
  (amount 0 :type integer)
  (buyer nil :type (or string null))
  (seller nil :type (or string null))
  (timestamp 0 :type integer))

(defstruct order-book
  "An order book for a trading pair."
  (pair nil :type (or string null))
  (bids nil :type list)  ; Sorted by price DESC, then time ASC
  (asks nil :type list)  ; Sorted by price ASC, then time ASC
  (last-trade-price 0 :type integer)
  (volume-24h 0 :type integer))

;;; ===================================================================
;;; Global State
;;; ===================================================================

(defvar *order-book-registry* (make-hash-table :test 'equal)
  "Registry of order books indexed by trading pair.")

(defvar *order-registry* (make-hash-table :test 'equal)
  "Registry of all orders indexed by order ID.")

(defvar *trade-history* nil
  "List of executed trades (most recent first).")

(defvar *order-id-counter* 0
  "Counter for generating unique order IDs.")

(defvar *trade-id-counter* 0
  "Counter for generating unique trade IDs.")

;;; ===================================================================
;;; Helper Functions
;;; ===================================================================

(defun get-current-time ()
  "Get current Unix timestamp."
  (get-universal-time))

(defun generate-order-id ()
  "Generate a unique order ID."
  (format nil "ORD-~A" (incf *order-id-counter*)))

(defun generate-trade-id ()
  "Generate a unique trade ID."
  (format nil "TRD-~A" (incf *trade-id-counter*)))

;;; ===================================================================
;;; Registry Operations
;;; ===================================================================

(defun register-order-book (book)
  "Register an order book in the global registry."
  (let ((pair (order-book-pair book)))
    (setf (gethash pair *order-book-registry*) book)
    book))

(defun get-order-book (pair)
  "Get an order book by trading pair."
  (or (gethash pair *order-book-registry*)
      (error 'order-book-not-found-error :pair pair :message "Order book not found")))

(defun get-all-order-books ()
  "Return a list of all order books."
  (let ((books nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v books)) *order-book-registry*)
    (nreverse books)))

(defun clear-registry ()
  "Clear all registries. Useful for testing."
  (clrhash *order-book-registry*)
  (clrhash *order-registry*)
  (setf *trade-history* nil)
  (setf *order-id-counter* 0)
  (setf *trade-id-counter* 0)
  t)

;;; End of types.lisp
