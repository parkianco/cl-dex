;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; orderbook-test.lisp - Tests for cl-dex

(defpackage #:cl-dex/test
  (:use #:cl #:cl-dex)
  (:export #:run-tests))

(in-package #:cl-dex/test)

;;; ===================================================================
;;; Test Infrastructure
;;; ===================================================================

(defvar *tests* nil "List of test functions")
(defvar *passed* 0 "Count of passed tests")
(defvar *failed* 0 "Count of failed tests")

(defmacro deftest (name &body body)
  "Define a test function."
  `(progn
     (defun ,name ()
       (handler-case
           (progn ,@body t)
         (error (e)
           (format t "~&FAIL: ~A - ~A~%" ',name e)
           nil)))
     (pushnew ',name *tests*)))

(defmacro assert-equal (expected actual &optional message)
  "Assert that expected equals actual."
  `(unless (equal ,expected ,actual)
     (error "~A~%  Expected: ~S~%  Actual: ~S"
            (or ,message "Assertion failed")
            ,expected ,actual)))

(defmacro assert-true (expr &optional message)
  "Assert that expression is true."
  `(unless ,expr
     (error "~A: ~S is not true" (or ,message "Assertion failed") ',expr)))

(defmacro assert-error (error-type &body body)
  "Assert that body signals an error of the given type."
  `(handler-case
       (progn ,@body
              (error "Expected ~A but no error was signaled" ',error-type))
     (,error-type () t)
     (error (e)
       (error "Expected ~A but got ~A" ',error-type (type-of e)))))

;;; ===================================================================
;;; Order Book Tests
;;; ===================================================================

(deftest test-create-order-book
  (cl-dex:clear-registry)
  (let ((book (cl-dex:create-order-book "ETH/USDC")))
    (assert-true (cl-dex:order-book-p book))
    (assert-equal "ETH/USDC" (cl-dex:order-book-pair book))
    (assert-equal nil (cl-dex:order-book-bids book))
    (assert-equal nil (cl-dex:order-book-asks book))))

;;; ===================================================================
;;; Limit Order Tests
;;; ===================================================================

(deftest test-create-limit-order-buy
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  (multiple-value-bind (order trades)
      (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 100)
    (assert-true (cl-dex:limit-order-p order))
    (assert-equal cl-dex:+side-buy+ (cl-dex:limit-order-side order))
    (assert-equal 2000 (cl-dex:limit-order-price order))
    (assert-equal 100 (cl-dex:limit-order-amount order))
    (assert-equal cl-dex:+status-open+ (cl-dex:limit-order-status order))
    (assert-equal nil trades "No trades without counterparty")))

(deftest test-create-limit-order-sell
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  (multiple-value-bind (order trades)
      (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2100 50)
    (assert-true (cl-dex:limit-order-p order))
    (assert-equal cl-dex:+side-sell+ (cl-dex:limit-order-side order))
    (assert-equal 2100 (cl-dex:limit-order-price order))
    (assert-equal 50 (cl-dex:limit-order-amount order))
    (assert-equal nil trades)))

(deftest test-invalid-order-zero-price
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  (assert-error cl-dex:invalid-order-error
    (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 0 100)))

(deftest test-invalid-order-zero-amount
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  (assert-error cl-dex:invalid-order-error
    (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 0)))

;;; ===================================================================
;;; Order Matching Tests
;;; ===================================================================

(deftest test-simple-match
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; Place a sell order
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2000 100)
  ;; Place a matching buy order
  (multiple-value-bind (order trades)
      (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 100)
    (assert-equal 1 (length trades) "Should have one trade")
    (assert-equal cl-dex:+status-filled+ (cl-dex:limit-order-status order))
    (let ((trade (car trades)))
      (assert-equal 2000 (cl-dex:trade-price trade))
      (assert-equal 100 (cl-dex:trade-amount trade)))))

(deftest test-partial-fill
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; Place a sell order for 100
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2000 100)
  ;; Place a buy order for 50 (partial fill)
  (multiple-value-bind (order trades)
      (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 50)
    (assert-equal 1 (length trades))
    (assert-equal cl-dex:+status-filled+ (cl-dex:limit-order-status order))
    (assert-equal 50 (cl-dex:trade-amount (car trades))))
  ;; Check remaining sell order
  (let ((best-ask (cl-dex:get-best-ask "ETH/USDC")))
    (assert-true best-ask "Should have remaining ask")
    (assert-equal 50 (cl-dex:limit-order-remaining-amount best-ask))))

(deftest test-price-time-priority
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; Place two sell orders at same price
  (let ((first-order (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2000 100)))
    (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2000 100)
    ;; Buy should match first order first
    (multiple-value-bind (order trades)
        (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 100)
      (declare (ignore order))
      (assert-equal 1 (length trades))
      ;; First trade should be with first order
      (assert-equal (cl-dex:limit-order-id first-order)
                    (cl-dex:trade-sell-order-id (car trades))))))

(deftest test-price-priority
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; Place sell at 2100
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2100 100)
  ;; Place sell at 2000 (better price)
  (let ((better-order (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2000 100)))
    ;; Buy at 2100 should match the 2000 order first
    (multiple-value-bind (order trades)
        (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2100 100)
      (declare (ignore order))
      (assert-equal 1 (length trades))
      (assert-equal 2000 (cl-dex:trade-price (car trades)))
      (assert-equal (cl-dex:limit-order-id better-order)
                    (cl-dex:trade-sell-order-id (car trades))))))

;;; ===================================================================
;;; Time-in-Force Tests
;;; ===================================================================

(deftest test-ioc-order-partial
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; Place sell for 50
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2000 50)
  ;; IOC buy for 100 - should fill 50 and cancel remainder
  (multiple-value-bind (order trades)
      (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 100
                                  :time-in-force cl-dex:+tif-ioc+)
    (assert-equal 1 (length trades))
    (assert-equal 50 (cl-dex:limit-order-filled-amount order))
    (assert-equal cl-dex:+status-cancelled+ (cl-dex:limit-order-status order))))

(deftest test-ioc-order-no-match
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; IOC buy with no counterparty - should be cancelled immediately
  (multiple-value-bind (order trades)
      (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 100
                                  :time-in-force cl-dex:+tif-ioc+)
    (assert-equal nil trades)
    (assert-equal 0 (cl-dex:limit-order-filled-amount order))
    (assert-equal cl-dex:+status-cancelled+ (cl-dex:limit-order-status order))))

(deftest test-gtc-order-remains
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; GTC order with no match should remain open
  (multiple-value-bind (order trades)
      (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 100
                                  :time-in-force cl-dex:+tif-gtc+)
    (assert-equal nil trades)
    (assert-equal cl-dex:+status-open+ (cl-dex:limit-order-status order))
    ;; Should be in the book
    (let ((best-bid (cl-dex:get-best-bid "ETH/USDC")))
      (assert-equal (cl-dex:limit-order-id order) (cl-dex:limit-order-id best-bid)))))

;;; ===================================================================
;;; Order Cancellation Tests
;;; ===================================================================

(deftest test-cancel-order
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  (let ((order (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 100)))
    (cl-dex:cancel-order (cl-dex:limit-order-id order))
    (assert-equal cl-dex:+status-cancelled+ (cl-dex:limit-order-status order))
    ;; Should not be in book
    (assert-equal nil (cl-dex:get-best-bid "ETH/USDC"))))

(deftest test-cancel-nonexistent-order
  (cl-dex:clear-registry)
  (assert-error cl-dex:order-not-found-error
    (cl-dex:cancel-order "FAKE-ORDER-ID")))

;;; ===================================================================
;;; Market Order Tests
;;; ===================================================================

(deftest test-market-order
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; Place some liquidity
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2000 50)
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2100 50)
  ;; Market buy
  (multiple-value-bind (trades filled avg-price)
      (cl-dex:process-market-order "ETH/USDC" cl-dex:+side-buy+ 75)
    (assert-equal 2 (length trades))
    (assert-equal 75 filled)
    ;; Average price: (50*2000 + 25*2100) / 75 = 2033
    (assert-true (and (>= avg-price 2000) (<= avg-price 2100)))))

(deftest test-market-order-no-liquidity
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  (assert-error cl-dex:insufficient-liquidity-error
    (cl-dex:process-market-order "ETH/USDC" cl-dex:+side-buy+ 100)))

;;; ===================================================================
;;; Query Tests
;;; ===================================================================

(deftest test-get-spread
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 1990 100)
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2010 100)
  (multiple-value-bind (spread spread-bps bid-price ask-price)
      (cl-dex:get-spread "ETH/USDC")
    (assert-equal 20 spread)
    (assert-equal 1990 bid-price)
    (assert-equal 2010 ask-price)
    (assert-true (> spread-bps 0))))

(deftest test-get-order-book-depth
  (cl-dex:clear-registry)
  (cl-dex:create-order-book "ETH/USDC")
  ;; Add multiple orders
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 100)
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 2000 50)  ; Same price
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-buy+ 1990 75)
  (cl-dex:create-limit-order "ETH/USDC" cl-dex:+side-sell+ 2010 80)
  (multiple-value-bind (bids asks)
      (cl-dex:get-order-book-depth "ETH/USDC" :levels 5)
    (assert-equal 2 (length bids))
    (assert-equal 1 (length asks))
    ;; First bid should be aggregated (100 + 50 = 150)
    (assert-equal 2000 (car (first bids)))
    (assert-equal 150 (cdr (first bids)))))

;;; ===================================================================
;;; Test Runner
;;; ===================================================================

(defun run-tests ()
  "Run all tests and report results."
  (setf *passed* 0 *failed* 0)
  (format t "~&Running cl-dex tests...~%")
  (format t "~&========================================~%")
  (dolist (test (reverse *tests*))
    (format t "~&  ~A... " test)
    (if (funcall test)
        (progn
          (format t "OK~%")
          (incf *passed*))
        (incf *failed*)))
  (format t "~&========================================~%")
  (format t "~&Results: ~A passed, ~A failed~%~%" *passed* *failed*)
  (zerop *failed*))

;;; End of orderbook-test.lisp
