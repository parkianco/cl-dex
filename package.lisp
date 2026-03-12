;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; package.lisp - Package definition for cl-dex

(defpackage #:cl-dex
  (:use #:cl)
  (:export
   ;;; ===================================================================
   ;;; Core Types
   ;;; ===================================================================

   ;; Limit Order
   #:limit-order
   #:make-limit-order
   #:limit-order-p
   #:limit-order-id
   #:limit-order-trader
   #:limit-order-pair
   #:limit-order-side
   #:limit-order-price
   #:limit-order-amount
   #:limit-order-filled-amount
   #:limit-order-remaining-amount
   #:limit-order-status
   #:limit-order-time-in-force
   #:limit-order-expiry
   #:limit-order-created-at
   #:limit-order-updated-at

   ;; Trade (Fill)
   #:trade
   #:make-trade
   #:trade-id
   #:trade-buy-order-id
   #:trade-sell-order-id
   #:trade-pair
   #:trade-price
   #:trade-amount
   #:trade-buyer
   #:trade-seller
   #:trade-timestamp

   ;; Order Book
   #:order-book
   #:make-order-book
   #:order-book-p
   #:order-book-pair
   #:order-book-bids
   #:order-book-asks
   #:order-book-last-trade-price
   #:order-book-volume-24h

   ;;; ===================================================================
   ;;; Constants
   ;;; ===================================================================
   #:+side-buy+
   #:+side-sell+
   #:+status-open+
   #:+status-partial+
   #:+status-filled+
   #:+status-cancelled+
   #:+status-expired+
   #:+tif-gtc+
   #:+tif-ioc+
   #:+tif-fok+
   #:+tif-gtt+

   ;;; ===================================================================
   ;;; Order Book Operations
   ;;; ===================================================================
   #:create-order-book
   #:create-limit-order
   #:cancel-order
   #:match-orders
   #:process-market-order
   #:get-best-bid
   #:get-best-ask
   #:get-spread
   #:get-order-book-depth
   #:get-open-orders
   #:get-order-by-id
   #:expire-orders

   ;;; ===================================================================
   ;;; Registry Operations
   ;;; ===================================================================
   #:*order-book-registry*
   #:*order-registry*
   #:*trade-history*
   #:register-order-book
   #:get-order-book
   #:get-all-order-books
   #:clear-registry

   ;;; ===================================================================
   ;;; Error Conditions
   ;;; ===================================================================
   #:dex-error
   #:order-not-found-error
   #:order-book-not-found-error
   #:invalid-order-error
   #:insufficient-liquidity-error
   #:order-expired-error))
