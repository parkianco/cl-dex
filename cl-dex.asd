;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; cl-dex.asd - DEX Order Book Matching Engine System Definition

(asdf:defsystem #:cl-dex
  :name "cl-dex"
  :version "0.1.0"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :description "DEX Order Book Matching Engine"
  :long-description "A standalone Common Lisp implementation of a Central Limit Order Book (CLOB)
matching engine. Supports limit orders, market orders, price-time priority matching,
and multiple time-in-force options (GTC, IOC, FOK, GTT)."
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "types")
                             (:file "orderbook"))))
  :in-order-to ((asdf:test-op (test-op #:cl-dex/test))))

(asdf:defsystem #:cl-dex/test
  :name "cl-dex/test"
  :version "0.1.0"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :description "Tests for cl-dex"
  :depends-on (#:cl-dex)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "orderbook-test"))))
  :perform (asdf:test-op (op c)
             (let ((result (uiop:symbol-call :cl-dex/test :run-tests)))
               (unless result
                 (error "Tests failed")))))
