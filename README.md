# cl-dex

A standalone Common Lisp implementation of a DEX Order Book Matching Engine.

## Overview

cl-dex implements a Central Limit Order Book (CLOB) with:

- Limit orders with price-time priority matching
- Market orders with slippage protection
- Multiple time-in-force options (GTC, IOC, FOK, GTT)
- Order cancellation
- Order book depth queries
- Bid-ask spread calculation

## Installation

Clone the repository and load with ASDF:

```lisp
(asdf:load-system :cl-dex)
```

## Quick Start

```lisp
(use-package :cl-dex)

;; Create an order book
(create-order-book "ETH/USDC")

;; Place a limit buy order
(create-limit-order "ETH/USDC" +side-buy+ 2000 100)

;; Place a limit sell order (may match immediately)
(multiple-value-bind (order trades)
    (create-limit-order "ETH/USDC" +side-sell+ 2000 50)
  (format t "Order: ~A~%Trades: ~A~%" order trades))

;; Place an IOC order (immediate-or-cancel)
(create-limit-order "ETH/USDC" +side-buy+ 2100 100 :time-in-force +tif-ioc+)

;; Execute a market order
(multiple-value-bind (trades filled avg-price)
    (process-market-order "ETH/USDC" +side-buy+ 75)
  (format t "Filled: ~A @ ~A~%" filled avg-price))

;; Query the order book
(get-best-bid "ETH/USDC")
(get-best-ask "ETH/USDC")
(get-spread "ETH/USDC")
(get-order-book-depth "ETH/USDC" :levels 10)

;; Cancel an order
(cancel-order "ORD-123")
```

## Order Types

### Time-in-Force Options

| Constant | Description |
|----------|-------------|
| `+tif-gtc+` | Good Till Cancelled - remains until filled or cancelled |
| `+tif-ioc+` | Immediate Or Cancel - fill what you can, cancel the rest |
| `+tif-fok+` | Fill Or Kill - fill entirely or not at all |
| `+tif-gtt+` | Good Till Time - expires at specified time |

### Order Sides

| Constant | Description |
|----------|-------------|
| `+side-buy+` | Buy order (bid) |
| `+side-sell+` | Sell order (ask) |

## API Reference

### Order Book Operations

- `create-order-book (pair)` - Create a new order book
- `get-order-book (pair)` - Get existing order book
- `match-orders (pair)` - Match all crossing orders

### Order Operations

- `create-limit-order (pair side price amount &key trader time-in-force expiry)` - Create limit order
- `process-market-order (pair side amount &key trader max-slippage-bps)` - Execute market order
- `cancel-order (order-id)` - Cancel an open order
- `expire-orders (pair)` - Expire all GTT orders past their time

### Query Operations

- `get-best-bid (pair)` - Get best bid order
- `get-best-ask (pair)` - Get best ask order
- `get-spread (pair)` - Get bid-ask spread
- `get-order-book-depth (pair &key levels)` - Get aggregated depth
- `get-open-orders (pair &key trader)` - Get open orders
- `get-order-by-id (order-id)` - Get order by ID

## Testing

```lisp
(asdf:test-system :cl-dex)
```

## License

BSD-3-Clause. See LICENSE file.
