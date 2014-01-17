# A simple Haskell implementation of an orderbook

[![Build Status](https://travis-ci.org/paulkoerbitz/orderbook.png)](https://travis-ci.org/paulkoerbitz/orderbook)

This is a simple implementation of an
[orderbook](http://en.wikipedia.org/wiki/Order_book_%28trading%29),
written in about 100 lines of Haskell. It maintains buy and sell
orders at different pricelevels and matches orders when bid and ask
limits overlap.

## Todo

- Sides of the orderbook should be maintained by an (indexed) priority queue
  to support cancels and for efficiency
- Implement cancels