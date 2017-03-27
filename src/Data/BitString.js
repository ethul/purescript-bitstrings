'use strict';

exports.bitsAnd = function (n1) {
  return function (n2) {
    return n1 & n2;
  };
};

exports.bitsOr = function (n1) {
  return function (n2) {
    return n1 | n2;
  };
};

exports.bitsShl = function (n1) {
  return function (n2) {
    return n1 << n2;
  };
};

exports.bitsZshr = function (n1) {
  return function (n2) {
    return n1 >>> n2;
  };
};

exports.bitsComplement = function (n) {
  return ~n;
};
