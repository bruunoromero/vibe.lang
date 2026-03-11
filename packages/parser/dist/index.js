// @bun
// ../vibe-parser/dist/Vibe/Vibe/Basics.js
var _PIPE_GT = (x) => (f) => f(x);
var not = (b) => b ? false : true;
var identity = (x) => x;
var flip = (f) => (x) => (y) => f(y)(x);

// ../vibe-parser/dist/Vibe/Vibe/Int.ffi.js
var intAdd = (a, b) => a + b | 0;
var intSub = (a, b) => a - b | 0;
var intMul = (a, b) => a * b | 0;
var intDiv = (a, b) => Math.trunc(a / b);
var intMod = (a, b) => a % b;
var numEq = (a, b) => a === b;
var numLt = (a, b) => a < b;
var numGt = (a, b) => a > b;
var numToString = (n) => n.toString();

// ../vibe-parser/dist/Vibe/Vibe/Int.js
var _PIPE_PIPE = (a) => (b) => a || b();
var _add = ($a0) => ($a1) => intAdd($a0, $a1);
var _sub = ($a0) => ($a1) => intSub($a0, $a1);
var _mul = ($a0) => ($a1) => intMul($a0, $a1);
var _div = ($a0) => ($a1) => intDiv($a0, $a1);
var _mod = ($a0) => ($a1) => intMod($a0, $a1);
var _eq = ($a0) => ($a1) => numEq($a0, $a1);
var _lt = ($a0) => ($a1) => numLt($a0, $a1);
var _gt = ($a0) => ($a1) => numGt($a0, $a1);
var _toString = ($a0) => numToString($a0);
var _negate = (x) => -x;
var $default_Ord_Int__LT_EQ = (x) => (y) => _PIPE_PIPE(_lt(x)(y))(() => $dict_Eq_Int._EQ_EQ(x)(y));
var $default_Ord_Int__GT_EQ = (x) => (y) => _PIPE_PIPE(_gt(x)(y))(() => $dict_Eq_Int._EQ_EQ(x)(y));
var $dict_Ord_Int = {
  _LT: _lt,
  _GT: _gt,
  _LT_EQ: $default_Ord_Int__LT_EQ,
  _GT_EQ: $default_Ord_Int__GT_EQ
};
var min = (a) => (b) => $dict_Ord_Int._LT(a)(b) ? a : b;
var $default_Eq_Int__SLASH_EQ = (x) => (y) => not(_eq(x)(y));
var $dict_Eq_Int = {
  _EQ_EQ: _eq,
  _SLASH_EQ: $default_Eq_Int__SLASH_EQ
};
var $dict_Num_Int = {
  _PLUS: _add,
  _MINUS: _sub,
  _STAR: _mul,
  negate: _negate
};
var $dict_Integral_Int = {
  _SLASH_SLASH: _div,
  _PERCENT: _mod
};
var $dict_Show_Int = {
  toString: _toString
};

// ../vibe-parser/dist/Vibe/Vibe/Float.ffi.js
var numToString2 = (n) => n.toString();

// ../vibe-parser/dist/Vibe/Vibe/Float.js
var _toString2 = ($a0) => numToString2($a0);
var $dict_Show_Float = {
  toString: _toString2
};

// ../vibe-parser/dist/Vibe/Vibe/String.ffi.js
var stringAppend = (a, b) => a + b;
var stringEq = (a, b) => a === b;
var stringLt = (a, b) => a < b;
var stringGt = (a, b) => a > b;
var parseInt = (just, nothing, s) => {
  const n = Number.parseInt(s, 10);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};
var parseFloat = (just, nothing, s) => {
  const n = Number.parseFloat(s);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};
var stringCharAt = (just, nothing, i, s) => {
  if (i >= 0 && i < s.length) {
    return just(s[i]);
  }
  return nothing;
};
var unsafeCharAt = (i, s) => s[i];
var stringToList = (s) => [...s];

// ../vibe-parser/dist/Vibe/Vibe/Result.js
var Ok = ($0) => ({ $tag: 0, $0 });
var Err = ($0) => ({ $tag: 1, $0 });

// ../vibe-parser/dist/Vibe/Vibe/Maybe.js
var Just = ($0) => ({ $tag: 0, $0 });
var Nothing = { $tag: 1 };

// ../vibe-parser/dist/Vibe/Vibe/String.js
var _PIPE_PIPE2 = (a) => (b) => a || b();
var _append = ($a0) => ($a1) => stringAppend($a0, $a1);
var _eq2 = ($a0) => ($a1) => stringEq($a0, $a1);
var _lt2 = ($a0) => ($a1) => stringLt($a0, $a1);
var _gt2 = ($a0) => ($a1) => stringGt($a0, $a1);
var _parseInt = ($a0) => ($a1) => ($a2) => parseInt($a0, $a1, $a2);
var _parseFloat = ($a0) => ($a1) => ($a2) => parseFloat($a0, $a1, $a2);
var length = ($recv) => $recv.length;
var _charAt = ($a0) => ($a1) => ($a2) => ($a3) => stringCharAt($a0, $a1, $a2, $a3);
var unsafeCharAt2 = ($a0) => ($a1) => unsafeCharAt($a0, $a1);
var _slice = ($recv) => ($a0) => ($a1) => $recv.slice($a0, $a1);
var slice = (start) => (end) => (s) => _slice(s)(start)(end);
var _startsWith = ($recv) => ($a0) => $recv.startsWith($a0);
var startsWith = (prefix) => (s) => _startsWith(s)(prefix);
var _endsWith = ($recv) => ($a0) => $recv.endsWith($a0);
var endsWith = (suffix) => (s) => _endsWith(s)(suffix);
var _includes = ($recv) => ($a0) => $recv.includes($a0);
var contains = (sub) => (s) => _includes(s)(sub);
var toList = ($a0) => stringToList($a0);
var _joinStrings = ($recv) => ($a0) => $recv.join($a0);
var join = (sep) => (lst) => _joinStrings(lst)(sep);
var charAt = _charAt(Just)(Nothing);
var toInt = _parseInt(Just)(Nothing);
var toFloat = _parseFloat(Just)(Nothing);
var isEmpty = (s) => $dict_Eq_Int._EQ_EQ(length(s))(0);
var $default_Eq_String__SLASH_EQ = (x) => (y) => not(_eq2(x)(y));
var $default_Ord_String__LT_EQ = (x) => (y) => _PIPE_PIPE2(_lt2(x)(y))(() => $dict_Eq_String._EQ_EQ(x)(y));
var $default_Ord_String__GT_EQ = (x) => (y) => _PIPE_PIPE2(_gt2(x)(y))(() => $dict_Eq_String._EQ_EQ(x)(y));
var $dict_Eq_String = {
  _EQ_EQ: _eq2,
  _SLASH_EQ: $default_Eq_String__SLASH_EQ
};
var $dict_Ord_String = {
  _LT: _lt2,
  _GT: _gt2,
  _LT_EQ: $default_Ord_String__LT_EQ,
  _GT_EQ: $default_Ord_String__GT_EQ
};
var $dict_Show_String = {
  toString: identity
};
var $dict_Appendable_String = {
  _PLUS_PLUS: _append
};

// ../vibe-parser/dist/Vibe/Vibe/Char.ffi.js
var charToString = (a) => a;
var charToCode = (c) => c.codePointAt(0);
var charOrd = (a, b) => a < b;
var charOrdGt = (a, b) => a > b;

// ../vibe-parser/dist/Vibe/Vibe/Char.js
var _PIPE_PIPE3 = (a) => (b) => a || b();
var _AMP_AMP = (a) => (b) => a && b();
var _toString3 = ($a0) => charToString($a0);
var _lt3 = ($a0) => ($a1) => charOrd($a0, $a1);
var _gt3 = ($a0) => ($a1) => charOrdGt($a0, $a1);
var toCode = ($a0) => charToCode($a0);
var $default_Ord_Char__LT_EQ = (x) => (y) => _PIPE_PIPE3(_lt3(x)(y))(() => $dict_Eq_Char._EQ_EQ(x)(y));
var $default_Ord_Char__GT_EQ = (x) => (y) => _PIPE_PIPE3(_gt3(x)(y))(() => $dict_Eq_Char._EQ_EQ(x)(y));
var $dict_Ord_Char = {
  _LT: _lt3,
  _GT: _gt3,
  _LT_EQ: $default_Ord_Char__LT_EQ,
  _GT_EQ: $default_Ord_Char__GT_EQ
};
var isUpper = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("A"))(() => $dict_Ord_Char._LT_EQ(c)("Z"));
var isLower = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("a"))(() => $dict_Ord_Char._LT_EQ(c)("z"));
var isAlpha = (c) => _PIPE_PIPE3(isUpper(c))(() => isLower(c));
var isDigit = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("0"))(() => $dict_Ord_Char._LT_EQ(c)("9"));
var $impl_Eq_Char__EQ_EQ = (x) => (y) => $dict_Eq_String._EQ_EQ($dict_Show_Char.toString(x))($dict_Show_Char.toString(y));
var $default_Eq_Char__SLASH_EQ = (x) => (y) => not($dict_Eq_Char._EQ_EQ(x)(y));
var $dict_Eq_Char = {
  _EQ_EQ: $impl_Eq_Char__EQ_EQ,
  _SLASH_EQ: $default_Eq_Char__SLASH_EQ
};
var $dict_Show_Char = {
  toString: _toString3
};

// ../vibe-parser/dist/Vibe/Vibe/List.ffi.js
var listReverse = (lst) => [...lst].reverse();
var listNth = (just, nothing, n, lst) => {
  if (n >= 0 && n < lst.length) {
    return just(lst[n]);
  }
  return nothing;
};

// ../vibe-parser/dist/Vibe/Vibe/List.js
var _AMP_AMP2 = (a) => (b) => a && b();
var _concat = ($recv) => ($a0) => $recv.concat($a0);
var _COLON_COLON = (x) => (xs) => _concat([x])(xs);
var _map = ($recv) => ($a0) => $recv.map($a0);
var map = flip(_map);
var _filter = ($recv) => ($a0) => $recv.filter($a0);
var filter = flip(_filter);
var head = (xs) => (($match_0) => {
  if (Array.isArray($match_0) && $match_0.length >= 1) {
    const x = $match_0[0];
    return Just(x);
  }
  if (Array.isArray($match_0) && $match_0.length === 0) {
    return Nothing;
  }
  throw new Error("Pattern match failed");
})(xs);
var last = (xs) => {
  while (true) {
    {
      const $match_2 = xs;
      if (Array.isArray($match_2) && $match_2.length === 0) {
        return Nothing;
      }
      if (Array.isArray($match_2) && $match_2.length === 1) {
        const x = $match_2[0];
        return Just(x);
      }
      if (Array.isArray($match_2) && $match_2.length >= 1) {
        const rest = $match_2.slice(1);
        xs = rest;
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
};
var isEmpty2 = (xs) => (($match_3) => {
  if (Array.isArray($match_3) && $match_3.length === 0) {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(xs);
var reverse = ($a0) => listReverse($a0);
var _nth = ($a0) => ($a1) => ($a2) => ($a3) => listNth($a0, $a1, $a2, $a3);
var nth = _nth(Just)(Nothing);
var any = (pred) => (xs) => {
  while (true) {
    {
      const $match_8 = xs;
      if (Array.isArray($match_8) && $match_8.length === 0) {
        return false;
      }
      if (Array.isArray($match_8) && $match_8.length >= 1) {
        const x = $match_8[0];
        const rest = $match_8.slice(1);
        if (pred(x)) {
          return true;
        } else {
          [pred, xs] = [pred, rest];
          continue;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var $impl_Eq_List_v356__EQ_EQ = ($dict_Eq) => (xs) => (ys) => (($match_15) => {
  if (Array.isArray($match_15[0]) && $match_15[0].length === 0 && Array.isArray($match_15[1]) && $match_15[1].length === 0) {
    return true;
  }
  if (Array.isArray($match_15[0]) && $match_15[0].length >= 1 && Array.isArray($match_15[1]) && $match_15[1].length >= 1) {
    const x = $match_15[0][0];
    const xtail = $match_15[0].slice(1);
    const y = $match_15[1][0];
    const ytail = $match_15[1].slice(1);
    return _AMP_AMP2($dict_Eq._EQ_EQ(x)(y))(() => $dict_Eq._EQ_EQ(xtail)(ytail));
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})([xs, ys]);
var $default_Eq_List_v356__SLASH_EQ = ($dict_Eq) => (x) => (y) => not($dict_Eq._EQ_EQ(x)(y));
var $impl_Show_List_v357_toString = ($dict_Show) => (lst) => ((elementStrings) => $dict_Appendable_String._PLUS_PLUS("[")($dict_Appendable_String._PLUS_PLUS(join(", ")(elementStrings))("]")))(map($dict_Show.toString)(lst));
var $dict_Eq_List_v356 = ($dict_Eq) => ({
  _EQ_EQ: $impl_Eq_List_v356__EQ_EQ($dict_Eq),
  _SLASH_EQ: $default_Eq_List_v356__SLASH_EQ($dict_Eq)
});
var $dict_Show_List_v357 = ($dict_Show) => ({
  toString: $impl_Show_List_v357_toString($dict_Show)
});
// ../vibe-parser/dist/Vibe/Vibe/Dict.js
var _AMP_AMP3 = (a) => (b) => a && b();
var RBNode = ($0) => ($1) => ($2) => ($3) => ($4) => ({ $tag: 1, $0, $1, $2, $3, $4 });
var empty = { $tag: 0 };
var get = ($dict_Ord) => (targetKey) => (dict) => (($match_0) => {
  if ($match_0.$tag === 0) {
    return Nothing;
  }
  if ($match_0.$tag === 1) {
    const key = $match_0.$1;
    const value = $match_0.$2;
    const left = $match_0.$3;
    const right = $match_0.$4;
    return $dict_Ord._LT(targetKey)(key) ? get($dict_Ord)(targetKey)(left) : $dict_Ord._GT(targetKey)(key) ? get($dict_Ord)(targetKey)(right) : Just(value);
  }
  throw new Error("Pattern match failed");
})(dict);
var _balance = (color) => (key) => (value) => (left) => (right) => {
  while (true) {
    {
      const $match_4 = right;
      if ($match_4.$tag === 1 && $match_4.$0.$tag === 0) {
        const rK = $match_4.$1;
        const rV = $match_4.$2;
        const rLeft = $match_4.$3;
        const rRight = $match_4.$4;
        {
          const $match_5 = left;
          if ($match_5.$tag === 1 && $match_5.$0.$tag === 0) {
            const lK = $match_5.$1;
            const lV = $match_5.$2;
            const lLeft = $match_5.$3;
            const lRight = $match_5.$4;
            return RBNode({ $tag: 0 })(key)(value)(RBNode({ $tag: 1 })(lK)(lV)(lLeft)(lRight))(RBNode({ $tag: 1 })(rK)(rV)(rLeft)(rRight));
          }
          {
            [color, key, value, left, right] = [color, rK, rV, RBNode({ $tag: 0 })(key)(value)(left)(rLeft), rRight];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
      {
        {
          const $match_6 = left;
          if ($match_6.$tag === 1 && $match_6.$0.$tag === 0) {
            const lK = $match_6.$1;
            const lV = $match_6.$2;
            const lLeft = $match_6.$3;
            const lRight = $match_6.$4;
            {
              const $match_7 = lLeft;
              if ($match_7.$tag === 1 && $match_7.$0.$tag === 0) {
                const llK = $match_7.$1;
                const llV = $match_7.$2;
                const llLeft = $match_7.$3;
                const llRight = $match_7.$4;
                return RBNode({ $tag: 0 })(lK)(lV)(RBNode({ $tag: 1 })(llK)(llV)(llLeft)(llRight))(RBNode({ $tag: 1 })(key)(value)(lRight)(right));
              }
              {
                return RBNode(color)(key)(value)(left)(right);
              }
              throw new Error("Pattern match failed");
            }
          }
          {
            return RBNode(color)(key)(value)(left)(right);
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var _insertHelp = ($dict_Ord) => (key) => (value) => (dict) => (($match_8) => {
  if ($match_8.$tag === 0) {
    return RBNode({ $tag: 0 })(key)(value)({ $tag: 0 })({ $tag: 0 });
  }
  if ($match_8.$tag === 1) {
    const nColor = $match_8.$0;
    const nKey = $match_8.$1;
    const nValue = $match_8.$2;
    const nLeft = $match_8.$3;
    const nRight = $match_8.$4;
    return $dict_Ord._LT(key)(nKey) ? _balance(nColor)(nKey)(nValue)(_insertHelp($dict_Ord)(key)(value)(nLeft))(nRight) : $dict_Ord._GT(key)(nKey) ? _balance(nColor)(nKey)(nValue)(nLeft)(_insertHelp($dict_Ord)(key)(value)(nRight)) : RBNode(nColor)(key)(value)(nLeft)(nRight);
  }
  throw new Error("Pattern match failed");
})(dict);
var insert = ($dict_Ord) => (key) => (value) => (dict) => (($match_9) => {
  if ($match_9.$tag === 1) {
    const k = $match_9.$1;
    const v = $match_9.$2;
    const l = $match_9.$3;
    const r = $match_9.$4;
    return RBNode({ $tag: 1 })(k)(v)(l)(r);
  }
  if ($match_9.$tag === 0) {
    return { $tag: 0 };
  }
  throw new Error("Pattern match failed");
})(_insertHelp($dict_Ord)(key)(value)(dict));
var _colorRed = (dict) => (($match_10) => {
  if ($match_10.$tag === 1) {
    const k = $match_10.$1;
    const v = $match_10.$2;
    const l = $match_10.$3;
    const r = $match_10.$4;
    return RBNode({ $tag: 0 })(k)(v)(l)(r);
  }
  if ($match_10.$tag === 0) {
    return { $tag: 0 };
  }
  throw new Error("Pattern match failed");
})(dict);
var _moveRedLeft = (dict) => (($match_11) => {
  if ($match_11.$tag === 1) {
    const color = $match_11.$0;
    const key = $match_11.$1;
    const value = $match_11.$2;
    const left = $match_11.$3;
    const right = $match_11.$4;
    return (($match_12) => {
      if ($match_12.$tag === 1 && $match_12.$3.$tag === 1 && $match_12.$3.$0.$tag === 0) {
        const rK = $match_12.$1;
        const rV = $match_12.$2;
        const rlK = $match_12.$3.$1;
        const rlV = $match_12.$3.$2;
        const rlLeft = $match_12.$3.$3;
        const rlRight = $match_12.$3.$4;
        const rRight = $match_12.$4;
        return RBNode({ $tag: 0 })(rlK)(rlV)(RBNode({ $tag: 1 })(key)(value)(_colorRed(left))(rlLeft))(RBNode({ $tag: 1 })(rK)(rV)(rlRight)(rRight));
      }
      {
        return RBNode(color)(key)(value)(_colorRed(left))(_colorRed(right));
      }
      throw new Error("Pattern match failed");
    })(right);
  }
  if ($match_11.$tag === 0) {
    return { $tag: 0 };
  }
  throw new Error("Pattern match failed");
})(dict);
var _getMin = (dict) => {
  while (true) {
    {
      const $match_13 = dict;
      if ($match_13.$tag === 1 && $match_13.$3.$tag === 0) {
        return dict;
      }
      if ($match_13.$tag === 1) {
        const left = $match_13.$3;
        dict = left;
        continue;
      }
      if ($match_13.$tag === 0) {
        return { $tag: 0 };
      }
      throw new Error("Pattern match failed");
    }
  }
};
var _removeMin = (dict) => (($match_14) => {
  if ($match_14.$tag === 1) {
    const color = $match_14.$0;
    const key = $match_14.$1;
    const value = $match_14.$2;
    const left = $match_14.$3;
    const right = $match_14.$4;
    return (($match_15) => {
      if ($match_15.$tag === 0) {
        return { $tag: 0 };
      }
      if ($match_15.$tag === 1 && $match_15.$0.$tag === 1) {
        const lLeft = $match_15.$3;
        return (($match_16) => {
          if ($match_16.$tag === 1 && $match_16.$0.$tag === 0) {
            return RBNode(color)(key)(value)(_removeMin(left))(right);
          }
          {
            return (($match_17) => {
              if ($match_17.$tag === 1) {
                const nColor = $match_17.$0;
                const nKey = $match_17.$1;
                const nValue = $match_17.$2;
                const nLeft = $match_17.$3;
                const nRight = $match_17.$4;
                return _balance(nColor)(nKey)(nValue)(_removeMin(nLeft))(nRight);
              }
              if ($match_17.$tag === 0) {
                return { $tag: 0 };
              }
              throw new Error("Pattern match failed");
            })(_moveRedLeft(dict));
          }
          throw new Error("Pattern match failed");
        })(lLeft);
      }
      {
        return RBNode(color)(key)(value)(_removeMin(left))(right);
      }
      throw new Error("Pattern match failed");
    })(left);
  }
  if ($match_14.$tag === 0) {
    return { $tag: 0 };
  }
  throw new Error("Pattern match failed");
})(dict);
var _moveRedRight = (dict) => (($match_18) => {
  if ($match_18.$tag === 1) {
    const color = $match_18.$0;
    const key = $match_18.$1;
    const value = $match_18.$2;
    const left = $match_18.$3;
    const right = $match_18.$4;
    return (($match_19) => {
      if ($match_19.$tag === 1 && $match_19.$3.$tag === 1 && $match_19.$3.$0.$tag === 0) {
        const lK = $match_19.$1;
        const lV = $match_19.$2;
        const llK = $match_19.$3.$1;
        const llV = $match_19.$3.$2;
        const llLeft = $match_19.$3.$3;
        const llRight = $match_19.$3.$4;
        const lRight = $match_19.$4;
        return RBNode({ $tag: 0 })(lK)(lV)(RBNode({ $tag: 1 })(llK)(llV)(llLeft)(llRight))(RBNode({ $tag: 1 })(key)(value)(lRight)(_colorRed(right)));
      }
      {
        return RBNode(color)(key)(value)(_colorRed(left))(_colorRed(right));
      }
      throw new Error("Pattern match failed");
    })(left);
  }
  if ($match_18.$tag === 0) {
    return { $tag: 0 };
  }
  throw new Error("Pattern match failed");
})(dict);
var _removeHelpPrepEQGT = (dict) => (color) => (key) => (value) => (left) => (right) => (($match_20) => {
  if ($match_20.$tag === 1 && $match_20.$0.$tag === 0) {
    const lK = $match_20.$1;
    const lV = $match_20.$2;
    const lLeft = $match_20.$3;
    const lRight = $match_20.$4;
    return RBNode(color)(lK)(lV)(lLeft)(RBNode({ $tag: 0 })(key)(value)(lRight)(right));
  }
  {
    return dict;
  }
  throw new Error("Pattern match failed");
})(left);
var _removeHelp;
var _removeHelpEQGT;
_removeHelp = ($dict_Eq) => ($dict_Ord) => (targetKey) => (dict) => (($match_21) => {
  if ($match_21.$tag === 0) {
    return { $tag: 0 };
  }
  if ($match_21.$tag === 1) {
    const color = $match_21.$0;
    const key = $match_21.$1;
    const value = $match_21.$2;
    const left = $match_21.$3;
    const right = $match_21.$4;
    return $dict_Ord._LT(targetKey)(key) ? (($match_22) => {
      if ($match_22.$tag === 1 && $match_22.$0.$tag === 1) {
        const lLeft = $match_22.$3;
        return (($match_23) => {
          if ($match_23.$tag === 1 && $match_23.$0.$tag === 0) {
            return RBNode(color)(key)(value)(_removeHelp($dict_Eq)($dict_Ord)(targetKey)(left))(right);
          }
          {
            return (($match_24) => {
              if ($match_24.$tag === 1) {
                const nColor = $match_24.$0;
                const nKey = $match_24.$1;
                const nValue = $match_24.$2;
                const nLeft = $match_24.$3;
                const nRight = $match_24.$4;
                return _balance(nColor)(nKey)(nValue)(_removeHelp($dict_Eq)($dict_Ord)(targetKey)(nLeft))(nRight);
              }
              if ($match_24.$tag === 0) {
                return { $tag: 0 };
              }
              throw new Error("Pattern match failed");
            })(_moveRedLeft(dict));
          }
          throw new Error("Pattern match failed");
        })(lLeft);
      }
      {
        return RBNode(color)(key)(value)(_removeHelp($dict_Eq)($dict_Ord)(targetKey)(left))(right);
      }
      throw new Error("Pattern match failed");
    })(left) : _removeHelpEQGT($dict_Eq)($dict_Ord)(targetKey)(_removeHelpPrepEQGT(dict)(color)(key)(value)(left)(right));
  }
  throw new Error("Pattern match failed");
})(dict);
_removeHelpEQGT = ($dict_Eq) => ($dict_Ord) => (targetKey) => (dict) => (($match_25) => {
  if ($match_25.$tag === 1) {
    const color = $match_25.$0;
    const key = $match_25.$1;
    const value = $match_25.$2;
    const left = $match_25.$3;
    const right = $match_25.$4;
    return $dict_Eq._EQ_EQ(targetKey)(key) ? (($match_26) => {
      if ($match_26.$tag === 1) {
        const minKey = $match_26.$1;
        const minValue = $match_26.$2;
        return _balance(color)(minKey)(minValue)(left)(_removeMin(right));
      }
      if ($match_26.$tag === 0) {
        return { $tag: 0 };
      }
      throw new Error("Pattern match failed");
    })(_getMin(right)) : (($match_27) => {
      if ($match_27.$tag === 1 && $match_27.$0.$tag === 1) {
        const rLeft = $match_27.$3;
        return (($match_28) => {
          if ($match_28.$tag === 1 && $match_28.$0.$tag === 0) {
            return RBNode(color)(key)(value)(left)(_removeHelp($dict_Eq)($dict_Ord)(targetKey)(right));
          }
          {
            return (($match_29) => {
              if ($match_29.$tag === 1) {
                const nColor = $match_29.$0;
                const nKey = $match_29.$1;
                const nValue = $match_29.$2;
                const nLeft = $match_29.$3;
                const nRight = $match_29.$4;
                return _balance(nColor)(nKey)(nValue)(nLeft)(_removeHelp($dict_Eq)($dict_Ord)(targetKey)(nRight));
              }
              if ($match_29.$tag === 0) {
                return { $tag: 0 };
              }
              throw new Error("Pattern match failed");
            })(_moveRedRight(dict));
          }
          throw new Error("Pattern match failed");
        })(rLeft);
      }
      {
        return RBNode(color)(key)(value)(left)(_removeHelp($dict_Eq)($dict_Ord)(targetKey)(right));
      }
      throw new Error("Pattern match failed");
    })(right);
  }
  if ($match_25.$tag === 0) {
    return { $tag: 0 };
  }
  throw new Error("Pattern match failed");
})(dict);
var foldl = (fn) => (acc) => (dict) => {
  while (true) {
    {
      const $match_32 = dict;
      if ($match_32.$tag === 0) {
        return acc;
      }
      if ($match_32.$tag === 1) {
        const key = $match_32.$1;
        const value = $match_32.$2;
        const left = $match_32.$3;
        const right = $match_32.$4;
        [fn, acc, dict] = [fn, fn(key)(value)(foldl(fn)(acc)(left)), right];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
};
var _foldr = (fn) => (acc) => (dict) => {
  while (true) {
    {
      const $match_33 = dict;
      if ($match_33.$tag === 0) {
        return acc;
      }
      if ($match_33.$tag === 1) {
        const key = $match_33.$1;
        const value = $match_33.$2;
        const left = $match_33.$3;
        const right = $match_33.$4;
        [fn, acc, dict] = [fn, fn(key)(value)(_foldr(fn)(acc)(right)), left];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
};
var toList2 = (dict) => _foldr((key) => (value) => (list) => _COLON_COLON([key, value])(list))([])(dict);
var _fromListHelp = ($dict_Ord) => (pairs) => (acc) => (($match_34) => {
  if (Array.isArray($match_34) && $match_34.length === 0) {
    return acc;
  }
  if (Array.isArray($match_34) && $match_34.length >= 1) {
    const pair = $match_34[0];
    const rest = $match_34.slice(1);
    return (($match_35) => {
      {
        const k = $match_35[0];
        const v = $match_35[1];
        return _fromListHelp($dict_Ord)(rest)(insert($dict_Ord)(k)(v)(acc));
      }
      throw new Error("Pattern match failed");
    })(pair);
  }
  throw new Error("Pattern match failed");
})(pairs);
var fromList = ($dict_Ord) => (pairs) => _fromListHelp($dict_Ord)(pairs)({ $tag: 0 });
var union = ($dict_Ord) => (t1) => (t2) => foldl((k) => (v) => (acc) => insert($dict_Ord)(k)(v)(acc))(t2)(t1);
var $impl_Eq_NColor__EQ_EQ = (x) => (y) => (($match_37) => {
  if ($match_37[0].$tag === 0 && $match_37[1].$tag === 0) {
    return true;
  }
  if ($match_37[0].$tag === 1 && $match_37[1].$tag === 1) {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})([x, y]);
var $default_Eq_NColor__SLASH_EQ = (x) => (y) => not($dict_Eq_NColor._EQ_EQ(x)(y));
var $dict_Eq_NColor = {
  _EQ_EQ: $impl_Eq_NColor__EQ_EQ,
  _SLASH_EQ: $default_Eq_NColor__SLASH_EQ
};
var $impl_Eq_Dict_v799_v800__EQ_EQ;
var $dict_Eq_Dict_v799_v800;
$impl_Eq_Dict_v799_v800__EQ_EQ = ($dict_Eq) => (x_impl) => (y_impl) => (($match_38) => {
  if ($match_38[0].$tag === 0 && $match_38[1].$tag === 0) {
    return true;
  }
  if ($match_38[0].$tag === 1 && $match_38[1].$tag === 1) {
    const a_0 = $match_38[0].$0;
    const a_1 = $match_38[0].$1;
    const a_2 = $match_38[0].$2;
    const a_3 = $match_38[0].$3;
    const a_4 = $match_38[0].$4;
    const b_0 = $match_38[1].$0;
    const b_1 = $match_38[1].$1;
    const b_2 = $match_38[1].$2;
    const b_3 = $match_38[1].$3;
    const b_4 = $match_38[1].$4;
    return _AMP_AMP3(_AMP_AMP3(_AMP_AMP3(_AMP_AMP3($dict_Eq_NColor._EQ_EQ(a_0)(b_0))(() => $dict_Eq._EQ_EQ(a_1)(b_1)))(() => $dict_Eq._EQ_EQ(a_2)(b_2)))(() => $dict_Eq._EQ_EQ(a_3)(b_3)))(() => $dict_Eq._EQ_EQ(a_4)(b_4));
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})([x_impl, y_impl]);
$dict_Eq_Dict_v799_v800 = ($dict_Eq) => ({
  _EQ_EQ: $impl_Eq_Dict_v799_v800__EQ_EQ($dict_Eq)
});

// ../vibe-parser/dist/VibeLexer/VibeLexer/Types.js
var LowerIdentifier = { $tag: 0 };
var UpperIdentifier = { $tag: 1 };
var Keyword = { $tag: 2 };
var NumberToken = { $tag: 3 };
var StringToken = { $tag: 4 };
var CharToken = { $tag: 5 };
var Operator = { $tag: 6 };
var Range = { $tag: 7 };
var Backslash = { $tag: 8 };
var LParen = { $tag: 9 };
var RParen = { $tag: 10 };
var LBrace = { $tag: 11 };
var RBrace = { $tag: 12 };
var LBracket = { $tag: 13 };
var RBracket = { $tag: 14 };
var Comma = { $tag: 15 };
var Dot = { $tag: 16 };
var Colon = { $tag: 17 };
var Equals = { $tag: 18 };
var Pipe = { $tag: 19 };
var Newline = { $tag: 20 };
var BlockStart = { $tag: 21 };
var BlockSep = { $tag: 22 };
var BlockEnd = { $tag: 23 };
var Eof = { $tag: 24 };
var $impl_Eq_TokenKind__EQ_EQ = (x_impl) => (y_impl) => (($match_0) => {
  if ($match_0[0].$tag === 0 && $match_0[1].$tag === 0) {
    return true;
  }
  if ($match_0[0].$tag === 1 && $match_0[1].$tag === 1) {
    return true;
  }
  if ($match_0[0].$tag === 2 && $match_0[1].$tag === 2) {
    return true;
  }
  if ($match_0[0].$tag === 3 && $match_0[1].$tag === 3) {
    return true;
  }
  if ($match_0[0].$tag === 4 && $match_0[1].$tag === 4) {
    return true;
  }
  if ($match_0[0].$tag === 5 && $match_0[1].$tag === 5) {
    return true;
  }
  if ($match_0[0].$tag === 6 && $match_0[1].$tag === 6) {
    return true;
  }
  if ($match_0[0].$tag === 7 && $match_0[1].$tag === 7) {
    return true;
  }
  if ($match_0[0].$tag === 8 && $match_0[1].$tag === 8) {
    return true;
  }
  if ($match_0[0].$tag === 9 && $match_0[1].$tag === 9) {
    return true;
  }
  if ($match_0[0].$tag === 10 && $match_0[1].$tag === 10) {
    return true;
  }
  if ($match_0[0].$tag === 11 && $match_0[1].$tag === 11) {
    return true;
  }
  if ($match_0[0].$tag === 12 && $match_0[1].$tag === 12) {
    return true;
  }
  if ($match_0[0].$tag === 13 && $match_0[1].$tag === 13) {
    return true;
  }
  if ($match_0[0].$tag === 14 && $match_0[1].$tag === 14) {
    return true;
  }
  if ($match_0[0].$tag === 15 && $match_0[1].$tag === 15) {
    return true;
  }
  if ($match_0[0].$tag === 16 && $match_0[1].$tag === 16) {
    return true;
  }
  if ($match_0[0].$tag === 17 && $match_0[1].$tag === 17) {
    return true;
  }
  if ($match_0[0].$tag === 18 && $match_0[1].$tag === 18) {
    return true;
  }
  if ($match_0[0].$tag === 19 && $match_0[1].$tag === 19) {
    return true;
  }
  if ($match_0[0].$tag === 20 && $match_0[1].$tag === 20) {
    return true;
  }
  if ($match_0[0].$tag === 21 && $match_0[1].$tag === 21) {
    return true;
  }
  if ($match_0[0].$tag === 22 && $match_0[1].$tag === 22) {
    return true;
  }
  if ($match_0[0].$tag === 23 && $match_0[1].$tag === 23) {
    return true;
  }
  if ($match_0[0].$tag === 24 && $match_0[1].$tag === 24) {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})([x_impl, y_impl]);
var $dict_Eq_TokenKind = {
  _EQ_EQ: $impl_Eq_TokenKind__EQ_EQ
};

// ../vibe-parser/dist/VibeParser/VibeParser/Types.js
var ExportValue = ($0) => ($1) => ({ $tag: 0, $0, $1 });
var ExportOperator = ($0) => ($1) => ({ $tag: 1, $0, $1 });
var ExportTypeAll = ($0) => ($1) => ({ $tag: 2, $0, $1 });
var ExportTypeSome = ($0) => ($1) => ($2) => ({ $tag: 3, $0, $1, $2 });
var ExposeAll = ($0) => ({ $tag: 0, $0 });
var ExposeExplicit = ($0) => ($1) => ({ $tag: 1, $0, $1 });
var PVar = ($0) => ($1) => ({ $tag: 0, $0, $1 });
var PWildcard = ($0) => ({ $tag: 1, $0 });
var PConstructor = ($0) => ($1) => ($2) => ({ $tag: 2, $0, $1, $2 });
var PTuple = ($0) => ($1) => ({ $tag: 3, $0, $1 });
var PList = ($0) => ($1) => ({ $tag: 4, $0, $1 });
var PCons = ($0) => ($1) => ($2) => ({ $tag: 5, $0, $1, $2 });
var PRecord = ($0) => ($1) => ({ $tag: 6, $0, $1 });
var PInt = ($0) => ($1) => ({ $tag: 7, $0, $1 });
var PFloat = ($0) => ($1) => ({ $tag: 8, $0, $1 });
var PString = ($0) => ($1) => ({ $tag: 9, $0, $1 });
var PChar = ($0) => ($1) => ({ $tag: 10, $0, $1 });
var TRef = ($0) => ($1) => ($2) => ({ $tag: 0, $0, $1, $2 });
var TFunction = ($0) => ($1) => ($2) => ({ $tag: 1, $0, $1, $2 });
var TTuple = ($0) => ($1) => ({ $tag: 2, $0, $1 });
var TRecord = ($0) => ($1) => ({ $tag: 3, $0, $1 });
var TQualified = ($0) => ($1) => ($2) => ({ $tag: 4, $0, $1, $2 });
var EVar = ($0) => ($1) => ($2) => ({ $tag: 0, $0, $1, $2 });
var EInt = ($0) => ($1) => ({ $tag: 1, $0, $1 });
var EFloat = ($0) => ($1) => ({ $tag: 2, $0, $1 });
var EString = ($0) => ($1) => ({ $tag: 3, $0, $1 });
var EChar = ($0) => ($1) => ({ $tag: 4, $0, $1 });
var ELambda = ($0) => ($1) => ($2) => ({ $tag: 5, $0, $1, $2 });
var EApply = ($0) => ($1) => ($2) => ({ $tag: 6, $0, $1, $2 });
var EIf = ($0) => ($1) => ($2) => ($3) => ({ $tag: 7, $0, $1, $2, $3 });
var ELetIn = ($0) => ($1) => ($2) => ({ $tag: 8, $0, $1, $2 });
var ECase = ($0) => ($1) => ($2) => ({ $tag: 9, $0, $1, $2 });
var EInfix = ($0) => ($1) => ($2) => ($3) => ({ $tag: 10, $0, $1, $2, $3 });
var EUnary = ($0) => ($1) => ($2) => ({ $tag: 11, $0, $1, $2 });
var EParen = ($0) => ($1) => ({ $tag: 12, $0, $1 });
var ETuple = ($0) => ($1) => ({ $tag: 13, $0, $1 });
var EUnit = ($0) => ({ $tag: 14, $0 });
var EList = ($0) => ($1) => ({ $tag: 15, $0, $1 });
var EListRange = ($0) => ($1) => ($2) => ({ $tag: 16, $0, $1, $2 });
var ERecord = ($0) => ($1) => ({ $tag: 17, $0, $1 });
var ERecordUpdate = ($0) => ($1) => ($2) => ({ $tag: 18, $0, $1, $2 });
var EFieldAccess = ($0) => ($1) => ($2) => ({ $tag: 19, $0, $1, $2 });
var DValue = ($0) => ({ $tag: 0, $0 });
var DTypeAnnotation = ($0) => ({ $tag: 1, $0 });
var DDecorated = ($0) => ($1) => ($2) => ($3) => ($4) => ({ $tag: 2, $0, $1, $2, $3, $4 });
var DType = ($0) => ($1) => ($2) => ($3) => ($4) => ({ $tag: 3, $0, $1, $2, $3, $4 });
var DTypeAlias = ($0) => ($1) => ($2) => ($3) => ({ $tag: 4, $0, $1, $2, $3 });
var DOpaqueType = ($0) => ($1) => ($2) => ({ $tag: 5, $0, $1, $2 });
var DRecordType = ($0) => ($1) => ($2) => ($3) => ($4) => ({ $tag: 6, $0, $1, $2, $3, $4 });
var DProtocol = ($0) => ($1) => ($2) => ($3) => ($4) => ({ $tag: 7, $0, $1, $2, $3, $4 });
var DImplementation = ($0) => ($1) => ($2) => ($3) => ($4) => ({ $tag: 8, $0, $1, $2, $3, $4 });
var DInfix = ($0) => ($1) => ($2) => ($3) => ({ $tag: 9, $0, $1, $2, $3 });
var AssocLeft = { $tag: 0 };
var AssocRight = { $tag: 1 };
var AssocNone = { $tag: 2 };

// ../vibe-parser/dist/VibeParser/VibeParser/OperatorRegistry.js
var emptyRegistry = empty;
var defaultOperatorInfo = { precedence: 9, associativity: AssocLeft };
var builtinRegistry = _PIPE_GT(_PIPE_GT(empty)(insert($dict_Ord_String)("&&")({ precedence: 3, associativity: AssocRight })))(insert($dict_Ord_String)("||")({ precedence: 2, associativity: AssocRight }));
var getOperatorInfo = (op) => (registry) => (($match_0) => {
  if ($match_0.$tag === 0) {
    const info = $match_0.$0;
    return info;
  }
  if ($match_0.$tag === 1) {
    return defaultOperatorInfo;
  }
  throw new Error("Pattern match failed");
})(get($dict_Ord_String)(op)(registry));
var insertOperator = insert($dict_Ord_String);
var mergeRegistries = (base) => (override) => union($dict_Ord_String)(override)(base);

// ../vibe-parser/dist/VibeParser/VibeParser/Parser.js
var _AMP_AMP4 = (a) => (b) => a && b();
var _PIPE_PIPE4 = (a) => (b) => a || b();
var PIAnnotation = ($0) => ($1) => ($2) => ({ $tag: 0, $0, $1, $2 });
var PIImpl = ($0) => ($1) => ($2) => ($3) => ({ $tag: 1, $0, $1, $2, $3 });
var initState = (tokens) => (registry) => ({ tokens, index: 0, registry });
var makeError = (msg) => (sp) => ({ message: msg, span: sp });
var eofToken = { kind: Eof, lexeme: "", span: { start: { offset: 0, line: 0, column: 0 }, end: { offset: 0, line: 0, column: 0 } } };
var nthToken = (n) => (tokens) => (($match_0) => {
  if ($match_0.$tag === 0) {
    const tok = $match_0.$0;
    return tok;
  }
  if ($match_0.$tag === 1) {
    return (($match_1) => {
      if ($match_1.$tag === 0) {
        const tok = $match_1.$0;
        return tok;
      }
      if ($match_1.$tag === 1) {
        return eofToken;
      }
      throw new Error("Pattern match failed");
    })(last(tokens));
  }
  throw new Error("Pattern match failed");
})(nth(n)(tokens));
var current = (state) => nthToken(state.index)(state.tokens);
var previous = (state) => nthToken($dict_Num_Int._MINUS(state.index)(1))(state.tokens);
var advance = (state) => ({ ...state, index: $dict_Num_Int._PLUS(state.index)(1) });
var peekAhead = (offset) => (state) => nth($dict_Num_Int._PLUS(state.index)(offset))(state.tokens);
var isAtEnd = (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(Eof);
var peekKind = (kind) => (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(kind);
var peekKeyword = (kw) => (state) => ((tok) => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)(kw)))(current(state));
var peekOperator = (op) => (state) => ((tok) => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)(op)))(current(state));
var matchKind = (kind) => (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(kind) ? Just(advance(state)) : Nothing;
var matchOperator = (op) => (state) => ((tok) => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)(op)) ? Just(advance(state)) : Nothing)(current(state));
var matchBlockSep = (state) => matchKind(BlockSep)(state);
var tokenKindStr = (kind) => (($match_2) => {
  if ($match_2.$tag === 0) {
    return "LowerIdentifier";
  }
  if ($match_2.$tag === 1) {
    return "UpperIdentifier";
  }
  if ($match_2.$tag === 2) {
    return "Keyword";
  }
  if ($match_2.$tag === 3) {
    return "Number";
  }
  if ($match_2.$tag === 4) {
    return "String";
  }
  if ($match_2.$tag === 5) {
    return "Char";
  }
  if ($match_2.$tag === 6) {
    return "Operator";
  }
  if ($match_2.$tag === 7) {
    return "Range";
  }
  if ($match_2.$tag === 8) {
    return "Backslash";
  }
  if ($match_2.$tag === 9) {
    return "LParen";
  }
  if ($match_2.$tag === 10) {
    return "RParen";
  }
  if ($match_2.$tag === 11) {
    return "LBrace";
  }
  if ($match_2.$tag === 12) {
    return "RBrace";
  }
  if ($match_2.$tag === 13) {
    return "LBracket";
  }
  if ($match_2.$tag === 14) {
    return "RBracket";
  }
  if ($match_2.$tag === 15) {
    return "Comma";
  }
  if ($match_2.$tag === 16) {
    return "Dot";
  }
  if ($match_2.$tag === 17) {
    return "Colon";
  }
  if ($match_2.$tag === 18) {
    return "Equals";
  }
  if ($match_2.$tag === 19) {
    return "Pipe";
  }
  if ($match_2.$tag === 20) {
    return "Newline";
  }
  if ($match_2.$tag === 21) {
    return "BlockStart";
  }
  if ($match_2.$tag === 22) {
    return "BlockSep";
  }
  if ($match_2.$tag === 23) {
    return "BlockEnd";
  }
  if ($match_2.$tag === 24) {
    return "Eof";
  }
  throw new Error("Pattern match failed");
})(kind);
var expect = (kind) => (label) => (state) => ((tok) => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(kind) ? Ok([tok, advance(state)]) : Err(makeError($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Expected ")(label))(" but found "))(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
var expectKeyword = (kw) => (state) => ((tok) => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)(kw)) ? Ok([tok, advance(state)]) : Err(makeError($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Expected keyword '")(kw))("' but found "))(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
var expectOperator = (op) => (state) => ((tok) => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)(op)) ? Ok([tok, advance(state)]) : Err(makeError($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Expected operator '")(op))("' but found "))(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
var expectBlockStart = (state) => expect(BlockStart)("indented block")(state);
var expectBlockEnd = (state) => expect(BlockEnd)("end of indented block")(state);
var onSameLine = (span) => (tok) => $dict_Eq_Int._EQ_EQ(span.end.line)(tok.span.start.line);
var continuesLayout = (baseIndent) => (lastEnd) => (next) => $dict_Eq_Int._EQ_EQ(next.span.start.line)(lastEnd.line) ? true : $dict_Ord_Int._GT_EQ(next.span.start.column)(baseIndent);
var isBlockToken = (kind) => (($match_3) => {
  if ($match_3.$tag === 21) {
    return true;
  }
  if ($match_3.$tag === 22) {
    return true;
  }
  if ($match_3.$tag === 23) {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(kind);
var exposingSpan = (exp) => (($match_4) => {
  if ($match_4.$tag === 0) {
    const span = $match_4.$0;
    return span;
  }
  if ($match_4.$tag === 1) {
    const span = $match_4.$1;
    return span;
  }
  throw new Error("Pattern match failed");
})(exp);
var typeSpan = (t) => (($match_5) => {
  if ($match_5.$tag === 0) {
    const sp = $match_5.$2;
    return sp;
  }
  if ($match_5.$tag === 1) {
    const sp = $match_5.$2;
    return sp;
  }
  if ($match_5.$tag === 2) {
    const sp = $match_5.$1;
    return sp;
  }
  if ($match_5.$tag === 3) {
    const sp = $match_5.$1;
    return sp;
  }
  if ($match_5.$tag === 4) {
    const sp = $match_5.$2;
    return sp;
  }
  throw new Error("Pattern match failed");
})(t);
var exprSpan = (e) => (($match_6) => {
  if ($match_6.$tag === 0) {
    const sp = $match_6.$2;
    return sp;
  }
  if ($match_6.$tag === 1) {
    const sp = $match_6.$1;
    return sp;
  }
  if ($match_6.$tag === 2) {
    const sp = $match_6.$1;
    return sp;
  }
  if ($match_6.$tag === 3) {
    const sp = $match_6.$1;
    return sp;
  }
  if ($match_6.$tag === 4) {
    const sp = $match_6.$1;
    return sp;
  }
  if ($match_6.$tag === 5) {
    const sp = $match_6.$2;
    return sp;
  }
  if ($match_6.$tag === 6) {
    const sp = $match_6.$2;
    return sp;
  }
  if ($match_6.$tag === 7) {
    const sp = $match_6.$3;
    return sp;
  }
  if ($match_6.$tag === 8) {
    const sp = $match_6.$2;
    return sp;
  }
  if ($match_6.$tag === 9) {
    const sp = $match_6.$2;
    return sp;
  }
  if ($match_6.$tag === 10) {
    const sp = $match_6.$3;
    return sp;
  }
  if ($match_6.$tag === 11) {
    const sp = $match_6.$2;
    return sp;
  }
  if ($match_6.$tag === 12) {
    const sp = $match_6.$1;
    return sp;
  }
  if ($match_6.$tag === 13) {
    const sp = $match_6.$1;
    return sp;
  }
  if ($match_6.$tag === 14) {
    const sp = $match_6.$0;
    return sp;
  }
  if ($match_6.$tag === 15) {
    const sp = $match_6.$1;
    return sp;
  }
  if ($match_6.$tag === 16) {
    const sp = $match_6.$2;
    return sp;
  }
  if ($match_6.$tag === 17) {
    const sp = $match_6.$1;
    return sp;
  }
  if ($match_6.$tag === 18) {
    const sp = $match_6.$2;
    return sp;
  }
  if ($match_6.$tag === 19) {
    const sp = $match_6.$2;
    return sp;
  }
  throw new Error("Pattern match failed");
})(e);
var patSpan = (p) => (($match_7) => {
  if ($match_7.$tag === 0) {
    const sp = $match_7.$1;
    return sp;
  }
  if ($match_7.$tag === 1) {
    const sp = $match_7.$0;
    return sp;
  }
  if ($match_7.$tag === 2) {
    const sp = $match_7.$2;
    return sp;
  }
  if ($match_7.$tag === 3) {
    const sp = $match_7.$1;
    return sp;
  }
  if ($match_7.$tag === 4) {
    const sp = $match_7.$1;
    return sp;
  }
  if ($match_7.$tag === 5) {
    const sp = $match_7.$2;
    return sp;
  }
  if ($match_7.$tag === 6) {
    const sp = $match_7.$1;
    return sp;
  }
  if ($match_7.$tag === 7) {
    const sp = $match_7.$1;
    return sp;
  }
  if ($match_7.$tag === 8) {
    const sp = $match_7.$1;
    return sp;
  }
  if ($match_7.$tag === 9) {
    const sp = $match_7.$1;
    return sp;
  }
  if ($match_7.$tag === 10) {
    const sp = $match_7.$1;
    return sp;
  }
  throw new Error("Pattern match failed");
})(p);
var isExpressionStart = (tok) => isBlockToken(tok.kind) ? false : (($match_8) => {
  if ($match_8.$tag === 0) {
    return true;
  }
  if ($match_8.$tag === 1) {
    return true;
  }
  if ($match_8.$tag === 3) {
    return true;
  }
  if ($match_8.$tag === 4) {
    return true;
  }
  if ($match_8.$tag === 5) {
    return true;
  }
  if ($match_8.$tag === 9) {
    return true;
  }
  if ($match_8.$tag === 13) {
    return true;
  }
  if ($match_8.$tag === 11) {
    return true;
  }
  if ($match_8.$tag === 8) {
    return true;
  }
  if ($match_8.$tag === 2) {
    return (($match_9) => {
      if ($match_9 === "if") {
        return true;
      }
      if ($match_9 === "let") {
        return true;
      }
      if ($match_9 === "case") {
        return true;
      }
      {
        return false;
      }
      throw new Error("Pattern match failed");
    })(tok.lexeme);
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(tok.kind);
var isTypeStart = (tok) => isBlockToken(tok.kind) ? false : (($match_10) => {
  if ($match_10.$tag === 0) {
    return true;
  }
  if ($match_10.$tag === 1) {
    return true;
  }
  if ($match_10.$tag === 9) {
    return true;
  }
  if ($match_10.$tag === 11) {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(tok.kind);
var isPatternStart = (tok) => isBlockToken(tok.kind) ? false : (($match_11) => {
  if ($match_11.$tag === 0) {
    return true;
  }
  if ($match_11.$tag === 1) {
    return true;
  }
  if ($match_11.$tag === 9) {
    return true;
  }
  if ($match_11.$tag === 13) {
    return true;
  }
  if ($match_11.$tag === 3) {
    return true;
  }
  if ($match_11.$tag === 4) {
    return true;
  }
  if ($match_11.$tag === 5) {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(tok.kind);
var isFunctionParamPatternStart = (tok) => isBlockToken(tok.kind) ? false : (($match_12) => {
  if ($match_12.$tag === 0) {
    return true;
  }
  if ($match_12.$tag === 1) {
    return true;
  }
  if ($match_12.$tag === 9) {
    return true;
  }
  if ($match_12.$tag === 11) {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(tok.kind);
var peekAheadSkipLayoutLoop = (j) => (remaining) => (tokens) => {
  while (true) {
    {
      const $match_13 = nth(j)(tokens);
      if ($match_13.$tag === 1) {
        return Nothing;
      }
      if ($match_13.$tag === 0) {
        const tok = $match_13.$0;
        if (isBlockToken(tok.kind)) {
          [j, remaining, tokens] = [$dict_Num_Int._PLUS(j)(1), remaining, tokens];
          continue;
        } else {
          if ($dict_Eq_Int._EQ_EQ(remaining)(0)) {
            return Just(tok);
          } else {
            [j, remaining, tokens] = [$dict_Num_Int._PLUS(j)(1), $dict_Num_Int._MINUS(remaining)(1), tokens];
            continue;
          }
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var peekAheadSkipLayout = (offset) => (state) => peekAheadSkipLayoutLoop(state.index)(offset)(state.tokens);
var peekConstraintContextLoop = (i) => (limit) => (state) => {
  while (true) {
    if ($dict_Ord_Int._GT(limit)(20)) {
      return false;
    } else {
      {
        const $match_14 = peekAheadSkipLayout(i)(state);
        if ($match_14.$tag === 1) {
          return false;
        }
        if ($match_14.$tag === 0) {
          const tok = $match_14.$0;
          if (_AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)("=>"))) {
            return true;
          } else {
            if (_AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)("where"))) {
              return false;
            } else {
              [i, limit, state] = [$dict_Num_Int._PLUS(i)(1), $dict_Num_Int._PLUS(limit)(1), state];
              continue;
            }
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var peekConstraintContext = (state) => peekConstraintContextLoop(0)(0)(state);
var peekConstraintCtxProtoLoop = (i) => (parenDepth) => (limit) => (state) => {
  while (true) {
    if ($dict_Ord_Int._GT(limit)(30)) {
      return false;
    } else {
      {
        const $match_15 = peekAheadSkipLayout(i)(state);
        if ($match_15.$tag === 1) {
          return false;
        }
        if ($match_15.$tag === 0) {
          const tok = $match_15.$0;
          {
            const newDepth = (($match_16) => {
              if ($match_16.$tag === 9) {
                return $dict_Num_Int._PLUS(parenDepth)(1);
              }
              if ($match_16.$tag === 10) {
                return $dict_Num_Int._MINUS(parenDepth)(1);
              }
              {
                return parenDepth;
              }
              throw new Error("Pattern match failed");
            })(tok.kind);
            if (_AMP_AMP4($dict_Eq_Int._EQ_EQ(newDepth)(0))(() => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)("=>")))) {
              return true;
            } else {
              if (_AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)("where"))) {
                return false;
              } else {
                [i, parenDepth, limit, state] = [$dict_Num_Int._PLUS(i)(1), newDepth, $dict_Num_Int._PLUS(limit)(1), state];
                continue;
              }
            }
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var peekConstraintContextInProtocol = (state) => peekConstraintCtxProtoLoop(0)(0)(0)(state);
var peekConstraintCtxTypeLoop = (i) => (parenDepth) => (limit) => (startLine) => (state) => {
  while (true) {
    if ($dict_Ord_Int._GT(limit)(30)) {
      return false;
    } else {
      {
        const $match_17 = peekAheadSkipLayout(i)(state);
        if ($match_17.$tag === 1) {
          return false;
        }
        if ($match_17.$tag === 0) {
          const tok = $match_17.$0;
          if ($dict_Eq_Int._SLASH_EQ(tok.span.start.line)(startLine)) {
            return false;
          } else {
            {
              const newDepth = (($match_18) => {
                if ($match_18.$tag === 9) {
                  return $dict_Num_Int._PLUS(parenDepth)(1);
                }
                if ($match_18.$tag === 10) {
                  return $dict_Num_Int._MINUS(parenDepth)(1);
                }
                {
                  return parenDepth;
                }
                throw new Error("Pattern match failed");
              })(tok.kind);
              if (_AMP_AMP4($dict_Eq_Int._EQ_EQ(newDepth)(0))(() => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)("=>")))) {
                return true;
              } else {
                if (_AMP_AMP4($dict_Eq_Int._EQ_EQ(newDepth)(0))(() => _PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Equals))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword)))) {
                  return false;
                } else {
                  [i, parenDepth, limit, startLine, state] = [$dict_Num_Int._PLUS(i)(1), newDepth, $dict_Num_Int._PLUS(limit)(1), startLine, state];
                  continue;
                }
              }
            }
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var peekConstraintContextInType = (state) => ((startLine) => peekConstraintCtxTypeLoop(0)(0)(0)(startLine)(state))(current(state).span.start.line);
var peekConstraintCtxTypeDeclLoop = (i) => (parenDepth) => (limit) => (state) => {
  while (true) {
    if ($dict_Ord_Int._GT(limit)(50)) {
      return false;
    } else {
      {
        const $match_19 = peekAheadSkipLayout(i)(state);
        if ($match_19.$tag === 1) {
          return false;
        }
        if ($match_19.$tag === 0) {
          const tok = $match_19.$0;
          {
            const newDepth = (($match_20) => {
              if ($match_20.$tag === 9) {
                return $dict_Num_Int._PLUS(parenDepth)(1);
              }
              if ($match_20.$tag === 10) {
                return $dict_Num_Int._MINUS(parenDepth)(1);
              }
              {
                return parenDepth;
              }
              throw new Error("Pattern match failed");
            })(tok.kind);
            if (_AMP_AMP4($dict_Eq_Int._EQ_EQ(newDepth)(0))(() => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)("=>")))) {
              return true;
            } else {
              if (_AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Equals))(() => $dict_Eq_Int._EQ_EQ(newDepth)(0))) {
                return false;
              } else {
                if (_AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword))(() => $dict_Eq_Int._EQ_EQ(newDepth)(0))) {
                  {
                    const $match_21 = tok.lexeme;
                    if ($match_21 === "implement") {
                      return false;
                    }
                    if ($match_21 === "protocol") {
                      return false;
                    }
                    if ($match_21 === "type") {
                      return false;
                    }
                    if ($match_21 === "module") {
                      return false;
                    }
                    if ($match_21 === "import") {
                      return false;
                    }
                    if ($match_21 === "infix") {
                      return false;
                    }
                    if ($match_21 === "infixl") {
                      return false;
                    }
                    if ($match_21 === "infixr") {
                      return false;
                    }
                    {
                      [i, parenDepth, limit, state] = [$dict_Num_Int._PLUS(i)(1), newDepth, $dict_Num_Int._PLUS(limit)(1), state];
                      continue;
                    }
                    throw new Error("Pattern match failed");
                  }
                } else {
                  [i, parenDepth, limit, state] = [$dict_Num_Int._PLUS(i)(1), newDepth, $dict_Num_Int._PLUS(limit)(1), state];
                  continue;
                }
              }
            }
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var peekConstraintContextInTypeDecl = (state) => peekConstraintCtxTypeDeclLoop(0)(0)(0)(state);
var peekDecorator = (state) => ((cur) => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(cur.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(cur.lexeme)("@")) ? (($match_22) => {
  if ($match_22.$tag === 1) {
    return false;
  }
  if ($match_22.$tag === 0) {
    const next = $match_22.$0;
    return _PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(next.kind)(LowerIdentifier))(() => $dict_Eq_TokenKind._EQ_EQ(next.kind)(Keyword));
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : false)(current(state));
var intToStr = (n) => $dict_Show_Int.toString(n);
var stringToInt = (s) => (($match_23) => {
  if ($match_23.$tag === 0) {
    const n = $match_23.$0;
    return n;
  }
  if ($match_23.$tag === 1) {
    return 0;
  }
  throw new Error("Pattern match failed");
})(toInt(s));
var parseModuleNameParts = (name) => (end) => (state) => {
  while (true) {
    if (peekKind(Dot)(state)) {
      {
        const s1 = advance(state);
        {
          const $match_24 = expect(UpperIdentifier)("module name segment")(s1);
          if ($match_24.$tag === 1) {
            const e = $match_24.$0;
            return Err(e);
          }
          if ($match_24.$tag === 0) {
            const next = $match_24.$0[0];
            const s2 = $match_24.$0[1];
            [name, end, state] = [$dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(name)("."))(next.lexeme), next.span.end, s2];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
    } else {
      return Ok([{ name, nameEnd: end }, state]);
    }
  }
};
var parseModuleName = (state) => (($match_25) => {
  if ($match_25.$tag === 1) {
    const e = $match_25.$0;
    return Err(e);
  }
  if ($match_25.$tag === 0) {
    const first = $match_25.$0[0];
    const s1 = $match_25.$0[1];
    return parseModuleNameParts(first.lexeme)(first.span.end)(s1);
  }
  throw new Error("Pattern match failed");
})(expect(UpperIdentifier)("module name")(state));
var parseExportMember = (state) => ((tok) => (($match_26) => {
  if ($match_26.$tag === 9) {
    return ((s1) => (($match_27) => {
      if ($match_27.$tag === 1) {
        const e = $match_27.$0;
        return Err(e);
      }
      if ($match_27.$tag === 0) {
        const opTok = $match_27.$0[0];
        const s2 = $match_27.$0[1];
        return (($match_28) => {
          if ($match_28.$tag === 1) {
            const e = $match_28.$0;
            return Err(e);
          }
          if ($match_28.$tag === 0) {
            const s3 = $match_28.$0[1];
            return Ok([opTok.lexeme, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("closing paren for operator member")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator)("operator in member list")(s1)))(advance(state));
  }
  if ($match_26.$tag === 1) {
    return ((s1) => Ok([tok.lexeme, s1]))(advance(state));
  }
  if ($match_26.$tag === 0) {
    return ((s1) => Ok([tok.lexeme, s1]))(advance(state));
  }
  {
    return Err(makeError($dict_Appendable_String._PLUS_PLUS("Expected member name in export list, got ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var parseExportMembers = (state) => (($match_29) => {
  if ($match_29.$tag === 1) {
    const e = $match_29.$0;
    return Err(e);
  }
  if ($match_29.$tag === 0) {
    const name = $match_29.$0[0];
    const s1 = $match_29.$0[1];
    return peekKind(Comma)(s1) ? ((s2) => (($match_30) => {
      if ($match_30.$tag === 1) {
        const e = $match_30.$0;
        return Err(e);
      }
      if ($match_30.$tag === 0) {
        const rest = $match_30.$0[0];
        const s3 = $match_30.$0[1];
        return Ok([_COLON_COLON(name)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseExportMembers(s2)))(advance(s1)) : Ok([[name], s1]);
  }
  throw new Error("Pattern match failed");
})(parseExportMember(state));
var parseExportSpec = (state) => ((tok) => (($match_31) => {
  if ($match_31.$tag === 9) {
    return ((s1) => (($match_32) => {
      if ($match_32.$tag === 1) {
        const e = $match_32.$0;
        return Err(e);
      }
      if ($match_32.$tag === 0) {
        const opTok = $match_32.$0[0];
        const s2 = $match_32.$0[1];
        return (($match_33) => {
          if ($match_33.$tag === 1) {
            const e = $match_33.$0;
            return Err(e);
          }
          if ($match_33.$tag === 0) {
            const cp = $match_33.$0[0];
            const s3 = $match_33.$0[1];
            return Ok([ExportOperator(opTok.lexeme)({ start: tok.span.start, end: cp.span.end }), s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("closing paren for operator export")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator)("operator in export")(s1)))(advance(state));
  }
  if ($match_31.$tag === 0) {
    return ((s1) => Ok([ExportValue(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_31.$tag === 1) {
    return ((nameTok) => ((s1) => peekKind(LParen)(s1) ? ((s2) => peekKind(Range)(s2) ? ((s3) => (($match_34) => {
      if ($match_34.$tag === 1) {
        const e = $match_34.$0;
        return Err(e);
      }
      if ($match_34.$tag === 0) {
        const cp = $match_34.$0[0];
        const s4 = $match_34.$0[1];
        return Ok([ExportTypeAll(nameTok.lexeme)({ start: nameTok.span.start, end: cp.span.end }), s4]);
      }
      throw new Error("Pattern match failed");
    })(expect(RParen)("closing paren for type export")(s3)))(advance(s2)) : (($match_35) => {
      if ($match_35.$tag === 1) {
        const e = $match_35.$0;
        return Err(e);
      }
      if ($match_35.$tag === 0) {
        const members = $match_35.$0[0];
        const s3 = $match_35.$0[1];
        return (($match_36) => {
          if ($match_36.$tag === 1) {
            const e = $match_36.$0;
            return Err(e);
          }
          if ($match_36.$tag === 0) {
            const cp = $match_36.$0[0];
            const s4 = $match_36.$0[1];
            return Ok([ExportTypeSome(nameTok.lexeme)(members)({ start: nameTok.span.start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("closing paren for type export")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseExportMembers(s2)))(advance(s1)) : Ok([ExportValue(nameTok.lexeme)(nameTok.span), s1]))(advance(state)))(tok);
  }
  {
    return Err(makeError($dict_Appendable_String._PLUS_PLUS("Expected export specification, got ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var parseExportSpecList = (state) => (($match_37) => {
  if ($match_37.$tag === 1) {
    const e = $match_37.$0;
    return Err(e);
  }
  if ($match_37.$tag === 0) {
    const spec = $match_37.$0[0];
    const s1 = $match_37.$0[1];
    return peekKind(Comma)(s1) ? ((s2) => (($match_38) => {
      if ($match_38.$tag === 1) {
        const e = $match_38.$0;
        return Err(e);
      }
      if ($match_38.$tag === 0) {
        const rest = $match_38.$0[0];
        const s3 = $match_38.$0[1];
        return Ok([_COLON_COLON(spec)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseExportSpecList(s2)))(advance(s1)) : Ok([[spec], s1]);
  }
  throw new Error("Pattern match failed");
})(parseExportSpec(state));
var parseExposing = (state) => (($match_39) => {
  if ($match_39.$tag === 1) {
    const e = $match_39.$0;
    return Err(e);
  }
  if ($match_39.$tag === 0) {
    const openParen = $match_39.$0[0];
    const s1 = $match_39.$0[1];
    return ((start) => peekKind(Range)(s1) ? ((s2) => (($match_40) => {
      if ($match_40.$tag === 1) {
        const e = $match_40.$0;
        return Err(e);
      }
      if ($match_40.$tag === 0) {
        const closeParen = $match_40.$0[0];
        const s3 = $match_40.$0[1];
        return Ok([ExposeAll({ start, end: closeParen.span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RParen)("close exposing (..)")(s2)))(advance(s1)) : (($match_41) => {
      if ($match_41.$tag === 1) {
        const e = $match_41.$0;
        return Err(e);
      }
      if ($match_41.$tag === 0) {
        const specs = $match_41.$0[0];
        const s2 = $match_41.$0[1];
        return (($match_42) => {
          if ($match_42.$tag === 1) {
            const e = $match_42.$0;
            return Err(e);
          }
          if ($match_42.$tag === 0) {
            const closeParen = $match_42.$0[0];
            const s3 = $match_42.$0[1];
            return Ok([ExposeExplicit(specs)({ start, end: closeParen.span.end }), s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("close exposing list")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseExportSpecList(s1)))(openParen.span.start);
  }
  throw new Error("Pattern match failed");
})(expect(LParen)("exposing list start")(state));
var parseModuleDeclaration = (state) => (($match_43) => {
  if ($match_43.$tag === 1) {
    const e = $match_43.$0;
    return Err(e);
  }
  if ($match_43.$tag === 0) {
    const modTok = $match_43.$0[0];
    const s1 = $match_43.$0[1];
    return (($match_44) => {
      if ($match_44.$tag === 1) {
        const e = $match_44.$0;
        return Err(e);
      }
      if ($match_44.$tag === 0) {
        const nameInfo = $match_44.$0[0];
        const s2 = $match_44.$0[1];
        return peekKeyword("exposing")(s2) ? (($match_45) => {
          if ($match_45.$tag === 1) {
            const e = $match_45.$0;
            return Err(e);
          }
          if ($match_45.$tag === 0) {
            const s3 = $match_45.$0[1];
            return (($match_46) => {
              if ($match_46.$tag === 1) {
                const e = $match_46.$0;
                return Err(e);
              }
              if ($match_46.$tag === 0) {
                const exp = $match_46.$0[0];
                const s4 = $match_46.$0[1];
                return Ok([{ name: nameInfo.name, exposingClause: exp, hasExposing: true, span: { start: modTok.span.start, end: exposingSpan(exp).end } }, s4]);
              }
              throw new Error("Pattern match failed");
            })(parseExposing(s3));
          }
          throw new Error("Pattern match failed");
        })(expectKeyword("exposing")(s2)) : Ok([{ name: nameInfo.name, exposingClause: ExposeAll({ start: nameInfo.nameEnd, end: nameInfo.nameEnd }), hasExposing: false, span: { start: modTok.span.start, end: nameInfo.nameEnd } }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseModuleName(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("module")(state));
var parseImportAlias = (state) => peekKeyword("as")(state) ? (($match_47) => {
  if ($match_47.$tag === 1) {
    const e = $match_47.$0;
    return Err(e);
  }
  if ($match_47.$tag === 0) {
    const s1 = $match_47.$0[1];
    return (($match_48) => {
      if ($match_48.$tag === 1) {
        const e = $match_48.$0;
        return Err(e);
      }
      if ($match_48.$tag === 0) {
        const aliasTok = $match_48.$0[0];
        const s2 = $match_48.$0[1];
        return Ok([{ aliasName: aliasTok.lexeme, hasAlias: true }, s2]);
      }
      throw new Error("Pattern match failed");
    })(expect(UpperIdentifier)("import alias")(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("as")(state)) : Ok([{ aliasName: "", hasAlias: false }, state]);
var parseImportExposing = (state) => ((dummySpan) => peekKeyword("exposing")(state) ? (($match_49) => {
  if ($match_49.$tag === 1) {
    const e = $match_49.$0;
    return Err(e);
  }
  if ($match_49.$tag === 0) {
    const s1 = $match_49.$0[1];
    return (($match_50) => {
      if ($match_50.$tag === 1) {
        const e = $match_50.$0;
        return Err(e);
      }
      if ($match_50.$tag === 0) {
        const exp = $match_50.$0[0];
        const s2 = $match_50.$0[1];
        return Ok([{ expClause: exp, hasExp: true }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseExposing(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("exposing")(state)) : Ok([{ expClause: ExposeAll(dummySpan), hasExp: false }, state]))({ start: { offset: 0, line: 0, column: 0 }, end: { offset: 0, line: 0, column: 0 } });
var parseImport = (state) => (($match_51) => {
  if ($match_51.$tag === 1) {
    const e = $match_51.$0;
    return Err(e);
  }
  if ($match_51.$tag === 0) {
    const importTok = $match_51.$0[0];
    const s1 = $match_51.$0[1];
    return (($match_52) => {
      if ($match_52.$tag === 1) {
        const e = $match_52.$0;
        return Err(e);
      }
      if ($match_52.$tag === 0) {
        const nameInfo = $match_52.$0[0];
        const s2 = $match_52.$0[1];
        return (($match_53) => {
          if ($match_53.$tag === 1) {
            const e = $match_53.$0;
            return Err(e);
          }
          if ($match_53.$tag === 0) {
            const aliasResult = $match_53.$0[0];
            const s3 = $match_53.$0[1];
            return (($match_54) => {
              if ($match_54.$tag === 1) {
                const e = $match_54.$0;
                return Err(e);
              }
              if ($match_54.$tag === 0) {
                const expResult = $match_54.$0[0];
                const s4 = $match_54.$0[1];
                return ((endPos) => Ok([{ moduleName: nameInfo.name, aliasName: aliasResult.aliasName, exposingClause: expResult.expClause, hasAlias: aliasResult.hasAlias, hasExposing: expResult.hasExp, span: { start: importTok.span.start, end: endPos } }, s4]))(expResult.hasExp ? exposingSpan(expResult.expClause).end : aliasResult.hasAlias ? previous(s3).span.end : nameInfo.nameEnd);
              }
              throw new Error("Pattern match failed");
            })(parseImportExposing(s3));
          }
          throw new Error("Pattern match failed");
        })(parseImportAlias(s2));
      }
      throw new Error("Pattern match failed");
    })(parseModuleName(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("import")(state));
var parseImports = (state) => peekKeyword("import")(state) ? ((tok) => $dict_Eq_Int._EQ_EQ(tok.span.start.column)(1) ? (($match_55) => {
  if ($match_55.$tag === 1) {
    const e = $match_55.$0;
    return Err(e);
  }
  if ($match_55.$tag === 0) {
    const imp = $match_55.$0[0];
    const s1 = $match_55.$0[1];
    return (($match_56) => {
      if ($match_56.$tag === 1) {
        const e = $match_56.$0;
        return Err(e);
      }
      if ($match_56.$tag === 0) {
        const rest = $match_56.$0[0];
        const s2 = $match_56.$0[1];
        return Ok([_COLON_COLON(imp)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseImports(s1));
  }
  throw new Error("Pattern match failed");
})(parseImport(state)) : Err(makeError("import declaration must start at column 1")(tok.span)))(current(state)) : Ok([[], state]);
var unquote = (lexeme) => _AMP_AMP4($dict_Ord_Int._GT_EQ(length(lexeme))(2))(() => _AMP_AMP4(startsWith('"')(lexeme))(() => endsWith('"')(lexeme))) ? slice(1)($dict_Num_Int._MINUS(length(lexeme))(1))(lexeme) : lexeme;
var parseDecoratorArgs = (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(StringToken) ? ((tok) => ((s1) => ((unquoted) => (($match_57) => {
  if ($match_57.$tag === 1) {
    const e = $match_57.$0;
    return Err(e);
  }
  if ($match_57.$tag === 0) {
    const rest = $match_57.$0[0];
    const s2 = $match_57.$0[1];
    return Ok([_COLON_COLON(unquoted)(rest), s2]);
  }
  throw new Error("Pattern match failed");
})(parseDecoratorArgs(s1)))(unquote(tok.lexeme)))(advance(state)))(current(state)) : Ok([[], state]);
var parseDeclarationName = (state) => ((tok) => (($match_58) => {
  if ($match_58.$tag === 0) {
    return ((s1) => Ok([{ declName: tok.lexeme, declSpan: tok.span }, s1]))(advance(state));
  }
  if ($match_58.$tag === 9) {
    return ((s1) => (($match_59) => {
      if ($match_59.$tag === 1) {
        const e = $match_59.$0;
        return Err(e);
      }
      if ($match_59.$tag === 0) {
        const opTok = $match_59.$0[0];
        const s2 = $match_59.$0[1];
        return (($match_60) => {
          if ($match_60.$tag === 1) {
            const e = $match_60.$0;
            return Err(e);
          }
          if ($match_60.$tag === 0) {
            const cp = $match_60.$0[0];
            const s3 = $match_60.$0[1];
            return Ok([{ declName: opTok.lexeme, declSpan: { start: tok.span.start, end: cp.span.end } }, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("close operator name")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator)("operator name")(s1)))(advance(state));
  }
  {
    return Err(makeError($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Expected declaration name but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var setTypeSpan = (typeExpr) => (span) => (($match_61) => {
  if ($match_61.$tag === 0) {
    const name = $match_61.$0;
    const args = $match_61.$1;
    return TRef(name)(args)(span);
  }
  if ($match_61.$tag === 1) {
    const from = $match_61.$0;
    const to = $match_61.$1;
    return TFunction(from)(to)(span);
  }
  if ($match_61.$tag === 2) {
    const elems = $match_61.$0;
    return TTuple(elems)(span);
  }
  if ($match_61.$tag === 3) {
    const fields = $match_61.$0;
    return TRecord(fields)(span);
  }
  if ($match_61.$tag === 4) {
    const constraints = $match_61.$0;
    const typ = $match_61.$1;
    return TQualified(constraints)(typ)(span);
  }
  throw new Error("Pattern match failed");
})(typeExpr);
var parseQualifiedName = (name) => (end) => (state) => {
  while (true) {
    if (peekKind(Dot)(state)) {
      {
        const $match_62 = peekAhead(1)(state);
        if ($match_62.$tag === 1) {
          return Ok([{ name, nameEnd: end }, state]);
        }
        if ($match_62.$tag === 0) {
          const next = $match_62.$0;
          if ($dict_Eq_TokenKind._EQ_EQ(next.kind)(UpperIdentifier)) {
            {
              const s1 = advance(state);
              {
                const $match_63 = expect(UpperIdentifier)("type name part")(s1);
                if ($match_63.$tag === 1) {
                  const e = $match_63.$0;
                  return Err(e);
                }
                if ($match_63.$tag === 0) {
                  const nextTok = $match_63.$0[0];
                  const s2 = $match_63.$0[1];
                  [name, end, state] = [$dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(name)("."))(nextTok.lexeme), nextTok.span.end, s2];
                  continue;
                }
                throw new Error("Pattern match failed");
              }
            }
          } else {
            return Ok([{ name, nameEnd: end }, state]);
          }
        }
        throw new Error("Pattern match failed");
      }
    } else {
      return Ok([{ name, nameEnd: end }, state]);
    }
  }
};
var parseConstraints;
var parseConstraintList;
var parseConstraint;
var parseConstraintTypeArgs;
var parseTypeExpression;
var parseTypeArrow;
var parseTypeTerm;
var parseRecordType;
var parseRecordTypeFieldList;
var parseMoreRecordTypeFields;
var parseRecordTypeField;
var parseParenOrTupleType;
var parseMoreTupleTypes;
var parseTypeRef;
var parseTypeApplicationArgs;
var parseTypeAtom;
parseConstraints = (state) => peekKind(LParen)(state) ? ((s1) => (($match_64) => {
  if ($match_64.$tag === 1) {
    const e = $match_64.$0;
    return Err(e);
  }
  if ($match_64.$tag === 0) {
    const constraints = $match_64.$0[0];
    const s2 = $match_64.$0[1];
    return (($match_65) => {
      if ($match_65.$tag === 1) {
        const e = $match_65.$0;
        return Err(e);
      }
      if ($match_65.$tag === 0) {
        const s3 = $match_65.$0[1];
        return (($match_66) => {
          if ($match_66.$tag === 1) {
            const e = $match_66.$0;
            return Err(e);
          }
          if ($match_66.$tag === 0) {
            const s4 = $match_66.$0[1];
            return Ok([constraints, s4]);
          }
          throw new Error("Pattern match failed");
        })(expectOperator("=>")(s3));
      }
      throw new Error("Pattern match failed");
    })(expect(RParen)("close constraint list")(s2));
  }
  throw new Error("Pattern match failed");
})(parseConstraintList(s1)))(advance(state)) : (($match_67) => {
  if ($match_67.$tag === 1) {
    const e = $match_67.$0;
    return Err(e);
  }
  if ($match_67.$tag === 0) {
    const c = $match_67.$0[0];
    const s1 = $match_67.$0[1];
    return (($match_68) => {
      if ($match_68.$tag === 1) {
        const e = $match_68.$0;
        return Err(e);
      }
      if ($match_68.$tag === 0) {
        const s2 = $match_68.$0[1];
        return Ok([[c], s2]);
      }
      throw new Error("Pattern match failed");
    })(expectOperator("=>")(s1));
  }
  throw new Error("Pattern match failed");
})(parseConstraint(state));
parseConstraintList = (state) => (($match_69) => {
  if ($match_69.$tag === 1) {
    const e = $match_69.$0;
    return Err(e);
  }
  if ($match_69.$tag === 0) {
    const c = $match_69.$0[0];
    const s1 = $match_69.$0[1];
    return peekKind(Comma)(s1) ? ((s2) => (($match_70) => {
      if ($match_70.$tag === 1) {
        const e = $match_70.$0;
        return Err(e);
      }
      if ($match_70.$tag === 0) {
        const rest = $match_70.$0[0];
        const s3 = $match_70.$0[1];
        return Ok([_COLON_COLON(c)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseConstraintList(s2)))(advance(s1)) : Ok([[c], s1]);
  }
  throw new Error("Pattern match failed");
})(parseConstraint(state));
parseConstraint = (state) => ((start) => (($match_71) => {
  if ($match_71.$tag === 1) {
    const e = $match_71.$0;
    return Err(e);
  }
  if ($match_71.$tag === 0) {
    const protocolTok = $match_71.$0[0];
    const s1 = $match_71.$0[1];
    return (($match_72) => {
      if ($match_72.$tag === 1) {
        const e = $match_72.$0;
        return Err(e);
      }
      if ($match_72.$tag === 0) {
        const typeArgs = $match_72.$0[0];
        const s2 = $match_72.$0[1];
        return isEmpty2(typeArgs) ? Err(makeError("Expected at least one type argument in constraint")(current(s2).span)) : ((endPos) => Ok([{ protocolName: protocolTok.lexeme, typeArgs, span: { start, end: endPos } }, s2]))((($match_73) => {
          if ($match_73.$tag === 0) {
            const t = $match_73.$0;
            return typeSpan(t).end;
          }
          if ($match_73.$tag === 1) {
            return protocolTok.span.end;
          }
          throw new Error("Pattern match failed");
        })(last(typeArgs)));
      }
      throw new Error("Pattern match failed");
    })(parseConstraintTypeArgs(s1));
  }
  throw new Error("Pattern match failed");
})(expect(UpperIdentifier)("protocol name in constraint")(state)))(current(state).span.start);
parseConstraintTypeArgs = (state) => isTypeStart(current(state)) ? (($match_74) => {
  if ($match_74.$tag === 1) {
    const e = $match_74.$0;
    return Err(e);
  }
  if ($match_74.$tag === 0) {
    const t = $match_74.$0[0];
    const s1 = $match_74.$0[1];
    return (($match_75) => {
      if ($match_75.$tag === 1) {
        const e = $match_75.$0;
        return Err(e);
      }
      if ($match_75.$tag === 0) {
        const rest = $match_75.$0[0];
        const s2 = $match_75.$0[1];
        return Ok([_COLON_COLON(t)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseConstraintTypeArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeTerm(state)) : Ok([[], state]);
parseTypeExpression = (state) => ((start) => peekConstraintContextInType(state) ? (($match_76) => {
  if ($match_76.$tag === 1) {
    const e = $match_76.$0;
    return Err(e);
  }
  if ($match_76.$tag === 0) {
    const constraints = $match_76.$0[0];
    const s1 = $match_76.$0[1];
    return (($match_77) => {
      if ($match_77.$tag === 1) {
        const e = $match_77.$0;
        return Err(e);
      }
      if ($match_77.$tag === 0) {
        const typ = $match_77.$0[0];
        const s2 = $match_77.$0[1];
        return Ok([TQualified(constraints)(typ)({ start, end: typeSpan(typ).end }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeArrow(s1));
  }
  throw new Error("Pattern match failed");
})(parseConstraints(state)) : parseTypeArrow(state))(current(state).span.start);
parseTypeArrow = (state) => (($match_78) => {
  if ($match_78.$tag === 1) {
    const e = $match_78.$0;
    return Err(e);
  }
  if ($match_78.$tag === 0) {
    const left = $match_78.$0[0];
    const s1 = $match_78.$0[1];
    return (($match_79) => {
      if ($match_79.$tag === 1) {
        return Ok([left, s1]);
      }
      if ($match_79.$tag === 0) {
        const s2 = $match_79.$0;
        return (($match_80) => {
          if ($match_80.$tag === 1) {
            const e = $match_80.$0;
            return Err(e);
          }
          if ($match_80.$tag === 0) {
            const right = $match_80.$0[0];
            const s3 = $match_80.$0[1];
            return Ok([TFunction(left)(right)({ start: typeSpan(left).start, end: typeSpan(right).end }), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseTypeArrow(s2));
      }
      throw new Error("Pattern match failed");
    })(matchOperator("->")(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeTerm(state));
parseTypeTerm = (state) => ((tok) => (($match_81) => {
  if ($match_81.$tag === 11) {
    return parseRecordType(state);
  }
  if ($match_81.$tag === 9) {
    return parseParenOrTupleType(state);
  }
  {
    return parseTypeRef(state);
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseRecordType = (state) => ((start) => ((s1) => peekKind(RBrace)(s1) ? ((s2) => Ok([TRecord([])({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : (($match_82) => {
  if ($match_82.$tag === 1) {
    const e = $match_82.$0;
    return Err(e);
  }
  if ($match_82.$tag === 0) {
    const fields = $match_82.$0[0];
    const s2 = $match_82.$0[1];
    return (($match_83) => {
      if ($match_83.$tag === 1) {
        const e = $match_83.$0;
        return Err(e);
      }
      if ($match_83.$tag === 0) {
        const s3 = $match_83.$0[1];
        return Ok([TRecord(fields)({ start, end: previous(s3).span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RBrace)("close record type")(s2));
  }
  throw new Error("Pattern match failed");
})(parseRecordTypeFieldList(s1)))(advance(state)))(current(state).span.start);
parseRecordTypeFieldList = (state) => (($match_84) => {
  if ($match_84.$tag === 1) {
    const e = $match_84.$0;
    return Err(e);
  }
  if ($match_84.$tag === 0) {
    const firstField = $match_84.$0[0];
    const s1 = $match_84.$0[1];
    return (($match_85) => {
      if ($match_85.$tag === 1) {
        const e = $match_85.$0;
        return Err(e);
      }
      if ($match_85.$tag === 0) {
        const rest = $match_85.$0[0];
        const s2 = $match_85.$0[1];
        return Ok([_COLON_COLON(firstField)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreRecordTypeFields(s1));
  }
  throw new Error("Pattern match failed");
})(parseRecordTypeField(state));
parseMoreRecordTypeFields = (state) => peekKind(Comma)(state) ? ((s1) => (($match_86) => {
  if ($match_86.$tag === 1) {
    const e = $match_86.$0;
    return Err(e);
  }
  if ($match_86.$tag === 0) {
    const field = $match_86.$0[0];
    const s2 = $match_86.$0[1];
    return (($match_87) => {
      if ($match_87.$tag === 1) {
        const e = $match_87.$0;
        return Err(e);
      }
      if ($match_87.$tag === 0) {
        const rest = $match_87.$0[0];
        const s3 = $match_87.$0[1];
        return Ok([_COLON_COLON(field)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreRecordTypeFields(s2));
  }
  throw new Error("Pattern match failed");
})(parseRecordTypeField(s1)))(advance(state)) : Ok([[], state]);
parseRecordTypeField = (state) => (($match_88) => {
  if ($match_88.$tag === 1) {
    const e = $match_88.$0;
    return Err(e);
  }
  if ($match_88.$tag === 0) {
    const nameTok = $match_88.$0[0];
    const s1 = $match_88.$0[1];
    return (($match_89) => {
      if ($match_89.$tag === 1) {
        const e = $match_89.$0;
        return Err(e);
      }
      if ($match_89.$tag === 0) {
        const s2 = $match_89.$0[1];
        return (($match_90) => {
          if ($match_90.$tag === 1) {
            const e = $match_90.$0;
            return Err(e);
          }
          if ($match_90.$tag === 0) {
            const fieldType = $match_90.$0[0];
            const s3 = $match_90.$0[1];
            return Ok([{ name: nameTok.lexeme, fieldType, span: { start: nameTok.span.start, end: typeSpan(fieldType).end } }, s3]);
          }
          throw new Error("Pattern match failed");
        })(parseTypeExpression(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Colon)("':' after field name")(s1));
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier)("record field name")(state));
parseParenOrTupleType = (state) => ((start) => ((s1) => peekKind(RParen)(s1) ? ((s2) => Ok([TRef("Unit")([])({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : (($match_91) => {
  if ($match_91.$tag === 1) {
    const e = $match_91.$0;
    return Err(e);
  }
  if ($match_91.$tag === 0) {
    const first = $match_91.$0[0];
    const s2 = $match_91.$0[1];
    return (($match_92) => {
      if ($match_92.$tag === 1) {
        const e = $match_92.$0;
        return Err(e);
      }
      if ($match_92.$tag === 0) {
        const moreTypes = $match_92.$0[0];
        const s3 = $match_92.$0[1];
        return (($match_93) => {
          if ($match_93.$tag === 1) {
            const e = $match_93.$0;
            return Err(e);
          }
          if ($match_93.$tag === 0) {
            const cp = $match_93.$0[0];
            const s4 = $match_93.$0[1];
            return isEmpty2(moreTypes) ? Ok([setTypeSpan(first)({ start, end: cp.span.end }), s4]) : Ok([TTuple(_COLON_COLON(first)(moreTypes))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("close type group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleTypes(s2));
  }
  throw new Error("Pattern match failed");
})(parseTypeExpression(s1)))(advance(state)))(current(state).span.start);
parseMoreTupleTypes = (state) => peekKind(Comma)(state) ? ((s1) => (($match_94) => {
  if ($match_94.$tag === 1) {
    const e = $match_94.$0;
    return Err(e);
  }
  if ($match_94.$tag === 0) {
    const t = $match_94.$0[0];
    const s2 = $match_94.$0[1];
    return (($match_95) => {
      if ($match_95.$tag === 1) {
        const e = $match_95.$0;
        return Err(e);
      }
      if ($match_95.$tag === 0) {
        const rest = $match_95.$0[0];
        const s3 = $match_95.$0[1];
        return Ok([_COLON_COLON(t)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleTypes(s2));
  }
  throw new Error("Pattern match failed");
})(parseTypeExpression(s1)))(advance(state)) : Ok([[], state]);
parseTypeRef = (state) => ((tok) => _PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier)) ? ((ident) => ((s1) => (($match_96) => {
  if ($match_96.$tag === 1) {
    const e = $match_96.$0;
    return Err(e);
  }
  if ($match_96.$tag === 0) {
    const nameResult = $match_96.$0[0];
    const s2 = $match_96.$0[1];
    return ((isTypeCtor) => isTypeCtor ? (($match_97) => {
      if ($match_97.$tag === 1) {
        const e = $match_97.$0;
        return Err(e);
      }
      if ($match_97.$tag === 0) {
        const args = $match_97.$0[0];
        const s3 = $match_97.$0[1];
        return ((endPos) => Ok([TRef(nameResult.name)(args)({ start: ident.span.start, end: endPos }), s3]))((($match_98) => {
          if ($match_98.$tag === 0) {
            const a = $match_98.$0;
            return typeSpan(a).end;
          }
          if ($match_98.$tag === 1) {
            return nameResult.nameEnd;
          }
          throw new Error("Pattern match failed");
        })(last(args)));
      }
      throw new Error("Pattern match failed");
    })(parseTypeApplicationArgs({ start: ident.span.start, end: nameResult.nameEnd })(s2)) : Ok([TRef(nameResult.name)([])({ start: ident.span.start, end: nameResult.nameEnd }), s2]))(_PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(ident.kind)(UpperIdentifier))(() => contains(".")(nameResult.name)));
  }
  throw new Error("Pattern match failed");
})(parseQualifiedName(ident.lexeme)(ident.span.end)(s1)))(advance(state)))(tok) : Err(makeError($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Expected type but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
parseTypeApplicationArgs = (lastSpan) => (state) => _AMP_AMP4(isTypeStart(current(state)))(() => onSameLine(lastSpan)(current(state))) ? (($match_99) => {
  if ($match_99.$tag === 1) {
    const e = $match_99.$0;
    return Err(e);
  }
  if ($match_99.$tag === 0) {
    const arg = $match_99.$0[0];
    const s1 = $match_99.$0[1];
    return (($match_100) => {
      if ($match_100.$tag === 1) {
        const e = $match_100.$0;
        return Err(e);
      }
      if ($match_100.$tag === 0) {
        const rest = $match_100.$0[0];
        const s2 = $match_100.$0[1];
        return Ok([_COLON_COLON(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeApplicationArgs(typeSpan(arg))(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok([[], state]);
parseTypeAtom = (state) => ((tok) => (($match_101) => {
  if ($match_101.$tag === 11) {
    return parseRecordType(state);
  }
  if ($match_101.$tag === 9) {
    return parseParenOrTupleType(state);
  }
  {
    return _PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier)) ? ((ident) => ((s1) => (($match_102) => {
      if ($match_102.$tag === 1) {
        const e = $match_102.$0;
        return Err(e);
      }
      if ($match_102.$tag === 0) {
        const nameResult = $match_102.$0[0];
        const s2 = $match_102.$0[1];
        return Ok([TRef(nameResult.name)([])({ start: ident.span.start, end: nameResult.nameEnd }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseQualifiedName(ident.lexeme)(ident.span.end)(s1)))(advance(state)))(tok) : Err(makeError($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Expected type but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var skipNewlines = (state) => {
  while (true) {
    if ($dict_Eq_TokenKind._EQ_EQ(current(state).kind)(Newline)) {
      state = advance(state);
      continue;
    } else {
      return state;
    }
  }
};
var parseDecoratedDeclaration = (state) => (($match_103) => {
  if ($match_103.$tag === 1) {
    const e = $match_103.$0;
    return Err(e);
  }
  if ($match_103.$tag === 0) {
    const atTok = $match_103.$0[0];
    const s1 = $match_103.$0[1];
    return ((decoratorTok) => _PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(decoratorTok.kind)(LowerIdentifier))(() => $dict_Eq_TokenKind._EQ_EQ(decoratorTok.kind)(Keyword)) ? ((decorator) => ((s2) => (($match_104) => {
      if ($match_104.$tag === 1) {
        const e = $match_104.$0;
        return Err(e);
      }
      if ($match_104.$tag === 0) {
        const args = $match_104.$0[0];
        const s3 = $match_104.$0[1];
        return ((s4) => (($match_105) => {
          if ($match_105.$tag === 1) {
            const e = $match_105.$0;
            return Err(e);
          }
          if ($match_105.$tag === 0) {
            const nameResult = $match_105.$0[0];
            const s5 = $match_105.$0[1];
            return (($match_106) => {
              if ($match_106.$tag === 1) {
                const e = $match_106.$0;
                return Err(e);
              }
              if ($match_106.$tag === 0) {
                const s6 = $match_106.$0[1];
                return (($match_107) => {
                  if ($match_107.$tag === 1) {
                    const e = $match_107.$0;
                    return Err(e);
                  }
                  if ($match_107.$tag === 0) {
                    const annotation = $match_107.$0[0];
                    const s7 = $match_107.$0[1];
                    return Ok([DDecorated(decorator)(args)(nameResult.declName)(annotation)({ start: atTok.span.start, end: typeSpan(annotation).end }), s7]);
                  }
                  throw new Error("Pattern match failed");
                })(parseTypeExpression(s6));
              }
              throw new Error("Pattern match failed");
            })(expect(Colon)("type annotation")(s5));
          }
          throw new Error("Pattern match failed");
        })(parseDeclarationName(s4)))(skipNewlines(s3));
      }
      throw new Error("Pattern match failed");
    })(parseDecoratorArgs(s2)))(advance(s1)))(decoratorTok.lexeme) : Err(makeError($dict_Appendable_String._PLUS_PLUS("Expected decorator name but found ")(tokenKindStr(decoratorTok.kind)))(decoratorTok.span)))(current(s1));
  }
  throw new Error("Pattern match failed");
})(expectOperator("@")(state));
var parseTypeParams = (state) => peekKind(LowerIdentifier)(state) ? ((tok) => ((s1) => (($match_108) => {
  if ($match_108.$tag === 1) {
    const e = $match_108.$0;
    return Err(e);
  }
  if ($match_108.$tag === 0) {
    const rest = $match_108.$0[0];
    const s2 = $match_108.$0[1];
    return Ok([_COLON_COLON(tok.lexeme)(rest), s2]);
  }
  throw new Error("Pattern match failed");
})(parseTypeParams(s1)))(advance(state)))(current(state)) : Ok([[], state]);
var parseTypeAliasDecl = (typeTok) => (state) => (($match_109) => {
  if ($match_109.$tag === 1) {
    const e = $match_109.$0;
    return Err(e);
  }
  if ($match_109.$tag === 0) {
    const s1 = $match_109.$0[1];
    return (($match_110) => {
      if ($match_110.$tag === 1) {
        const e = $match_110.$0;
        return Err(e);
      }
      if ($match_110.$tag === 0) {
        const nameTok = $match_110.$0[0];
        const s2 = $match_110.$0[1];
        return (($match_111) => {
          if ($match_111.$tag === 1) {
            const e = $match_111.$0;
            return Err(e);
          }
          if ($match_111.$tag === 0) {
            const params = $match_111.$0[0];
            const s3 = $match_111.$0[1];
            return (($match_112) => {
              if ($match_112.$tag === 1) {
                const e = $match_112.$0;
                return Err(e);
              }
              if ($match_112.$tag === 0) {
                const s4 = $match_112.$0[1];
                return (($match_113) => {
                  if ($match_113.$tag === 1) {
                    const e = $match_113.$0;
                    return Err(e);
                  }
                  if ($match_113.$tag === 0) {
                    const value = $match_113.$0[0];
                    const s5 = $match_113.$0[1];
                    return Ok([DTypeAlias(nameTok.lexeme)(params)(value)({ start: typeTok.span.start, end: typeSpan(value).end }), s5]);
                  }
                  throw new Error("Pattern match failed");
                })(parseTypeExpression(s4));
              }
              throw new Error("Pattern match failed");
            })(expect(Equals)("type alias definition")(s3));
          }
          throw new Error("Pattern match failed");
        })(parseTypeParams(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(UpperIdentifier)("type alias name")(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("alias")(state));
var parseOptionalConstraintsForTypeDecl = (state) => peekConstraintContextInTypeDecl(state) ? parseConstraints(state) : Ok([[], state]);
var parseTypeParamsWithLayout = (typeLine) => (typeCol) => (state) => peekKind(LowerIdentifier)(state) ? ((tok) => _AMP_AMP4($dict_Eq_Int._SLASH_EQ(tok.span.start.line)(typeLine))(() => $dict_Ord_Int._LT_EQ(tok.span.start.column)(typeCol)) ? Ok([[], state]) : ((s1) => (($match_114) => {
  if ($match_114.$tag === 1) {
    const e = $match_114.$0;
    return Err(e);
  }
  if ($match_114.$tag === 0) {
    const rest = $match_114.$0[0];
    const s2 = $match_114.$0[1];
    return Ok([_COLON_COLON(tok.lexeme)(rest), s2]);
  }
  throw new Error("Pattern match failed");
})(parseTypeParamsWithLayout(typeLine)(typeCol)(s1)))(advance(state)))(current(state)) : Ok([[], state]);
var parseRecordTypeFields = (state) => (($match_115) => {
  if ($match_115.$tag === 1) {
    const e = $match_115.$0;
    return Err(e);
  }
  if ($match_115.$tag === 0) {
    const s1 = $match_115.$0[1];
    return peekKind(RBrace)(s1) ? ((s2) => Ok([[], s2]))(advance(s1)) : (($match_116) => {
      if ($match_116.$tag === 1) {
        const e = $match_116.$0;
        return Err(e);
      }
      if ($match_116.$tag === 0) {
        const fields = $match_116.$0[0];
        const s2 = $match_116.$0[1];
        return (($match_117) => {
          if ($match_117.$tag === 1) {
            const e = $match_117.$0;
            return Err(e);
          }
          if ($match_117.$tag === 0) {
            const s3 = $match_117.$0[1];
            return Ok([fields, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RBrace)("close record type '}'")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseRecordTypeFieldList(s1));
  }
  throw new Error("Pattern match failed");
})(expect(LBrace)("record type start '{'")(state));
var parseConstructorArgs = (lastSpan) => (state) => _AMP_AMP4(isTypeStart(current(state)))(() => _AMP_AMP4(onSameLine(lastSpan)(current(state)))(() => not(peekKind(Pipe)(state)))) ? (($match_118) => {
  if ($match_118.$tag === 1) {
    const e = $match_118.$0;
    return Err(e);
  }
  if ($match_118.$tag === 0) {
    const arg = $match_118.$0[0];
    const s1 = $match_118.$0[1];
    return (($match_119) => {
      if ($match_119.$tag === 1) {
        const e = $match_119.$0;
        return Err(e);
      }
      if ($match_119.$tag === 0) {
        const rest = $match_119.$0[0];
        const s2 = $match_119.$0[1];
        return Ok([_COLON_COLON(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseConstructorArgs(typeSpan(arg))(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok([[], state]);
var parseConstructorVariant = (state) => (($match_120) => {
  if ($match_120.$tag === 1) {
    const e = $match_120.$0;
    return Err(e);
  }
  if ($match_120.$tag === 0) {
    const nameTok = $match_120.$0[0];
    const s1 = $match_120.$0[1];
    return (($match_121) => {
      if ($match_121.$tag === 1) {
        const e = $match_121.$0;
        return Err(e);
      }
      if ($match_121.$tag === 0) {
        const args = $match_121.$0[0];
        const s2 = $match_121.$0[1];
        return ((endPos) => Ok([{ name: nameTok.lexeme, args, span: { start: nameTok.span.start, end: endPos } }, s2]))((($match_122) => {
          if ($match_122.$tag === 0) {
            const a = $match_122.$0;
            return typeSpan(a).end;
          }
          if ($match_122.$tag === 1) {
            return nameTok.span.end;
          }
          throw new Error("Pattern match failed");
        })(last(args)));
      }
      throw new Error("Pattern match failed");
    })(parseConstructorArgs(nameTok.span)(s1));
  }
  throw new Error("Pattern match failed");
})(expect(UpperIdentifier)("constructor name")(state));
var parseMoreConstructors = (equalsCol) => (equalsLine) => (state) => peekKind(Pipe)(state) ? ((pipeTok) => _AMP_AMP4($dict_Eq_Int._SLASH_EQ(pipeTok.span.start.line)(equalsLine))(() => $dict_Eq_Int._SLASH_EQ(pipeTok.span.start.column)(equalsCol)) ? Err(makeError("Constructor variant '|' must align with '='")(pipeTok.span)) : ((s1) => (($match_123) => {
  if ($match_123.$tag === 1) {
    const e = $match_123.$0;
    return Err(e);
  }
  if ($match_123.$tag === 0) {
    const ctor = $match_123.$0[0];
    const s2 = $match_123.$0[1];
    return (($match_124) => {
      if ($match_124.$tag === 1) {
        const e = $match_124.$0;
        return Err(e);
      }
      if ($match_124.$tag === 0) {
        const rest = $match_124.$0[0];
        const s3 = $match_124.$0[1];
        return Ok([_COLON_COLON(ctor)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreConstructors(equalsCol)(equalsLine)(s2));
  }
  throw new Error("Pattern match failed");
})(parseConstructorVariant(s1)))(advance(state)))(current(state)) : Ok([[], state]);
var parseConstructorVariants = (equalsCol) => (equalsLine) => (state) => (($match_125) => {
  if ($match_125.$tag === 1) {
    const e = $match_125.$0;
    return Err(e);
  }
  if ($match_125.$tag === 0) {
    const first = $match_125.$0[0];
    const s1 = $match_125.$0[1];
    return (($match_126) => {
      if ($match_126.$tag === 1) {
        const e = $match_126.$0;
        return Err(e);
      }
      if ($match_126.$tag === 0) {
        const rest = $match_126.$0[0];
        const s2 = $match_126.$0[1];
        return Ok([_COLON_COLON(first)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreConstructors(equalsCol)(equalsLine)(s1));
  }
  throw new Error("Pattern match failed");
})(parseConstructorVariant(state));
var parseTypeOrOpaqueDecl = (typeTok) => (state) => ((typeColumn) => ((typeLine) => (($match_127) => {
  if ($match_127.$tag === 1) {
    const e = $match_127.$0;
    return Err(e);
  }
  if ($match_127.$tag === 0) {
    const constraints = $match_127.$0[0];
    const s1 = $match_127.$0[1];
    return (($match_128) => {
      if ($match_128.$tag === 1) {
        const e = $match_128.$0;
        return Err(e);
      }
      if ($match_128.$tag === 0) {
        const nameTok = $match_128.$0[0];
        const s2 = $match_128.$0[1];
        return (($match_129) => {
          if ($match_129.$tag === 1) {
            const e = $match_129.$0;
            return Err(e);
          }
          if ($match_129.$tag === 0) {
            const params = $match_129.$0[0];
            const s3 = $match_129.$0[1];
            return peekKind(Equals)(s3) ? ((eqTok) => ((s4) => ((equalsCol) => ((equalsLine) => peekKind(LBrace)(s4) ? (($match_130) => {
              if ($match_130.$tag === 1) {
                const e = $match_130.$0;
                return Err(e);
              }
              if ($match_130.$tag === 0) {
                const fields = $match_130.$0[0];
                const s5 = $match_130.$0[1];
                return Ok([DRecordType(nameTok.lexeme)(params)(constraints)(fields)({ start: typeTok.span.start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(parseRecordTypeFields(s4)) : (($match_131) => {
              if ($match_131.$tag === 1) {
                const e = $match_131.$0;
                return Err(e);
              }
              if ($match_131.$tag === 0) {
                const ctors = $match_131.$0[0];
                const s5 = $match_131.$0[1];
                return ((endPos) => Ok([DType(nameTok.lexeme)(params)(constraints)(ctors)({ start: typeTok.span.start, end: endPos }), s5]))((($match_132) => {
                  if ($match_132.$tag === 0) {
                    const c = $match_132.$0;
                    return c.span.end;
                  }
                  if ($match_132.$tag === 1) {
                    return eqTok.span.end;
                  }
                  throw new Error("Pattern match failed");
                })(last(ctors)));
              }
              throw new Error("Pattern match failed");
            })(parseConstructorVariants(equalsCol)(equalsLine)(s4)))(eqTok.span.start.line))(eqTok.span.start.column))(advance(s3)))(current(s3)) : isEmpty2(constraints) ? ((endPos) => Ok([DOpaqueType(nameTok.lexeme)(params)({ start: typeTok.span.start, end: endPos }), s3]))(isEmpty2(params) ? nameTok.span.end : previous(s3).span.end) : Err(makeError("Opaque types cannot have constraints")({ start: typeTok.span.start, end: previous(s3).span.end }));
          }
          throw new Error("Pattern match failed");
        })(parseTypeParamsWithLayout(typeLine)(typeColumn)(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(UpperIdentifier)("type name")(s1));
  }
  throw new Error("Pattern match failed");
})(parseOptionalConstraintsForTypeDecl(state)))(typeTok.span.start.line))(typeTok.span.start.column);
var parseTypeOrAliasDeclaration = (state) => (($match_133) => {
  if ($match_133.$tag === 1) {
    const e = $match_133.$0;
    return Err(e);
  }
  if ($match_133.$tag === 0) {
    const typeTok = $match_133.$0[0];
    const s1 = $match_133.$0[1];
    return peekKeyword("alias")(s1) ? parseTypeAliasDecl(typeTok)(s1) : parseTypeOrOpaqueDecl(typeTok)(s1);
  }
  throw new Error("Pattern match failed");
})(expectKeyword("type")(state));
var parseOptionalConstraints = (hasConstraints) => (state) => hasConstraints ? parseConstraints(state) : Ok([[], state]);
var parseProtocolMethodName = (state) => ((tok) => (($match_134) => {
  if ($match_134.$tag === 9) {
    return ((s1) => (($match_135) => {
      if ($match_135.$tag === 1) {
        const e = $match_135.$0;
        return Err(e);
      }
      if ($match_135.$tag === 0) {
        const opTok = $match_135.$0[0];
        const s2 = $match_135.$0[1];
        return (($match_136) => {
          if ($match_136.$tag === 1) {
            const e = $match_136.$0;
            return Err(e);
          }
          if ($match_136.$tag === 0) {
            const s3 = $match_136.$0[1];
            return Ok([opTok.lexeme, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("close paren after operator")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator)("operator in protocol method")(s1)))(advance(state));
  }
  if ($match_134.$tag === 0) {
    return ((s1) => Ok([tok.lexeme, s1]))(advance(state));
  }
  {
    return Err(makeError($dict_Appendable_String._PLUS_PLUS("Expected method name but found ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var setPatSpan = (pat) => (span) => (($match_137) => {
  if ($match_137.$tag === 0) {
    const n = $match_137.$0;
    return PVar(n)(span);
  }
  if ($match_137.$tag === 1) {
    return PWildcard(span);
  }
  if ($match_137.$tag === 2) {
    const n = $match_137.$0;
    const a = $match_137.$1;
    return PConstructor(n)(a)(span);
  }
  if ($match_137.$tag === 3) {
    const e = $match_137.$0;
    return PTuple(e)(span);
  }
  if ($match_137.$tag === 4) {
    const e = $match_137.$0;
    return PList(e)(span);
  }
  if ($match_137.$tag === 5) {
    const h = $match_137.$0;
    const t = $match_137.$1;
    return PCons(h)(t)(span);
  }
  if ($match_137.$tag === 6) {
    const f = $match_137.$0;
    return PRecord(f)(span);
  }
  if ($match_137.$tag === 7) {
    const v = $match_137.$0;
    return PInt(v)(span);
  }
  if ($match_137.$tag === 8) {
    const v = $match_137.$0;
    return PFloat(v)(span);
  }
  if ($match_137.$tag === 9) {
    const v = $match_137.$0;
    return PString(v)(span);
  }
  if ($match_137.$tag === 10) {
    const v = $match_137.$0;
    return PChar(v)(span);
  }
  throw new Error("Pattern match failed");
})(pat);
var parseFunctionParamPattern;
var parseFPCtorPattern;
var parseFPCtorArgs;
var parseFPTuplePattern;
var parseFPTupleRest;
var parseFPRecordPattern;
var parseRecordFieldPatterns;
var parseRecordFieldPattern;
parseFunctionParamPattern = (state) => ((tok) => (($match_138) => {
  if ($match_138.$tag === 0) {
    return ((s1) => $dict_Eq_String._EQ_EQ(tok.lexeme)("_") ? Ok([PWildcard(tok.span), s1]) : Ok([PVar(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_138.$tag === 1) {
    return parseFPCtorPattern(state);
  }
  if ($match_138.$tag === 9) {
    return parseFPTuplePattern(state);
  }
  if ($match_138.$tag === 11) {
    return parseFPRecordPattern(state);
  }
  if ($match_138.$tag === 13) {
    return Err(makeError("List patterns are not allowed in function parameters")(tok.span));
  }
  {
    return Err(makeError($dict_Appendable_String._PLUS_PLUS("Expected function parameter pattern but found ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseFPCtorPattern = (state) => ((ctor) => ((s1) => (($match_139) => {
  if ($match_139.$tag === 1) {
    const e = $match_139.$0;
    return Err(e);
  }
  if ($match_139.$tag === 0) {
    const args = $match_139.$0[0];
    const s2 = $match_139.$0[1];
    return ((endPos) => Ok([PConstructor(ctor.lexeme)(args)({ start: ctor.span.start, end: endPos }), s2]))((($match_140) => {
      if ($match_140.$tag === 0) {
        const a = $match_140.$0;
        return patSpan(a).end;
      }
      if ($match_140.$tag === 1) {
        return ctor.span.end;
      }
      throw new Error("Pattern match failed");
    })(last(args)));
  }
  throw new Error("Pattern match failed");
})(parseFPCtorArgs(s1)))(advance(state)))(current(state));
parseFPCtorArgs = (state) => isFunctionParamPatternStart(current(state)) ? (($match_141) => {
  if ($match_141.$tag === 1) {
    const e = $match_141.$0;
    return Err(e);
  }
  if ($match_141.$tag === 0) {
    const pat = $match_141.$0[0];
    const s1 = $match_141.$0[1];
    return (($match_142) => {
      if ($match_142.$tag === 1) {
        const e = $match_142.$0;
        return Err(e);
      }
      if ($match_142.$tag === 0) {
        const rest = $match_142.$0[0];
        const s2 = $match_142.$0[1];
        return Ok([_COLON_COLON(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseFPCtorArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(state)) : Ok([[], state]);
parseFPTuplePattern = (state) => ((start) => ((s1) => peekKind(RParen)(s1) ? ((s2) => Ok([PTuple([])({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : (($match_143) => {
  if ($match_143.$tag === 1) {
    const e = $match_143.$0;
    return Err(e);
  }
  if ($match_143.$tag === 0) {
    const first = $match_143.$0[0];
    const s2 = $match_143.$0[1];
    return (($match_144) => {
      if ($match_144.$tag === 1) {
        const e = $match_144.$0;
        return Err(e);
      }
      if ($match_144.$tag === 0) {
        const more = $match_144.$0[0];
        const s3 = $match_144.$0[1];
        return (($match_145) => {
          if ($match_145.$tag === 1) {
            const e = $match_145.$0;
            return Err(e);
          }
          if ($match_145.$tag === 0) {
            const cp = $match_145.$0[0];
            const s4 = $match_145.$0[1];
            return isEmpty2(more) ? Ok([setPatSpan(first)({ start, end: cp.span.end }), s4]) : Ok([PTuple(_COLON_COLON(first)(more))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("close pattern group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseFPTupleRest(s2));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(s1)))(advance(state)))(current(state).span.start);
parseFPTupleRest = (state) => peekKind(Comma)(state) ? ((s1) => (($match_146) => {
  if ($match_146.$tag === 1) {
    const e = $match_146.$0;
    return Err(e);
  }
  if ($match_146.$tag === 0) {
    const pat = $match_146.$0[0];
    const s2 = $match_146.$0[1];
    return (($match_147) => {
      if ($match_147.$tag === 1) {
        const e = $match_147.$0;
        return Err(e);
      }
      if ($match_147.$tag === 0) {
        const rest = $match_147.$0[0];
        const s3 = $match_147.$0[1];
        return Ok([_COLON_COLON(pat)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseFPTupleRest(s2));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(s1)))(advance(state)) : Ok([[], state]);
parseFPRecordPattern = (state) => ((start) => ((s1) => peekKind(RBrace)(s1) ? ((s2) => Ok([PRecord([])({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : (($match_148) => {
  if ($match_148.$tag === 1) {
    const e = $match_148.$0;
    return Err(e);
  }
  if ($match_148.$tag === 0) {
    const fields = $match_148.$0[0];
    const s2 = $match_148.$0[1];
    return (($match_149) => {
      if ($match_149.$tag === 1) {
        const e = $match_149.$0;
        return Err(e);
      }
      if ($match_149.$tag === 0) {
        const s3 = $match_149.$0[1];
        return Ok([PRecord(fields)({ start, end: previous(s3).span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RBrace)("close record pattern")(s2));
  }
  throw new Error("Pattern match failed");
})(parseRecordFieldPatterns(s1)))(advance(state)))(current(state).span.start);
parseRecordFieldPatterns = (state) => (($match_150) => {
  if ($match_150.$tag === 1) {
    const e = $match_150.$0;
    return Err(e);
  }
  if ($match_150.$tag === 0) {
    const field = $match_150.$0[0];
    const s1 = $match_150.$0[1];
    return peekKind(Comma)(s1) ? ((s2) => (($match_151) => {
      if ($match_151.$tag === 1) {
        const e = $match_151.$0;
        return Err(e);
      }
      if ($match_151.$tag === 0) {
        const rest = $match_151.$0[0];
        const s3 = $match_151.$0[1];
        return Ok([_COLON_COLON(field)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseRecordFieldPatterns(s2)))(advance(s1)) : Ok([[field], s1]);
  }
  throw new Error("Pattern match failed");
})(parseRecordFieldPattern(state));
parseRecordFieldPattern = (state) => (($match_152) => {
  if ($match_152.$tag === 1) {
    const e = $match_152.$0;
    return Err(e);
  }
  if ($match_152.$tag === 0) {
    const nameTok = $match_152.$0[0];
    const s1 = $match_152.$0[1];
    return peekKind(Equals)(s1) ? ((s2) => (($match_153) => {
      if ($match_153.$tag === 1) {
        const e = $match_153.$0;
        return Err(e);
      }
      if ($match_153.$tag === 0) {
        const pat = $match_153.$0[0];
        const s3 = $match_153.$0[1];
        return Ok([{ name: nameTok.lexeme, pattern: pat }, s3]);
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParamPattern(s2)))(advance(s1)) : Ok([{ name: nameTok.lexeme, pattern: PVar(nameTok.lexeme)(nameTok.span) }, s1]);
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier)("record field name")(state));
var parseFunctionParams = (state) => isFunctionParamPatternStart(current(state)) ? (($match_154) => {
  if ($match_154.$tag === 1) {
    const e = $match_154.$0;
    return Err(e);
  }
  if ($match_154.$tag === 0) {
    const pat = $match_154.$0[0];
    const s1 = $match_154.$0[1];
    return (($match_155) => {
      if ($match_155.$tag === 1) {
        const e = $match_155.$0;
        return Err(e);
      }
      if ($match_155.$tag === 0) {
        const rest = $match_155.$0[0];
        const s2 = $match_155.$0[1];
        return Ok([_COLON_COLON(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParams(s1));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(state)) : Ok([[], state]);
var declToValueDecl = (decl) => (($match_156) => {
  if ($match_156.$tag === 0) {
    const vd = $match_156.$0;
    return vd;
  }
  if ($match_156.$tag === 1) {
    const ta = $match_156.$0;
    return { name: ta.name, args: [], body: EUnit(ta.span), span: ta.span };
  }
  {
    return { name: "", args: [], body: EUnit(eofToken.span), span: eofToken.span };
  }
  throw new Error("Pattern match failed");
})(decl);
var parseQualifiedCtorName = (name) => (end) => (state) => {
  while (true) {
    if (peekKind(Dot)(state)) {
      {
        const $match_157 = peekAhead(1)(state);
        if ($match_157.$tag === 1) {
          return Ok([{ name, nameEnd: end }, state]);
        }
        if ($match_157.$tag === 0) {
          const next = $match_157.$0;
          if ($dict_Eq_TokenKind._EQ_EQ(next.kind)(UpperIdentifier)) {
            {
              const s1 = advance(state);
              {
                const $match_158 = expect(UpperIdentifier)("constructor name part")(s1);
                if ($match_158.$tag === 1) {
                  const e = $match_158.$0;
                  return Err(e);
                }
                if ($match_158.$tag === 0) {
                  const nextTok = $match_158.$0[0];
                  const s2 = $match_158.$0[1];
                  [name, end, state] = [$dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(name)("."))(nextTok.lexeme), nextTok.span.end, s2];
                  continue;
                }
                throw new Error("Pattern match failed");
              }
            }
          } else {
            return Ok([{ name, nameEnd: end }, state]);
          }
        }
        throw new Error("Pattern match failed");
      }
    } else {
      return Ok([{ name, nameEnd: end }, state]);
    }
  }
};
var parsePattern;
var parsePrimaryPattern;
var parseConstructorPattern;
var parseConstructorPatternArgs;
var parseAtomicPattern;
var parseListPattern;
var parsePatternList;
var parseParenOrTuplePattern;
var parseTuplePatternRest;
parsePattern = (state) => (($match_159) => {
  if ($match_159.$tag === 1) {
    const e = $match_159.$0;
    return Err(e);
  }
  if ($match_159.$tag === 0) {
    const primary = $match_159.$0[0];
    const s1 = $match_159.$0[1];
    return peekOperator("::")(s1) ? ((s2) => (($match_160) => {
      if ($match_160.$tag === 1) {
        const e = $match_160.$0;
        return Err(e);
      }
      if ($match_160.$tag === 0) {
        const tail = $match_160.$0[0];
        const s3 = $match_160.$0[1];
        return Ok([PCons(primary)(tail)({ start: patSpan(primary).start, end: patSpan(tail).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parsePattern(s2)))(advance(s1)) : Ok([primary, s1]);
  }
  throw new Error("Pattern match failed");
})(parsePrimaryPattern(state));
parsePrimaryPattern = (state) => ((tok) => (($match_161) => {
  if ($match_161.$tag === 0) {
    return ((s1) => $dict_Eq_String._EQ_EQ(tok.lexeme)("_") ? Ok([PWildcard(tok.span), s1]) : Ok([PVar(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_161.$tag === 3) {
    return ((s1) => contains(".")(tok.lexeme) ? Ok([PFloat(tok.lexeme)(tok.span), s1]) : Ok([PInt(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_161.$tag === 4) {
    return ((s1) => Ok([PString(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_161.$tag === 5) {
    return ((s1) => Ok([PChar(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_161.$tag === 1) {
    return parseConstructorPattern(state);
  }
  if ($match_161.$tag === 13) {
    return parseListPattern(state);
  }
  if ($match_161.$tag === 9) {
    return parseParenOrTuplePattern(state);
  }
  {
    return Err(makeError($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Expected pattern but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseConstructorPattern = (state) => ((ctor) => ((s1) => (($match_162) => {
  if ($match_162.$tag === 1) {
    const e = $match_162.$0;
    return Err(e);
  }
  if ($match_162.$tag === 0) {
    const nameResult = $match_162.$0[0];
    const s2 = $match_162.$0[1];
    return (($match_163) => {
      if ($match_163.$tag === 1) {
        const e = $match_163.$0;
        return Err(e);
      }
      if ($match_163.$tag === 0) {
        const args = $match_163.$0[0];
        const s3 = $match_163.$0[1];
        return ((endPos) => Ok([PConstructor(nameResult.name)(args)({ start: ctor.span.start, end: endPos }), s3]))((($match_164) => {
          if ($match_164.$tag === 0) {
            const a = $match_164.$0;
            return patSpan(a).end;
          }
          if ($match_164.$tag === 1) {
            return nameResult.nameEnd;
          }
          throw new Error("Pattern match failed");
        })(last(args)));
      }
      throw new Error("Pattern match failed");
    })(parseConstructorPatternArgs(s2));
  }
  throw new Error("Pattern match failed");
})(parseQualifiedCtorName(ctor.lexeme)(ctor.span.end)(s1)))(advance(state)))(current(state));
parseConstructorPatternArgs = (state) => isPatternStart(current(state)) ? (($match_165) => {
  if ($match_165.$tag === 1) {
    const e = $match_165.$0;
    return Err(e);
  }
  if ($match_165.$tag === 0) {
    const pat = $match_165.$0[0];
    const s1 = $match_165.$0[1];
    return (($match_166) => {
      if ($match_166.$tag === 1) {
        const e = $match_166.$0;
        return Err(e);
      }
      if ($match_166.$tag === 0) {
        const rest = $match_166.$0[0];
        const s2 = $match_166.$0[1];
        return Ok([_COLON_COLON(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseConstructorPatternArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseAtomicPattern(state)) : Ok([[], state]);
parseAtomicPattern = (state) => ((tok) => (($match_167) => {
  if ($match_167.$tag === 1) {
    return ((s1) => (($match_168) => {
      if ($match_168.$tag === 1) {
        const e = $match_168.$0;
        return Err(e);
      }
      if ($match_168.$tag === 0) {
        const nameResult = $match_168.$0[0];
        const s2 = $match_168.$0[1];
        return Ok([PConstructor(nameResult.name)([])({ start: tok.span.start, end: nameResult.nameEnd }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseQualifiedCtorName(tok.lexeme)(tok.span.end)(s1)))(advance(state));
  }
  {
    return parsePrimaryPattern(state);
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseListPattern = (state) => ((start) => ((s1) => peekKind(RBracket)(s1) ? ((s2) => Ok([PList([])({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : (($match_169) => {
  if ($match_169.$tag === 1) {
    const e = $match_169.$0;
    return Err(e);
  }
  if ($match_169.$tag === 0) {
    const elements = $match_169.$0[0];
    const s2 = $match_169.$0[1];
    return (($match_170) => {
      if ($match_170.$tag === 1) {
        const e = $match_170.$0;
        return Err(e);
      }
      if ($match_170.$tag === 0) {
        const s3 = $match_170.$0[1];
        return Ok([PList(elements)({ start, end: previous(s3).span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RBracket)("close list pattern")(s2));
  }
  throw new Error("Pattern match failed");
})(parsePatternList(s1)))(advance(state)))(current(state).span.start);
parsePatternList = (state) => (($match_171) => {
  if ($match_171.$tag === 1) {
    const e = $match_171.$0;
    return Err(e);
  }
  if ($match_171.$tag === 0) {
    const pat = $match_171.$0[0];
    const s1 = $match_171.$0[1];
    return peekKind(Comma)(s1) ? ((s2) => (($match_172) => {
      if ($match_172.$tag === 1) {
        const e = $match_172.$0;
        return Err(e);
      }
      if ($match_172.$tag === 0) {
        const rest = $match_172.$0[0];
        const s3 = $match_172.$0[1];
        return Ok([_COLON_COLON(pat)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parsePatternList(s2)))(advance(s1)) : Ok([[pat], s1]);
  }
  throw new Error("Pattern match failed");
})(parsePattern(state));
parseParenOrTuplePattern = (state) => ((start) => ((s1) => peekKind(RParen)(s1) ? ((s2) => Ok([PTuple([])({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : (($match_173) => {
  if ($match_173.$tag === 1) {
    const e = $match_173.$0;
    return Err(e);
  }
  if ($match_173.$tag === 0) {
    const first = $match_173.$0[0];
    const s2 = $match_173.$0[1];
    return (($match_174) => {
      if ($match_174.$tag === 1) {
        const e = $match_174.$0;
        return Err(e);
      }
      if ($match_174.$tag === 0) {
        const more = $match_174.$0[0];
        const s3 = $match_174.$0[1];
        return (($match_175) => {
          if ($match_175.$tag === 1) {
            const e = $match_175.$0;
            return Err(e);
          }
          if ($match_175.$tag === 0) {
            const cp = $match_175.$0[0];
            const s4 = $match_175.$0[1];
            return isEmpty2(more) ? Ok([setPatSpan(first)({ start, end: cp.span.end }), s4]) : Ok([PTuple(_COLON_COLON(first)(more))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("close pattern group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseTuplePatternRest(s2));
  }
  throw new Error("Pattern match failed");
})(parsePattern(s1)))(advance(state)))(current(state).span.start);
parseTuplePatternRest = (state) => peekKind(Comma)(state) ? ((s1) => (($match_176) => {
  if ($match_176.$tag === 1) {
    const e = $match_176.$0;
    return Err(e);
  }
  if ($match_176.$tag === 0) {
    const pat = $match_176.$0[0];
    const s2 = $match_176.$0[1];
    return (($match_177) => {
      if ($match_177.$tag === 1) {
        const e = $match_177.$0;
        return Err(e);
      }
      if ($match_177.$tag === 0) {
        const rest = $match_177.$0[0];
        const s3 = $match_177.$0[1];
        return Ok([_COLON_COLON(pat)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseTuplePatternRest(s2));
  }
  throw new Error("Pattern match failed");
})(parsePattern(s1)))(advance(state)) : Ok([[], state]);
var parseLambdaArgs = (state) => (($match_178) => {
  if ($match_178.$tag === 0) {
    const s1 = $match_178.$0;
    return Ok([[], s1]);
  }
  if ($match_178.$tag === 1) {
    return isPatternStart(current(state)) ? (($match_179) => {
      if ($match_179.$tag === 1) {
        const e = $match_179.$0;
        return Err(e);
      }
      if ($match_179.$tag === 0) {
        const pat = $match_179.$0[0];
        const s2 = $match_179.$0[1];
        return (($match_180) => {
          if ($match_180.$tag === 1) {
            const e = $match_180.$0;
            return Err(e);
          }
          if ($match_180.$tag === 0) {
            const rest = $match_180.$0[0];
            const s3 = $match_180.$0[1];
            return Ok([_COLON_COLON(pat)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseLambdaArgs(s2));
      }
      throw new Error("Pattern match failed");
    })(parsePattern(state)) : Err(makeError("Expected lambda argument")(current(state).span));
  }
  throw new Error("Pattern match failed");
})(matchOperator("->")(state));
var parseFieldAccesses = (expr) => (state) => {
  while (true) {
    {
      const $match_181 = matchKind(Dot)(state);
      if ($match_181.$tag === 1) {
        return Ok([expr, state]);
      }
      if ($match_181.$tag === 0) {
        const s1 = $match_181.$0;
        {
          const tok = current(s1);
          if (_PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier))) {
            {
              const field = tok;
              {
                const s2 = advance(s1);
                [expr, state] = [EFieldAccess(expr)(field.lexeme)({ start: exprSpan(expr).start, end: field.span.end }), s2];
                continue;
              }
            }
          } else {
            return Err(makeError($dict_Appendable_String._PLUS_PLUS("Expected field name but found ")(tokenKindStr(tok.kind)))(tok.span));
          }
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var noDefaultMethod = (name) => (typeExpr) => (span) => ({ name, methodType: typeExpr, defaultArgs: [], defaultBody: EUnit(span), hasDefault: false, hasType: true, span });
var mergeProtocolItems = (items) => (($match_182) => {
  if (Array.isArray($match_182) && $match_182.length === 0) {
    return [];
  }
  if (Array.isArray($match_182) && $match_182.length >= 1 && $match_182[0].$tag === 0) {
    const name = $match_182[0].$0;
    const typeExpr = $match_182[0].$1;
    const span = $match_182[0].$2;
    const rest = $match_182.slice(1);
    return (($match_183) => {
      if (Array.isArray($match_183) && $match_183.length >= 1 && $match_183[0].$tag === 1) {
        const implName = $match_183[0].$0;
        const args = $match_183[0].$1;
        const body = $match_183[0].$2;
        const implSpan = $match_183[0].$3;
        const rest2 = $match_183.slice(1);
        return $dict_Eq_String._EQ_EQ(name)(implName) ? _COLON_COLON({ name, methodType: typeExpr, defaultArgs: args, defaultBody: body, hasDefault: true, hasType: true, span: { start: span.start, end: implSpan.end } })(mergeProtocolItems(rest2)) : _COLON_COLON(noDefaultMethod(name)(typeExpr)(span))(mergeProtocolItems(rest));
      }
      {
        return _COLON_COLON(noDefaultMethod(name)(typeExpr)(span))(mergeProtocolItems(rest));
      }
      throw new Error("Pattern match failed");
    })(rest);
  }
  if (Array.isArray($match_182) && $match_182.length >= 1 && $match_182[0].$tag === 1) {
    const name = $match_182[0].$0;
    const args = $match_182[0].$1;
    const body = $match_182[0].$2;
    const span = $match_182[0].$3;
    const rest = $match_182.slice(1);
    return ((dummyType) => _COLON_COLON({ name, methodType: dummyType, defaultArgs: args, defaultBody: body, hasDefault: true, hasType: false, span })(mergeProtocolItems(rest)))(TRef("Unknown")([])(span));
  }
  throw new Error("Pattern match failed");
})(items);
var parseImplementationTypeArgs = (state) => isTypeStart(current(state)) ? (($match_184) => {
  if ($match_184.$tag === 1) {
    const e = $match_184.$0;
    return Err(e);
  }
  if ($match_184.$tag === 0) {
    const t = $match_184.$0[0];
    const s1 = $match_184.$0[1];
    return (($match_185) => {
      if ($match_185.$tag === 1) {
        const e = $match_185.$0;
        return Err(e);
      }
      if ($match_185.$tag === 0) {
        const rest = $match_185.$0[0];
        const s2 = $match_185.$0[1];
        return Ok([_COLON_COLON(t)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseImplementationTypeArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok([[], state]);
var parseInfixOperator = (state) => peekKind(LParen)(state) ? ((s1) => (($match_186) => {
  if ($match_186.$tag === 1) {
    const e = $match_186.$0;
    return Err(e);
  }
  if ($match_186.$tag === 0) {
    const opTok = $match_186.$0[0];
    const s2 = $match_186.$0[1];
    return (($match_187) => {
      if ($match_187.$tag === 1) {
        const e = $match_187.$0;
        return Err(e);
      }
      if ($match_187.$tag === 0) {
        const cp = $match_187.$0[0];
        const s3 = $match_187.$0[1];
        return Ok([{ opName: opTok.lexeme, opEnd: cp.span.end }, s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RParen)("close paren")(s2));
  }
  throw new Error("Pattern match failed");
})(expect(Operator)("operator")(s1)))(advance(state)) : (($match_188) => {
  if ($match_188.$tag === 1) {
    const e = $match_188.$0;
    return Err(e);
  }
  if ($match_188.$tag === 0) {
    const opTok = $match_188.$0[0];
    const s1 = $match_188.$0[1];
    return Ok([{ opName: opTok.lexeme, opEnd: opTok.span.end }, s1]);
  }
  throw new Error("Pattern match failed");
})(expect(Operator)("operator")(state));
var parseInfixDeclaration = (state) => ((fixityTok) => ((start) => ((s1) => (($match_189) => {
  if ($match_189.$tag === 1) {
    const e = $match_189.$0;
    return Err(e);
  }
  if ($match_189.$tag === 0) {
    const precTok = $match_189.$0[0];
    const s2 = $match_189.$0[1];
    return (($match_190) => {
      if ($match_190.$tag === 1) {
        const e = $match_190.$0;
        return Err(e);
      }
      if ($match_190.$tag === 0) {
        const opResult = $match_190.$0[0];
        const s3 = $match_190.$0[1];
        return ((assocStr) => Ok([DInfix(assocStr)(stringToInt(precTok.lexeme))(opResult.opName)({ start, end: opResult.opEnd }), s3]))((($match_191) => {
          if ($match_191 === "infixl") {
            return "left";
          }
          if ($match_191 === "infixr") {
            return "right";
          }
          {
            return "none";
          }
          throw new Error("Pattern match failed");
        })(fixityTok.lexeme));
      }
      throw new Error("Pattern match failed");
    })(parseInfixOperator(s2));
  }
  throw new Error("Pattern match failed");
})(expect(NumberToken)("precedence number")(s1)))(advance(state)))(fixityTok.span.start))(current(state));
var parseDeclaration;
var parseAnnotationOrValue;
var parseMethodBody;
var parseProtocolDeclaration;
var parseProtocolItems;
var parseProtocolItem;
var parseImplementationDeclaration;
var parseImplementationMethods;
var parseImplMethodList;
var parseImplMethod;
var parseExpression;
var parseBinaryExpression;
var parseBinExprLoop;
var parseUnary;
var parseApplication;
var parseApplicationArgs;
var parsePrimaryWithAccess;
var parsePrimary;
var parseLambda;
var parseIf;
var parseLetIn;
var parseLetBindings;
var parseLetBinding;
var parseCase;
var parseCaseBranches;
var parseCaseBranch;
var parseParenExpr;
var parseParenExprContents;
var parseMoreTupleExprs;
var parseListExpr;
var parseMoreListExprs;
var parseRecordExpr;
var parseRecordFieldList;
var parseRecordFieldValue;
var parseMoreRecordFields;
parseDeclaration = (state) => peekDecorator(state) ? parseDecoratedDeclaration(state) : peekKeyword("type")(state) ? parseTypeOrAliasDeclaration(state) : peekKeyword("protocol")(state) ? parseProtocolDeclaration(state) : peekKeyword("implement")(state) ? parseImplementationDeclaration(state) : _PIPE_PIPE4(peekKeyword("infix")(state))(() => _PIPE_PIPE4(peekKeyword("infixl")(state))(() => peekKeyword("infixr")(state))) ? parseInfixDeclaration(state) : parseAnnotationOrValue(state);
parseAnnotationOrValue = (state) => (($match_192) => {
  if ($match_192.$tag === 1) {
    const e = $match_192.$0;
    return Err(e);
  }
  if ($match_192.$tag === 0) {
    const nameResult = $match_192.$0[0];
    const s1 = $match_192.$0[1];
    return peekKind(Colon)(s1) ? ((s2) => (($match_193) => {
      if ($match_193.$tag === 1) {
        const e = $match_193.$0;
        return Err(e);
      }
      if ($match_193.$tag === 0) {
        const annotation = $match_193.$0[0];
        const s3 = $match_193.$0[1];
        return Ok([DTypeAnnotation({ name: nameResult.declName, annotation, span: { start: nameResult.declSpan.start, end: typeSpan(annotation).end } }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeExpression(s2)))(advance(s1)) : (($match_194) => {
      if ($match_194.$tag === 1) {
        const e = $match_194.$0;
        return Err(e);
      }
      if ($match_194.$tag === 0) {
        const result = $match_194.$0[0];
        const s2 = $match_194.$0[1];
        return Ok([DValue({ name: nameResult.declName, args: result.mArgs, body: result.mBody, span: { start: nameResult.declSpan.start, end: exprSpan(result.mBody).end } }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMethodBody(Just(nameResult.declSpan.start.column))(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclarationName(state));
parseMethodBody = (declColumn) => (state) => (($match_195) => {
  if ($match_195.$tag === 1) {
    const e = $match_195.$0;
    return Err(e);
  }
  if ($match_195.$tag === 0) {
    const args = $match_195.$0[0];
    const s1 = $match_195.$0[1];
    return (($match_196) => {
      if ($match_196.$tag === 1) {
        const e = $match_196.$0;
        return Err(e);
      }
      if ($match_196.$tag === 0) {
        const eqTok = $match_196.$0[0];
        const s2 = $match_196.$0[1];
        return (($match_197) => {
          if ($match_197.$tag === 1) {
            return (($match_198) => {
              if ($match_198.$tag === 1) {
                const e = $match_198.$0;
                return Err(e);
              }
              if ($match_198.$tag === 0) {
                const body = $match_198.$0[0];
                const s3 = $match_198.$0[1];
                return Ok([{ mArgs: args, mBody: body }, s3]);
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing)(s2));
          }
          if ($match_197.$tag === 0) {
            const col = $match_197.$0;
            return ((bodyTok) => _AMP_AMP4($dict_Eq_Int._SLASH_EQ(bodyTok.span.start.line)(eqTok.span.start.line))(() => $dict_Ord_Int._LT_EQ(bodyTok.span.start.column)(col)) ? Err(makeError($dict_Appendable_String._PLUS_PLUS("Function body must be indented past column ")(intToStr(col)))(bodyTok.span)) : (($match_199) => {
              if ($match_199.$tag === 1) {
                const e = $match_199.$0;
                return Err(e);
              }
              if ($match_199.$tag === 0) {
                const body = $match_199.$0[0];
                const s3 = $match_199.$0[1];
                return Ok([{ mArgs: args, mBody: body }, s3]);
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing)(s2)))(current(s2));
          }
          throw new Error("Pattern match failed");
        })(declColumn);
      }
      throw new Error("Pattern match failed");
    })(expect(Equals)("'=' after parameters")(s1));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParams(state));
parseProtocolDeclaration = (state) => (($match_200) => {
  if ($match_200.$tag === 1) {
    const e = $match_200.$0;
    return Err(e);
  }
  if ($match_200.$tag === 0) {
    const protocolTok = $match_200.$0[0];
    const s1 = $match_200.$0[1];
    return ((hasConstraints) => (($match_201) => {
      if ($match_201.$tag === 1) {
        const e = $match_201.$0;
        return Err(e);
      }
      if ($match_201.$tag === 0) {
        const constraints = $match_201.$0[0];
        const s2 = $match_201.$0[1];
        return (($match_202) => {
          if ($match_202.$tag === 1) {
            const e = $match_202.$0;
            return Err(e);
          }
          if ($match_202.$tag === 0) {
            const nameTok = $match_202.$0[0];
            const s3 = $match_202.$0[1];
            return (($match_203) => {
              if ($match_203.$tag === 1) {
                const e = $match_203.$0;
                return Err(e);
              }
              if ($match_203.$tag === 0) {
                const params = $match_203.$0[0];
                const s4 = $match_203.$0[1];
                return (($match_204) => {
                  if ($match_204.$tag === 1) {
                    const e = $match_204.$0;
                    return Err(e);
                  }
                  if ($match_204.$tag === 0) {
                    const s5 = $match_204.$0[1];
                    return (($match_205) => {
                      if ($match_205.$tag === 1) {
                        const e = $match_205.$0;
                        return Err(e);
                      }
                      if ($match_205.$tag === 0) {
                        const s6 = $match_205.$0[1];
                        return (($match_206) => {
                          if ($match_206.$tag === 1) {
                            const e = $match_206.$0;
                            return Err(e);
                          }
                          if ($match_206.$tag === 0) {
                            const items = $match_206.$0[0];
                            const s7 = $match_206.$0[1];
                            return (($match_207) => {
                              if ($match_207.$tag === 1) {
                                const e = $match_207.$0;
                                return Err(e);
                              }
                              if ($match_207.$tag === 0) {
                                const s8 = $match_207.$0[1];
                                return ((methods) => ((endPos) => Ok([DProtocol(nameTok.lexeme)(params)(constraints)(methods)({ start: protocolTok.span.start, end: endPos }), s8]))((($match_208) => {
                                  if ($match_208.$tag === 0) {
                                    const m = $match_208.$0;
                                    return m.span.end;
                                  }
                                  if ($match_208.$tag === 1) {
                                    return nameTok.span.end;
                                  }
                                  throw new Error("Pattern match failed");
                                })(last(methods))))(mergeProtocolItems(items));
                              }
                              throw new Error("Pattern match failed");
                            })(expectBlockEnd(s7));
                          }
                          throw new Error("Pattern match failed");
                        })(parseProtocolItems(s6));
                      }
                      throw new Error("Pattern match failed");
                    })(expectBlockStart(s5));
                  }
                  throw new Error("Pattern match failed");
                })(expectKeyword("where")(s4));
              }
              throw new Error("Pattern match failed");
            })(parseTypeParams(s3));
          }
          throw new Error("Pattern match failed");
        })(expect(UpperIdentifier)("protocol name")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseOptionalConstraints(hasConstraints)(s1)))(peekConstraintContextInProtocol(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("protocol")(state));
parseProtocolItems = (state) => (($match_209) => {
  if ($match_209.$tag === 1) {
    const e = $match_209.$0;
    return Err(e);
  }
  if ($match_209.$tag === 0) {
    const item = $match_209.$0[0];
    const s1 = $match_209.$0[1];
    return (($match_210) => {
      if ($match_210.$tag === 1) {
        return Ok([[item], s1]);
      }
      if ($match_210.$tag === 0) {
        const s2 = $match_210.$0;
        return (($match_211) => {
          if ($match_211.$tag === 1) {
            const e = $match_211.$0;
            return Err(e);
          }
          if ($match_211.$tag === 0) {
            const rest = $match_211.$0[0];
            const s3 = $match_211.$0[1];
            return Ok([_COLON_COLON(item)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseProtocolItems(s2));
      }
      throw new Error("Pattern match failed");
    })(matchBlockSep(s1));
  }
  throw new Error("Pattern match failed");
})(parseProtocolItem(state));
parseProtocolItem = (state) => ((start) => (($match_212) => {
  if ($match_212.$tag === 1) {
    const e = $match_212.$0;
    return Err(e);
  }
  if ($match_212.$tag === 0) {
    const methodName = $match_212.$0[0];
    const s1 = $match_212.$0[1];
    return peekKind(Colon)(s1) ? ((s2) => (($match_213) => {
      if ($match_213.$tag === 1) {
        const e = $match_213.$0;
        return Err(e);
      }
      if ($match_213.$tag === 0) {
        const methodType = $match_213.$0[0];
        const s3 = $match_213.$0[1];
        return Ok([PIAnnotation(methodName)(methodType)({ start, end: typeSpan(methodType).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeExpression(s2)))(advance(s1)) : (($match_214) => {
      if ($match_214.$tag === 1) {
        const e = $match_214.$0;
        return Err(e);
      }
      if ($match_214.$tag === 0) {
        const args = $match_214.$0[0];
        const s2 = $match_214.$0[1];
        return (($match_215) => {
          if ($match_215.$tag === 1) {
            const e = $match_215.$0;
            return Err(e);
          }
          if ($match_215.$tag === 0) {
            const s3 = $match_215.$0[1];
            return (($match_216) => {
              if ($match_216.$tag === 1) {
                const e = $match_216.$0;
                return Err(e);
              }
              if ($match_216.$tag === 0) {
                const body = $match_216.$0[0];
                const s4 = $match_216.$0[1];
                return Ok([PIImpl(methodName)(args)(body)({ start, end: exprSpan(body).end }), s4]);
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing)(s3));
          }
          throw new Error("Pattern match failed");
        })(expect(Equals)("'=' after parameters")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParams(s1));
  }
  throw new Error("Pattern match failed");
})(parseProtocolMethodName(state)))(current(state).span.start);
parseImplementationDeclaration = (state) => (($match_217) => {
  if ($match_217.$tag === 1) {
    const e = $match_217.$0;
    return Err(e);
  }
  if ($match_217.$tag === 0) {
    const implTok = $match_217.$0[0];
    const s1 = $match_217.$0[1];
    return ((hasConstraints) => (($match_218) => {
      if ($match_218.$tag === 1) {
        const e = $match_218.$0;
        return Err(e);
      }
      if ($match_218.$tag === 0) {
        const constraints = $match_218.$0[0];
        const s2 = $match_218.$0[1];
        return (($match_219) => {
          if ($match_219.$tag === 1) {
            const e = $match_219.$0;
            return Err(e);
          }
          if ($match_219.$tag === 0) {
            const protocolTok = $match_219.$0[0];
            const s3 = $match_219.$0[1];
            return (($match_220) => {
              if ($match_220.$tag === 1) {
                const e = $match_220.$0;
                return Err(e);
              }
              if ($match_220.$tag === 0) {
                const typeArgs = $match_220.$0[0];
                const s4 = $match_220.$0[1];
                return (($match_221) => {
                  if ($match_221.$tag === 1) {
                    const e = $match_221.$0;
                    return Err(e);
                  }
                  if ($match_221.$tag === 0) {
                    const methods = $match_221.$0[0];
                    const s5 = $match_221.$0[1];
                    return ((endPos) => Ok([DImplementation(constraints)(protocolTok.lexeme)(typeArgs)(methods)({ start: implTok.span.start, end: endPos }), s5]))((($match_222) => {
                      if ($match_222.$tag === 0) {
                        const m = $match_222.$0;
                        return m.span.end;
                      }
                      if ($match_222.$tag === 1) {
                        return (($match_223) => {
                          if ($match_223.$tag === 0) {
                            const t = $match_223.$0;
                            return typeSpan(t).end;
                          }
                          if ($match_223.$tag === 1) {
                            return protocolTok.span.end;
                          }
                          throw new Error("Pattern match failed");
                        })(last(typeArgs));
                      }
                      throw new Error("Pattern match failed");
                    })(last(methods)));
                  }
                  throw new Error("Pattern match failed");
                })(parseImplementationMethods(s4));
              }
              throw new Error("Pattern match failed");
            })(parseImplementationTypeArgs(s3));
          }
          throw new Error("Pattern match failed");
        })(expect(UpperIdentifier)("protocol name")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseOptionalConstraints(hasConstraints)(s1)))(peekConstraintContext(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("implement")(state));
parseImplementationMethods = (state) => peekKeyword("where")(state) ? (($match_224) => {
  if ($match_224.$tag === 1) {
    const e = $match_224.$0;
    return Err(e);
  }
  if ($match_224.$tag === 0) {
    const s1 = $match_224.$0[1];
    return (($match_225) => {
      if ($match_225.$tag === 1) {
        const e = $match_225.$0;
        return Err(e);
      }
      if ($match_225.$tag === 0) {
        const s2 = $match_225.$0[1];
        return (($match_226) => {
          if ($match_226.$tag === 1) {
            const e = $match_226.$0;
            return Err(e);
          }
          if ($match_226.$tag === 0) {
            const methods = $match_226.$0[0];
            const s3 = $match_226.$0[1];
            return (($match_227) => {
              if ($match_227.$tag === 1) {
                const e = $match_227.$0;
                return Err(e);
              }
              if ($match_227.$tag === 0) {
                const s4 = $match_227.$0[1];
                return Ok([methods, s4]);
              }
              throw new Error("Pattern match failed");
            })(expectBlockEnd(s3));
          }
          throw new Error("Pattern match failed");
        })(parseImplMethodList(s2));
      }
      throw new Error("Pattern match failed");
    })(expectBlockStart(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("where")(state)) : Ok([[], state]);
parseImplMethodList = (state) => (($match_228) => {
  if ($match_228.$tag === 1) {
    const e = $match_228.$0;
    return Err(e);
  }
  if ($match_228.$tag === 0) {
    const method = $match_228.$0[0];
    const s1 = $match_228.$0[1];
    return (($match_229) => {
      if ($match_229.$tag === 1) {
        return Ok([[method], s1]);
      }
      if ($match_229.$tag === 0) {
        const s2 = $match_229.$0;
        return (($match_230) => {
          if ($match_230.$tag === 1) {
            const e = $match_230.$0;
            return Err(e);
          }
          if ($match_230.$tag === 0) {
            const rest = $match_230.$0[0];
            const s3 = $match_230.$0[1];
            return Ok([_COLON_COLON(method)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseImplMethodList(s2));
      }
      throw new Error("Pattern match failed");
    })(matchBlockSep(s1));
  }
  throw new Error("Pattern match failed");
})(parseImplMethod(state));
parseImplMethod = (state) => (($match_231) => {
  if ($match_231.$tag === 1) {
    const e = $match_231.$0;
    return Err(e);
  }
  if ($match_231.$tag === 0) {
    const nameResult = $match_231.$0[0];
    const s1 = $match_231.$0[1];
    return (($match_232) => {
      if ($match_232.$tag === 1) {
        const e = $match_232.$0;
        return Err(e);
      }
      if ($match_232.$tag === 0) {
        const result = $match_232.$0[0];
        const s2 = $match_232.$0[1];
        return Ok([{ name: nameResult.declName, implArgs: result.mArgs, implementation: result.mBody, span: { start: nameResult.declSpan.start, end: exprSpan(result.mBody).end } }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMethodBody(Nothing)(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclarationName(state));
parseExpression = (baseIndentFloor) => (state) => parseBinaryExpression(0)(baseIndentFloor)(state);
parseBinaryExpression = (minPrec) => (baseIndentFloor) => (state) => (($match_233) => {
  if ($match_233.$tag === 1) {
    const e = $match_233.$0;
    return Err(e);
  }
  if ($match_233.$tag === 0) {
    const left = $match_233.$0[0];
    const s1 = $match_233.$0[1];
    return parseBinExprLoop(left)(minPrec)(baseIndentFloor)(s1);
  }
  throw new Error("Pattern match failed");
})(parseUnary(baseIndentFloor)(state));
parseBinExprLoop = (left) => (minPrec) => (baseIndentFloor) => (state) => {
  while (true) {
    {
      const opTok = current(state);
      if ($dict_Eq_TokenKind._EQ_EQ(opTok.kind)(Operator)) {
        if ($dict_Eq_String._EQ_EQ(opTok.lexeme)("@")) {
          return Ok([left, state]);
        } else {
          {
            const info = getOperatorInfo(opTok.lexeme)(state.registry);
            {
              const prec = info.precedence;
              if ($dict_Ord_Int._LT(prec)(minPrec)) {
                return Ok([left, state]);
              } else {
                {
                  const s1 = advance(state);
                  {
                    const nextMin = (($match_234) => {
                      if ($match_234.$tag === 1) {
                        return prec;
                      }
                      {
                        return $dict_Num_Int._PLUS(prec)(1);
                      }
                      throw new Error("Pattern match failed");
                    })(info.associativity);
                    {
                      const $match_235 = parseBinaryExpression(nextMin)(baseIndentFloor)(s1);
                      if ($match_235.$tag === 1) {
                        const e = $match_235.$0;
                        return Err(e);
                      }
                      if ($match_235.$tag === 0) {
                        const right = $match_235.$0[0];
                        const s2 = $match_235.$0[1];
                        {
                          const node = EInfix(left)(opTok.lexeme)(right)({ start: exprSpan(left).start, end: exprSpan(right).end });
                          [left, minPrec, baseIndentFloor, state] = [node, minPrec, baseIndentFloor, s2];
                          continue;
                        }
                      }
                      throw new Error("Pattern match failed");
                    }
                  }
                }
              }
            }
          }
        }
      } else {
        return Ok([left, state]);
      }
    }
  }
};
parseUnary = (baseIndentFloor) => (state) => ((tok) => _AMP_AMP4($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)("-")) ? (($match_236) => {
  if ($match_236.$tag === 1) {
    return parseApplication(baseIndentFloor)(state);
  }
  if ($match_236.$tag === 0) {
    const nextTok = $match_236.$0;
    return _PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(LowerIdentifier))(() => _PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(UpperIdentifier))(() => _PIPE_PIPE4($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(NumberToken))(() => $dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(LParen)))) ? ((s1) => (($match_237) => {
      if ($match_237.$tag === 1) {
        const e = $match_237.$0;
        return Err(e);
      }
      if ($match_237.$tag === 0) {
        const operand = $match_237.$0[0];
        const s2 = $match_237.$0[1];
        return Ok([EUnary("-")(operand)({ start: tok.span.start, end: exprSpan(operand).end }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseUnary(baseIndentFloor)(s1)))(advance(state)) : parseApplication(baseIndentFloor)(state);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : parseApplication(baseIndentFloor)(state))(current(state));
parseApplication = (baseIndentFloor) => (state) => (($match_238) => {
  if ($match_238.$tag === 1) {
    const e = $match_238.$0;
    return Err(e);
  }
  if ($match_238.$tag === 0) {
    const callee = $match_238.$0[0];
    const s1 = $match_238.$0[1];
    return ((baseIndent) => ((effectiveIndent) => (($match_239) => {
      if ($match_239.$tag === 1) {
        const e = $match_239.$0;
        return Err(e);
      }
      if ($match_239.$tag === 0) {
        const args = $match_239.$0[0];
        const s2 = $match_239.$0[1];
        return isEmpty2(args) ? Ok([callee, s2]) : ((endPos) => Ok([EApply(callee)(args)({ start: exprSpan(callee).start, end: endPos }), s2]))((($match_240) => {
          if ($match_240.$tag === 0) {
            const a = $match_240.$0;
            return exprSpan(a).end;
          }
          if ($match_240.$tag === 1) {
            return exprSpan(callee).end;
          }
          throw new Error("Pattern match failed");
        })(last(args)));
      }
      throw new Error("Pattern match failed");
    })(parseApplicationArgs(effectiveIndent)(exprSpan(callee).end)(s1)))((($match_241) => {
      if ($match_241.$tag === 1) {
        return baseIndent;
      }
      if ($match_241.$tag === 0) {
        const floor = $match_241.$0;
        return min(baseIndent)(floor);
      }
      throw new Error("Pattern match failed");
    })(baseIndentFloor)))(exprSpan(callee).start.column);
  }
  throw new Error("Pattern match failed");
})(parsePrimaryWithAccess(state));
parseApplicationArgs = (effectiveIndent) => (lastEnd) => (state) => isExpressionStart(current(state)) ? continuesLayout(effectiveIndent)(lastEnd)(current(state)) ? (($match_242) => {
  if ($match_242.$tag === 1) {
    const e = $match_242.$0;
    return Err(e);
  }
  if ($match_242.$tag === 0) {
    const arg = $match_242.$0[0];
    const s1 = $match_242.$0[1];
    return (($match_243) => {
      if ($match_243.$tag === 1) {
        const e = $match_243.$0;
        return Err(e);
      }
      if ($match_243.$tag === 0) {
        const rest = $match_243.$0[0];
        const s2 = $match_243.$0[1];
        return Ok([_COLON_COLON(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseApplicationArgs(effectiveIndent)(exprSpan(arg).end)(s1));
  }
  throw new Error("Pattern match failed");
})(parsePrimaryWithAccess(state)) : Ok([[], state]) : Ok([[], state]);
parsePrimaryWithAccess = (state) => (($match_244) => {
  if ($match_244.$tag === 1) {
    const e = $match_244.$0;
    return Err(e);
  }
  if ($match_244.$tag === 0) {
    const expr = $match_244.$0[0];
    const s1 = $match_244.$0[1];
    return parseFieldAccesses(expr)(s1);
  }
  throw new Error("Pattern match failed");
})(parsePrimary(state));
parsePrimary = (state) => ((tok) => peekKeyword("if")(state) ? parseIf(state) : peekKeyword("let")(state) ? parseLetIn(state) : peekKeyword("case")(state) ? parseCase(state) : (($match_245) => {
  if ($match_245.$tag === 8) {
    return parseLambda(state);
  }
  if ($match_245.$tag === 0) {
    return ((s1) => Ok([EVar(tok.lexeme)("lower")(tok.span), s1]))(advance(state));
  }
  if ($match_245.$tag === 1) {
    return ((s1) => Ok([EVar(tok.lexeme)("upper")(tok.span), s1]))(advance(state));
  }
  if ($match_245.$tag === 3) {
    return ((s1) => contains(".")(tok.lexeme) ? Ok([EFloat(tok.lexeme)(tok.span), s1]) : Ok([EInt(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_245.$tag === 4) {
    return ((s1) => Ok([EString(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_245.$tag === 5) {
    return ((s1) => Ok([EChar(tok.lexeme)(tok.span), s1]))(advance(state));
  }
  if ($match_245.$tag === 9) {
    return parseParenExpr(state);
  }
  if ($match_245.$tag === 13) {
    return parseListExpr(state);
  }
  if ($match_245.$tag === 11) {
    return parseRecordExpr(state);
  }
  {
    return Err(makeError($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Expected expression but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseLambda = (state) => ((start) => ((s1) => (($match_246) => {
  if ($match_246.$tag === 1) {
    const e = $match_246.$0;
    return Err(e);
  }
  if ($match_246.$tag === 0) {
    const args = $match_246.$0[0];
    const s2 = $match_246.$0[1];
    return (($match_247) => {
      if ($match_247.$tag === 1) {
        const e = $match_247.$0;
        return Err(e);
      }
      if ($match_247.$tag === 0) {
        const body = $match_247.$0[0];
        const s3 = $match_247.$0[1];
        return Ok([ELambda(args)(body)({ start, end: exprSpan(body).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Nothing)(s2));
  }
  throw new Error("Pattern match failed");
})(parseLambdaArgs(s1)))(advance(state)))(current(state).span.start);
parseIf = (state) => (($match_248) => {
  if ($match_248.$tag === 1) {
    const e = $match_248.$0;
    return Err(e);
  }
  if ($match_248.$tag === 0) {
    const ifTok = $match_248.$0[0];
    const s1 = $match_248.$0[1];
    return (($match_249) => {
      if ($match_249.$tag === 1) {
        const e = $match_249.$0;
        return Err(e);
      }
      if ($match_249.$tag === 0) {
        const condition = $match_249.$0[0];
        const s2 = $match_249.$0[1];
        return (($match_250) => {
          if ($match_250.$tag === 1) {
            const e = $match_250.$0;
            return Err(e);
          }
          if ($match_250.$tag === 0) {
            const s3 = $match_250.$0[1];
            return (($match_251) => {
              if ($match_251.$tag === 1) {
                const e = $match_251.$0;
                return Err(e);
              }
              if ($match_251.$tag === 0) {
                const thenBranch = $match_251.$0[0];
                const s4 = $match_251.$0[1];
                return (($match_252) => {
                  if ($match_252.$tag === 1) {
                    const e = $match_252.$0;
                    return Err(e);
                  }
                  if ($match_252.$tag === 0) {
                    const s5 = $match_252.$0[1];
                    return (($match_253) => {
                      if ($match_253.$tag === 1) {
                        const e = $match_253.$0;
                        return Err(e);
                      }
                      if ($match_253.$tag === 0) {
                        const elseBranch = $match_253.$0[0];
                        const s6 = $match_253.$0[1];
                        return Ok([EIf(condition)(thenBranch)(elseBranch)({ start: ifTok.span.start, end: exprSpan(elseBranch).end }), s6]);
                      }
                      throw new Error("Pattern match failed");
                    })(parseExpression(Nothing)(s5));
                  }
                  throw new Error("Pattern match failed");
                })(expectKeyword("else")(s4));
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing)(s3));
          }
          throw new Error("Pattern match failed");
        })(expectKeyword("then")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Nothing)(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("if")(state));
parseLetIn = (state) => (($match_254) => {
  if ($match_254.$tag === 1) {
    const e = $match_254.$0;
    return Err(e);
  }
  if ($match_254.$tag === 0) {
    const letTok = $match_254.$0[0];
    const s1 = $match_254.$0[1];
    return (($match_255) => {
      if ($match_255.$tag === 1) {
        const e = $match_255.$0;
        return Err(e);
      }
      if ($match_255.$tag === 0) {
        const s2 = $match_255.$0[1];
        return (($match_256) => {
          if ($match_256.$tag === 1) {
            const e = $match_256.$0;
            return Err(e);
          }
          if ($match_256.$tag === 0) {
            const bindings = $match_256.$0[0];
            const s3 = $match_256.$0[1];
            return (($match_257) => {
              if ($match_257.$tag === 1) {
                const e = $match_257.$0;
                return Err(e);
              }
              if ($match_257.$tag === 0) {
                const s4 = $match_257.$0[1];
                return (($match_258) => {
                  if ($match_258.$tag === 1) {
                    const e = $match_258.$0;
                    return Err(e);
                  }
                  if ($match_258.$tag === 0) {
                    const s5 = $match_258.$0[1];
                    return (($match_259) => {
                      if ($match_259.$tag === 1) {
                        const e = $match_259.$0;
                        return Err(e);
                      }
                      if ($match_259.$tag === 0) {
                        const body = $match_259.$0[0];
                        const s6 = $match_259.$0[1];
                        return Ok([ELetIn(bindings)(body)({ start: letTok.span.start, end: exprSpan(body).end }), s6]);
                      }
                      throw new Error("Pattern match failed");
                    })(parseExpression(Nothing)(s5));
                  }
                  throw new Error("Pattern match failed");
                })(expectKeyword("in")(s4));
              }
              throw new Error("Pattern match failed");
            })(expectBlockEnd(s3));
          }
          throw new Error("Pattern match failed");
        })(parseLetBindings(s2));
      }
      throw new Error("Pattern match failed");
    })(expectBlockStart(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("let")(state));
parseLetBindings = (state) => (($match_260) => {
  if ($match_260.$tag === 1) {
    const e = $match_260.$0;
    return Err(e);
  }
  if ($match_260.$tag === 0) {
    const binding = $match_260.$0[0];
    const s1 = $match_260.$0[1];
    return (($match_261) => {
      if ($match_261.$tag === 1) {
        return Ok([[binding], s1]);
      }
      if ($match_261.$tag === 0) {
        const s2 = $match_261.$0;
        return (($match_262) => {
          if ($match_262.$tag === 1) {
            const e = $match_262.$0;
            return Err(e);
          }
          if ($match_262.$tag === 0) {
            const rest = $match_262.$0[0];
            const s3 = $match_262.$0[1];
            return Ok([_COLON_COLON(binding)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseLetBindings(s2));
      }
      throw new Error("Pattern match failed");
    })(matchBlockSep(s1));
  }
  throw new Error("Pattern match failed");
})(parseLetBinding(state));
parseLetBinding = (state) => (($match_263) => {
  if ($match_263.$tag === 1) {
    const e = $match_263.$0;
    return Err(e);
  }
  if ($match_263.$tag === 0) {
    const decl = $match_263.$0[0];
    const s1 = $match_263.$0[1];
    return Ok([declToValueDecl(decl), s1]);
  }
  throw new Error("Pattern match failed");
})(parseDeclaration(state));
parseCase = (state) => (($match_264) => {
  if ($match_264.$tag === 1) {
    const e = $match_264.$0;
    return Err(e);
  }
  if ($match_264.$tag === 0) {
    const caseTok = $match_264.$0[0];
    const s1 = $match_264.$0[1];
    return (($match_265) => {
      if ($match_265.$tag === 1) {
        const e = $match_265.$0;
        return Err(e);
      }
      if ($match_265.$tag === 0) {
        const discriminant = $match_265.$0[0];
        const s2 = $match_265.$0[1];
        return (($match_266) => {
          if ($match_266.$tag === 1) {
            const e = $match_266.$0;
            return Err(e);
          }
          if ($match_266.$tag === 0) {
            const s3 = $match_266.$0[1];
            return (($match_267) => {
              if ($match_267.$tag === 1) {
                const e = $match_267.$0;
                return Err(e);
              }
              if ($match_267.$tag === 0) {
                const s4 = $match_267.$0[1];
                return (($match_268) => {
                  if ($match_268.$tag === 1) {
                    const e = $match_268.$0;
                    return Err(e);
                  }
                  if ($match_268.$tag === 0) {
                    const branches = $match_268.$0[0];
                    const s5 = $match_268.$0[1];
                    return (($match_269) => {
                      if ($match_269.$tag === 1) {
                        const e = $match_269.$0;
                        return Err(e);
                      }
                      if ($match_269.$tag === 0) {
                        const s6 = $match_269.$0[1];
                        return ((endPos) => Ok([ECase(discriminant)(branches)({ start: caseTok.span.start, end: endPos }), s6]))((($match_270) => {
                          if ($match_270.$tag === 0) {
                            const b = $match_270.$0;
                            return b.span.end;
                          }
                          if ($match_270.$tag === 1) {
                            return exprSpan(discriminant).end;
                          }
                          throw new Error("Pattern match failed");
                        })(last(branches)));
                      }
                      throw new Error("Pattern match failed");
                    })(expectBlockEnd(s5));
                  }
                  throw new Error("Pattern match failed");
                })(parseCaseBranches(s4));
              }
              throw new Error("Pattern match failed");
            })(expectBlockStart(s3));
          }
          throw new Error("Pattern match failed");
        })(expectKeyword("of")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Nothing)(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("case")(state));
parseCaseBranches = (state) => (($match_271) => {
  if ($match_271.$tag === 1) {
    const e = $match_271.$0;
    return Err(e);
  }
  if ($match_271.$tag === 0) {
    const branch = $match_271.$0[0];
    const s1 = $match_271.$0[1];
    return (($match_272) => {
      if ($match_272.$tag === 1) {
        return Ok([[branch], s1]);
      }
      if ($match_272.$tag === 0) {
        const s2 = $match_272.$0;
        return (($match_273) => {
          if ($match_273.$tag === 1) {
            const e = $match_273.$0;
            return Err(e);
          }
          if ($match_273.$tag === 0) {
            const rest = $match_273.$0[0];
            const s3 = $match_273.$0[1];
            return Ok([_COLON_COLON(branch)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseCaseBranches(s2));
      }
      throw new Error("Pattern match failed");
    })(matchBlockSep(s1));
  }
  throw new Error("Pattern match failed");
})(parseCaseBranch(state));
parseCaseBranch = (state) => (($match_274) => {
  if ($match_274.$tag === 1) {
    const e = $match_274.$0;
    return Err(e);
  }
  if ($match_274.$tag === 0) {
    const pat = $match_274.$0[0];
    const s1 = $match_274.$0[1];
    return (($match_275) => {
      if ($match_275.$tag === 1) {
        const e = $match_275.$0;
        return Err(e);
      }
      if ($match_275.$tag === 0) {
        const s2 = $match_275.$0[1];
        return ((branchIndent) => (($match_276) => {
          if ($match_276.$tag === 1) {
            const e = $match_276.$0;
            return Err(e);
          }
          if ($match_276.$tag === 0) {
            const body = $match_276.$0[0];
            const s3 = $match_276.$0[1];
            return Ok([{ pattern: pat, body, span: { start: patSpan(pat).start, end: exprSpan(body).end } }, s3]);
          }
          throw new Error("Pattern match failed");
        })(parseExpression(Just(branchIndent))(s2)))($dict_Num_Int._PLUS(patSpan(pat).start.column)(1));
      }
      throw new Error("Pattern match failed");
    })(expectOperator("->")(s1));
  }
  throw new Error("Pattern match failed");
})(parsePattern(state));
parseParenExpr = (state) => ((start) => ((s1) => peekKind(RParen)(s1) ? ((s2) => Ok([EUnit({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : peekKind(Operator)(s1) ? (($match_277) => {
  if ($match_277.$tag === 0) {
    const next = $match_277.$0;
    return $dict_Eq_TokenKind._EQ_EQ(next.kind)(RParen) ? ((opTok) => ((s2) => ((s3) => Ok([EVar(opTok.lexeme)("operator")({ start, end: previous(s3).span.end }), s3]))(advance(s2)))(advance(s1)))(current(s1)) : parseParenExprContents(start)(s1);
  }
  if ($match_277.$tag === 1) {
    return parseParenExprContents(start)(s1);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(s1)) : parseParenExprContents(start)(s1))(advance(state)))(current(state).span.start);
parseParenExprContents = (start) => (state) => (($match_278) => {
  if ($match_278.$tag === 1) {
    const e = $match_278.$0;
    return Err(e);
  }
  if ($match_278.$tag === 0) {
    const first = $match_278.$0[0];
    const s1 = $match_278.$0[1];
    return (($match_279) => {
      if ($match_279.$tag === 1) {
        const e = $match_279.$0;
        return Err(e);
      }
      if ($match_279.$tag === 0) {
        const more = $match_279.$0[0];
        const s2 = $match_279.$0[1];
        return (($match_280) => {
          if ($match_280.$tag === 1) {
            const e = $match_280.$0;
            return Err(e);
          }
          if ($match_280.$tag === 0) {
            const cp = $match_280.$0[0];
            const s3 = $match_280.$0[1];
            return isEmpty2(more) ? Ok([EParen(first)({ start, end: cp.span.end }), s3]) : Ok([ETuple(_COLON_COLON(first)(more))({ start, end: cp.span.end }), s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen)("close group")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleExprs(s1));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing)(state));
parseMoreTupleExprs = (state) => peekKind(Comma)(state) ? ((s1) => (($match_281) => {
  if ($match_281.$tag === 1) {
    const e = $match_281.$0;
    return Err(e);
  }
  if ($match_281.$tag === 0) {
    const expr = $match_281.$0[0];
    const s2 = $match_281.$0[1];
    return (($match_282) => {
      if ($match_282.$tag === 1) {
        const e2 = $match_282.$0;
        return Err(e2);
      }
      if ($match_282.$tag === 0) {
        const rest = $match_282.$0[0];
        const s3 = $match_282.$0[1];
        return Ok([_COLON_COLON(expr)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleExprs(s2));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing)(s1)))(advance(state)) : Ok([[], state]);
parseListExpr = (state) => ((start) => ((s1) => peekKind(RBracket)(s1) ? ((s2) => Ok([EList([])({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : (($match_283) => {
  if ($match_283.$tag === 1) {
    const e = $match_283.$0;
    return Err(e);
  }
  if ($match_283.$tag === 0) {
    const first = $match_283.$0[0];
    const s2 = $match_283.$0[1];
    return (($match_284) => {
      if ($match_284.$tag === 0) {
        const s3 = $match_284.$0;
        return (($match_285) => {
          if ($match_285.$tag === 1) {
            const e = $match_285.$0;
            return Err(e);
          }
          if ($match_285.$tag === 0) {
            const endExpr = $match_285.$0[0];
            const s4 = $match_285.$0[1];
            return (($match_286) => {
              if ($match_286.$tag === 1) {
                const e = $match_286.$0;
                return Err(e);
              }
              if ($match_286.$tag === 0) {
                const s5 = $match_286.$0[1];
                return Ok([EListRange(first)(endExpr)({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBracket)("close list range")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseExpression(Nothing)(s3));
      }
      if ($match_284.$tag === 1) {
        return (($match_287) => {
          if ($match_287.$tag === 1) {
            const e = $match_287.$0;
            return Err(e);
          }
          if ($match_287.$tag === 0) {
            const more = $match_287.$0[0];
            const s4 = $match_287.$0[1];
            return (($match_288) => {
              if ($match_288.$tag === 1) {
                const e = $match_288.$0;
                return Err(e);
              }
              if ($match_288.$tag === 0) {
                const s5 = $match_288.$0[1];
                return Ok([EList(_COLON_COLON(first)(more))({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBracket)("close list")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseMoreListExprs(s2));
      }
      throw new Error("Pattern match failed");
    })(matchKind(Range)(s2));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing)(s1)))(advance(state)))(current(state).span.start);
parseMoreListExprs = (state) => peekKind(Comma)(state) ? ((s1) => (($match_289) => {
  if ($match_289.$tag === 1) {
    const e = $match_289.$0;
    return Err(e);
  }
  if ($match_289.$tag === 0) {
    const expr = $match_289.$0[0];
    const s2 = $match_289.$0[1];
    return (($match_290) => {
      if ($match_290.$tag === 1) {
        const e2 = $match_290.$0;
        return Err(e2);
      }
      if ($match_290.$tag === 0) {
        const rest = $match_290.$0[0];
        const s3 = $match_290.$0[1];
        return Ok([_COLON_COLON(expr)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreListExprs(s2));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing)(s1)))(advance(state)) : Ok([[], state]);
parseRecordExpr = (state) => ((start) => ((s1) => peekKind(RBrace)(s1) ? ((s2) => Ok([ERecord([])({ start, end: previous(s2).span.end }), s2]))(advance(s1)) : (($match_291) => {
  if ($match_291.$tag === 1) {
    const e = $match_291.$0;
    return Err(e);
  }
  if ($match_291.$tag === 0) {
    const head2 = $match_291.$0[0];
    const s2 = $match_291.$0[1];
    return (($match_292) => {
      if ($match_292.$tag === 0) {
        const s3 = $match_292.$0;
        return (($match_293) => {
          if ($match_293.$tag === 1) {
            const e = $match_293.$0;
            return Err(e);
          }
          if ($match_293.$tag === 0) {
            const fields = $match_293.$0[0];
            const s4 = $match_293.$0[1];
            return (($match_294) => {
              if ($match_294.$tag === 1) {
                const e = $match_294.$0;
                return Err(e);
              }
              if ($match_294.$tag === 0) {
                const s5 = $match_294.$0[1];
                return Ok([ERecordUpdate(head2.lexeme)(fields)({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBrace)("close record update")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldList(Nothing)(head2.span.start.column)(s3));
      }
      if ($match_292.$tag === 1) {
        return (($match_295) => {
          if ($match_295.$tag === 1) {
            const e = $match_295.$0;
            return Err(e);
          }
          if ($match_295.$tag === 0) {
            const fields = $match_295.$0[0];
            const s4 = $match_295.$0[1];
            return (($match_296) => {
              if ($match_296.$tag === 1) {
                const e = $match_296.$0;
                return Err(e);
              }
              if ($match_296.$tag === 0) {
                const s5 = $match_296.$0[1];
                return Ok([ERecord(fields)({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBrace)("close record")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldList(Just(head2))(head2.span.start.column)(s2));
      }
      throw new Error("Pattern match failed");
    })(matchKind(Pipe)(s2));
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier)("record field or base")(s1)))(advance(state)))(current(state).span.start);
parseRecordFieldList = (firstFieldToken) => (baseIndent) => (state) => (($match_297) => {
  if ($match_297.$tag === 0) {
    const tok = $match_297.$0;
    return (($match_298) => {
      if ($match_298.$tag === 1) {
        const e = $match_298.$0;
        return Err(e);
      }
      if ($match_298.$tag === 0) {
        const firstField = $match_298.$0[0];
        const s1 = $match_298.$0[1];
        return (($match_299) => {
          if ($match_299.$tag === 1) {
            const e = $match_299.$0;
            return Err(e);
          }
          if ($match_299.$tag === 0) {
            const rest = $match_299.$0[0];
            const s2 = $match_299.$0[1];
            return Ok([_COLON_COLON(firstField)(rest), s2]);
          }
          throw new Error("Pattern match failed");
        })(parseMoreRecordFields(baseIndent)(s1));
      }
      throw new Error("Pattern match failed");
    })(parseRecordFieldValue(tok)(baseIndent)(state));
  }
  if ($match_297.$tag === 1) {
    return (($match_300) => {
      if ($match_300.$tag === 1) {
        const e = $match_300.$0;
        return Err(e);
      }
      if ($match_300.$tag === 0) {
        const tok = $match_300.$0[0];
        const s1 = $match_300.$0[1];
        return (($match_301) => {
          if ($match_301.$tag === 1) {
            const e = $match_301.$0;
            return Err(e);
          }
          if ($match_301.$tag === 0) {
            const firstField = $match_301.$0[0];
            const s2 = $match_301.$0[1];
            return (($match_302) => {
              if ($match_302.$tag === 1) {
                const e = $match_302.$0;
                return Err(e);
              }
              if ($match_302.$tag === 0) {
                const rest = $match_302.$0[0];
                const s3 = $match_302.$0[1];
                return Ok([_COLON_COLON(firstField)(rest), s3]);
              }
              throw new Error("Pattern match failed");
            })(parseMoreRecordFields(baseIndent)(s2));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldValue(tok)(baseIndent)(s1));
      }
      throw new Error("Pattern match failed");
    })(expect(LowerIdentifier)("record field name")(state));
  }
  throw new Error("Pattern match failed");
})(firstFieldToken);
parseRecordFieldValue = (fieldTok) => (baseIndent) => (state) => (($match_303) => {
  if ($match_303.$tag === 1) {
    const e = $match_303.$0;
    return Err(e);
  }
  if ($match_303.$tag === 0) {
    const s1 = $match_303.$0[1];
    return (($match_304) => {
      if ($match_304.$tag === 1) {
        const e = $match_304.$0;
        return Err(e);
      }
      if ($match_304.$tag === 0) {
        const value = $match_304.$0[0];
        const s2 = $match_304.$0[1];
        return Ok([{ name: fieldTok.lexeme, value, span: { start: fieldTok.span.start, end: exprSpan(value).end } }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Just(baseIndent))(s1));
  }
  throw new Error("Pattern match failed");
})(expect(Equals)("record field assignment")(state));
parseMoreRecordFields = (baseIndent) => (state) => peekKind(Comma)(state) ? ((s1) => (($match_305) => {
  if ($match_305.$tag === 1) {
    const e = $match_305.$0;
    return Err(e);
  }
  if ($match_305.$tag === 0) {
    const fieldTok = $match_305.$0[0];
    const s2 = $match_305.$0[1];
    return (($match_306) => {
      if ($match_306.$tag === 1) {
        const e = $match_306.$0;
        return Err(e);
      }
      if ($match_306.$tag === 0) {
        const field = $match_306.$0[0];
        const s3 = $match_306.$0[1];
        return (($match_307) => {
          if ($match_307.$tag === 1) {
            const e = $match_307.$0;
            return Err(e);
          }
          if ($match_307.$tag === 0) {
            const rest = $match_307.$0[0];
            const s4 = $match_307.$0[1];
            return Ok([_COLON_COLON(field)(rest), s4]);
          }
          throw new Error("Pattern match failed");
        })(parseMoreRecordFields(baseIndent)(s3));
      }
      throw new Error("Pattern match failed");
    })(parseRecordFieldValue(fieldTok)(baseIndent)(s2));
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier)("record field name")(s1)))(advance(state)) : Ok([[], state]);
var parseDeclarations = (state) => isAtEnd(state) ? Ok([[], state]) : ((tok) => $dict_Eq_Int._EQ_EQ(tok.span.start.column)(1) ? (($match_308) => {
  if ($match_308.$tag === 1) {
    const e = $match_308.$0;
    return Err(e);
  }
  if ($match_308.$tag === 0) {
    const decl = $match_308.$0[0];
    const s1 = $match_308.$0[1];
    return (($match_309) => {
      if ($match_309.$tag === 1) {
        const e = $match_309.$0;
        return Err(e);
      }
      if ($match_309.$tag === 0) {
        const rest = $match_309.$0[0];
        const s2 = $match_309.$0[1];
        return Ok([_COLON_COLON(decl)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseDeclarations(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclaration(state)) : Err(makeError("top-level declaration must start at column 1")(tok.span)))(current(state));
var parseProgram = (state) => ((tok) => $dict_Eq_Int._EQ_EQ(tok.span.start.column)(1) ? (($match_310) => {
  if ($match_310.$tag === 1) {
    const e = $match_310.$0;
    return Err(e);
  }
  if ($match_310.$tag === 0) {
    const modDecl = $match_310.$0[0];
    const s1 = $match_310.$0[1];
    return (($match_311) => {
      if ($match_311.$tag === 1) {
        const e = $match_311.$0;
        return Err(e);
      }
      if ($match_311.$tag === 0) {
        const imports = $match_311.$0[0];
        const s2 = $match_311.$0[1];
        return (($match_312) => {
          if ($match_312.$tag === 1) {
            const e = $match_312.$0;
            return Err(e);
          }
          if ($match_312.$tag === 0) {
            const decls = $match_312.$0[0];
            const s3 = $match_312.$0[1];
            return Ok([{ moduleDecl: modDecl, imports, declarations: decls }, s3]);
          }
          throw new Error("Pattern match failed");
        })(parseDeclarations(s2));
      }
      throw new Error("Pattern match failed");
    })(parseImports(s1));
  }
  throw new Error("Pattern match failed");
})(parseModuleDeclaration(state)) : Err(makeError("module declaration must start at column 1")(tok.span)))(current(state));
var parseTokens = (tokens) => (registry) => ((state) => (($match_313) => {
  if ($match_313.$tag === 1) {
    const e = $match_313.$0;
    return Err(e);
  }
  if ($match_313.$tag === 0) {
    const prog = $match_313.$0[0];
    return Ok(prog);
  }
  throw new Error("Pattern match failed");
})(parseProgram(state)))(initState(tokens)(registry));

// ../vibe-parser/dist/VibeParser/VibeParser/Layout.js
var initState2 = { output: [], stack: [], bracketDepth: 0 };
var makeVirtual = (kind) => (span) => ({ kind, lexeme: "", span });
var isLayoutKeyword = (lexeme) => (($match_0) => {
  if ($match_0 === "let") {
    return true;
  }
  if ($match_0 === "of") {
    return true;
  }
  if ($match_0 === "where") {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(lexeme);
var isContinuationKeyword = (tok) => (($match_1) => {
  if ($match_1.$tag === 2) {
    return (($match_2) => {
      if ($match_2 === "then") {
        return true;
      }
      if ($match_2 === "else") {
        return true;
      }
      {
        return false;
      }
      throw new Error("Pattern match failed");
    })(tok.lexeme);
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(tok.kind);
var stackTop = (stack) => head(stack);
var stackPop = (stack) => (($match_3) => {
  if (Array.isArray($match_3) && $match_3.length >= 1) {
    const rest = $match_3.slice(1);
    return rest;
  }
  if (Array.isArray($match_3) && $match_3.length === 0) {
    return [];
  }
  throw new Error("Pattern match failed");
})(stack);
var isLetContext = (ctx) => $dict_Eq_String._EQ_EQ(ctx.keyword)("let");
var hasLetContext = (stack) => any(isLetContext)(stack);
var skipNewlines2 = (tokens) => {
  while (true) {
    {
      const $match_4 = tokens;
      if (Array.isArray($match_4) && $match_4.length === 0) {
        return [];
      }
      if (Array.isArray($match_4) && $match_4.length >= 1) {
        const tok = $match_4[0];
        const rest = $match_4.slice(1);
        {
          const $match_5 = tok.kind;
          if ($match_5.$tag === 20) {
            tokens = rest;
            continue;
          }
          {
            return tokens;
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var nextReal = (tokens) => {
  while (true) {
    {
      const $match_6 = tokens;
      if (Array.isArray($match_6) && $match_6.length === 0) {
        return Nothing;
      }
      if (Array.isArray($match_6) && $match_6.length >= 1) {
        const tok = $match_6[0];
        const rest = $match_6.slice(1);
        {
          const $match_7 = tok.kind;
          if ($match_7.$tag === 20) {
            tokens = rest;
            continue;
          }
          {
            return Just(tok);
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var closeContextsBelow = (col) => (span) => (state) => {
  while (true) {
    {
      const $match_8 = stackTop(state.stack);
      if ($match_8.$tag === 1) {
        return state;
      }
      if ($match_8.$tag === 0) {
        const ctx = $match_8.$0;
        if ($dict_Ord_Int._LT(col)(ctx.column)) {
          [col, span, state] = [col, span, { ...state, output: _COLON_COLON(makeVirtual(BlockEnd)(span))(state.output), stack: stackPop(state.stack) }];
          continue;
        } else {
          return state;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var closeContextsForBracket = (state) => (span) => {
  while (true) {
    {
      const $match_9 = stackTop(state.stack);
      if ($match_9.$tag === 1) {
        return state;
      }
      if ($match_9.$tag === 0) {
        const ctx = $match_9.$0;
        if ($dict_Ord_Int._GT_EQ(ctx.bracketDepth)(state.bracketDepth)) {
          [state, span] = [{ ...state, output: _COLON_COLON(makeVirtual(BlockEnd)(span))(state.output), stack: stackPop(state.stack) }, span];
          continue;
        } else {
          return state;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var closeForIn = (state) => (span) => {
  while (true) {
    {
      const $match_10 = stackTop(state.stack);
      if ($match_10.$tag === 1) {
        return state;
      }
      if ($match_10.$tag === 0) {
        const ctx = $match_10.$0;
        {
          const newState = { ...state, output: _COLON_COLON(makeVirtual(BlockEnd)(span))(state.output), stack: stackPop(state.stack) };
          if ($dict_Eq_String._EQ_EQ(ctx.keyword)("let")) {
            return newState;
          } else {
            [state, span] = [newState, span];
            continue;
          }
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var closeAllContexts = (span) => (state) => {
  while (true) {
    {
      const $match_11 = stackTop(state.stack);
      if ($match_11.$tag === 1) {
        return state;
      }
      if ($match_11.$tag === 0) {
        [span, state] = [span, { ...state, output: _COLON_COLON(makeVirtual(BlockEnd)(span))(state.output), stack: stackPop(state.stack) }];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
};
var handleEof = (tok) => (state) => ((closed) => ({ ...closed, output: _COLON_COLON(tok)(closed.output) }))(closeAllContexts(tok.span)(state));
var processTokens;
var handleNewline;
var handleOpenBracket;
var handleCloseBracket;
var handleKeyword;
var handleIn;
var handleLayoutKeyword;
processTokens = (tokens) => (state) => {
  while (true) {
    {
      const $match_12 = tokens;
      if (Array.isArray($match_12) && $match_12.length === 0) {
        return state;
      }
      if (Array.isArray($match_12) && $match_12.length >= 1) {
        const tok = $match_12[0];
        const rest = $match_12.slice(1);
        {
          const $match_13 = tok.kind;
          if ($match_13.$tag === 24) {
            return handleEof(tok)(state);
          }
          if ($match_13.$tag === 20) {
            return handleNewline(rest)(state);
          }
          if ($match_13.$tag === 9) {
            return handleOpenBracket(tok)(rest)(state);
          }
          if ($match_13.$tag === 13) {
            return handleOpenBracket(tok)(rest)(state);
          }
          if ($match_13.$tag === 11) {
            return handleOpenBracket(tok)(rest)(state);
          }
          if ($match_13.$tag === 10) {
            return handleCloseBracket(tok)(rest)(state);
          }
          if ($match_13.$tag === 14) {
            return handleCloseBracket(tok)(rest)(state);
          }
          if ($match_13.$tag === 12) {
            return handleCloseBracket(tok)(rest)(state);
          }
          if ($match_13.$tag === 2) {
            return handleKeyword(tok)(rest)(state);
          }
          {
            [tokens, state] = [rest, { ...state, output: _COLON_COLON(tok)(state.output) }];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
handleNewline = (rest) => (state) => (($match_14) => {
  if ($match_14.$tag === 1) {
    return processTokens(rest)(state);
  }
  if ($match_14.$tag === 0) {
    const nextTok = $match_14.$0;
    return (($match_15) => {
      if ($match_15.$tag === 24) {
        return processTokens(rest)(state);
      }
      {
        return isContinuationKeyword(nextTok) ? processTokens(rest)(state) : ((col) => ((closed) => ((withSep) => processTokens(rest)(withSep))((($match_16) => {
          if ($match_16.$tag === 1) {
            return closed;
          }
          if ($match_16.$tag === 0) {
            const ctx = $match_16.$0;
            return $dict_Eq_Int._EQ_EQ(col)(ctx.column) ? { ...closed, output: _COLON_COLON(makeVirtual(BlockSep)(nextTok.span))(closed.output) } : closed;
          }
          throw new Error("Pattern match failed");
        })(stackTop(closed.stack))))(closeContextsBelow(col)(nextTok.span)(state)))(nextTok.span.start.column);
      }
      throw new Error("Pattern match failed");
    })(nextTok.kind);
  }
  throw new Error("Pattern match failed");
})(nextReal(rest));
handleOpenBracket = (tok) => (rest) => (state) => processTokens(rest)({ ...state, output: _COLON_COLON(tok)(state.output), bracketDepth: $dict_Num_Int._PLUS(state.bracketDepth)(1) });
handleCloseBracket = (tok) => (rest) => (state) => ((closed) => ((newDepth) => processTokens(rest)({ ...closed, output: _COLON_COLON(tok)(closed.output), bracketDepth: newDepth }))($dict_Ord_Int._GT(closed.bracketDepth)(0) ? $dict_Num_Int._MINUS(closed.bracketDepth)(1) : 0))(closeContextsForBracket(state)(tok.span));
handleKeyword = (tok) => (rest) => (state) => $dict_Eq_String._EQ_EQ(tok.lexeme)("in") ? handleIn(tok)(rest)(state) : isLayoutKeyword(tok.lexeme) ? handleLayoutKeyword(tok)(rest)(state) : processTokens(rest)({ ...state, output: _COLON_COLON(tok)(state.output) });
handleIn = (tok) => (rest) => (state) => ((closed) => processTokens(rest)({ ...closed, output: _COLON_COLON(tok)(closed.output) }))(hasLetContext(state.stack) ? closeForIn(state)(tok.span) : state);
handleLayoutKeyword = (tok) => (rest) => (state) => ((stateWithTok) => ((remaining) => (($match_17) => {
  if (Array.isArray($match_17) && $match_17.length === 0) {
    return processTokens(remaining)(stateWithTok);
  }
  if (Array.isArray($match_17) && $match_17.length >= 1) {
    const nextTok = $match_17[0];
    return (($match_18) => {
      if ($match_18.$tag === 24) {
        return processTokens(remaining)(stateWithTok);
      }
      {
        return ((ctx) => ((newState) => processTokens(remaining)(newState))({ ...stateWithTok, stack: _COLON_COLON(ctx)(stateWithTok.stack), output: _COLON_COLON(makeVirtual(BlockStart)(nextTok.span))(stateWithTok.output) }))({ column: nextTok.span.start.column, keyword: tok.lexeme, bracketDepth: stateWithTok.bracketDepth });
      }
      throw new Error("Pattern match failed");
    })(nextTok.kind);
  }
  throw new Error("Pattern match failed");
})(remaining))(skipNewlines2(rest)))({ ...state, output: _COLON_COLON(tok)(state.output) });
var insertLayoutTokens = (tokens) => ((finalState) => reverse(finalState.output))(processTokens(tokens)(initState2));

// ../vibe-parser/dist/Json/Json/Encode.js
var JsonString = ($0) => ({ $tag: 0, $0 });
var JsonInt = ($0) => ({ $tag: 1, $0 });
var JsonFloat = ($0) => ({ $tag: 2, $0 });
var JsonBool = ($0) => ({ $tag: 3, $0 });
var JsonNull = { $tag: 4 };
var JsonArray = ($0) => ({ $tag: 5, $0 });
var JsonObject = ($0) => ({ $tag: 6, $0 });
var string = (s) => JsonString(s);
var int = (n) => JsonInt(n);
var bool = (b) => JsonBool(b);
var $null = { $tag: 4 };
var hexDigit = (n) => (($match_0) => {
  if ($match_0 === 0) {
    return "0";
  }
  if ($match_0 === 1) {
    return "1";
  }
  if ($match_0 === 2) {
    return "2";
  }
  if ($match_0 === 3) {
    return "3";
  }
  if ($match_0 === 4) {
    return "4";
  }
  if ($match_0 === 5) {
    return "5";
  }
  if ($match_0 === 6) {
    return "6";
  }
  if ($match_0 === 7) {
    return "7";
  }
  if ($match_0 === 8) {
    return "8";
  }
  if ($match_0 === 9) {
    return "9";
  }
  if ($match_0 === 10) {
    return "a";
  }
  if ($match_0 === 11) {
    return "b";
  }
  if ($match_0 === 12) {
    return "c";
  }
  if ($match_0 === 13) {
    return "d";
  }
  if ($match_0 === 14) {
    return "e";
  }
  {
    return "f";
  }
  throw new Error("Pattern match failed");
})(n);
var hexByte = (n) => ((hi) => ((lo) => $dict_Appendable_String._PLUS_PLUS(hexDigit(hi))(hexDigit(lo)))($dict_Integral_Int._PERCENT(n)(16)))($dict_Integral_Int._SLASH_SLASH(n)(16));
var escapeControl = (code) => $dict_Appendable_String._PLUS_PLUS("\\u00")(hexByte(code));
var escapeChar = (c) => (($match_1) => {
  if ($match_1 === '"') {
    return "\\\"";
  }
  if ($match_1 === "\\") {
    return "\\\\";
  }
  if ($match_1 === `
`) {
    return "\\n";
  }
  if ($match_1 === "\r") {
    return "\\r";
  }
  if ($match_1 === "\t") {
    return "\\t";
  }
  {
    return ((code) => $dict_Ord_Int._LT(code)(32) ? escapeControl(code) : $dict_Show_Char.toString(c))(toCode(c));
  }
  throw new Error("Pattern match failed");
})(c);
var escapeChars = (chars) => ((go) => go(chars)(""))((cs) => (acc) => {
  while (true) {
    {
      const $match_2 = cs;
      if (Array.isArray($match_2) && $match_2.length === 0) {
        return acc;
      }
      if (Array.isArray($match_2) && $match_2.length >= 1) {
        const c = $match_2[0];
        const rest = $match_2.slice(1);
        [cs, acc] = [rest, $dict_Appendable_String._PLUS_PLUS(acc)(escapeChar(c))];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
var renderString = (s) => $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS('"')(escapeChars(toList(s))))('"');
var renderFloat = (n) => ((s) => (($match_3) => {
  if ($match_3 === "NaN") {
    return "null";
  }
  if ($match_3 === "Infinity") {
    return "null";
  }
  if ($match_3 === "-Infinity") {
    return "null";
  }
  {
    return s;
  }
  throw new Error("Pattern match failed");
})(s))($dict_Show_Float.toString(n));
var repeatSpaces = (n) => $dict_Ord_Int._LT_EQ(n)(0) ? "" : $dict_Appendable_String._PLUS_PLUS(" ")(repeatSpaces($dict_Num_Int._MINUS(n)(1)));
var makeIndent = (indent) => (depth) => repeatSpaces($dict_Num_Int._STAR(indent)(depth));
var renderValue;
var renderArray;
var renderObject;
var joinValues;
var joinValuesIndented;
var joinPairs;
var joinPairsIndented;
renderValue = (indent) => (depth) => (val) => (($match_4) => {
  if ($match_4.$tag === 0) {
    const s = $match_4.$0;
    return renderString(s);
  }
  if ($match_4.$tag === 1) {
    const n = $match_4.$0;
    return $dict_Show_Int.toString(n);
  }
  if ($match_4.$tag === 2) {
    const n = $match_4.$0;
    return renderFloat(n);
  }
  if ($match_4.$tag === 3) {
    const b = $match_4.$0;
    return b ? "true" : "false";
  }
  if ($match_4.$tag === 4) {
    return "null";
  }
  if ($match_4.$tag === 5) {
    const items = $match_4.$0;
    return renderArray(indent)(depth)(items);
  }
  if ($match_4.$tag === 6) {
    const pairs = $match_4.$0;
    return renderObject(indent)(depth)(pairs);
  }
  throw new Error("Pattern match failed");
})(val);
renderArray = (indent) => (depth) => (items) => (($match_5) => {
  if (Array.isArray($match_5) && $match_5.length === 0) {
    return "[]";
  }
  {
    return $dict_Eq_Int._EQ_EQ(indent)(0) ? $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("[")(joinValues(indent)(depth)(items)))("]") : ((newDepth) => ((pad) => ((innerPad) => $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(`[
`)(pad))(joinValuesIndented(indent)(newDepth)(items)))(`
`))(innerPad))("]"))(makeIndent(indent)(depth)))(makeIndent(indent)(newDepth)))($dict_Num_Int._PLUS(depth)(1));
  }
  throw new Error("Pattern match failed");
})(items);
renderObject = (indent) => (depth) => (pairs) => (($match_6) => {
  if (Array.isArray($match_6) && $match_6.length === 0) {
    return "{}";
  }
  {
    return $dict_Eq_Int._EQ_EQ(indent)(0) ? $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("{")(joinPairs(indent)(depth)(pairs)))("}") : ((newDepth) => ((pad) => ((innerPad) => $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(`{
`)(pad))(joinPairsIndented(indent)(newDepth)(pairs)))(`
`))(innerPad))("}"))(makeIndent(indent)(depth)))(makeIndent(indent)(newDepth)))($dict_Num_Int._PLUS(depth)(1));
  }
  throw new Error("Pattern match failed");
})(pairs);
joinValues = (indent) => (depth) => (items) => ((go) => go(items)(""))((xs) => (acc) => {
  while (true) {
    {
      const $match_7 = xs;
      if (Array.isArray($match_7) && $match_7.length === 0) {
        return acc;
      }
      if (Array.isArray($match_7) && $match_7.length === 1) {
        const x = $match_7[0];
        return $dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x));
      }
      if (Array.isArray($match_7) && $match_7.length >= 1) {
        const x = $match_7[0];
        const rest = $match_7.slice(1);
        [xs, acc] = [rest, $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x)))(",")];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
joinValuesIndented = (indent) => (depth) => (items) => ((pad) => ((go) => go(items)(""))((xs) => (acc) => {
  while (true) {
    {
      const $match_8 = xs;
      if (Array.isArray($match_8) && $match_8.length === 0) {
        return acc;
      }
      if (Array.isArray($match_8) && $match_8.length === 1) {
        const x = $match_8[0];
        return $dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x));
      }
      if (Array.isArray($match_8) && $match_8.length >= 1) {
        const x = $match_8[0];
        const rest = $match_8.slice(1);
        [xs, acc] = [rest, $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x)))(`,
`))(pad)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
}))(makeIndent(indent)(depth));
joinPairs = (indent) => (depth) => (pairs) => ((go) => go(pairs)(""))((ps) => (acc) => {
  while (true) {
    {
      const $match_9 = ps;
      if (Array.isArray($match_9) && $match_9.length === 0) {
        return acc;
      }
      if (Array.isArray($match_9) && $match_9.length === 1) {
        const k = $match_9[0][0];
        const v = $match_9[0][1];
        return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(":"))(renderValue(indent)(depth)(v));
      }
      if (Array.isArray($match_9) && $match_9.length >= 1) {
        const k = $match_9[0][0];
        const v = $match_9[0][1];
        const rest = $match_9.slice(1);
        [ps, acc] = [rest, $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(":"))(renderValue(indent)(depth)(v)))(",")];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
joinPairsIndented = (indent) => (depth) => (pairs) => ((pad) => ((go) => go(pairs)(""))((ps) => (acc) => {
  while (true) {
    {
      const $match_10 = ps;
      if (Array.isArray($match_10) && $match_10.length === 0) {
        return acc;
      }
      if (Array.isArray($match_10) && $match_10.length === 1) {
        const k = $match_10[0][0];
        const v = $match_10[0][1];
        return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(": "))(renderValue(indent)(depth)(v));
      }
      if (Array.isArray($match_10) && $match_10.length >= 1) {
        const k = $match_10[0][0];
        const v = $match_10[0][1];
        const rest = $match_10.slice(1);
        [ps, acc] = [rest, $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(": "))(renderValue(indent)(depth)(v)))(`,
`))(pad)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
}))(makeIndent(indent)(depth));
var encode = (indent) => (val) => renderValue(indent)(0)(val);
var list = (encode2) => (items) => JsonArray(map(encode2)(items));
var object = (pairs) => JsonObject(pairs);

// ../vibe-parser/dist/Json/Json/Decode.ffi.js
var jsonParse = (ok, err, str) => {
  try {
    return ok(JSON.parse(str));
  } catch (e) {
    return err(e.message);
  }
};
var jsonIsNull = (v) => v === null;
var jsonIsBool = (v) => typeof v === "boolean";
var jsonIsInt = (v) => typeof v === "number" && Number.isInteger(v);
var jsonIsNumber = (v) => typeof v === "number";
var jsonIsString = (v) => typeof v === "string";
var jsonIsArray = (v) => Array.isArray(v);
var jsonIsObject = (v) => v !== null && typeof v === "object" && !Array.isArray(v);
var jsonToBool = (v) => v;
var jsonToInt = (v) => v;
var jsonToFloat = (v) => v;
var jsonToString = (v) => v;
var jsonArrayLength = (v) => v.length;
var jsonArrayGet = (just, nothing, i, v) => {
  if (i >= 0 && i < v.length) {
    return just(v[i]);
  }
  return nothing;
};
var jsonObjectKeys = (v) => Object.keys(v);
var jsonObjectGet = (just, nothing, key, v) => {
  if (Object.prototype.hasOwnProperty.call(v, key)) {
    return just(v[key]);
  }
  return nothing;
};

// ../vibe-parser/dist/Json/Json/Decode.js
var _AMP_AMP5 = (a) => (b) => a && b();
var Decoder = ($0) => ({ $tag: 0, $0 });
var Field = ($0) => ($1) => ({ $tag: 0, $0, $1 });
var Index = ($0) => ($1) => ({ $tag: 1, $0, $1 });
var Failure = ($0) => ({ $tag: 3, $0 });
var _jsonParse = ($a0) => ($a1) => ($a2) => jsonParse($a0, $a1, $a2);
var _isNull = ($a0) => jsonIsNull($a0);
var _isBool = ($a0) => jsonIsBool($a0);
var _isInt = ($a0) => jsonIsInt($a0);
var _isNumber = ($a0) => jsonIsNumber($a0);
var _isString = ($a0) => jsonIsString($a0);
var _isArray = ($a0) => jsonIsArray($a0);
var _isObject = ($a0) => jsonIsObject($a0);
var _toBool = ($a0) => jsonToBool($a0);
var _toInt = ($a0) => jsonToInt($a0);
var _toFloat = ($a0) => jsonToFloat($a0);
var _jsonToString = ($a0) => jsonToString($a0);
var _arrayLength = ($a0) => jsonArrayLength($a0);
var _arrayGet = ($a0) => ($a1) => ($a2) => ($a3) => jsonArrayGet($a0, $a1, $a2, $a3);
var _objectKeys = ($a0) => jsonObjectKeys($a0);
var _objectGet = ($a0) => ($a1) => ($a2) => ($a3) => jsonObjectGet($a0, $a1, $a2, $a3);
var runDecoder = (decoder) => (json) => (($match_0) => {
  if ($match_0.$tag === 0) {
    const f = $match_0.$0;
    return f(json);
  }
  throw new Error("Pattern match failed");
})(decoder);
var arrayGet = _arrayGet(Just)(Nothing);
var objectGet = _objectGet(Just)(Nothing);
var jsonParse2 = _jsonParse(Ok)(Err);
var decodeString_ = (json) => _isString(json) ? Ok(_jsonToString(json)) : Err(Failure("Expecting a STRING"));
var string2 = Decoder(decodeString_);
var decodeInt_ = (json) => _isInt(json) ? Ok(_toInt(json)) : Err(Failure("Expecting an INT"));
var int2 = Decoder(decodeInt_);
var decodeFloat_ = (json) => _isNumber(json) ? Ok(_toFloat(json)) : Err(Failure("Expecting a FLOAT"));
var float = Decoder(decodeFloat_);
var decodeBool_ = (json) => _isBool(json) ? Ok(_toBool(json)) : Err(Failure("Expecting a BOOL"));
var bool2 = Decoder(decodeBool_);
var decodeListItems = (decoder) => (json) => (idx) => (len) => $dict_Ord_Int._GT_EQ(idx)(len) ? Ok([]) : (($match_5) => {
  if ($match_5.$tag === 1) {
    return Err(Index(idx)(Failure("Index out of bounds")));
  }
  if ($match_5.$tag === 0) {
    const item = $match_5.$0;
    return (($match_6) => {
      if ($match_6.$tag === 1) {
        const e = $match_6.$0;
        return Err(Index(idx)(e));
      }
      if ($match_6.$tag === 0) {
        const val = $match_6.$0;
        return (($match_7) => {
          if ($match_7.$tag === 1) {
            const e = $match_7.$0;
            return Err(e);
          }
          if ($match_7.$tag === 0) {
            const rest = $match_7.$0;
            return Ok(_COLON_COLON(val)(rest));
          }
          throw new Error("Pattern match failed");
        })(decodeListItems(decoder)(json)($dict_Num_Int._PLUS(idx)(1))(len));
      }
      throw new Error("Pattern match failed");
    })(runDecoder(decoder)(item));
  }
  throw new Error("Pattern match failed");
})(arrayGet(idx)(json));
var decodeList_ = (decoder) => (json) => _isArray(json) ? decodeListItems(decoder)(json)(0)(_arrayLength(json)) : Err(Failure("Expecting an ARRAY"));
var list2 = (decoder) => Decoder(decodeList_(decoder));
var decodeField_ = (key) => (decoder) => (json) => _isObject(json) ? (($match_12) => {
  if ($match_12.$tag === 1) {
    return Err(Field(key)(Failure("Missing field")));
  }
  if ($match_12.$tag === 0) {
    const sub = $match_12.$0;
    return (($match_13) => {
      if ($match_13.$tag === 1) {
        const e = $match_13.$0;
        return Err(Field(key)(e));
      }
      if ($match_13.$tag === 0) {
        const val = $match_13.$0;
        return Ok(val);
      }
      throw new Error("Pattern match failed");
    })(runDecoder(decoder)(sub));
  }
  throw new Error("Pattern match failed");
})(objectGet(key)(json)) : Err(Failure("Expecting an OBJECT"));
var field = (key) => (decoder) => Decoder(decodeField_(key)(decoder));
var decodeMap3_ = (f) => (da) => (db) => (dc) => (json) => (($match_19) => {
  if ($match_19.$tag === 1) {
    const e = $match_19.$0;
    return Err(e);
  }
  if ($match_19.$tag === 0) {
    const a = $match_19.$0;
    return (($match_20) => {
      if ($match_20.$tag === 1) {
        const e = $match_20.$0;
        return Err(e);
      }
      if ($match_20.$tag === 0) {
        const b = $match_20.$0;
        return (($match_21) => {
          if ($match_21.$tag === 1) {
            const e = $match_21.$0;
            return Err(e);
          }
          if ($match_21.$tag === 0) {
            const c = $match_21.$0;
            return Ok(f(a)(b)(c));
          }
          throw new Error("Pattern match failed");
        })(runDecoder(dc)(json));
      }
      throw new Error("Pattern match failed");
    })(runDecoder(db)(json));
  }
  throw new Error("Pattern match failed");
})(runDecoder(da)(json));
var map3 = (f) => (da) => (db) => (dc) => Decoder(decodeMap3_(f)(da)(db)(dc));
var decodeSucceed_ = (val) => (_) => Ok(val);
var succeed = (val) => Decoder(decodeSucceed_(val));
var decodeAndThen_ = (callback) => (decoder) => (json) => (($match_31) => {
  if ($match_31.$tag === 1) {
    const e = $match_31.$0;
    return Err(e);
  }
  if ($match_31.$tag === 0) {
    const a = $match_31.$0;
    return runDecoder(callback(a))(json);
  }
  throw new Error("Pattern match failed");
})(runDecoder(decoder)(json));
var andThen = (callback) => (decoder) => Decoder(decodeAndThen_(callback)(decoder));
var jsonToValue;
var convertArray;
var convertObject;
jsonToValue = (json) => _isNull(json) ? JsonNull : _isBool(json) ? JsonBool(_toBool(json)) : _isInt(json) ? JsonInt(_toInt(json)) : _isNumber(json) ? JsonFloat(_toFloat(json)) : _isString(json) ? JsonString(_jsonToString(json)) : _isArray(json) ? JsonArray(convertArray(json)(0)(_arrayLength(json))) : _isObject(json) ? JsonObject(convertObject(json)(_objectKeys(json))) : JsonNull;
convertArray = (json) => (idx) => (len) => $dict_Ord_Int._GT_EQ(idx)(len) ? [] : (($match_32) => {
  if ($match_32.$tag === 1) {
    return [];
  }
  if ($match_32.$tag === 0) {
    const item = $match_32.$0;
    return _COLON_COLON(jsonToValue(item))(convertArray(json)($dict_Num_Int._PLUS(idx)(1))(len));
  }
  throw new Error("Pattern match failed");
})(arrayGet(idx)(json));
convertObject = (json) => (keys) => {
  while (true) {
    {
      const $match_33 = keys;
      if (Array.isArray($match_33) && $match_33.length === 0) {
        return [];
      }
      if (Array.isArray($match_33) && $match_33.length >= 1) {
        const k = $match_33[0];
        const rest = $match_33.slice(1);
        {
          const $match_34 = objectGet(k)(json);
          if ($match_34.$tag === 1) {
            [json, keys] = [json, rest];
            continue;
          }
          if ($match_34.$tag === 0) {
            const item = $match_34.$0;
            return _COLON_COLON([k, jsonToValue(item)])(convertObject(json)(rest));
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var decodeValue_ = (json) => Ok(jsonToValue(json));
var value = Decoder(decodeValue_);
var errorToString;
var errorToStringHelp;
var formatOneOf;
errorToString = (err) => errorToStringHelp("")(err);
errorToStringHelp = (context) => (err) => {
  while (true) {
    {
      const $match_35 = err;
      if ($match_35.$tag === 3) {
        const msg = $match_35.$0;
        if (isEmpty(context)) {
          return msg;
        } else {
          return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("At ")(context))(": "))(msg);
        }
      }
      if ($match_35.$tag === 0) {
        const key = $match_35.$0;
        const inner = $match_35.$1;
        {
          const newContext = isEmpty(context) ? $dict_Appendable_String._PLUS_PLUS(".")(key) : $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(context)("."))(key);
          [context, err] = [newContext, inner];
          continue;
        }
      }
      if ($match_35.$tag === 1) {
        const idx = $match_35.$0;
        const inner = $match_35.$1;
        {
          const newContext = isEmpty(context) ? $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("[")($dict_Show_Int.toString(idx)))("]") : $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(context)("["))($dict_Show_Int.toString(idx)))("]");
          [context, err] = [newContext, inner];
          continue;
        }
      }
      if ($match_35.$tag === 2) {
        const errors = $match_35.$0;
        {
          const prefix = isEmpty(context) ? `One of the following failed:
` : $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("At ")(context))(`, one of the following failed:
`);
          return $dict_Appendable_String._PLUS_PLUS(prefix)(formatOneOf(errors)(1));
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
formatOneOf = (errors) => (n) => (($match_36) => {
  if (Array.isArray($match_36) && $match_36.length === 0) {
    return "";
  }
  if (Array.isArray($match_36) && $match_36.length >= 1) {
    const e = $match_36[0];
    const rest = $match_36.slice(1);
    return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("  (")($dict_Show_Int.toString(n)))(") "))(errorToString(e)))(`
`))(formatOneOf(rest)($dict_Num_Int._PLUS(n)(1)));
  }
  throw new Error("Pattern match failed");
})(errors);
var decodeString = (decoder) => (str) => (($match_37) => {
  if ($match_37.$tag === 1) {
    const msg = $match_37.$0;
    return Err($dict_Appendable_String._PLUS_PLUS("Invalid JSON: ")(msg));
  }
  if ($match_37.$tag === 0) {
    const json = $match_37.$0;
    return (($match_38) => {
      if ($match_38.$tag === 0) {
        const val = $match_38.$0;
        return Ok(val);
      }
      if ($match_38.$tag === 1) {
        const err = $match_38.$0;
        return Err(errorToString(err));
      }
      throw new Error("Pattern match failed");
    })(runDecoder(decoder)(json));
  }
  throw new Error("Pattern match failed");
})(jsonParse2(str));
var $impl_Eq_Error__EQ_EQ;
var $dict_Eq_Error;
$impl_Eq_Error__EQ_EQ = (x_impl) => (y_impl) => (($match_39) => {
  if ($match_39[0].$tag === 0 && $match_39[1].$tag === 0) {
    const a_0 = $match_39[0].$0;
    const a_1 = $match_39[0].$1;
    const b_0 = $match_39[1].$0;
    const b_1 = $match_39[1].$1;
    return _AMP_AMP5($dict_Eq_String._EQ_EQ(a_0)(b_0))(() => $dict_Eq_Error._EQ_EQ(a_1)(b_1));
  }
  if ($match_39[0].$tag === 1 && $match_39[1].$tag === 1) {
    const a_0 = $match_39[0].$0;
    const a_1 = $match_39[0].$1;
    const b_0 = $match_39[1].$0;
    const b_1 = $match_39[1].$1;
    return _AMP_AMP5($dict_Eq_Int._EQ_EQ(a_0)(b_0))(() => $dict_Eq_Error._EQ_EQ(a_1)(b_1));
  }
  if ($match_39[0].$tag === 2 && $match_39[1].$tag === 2) {
    const a_0 = $match_39[0].$0;
    const b_0 = $match_39[1].$0;
    return $dict_Eq_List_v356($dict_Eq_Error)._EQ_EQ(a_0)(b_0);
  }
  if ($match_39[0].$tag === 3 && $match_39[1].$tag === 3) {
    const a_0 = $match_39[0].$0;
    const b_0 = $match_39[1].$0;
    return $dict_Eq_String._EQ_EQ(a_0)(b_0);
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})([x_impl, y_impl]);
$dict_Eq_Error = {
  _EQ_EQ: $impl_Eq_Error__EQ_EQ
};
var $impl_Show_Error_toString;
var $dict_Show_Error;
$impl_Show_Error_toString = (x_impl) => (($match_40) => {
  if ($match_40.$tag === 0) {
    const a0 = $match_40.$0;
    const a1 = $match_40.$1;
    return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Field(")($dict_Show_String.toString(a0)))(", "))($dict_Show_Error.toString(a1)))(")");
  }
  if ($match_40.$tag === 1) {
    const a0 = $match_40.$0;
    const a1 = $match_40.$1;
    return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Index(")($dict_Show_Int.toString(a0)))(", "))($dict_Show_Error.toString(a1)))(")");
  }
  if ($match_40.$tag === 2) {
    const a0 = $match_40.$0;
    return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("OneOf(")($dict_Show_List_v357($dict_Show_Error).toString(a0)))(")");
  }
  if ($match_40.$tag === 3) {
    const a0 = $match_40.$0;
    return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Failure(")($dict_Show_String.toString(a0)))(")");
  }
  throw new Error("Pattern match failed");
})(x_impl);
$dict_Show_Error = {
  toString: $impl_Show_Error_toString
};

// ../vibe-parser/dist/VibeLexer/VibeLexer.js
var _PIPE_PIPE5 = (a) => (b) => a || b();
var _AMP_AMP6 = (a) => (b) => a && b();
var initState3 = (source) => ({ source, index: 0, line: 1, column: 1, sourceLen: length(source) });
var isAtEnd2 = (state) => $dict_Ord_Int._GT_EQ(state.index)(state.sourceLen);
var peek = (state) => charAt(state.index)(state.source);
var peekAt = (offset) => (state) => charAt($dict_Num_Int._PLUS(state.index)(offset))(state.source);
var position = (state) => ({ offset: state.index, line: state.line, column: state.column });
var advance2 = (state) => ((ch) => ((newIndex) => ((newState) => [ch, newState])((($match_0) => {
  if ($match_0 === `
`) {
    return { ...state, index: newIndex, line: $dict_Num_Int._PLUS(state.line)(1), column: 1 };
  }
  if ($match_0 === "\t") {
    return { ...state, index: newIndex, column: $dict_Num_Int._PLUS($dict_Num_Int._STAR($dict_Integral_Int._SLASH_SLASH($dict_Num_Int._MINUS(state.column)(1))(8))(8))(9) };
  }
  {
    return { ...state, index: newIndex, column: $dict_Num_Int._PLUS(state.column)(1) };
  }
  throw new Error("Pattern match failed");
})(ch)))($dict_Num_Int._PLUS(state.index)(1)))(unsafeCharAt2(state.index)(state.source));
var skip = (state) => (($match_1) => {
  {
    const s = $match_1[1];
    return s;
  }
  throw new Error("Pattern match failed");
})(advance2(state));
var skip2 = (state) => skip(skip(state));
var sliceFrom = (startOffset) => (state) => slice(startOffset)(state.index)(state.source);
var isIdentifierStart = (c) => _PIPE_PIPE5(isAlpha(c))(() => $dict_Eq_Char._EQ_EQ(c)("_"));
var isIdentifierPart = (c) => _PIPE_PIPE5(isIdentifierStart(c))(() => isDigit(c));
var isWhitespace = (c) => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)(" "))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("\t"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)(`
`))(() => $dict_Eq_Char._EQ_EQ(c)("\r"))));
var isOperatorChar = (c) => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("!"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("#"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("$"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("%"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("&"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("*"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("+"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("."))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("/"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("<"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("="))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)(">"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("?"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("@"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("\\"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("^"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("|"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)("~"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)(":"))(() => $dict_Eq_Char._EQ_EQ(c)("-"))))))))))))))))))));
var isKeyword = (word) => (($match_2) => {
  if ($match_2 === "if") {
    return true;
  }
  if ($match_2 === "then") {
    return true;
  }
  if ($match_2 === "else") {
    return true;
  }
  if ($match_2 === "let") {
    return true;
  }
  if ($match_2 === "in") {
    return true;
  }
  if ($match_2 === "case") {
    return true;
  }
  if ($match_2 === "of") {
    return true;
  }
  if ($match_2 === "type") {
    return true;
  }
  if ($match_2 === "alias") {
    return true;
  }
  if ($match_2 === "module") {
    return true;
  }
  if ($match_2 === "import") {
    return true;
  }
  if ($match_2 === "exposing") {
    return true;
  }
  if ($match_2 === "as") {
    return true;
  }
  if ($match_2 === "port") {
    return true;
  }
  if ($match_2 === "infix") {
    return true;
  }
  if ($match_2 === "infixl") {
    return true;
  }
  if ($match_2 === "infixr") {
    return true;
  }
  if ($match_2 === "protocol") {
    return true;
  }
  if ($match_2 === "implement") {
    return true;
  }
  if ($match_2 === "where") {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(word);
var skipLineComment = (state) => {
  while (true) {
    if (isAtEnd2(state)) {
      return state;
    } else {
      {
        const $match_4 = peek(state);
        if ($match_4.$tag === 1) {
          return state;
        }
        if ($match_4.$tag === 0) {
          const c = $match_4.$0;
          if ($dict_Eq_Char._EQ_EQ(c)(`
`)) {
            return state;
          } else {
            state = skip(state);
            continue;
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var skipBlockComment;
var skipBlockCommentPair;
skipBlockComment = (depth) => (state) => {
  while (true) {
    if ($dict_Eq_Int._EQ_EQ(depth)(0)) {
      return Ok(state);
    } else {
      if (isAtEnd2(state)) {
        return Err({ message: "Unterminated block comment", span: { start: position(state), end: position(state) } });
      } else {
        {
          const $match_5 = [peek(state), peekAt(1)(state)];
          if ($match_5[0].$tag === 0 && $match_5[1].$tag === 0) {
            const c1 = $match_5[0].$0;
            const c2 = $match_5[1].$0;
            return skipBlockCommentPair(depth)(state)(c1)(c2);
          }
          {
            [depth, state] = [depth, skip(state)];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
    }
  }
};
skipBlockCommentPair = (depth) => (state) => (c1) => (c2) => (($match_6) => {
  if ($match_6 === "{") {
    return (($match_7) => {
      if ($match_7 === "-") {
        return skipBlockComment($dict_Num_Int._PLUS(depth)(1))(skip2(state));
      }
      {
        return skipBlockComment(depth)(skip(state));
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  if ($match_6 === "-") {
    return (($match_8) => {
      if ($match_8 === "}") {
        return skipBlockComment($dict_Num_Int._MINUS(depth)(1))(skip2(state));
      }
      {
        return skipBlockComment(depth)(skip(state));
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return skipBlockComment(depth)(skip(state));
  }
  throw new Error("Pattern match failed");
})(c1);
var skipWhitespaceAndComments;
var skipWsDispatch;
var skipWsMaybeLine;
var skipWsMaybeBlock;
skipWhitespaceAndComments = (sawNl) => (state) => isAtEnd2(state) ? Ok({ state, sawNewline: sawNl }) : (($match_9) => {
  if ($match_9.$tag === 1) {
    return Ok({ state, sawNewline: sawNl });
  }
  if ($match_9.$tag === 0) {
    const c = $match_9.$0;
    return skipWsDispatch(sawNl)(state)(c);
  }
  throw new Error("Pattern match failed");
})(peek(state));
skipWsDispatch = (sawNl) => (state) => (c) => isWhitespace(c) ? ((nl) => skipWhitespaceAndComments(nl)(skip(state)))(_PIPE_PIPE5(sawNl)(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(c)(`
`))(() => $dict_Eq_Char._EQ_EQ(c)("\r")))) : (($match_10) => {
  if ($match_10 === "-") {
    return skipWsMaybeLine(sawNl)(state);
  }
  if ($match_10 === "{") {
    return skipWsMaybeBlock(sawNl)(state);
  }
  {
    return Ok({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(c);
skipWsMaybeLine = (sawNl) => (state) => (($match_11) => {
  if ($match_11.$tag === 0) {
    const c2 = $match_11.$0;
    return (($match_12) => {
      if ($match_12 === "-") {
        return ((s1) => skipWhitespaceAndComments(true)(s1))(skipLineComment(state));
      }
      {
        return Ok({ state, sawNewline: sawNl });
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return Ok({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
skipWsMaybeBlock = (sawNl) => (state) => (($match_13) => {
  if ($match_13.$tag === 0) {
    const c2 = $match_13.$0;
    return (($match_14) => {
      if ($match_14 === "-") {
        return ((lineBefore) => ((s2) => (($match_15) => {
          if ($match_15.$tag === 1) {
            const e = $match_15.$0;
            return Err(e);
          }
          if ($match_15.$tag === 0) {
            const s3 = $match_15.$0;
            return ((nl) => skipWhitespaceAndComments(nl)(s3))(_PIPE_PIPE5(sawNl)(() => $dict_Ord_Int._GT(s3.line)(lineBefore)));
          }
          throw new Error("Pattern match failed");
        })(skipBlockComment(1)(s2)))(skip2(state)))(state.line);
      }
      {
        return Ok({ state, sawNewline: sawNl });
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return Ok({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var identKind = (text) => (isUpper2) => isKeyword(text) ? Keyword : isUpper2 ? UpperIdentifier : LowerIdentifier;
var consumeIdentifierChars = (state) => {
  while (true) {
    {
      const $match_16 = peek(state);
      if ($match_16.$tag === 1) {
        return state;
      }
      if ($match_16.$tag === 0) {
        const c = $match_16.$0;
        if (_PIPE_PIPE5(isIdentifierPart(c))(() => $dict_Eq_Char._EQ_EQ(c)("'"))) {
          state = skip(state);
          continue;
        } else {
          return state;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var readIdentifierOrKeyword = (state) => (startPos) => ((startIdx) => (($match_17) => {
  {
    const firstChar = $match_17[0];
    const s1 = $match_17[1];
    return ((isUpper2) => ((s2) => ((text) => ((endPos) => ((span) => ((kind) => [{ kind, lexeme: text, span }, s2])(identKind(text)(isUpper2)))({ start: startPos, end: endPos }))(position(s2)))(sliceFrom(startIdx)(s2)))(consumeIdentifierChars(s1)))(isUpper(firstChar));
  }
  throw new Error("Pattern match failed");
})(advance2(state)))(state.index);
var makeNumberToken = (state) => (startIdx) => (startPos) => ((text) => ((endPos) => [{ kind: NumberToken, lexeme: text, span: { start: startPos, end: endPos } }, state])(position(state)))(sliceFrom(startIdx)(state));
var consumeDigits = (state) => {
  while (true) {
    {
      const $match_18 = peek(state);
      if ($match_18.$tag === 1) {
        return state;
      }
      if ($match_18.$tag === 0) {
        const c = $match_18.$0;
        if (isDigit(c)) {
          state = skip(state);
          continue;
        } else {
          return state;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var readNumberAfterDot = (s1) => (startIdx) => (startPos) => (($match_19) => {
  if ($match_19.$tag === 0) {
    const d = $match_19.$0;
    return isDigit(d) ? ((s2) => ((s3) => makeNumberToken(s3)(startIdx)(startPos))(consumeDigits(s2)))(skip(s1)) : makeNumberToken(s1)(startIdx)(startPos);
  }
  {
    return makeNumberToken(s1)(startIdx)(startPos);
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(s1));
var readNumberAfterInt = (s1) => (startIdx) => (startPos) => (($match_20) => {
  if ($match_20.$tag === 0) {
    const c = $match_20.$0;
    return (($match_21) => {
      if ($match_21 === ".") {
        return readNumberAfterDot(s1)(startIdx)(startPos);
      }
      {
        return makeNumberToken(s1)(startIdx)(startPos);
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return makeNumberToken(s1)(startIdx)(startPos);
  }
  throw new Error("Pattern match failed");
})(peek(s1));
var readNumber = (state) => (startPos) => ((startIdx) => ((s1) => readNumberAfterInt(s1)(startIdx)(startPos))(consumeDigits(state)))(state.index);
var isValidStringEscape = (esc) => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(esc)("n"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(esc)("r"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(esc)("t"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(esc)('"'))(() => $dict_Eq_Char._EQ_EQ(esc)("\\")))));
var readStringBody;
var readStringEscape;
readStringBody = (state) => (startIdx) => (startPos) => {
  while (true) {
    {
      const $match_22 = peek(state);
      if ($match_22.$tag === 1) {
        return Err({ message: "Unterminated string literal", span: { start: startPos, end: position(state) } });
      }
      if ($match_22.$tag === 0) {
        const c = $match_22.$0;
        {
          const $match_23 = c;
          if ($match_23 === '"') {
            {
              const s1 = skip(state);
              {
                const text = sliceFrom(startIdx)(s1);
                {
                  const endPos = position(s1);
                  return Ok([{ kind: StringToken, lexeme: text, span: { start: startPos, end: endPos } }, s1]);
                }
              }
            }
          }
          if ($match_23 === `
`) {
            return Err({ message: "Unterminated string literal", span: { start: startPos, end: position(state) } });
          }
          if ($match_23 === "\\") {
            return readStringEscape(state)(startIdx)(startPos);
          }
          {
            [state, startIdx, startPos] = [skip(state), startIdx, startPos];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
readStringEscape = (state) => (startIdx) => (startPos) => ((s1) => (($match_24) => {
  if ($match_24.$tag === 1) {
    return Err({ message: "Unterminated string literal", span: { start: startPos, end: position(s1) } });
  }
  if ($match_24.$tag === 0) {
    const esc = $match_24.$0;
    return isValidStringEscape(esc) ? readStringBody(skip(s1))(startIdx)(startPos) : Err({ message: "Invalid escape in string", span: { start: startPos, end: position(s1) } });
  }
  throw new Error("Pattern match failed");
})(peek(s1)))(skip(state));
var readString = (state) => (startPos) => ((startIdx) => ((s1) => readStringBody(s1)(startIdx)(startPos))(skip(state)))(state.index);
var isValidCharEscape = (esc) => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(esc)("n"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(esc)("r"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(esc)("t"))(() => _PIPE_PIPE5($dict_Eq_Char._EQ_EQ(esc)("'"))(() => $dict_Eq_Char._EQ_EQ(esc)("\\")))));
var expectClosingQuote = (state) => (startIdx) => (startPos) => (($match_25) => {
  if ($match_25.$tag === 0) {
    const c = $match_25.$0;
    return (($match_26) => {
      if ($match_26 === "'") {
        return ((s1) => ((text) => ((endPos) => Ok([{ kind: CharToken, lexeme: text, span: { start: startPos, end: endPos } }, s1]))(position(s1)))(sliceFrom(startIdx)(s1)))(skip(state));
      }
      {
        return Err({ message: "Char literal must contain exactly one character", span: { start: startPos, end: position(state) } });
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Err({ message: "Char literal must contain exactly one character", span: { start: startPos, end: position(state) } });
  }
  throw new Error("Pattern match failed");
})(peek(state));
var readCharEscape = (s1) => (startIdx) => (startPos) => ((s2) => (($match_27) => {
  if ($match_27.$tag === 1) {
    return Err({ message: "Unterminated char literal", span: { start: startPos, end: position(s2) } });
  }
  if ($match_27.$tag === 0) {
    const esc = $match_27.$0;
    return isValidCharEscape(esc) ? ((s3) => expectClosingQuote(s3)(startIdx)(startPos))(skip(s2)) : Err({ message: "Invalid escape in char literal", span: { start: startPos, end: position(s2) } });
  }
  throw new Error("Pattern match failed");
})(peek(s2)))(skip(s1));
var readChar = (state) => (startPos) => ((startIdx) => ((s1) => (($match_28) => {
  if ($match_28.$tag === 1) {
    return Err({ message: "Unterminated char literal", span: { start: startPos, end: position(s1) } });
  }
  if ($match_28.$tag === 0) {
    const c = $match_28.$0;
    return (($match_29) => {
      if ($match_29 === `
`) {
        return Err({ message: "Unterminated char literal", span: { start: startPos, end: position(s1) } });
      }
      if ($match_29 === "\\") {
        return readCharEscape(s1)(startIdx)(startPos);
      }
      {
        return ((s2) => expectClosingQuote(s2)(startIdx)(startPos))(skip(s1));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  throw new Error("Pattern match failed");
})(peek(s1)))(skip(state)))(state.index);
var makeSimpleToken = (state) => (startPos) => (kind) => ((startIdx) => ((s1) => ((text) => ((endPos) => [{ kind, lexeme: text, span: { start: startPos, end: endPos } }, s1])(position(s1)))(sliceFrom(startIdx)(s1)))(skip(state)))(state.index);
var makeTwoCharToken = (state) => (startPos) => (startIdx) => (kind) => ((s2) => ((text) => ((endPos) => [{ kind, lexeme: text, span: { start: startPos, end: endPos } }, s2])(position(s2)))(sliceFrom(startIdx)(s2)))(skip2(state));
var readDot = (state) => (startPos) => (startIdx) => (($match_30) => {
  if ($match_30.$tag === 0) {
    const c = $match_30.$0;
    return (($match_31) => {
      if ($match_31 === ".") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Range)));
      }
      {
        return Ok(Just(makeSimpleToken(state)(startPos)(Dot)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok(Just(makeSimpleToken(state)(startPos)(Dot)));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readColon = (state) => (startPos) => (startIdx) => (($match_32) => {
  if ($match_32.$tag === 0) {
    const c = $match_32.$0;
    return (($match_33) => {
      if ($match_33 === ":") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return Ok(Just(makeSimpleToken(state)(startPos)(Colon)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok(Just(makeSimpleToken(state)(startPos)(Colon)));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readEquals = (state) => (startPos) => (startIdx) => (($match_34) => {
  if ($match_34.$tag === 0) {
    const c = $match_34.$0;
    return (($match_35) => {
      if ($match_35 === ">") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      if ($match_35 === "=") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return Ok(Just(makeSimpleToken(state)(startPos)(Equals)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok(Just(makeSimpleToken(state)(startPos)(Equals)));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readPipe = (state) => (startPos) => (startIdx) => (($match_36) => {
  if ($match_36.$tag === 0) {
    const c = $match_36.$0;
    return (($match_37) => {
      if ($match_37 === ">") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      if ($match_37 === "|") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return Ok(Just(makeSimpleToken(state)(startPos)(Pipe)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok(Just(makeSimpleToken(state)(startPos)(Pipe)));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var consumeOperator;
var consumeOperatorCheck;
consumeOperator = (state) => (($match_38) => {
  if ($match_38.$tag === 1) {
    return state;
  }
  if ($match_38.$tag === 0) {
    const c = $match_38.$0;
    return isOperatorChar(c) ? consumeOperatorCheck(state)(c) : state;
  }
  throw new Error("Pattern match failed");
})(peek(state));
consumeOperatorCheck = (state) => (c) => (($match_39) => {
  if ($match_39 === "-") {
    return (($match_40) => {
      if ($match_40.$tag === 0) {
        const c2 = $match_40.$0;
        return (($match_41) => {
          if ($match_41 === ">") {
            return state;
          }
          {
            return consumeOperator(skip(state));
          }
          throw new Error("Pattern match failed");
        })(c2);
      }
      {
        return consumeOperator(skip(state));
      }
      throw new Error("Pattern match failed");
    })(peekAt(1)(state));
  }
  {
    return consumeOperator(skip(state));
  }
  throw new Error("Pattern match failed");
})(c);
var readLAngle = (state) => (startPos) => (startIdx) => (($match_42) => {
  if ($match_42.$tag === 0) {
    const c = $match_42.$0;
    return (($match_43) => {
      if ($match_43 === "|") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      if ($match_43 === "=") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readDash = (state) => (startPos) => (startIdx) => (($match_44) => {
  if ($match_44.$tag === 0) {
    const c = $match_44.$0;
    return (($match_45) => {
      if ($match_45 === ">") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      if ($match_45 === "=") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readGt = (state) => (startPos) => (startIdx) => (($match_46) => {
  if ($match_46.$tag === 0) {
    const c = $match_46.$0;
    return (($match_47) => {
      if ($match_47 === "=") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readGenericOperator = (state) => (startPos) => (startIdx) => (c) => isOperatorChar(c) ? ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state)) : Ok(Nothing);
var readPunctuationOrOperator = (state) => (startPos) => ((startIdx) => (($match_48) => {
  if ($match_48.$tag === 1) {
    return Ok(Nothing);
  }
  if ($match_48.$tag === 0) {
    const c = $match_48.$0;
    return (($match_49) => {
      if ($match_49 === "(") {
        return Ok(Just(makeSimpleToken(state)(startPos)(LParen)));
      }
      if ($match_49 === ")") {
        return Ok(Just(makeSimpleToken(state)(startPos)(RParen)));
      }
      if ($match_49 === "{") {
        return Ok(Just(makeSimpleToken(state)(startPos)(LBrace)));
      }
      if ($match_49 === "}") {
        return Ok(Just(makeSimpleToken(state)(startPos)(RBrace)));
      }
      if ($match_49 === "[") {
        return Ok(Just(makeSimpleToken(state)(startPos)(LBracket)));
      }
      if ($match_49 === "]") {
        return Ok(Just(makeSimpleToken(state)(startPos)(RBracket)));
      }
      if ($match_49 === ",") {
        return Ok(Just(makeSimpleToken(state)(startPos)(Comma)));
      }
      if ($match_49 === ".") {
        return readDot(state)(startPos)(startIdx);
      }
      if ($match_49 === ":") {
        return readColon(state)(startPos)(startIdx);
      }
      if ($match_49 === "=") {
        return readEquals(state)(startPos)(startIdx);
      }
      if ($match_49 === "|") {
        return readPipe(state)(startPos)(startIdx);
      }
      if ($match_49 === "<") {
        return readLAngle(state)(startPos)(startIdx);
      }
      if ($match_49 === "-") {
        return readDash(state)(startPos)(startIdx);
      }
      if ($match_49 === ">") {
        return readGt(state)(startPos)(startIdx);
      }
      if ($match_49 === "\\") {
        return Ok(Just(makeSimpleToken(state)(startPos)(Backslash)));
      }
      {
        return readGenericOperator(state)(startPos)(startIdx)(c);
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  throw new Error("Pattern match failed");
})(peek(state)))(state.index);
var readTokenDispatch = (state) => (startPos) => (c) => isIdentifierStart(c) ? Ok(readIdentifierOrKeyword(state)(startPos)) : isDigit(c) ? Ok(readNumber(state)(startPos)) : (($match_50) => {
  if ($match_50 === '"') {
    return readString(state)(startPos);
  }
  if ($match_50 === "'") {
    return readChar(state)(startPos);
  }
  {
    return (($match_51) => {
      if ($match_51.$tag === 1) {
        const e = $match_51.$0;
        return Err(e);
      }
      if ($match_51.$tag === 0 && $match_51.$0.$tag === 1) {
        return Err({ message: $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Unexpected character '")($dict_Show_Char.toString(c)))("'"), span: { start: startPos, end: startPos } });
      }
      if ($match_51.$tag === 0 && $match_51.$0.$tag === 0) {
        const result = $match_51.$0.$0;
        return Ok(result);
      }
      throw new Error("Pattern match failed");
    })(readPunctuationOrOperator(state)(startPos));
  }
  throw new Error("Pattern match failed");
})(c);
var readToken = (state) => (startPos) => (($match_52) => {
  if ($match_52.$tag === 1) {
    return Err({ message: "Unexpected end of input", span: { start: startPos, end: startPos } });
  }
  if ($match_52.$tag === 0) {
    const c = $match_52.$0;
    return readTokenDispatch(state)(startPos)(c);
  }
  throw new Error("Pattern match failed");
})(peek(state));
var maybeInsertNewline = (tokens) => (sawNl) => (hasEmitted) => (state) => _AMP_AMP6(sawNl)(() => hasEmitted) ? ((nlPos) => ((nlToken) => _COLON_COLON(nlToken)(tokens))({ kind: Newline, lexeme: `
`, span: { start: nlPos, end: nlPos } }))(position(state)) : tokens;
var lexLoop = (loop) => {
  while (true) {
    if (isAtEnd2(loop.state)) {
      return Ok(loop);
    } else {
      {
        const $match_53 = skipWhitespaceAndComments(loop.sawNewline)(loop.state);
        if ($match_53.$tag === 1) {
          const e = $match_53.$0;
          return Err(e);
        }
        if ($match_53.$tag === 0) {
          const skipResult = $match_53.$0;
          if (isAtEnd2(skipResult.state)) {
            return Ok({ ...loop, state: skipResult.state, sawNewline: skipResult.sawNewline });
          } else {
            {
              const tokens1 = maybeInsertNewline(loop.tokens)(skipResult.sawNewline)(loop.hasEmittedToken)(skipResult.state);
              {
                const startPos = position(skipResult.state);
                {
                  const $match_54 = readToken(skipResult.state)(startPos);
                  if ($match_54.$tag === 1) {
                    const e = $match_54.$0;
                    return Err(e);
                  }
                  if ($match_54.$tag === 0) {
                    const tok = $match_54.$0[0];
                    const newState = $match_54.$0[1];
                    loop = { tokens: _COLON_COLON(tok)(tokens1), state: newState, sawNewline: false, hasEmittedToken: true };
                    continue;
                  }
                  throw new Error("Pattern match failed");
                }
              }
            }
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var reverseHelper = (lst) => (acc) => {
  while (true) {
    {
      const $match_55 = lst;
      if (Array.isArray($match_55) && $match_55.length === 0) {
        return acc;
      }
      if (Array.isArray($match_55) && $match_55.length >= 1) {
        const x = $match_55[0];
        const xs = $match_55.slice(1);
        [lst, acc] = [xs, _COLON_COLON(x)(acc)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
};
var reverse2 = (lst) => reverseHelper(lst)([]);
var lex = (source) => ((initial) => (($match_56) => {
  if ($match_56.$tag === 1) {
    const e = $match_56.$0;
    return Err(e);
  }
  if ($match_56.$tag === 0) {
    const loop = $match_56.$0;
    return ((endPos) => ((eofToken2) => Ok(reverse2(_COLON_COLON(eofToken2)(loop.tokens))))({ kind: Eof, lexeme: "", span: { start: endPos, end: endPos } }))(position(loop.state));
  }
  throw new Error("Pattern match failed");
})(lexLoop(initial)))({ tokens: [], state: initState3(source), sawNewline: false, hasEmittedToken: false });

// ../vibe-parser/dist/VibeParser/VibeParser/Json.js
var encodePosition = (pos) => object([["offset", int(pos.offset)], ["line", int(pos.line)], ["column", int(pos.column)]]);
var encodeSpan = (span) => object([["start", encodePosition(span.start)], ["end", encodePosition(span.end)]]);
var encodeExportSpec = (spec) => (($match_0) => {
  if ($match_0.$tag === 0) {
    const name = $match_0.$0;
    const span = $match_0.$1;
    return object([["kind", string("ExportValue")], ["name", string(name)], ["span", encodeSpan(span)]]);
  }
  if ($match_0.$tag === 1) {
    const name = $match_0.$0;
    const span = $match_0.$1;
    return object([["kind", string("ExportOperator")], ["operator", string(name)], ["span", encodeSpan(span)]]);
  }
  if ($match_0.$tag === 2) {
    const name = $match_0.$0;
    const span = $match_0.$1;
    return object([["kind", string("ExportTypeAll")], ["name", string(name)], ["span", encodeSpan(span)]]);
  }
  if ($match_0.$tag === 3) {
    const name = $match_0.$0;
    const members = $match_0.$1;
    const span = $match_0.$2;
    return object([["kind", string("ExportTypeSome")], ["name", string(name)], ["members", list(string)(members)], ["span", encodeSpan(span)]]);
  }
  throw new Error("Pattern match failed");
})(spec);
var encodeExposing = (exp) => (($match_1) => {
  if ($match_1.$tag === 0) {
    const span = $match_1.$0;
    return object([["kind", string("All")], ["span", encodeSpan(span)]]);
  }
  if ($match_1.$tag === 1) {
    const specs = $match_1.$0;
    const span = $match_1.$1;
    return object([["kind", string("Explicit")], ["exports", list(encodeExportSpec)(specs)], ["span", encodeSpan(span)]]);
  }
  throw new Error("Pattern match failed");
})(exp);
var encodeModuleDecl = (m) => ((exposingVal) => object([["name", string(m.name)], ["exposing", exposingVal], ["span", encodeSpan(m.span)]]))(m.hasExposing ? encodeExposing(m.exposingClause) : $null);
var encodeImport = (imp) => ((aliasVal) => ((exposingVal) => object([["moduleName", string(imp.moduleName)], ["alias", aliasVal], ["exposing", exposingVal], ["span", encodeSpan(imp.span)]]))(imp.hasExposing ? encodeExposing(imp.exposingClause) : $null))(imp.hasAlias ? string(imp.aliasName) : $null);
var encodePattern;
var encodeRecordFieldPattern;
encodePattern = (pat) => (($match_2) => {
  if ($match_2.$tag === 0) {
    const name = $match_2.$0;
    const span = $match_2.$1;
    return object([["kind", string("VarPattern")], ["name", string(name)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 1) {
    const span = $match_2.$0;
    return object([["kind", string("WildcardPattern")], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 2) {
    const name = $match_2.$0;
    const args = $match_2.$1;
    const span = $match_2.$2;
    return object([["kind", string("ConstructorPattern")], ["name", string(name)], ["args", list(encodePattern)(args)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 3) {
    const elems = $match_2.$0;
    const span = $match_2.$1;
    return object([["kind", string("TuplePattern")], ["elements", list(encodePattern)(elems)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 4) {
    const elems = $match_2.$0;
    const span = $match_2.$1;
    return object([["kind", string("ListPattern")], ["elements", list(encodePattern)(elems)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 5) {
    const head2 = $match_2.$0;
    const tail = $match_2.$1;
    const span = $match_2.$2;
    return object([["kind", string("ConsPattern")], ["head", encodePattern(head2)], ["tail", encodePattern(tail)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 6) {
    const fields = $match_2.$0;
    const span = $match_2.$1;
    return object([["kind", string("RecordPattern")], ["fields", list(encodeRecordFieldPattern)(fields)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 7) {
    const value2 = $match_2.$0;
    const span = $match_2.$1;
    return object([["kind", string("IntPattern")], ["value", string(value2)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 8) {
    const value2 = $match_2.$0;
    const span = $match_2.$1;
    return object([["kind", string("FloatPattern")], ["value", string(value2)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 9) {
    const value2 = $match_2.$0;
    const span = $match_2.$1;
    return object([["kind", string("StringPattern")], ["value", string(value2)], ["span", encodeSpan(span)]]);
  }
  if ($match_2.$tag === 10) {
    const value2 = $match_2.$0;
    const span = $match_2.$1;
    return object([["kind", string("CharPattern")], ["value", string(value2)], ["span", encodeSpan(span)]]);
  }
  throw new Error("Pattern match failed");
})(pat);
encodeRecordFieldPattern = (rfp) => object([["name", string(rfp.name)], ["pattern", encodePattern(rfp.pattern)]]);
var encodeExpr;
var encodeRecordField;
var encodeCaseBranch;
var encodeValueDecl;
encodeExpr = (expr) => (($match_3) => {
  if ($match_3.$tag === 0) {
    const name = $match_3.$0;
    const ns = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("Var")], ["name", string(name)], ["namespace", string(ns)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 1) {
    const value2 = $match_3.$0;
    const span = $match_3.$1;
    return object([["kind", string("Number")], ["value", string(value2)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 2) {
    const value2 = $match_3.$0;
    const span = $match_3.$1;
    return object([["kind", string("Number")], ["value", string(value2)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 3) {
    const value2 = $match_3.$0;
    const span = $match_3.$1;
    return object([["kind", string("String")], ["value", string(value2)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 4) {
    const value2 = $match_3.$0;
    const span = $match_3.$1;
    return object([["kind", string("Char")], ["value", string(value2)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 5) {
    const args = $match_3.$0;
    const body = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("Lambda")], ["args", list(encodePattern)(args)], ["body", encodeExpr(body)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 6) {
    const callee = $match_3.$0;
    const args = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("Apply")], ["callee", encodeExpr(callee)], ["args", list(encodeExpr)(args)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 7) {
    const cond = $match_3.$0;
    const thenBranch = $match_3.$1;
    const elseBranch = $match_3.$2;
    const span = $match_3.$3;
    return object([["kind", string("If")], ["condition", encodeExpr(cond)], ["thenBranch", encodeExpr(thenBranch)], ["elseBranch", encodeExpr(elseBranch)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 8) {
    const bindings = $match_3.$0;
    const body = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("LetIn")], ["bindings", list(encodeValueDecl)(bindings)], ["body", encodeExpr(body)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 9) {
    const discriminant = $match_3.$0;
    const branches = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("Case")], ["discriminant", encodeExpr(discriminant)], ["branches", list(encodeCaseBranch)(branches)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 10) {
    const left = $match_3.$0;
    const op = $match_3.$1;
    const right = $match_3.$2;
    const span = $match_3.$3;
    return object([["kind", string("Infix")], ["left", encodeExpr(left)], ["operator", string(op)], ["right", encodeExpr(right)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 11) {
    const op = $match_3.$0;
    const operand = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("Unary")], ["operator", string(op)], ["operand", encodeExpr(operand)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 12) {
    const inner = $match_3.$0;
    const span = $match_3.$1;
    return object([["kind", string("Paren")], ["expression", encodeExpr(inner)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 13) {
    const elems = $match_3.$0;
    const span = $match_3.$1;
    return object([["kind", string("Tuple")], ["elements", list(encodeExpr)(elems)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 14) {
    const span = $match_3.$0;
    return object([["kind", string("Unit")], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 15) {
    const elems = $match_3.$0;
    const span = $match_3.$1;
    return object([["kind", string("List")], ["elements", list(encodeExpr)(elems)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 16) {
    const start = $match_3.$0;
    const end = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("ListRange")], ["start", encodeExpr(start)], ["end", encodeExpr(end)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 17) {
    const fields = $match_3.$0;
    const span = $match_3.$1;
    return object([["kind", string("Record")], ["fields", list(encodeRecordField)(fields)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 18) {
    const base = $match_3.$0;
    const fields = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("RecordUpdate")], ["base", string(base)], ["fields", list(encodeRecordField)(fields)], ["span", encodeSpan(span)]]);
  }
  if ($match_3.$tag === 19) {
    const target = $match_3.$0;
    const field2 = $match_3.$1;
    const span = $match_3.$2;
    return object([["kind", string("FieldAccess")], ["target", encodeExpr(target)], ["field", string(field2)], ["span", encodeSpan(span)]]);
  }
  throw new Error("Pattern match failed");
})(expr);
encodeRecordField = (rf) => object([["name", string(rf.name)], ["value", encodeExpr(rf.value)], ["span", encodeSpan(rf.span)]]);
encodeCaseBranch = (cb) => object([["pattern", encodePattern(cb.pattern)], ["body", encodeExpr(cb.body)], ["span", encodeSpan(cb.span)]]);
encodeValueDecl = (vd) => object([["kind", string("ValueDeclaration")], ["name", string(vd.name)], ["args", list(encodePattern)(vd.args)], ["body", encodeExpr(vd.body)], ["span", encodeSpan(vd.span)]]);
var encodeTypeExpr;
var encodeConstraint;
var encodeRecordFieldType;
encodeTypeExpr = (te) => (($match_4) => {
  if ($match_4.$tag === 0) {
    const name = $match_4.$0;
    const args = $match_4.$1;
    const span = $match_4.$2;
    return object([["kind", string("TypeRef")], ["name", string(name)], ["args", list(encodeTypeExpr)(args)], ["span", encodeSpan(span)]]);
  }
  if ($match_4.$tag === 1) {
    const from = $match_4.$0;
    const to = $match_4.$1;
    const span = $match_4.$2;
    return object([["kind", string("FunctionType")], ["from", encodeTypeExpr(from)], ["to", encodeTypeExpr(to)], ["span", encodeSpan(span)]]);
  }
  if ($match_4.$tag === 2) {
    const elems = $match_4.$0;
    const span = $match_4.$1;
    return object([["kind", string("TupleType")], ["elements", list(encodeTypeExpr)(elems)], ["span", encodeSpan(span)]]);
  }
  if ($match_4.$tag === 3) {
    const fields = $match_4.$0;
    const span = $match_4.$1;
    return object([["kind", string("RecordType")], ["fields", list(encodeRecordFieldType)(fields)], ["span", encodeSpan(span)]]);
  }
  if ($match_4.$tag === 4) {
    const constraints = $match_4.$0;
    const typ = $match_4.$1;
    const span = $match_4.$2;
    return object([["kind", string("QualifiedType")], ["constraints", list(encodeConstraint)(constraints)], ["type", encodeTypeExpr(typ)], ["span", encodeSpan(span)]]);
  }
  throw new Error("Pattern match failed");
})(te);
encodeConstraint = (c) => object([["protocolName", string(c.protocolName)], ["typeArgs", list(encodeTypeExpr)(c.typeArgs)], ["span", encodeSpan(c.span)]]);
encodeRecordFieldType = (rft) => object([["name", string(rft.name)], ["type", encodeTypeExpr(rft.fieldType)], ["span", encodeSpan(rft.span)]]);
var encodeConstructor = (cv) => object([["name", string(cv.name)], ["args", list(encodeTypeExpr)(cv.args)], ["span", encodeSpan(cv.span)]]);
var encodeProtocolMethod = (pm) => ((typeVal) => ((defaultVal) => object([["name", string(pm.name)], ["type", typeVal], ["defaultImpl", defaultVal], ["span", encodeSpan(pm.span)]]))(pm.hasDefault ? object([["args", list(encodePattern)(pm.defaultArgs)], ["body", encodeExpr(pm.defaultBody)]]) : $null))(pm.hasType ? encodeTypeExpr(pm.methodType) : $null);
var encodeMethodImpl = (mi) => ((argsVal) => object([["name", string(mi.name)], ["args", argsVal], ["implementation", encodeExpr(mi.implementation)], ["span", encodeSpan(mi.span)]]))((($match_5) => {
  if (Array.isArray($match_5) && $match_5.length === 0) {
    return $null;
  }
  {
    return list(encodePattern)(mi.implArgs);
  }
  throw new Error("Pattern match failed");
})(mi.implArgs));
var encodeDeclaration = (decl) => (($match_6) => {
  if ($match_6.$tag === 0) {
    const vd = $match_6.$0;
    return object([["kind", string("ValueDeclaration")], ["name", string(vd.name)], ["args", list(encodePattern)(vd.args)], ["body", encodeExpr(vd.body)], ["span", encodeSpan(vd.span)]]);
  }
  if ($match_6.$tag === 1) {
    const ta = $match_6.$0;
    return object([["kind", string("TypeAnnotationDeclaration")], ["name", string(ta.name)], ["annotation", encodeTypeExpr(ta.annotation)], ["span", encodeSpan(ta.span)]]);
  }
  if ($match_6.$tag === 2) {
    const decorator = $match_6.$0;
    const args = $match_6.$1;
    const name = $match_6.$2;
    const annotation = $match_6.$3;
    const span = $match_6.$4;
    return object([["kind", string("DecoratedDeclaration")], ["decorator", string(decorator)], ["args", list(string)(args)], ["name", string(name)], ["annotation", encodeTypeExpr(annotation)], ["span", encodeSpan(span)]]);
  }
  if ($match_6.$tag === 3) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const constraints = $match_6.$2;
    const ctors = $match_6.$3;
    const span = $match_6.$4;
    return object([["kind", string("TypeDeclaration")], ["name", string(name)], ["params", list(string)(params)], ["constraints", list(encodeConstraint)(constraints)], ["constructors", list(encodeConstructor)(ctors)], ["span", encodeSpan(span)]]);
  }
  if ($match_6.$tag === 4) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const value2 = $match_6.$2;
    const span = $match_6.$3;
    return object([["kind", string("TypeAliasDeclaration")], ["name", string(name)], ["params", list(string)(params)], ["value", encodeTypeExpr(value2)], ["span", encodeSpan(span)]]);
  }
  if ($match_6.$tag === 5) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const span = $match_6.$2;
    return object([["kind", string("OpaqueTypeDeclaration")], ["name", string(name)], ["params", list(string)(params)], ["span", encodeSpan(span)]]);
  }
  if ($match_6.$tag === 6) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const constraints = $match_6.$2;
    const fields = $match_6.$3;
    const span = $match_6.$4;
    return object([["kind", string("TypeDeclaration")], ["name", string(name)], ["params", list(string)(params)], ["constraints", list(encodeConstraint)(constraints)], ["recordFields", list(encodeRecordFieldType)(fields)], ["span", encodeSpan(span)]]);
  }
  if ($match_6.$tag === 7) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const constraints = $match_6.$2;
    const methods = $match_6.$3;
    const span = $match_6.$4;
    return object([["kind", string("ProtocolDeclaration")], ["name", string(name)], ["params", list(string)(params)], ["constraints", list(encodeConstraint)(constraints)], ["methods", list(encodeProtocolMethod)(methods)], ["span", encodeSpan(span)]]);
  }
  if ($match_6.$tag === 8) {
    const constraints = $match_6.$0;
    const protoName = $match_6.$1;
    const typeArgs = $match_6.$2;
    const methods = $match_6.$3;
    const span = $match_6.$4;
    return object([["kind", string("ImplementationDeclaration")], ["constraints", list(encodeConstraint)(constraints)], ["protocolName", string(protoName)], ["typeArgs", list(encodeTypeExpr)(typeArgs)], ["methods", list(encodeMethodImpl)(methods)], ["span", encodeSpan(span)]]);
  }
  if ($match_6.$tag === 9) {
    const assoc = $match_6.$0;
    const prec = $match_6.$1;
    const name = $match_6.$2;
    const span = $match_6.$3;
    return ((fixity) => object([["kind", string("InfixDeclaration")], ["fixity", string(fixity)], ["precedence", int(prec)], ["operator", string(name)], ["span", encodeSpan(span)]]))((($match_7) => {
      if ($match_7 === "left") {
        return "infixl";
      }
      if ($match_7 === "right") {
        return "infixr";
      }
      {
        return "infix";
      }
      throw new Error("Pattern match failed");
    })(assoc));
  }
  throw new Error("Pattern match failed");
})(decl);
var encodeProgram = (prog) => object([["module", encodeModuleDecl(prog.moduleDecl)], ["imports", list(encodeImport)(prog.imports)], ["declarations", list(encodeDeclaration)(prog.declarations)]]);
var parseToJson = (result) => (($match_8) => {
  if ($match_8.$tag === 0) {
    const prog = $match_8.$0;
    return encode(0)(object([["ok", bool(true)], ["program", encodeProgram(prog)]]));
  }
  if ($match_8.$tag === 1) {
    const err = $match_8.$0;
    return encode(0)(object([["ok", bool(false)], ["message", string(err.message)], ["span", encodeSpan(err.span)]]));
  }
  throw new Error("Pattern match failed");
})(result);
var encodeAssociativity = (assoc) => (($match_9) => {
  if ($match_9.$tag === 0) {
    return string("left");
  }
  if ($match_9.$tag === 1) {
    return string("right");
  }
  if ($match_9.$tag === 2) {
    return string("none");
  }
  throw new Error("Pattern match failed");
})(assoc);
var encodeRegistryEntry = (entry) => (($match_10) => {
  {
    const op = $match_10[0];
    const info = $match_10[1];
    return object([["op", string(op)], ["precedence", int(info.precedence)], ["associativity", encodeAssociativity(info.associativity)]]);
  }
  throw new Error("Pattern match failed");
})(entry);
var encodeRegistry = (registry) => list(encodeRegistryEntry)(toList2(registry));

// ../vibe-parser/dist/VibeParser/VibeParser.js
var _AMP_AMP7 = (a) => (b) => a && b();
var _PIPE_PIPE6 = (a) => (b) => a || b();
var parse = (source) => ((rawTokens) => (($match_0) => {
  if ($match_0.$tag === 1) {
    const lexErr = $match_0.$0;
    return Err({ message: lexErr.message, span: { start: lexErr.span.start, end: lexErr.span.end } });
  }
  if ($match_0.$tag === 0) {
    const tokens = $match_0.$0;
    return ((layoutTokens) => parseTokens(layoutTokens)(builtinRegistry))(insertLayoutTokens(tokens));
  }
  throw new Error("Pattern match failed");
})(rawTokens))(lex(source));
var findOperator = (tokens) => (($match_1) => {
  if (Array.isArray($match_1) && $match_1.length === 0) {
    return Nothing;
  }
  if (Array.isArray($match_1) && $match_1.length >= 1) {
    const tok = $match_1[0];
    const rest = $match_1.slice(1);
    return $dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator) ? Just([tok.lexeme, rest]) : $dict_Eq_TokenKind._EQ_EQ(tok.kind)(LParen) ? (($match_2) => {
      if (Array.isArray($match_2) && $match_2.length >= 1) {
        const opTok = $match_2[0];
        const rest2 = $match_2.slice(1);
        return $dict_Eq_TokenKind._EQ_EQ(opTok.kind)(Operator) ? (($match_3) => {
          if (Array.isArray($match_3) && $match_3.length >= 1) {
            const closeParen = $match_3[0];
            const rest3 = $match_3.slice(1);
            return $dict_Eq_TokenKind._EQ_EQ(closeParen.kind)(RParen) ? Just([opTok.lexeme, rest3]) : Nothing;
          }
          {
            return Nothing;
          }
          throw new Error("Pattern match failed");
        })(rest2) : Nothing;
      }
      {
        return Nothing;
      }
      throw new Error("Pattern match failed");
    })(rest) : Nothing;
  }
  throw new Error("Pattern match failed");
})(tokens);
var stringToIntSafe = (s) => (($match_4) => {
  if ($match_4.$tag === 0) {
    const n = $match_4.$0;
    return n;
  }
  if ($match_4.$tag === 1) {
    return 9;
  }
  throw new Error("Pattern match failed");
})(toInt(s));
var collectInfixLoop = (tokens) => (registry) => {
  while (true) {
    {
      const $match_5 = tokens;
      if (Array.isArray($match_5) && $match_5.length === 0) {
        return registry;
      }
      if (Array.isArray($match_5) && $match_5.length >= 1) {
        const tok = $match_5[0];
        const rest = $match_5.slice(1);
        if (_AMP_AMP7($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword))(() => _PIPE_PIPE6($dict_Eq_String._EQ_EQ(tok.lexeme)("infix"))(() => _PIPE_PIPE6($dict_Eq_String._EQ_EQ(tok.lexeme)("infixl"))(() => $dict_Eq_String._EQ_EQ(tok.lexeme)("infixr"))))) {
          {
            const $match_6 = rest;
            if (Array.isArray($match_6) && $match_6.length >= 1) {
              const precTok = $match_6[0];
              const opRest = $match_6.slice(1);
              if ($dict_Eq_TokenKind._EQ_EQ(precTok.kind)(NumberToken)) {
                {
                  const $match_7 = findOperator(opRest);
                  if ($match_7.$tag === 0) {
                    const opName = $match_7.$0[0];
                    const remaining = $match_7.$0[1];
                    {
                      const assoc = (($match_8) => {
                        if ($match_8 === "infixl") {
                          return AssocLeft;
                        }
                        if ($match_8 === "infixr") {
                          return AssocRight;
                        }
                        {
                          return AssocNone;
                        }
                        throw new Error("Pattern match failed");
                      })(tok.lexeme);
                      {
                        const prec = stringToIntSafe(precTok.lexeme);
                        {
                          const newRegistry = insertOperator(opName)({ precedence: prec, associativity: assoc })(registry);
                          [tokens, registry] = [remaining, newRegistry];
                          continue;
                        }
                      }
                    }
                  }
                  if ($match_7.$tag === 1) {
                    [tokens, registry] = [opRest, registry];
                    continue;
                  }
                  throw new Error("Pattern match failed");
                }
              } else {
                [tokens, registry] = [rest, registry];
                continue;
              }
            }
            {
              [tokens, registry] = [rest, registry];
              continue;
            }
            throw new Error("Pattern match failed");
          }
        } else {
          [tokens, registry] = [rest, registry];
          continue;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var collectInfixFromTokens = (tokens) => (registry) => collectInfixLoop(tokens)(registry);
var parseToJson2 = (source) => parseToJson(parse(source));
var encodeError = (message) => (span) => encode(0)(object([["ok", bool(false)], ["message", string(message)], ["span", encodeSpan(span)]]));
var parseWithInfixToJson = (source) => (($match_10) => {
  if ($match_10.$tag === 1) {
    const lexErr = $match_10.$0;
    return encodeError(lexErr.message)({ start: lexErr.span.start, end: lexErr.span.end });
  }
  if ($match_10.$tag === 0) {
    const tokens = $match_10.$0;
    return ((registry) => ((layoutTokens) => (($match_11) => {
      if ($match_11.$tag === 1) {
        const err = $match_11.$0;
        return encodeError(err.message)(err.span);
      }
      if ($match_11.$tag === 0) {
        const program = $match_11.$0;
        return encode(0)(object([["ok", bool(true)], ["program", encodeProgram(program)], ["registry", encodeRegistry(registry)]]));
      }
      throw new Error("Pattern match failed");
    })(parseTokens(layoutTokens)(registry)))(insertLayoutTokens(tokens)))(collectInfixFromTokens(tokens)(builtinRegistry));
  }
  throw new Error("Pattern match failed");
})(lex(source));
var collectInfixToJson = (source) => (($match_12) => {
  if ($match_12.$tag === 1) {
    return encode(0)(object([["registry", list(identity)([])]]));
  }
  if ($match_12.$tag === 0) {
    const tokens = $match_12.$0;
    return ((registry) => encode(0)(object([["registry", encodeRegistry(registry)]])))(collectInfixFromTokens(tokens)(emptyRegistry));
  }
  throw new Error("Pattern match failed");
})(lex(source));
var assocDecoder = ((decode) => andThen(decode)(string2))((s) => (($match_13) => {
  if ($match_13 === "left") {
    return succeed(AssocLeft);
  }
  if ($match_13 === "right") {
    return succeed(AssocRight);
  }
  {
    return succeed(AssocNone);
  }
  throw new Error("Pattern match failed");
})(s));
var registryEntryDecoder = map3((op) => (p) => (a) => [op, { precedence: p, associativity: a }])(field("op")(string2))(field("precedence")(int2))(field("associativity")(assocDecoder));
var registryDecoder = list2(registryEntryDecoder);
var decodeRegistryJson = (json) => (($match_14) => {
  if ($match_14.$tag === 0) {
    const entries = $match_14.$0;
    return Ok(fromList($dict_Ord_String)(entries));
  }
  if ($match_14.$tag === 1) {
    const err = $match_14.$0;
    return Err(err);
  }
  throw new Error("Pattern match failed");
})(decodeString(registryDecoder)(json));
var parseWithRegistryToJson = (registryJson) => (source) => (($match_15) => {
  if ($match_15.$tag === 1) {
    return parseToJson(parse(source));
  }
  if ($match_15.$tag === 0) {
    const externalRegistry = $match_15.$0;
    return ((mergedRegistry) => (($match_16) => {
      if ($match_16.$tag === 1) {
        const lexErr = $match_16.$0;
        return encodeError(lexErr.message)({ start: lexErr.span.start, end: lexErr.span.end });
      }
      if ($match_16.$tag === 0) {
        const tokens = $match_16.$0;
        return ((layoutTokens) => (($match_17) => {
          if ($match_17.$tag === 1) {
            const err = $match_17.$0;
            return encodeError(err.message)(err.span);
          }
          if ($match_17.$tag === 0) {
            const program = $match_17.$0;
            return encode(0)(object([["ok", bool(true)], ["program", encodeProgram(program)]]));
          }
          throw new Error("Pattern match failed");
        })(parseTokens(layoutTokens)(mergedRegistry)))(insertLayoutTokens(tokens));
      }
      throw new Error("Pattern match failed");
    })(lex(source)))(mergeRegistries(builtinRegistry)(externalRegistry));
  }
  throw new Error("Pattern match failed");
})(decodeRegistryJson(registryJson));

// src/index.ts
class ParseError extends Error {
  span;
  filePath;
  constructor(message, span, filePath) {
    super(message);
    this.span = span;
    this.filePath = filePath;
  }
}
var nullReviver = (_key, value2) => value2 === null ? undefined : value2;
function deserializeRegistry(entries) {
  const map2 = new Map;
  for (const entry of entries) {
    map2.set(entry.op, {
      precedence: entry.precedence,
      associativity: entry.associativity
    });
  }
  return map2;
}
function serializeRegistry(registry) {
  const entries = [];
  for (const [op, info] of registry) {
    entries.push({
      op,
      precedence: info.precedence,
      associativity: info.associativity
    });
  }
  return JSON.stringify(entries);
}
function parse2(source, operatorRegistry) {
  let json;
  if (operatorRegistry) {
    const registryJson = serializeRegistry(operatorRegistry);
    json = parseWithRegistryToJson(registryJson)(source);
  } else {
    json = parseToJson2(source);
  }
  const result = JSON.parse(json, nullReviver);
  if (!result.ok) {
    throw new ParseError(result.message, result.span);
  }
  const program = result.program;
  if (program.module && program.module.exposing === undefined) {
    program.module.exposing = null;
  }
  return program;
}
function parseWithInfix(source) {
  const json = parseWithInfixToJson(source);
  const result = JSON.parse(json, nullReviver);
  if (!result.ok) {
    throw new ParseError(result.message, result.span);
  }
  const program = result.program;
  if (program.module && program.module.exposing === undefined) {
    program.module.exposing = null;
  }
  return {
    program,
    operatorRegistry: deserializeRegistry(result.registry),
    infixErrors: []
  };
}
function collectInfixDeclarations(source) {
  const json = collectInfixToJson(source);
  const result = JSON.parse(json, nullReviver);
  return {
    registry: deserializeRegistry(result.registry),
    declarations: [],
    errors: []
  };
}
export {
  parseWithInfix,
  parse2 as parse,
  collectInfixDeclarations,
  ParseError
};
