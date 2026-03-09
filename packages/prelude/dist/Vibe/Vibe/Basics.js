// Values
const _PIPE_GT = (x) => (f) => f(x);
const _LT_PIPE = (f) => (x) => f(x);
const _GT_GT = (f) => (g) => (x) => g(f(x));
const _LT_LT = (g) => (f) => (x) => g(f(x));
const not = (b) => (b ? false : true);
const identity = (x) => x;
const always = (x) => (_) => x;
const flip = (f) => (x) => (y) => f(y)(x);
const apply = (f) => (x) => f(x);

export { _GT_GT, _LT_LT, _LT_PIPE, _PIPE_GT, always, apply, flip, identity, not };