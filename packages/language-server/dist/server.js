import { createRequire } from "node:module";
var __create = Object.create;
var __getProtoOf = Object.getPrototypeOf;
var __defProp = Object.defineProperty;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __hasOwnProp = Object.prototype.hasOwnProperty;
function __accessProp(key) {
  return this[key];
}
var __toESMCache_node;
var __toESMCache_esm;
var __toESM = (mod, isNodeMode, target) => {
  var canCache = mod != null && typeof mod === "object";
  if (canCache) {
    var cache = isNodeMode ? __toESMCache_node ??= new WeakMap : __toESMCache_esm ??= new WeakMap;
    var cached = cache.get(mod);
    if (cached)
      return cached;
  }
  target = mod != null ? __create(__getProtoOf(mod)) : {};
  const to = isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target;
  for (let key of __getOwnPropNames(mod))
    if (!__hasOwnProp.call(to, key))
      __defProp(to, key, {
        get: __accessProp.bind(mod, key),
        enumerable: true
      });
  if (canCache)
    cache.set(mod, to);
  return to;
};
var __commonJS = (cb, mod) => () => (mod || cb((mod = { exports: {} }).exports, mod), mod.exports);
var __require = /* @__PURE__ */ createRequire(import.meta.url);

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/utils/is.js
var require_is = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.thenable = exports.typedArray = exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = undefined;
  function boolean(value) {
    return value === true || value === false;
  }
  exports.boolean = boolean;
  function string(value) {
    return typeof value === "string" || value instanceof String;
  }
  exports.string = string;
  function number(value) {
    return typeof value === "number" || value instanceof Number;
  }
  exports.number = number;
  function error(value) {
    return value instanceof Error;
  }
  exports.error = error;
  function func(value) {
    return typeof value === "function";
  }
  exports.func = func;
  function array(value) {
    return Array.isArray(value);
  }
  exports.array = array;
  function stringArray(value) {
    return array(value) && value.every((elem) => string(elem));
  }
  exports.stringArray = stringArray;
  function typedArray(value, check) {
    return Array.isArray(value) && value.every(check);
  }
  exports.typedArray = typedArray;
  function thenable(value) {
    return value && func(value.then);
  }
  exports.thenable = thenable;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/is.js
var require_is2 = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = undefined;
  function boolean(value) {
    return value === true || value === false;
  }
  exports.boolean = boolean;
  function string(value) {
    return typeof value === "string" || value instanceof String;
  }
  exports.string = string;
  function number(value) {
    return typeof value === "number" || value instanceof Number;
  }
  exports.number = number;
  function error(value) {
    return value instanceof Error;
  }
  exports.error = error;
  function func(value) {
    return typeof value === "function";
  }
  exports.func = func;
  function array(value) {
    return Array.isArray(value);
  }
  exports.array = array;
  function stringArray(value) {
    return array(value) && value.every((elem) => string(elem));
  }
  exports.stringArray = stringArray;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/messages.js
var require_messages = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.Message = exports.NotificationType9 = exports.NotificationType8 = exports.NotificationType7 = exports.NotificationType6 = exports.NotificationType5 = exports.NotificationType4 = exports.NotificationType3 = exports.NotificationType2 = exports.NotificationType1 = exports.NotificationType0 = exports.NotificationType = exports.RequestType9 = exports.RequestType8 = exports.RequestType7 = exports.RequestType6 = exports.RequestType5 = exports.RequestType4 = exports.RequestType3 = exports.RequestType2 = exports.RequestType1 = exports.RequestType = exports.RequestType0 = exports.AbstractMessageSignature = exports.ParameterStructures = exports.ResponseError = exports.ErrorCodes = undefined;
  var is = require_is2();
  var ErrorCodes;
  (function(ErrorCodes2) {
    ErrorCodes2.ParseError = -32700;
    ErrorCodes2.InvalidRequest = -32600;
    ErrorCodes2.MethodNotFound = -32601;
    ErrorCodes2.InvalidParams = -32602;
    ErrorCodes2.InternalError = -32603;
    ErrorCodes2.jsonrpcReservedErrorRangeStart = -32099;
    ErrorCodes2.serverErrorStart = -32099;
    ErrorCodes2.MessageWriteError = -32099;
    ErrorCodes2.MessageReadError = -32098;
    ErrorCodes2.PendingResponseRejected = -32097;
    ErrorCodes2.ConnectionInactive = -32096;
    ErrorCodes2.ServerNotInitialized = -32002;
    ErrorCodes2.UnknownErrorCode = -32001;
    ErrorCodes2.jsonrpcReservedErrorRangeEnd = -32000;
    ErrorCodes2.serverErrorEnd = -32000;
  })(ErrorCodes || (exports.ErrorCodes = ErrorCodes = {}));

  class ResponseError extends Error {
    constructor(code, message, data) {
      super(message);
      this.code = is.number(code) ? code : ErrorCodes.UnknownErrorCode;
      this.data = data;
      Object.setPrototypeOf(this, ResponseError.prototype);
    }
    toJson() {
      const result = {
        code: this.code,
        message: this.message
      };
      if (this.data !== undefined) {
        result.data = this.data;
      }
      return result;
    }
  }
  exports.ResponseError = ResponseError;

  class ParameterStructures {
    constructor(kind) {
      this.kind = kind;
    }
    static is(value) {
      return value === ParameterStructures.auto || value === ParameterStructures.byName || value === ParameterStructures.byPosition;
    }
    toString() {
      return this.kind;
    }
  }
  exports.ParameterStructures = ParameterStructures;
  ParameterStructures.auto = new ParameterStructures("auto");
  ParameterStructures.byPosition = new ParameterStructures("byPosition");
  ParameterStructures.byName = new ParameterStructures("byName");

  class AbstractMessageSignature {
    constructor(method, numberOfParams) {
      this.method = method;
      this.numberOfParams = numberOfParams;
    }
    get parameterStructures() {
      return ParameterStructures.auto;
    }
  }
  exports.AbstractMessageSignature = AbstractMessageSignature;

  class RequestType0 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 0);
    }
  }
  exports.RequestType0 = RequestType0;

  class RequestType extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
      super(method, 1);
      this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
      return this._parameterStructures;
    }
  }
  exports.RequestType = RequestType;

  class RequestType1 extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
      super(method, 1);
      this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
      return this._parameterStructures;
    }
  }
  exports.RequestType1 = RequestType1;

  class RequestType2 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 2);
    }
  }
  exports.RequestType2 = RequestType2;

  class RequestType3 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 3);
    }
  }
  exports.RequestType3 = RequestType3;

  class RequestType4 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 4);
    }
  }
  exports.RequestType4 = RequestType4;

  class RequestType5 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 5);
    }
  }
  exports.RequestType5 = RequestType5;

  class RequestType6 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 6);
    }
  }
  exports.RequestType6 = RequestType6;

  class RequestType7 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 7);
    }
  }
  exports.RequestType7 = RequestType7;

  class RequestType8 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 8);
    }
  }
  exports.RequestType8 = RequestType8;

  class RequestType9 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 9);
    }
  }
  exports.RequestType9 = RequestType9;

  class NotificationType extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
      super(method, 1);
      this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
      return this._parameterStructures;
    }
  }
  exports.NotificationType = NotificationType;

  class NotificationType0 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 0);
    }
  }
  exports.NotificationType0 = NotificationType0;

  class NotificationType1 extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
      super(method, 1);
      this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
      return this._parameterStructures;
    }
  }
  exports.NotificationType1 = NotificationType1;

  class NotificationType2 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 2);
    }
  }
  exports.NotificationType2 = NotificationType2;

  class NotificationType3 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 3);
    }
  }
  exports.NotificationType3 = NotificationType3;

  class NotificationType4 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 4);
    }
  }
  exports.NotificationType4 = NotificationType4;

  class NotificationType5 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 5);
    }
  }
  exports.NotificationType5 = NotificationType5;

  class NotificationType6 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 6);
    }
  }
  exports.NotificationType6 = NotificationType6;

  class NotificationType7 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 7);
    }
  }
  exports.NotificationType7 = NotificationType7;

  class NotificationType8 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 8);
    }
  }
  exports.NotificationType8 = NotificationType8;

  class NotificationType9 extends AbstractMessageSignature {
    constructor(method) {
      super(method, 9);
    }
  }
  exports.NotificationType9 = NotificationType9;
  var Message;
  (function(Message2) {
    function isRequest(message) {
      const candidate = message;
      return candidate && is.string(candidate.method) && (is.string(candidate.id) || is.number(candidate.id));
    }
    Message2.isRequest = isRequest;
    function isNotification(message) {
      const candidate = message;
      return candidate && is.string(candidate.method) && message.id === undefined;
    }
    Message2.isNotification = isNotification;
    function isResponse(message) {
      const candidate = message;
      return candidate && (candidate.result !== undefined || !!candidate.error) && (is.string(candidate.id) || is.number(candidate.id) || candidate.id === null);
    }
    Message2.isResponse = isResponse;
  })(Message || (exports.Message = Message = {}));
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/linkedMap.js
var require_linkedMap = __commonJS((exports) => {
  var _a;
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.LRUCache = exports.LinkedMap = exports.Touch = undefined;
  var Touch;
  (function(Touch2) {
    Touch2.None = 0;
    Touch2.First = 1;
    Touch2.AsOld = Touch2.First;
    Touch2.Last = 2;
    Touch2.AsNew = Touch2.Last;
  })(Touch || (exports.Touch = Touch = {}));

  class LinkedMap {
    constructor() {
      this[_a] = "LinkedMap";
      this._map = new Map;
      this._head = undefined;
      this._tail = undefined;
      this._size = 0;
      this._state = 0;
    }
    clear() {
      this._map.clear();
      this._head = undefined;
      this._tail = undefined;
      this._size = 0;
      this._state++;
    }
    isEmpty() {
      return !this._head && !this._tail;
    }
    get size() {
      return this._size;
    }
    get first() {
      return this._head?.value;
    }
    get last() {
      return this._tail?.value;
    }
    has(key) {
      return this._map.has(key);
    }
    get(key, touch = Touch.None) {
      const item = this._map.get(key);
      if (!item) {
        return;
      }
      if (touch !== Touch.None) {
        this.touch(item, touch);
      }
      return item.value;
    }
    set(key, value, touch = Touch.None) {
      let item = this._map.get(key);
      if (item) {
        item.value = value;
        if (touch !== Touch.None) {
          this.touch(item, touch);
        }
      } else {
        item = { key, value, next: undefined, previous: undefined };
        switch (touch) {
          case Touch.None:
            this.addItemLast(item);
            break;
          case Touch.First:
            this.addItemFirst(item);
            break;
          case Touch.Last:
            this.addItemLast(item);
            break;
          default:
            this.addItemLast(item);
            break;
        }
        this._map.set(key, item);
        this._size++;
      }
      return this;
    }
    delete(key) {
      return !!this.remove(key);
    }
    remove(key) {
      const item = this._map.get(key);
      if (!item) {
        return;
      }
      this._map.delete(key);
      this.removeItem(item);
      this._size--;
      return item.value;
    }
    shift() {
      if (!this._head && !this._tail) {
        return;
      }
      if (!this._head || !this._tail) {
        throw new Error("Invalid list");
      }
      const item = this._head;
      this._map.delete(item.key);
      this.removeItem(item);
      this._size--;
      return item.value;
    }
    forEach(callbackfn, thisArg) {
      const state = this._state;
      let current = this._head;
      while (current) {
        if (thisArg) {
          callbackfn.bind(thisArg)(current.value, current.key, this);
        } else {
          callbackfn(current.value, current.key, this);
        }
        if (this._state !== state) {
          throw new Error(`LinkedMap got modified during iteration.`);
        }
        current = current.next;
      }
    }
    keys() {
      const state = this._state;
      let current = this._head;
      const iterator = {
        [Symbol.iterator]: () => {
          return iterator;
        },
        next: () => {
          if (this._state !== state) {
            throw new Error(`LinkedMap got modified during iteration.`);
          }
          if (current) {
            const result = { value: current.key, done: false };
            current = current.next;
            return result;
          } else {
            return { value: undefined, done: true };
          }
        }
      };
      return iterator;
    }
    values() {
      const state = this._state;
      let current = this._head;
      const iterator = {
        [Symbol.iterator]: () => {
          return iterator;
        },
        next: () => {
          if (this._state !== state) {
            throw new Error(`LinkedMap got modified during iteration.`);
          }
          if (current) {
            const result = { value: current.value, done: false };
            current = current.next;
            return result;
          } else {
            return { value: undefined, done: true };
          }
        }
      };
      return iterator;
    }
    entries() {
      const state = this._state;
      let current = this._head;
      const iterator = {
        [Symbol.iterator]: () => {
          return iterator;
        },
        next: () => {
          if (this._state !== state) {
            throw new Error(`LinkedMap got modified during iteration.`);
          }
          if (current) {
            const result = { value: [current.key, current.value], done: false };
            current = current.next;
            return result;
          } else {
            return { value: undefined, done: true };
          }
        }
      };
      return iterator;
    }
    [(_a = Symbol.toStringTag, Symbol.iterator)]() {
      return this.entries();
    }
    trimOld(newSize) {
      if (newSize >= this.size) {
        return;
      }
      if (newSize === 0) {
        this.clear();
        return;
      }
      let current = this._head;
      let currentSize = this.size;
      while (current && currentSize > newSize) {
        this._map.delete(current.key);
        current = current.next;
        currentSize--;
      }
      this._head = current;
      this._size = currentSize;
      if (current) {
        current.previous = undefined;
      }
      this._state++;
    }
    addItemFirst(item) {
      if (!this._head && !this._tail) {
        this._tail = item;
      } else if (!this._head) {
        throw new Error("Invalid list");
      } else {
        item.next = this._head;
        this._head.previous = item;
      }
      this._head = item;
      this._state++;
    }
    addItemLast(item) {
      if (!this._head && !this._tail) {
        this._head = item;
      } else if (!this._tail) {
        throw new Error("Invalid list");
      } else {
        item.previous = this._tail;
        this._tail.next = item;
      }
      this._tail = item;
      this._state++;
    }
    removeItem(item) {
      if (item === this._head && item === this._tail) {
        this._head = undefined;
        this._tail = undefined;
      } else if (item === this._head) {
        if (!item.next) {
          throw new Error("Invalid list");
        }
        item.next.previous = undefined;
        this._head = item.next;
      } else if (item === this._tail) {
        if (!item.previous) {
          throw new Error("Invalid list");
        }
        item.previous.next = undefined;
        this._tail = item.previous;
      } else {
        const next = item.next;
        const previous = item.previous;
        if (!next || !previous) {
          throw new Error("Invalid list");
        }
        next.previous = previous;
        previous.next = next;
      }
      item.next = undefined;
      item.previous = undefined;
      this._state++;
    }
    touch(item, touch) {
      if (!this._head || !this._tail) {
        throw new Error("Invalid list");
      }
      if (touch !== Touch.First && touch !== Touch.Last) {
        return;
      }
      if (touch === Touch.First) {
        if (item === this._head) {
          return;
        }
        const next = item.next;
        const previous = item.previous;
        if (item === this._tail) {
          previous.next = undefined;
          this._tail = previous;
        } else {
          next.previous = previous;
          previous.next = next;
        }
        item.previous = undefined;
        item.next = this._head;
        this._head.previous = item;
        this._head = item;
        this._state++;
      } else if (touch === Touch.Last) {
        if (item === this._tail) {
          return;
        }
        const next = item.next;
        const previous = item.previous;
        if (item === this._head) {
          next.previous = undefined;
          this._head = next;
        } else {
          next.previous = previous;
          previous.next = next;
        }
        item.next = undefined;
        item.previous = this._tail;
        this._tail.next = item;
        this._tail = item;
        this._state++;
      }
    }
    toJSON() {
      const data = [];
      this.forEach((value, key) => {
        data.push([key, value]);
      });
      return data;
    }
    fromJSON(data) {
      this.clear();
      for (const [key, value] of data) {
        this.set(key, value);
      }
    }
  }
  exports.LinkedMap = LinkedMap;

  class LRUCache extends LinkedMap {
    constructor(limit, ratio = 1) {
      super();
      this._limit = limit;
      this._ratio = Math.min(Math.max(0, ratio), 1);
    }
    get limit() {
      return this._limit;
    }
    set limit(limit) {
      this._limit = limit;
      this.checkTrim();
    }
    get ratio() {
      return this._ratio;
    }
    set ratio(ratio) {
      this._ratio = Math.min(Math.max(0, ratio), 1);
      this.checkTrim();
    }
    get(key, touch = Touch.AsNew) {
      return super.get(key, touch);
    }
    peek(key) {
      return super.get(key, Touch.None);
    }
    set(key, value) {
      super.set(key, value, Touch.Last);
      this.checkTrim();
      return this;
    }
    checkTrim() {
      if (this.size > this._limit) {
        this.trimOld(Math.round(this._limit * this._ratio));
      }
    }
  }
  exports.LRUCache = LRUCache;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/disposable.js
var require_disposable = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.Disposable = undefined;
  var Disposable;
  (function(Disposable2) {
    function create(func) {
      return {
        dispose: func
      };
    }
    Disposable2.create = create;
  })(Disposable || (exports.Disposable = Disposable = {}));
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/ral.js
var require_ral = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  var _ral;
  function RAL() {
    if (_ral === undefined) {
      throw new Error(`No runtime abstraction layer installed`);
    }
    return _ral;
  }
  (function(RAL2) {
    function install(ral) {
      if (ral === undefined) {
        throw new Error(`No runtime abstraction layer provided`);
      }
      _ral = ral;
    }
    RAL2.install = install;
  })(RAL || (RAL = {}));
  exports.default = RAL;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/events.js
var require_events = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.Emitter = exports.Event = undefined;
  var ral_1 = require_ral();
  var Event;
  (function(Event2) {
    const _disposable = { dispose() {} };
    Event2.None = function() {
      return _disposable;
    };
  })(Event || (exports.Event = Event = {}));

  class CallbackList {
    add(callback, context = null, bucket) {
      if (!this._callbacks) {
        this._callbacks = [];
        this._contexts = [];
      }
      this._callbacks.push(callback);
      this._contexts.push(context);
      if (Array.isArray(bucket)) {
        bucket.push({ dispose: () => this.remove(callback, context) });
      }
    }
    remove(callback, context = null) {
      if (!this._callbacks) {
        return;
      }
      let foundCallbackWithDifferentContext = false;
      for (let i = 0, len = this._callbacks.length;i < len; i++) {
        if (this._callbacks[i] === callback) {
          if (this._contexts[i] === context) {
            this._callbacks.splice(i, 1);
            this._contexts.splice(i, 1);
            return;
          } else {
            foundCallbackWithDifferentContext = true;
          }
        }
      }
      if (foundCallbackWithDifferentContext) {
        throw new Error("When adding a listener with a context, you should remove it with the same context");
      }
    }
    invoke(...args) {
      if (!this._callbacks) {
        return [];
      }
      const ret = [], callbacks = this._callbacks.slice(0), contexts = this._contexts.slice(0);
      for (let i = 0, len = callbacks.length;i < len; i++) {
        try {
          ret.push(callbacks[i].apply(contexts[i], args));
        } catch (e) {
          (0, ral_1.default)().console.error(e);
        }
      }
      return ret;
    }
    isEmpty() {
      return !this._callbacks || this._callbacks.length === 0;
    }
    dispose() {
      this._callbacks = undefined;
      this._contexts = undefined;
    }
  }

  class Emitter {
    constructor(_options) {
      this._options = _options;
    }
    get event() {
      if (!this._event) {
        this._event = (listener, thisArgs, disposables) => {
          if (!this._callbacks) {
            this._callbacks = new CallbackList;
          }
          if (this._options && this._options.onFirstListenerAdd && this._callbacks.isEmpty()) {
            this._options.onFirstListenerAdd(this);
          }
          this._callbacks.add(listener, thisArgs);
          const result = {
            dispose: () => {
              if (!this._callbacks) {
                return;
              }
              this._callbacks.remove(listener, thisArgs);
              result.dispose = Emitter._noop;
              if (this._options && this._options.onLastListenerRemove && this._callbacks.isEmpty()) {
                this._options.onLastListenerRemove(this);
              }
            }
          };
          if (Array.isArray(disposables)) {
            disposables.push(result);
          }
          return result;
        };
      }
      return this._event;
    }
    fire(event) {
      if (this._callbacks) {
        this._callbacks.invoke.call(this._callbacks, event);
      }
    }
    dispose() {
      if (this._callbacks) {
        this._callbacks.dispose();
        this._callbacks = undefined;
      }
    }
  }
  exports.Emitter = Emitter;
  Emitter._noop = function() {};
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/cancellation.js
var require_cancellation = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.CancellationTokenSource = exports.CancellationToken = undefined;
  var ral_1 = require_ral();
  var Is = require_is2();
  var events_1 = require_events();
  var CancellationToken;
  (function(CancellationToken2) {
    CancellationToken2.None = Object.freeze({
      isCancellationRequested: false,
      onCancellationRequested: events_1.Event.None
    });
    CancellationToken2.Cancelled = Object.freeze({
      isCancellationRequested: true,
      onCancellationRequested: events_1.Event.None
    });
    function is(value) {
      const candidate = value;
      return candidate && (candidate === CancellationToken2.None || candidate === CancellationToken2.Cancelled || Is.boolean(candidate.isCancellationRequested) && !!candidate.onCancellationRequested);
    }
    CancellationToken2.is = is;
  })(CancellationToken || (exports.CancellationToken = CancellationToken = {}));
  var shortcutEvent = Object.freeze(function(callback, context) {
    const handle = (0, ral_1.default)().timer.setTimeout(callback.bind(context), 0);
    return { dispose() {
      handle.dispose();
    } };
  });

  class MutableToken {
    constructor() {
      this._isCancelled = false;
    }
    cancel() {
      if (!this._isCancelled) {
        this._isCancelled = true;
        if (this._emitter) {
          this._emitter.fire(undefined);
          this.dispose();
        }
      }
    }
    get isCancellationRequested() {
      return this._isCancelled;
    }
    get onCancellationRequested() {
      if (this._isCancelled) {
        return shortcutEvent;
      }
      if (!this._emitter) {
        this._emitter = new events_1.Emitter;
      }
      return this._emitter.event;
    }
    dispose() {
      if (this._emitter) {
        this._emitter.dispose();
        this._emitter = undefined;
      }
    }
  }

  class CancellationTokenSource {
    get token() {
      if (!this._token) {
        this._token = new MutableToken;
      }
      return this._token;
    }
    cancel() {
      if (!this._token) {
        this._token = CancellationToken.Cancelled;
      } else {
        this._token.cancel();
      }
    }
    dispose() {
      if (!this._token) {
        this._token = CancellationToken.None;
      } else if (this._token instanceof MutableToken) {
        this._token.dispose();
      }
    }
  }
  exports.CancellationTokenSource = CancellationTokenSource;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/sharedArrayCancellation.js
var require_sharedArrayCancellation = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.SharedArrayReceiverStrategy = exports.SharedArraySenderStrategy = undefined;
  var cancellation_1 = require_cancellation();
  var CancellationState;
  (function(CancellationState2) {
    CancellationState2.Continue = 0;
    CancellationState2.Cancelled = 1;
  })(CancellationState || (CancellationState = {}));

  class SharedArraySenderStrategy {
    constructor() {
      this.buffers = new Map;
    }
    enableCancellation(request) {
      if (request.id === null) {
        return;
      }
      const buffer = new SharedArrayBuffer(4);
      const data = new Int32Array(buffer, 0, 1);
      data[0] = CancellationState.Continue;
      this.buffers.set(request.id, buffer);
      request.$cancellationData = buffer;
    }
    async sendCancellation(_conn, id) {
      const buffer = this.buffers.get(id);
      if (buffer === undefined) {
        return;
      }
      const data = new Int32Array(buffer, 0, 1);
      Atomics.store(data, 0, CancellationState.Cancelled);
    }
    cleanup(id) {
      this.buffers.delete(id);
    }
    dispose() {
      this.buffers.clear();
    }
  }
  exports.SharedArraySenderStrategy = SharedArraySenderStrategy;

  class SharedArrayBufferCancellationToken {
    constructor(buffer) {
      this.data = new Int32Array(buffer, 0, 1);
    }
    get isCancellationRequested() {
      return Atomics.load(this.data, 0) === CancellationState.Cancelled;
    }
    get onCancellationRequested() {
      throw new Error(`Cancellation over SharedArrayBuffer doesn't support cancellation events`);
    }
  }

  class SharedArrayBufferCancellationTokenSource {
    constructor(buffer) {
      this.token = new SharedArrayBufferCancellationToken(buffer);
    }
    cancel() {}
    dispose() {}
  }

  class SharedArrayReceiverStrategy {
    constructor() {
      this.kind = "request";
    }
    createCancellationTokenSource(request) {
      const buffer = request.$cancellationData;
      if (buffer === undefined) {
        return new cancellation_1.CancellationTokenSource;
      }
      return new SharedArrayBufferCancellationTokenSource(buffer);
    }
  }
  exports.SharedArrayReceiverStrategy = SharedArrayReceiverStrategy;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/semaphore.js
var require_semaphore = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.Semaphore = undefined;
  var ral_1 = require_ral();

  class Semaphore {
    constructor(capacity = 1) {
      if (capacity <= 0) {
        throw new Error("Capacity must be greater than 0");
      }
      this._capacity = capacity;
      this._active = 0;
      this._waiting = [];
    }
    lock(thunk) {
      return new Promise((resolve, reject) => {
        this._waiting.push({ thunk, resolve, reject });
        this.runNext();
      });
    }
    get active() {
      return this._active;
    }
    runNext() {
      if (this._waiting.length === 0 || this._active === this._capacity) {
        return;
      }
      (0, ral_1.default)().timer.setImmediate(() => this.doRunNext());
    }
    doRunNext() {
      if (this._waiting.length === 0 || this._active === this._capacity) {
        return;
      }
      const next = this._waiting.shift();
      this._active++;
      if (this._active > this._capacity) {
        throw new Error(`To many thunks active`);
      }
      try {
        const result = next.thunk();
        if (result instanceof Promise) {
          result.then((value) => {
            this._active--;
            next.resolve(value);
            this.runNext();
          }, (err) => {
            this._active--;
            next.reject(err);
            this.runNext();
          });
        } else {
          this._active--;
          next.resolve(result);
          this.runNext();
        }
      } catch (err) {
        this._active--;
        next.reject(err);
        this.runNext();
      }
    }
  }
  exports.Semaphore = Semaphore;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/messageReader.js
var require_messageReader = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ReadableStreamMessageReader = exports.AbstractMessageReader = exports.MessageReader = undefined;
  var ral_1 = require_ral();
  var Is = require_is2();
  var events_1 = require_events();
  var semaphore_1 = require_semaphore();
  var MessageReader;
  (function(MessageReader2) {
    function is(value) {
      let candidate = value;
      return candidate && Is.func(candidate.listen) && Is.func(candidate.dispose) && Is.func(candidate.onError) && Is.func(candidate.onClose) && Is.func(candidate.onPartialMessage);
    }
    MessageReader2.is = is;
  })(MessageReader || (exports.MessageReader = MessageReader = {}));

  class AbstractMessageReader {
    constructor() {
      this.errorEmitter = new events_1.Emitter;
      this.closeEmitter = new events_1.Emitter;
      this.partialMessageEmitter = new events_1.Emitter;
    }
    dispose() {
      this.errorEmitter.dispose();
      this.closeEmitter.dispose();
    }
    get onError() {
      return this.errorEmitter.event;
    }
    fireError(error) {
      this.errorEmitter.fire(this.asError(error));
    }
    get onClose() {
      return this.closeEmitter.event;
    }
    fireClose() {
      this.closeEmitter.fire(undefined);
    }
    get onPartialMessage() {
      return this.partialMessageEmitter.event;
    }
    firePartialMessage(info) {
      this.partialMessageEmitter.fire(info);
    }
    asError(error) {
      if (error instanceof Error) {
        return error;
      } else {
        return new Error(`Reader received error. Reason: ${Is.string(error.message) ? error.message : "unknown"}`);
      }
    }
  }
  exports.AbstractMessageReader = AbstractMessageReader;
  var ResolvedMessageReaderOptions;
  (function(ResolvedMessageReaderOptions2) {
    function fromOptions(options) {
      let charset;
      let result;
      let contentDecoder;
      const contentDecoders = new Map;
      let contentTypeDecoder;
      const contentTypeDecoders = new Map;
      if (options === undefined || typeof options === "string") {
        charset = options ?? "utf-8";
      } else {
        charset = options.charset ?? "utf-8";
        if (options.contentDecoder !== undefined) {
          contentDecoder = options.contentDecoder;
          contentDecoders.set(contentDecoder.name, contentDecoder);
        }
        if (options.contentDecoders !== undefined) {
          for (const decoder of options.contentDecoders) {
            contentDecoders.set(decoder.name, decoder);
          }
        }
        if (options.contentTypeDecoder !== undefined) {
          contentTypeDecoder = options.contentTypeDecoder;
          contentTypeDecoders.set(contentTypeDecoder.name, contentTypeDecoder);
        }
        if (options.contentTypeDecoders !== undefined) {
          for (const decoder of options.contentTypeDecoders) {
            contentTypeDecoders.set(decoder.name, decoder);
          }
        }
      }
      if (contentTypeDecoder === undefined) {
        contentTypeDecoder = (0, ral_1.default)().applicationJson.decoder;
        contentTypeDecoders.set(contentTypeDecoder.name, contentTypeDecoder);
      }
      return { charset, contentDecoder, contentDecoders, contentTypeDecoder, contentTypeDecoders };
    }
    ResolvedMessageReaderOptions2.fromOptions = fromOptions;
  })(ResolvedMessageReaderOptions || (ResolvedMessageReaderOptions = {}));

  class ReadableStreamMessageReader extends AbstractMessageReader {
    constructor(readable, options) {
      super();
      this.readable = readable;
      this.options = ResolvedMessageReaderOptions.fromOptions(options);
      this.buffer = (0, ral_1.default)().messageBuffer.create(this.options.charset);
      this._partialMessageTimeout = 1e4;
      this.nextMessageLength = -1;
      this.messageToken = 0;
      this.readSemaphore = new semaphore_1.Semaphore(1);
    }
    set partialMessageTimeout(timeout) {
      this._partialMessageTimeout = timeout;
    }
    get partialMessageTimeout() {
      return this._partialMessageTimeout;
    }
    listen(callback) {
      this.nextMessageLength = -1;
      this.messageToken = 0;
      this.partialMessageTimer = undefined;
      this.callback = callback;
      const result = this.readable.onData((data) => {
        this.onData(data);
      });
      this.readable.onError((error) => this.fireError(error));
      this.readable.onClose(() => this.fireClose());
      return result;
    }
    onData(data) {
      try {
        this.buffer.append(data);
        while (true) {
          if (this.nextMessageLength === -1) {
            const headers = this.buffer.tryReadHeaders(true);
            if (!headers) {
              return;
            }
            const contentLength = headers.get("content-length");
            if (!contentLength) {
              this.fireError(new Error(`Header must provide a Content-Length property.
${JSON.stringify(Object.fromEntries(headers))}`));
              return;
            }
            const length = parseInt(contentLength);
            if (isNaN(length)) {
              this.fireError(new Error(`Content-Length value must be a number. Got ${contentLength}`));
              return;
            }
            this.nextMessageLength = length;
          }
          const body = this.buffer.tryReadBody(this.nextMessageLength);
          if (body === undefined) {
            this.setPartialMessageTimer();
            return;
          }
          this.clearPartialMessageTimer();
          this.nextMessageLength = -1;
          this.readSemaphore.lock(async () => {
            const bytes = this.options.contentDecoder !== undefined ? await this.options.contentDecoder.decode(body) : body;
            const message = await this.options.contentTypeDecoder.decode(bytes, this.options);
            this.callback(message);
          }).catch((error) => {
            this.fireError(error);
          });
        }
      } catch (error) {
        this.fireError(error);
      }
    }
    clearPartialMessageTimer() {
      if (this.partialMessageTimer) {
        this.partialMessageTimer.dispose();
        this.partialMessageTimer = undefined;
      }
    }
    setPartialMessageTimer() {
      this.clearPartialMessageTimer();
      if (this._partialMessageTimeout <= 0) {
        return;
      }
      this.partialMessageTimer = (0, ral_1.default)().timer.setTimeout((token, timeout) => {
        this.partialMessageTimer = undefined;
        if (token === this.messageToken) {
          this.firePartialMessage({ messageToken: token, waitingTime: timeout });
          this.setPartialMessageTimer();
        }
      }, this._partialMessageTimeout, this.messageToken, this._partialMessageTimeout);
    }
  }
  exports.ReadableStreamMessageReader = ReadableStreamMessageReader;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/messageWriter.js
var require_messageWriter = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.WriteableStreamMessageWriter = exports.AbstractMessageWriter = exports.MessageWriter = undefined;
  var ral_1 = require_ral();
  var Is = require_is2();
  var semaphore_1 = require_semaphore();
  var events_1 = require_events();
  var ContentLength = "Content-Length: ";
  var CRLF = `\r
`;
  var MessageWriter;
  (function(MessageWriter2) {
    function is(value) {
      let candidate = value;
      return candidate && Is.func(candidate.dispose) && Is.func(candidate.onClose) && Is.func(candidate.onError) && Is.func(candidate.write);
    }
    MessageWriter2.is = is;
  })(MessageWriter || (exports.MessageWriter = MessageWriter = {}));

  class AbstractMessageWriter {
    constructor() {
      this.errorEmitter = new events_1.Emitter;
      this.closeEmitter = new events_1.Emitter;
    }
    dispose() {
      this.errorEmitter.dispose();
      this.closeEmitter.dispose();
    }
    get onError() {
      return this.errorEmitter.event;
    }
    fireError(error, message, count) {
      this.errorEmitter.fire([this.asError(error), message, count]);
    }
    get onClose() {
      return this.closeEmitter.event;
    }
    fireClose() {
      this.closeEmitter.fire(undefined);
    }
    asError(error) {
      if (error instanceof Error) {
        return error;
      } else {
        return new Error(`Writer received error. Reason: ${Is.string(error.message) ? error.message : "unknown"}`);
      }
    }
  }
  exports.AbstractMessageWriter = AbstractMessageWriter;
  var ResolvedMessageWriterOptions;
  (function(ResolvedMessageWriterOptions2) {
    function fromOptions(options) {
      if (options === undefined || typeof options === "string") {
        return { charset: options ?? "utf-8", contentTypeEncoder: (0, ral_1.default)().applicationJson.encoder };
      } else {
        return { charset: options.charset ?? "utf-8", contentEncoder: options.contentEncoder, contentTypeEncoder: options.contentTypeEncoder ?? (0, ral_1.default)().applicationJson.encoder };
      }
    }
    ResolvedMessageWriterOptions2.fromOptions = fromOptions;
  })(ResolvedMessageWriterOptions || (ResolvedMessageWriterOptions = {}));

  class WriteableStreamMessageWriter extends AbstractMessageWriter {
    constructor(writable, options) {
      super();
      this.writable = writable;
      this.options = ResolvedMessageWriterOptions.fromOptions(options);
      this.errorCount = 0;
      this.writeSemaphore = new semaphore_1.Semaphore(1);
      this.writable.onError((error) => this.fireError(error));
      this.writable.onClose(() => this.fireClose());
    }
    async write(msg) {
      return this.writeSemaphore.lock(async () => {
        const payload = this.options.contentTypeEncoder.encode(msg, this.options).then((buffer) => {
          if (this.options.contentEncoder !== undefined) {
            return this.options.contentEncoder.encode(buffer);
          } else {
            return buffer;
          }
        });
        return payload.then((buffer) => {
          const headers = [];
          headers.push(ContentLength, buffer.byteLength.toString(), CRLF);
          headers.push(CRLF);
          return this.doWrite(msg, headers, buffer);
        }, (error) => {
          this.fireError(error);
          throw error;
        });
      });
    }
    async doWrite(msg, headers, data) {
      try {
        await this.writable.write(headers.join(""), "ascii");
        return this.writable.write(data);
      } catch (error) {
        this.handleError(error, msg);
        return Promise.reject(error);
      }
    }
    handleError(error, msg) {
      this.errorCount++;
      this.fireError(error, msg, this.errorCount);
    }
    end() {
      this.writable.end();
    }
  }
  exports.WriteableStreamMessageWriter = WriteableStreamMessageWriter;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/messageBuffer.js
var require_messageBuffer = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.AbstractMessageBuffer = undefined;
  var CR = 13;
  var LF = 10;
  var CRLF = `\r
`;

  class AbstractMessageBuffer {
    constructor(encoding = "utf-8") {
      this._encoding = encoding;
      this._chunks = [];
      this._totalLength = 0;
    }
    get encoding() {
      return this._encoding;
    }
    append(chunk) {
      const toAppend = typeof chunk === "string" ? this.fromString(chunk, this._encoding) : chunk;
      this._chunks.push(toAppend);
      this._totalLength += toAppend.byteLength;
    }
    tryReadHeaders(lowerCaseKeys = false) {
      if (this._chunks.length === 0) {
        return;
      }
      let state = 0;
      let chunkIndex = 0;
      let offset = 0;
      let chunkBytesRead = 0;
      row:
        while (chunkIndex < this._chunks.length) {
          const chunk = this._chunks[chunkIndex];
          offset = 0;
          column:
            while (offset < chunk.length) {
              const value = chunk[offset];
              switch (value) {
                case CR:
                  switch (state) {
                    case 0:
                      state = 1;
                      break;
                    case 2:
                      state = 3;
                      break;
                    default:
                      state = 0;
                  }
                  break;
                case LF:
                  switch (state) {
                    case 1:
                      state = 2;
                      break;
                    case 3:
                      state = 4;
                      offset++;
                      break row;
                    default:
                      state = 0;
                  }
                  break;
                default:
                  state = 0;
              }
              offset++;
            }
          chunkBytesRead += chunk.byteLength;
          chunkIndex++;
        }
      if (state !== 4) {
        return;
      }
      const buffer = this._read(chunkBytesRead + offset);
      const result = new Map;
      const headers = this.toString(buffer, "ascii").split(CRLF);
      if (headers.length < 2) {
        return result;
      }
      for (let i = 0;i < headers.length - 2; i++) {
        const header = headers[i];
        const index = header.indexOf(":");
        if (index === -1) {
          throw new Error(`Message header must separate key and value using ':'
${header}`);
        }
        const key = header.substr(0, index);
        const value = header.substr(index + 1).trim();
        result.set(lowerCaseKeys ? key.toLowerCase() : key, value);
      }
      return result;
    }
    tryReadBody(length) {
      if (this._totalLength < length) {
        return;
      }
      return this._read(length);
    }
    get numberOfBytes() {
      return this._totalLength;
    }
    _read(byteCount) {
      if (byteCount === 0) {
        return this.emptyBuffer();
      }
      if (byteCount > this._totalLength) {
        throw new Error(`Cannot read so many bytes!`);
      }
      if (this._chunks[0].byteLength === byteCount) {
        const chunk = this._chunks[0];
        this._chunks.shift();
        this._totalLength -= byteCount;
        return this.asNative(chunk);
      }
      if (this._chunks[0].byteLength > byteCount) {
        const chunk = this._chunks[0];
        const result2 = this.asNative(chunk, byteCount);
        this._chunks[0] = chunk.slice(byteCount);
        this._totalLength -= byteCount;
        return result2;
      }
      const result = this.allocNative(byteCount);
      let resultOffset = 0;
      let chunkIndex = 0;
      while (byteCount > 0) {
        const chunk = this._chunks[chunkIndex];
        if (chunk.byteLength > byteCount) {
          const chunkPart = chunk.slice(0, byteCount);
          result.set(chunkPart, resultOffset);
          resultOffset += byteCount;
          this._chunks[chunkIndex] = chunk.slice(byteCount);
          this._totalLength -= byteCount;
          byteCount -= byteCount;
        } else {
          result.set(chunk, resultOffset);
          resultOffset += chunk.byteLength;
          this._chunks.shift();
          this._totalLength -= chunk.byteLength;
          byteCount -= chunk.byteLength;
        }
      }
      return result;
    }
  }
  exports.AbstractMessageBuffer = AbstractMessageBuffer;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/connection.js
var require_connection = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.createMessageConnection = exports.ConnectionOptions = exports.MessageStrategy = exports.CancellationStrategy = exports.CancellationSenderStrategy = exports.CancellationReceiverStrategy = exports.RequestCancellationReceiverStrategy = exports.IdCancellationReceiverStrategy = exports.ConnectionStrategy = exports.ConnectionError = exports.ConnectionErrors = exports.LogTraceNotification = exports.SetTraceNotification = exports.TraceFormat = exports.TraceValues = exports.Trace = exports.NullLogger = exports.ProgressType = exports.ProgressToken = undefined;
  var ral_1 = require_ral();
  var Is = require_is2();
  var messages_1 = require_messages();
  var linkedMap_1 = require_linkedMap();
  var events_1 = require_events();
  var cancellation_1 = require_cancellation();
  var CancelNotification;
  (function(CancelNotification2) {
    CancelNotification2.type = new messages_1.NotificationType("$/cancelRequest");
  })(CancelNotification || (CancelNotification = {}));
  var ProgressToken;
  (function(ProgressToken2) {
    function is(value) {
      return typeof value === "string" || typeof value === "number";
    }
    ProgressToken2.is = is;
  })(ProgressToken || (exports.ProgressToken = ProgressToken = {}));
  var ProgressNotification;
  (function(ProgressNotification2) {
    ProgressNotification2.type = new messages_1.NotificationType("$/progress");
  })(ProgressNotification || (ProgressNotification = {}));

  class ProgressType {
    constructor() {}
  }
  exports.ProgressType = ProgressType;
  var StarRequestHandler;
  (function(StarRequestHandler2) {
    function is(value) {
      return Is.func(value);
    }
    StarRequestHandler2.is = is;
  })(StarRequestHandler || (StarRequestHandler = {}));
  exports.NullLogger = Object.freeze({
    error: () => {},
    warn: () => {},
    info: () => {},
    log: () => {}
  });
  var Trace;
  (function(Trace2) {
    Trace2[Trace2["Off"] = 0] = "Off";
    Trace2[Trace2["Messages"] = 1] = "Messages";
    Trace2[Trace2["Compact"] = 2] = "Compact";
    Trace2[Trace2["Verbose"] = 3] = "Verbose";
  })(Trace || (exports.Trace = Trace = {}));
  var TraceValues;
  (function(TraceValues2) {
    TraceValues2.Off = "off";
    TraceValues2.Messages = "messages";
    TraceValues2.Compact = "compact";
    TraceValues2.Verbose = "verbose";
  })(TraceValues || (exports.TraceValues = TraceValues = {}));
  (function(Trace2) {
    function fromString(value) {
      if (!Is.string(value)) {
        return Trace2.Off;
      }
      value = value.toLowerCase();
      switch (value) {
        case "off":
          return Trace2.Off;
        case "messages":
          return Trace2.Messages;
        case "compact":
          return Trace2.Compact;
        case "verbose":
          return Trace2.Verbose;
        default:
          return Trace2.Off;
      }
    }
    Trace2.fromString = fromString;
    function toString(value) {
      switch (value) {
        case Trace2.Off:
          return "off";
        case Trace2.Messages:
          return "messages";
        case Trace2.Compact:
          return "compact";
        case Trace2.Verbose:
          return "verbose";
        default:
          return "off";
      }
    }
    Trace2.toString = toString;
  })(Trace || (exports.Trace = Trace = {}));
  var TraceFormat;
  (function(TraceFormat2) {
    TraceFormat2["Text"] = "text";
    TraceFormat2["JSON"] = "json";
  })(TraceFormat || (exports.TraceFormat = TraceFormat = {}));
  (function(TraceFormat2) {
    function fromString(value) {
      if (!Is.string(value)) {
        return TraceFormat2.Text;
      }
      value = value.toLowerCase();
      if (value === "json") {
        return TraceFormat2.JSON;
      } else {
        return TraceFormat2.Text;
      }
    }
    TraceFormat2.fromString = fromString;
  })(TraceFormat || (exports.TraceFormat = TraceFormat = {}));
  var SetTraceNotification;
  (function(SetTraceNotification2) {
    SetTraceNotification2.type = new messages_1.NotificationType("$/setTrace");
  })(SetTraceNotification || (exports.SetTraceNotification = SetTraceNotification = {}));
  var LogTraceNotification;
  (function(LogTraceNotification2) {
    LogTraceNotification2.type = new messages_1.NotificationType("$/logTrace");
  })(LogTraceNotification || (exports.LogTraceNotification = LogTraceNotification = {}));
  var ConnectionErrors;
  (function(ConnectionErrors2) {
    ConnectionErrors2[ConnectionErrors2["Closed"] = 1] = "Closed";
    ConnectionErrors2[ConnectionErrors2["Disposed"] = 2] = "Disposed";
    ConnectionErrors2[ConnectionErrors2["AlreadyListening"] = 3] = "AlreadyListening";
  })(ConnectionErrors || (exports.ConnectionErrors = ConnectionErrors = {}));

  class ConnectionError extends Error {
    constructor(code, message) {
      super(message);
      this.code = code;
      Object.setPrototypeOf(this, ConnectionError.prototype);
    }
  }
  exports.ConnectionError = ConnectionError;
  var ConnectionStrategy;
  (function(ConnectionStrategy2) {
    function is(value) {
      const candidate = value;
      return candidate && Is.func(candidate.cancelUndispatched);
    }
    ConnectionStrategy2.is = is;
  })(ConnectionStrategy || (exports.ConnectionStrategy = ConnectionStrategy = {}));
  var IdCancellationReceiverStrategy;
  (function(IdCancellationReceiverStrategy2) {
    function is(value) {
      const candidate = value;
      return candidate && (candidate.kind === undefined || candidate.kind === "id") && Is.func(candidate.createCancellationTokenSource) && (candidate.dispose === undefined || Is.func(candidate.dispose));
    }
    IdCancellationReceiverStrategy2.is = is;
  })(IdCancellationReceiverStrategy || (exports.IdCancellationReceiverStrategy = IdCancellationReceiverStrategy = {}));
  var RequestCancellationReceiverStrategy;
  (function(RequestCancellationReceiverStrategy2) {
    function is(value) {
      const candidate = value;
      return candidate && candidate.kind === "request" && Is.func(candidate.createCancellationTokenSource) && (candidate.dispose === undefined || Is.func(candidate.dispose));
    }
    RequestCancellationReceiverStrategy2.is = is;
  })(RequestCancellationReceiverStrategy || (exports.RequestCancellationReceiverStrategy = RequestCancellationReceiverStrategy = {}));
  var CancellationReceiverStrategy;
  (function(CancellationReceiverStrategy2) {
    CancellationReceiverStrategy2.Message = Object.freeze({
      createCancellationTokenSource(_) {
        return new cancellation_1.CancellationTokenSource;
      }
    });
    function is(value) {
      return IdCancellationReceiverStrategy.is(value) || RequestCancellationReceiverStrategy.is(value);
    }
    CancellationReceiverStrategy2.is = is;
  })(CancellationReceiverStrategy || (exports.CancellationReceiverStrategy = CancellationReceiverStrategy = {}));
  var CancellationSenderStrategy;
  (function(CancellationSenderStrategy2) {
    CancellationSenderStrategy2.Message = Object.freeze({
      sendCancellation(conn, id) {
        return conn.sendNotification(CancelNotification.type, { id });
      },
      cleanup(_) {}
    });
    function is(value) {
      const candidate = value;
      return candidate && Is.func(candidate.sendCancellation) && Is.func(candidate.cleanup);
    }
    CancellationSenderStrategy2.is = is;
  })(CancellationSenderStrategy || (exports.CancellationSenderStrategy = CancellationSenderStrategy = {}));
  var CancellationStrategy;
  (function(CancellationStrategy2) {
    CancellationStrategy2.Message = Object.freeze({
      receiver: CancellationReceiverStrategy.Message,
      sender: CancellationSenderStrategy.Message
    });
    function is(value) {
      const candidate = value;
      return candidate && CancellationReceiverStrategy.is(candidate.receiver) && CancellationSenderStrategy.is(candidate.sender);
    }
    CancellationStrategy2.is = is;
  })(CancellationStrategy || (exports.CancellationStrategy = CancellationStrategy = {}));
  var MessageStrategy;
  (function(MessageStrategy2) {
    function is(value) {
      const candidate = value;
      return candidate && Is.func(candidate.handleMessage);
    }
    MessageStrategy2.is = is;
  })(MessageStrategy || (exports.MessageStrategy = MessageStrategy = {}));
  var ConnectionOptions;
  (function(ConnectionOptions2) {
    function is(value) {
      const candidate = value;
      return candidate && (CancellationStrategy.is(candidate.cancellationStrategy) || ConnectionStrategy.is(candidate.connectionStrategy) || MessageStrategy.is(candidate.messageStrategy));
    }
    ConnectionOptions2.is = is;
  })(ConnectionOptions || (exports.ConnectionOptions = ConnectionOptions = {}));
  var ConnectionState;
  (function(ConnectionState2) {
    ConnectionState2[ConnectionState2["New"] = 1] = "New";
    ConnectionState2[ConnectionState2["Listening"] = 2] = "Listening";
    ConnectionState2[ConnectionState2["Closed"] = 3] = "Closed";
    ConnectionState2[ConnectionState2["Disposed"] = 4] = "Disposed";
  })(ConnectionState || (ConnectionState = {}));
  function createMessageConnection(messageReader, messageWriter, _logger, options) {
    const logger = _logger !== undefined ? _logger : exports.NullLogger;
    let sequenceNumber = 0;
    let notificationSequenceNumber = 0;
    let unknownResponseSequenceNumber = 0;
    const version = "2.0";
    let starRequestHandler = undefined;
    const requestHandlers = new Map;
    let starNotificationHandler = undefined;
    const notificationHandlers = new Map;
    const progressHandlers = new Map;
    let timer;
    let messageQueue = new linkedMap_1.LinkedMap;
    let responsePromises = new Map;
    let knownCanceledRequests = new Set;
    let requestTokens = new Map;
    let trace = Trace.Off;
    let traceFormat = TraceFormat.Text;
    let tracer;
    let state = ConnectionState.New;
    const errorEmitter = new events_1.Emitter;
    const closeEmitter = new events_1.Emitter;
    const unhandledNotificationEmitter = new events_1.Emitter;
    const unhandledProgressEmitter = new events_1.Emitter;
    const disposeEmitter = new events_1.Emitter;
    const cancellationStrategy = options && options.cancellationStrategy ? options.cancellationStrategy : CancellationStrategy.Message;
    function createRequestQueueKey(id) {
      if (id === null) {
        throw new Error(`Can't send requests with id null since the response can't be correlated.`);
      }
      return "req-" + id.toString();
    }
    function createResponseQueueKey(id) {
      if (id === null) {
        return "res-unknown-" + (++unknownResponseSequenceNumber).toString();
      } else {
        return "res-" + id.toString();
      }
    }
    function createNotificationQueueKey() {
      return "not-" + (++notificationSequenceNumber).toString();
    }
    function addMessageToQueue(queue, message) {
      if (messages_1.Message.isRequest(message)) {
        queue.set(createRequestQueueKey(message.id), message);
      } else if (messages_1.Message.isResponse(message)) {
        queue.set(createResponseQueueKey(message.id), message);
      } else {
        queue.set(createNotificationQueueKey(), message);
      }
    }
    function cancelUndispatched(_message) {
      return;
    }
    function isListening() {
      return state === ConnectionState.Listening;
    }
    function isClosed() {
      return state === ConnectionState.Closed;
    }
    function isDisposed() {
      return state === ConnectionState.Disposed;
    }
    function closeHandler() {
      if (state === ConnectionState.New || state === ConnectionState.Listening) {
        state = ConnectionState.Closed;
        closeEmitter.fire(undefined);
      }
    }
    function readErrorHandler(error) {
      errorEmitter.fire([error, undefined, undefined]);
    }
    function writeErrorHandler(data) {
      errorEmitter.fire(data);
    }
    messageReader.onClose(closeHandler);
    messageReader.onError(readErrorHandler);
    messageWriter.onClose(closeHandler);
    messageWriter.onError(writeErrorHandler);
    function triggerMessageQueue() {
      if (timer || messageQueue.size === 0) {
        return;
      }
      timer = (0, ral_1.default)().timer.setImmediate(() => {
        timer = undefined;
        processMessageQueue();
      });
    }
    function handleMessage(message) {
      if (messages_1.Message.isRequest(message)) {
        handleRequest(message);
      } else if (messages_1.Message.isNotification(message)) {
        handleNotification(message);
      } else if (messages_1.Message.isResponse(message)) {
        handleResponse(message);
      } else {
        handleInvalidMessage(message);
      }
    }
    function processMessageQueue() {
      if (messageQueue.size === 0) {
        return;
      }
      const message = messageQueue.shift();
      try {
        const messageStrategy = options?.messageStrategy;
        if (MessageStrategy.is(messageStrategy)) {
          messageStrategy.handleMessage(message, handleMessage);
        } else {
          handleMessage(message);
        }
      } finally {
        triggerMessageQueue();
      }
    }
    const callback = (message) => {
      try {
        if (messages_1.Message.isNotification(message) && message.method === CancelNotification.type.method) {
          const cancelId = message.params.id;
          const key = createRequestQueueKey(cancelId);
          const toCancel = messageQueue.get(key);
          if (messages_1.Message.isRequest(toCancel)) {
            const strategy = options?.connectionStrategy;
            const response = strategy && strategy.cancelUndispatched ? strategy.cancelUndispatched(toCancel, cancelUndispatched) : cancelUndispatched(toCancel);
            if (response && (response.error !== undefined || response.result !== undefined)) {
              messageQueue.delete(key);
              requestTokens.delete(cancelId);
              response.id = toCancel.id;
              traceSendingResponse(response, message.method, Date.now());
              messageWriter.write(response).catch(() => logger.error(`Sending response for canceled message failed.`));
              return;
            }
          }
          const cancellationToken = requestTokens.get(cancelId);
          if (cancellationToken !== undefined) {
            cancellationToken.cancel();
            traceReceivedNotification(message);
            return;
          } else {
            knownCanceledRequests.add(cancelId);
          }
        }
        addMessageToQueue(messageQueue, message);
      } finally {
        triggerMessageQueue();
      }
    };
    function handleRequest(requestMessage) {
      if (isDisposed()) {
        return;
      }
      function reply(resultOrError, method, startTime2) {
        const message = {
          jsonrpc: version,
          id: requestMessage.id
        };
        if (resultOrError instanceof messages_1.ResponseError) {
          message.error = resultOrError.toJson();
        } else {
          message.result = resultOrError === undefined ? null : resultOrError;
        }
        traceSendingResponse(message, method, startTime2);
        messageWriter.write(message).catch(() => logger.error(`Sending response failed.`));
      }
      function replyError(error, method, startTime2) {
        const message = {
          jsonrpc: version,
          id: requestMessage.id,
          error: error.toJson()
        };
        traceSendingResponse(message, method, startTime2);
        messageWriter.write(message).catch(() => logger.error(`Sending response failed.`));
      }
      function replySuccess(result, method, startTime2) {
        if (result === undefined) {
          result = null;
        }
        const message = {
          jsonrpc: version,
          id: requestMessage.id,
          result
        };
        traceSendingResponse(message, method, startTime2);
        messageWriter.write(message).catch(() => logger.error(`Sending response failed.`));
      }
      traceReceivedRequest(requestMessage);
      const element = requestHandlers.get(requestMessage.method);
      let type;
      let requestHandler;
      if (element) {
        type = element.type;
        requestHandler = element.handler;
      }
      const startTime = Date.now();
      if (requestHandler || starRequestHandler) {
        const tokenKey = requestMessage.id ?? String(Date.now());
        const cancellationSource = IdCancellationReceiverStrategy.is(cancellationStrategy.receiver) ? cancellationStrategy.receiver.createCancellationTokenSource(tokenKey) : cancellationStrategy.receiver.createCancellationTokenSource(requestMessage);
        if (requestMessage.id !== null && knownCanceledRequests.has(requestMessage.id)) {
          cancellationSource.cancel();
        }
        if (requestMessage.id !== null) {
          requestTokens.set(tokenKey, cancellationSource);
        }
        try {
          let handlerResult;
          if (requestHandler) {
            if (requestMessage.params === undefined) {
              if (type !== undefined && type.numberOfParams !== 0) {
                replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines ${type.numberOfParams} params but received none.`), requestMessage.method, startTime);
                return;
              }
              handlerResult = requestHandler(cancellationSource.token);
            } else if (Array.isArray(requestMessage.params)) {
              if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byName) {
                replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines parameters by name but received parameters by position`), requestMessage.method, startTime);
                return;
              }
              handlerResult = requestHandler(...requestMessage.params, cancellationSource.token);
            } else {
              if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byPosition) {
                replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines parameters by position but received parameters by name`), requestMessage.method, startTime);
                return;
              }
              handlerResult = requestHandler(requestMessage.params, cancellationSource.token);
            }
          } else if (starRequestHandler) {
            handlerResult = starRequestHandler(requestMessage.method, requestMessage.params, cancellationSource.token);
          }
          const promise = handlerResult;
          if (!handlerResult) {
            requestTokens.delete(tokenKey);
            replySuccess(handlerResult, requestMessage.method, startTime);
          } else if (promise.then) {
            promise.then((resultOrError) => {
              requestTokens.delete(tokenKey);
              reply(resultOrError, requestMessage.method, startTime);
            }, (error) => {
              requestTokens.delete(tokenKey);
              if (error instanceof messages_1.ResponseError) {
                replyError(error, requestMessage.method, startTime);
              } else if (error && Is.string(error.message)) {
                replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed with message: ${error.message}`), requestMessage.method, startTime);
              } else {
                replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed unexpectedly without providing any details.`), requestMessage.method, startTime);
              }
            });
          } else {
            requestTokens.delete(tokenKey);
            reply(handlerResult, requestMessage.method, startTime);
          }
        } catch (error) {
          requestTokens.delete(tokenKey);
          if (error instanceof messages_1.ResponseError) {
            reply(error, requestMessage.method, startTime);
          } else if (error && Is.string(error.message)) {
            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed with message: ${error.message}`), requestMessage.method, startTime);
          } else {
            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed unexpectedly without providing any details.`), requestMessage.method, startTime);
          }
        }
      } else {
        replyError(new messages_1.ResponseError(messages_1.ErrorCodes.MethodNotFound, `Unhandled method ${requestMessage.method}`), requestMessage.method, startTime);
      }
    }
    function handleResponse(responseMessage) {
      if (isDisposed()) {
        return;
      }
      if (responseMessage.id === null) {
        if (responseMessage.error) {
          logger.error(`Received response message without id: Error is: 
${JSON.stringify(responseMessage.error, undefined, 4)}`);
        } else {
          logger.error(`Received response message without id. No further error information provided.`);
        }
      } else {
        const key = responseMessage.id;
        const responsePromise = responsePromises.get(key);
        traceReceivedResponse(responseMessage, responsePromise);
        if (responsePromise !== undefined) {
          responsePromises.delete(key);
          try {
            if (responseMessage.error) {
              const error = responseMessage.error;
              responsePromise.reject(new messages_1.ResponseError(error.code, error.message, error.data));
            } else if (responseMessage.result !== undefined) {
              responsePromise.resolve(responseMessage.result);
            } else {
              throw new Error("Should never happen.");
            }
          } catch (error) {
            if (error.message) {
              logger.error(`Response handler '${responsePromise.method}' failed with message: ${error.message}`);
            } else {
              logger.error(`Response handler '${responsePromise.method}' failed unexpectedly.`);
            }
          }
        }
      }
    }
    function handleNotification(message) {
      if (isDisposed()) {
        return;
      }
      let type = undefined;
      let notificationHandler;
      if (message.method === CancelNotification.type.method) {
        const cancelId = message.params.id;
        knownCanceledRequests.delete(cancelId);
        traceReceivedNotification(message);
        return;
      } else {
        const element = notificationHandlers.get(message.method);
        if (element) {
          notificationHandler = element.handler;
          type = element.type;
        }
      }
      if (notificationHandler || starNotificationHandler) {
        try {
          traceReceivedNotification(message);
          if (notificationHandler) {
            if (message.params === undefined) {
              if (type !== undefined) {
                if (type.numberOfParams !== 0 && type.parameterStructures !== messages_1.ParameterStructures.byName) {
                  logger.error(`Notification ${message.method} defines ${type.numberOfParams} params but received none.`);
                }
              }
              notificationHandler();
            } else if (Array.isArray(message.params)) {
              const params = message.params;
              if (message.method === ProgressNotification.type.method && params.length === 2 && ProgressToken.is(params[0])) {
                notificationHandler({ token: params[0], value: params[1] });
              } else {
                if (type !== undefined) {
                  if (type.parameterStructures === messages_1.ParameterStructures.byName) {
                    logger.error(`Notification ${message.method} defines parameters by name but received parameters by position`);
                  }
                  if (type.numberOfParams !== message.params.length) {
                    logger.error(`Notification ${message.method} defines ${type.numberOfParams} params but received ${params.length} arguments`);
                  }
                }
                notificationHandler(...params);
              }
            } else {
              if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byPosition) {
                logger.error(`Notification ${message.method} defines parameters by position but received parameters by name`);
              }
              notificationHandler(message.params);
            }
          } else if (starNotificationHandler) {
            starNotificationHandler(message.method, message.params);
          }
        } catch (error) {
          if (error.message) {
            logger.error(`Notification handler '${message.method}' failed with message: ${error.message}`);
          } else {
            logger.error(`Notification handler '${message.method}' failed unexpectedly.`);
          }
        }
      } else {
        unhandledNotificationEmitter.fire(message);
      }
    }
    function handleInvalidMessage(message) {
      if (!message) {
        logger.error("Received empty message.");
        return;
      }
      logger.error(`Received message which is neither a response nor a notification message:
${JSON.stringify(message, null, 4)}`);
      const responseMessage = message;
      if (Is.string(responseMessage.id) || Is.number(responseMessage.id)) {
        const key = responseMessage.id;
        const responseHandler = responsePromises.get(key);
        if (responseHandler) {
          responseHandler.reject(new Error("The received response has neither a result nor an error property."));
        }
      }
    }
    function stringifyTrace(params) {
      if (params === undefined || params === null) {
        return;
      }
      switch (trace) {
        case Trace.Verbose:
          return JSON.stringify(params, null, 4);
        case Trace.Compact:
          return JSON.stringify(params);
        default:
          return;
      }
    }
    function traceSendingRequest(message) {
      if (trace === Trace.Off || !tracer) {
        return;
      }
      if (traceFormat === TraceFormat.Text) {
        let data = undefined;
        if ((trace === Trace.Verbose || trace === Trace.Compact) && message.params) {
          data = `Params: ${stringifyTrace(message.params)}

`;
        }
        tracer.log(`Sending request '${message.method} - (${message.id})'.`, data);
      } else {
        logLSPMessage("send-request", message);
      }
    }
    function traceSendingNotification(message) {
      if (trace === Trace.Off || !tracer) {
        return;
      }
      if (traceFormat === TraceFormat.Text) {
        let data = undefined;
        if (trace === Trace.Verbose || trace === Trace.Compact) {
          if (message.params) {
            data = `Params: ${stringifyTrace(message.params)}

`;
          } else {
            data = `No parameters provided.

`;
          }
        }
        tracer.log(`Sending notification '${message.method}'.`, data);
      } else {
        logLSPMessage("send-notification", message);
      }
    }
    function traceSendingResponse(message, method, startTime) {
      if (trace === Trace.Off || !tracer) {
        return;
      }
      if (traceFormat === TraceFormat.Text) {
        let data = undefined;
        if (trace === Trace.Verbose || trace === Trace.Compact) {
          if (message.error && message.error.data) {
            data = `Error data: ${stringifyTrace(message.error.data)}

`;
          } else {
            if (message.result) {
              data = `Result: ${stringifyTrace(message.result)}

`;
            } else if (message.error === undefined) {
              data = `No result returned.

`;
            }
          }
        }
        tracer.log(`Sending response '${method} - (${message.id})'. Processing request took ${Date.now() - startTime}ms`, data);
      } else {
        logLSPMessage("send-response", message);
      }
    }
    function traceReceivedRequest(message) {
      if (trace === Trace.Off || !tracer) {
        return;
      }
      if (traceFormat === TraceFormat.Text) {
        let data = undefined;
        if ((trace === Trace.Verbose || trace === Trace.Compact) && message.params) {
          data = `Params: ${stringifyTrace(message.params)}

`;
        }
        tracer.log(`Received request '${message.method} - (${message.id})'.`, data);
      } else {
        logLSPMessage("receive-request", message);
      }
    }
    function traceReceivedNotification(message) {
      if (trace === Trace.Off || !tracer || message.method === LogTraceNotification.type.method) {
        return;
      }
      if (traceFormat === TraceFormat.Text) {
        let data = undefined;
        if (trace === Trace.Verbose || trace === Trace.Compact) {
          if (message.params) {
            data = `Params: ${stringifyTrace(message.params)}

`;
          } else {
            data = `No parameters provided.

`;
          }
        }
        tracer.log(`Received notification '${message.method}'.`, data);
      } else {
        logLSPMessage("receive-notification", message);
      }
    }
    function traceReceivedResponse(message, responsePromise) {
      if (trace === Trace.Off || !tracer) {
        return;
      }
      if (traceFormat === TraceFormat.Text) {
        let data = undefined;
        if (trace === Trace.Verbose || trace === Trace.Compact) {
          if (message.error && message.error.data) {
            data = `Error data: ${stringifyTrace(message.error.data)}

`;
          } else {
            if (message.result) {
              data = `Result: ${stringifyTrace(message.result)}

`;
            } else if (message.error === undefined) {
              data = `No result returned.

`;
            }
          }
        }
        if (responsePromise) {
          const error = message.error ? ` Request failed: ${message.error.message} (${message.error.code}).` : "";
          tracer.log(`Received response '${responsePromise.method} - (${message.id})' in ${Date.now() - responsePromise.timerStart}ms.${error}`, data);
        } else {
          tracer.log(`Received response ${message.id} without active response promise.`, data);
        }
      } else {
        logLSPMessage("receive-response", message);
      }
    }
    function logLSPMessage(type, message) {
      if (!tracer || trace === Trace.Off) {
        return;
      }
      const lspMessage = {
        isLSPMessage: true,
        type,
        message,
        timestamp: Date.now()
      };
      tracer.log(lspMessage);
    }
    function throwIfClosedOrDisposed() {
      if (isClosed()) {
        throw new ConnectionError(ConnectionErrors.Closed, "Connection is closed.");
      }
      if (isDisposed()) {
        throw new ConnectionError(ConnectionErrors.Disposed, "Connection is disposed.");
      }
    }
    function throwIfListening() {
      if (isListening()) {
        throw new ConnectionError(ConnectionErrors.AlreadyListening, "Connection is already listening");
      }
    }
    function throwIfNotListening() {
      if (!isListening()) {
        throw new Error("Call listen() first.");
      }
    }
    function undefinedToNull(param) {
      if (param === undefined) {
        return null;
      } else {
        return param;
      }
    }
    function nullToUndefined(param) {
      if (param === null) {
        return;
      } else {
        return param;
      }
    }
    function isNamedParam(param) {
      return param !== undefined && param !== null && !Array.isArray(param) && typeof param === "object";
    }
    function computeSingleParam(parameterStructures, param) {
      switch (parameterStructures) {
        case messages_1.ParameterStructures.auto:
          if (isNamedParam(param)) {
            return nullToUndefined(param);
          } else {
            return [undefinedToNull(param)];
          }
        case messages_1.ParameterStructures.byName:
          if (!isNamedParam(param)) {
            throw new Error(`Received parameters by name but param is not an object literal.`);
          }
          return nullToUndefined(param);
        case messages_1.ParameterStructures.byPosition:
          return [undefinedToNull(param)];
        default:
          throw new Error(`Unknown parameter structure ${parameterStructures.toString()}`);
      }
    }
    function computeMessageParams(type, params) {
      let result;
      const numberOfParams = type.numberOfParams;
      switch (numberOfParams) {
        case 0:
          result = undefined;
          break;
        case 1:
          result = computeSingleParam(type.parameterStructures, params[0]);
          break;
        default:
          result = [];
          for (let i = 0;i < params.length && i < numberOfParams; i++) {
            result.push(undefinedToNull(params[i]));
          }
          if (params.length < numberOfParams) {
            for (let i = params.length;i < numberOfParams; i++) {
              result.push(null);
            }
          }
          break;
      }
      return result;
    }
    const connection = {
      sendNotification: (type, ...args) => {
        throwIfClosedOrDisposed();
        let method;
        let messageParams;
        if (Is.string(type)) {
          method = type;
          const first = args[0];
          let paramStart = 0;
          let parameterStructures = messages_1.ParameterStructures.auto;
          if (messages_1.ParameterStructures.is(first)) {
            paramStart = 1;
            parameterStructures = first;
          }
          let paramEnd = args.length;
          const numberOfParams = paramEnd - paramStart;
          switch (numberOfParams) {
            case 0:
              messageParams = undefined;
              break;
            case 1:
              messageParams = computeSingleParam(parameterStructures, args[paramStart]);
              break;
            default:
              if (parameterStructures === messages_1.ParameterStructures.byName) {
                throw new Error(`Received ${numberOfParams} parameters for 'by Name' notification parameter structure.`);
              }
              messageParams = args.slice(paramStart, paramEnd).map((value) => undefinedToNull(value));
              break;
          }
        } else {
          const params = args;
          method = type.method;
          messageParams = computeMessageParams(type, params);
        }
        const notificationMessage = {
          jsonrpc: version,
          method,
          params: messageParams
        };
        traceSendingNotification(notificationMessage);
        return messageWriter.write(notificationMessage).catch((error) => {
          logger.error(`Sending notification failed.`);
          throw error;
        });
      },
      onNotification: (type, handler) => {
        throwIfClosedOrDisposed();
        let method;
        if (Is.func(type)) {
          starNotificationHandler = type;
        } else if (handler) {
          if (Is.string(type)) {
            method = type;
            notificationHandlers.set(type, { type: undefined, handler });
          } else {
            method = type.method;
            notificationHandlers.set(type.method, { type, handler });
          }
        }
        return {
          dispose: () => {
            if (method !== undefined) {
              notificationHandlers.delete(method);
            } else {
              starNotificationHandler = undefined;
            }
          }
        };
      },
      onProgress: (_type, token, handler) => {
        if (progressHandlers.has(token)) {
          throw new Error(`Progress handler for token ${token} already registered`);
        }
        progressHandlers.set(token, handler);
        return {
          dispose: () => {
            progressHandlers.delete(token);
          }
        };
      },
      sendProgress: (_type, token, value) => {
        return connection.sendNotification(ProgressNotification.type, { token, value });
      },
      onUnhandledProgress: unhandledProgressEmitter.event,
      sendRequest: (type, ...args) => {
        throwIfClosedOrDisposed();
        throwIfNotListening();
        let method;
        let messageParams;
        let token = undefined;
        if (Is.string(type)) {
          method = type;
          const first = args[0];
          const last = args[args.length - 1];
          let paramStart = 0;
          let parameterStructures = messages_1.ParameterStructures.auto;
          if (messages_1.ParameterStructures.is(first)) {
            paramStart = 1;
            parameterStructures = first;
          }
          let paramEnd = args.length;
          if (cancellation_1.CancellationToken.is(last)) {
            paramEnd = paramEnd - 1;
            token = last;
          }
          const numberOfParams = paramEnd - paramStart;
          switch (numberOfParams) {
            case 0:
              messageParams = undefined;
              break;
            case 1:
              messageParams = computeSingleParam(parameterStructures, args[paramStart]);
              break;
            default:
              if (parameterStructures === messages_1.ParameterStructures.byName) {
                throw new Error(`Received ${numberOfParams} parameters for 'by Name' request parameter structure.`);
              }
              messageParams = args.slice(paramStart, paramEnd).map((value) => undefinedToNull(value));
              break;
          }
        } else {
          const params = args;
          method = type.method;
          messageParams = computeMessageParams(type, params);
          const numberOfParams = type.numberOfParams;
          token = cancellation_1.CancellationToken.is(params[numberOfParams]) ? params[numberOfParams] : undefined;
        }
        const id = sequenceNumber++;
        let disposable;
        if (token) {
          disposable = token.onCancellationRequested(() => {
            const p = cancellationStrategy.sender.sendCancellation(connection, id);
            if (p === undefined) {
              logger.log(`Received no promise from cancellation strategy when cancelling id ${id}`);
              return Promise.resolve();
            } else {
              return p.catch(() => {
                logger.log(`Sending cancellation messages for id ${id} failed`);
              });
            }
          });
        }
        const requestMessage = {
          jsonrpc: version,
          id,
          method,
          params: messageParams
        };
        traceSendingRequest(requestMessage);
        if (typeof cancellationStrategy.sender.enableCancellation === "function") {
          cancellationStrategy.sender.enableCancellation(requestMessage);
        }
        return new Promise(async (resolve, reject) => {
          const resolveWithCleanup = (r) => {
            resolve(r);
            cancellationStrategy.sender.cleanup(id);
            disposable?.dispose();
          };
          const rejectWithCleanup = (r) => {
            reject(r);
            cancellationStrategy.sender.cleanup(id);
            disposable?.dispose();
          };
          const responsePromise = { method, timerStart: Date.now(), resolve: resolveWithCleanup, reject: rejectWithCleanup };
          try {
            await messageWriter.write(requestMessage);
            responsePromises.set(id, responsePromise);
          } catch (error) {
            logger.error(`Sending request failed.`);
            responsePromise.reject(new messages_1.ResponseError(messages_1.ErrorCodes.MessageWriteError, error.message ? error.message : "Unknown reason"));
            throw error;
          }
        });
      },
      onRequest: (type, handler) => {
        throwIfClosedOrDisposed();
        let method = null;
        if (StarRequestHandler.is(type)) {
          method = undefined;
          starRequestHandler = type;
        } else if (Is.string(type)) {
          method = null;
          if (handler !== undefined) {
            method = type;
            requestHandlers.set(type, { handler, type: undefined });
          }
        } else {
          if (handler !== undefined) {
            method = type.method;
            requestHandlers.set(type.method, { type, handler });
          }
        }
        return {
          dispose: () => {
            if (method === null) {
              return;
            }
            if (method !== undefined) {
              requestHandlers.delete(method);
            } else {
              starRequestHandler = undefined;
            }
          }
        };
      },
      hasPendingResponse: () => {
        return responsePromises.size > 0;
      },
      trace: async (_value, _tracer, sendNotificationOrTraceOptions) => {
        let _sendNotification = false;
        let _traceFormat = TraceFormat.Text;
        if (sendNotificationOrTraceOptions !== undefined) {
          if (Is.boolean(sendNotificationOrTraceOptions)) {
            _sendNotification = sendNotificationOrTraceOptions;
          } else {
            _sendNotification = sendNotificationOrTraceOptions.sendNotification || false;
            _traceFormat = sendNotificationOrTraceOptions.traceFormat || TraceFormat.Text;
          }
        }
        trace = _value;
        traceFormat = _traceFormat;
        if (trace === Trace.Off) {
          tracer = undefined;
        } else {
          tracer = _tracer;
        }
        if (_sendNotification && !isClosed() && !isDisposed()) {
          await connection.sendNotification(SetTraceNotification.type, { value: Trace.toString(_value) });
        }
      },
      onError: errorEmitter.event,
      onClose: closeEmitter.event,
      onUnhandledNotification: unhandledNotificationEmitter.event,
      onDispose: disposeEmitter.event,
      end: () => {
        messageWriter.end();
      },
      dispose: () => {
        if (isDisposed()) {
          return;
        }
        state = ConnectionState.Disposed;
        disposeEmitter.fire(undefined);
        const error = new messages_1.ResponseError(messages_1.ErrorCodes.PendingResponseRejected, "Pending response rejected since connection got disposed");
        for (const promise of responsePromises.values()) {
          promise.reject(error);
        }
        responsePromises = new Map;
        requestTokens = new Map;
        knownCanceledRequests = new Set;
        messageQueue = new linkedMap_1.LinkedMap;
        if (Is.func(messageWriter.dispose)) {
          messageWriter.dispose();
        }
        if (Is.func(messageReader.dispose)) {
          messageReader.dispose();
        }
      },
      listen: () => {
        throwIfClosedOrDisposed();
        throwIfListening();
        state = ConnectionState.Listening;
        messageReader.listen(callback);
      },
      inspect: () => {
        (0, ral_1.default)().console.log("inspect");
      }
    };
    connection.onNotification(LogTraceNotification.type, (params) => {
      if (trace === Trace.Off || !tracer) {
        return;
      }
      const verbose = trace === Trace.Verbose || trace === Trace.Compact;
      tracer.log(params.message, verbose ? params.verbose : undefined);
    });
    connection.onNotification(ProgressNotification.type, (params) => {
      const handler = progressHandlers.get(params.token);
      if (handler) {
        handler(params.value);
      } else {
        unhandledProgressEmitter.fire(params);
      }
    });
    return connection;
  }
  exports.createMessageConnection = createMessageConnection;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/common/api.js
var require_api = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ProgressType = exports.ProgressToken = exports.createMessageConnection = exports.NullLogger = exports.ConnectionOptions = exports.ConnectionStrategy = exports.AbstractMessageBuffer = exports.WriteableStreamMessageWriter = exports.AbstractMessageWriter = exports.MessageWriter = exports.ReadableStreamMessageReader = exports.AbstractMessageReader = exports.MessageReader = exports.SharedArrayReceiverStrategy = exports.SharedArraySenderStrategy = exports.CancellationToken = exports.CancellationTokenSource = exports.Emitter = exports.Event = exports.Disposable = exports.LRUCache = exports.Touch = exports.LinkedMap = exports.ParameterStructures = exports.NotificationType9 = exports.NotificationType8 = exports.NotificationType7 = exports.NotificationType6 = exports.NotificationType5 = exports.NotificationType4 = exports.NotificationType3 = exports.NotificationType2 = exports.NotificationType1 = exports.NotificationType0 = exports.NotificationType = exports.ErrorCodes = exports.ResponseError = exports.RequestType9 = exports.RequestType8 = exports.RequestType7 = exports.RequestType6 = exports.RequestType5 = exports.RequestType4 = exports.RequestType3 = exports.RequestType2 = exports.RequestType1 = exports.RequestType0 = exports.RequestType = exports.Message = exports.RAL = undefined;
  exports.MessageStrategy = exports.CancellationStrategy = exports.CancellationSenderStrategy = exports.CancellationReceiverStrategy = exports.ConnectionError = exports.ConnectionErrors = exports.LogTraceNotification = exports.SetTraceNotification = exports.TraceFormat = exports.TraceValues = exports.Trace = undefined;
  var messages_1 = require_messages();
  Object.defineProperty(exports, "Message", { enumerable: true, get: function() {
    return messages_1.Message;
  } });
  Object.defineProperty(exports, "RequestType", { enumerable: true, get: function() {
    return messages_1.RequestType;
  } });
  Object.defineProperty(exports, "RequestType0", { enumerable: true, get: function() {
    return messages_1.RequestType0;
  } });
  Object.defineProperty(exports, "RequestType1", { enumerable: true, get: function() {
    return messages_1.RequestType1;
  } });
  Object.defineProperty(exports, "RequestType2", { enumerable: true, get: function() {
    return messages_1.RequestType2;
  } });
  Object.defineProperty(exports, "RequestType3", { enumerable: true, get: function() {
    return messages_1.RequestType3;
  } });
  Object.defineProperty(exports, "RequestType4", { enumerable: true, get: function() {
    return messages_1.RequestType4;
  } });
  Object.defineProperty(exports, "RequestType5", { enumerable: true, get: function() {
    return messages_1.RequestType5;
  } });
  Object.defineProperty(exports, "RequestType6", { enumerable: true, get: function() {
    return messages_1.RequestType6;
  } });
  Object.defineProperty(exports, "RequestType7", { enumerable: true, get: function() {
    return messages_1.RequestType7;
  } });
  Object.defineProperty(exports, "RequestType8", { enumerable: true, get: function() {
    return messages_1.RequestType8;
  } });
  Object.defineProperty(exports, "RequestType9", { enumerable: true, get: function() {
    return messages_1.RequestType9;
  } });
  Object.defineProperty(exports, "ResponseError", { enumerable: true, get: function() {
    return messages_1.ResponseError;
  } });
  Object.defineProperty(exports, "ErrorCodes", { enumerable: true, get: function() {
    return messages_1.ErrorCodes;
  } });
  Object.defineProperty(exports, "NotificationType", { enumerable: true, get: function() {
    return messages_1.NotificationType;
  } });
  Object.defineProperty(exports, "NotificationType0", { enumerable: true, get: function() {
    return messages_1.NotificationType0;
  } });
  Object.defineProperty(exports, "NotificationType1", { enumerable: true, get: function() {
    return messages_1.NotificationType1;
  } });
  Object.defineProperty(exports, "NotificationType2", { enumerable: true, get: function() {
    return messages_1.NotificationType2;
  } });
  Object.defineProperty(exports, "NotificationType3", { enumerable: true, get: function() {
    return messages_1.NotificationType3;
  } });
  Object.defineProperty(exports, "NotificationType4", { enumerable: true, get: function() {
    return messages_1.NotificationType4;
  } });
  Object.defineProperty(exports, "NotificationType5", { enumerable: true, get: function() {
    return messages_1.NotificationType5;
  } });
  Object.defineProperty(exports, "NotificationType6", { enumerable: true, get: function() {
    return messages_1.NotificationType6;
  } });
  Object.defineProperty(exports, "NotificationType7", { enumerable: true, get: function() {
    return messages_1.NotificationType7;
  } });
  Object.defineProperty(exports, "NotificationType8", { enumerable: true, get: function() {
    return messages_1.NotificationType8;
  } });
  Object.defineProperty(exports, "NotificationType9", { enumerable: true, get: function() {
    return messages_1.NotificationType9;
  } });
  Object.defineProperty(exports, "ParameterStructures", { enumerable: true, get: function() {
    return messages_1.ParameterStructures;
  } });
  var linkedMap_1 = require_linkedMap();
  Object.defineProperty(exports, "LinkedMap", { enumerable: true, get: function() {
    return linkedMap_1.LinkedMap;
  } });
  Object.defineProperty(exports, "LRUCache", { enumerable: true, get: function() {
    return linkedMap_1.LRUCache;
  } });
  Object.defineProperty(exports, "Touch", { enumerable: true, get: function() {
    return linkedMap_1.Touch;
  } });
  var disposable_1 = require_disposable();
  Object.defineProperty(exports, "Disposable", { enumerable: true, get: function() {
    return disposable_1.Disposable;
  } });
  var events_1 = require_events();
  Object.defineProperty(exports, "Event", { enumerable: true, get: function() {
    return events_1.Event;
  } });
  Object.defineProperty(exports, "Emitter", { enumerable: true, get: function() {
    return events_1.Emitter;
  } });
  var cancellation_1 = require_cancellation();
  Object.defineProperty(exports, "CancellationTokenSource", { enumerable: true, get: function() {
    return cancellation_1.CancellationTokenSource;
  } });
  Object.defineProperty(exports, "CancellationToken", { enumerable: true, get: function() {
    return cancellation_1.CancellationToken;
  } });
  var sharedArrayCancellation_1 = require_sharedArrayCancellation();
  Object.defineProperty(exports, "SharedArraySenderStrategy", { enumerable: true, get: function() {
    return sharedArrayCancellation_1.SharedArraySenderStrategy;
  } });
  Object.defineProperty(exports, "SharedArrayReceiverStrategy", { enumerable: true, get: function() {
    return sharedArrayCancellation_1.SharedArrayReceiverStrategy;
  } });
  var messageReader_1 = require_messageReader();
  Object.defineProperty(exports, "MessageReader", { enumerable: true, get: function() {
    return messageReader_1.MessageReader;
  } });
  Object.defineProperty(exports, "AbstractMessageReader", { enumerable: true, get: function() {
    return messageReader_1.AbstractMessageReader;
  } });
  Object.defineProperty(exports, "ReadableStreamMessageReader", { enumerable: true, get: function() {
    return messageReader_1.ReadableStreamMessageReader;
  } });
  var messageWriter_1 = require_messageWriter();
  Object.defineProperty(exports, "MessageWriter", { enumerable: true, get: function() {
    return messageWriter_1.MessageWriter;
  } });
  Object.defineProperty(exports, "AbstractMessageWriter", { enumerable: true, get: function() {
    return messageWriter_1.AbstractMessageWriter;
  } });
  Object.defineProperty(exports, "WriteableStreamMessageWriter", { enumerable: true, get: function() {
    return messageWriter_1.WriteableStreamMessageWriter;
  } });
  var messageBuffer_1 = require_messageBuffer();
  Object.defineProperty(exports, "AbstractMessageBuffer", { enumerable: true, get: function() {
    return messageBuffer_1.AbstractMessageBuffer;
  } });
  var connection_1 = require_connection();
  Object.defineProperty(exports, "ConnectionStrategy", { enumerable: true, get: function() {
    return connection_1.ConnectionStrategy;
  } });
  Object.defineProperty(exports, "ConnectionOptions", { enumerable: true, get: function() {
    return connection_1.ConnectionOptions;
  } });
  Object.defineProperty(exports, "NullLogger", { enumerable: true, get: function() {
    return connection_1.NullLogger;
  } });
  Object.defineProperty(exports, "createMessageConnection", { enumerable: true, get: function() {
    return connection_1.createMessageConnection;
  } });
  Object.defineProperty(exports, "ProgressToken", { enumerable: true, get: function() {
    return connection_1.ProgressToken;
  } });
  Object.defineProperty(exports, "ProgressType", { enumerable: true, get: function() {
    return connection_1.ProgressType;
  } });
  Object.defineProperty(exports, "Trace", { enumerable: true, get: function() {
    return connection_1.Trace;
  } });
  Object.defineProperty(exports, "TraceValues", { enumerable: true, get: function() {
    return connection_1.TraceValues;
  } });
  Object.defineProperty(exports, "TraceFormat", { enumerable: true, get: function() {
    return connection_1.TraceFormat;
  } });
  Object.defineProperty(exports, "SetTraceNotification", { enumerable: true, get: function() {
    return connection_1.SetTraceNotification;
  } });
  Object.defineProperty(exports, "LogTraceNotification", { enumerable: true, get: function() {
    return connection_1.LogTraceNotification;
  } });
  Object.defineProperty(exports, "ConnectionErrors", { enumerable: true, get: function() {
    return connection_1.ConnectionErrors;
  } });
  Object.defineProperty(exports, "ConnectionError", { enumerable: true, get: function() {
    return connection_1.ConnectionError;
  } });
  Object.defineProperty(exports, "CancellationReceiverStrategy", { enumerable: true, get: function() {
    return connection_1.CancellationReceiverStrategy;
  } });
  Object.defineProperty(exports, "CancellationSenderStrategy", { enumerable: true, get: function() {
    return connection_1.CancellationSenderStrategy;
  } });
  Object.defineProperty(exports, "CancellationStrategy", { enumerable: true, get: function() {
    return connection_1.CancellationStrategy;
  } });
  Object.defineProperty(exports, "MessageStrategy", { enumerable: true, get: function() {
    return connection_1.MessageStrategy;
  } });
  var ral_1 = require_ral();
  exports.RAL = ral_1.default;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/node/ril.js
var require_ril = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  var util_1 = __require("util");
  var api_1 = require_api();

  class MessageBuffer extends api_1.AbstractMessageBuffer {
    constructor(encoding = "utf-8") {
      super(encoding);
    }
    emptyBuffer() {
      return MessageBuffer.emptyBuffer;
    }
    fromString(value, encoding) {
      return Buffer.from(value, encoding);
    }
    toString(value, encoding) {
      if (value instanceof Buffer) {
        return value.toString(encoding);
      } else {
        return new util_1.TextDecoder(encoding).decode(value);
      }
    }
    asNative(buffer, length) {
      if (length === undefined) {
        return buffer instanceof Buffer ? buffer : Buffer.from(buffer);
      } else {
        return buffer instanceof Buffer ? buffer.slice(0, length) : Buffer.from(buffer, 0, length);
      }
    }
    allocNative(length) {
      return Buffer.allocUnsafe(length);
    }
  }
  MessageBuffer.emptyBuffer = Buffer.allocUnsafe(0);

  class ReadableStreamWrapper {
    constructor(stream) {
      this.stream = stream;
    }
    onClose(listener) {
      this.stream.on("close", listener);
      return api_1.Disposable.create(() => this.stream.off("close", listener));
    }
    onError(listener) {
      this.stream.on("error", listener);
      return api_1.Disposable.create(() => this.stream.off("error", listener));
    }
    onEnd(listener) {
      this.stream.on("end", listener);
      return api_1.Disposable.create(() => this.stream.off("end", listener));
    }
    onData(listener) {
      this.stream.on("data", listener);
      return api_1.Disposable.create(() => this.stream.off("data", listener));
    }
  }

  class WritableStreamWrapper {
    constructor(stream) {
      this.stream = stream;
    }
    onClose(listener) {
      this.stream.on("close", listener);
      return api_1.Disposable.create(() => this.stream.off("close", listener));
    }
    onError(listener) {
      this.stream.on("error", listener);
      return api_1.Disposable.create(() => this.stream.off("error", listener));
    }
    onEnd(listener) {
      this.stream.on("end", listener);
      return api_1.Disposable.create(() => this.stream.off("end", listener));
    }
    write(data, encoding) {
      return new Promise((resolve, reject) => {
        const callback = (error) => {
          if (error === undefined || error === null) {
            resolve();
          } else {
            reject(error);
          }
        };
        if (typeof data === "string") {
          this.stream.write(data, encoding, callback);
        } else {
          this.stream.write(data, callback);
        }
      });
    }
    end() {
      this.stream.end();
    }
  }
  var _ril = Object.freeze({
    messageBuffer: Object.freeze({
      create: (encoding) => new MessageBuffer(encoding)
    }),
    applicationJson: Object.freeze({
      encoder: Object.freeze({
        name: "application/json",
        encode: (msg, options) => {
          try {
            return Promise.resolve(Buffer.from(JSON.stringify(msg, undefined, 0), options.charset));
          } catch (err) {
            return Promise.reject(err);
          }
        }
      }),
      decoder: Object.freeze({
        name: "application/json",
        decode: (buffer, options) => {
          try {
            if (buffer instanceof Buffer) {
              return Promise.resolve(JSON.parse(buffer.toString(options.charset)));
            } else {
              return Promise.resolve(JSON.parse(new util_1.TextDecoder(options.charset).decode(buffer)));
            }
          } catch (err) {
            return Promise.reject(err);
          }
        }
      })
    }),
    stream: Object.freeze({
      asReadableStream: (stream) => new ReadableStreamWrapper(stream),
      asWritableStream: (stream) => new WritableStreamWrapper(stream)
    }),
    console,
    timer: Object.freeze({
      setTimeout(callback, ms, ...args) {
        const handle = setTimeout(callback, ms, ...args);
        return { dispose: () => clearTimeout(handle) };
      },
      setImmediate(callback, ...args) {
        const handle = setImmediate(callback, ...args);
        return { dispose: () => clearImmediate(handle) };
      },
      setInterval(callback, ms, ...args) {
        const handle = setInterval(callback, ms, ...args);
        return { dispose: () => clearInterval(handle) };
      }
    })
  });
  function RIL() {
    return _ril;
  }
  (function(RIL2) {
    function install() {
      api_1.RAL.install(_ril);
    }
    RIL2.install = install;
  })(RIL || (RIL = {}));
  exports.default = RIL;
});

// ../../node_modules/.bun/vscode-jsonrpc@8.2.0/node_modules/vscode-jsonrpc/lib/node/main.js
var require_main = __commonJS((exports) => {
  var __createBinding = exports && exports.__createBinding || (Object.create ? function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() {
        return m[k];
      } };
    }
    Object.defineProperty(o, k2, desc);
  } : function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    o[k2] = m[k];
  });
  var __exportStar = exports && exports.__exportStar || function(m, exports2) {
    for (var p in m)
      if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports2, p))
        __createBinding(exports2, m, p);
  };
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.createMessageConnection = exports.createServerSocketTransport = exports.createClientSocketTransport = exports.createServerPipeTransport = exports.createClientPipeTransport = exports.generateRandomPipeName = exports.StreamMessageWriter = exports.StreamMessageReader = exports.SocketMessageWriter = exports.SocketMessageReader = exports.PortMessageWriter = exports.PortMessageReader = exports.IPCMessageWriter = exports.IPCMessageReader = undefined;
  var ril_1 = require_ril();
  ril_1.default.install();
  var path = __require("path");
  var os = __require("os");
  var crypto_1 = __require("crypto");
  var net_1 = __require("net");
  var api_1 = require_api();
  __exportStar(require_api(), exports);

  class IPCMessageReader extends api_1.AbstractMessageReader {
    constructor(process2) {
      super();
      this.process = process2;
      let eventEmitter = this.process;
      eventEmitter.on("error", (error) => this.fireError(error));
      eventEmitter.on("close", () => this.fireClose());
    }
    listen(callback) {
      this.process.on("message", callback);
      return api_1.Disposable.create(() => this.process.off("message", callback));
    }
  }
  exports.IPCMessageReader = IPCMessageReader;

  class IPCMessageWriter extends api_1.AbstractMessageWriter {
    constructor(process2) {
      super();
      this.process = process2;
      this.errorCount = 0;
      const eventEmitter = this.process;
      eventEmitter.on("error", (error) => this.fireError(error));
      eventEmitter.on("close", () => this.fireClose);
    }
    write(msg) {
      try {
        if (typeof this.process.send === "function") {
          this.process.send(msg, undefined, undefined, (error) => {
            if (error) {
              this.errorCount++;
              this.handleError(error, msg);
            } else {
              this.errorCount = 0;
            }
          });
        }
        return Promise.resolve();
      } catch (error) {
        this.handleError(error, msg);
        return Promise.reject(error);
      }
    }
    handleError(error, msg) {
      this.errorCount++;
      this.fireError(error, msg, this.errorCount);
    }
    end() {}
  }
  exports.IPCMessageWriter = IPCMessageWriter;

  class PortMessageReader extends api_1.AbstractMessageReader {
    constructor(port) {
      super();
      this.onData = new api_1.Emitter;
      port.on("close", () => this.fireClose);
      port.on("error", (error) => this.fireError(error));
      port.on("message", (message) => {
        this.onData.fire(message);
      });
    }
    listen(callback) {
      return this.onData.event(callback);
    }
  }
  exports.PortMessageReader = PortMessageReader;

  class PortMessageWriter extends api_1.AbstractMessageWriter {
    constructor(port) {
      super();
      this.port = port;
      this.errorCount = 0;
      port.on("close", () => this.fireClose());
      port.on("error", (error) => this.fireError(error));
    }
    write(msg) {
      try {
        this.port.postMessage(msg);
        return Promise.resolve();
      } catch (error) {
        this.handleError(error, msg);
        return Promise.reject(error);
      }
    }
    handleError(error, msg) {
      this.errorCount++;
      this.fireError(error, msg, this.errorCount);
    }
    end() {}
  }
  exports.PortMessageWriter = PortMessageWriter;

  class SocketMessageReader extends api_1.ReadableStreamMessageReader {
    constructor(socket, encoding = "utf-8") {
      super((0, ril_1.default)().stream.asReadableStream(socket), encoding);
    }
  }
  exports.SocketMessageReader = SocketMessageReader;

  class SocketMessageWriter extends api_1.WriteableStreamMessageWriter {
    constructor(socket, options) {
      super((0, ril_1.default)().stream.asWritableStream(socket), options);
      this.socket = socket;
    }
    dispose() {
      super.dispose();
      this.socket.destroy();
    }
  }
  exports.SocketMessageWriter = SocketMessageWriter;

  class StreamMessageReader extends api_1.ReadableStreamMessageReader {
    constructor(readable, encoding) {
      super((0, ril_1.default)().stream.asReadableStream(readable), encoding);
    }
  }
  exports.StreamMessageReader = StreamMessageReader;

  class StreamMessageWriter extends api_1.WriteableStreamMessageWriter {
    constructor(writable, options) {
      super((0, ril_1.default)().stream.asWritableStream(writable), options);
    }
  }
  exports.StreamMessageWriter = StreamMessageWriter;
  var XDG_RUNTIME_DIR = process.env["XDG_RUNTIME_DIR"];
  var safeIpcPathLengths = new Map([
    ["linux", 107],
    ["darwin", 103]
  ]);
  function generateRandomPipeName() {
    const randomSuffix = (0, crypto_1.randomBytes)(21).toString("hex");
    if (process.platform === "win32") {
      return `\\\\.\\pipe\\vscode-jsonrpc-${randomSuffix}-sock`;
    }
    let result;
    if (XDG_RUNTIME_DIR) {
      result = path.join(XDG_RUNTIME_DIR, `vscode-ipc-${randomSuffix}.sock`);
    } else {
      result = path.join(os.tmpdir(), `vscode-${randomSuffix}.sock`);
    }
    const limit = safeIpcPathLengths.get(process.platform);
    if (limit !== undefined && result.length > limit) {
      (0, ril_1.default)().console.warn(`WARNING: IPC handle "${result}" is longer than ${limit} characters.`);
    }
    return result;
  }
  exports.generateRandomPipeName = generateRandomPipeName;
  function createClientPipeTransport(pipeName, encoding = "utf-8") {
    let connectResolve;
    const connected = new Promise((resolve, _reject) => {
      connectResolve = resolve;
    });
    return new Promise((resolve, reject) => {
      let server = (0, net_1.createServer)((socket) => {
        server.close();
        connectResolve([
          new SocketMessageReader(socket, encoding),
          new SocketMessageWriter(socket, encoding)
        ]);
      });
      server.on("error", reject);
      server.listen(pipeName, () => {
        server.removeListener("error", reject);
        resolve({
          onConnected: () => {
            return connected;
          }
        });
      });
    });
  }
  exports.createClientPipeTransport = createClientPipeTransport;
  function createServerPipeTransport(pipeName, encoding = "utf-8") {
    const socket = (0, net_1.createConnection)(pipeName);
    return [
      new SocketMessageReader(socket, encoding),
      new SocketMessageWriter(socket, encoding)
    ];
  }
  exports.createServerPipeTransport = createServerPipeTransport;
  function createClientSocketTransport(port, encoding = "utf-8") {
    let connectResolve;
    const connected = new Promise((resolve, _reject) => {
      connectResolve = resolve;
    });
    return new Promise((resolve, reject) => {
      const server = (0, net_1.createServer)((socket) => {
        server.close();
        connectResolve([
          new SocketMessageReader(socket, encoding),
          new SocketMessageWriter(socket, encoding)
        ]);
      });
      server.on("error", reject);
      server.listen(port, "127.0.0.1", () => {
        server.removeListener("error", reject);
        resolve({
          onConnected: () => {
            return connected;
          }
        });
      });
    });
  }
  exports.createClientSocketTransport = createClientSocketTransport;
  function createServerSocketTransport(port, encoding = "utf-8") {
    const socket = (0, net_1.createConnection)(port, "127.0.0.1");
    return [
      new SocketMessageReader(socket, encoding),
      new SocketMessageWriter(socket, encoding)
    ];
  }
  exports.createServerSocketTransport = createServerSocketTransport;
  function isReadableStream(value) {
    const candidate = value;
    return candidate.read !== undefined && candidate.addListener !== undefined;
  }
  function isWritableStream(value) {
    const candidate = value;
    return candidate.write !== undefined && candidate.addListener !== undefined;
  }
  function createMessageConnection(input, output, logger, options) {
    if (!logger) {
      logger = api_1.NullLogger;
    }
    const reader = isReadableStream(input) ? new StreamMessageReader(input) : input;
    const writer = isWritableStream(output) ? new StreamMessageWriter(output) : output;
    if (api_1.ConnectionStrategy.is(options)) {
      options = { connectionStrategy: options };
    }
    return (0, api_1.createMessageConnection)(reader, writer, logger, options);
  }
  exports.createMessageConnection = createMessageConnection;
});

// ../../node_modules/.bun/vscode-languageserver-types@3.17.5/node_modules/vscode-languageserver-types/lib/umd/main.js
var require_main2 = __commonJS((exports, module) => {
  (function(factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
      var v = factory(__require, exports);
      if (v !== undefined)
        module.exports = v;
    } else if (typeof define === "function" && define.amd) {
      define(["require", "exports"], factory);
    }
  })(function(require2, exports2) {
    Object.defineProperty(exports2, "__esModule", { value: true });
    exports2.TextDocument = exports2.EOL = exports2.WorkspaceFolder = exports2.InlineCompletionContext = exports2.SelectedCompletionInfo = exports2.InlineCompletionTriggerKind = exports2.InlineCompletionList = exports2.InlineCompletionItem = exports2.StringValue = exports2.InlayHint = exports2.InlayHintLabelPart = exports2.InlayHintKind = exports2.InlineValueContext = exports2.InlineValueEvaluatableExpression = exports2.InlineValueVariableLookup = exports2.InlineValueText = exports2.SemanticTokens = exports2.SemanticTokenModifiers = exports2.SemanticTokenTypes = exports2.SelectionRange = exports2.DocumentLink = exports2.FormattingOptions = exports2.CodeLens = exports2.CodeAction = exports2.CodeActionContext = exports2.CodeActionTriggerKind = exports2.CodeActionKind = exports2.DocumentSymbol = exports2.WorkspaceSymbol = exports2.SymbolInformation = exports2.SymbolTag = exports2.SymbolKind = exports2.DocumentHighlight = exports2.DocumentHighlightKind = exports2.SignatureInformation = exports2.ParameterInformation = exports2.Hover = exports2.MarkedString = exports2.CompletionList = exports2.CompletionItem = exports2.CompletionItemLabelDetails = exports2.InsertTextMode = exports2.InsertReplaceEdit = exports2.CompletionItemTag = exports2.InsertTextFormat = exports2.CompletionItemKind = exports2.MarkupContent = exports2.MarkupKind = exports2.TextDocumentItem = exports2.OptionalVersionedTextDocumentIdentifier = exports2.VersionedTextDocumentIdentifier = exports2.TextDocumentIdentifier = exports2.WorkspaceChange = exports2.WorkspaceEdit = exports2.DeleteFile = exports2.RenameFile = exports2.CreateFile = exports2.TextDocumentEdit = exports2.AnnotatedTextEdit = exports2.ChangeAnnotationIdentifier = exports2.ChangeAnnotation = exports2.TextEdit = exports2.Command = exports2.Diagnostic = exports2.CodeDescription = exports2.DiagnosticTag = exports2.DiagnosticSeverity = exports2.DiagnosticRelatedInformation = exports2.FoldingRange = exports2.FoldingRangeKind = exports2.ColorPresentation = exports2.ColorInformation = exports2.Color = exports2.LocationLink = exports2.Location = exports2.Range = exports2.Position = exports2.uinteger = exports2.integer = exports2.URI = exports2.DocumentUri = undefined;
    var DocumentUri;
    (function(DocumentUri2) {
      function is(value) {
        return typeof value === "string";
      }
      DocumentUri2.is = is;
    })(DocumentUri || (exports2.DocumentUri = DocumentUri = {}));
    var URI;
    (function(URI2) {
      function is(value) {
        return typeof value === "string";
      }
      URI2.is = is;
    })(URI || (exports2.URI = URI = {}));
    var integer;
    (function(integer2) {
      integer2.MIN_VALUE = -2147483648;
      integer2.MAX_VALUE = 2147483647;
      function is(value) {
        return typeof value === "number" && integer2.MIN_VALUE <= value && value <= integer2.MAX_VALUE;
      }
      integer2.is = is;
    })(integer || (exports2.integer = integer = {}));
    var uinteger;
    (function(uinteger2) {
      uinteger2.MIN_VALUE = 0;
      uinteger2.MAX_VALUE = 2147483647;
      function is(value) {
        return typeof value === "number" && uinteger2.MIN_VALUE <= value && value <= uinteger2.MAX_VALUE;
      }
      uinteger2.is = is;
    })(uinteger || (exports2.uinteger = uinteger = {}));
    var Position;
    (function(Position2) {
      function create(line, character) {
        if (line === Number.MAX_VALUE) {
          line = uinteger.MAX_VALUE;
        }
        if (character === Number.MAX_VALUE) {
          character = uinteger.MAX_VALUE;
        }
        return { line, character };
      }
      Position2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.uinteger(candidate.line) && Is.uinteger(candidate.character);
      }
      Position2.is = is;
    })(Position || (exports2.Position = Position = {}));
    var Range;
    (function(Range2) {
      function create(one, two, three, four) {
        if (Is.uinteger(one) && Is.uinteger(two) && Is.uinteger(three) && Is.uinteger(four)) {
          return { start: Position.create(one, two), end: Position.create(three, four) };
        } else if (Position.is(one) && Position.is(two)) {
          return { start: one, end: two };
        } else {
          throw new Error("Range#create called with invalid arguments[".concat(one, ", ").concat(two, ", ").concat(three, ", ").concat(four, "]"));
        }
      }
      Range2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Position.is(candidate.start) && Position.is(candidate.end);
      }
      Range2.is = is;
    })(Range || (exports2.Range = Range = {}));
    var Location;
    (function(Location2) {
      function create(uri, range) {
        return { uri, range };
      }
      Location2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Range.is(candidate.range) && (Is.string(candidate.uri) || Is.undefined(candidate.uri));
      }
      Location2.is = is;
    })(Location || (exports2.Location = Location = {}));
    var LocationLink;
    (function(LocationLink2) {
      function create(targetUri, targetRange, targetSelectionRange, originSelectionRange) {
        return { targetUri, targetRange, targetSelectionRange, originSelectionRange };
      }
      LocationLink2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Range.is(candidate.targetRange) && Is.string(candidate.targetUri) && Range.is(candidate.targetSelectionRange) && (Range.is(candidate.originSelectionRange) || Is.undefined(candidate.originSelectionRange));
      }
      LocationLink2.is = is;
    })(LocationLink || (exports2.LocationLink = LocationLink = {}));
    var Color;
    (function(Color2) {
      function create(red, green, blue, alpha) {
        return {
          red,
          green,
          blue,
          alpha
        };
      }
      Color2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.numberRange(candidate.red, 0, 1) && Is.numberRange(candidate.green, 0, 1) && Is.numberRange(candidate.blue, 0, 1) && Is.numberRange(candidate.alpha, 0, 1);
      }
      Color2.is = is;
    })(Color || (exports2.Color = Color = {}));
    var ColorInformation;
    (function(ColorInformation2) {
      function create(range, color) {
        return {
          range,
          color
        };
      }
      ColorInformation2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Range.is(candidate.range) && Color.is(candidate.color);
      }
      ColorInformation2.is = is;
    })(ColorInformation || (exports2.ColorInformation = ColorInformation = {}));
    var ColorPresentation;
    (function(ColorPresentation2) {
      function create(label, textEdit, additionalTextEdits) {
        return {
          label,
          textEdit,
          additionalTextEdits
        };
      }
      ColorPresentation2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.string(candidate.label) && (Is.undefined(candidate.textEdit) || TextEdit.is(candidate)) && (Is.undefined(candidate.additionalTextEdits) || Is.typedArray(candidate.additionalTextEdits, TextEdit.is));
      }
      ColorPresentation2.is = is;
    })(ColorPresentation || (exports2.ColorPresentation = ColorPresentation = {}));
    var FoldingRangeKind;
    (function(FoldingRangeKind2) {
      FoldingRangeKind2.Comment = "comment";
      FoldingRangeKind2.Imports = "imports";
      FoldingRangeKind2.Region = "region";
    })(FoldingRangeKind || (exports2.FoldingRangeKind = FoldingRangeKind = {}));
    var FoldingRange;
    (function(FoldingRange2) {
      function create(startLine, endLine, startCharacter, endCharacter, kind, collapsedText) {
        var result = {
          startLine,
          endLine
        };
        if (Is.defined(startCharacter)) {
          result.startCharacter = startCharacter;
        }
        if (Is.defined(endCharacter)) {
          result.endCharacter = endCharacter;
        }
        if (Is.defined(kind)) {
          result.kind = kind;
        }
        if (Is.defined(collapsedText)) {
          result.collapsedText = collapsedText;
        }
        return result;
      }
      FoldingRange2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.uinteger(candidate.startLine) && Is.uinteger(candidate.startLine) && (Is.undefined(candidate.startCharacter) || Is.uinteger(candidate.startCharacter)) && (Is.undefined(candidate.endCharacter) || Is.uinteger(candidate.endCharacter)) && (Is.undefined(candidate.kind) || Is.string(candidate.kind));
      }
      FoldingRange2.is = is;
    })(FoldingRange || (exports2.FoldingRange = FoldingRange = {}));
    var DiagnosticRelatedInformation;
    (function(DiagnosticRelatedInformation2) {
      function create(location, message) {
        return {
          location,
          message
        };
      }
      DiagnosticRelatedInformation2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Location.is(candidate.location) && Is.string(candidate.message);
      }
      DiagnosticRelatedInformation2.is = is;
    })(DiagnosticRelatedInformation || (exports2.DiagnosticRelatedInformation = DiagnosticRelatedInformation = {}));
    var DiagnosticSeverity;
    (function(DiagnosticSeverity2) {
      DiagnosticSeverity2.Error = 1;
      DiagnosticSeverity2.Warning = 2;
      DiagnosticSeverity2.Information = 3;
      DiagnosticSeverity2.Hint = 4;
    })(DiagnosticSeverity || (exports2.DiagnosticSeverity = DiagnosticSeverity = {}));
    var DiagnosticTag;
    (function(DiagnosticTag2) {
      DiagnosticTag2.Unnecessary = 1;
      DiagnosticTag2.Deprecated = 2;
    })(DiagnosticTag || (exports2.DiagnosticTag = DiagnosticTag = {}));
    var CodeDescription;
    (function(CodeDescription2) {
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.string(candidate.href);
      }
      CodeDescription2.is = is;
    })(CodeDescription || (exports2.CodeDescription = CodeDescription = {}));
    var Diagnostic;
    (function(Diagnostic2) {
      function create(range, message, severity, code, source, relatedInformation) {
        var result = { range, message };
        if (Is.defined(severity)) {
          result.severity = severity;
        }
        if (Is.defined(code)) {
          result.code = code;
        }
        if (Is.defined(source)) {
          result.source = source;
        }
        if (Is.defined(relatedInformation)) {
          result.relatedInformation = relatedInformation;
        }
        return result;
      }
      Diagnostic2.create = create;
      function is(value) {
        var _a;
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && Is.string(candidate.message) && (Is.number(candidate.severity) || Is.undefined(candidate.severity)) && (Is.integer(candidate.code) || Is.string(candidate.code) || Is.undefined(candidate.code)) && (Is.undefined(candidate.codeDescription) || Is.string((_a = candidate.codeDescription) === null || _a === undefined ? undefined : _a.href)) && (Is.string(candidate.source) || Is.undefined(candidate.source)) && (Is.undefined(candidate.relatedInformation) || Is.typedArray(candidate.relatedInformation, DiagnosticRelatedInformation.is));
      }
      Diagnostic2.is = is;
    })(Diagnostic || (exports2.Diagnostic = Diagnostic = {}));
    var Command;
    (function(Command2) {
      function create(title, command) {
        var args = [];
        for (var _i = 2;_i < arguments.length; _i++) {
          args[_i - 2] = arguments[_i];
        }
        var result = { title, command };
        if (Is.defined(args) && args.length > 0) {
          result.arguments = args;
        }
        return result;
      }
      Command2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.title) && Is.string(candidate.command);
      }
      Command2.is = is;
    })(Command || (exports2.Command = Command = {}));
    var TextEdit;
    (function(TextEdit2) {
      function replace(range, newText) {
        return { range, newText };
      }
      TextEdit2.replace = replace;
      function insert(position, newText) {
        return { range: { start: position, end: position }, newText };
      }
      TextEdit2.insert = insert;
      function del(range) {
        return { range, newText: "" };
      }
      TextEdit2.del = del;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.string(candidate.newText) && Range.is(candidate.range);
      }
      TextEdit2.is = is;
    })(TextEdit || (exports2.TextEdit = TextEdit = {}));
    var ChangeAnnotation;
    (function(ChangeAnnotation2) {
      function create(label, needsConfirmation, description) {
        var result = { label };
        if (needsConfirmation !== undefined) {
          result.needsConfirmation = needsConfirmation;
        }
        if (description !== undefined) {
          result.description = description;
        }
        return result;
      }
      ChangeAnnotation2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.string(candidate.label) && (Is.boolean(candidate.needsConfirmation) || candidate.needsConfirmation === undefined) && (Is.string(candidate.description) || candidate.description === undefined);
      }
      ChangeAnnotation2.is = is;
    })(ChangeAnnotation || (exports2.ChangeAnnotation = ChangeAnnotation = {}));
    var ChangeAnnotationIdentifier;
    (function(ChangeAnnotationIdentifier2) {
      function is(value) {
        var candidate = value;
        return Is.string(candidate);
      }
      ChangeAnnotationIdentifier2.is = is;
    })(ChangeAnnotationIdentifier || (exports2.ChangeAnnotationIdentifier = ChangeAnnotationIdentifier = {}));
    var AnnotatedTextEdit;
    (function(AnnotatedTextEdit2) {
      function replace(range, newText, annotation) {
        return { range, newText, annotationId: annotation };
      }
      AnnotatedTextEdit2.replace = replace;
      function insert(position, newText, annotation) {
        return { range: { start: position, end: position }, newText, annotationId: annotation };
      }
      AnnotatedTextEdit2.insert = insert;
      function del(range, annotation) {
        return { range, newText: "", annotationId: annotation };
      }
      AnnotatedTextEdit2.del = del;
      function is(value) {
        var candidate = value;
        return TextEdit.is(candidate) && (ChangeAnnotation.is(candidate.annotationId) || ChangeAnnotationIdentifier.is(candidate.annotationId));
      }
      AnnotatedTextEdit2.is = is;
    })(AnnotatedTextEdit || (exports2.AnnotatedTextEdit = AnnotatedTextEdit = {}));
    var TextDocumentEdit;
    (function(TextDocumentEdit2) {
      function create(textDocument, edits) {
        return { textDocument, edits };
      }
      TextDocumentEdit2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && OptionalVersionedTextDocumentIdentifier.is(candidate.textDocument) && Array.isArray(candidate.edits);
      }
      TextDocumentEdit2.is = is;
    })(TextDocumentEdit || (exports2.TextDocumentEdit = TextDocumentEdit = {}));
    var CreateFile;
    (function(CreateFile2) {
      function create(uri, options, annotation) {
        var result = {
          kind: "create",
          uri
        };
        if (options !== undefined && (options.overwrite !== undefined || options.ignoreIfExists !== undefined)) {
          result.options = options;
        }
        if (annotation !== undefined) {
          result.annotationId = annotation;
        }
        return result;
      }
      CreateFile2.create = create;
      function is(value) {
        var candidate = value;
        return candidate && candidate.kind === "create" && Is.string(candidate.uri) && (candidate.options === undefined || (candidate.options.overwrite === undefined || Is.boolean(candidate.options.overwrite)) && (candidate.options.ignoreIfExists === undefined || Is.boolean(candidate.options.ignoreIfExists))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
      }
      CreateFile2.is = is;
    })(CreateFile || (exports2.CreateFile = CreateFile = {}));
    var RenameFile;
    (function(RenameFile2) {
      function create(oldUri, newUri, options, annotation) {
        var result = {
          kind: "rename",
          oldUri,
          newUri
        };
        if (options !== undefined && (options.overwrite !== undefined || options.ignoreIfExists !== undefined)) {
          result.options = options;
        }
        if (annotation !== undefined) {
          result.annotationId = annotation;
        }
        return result;
      }
      RenameFile2.create = create;
      function is(value) {
        var candidate = value;
        return candidate && candidate.kind === "rename" && Is.string(candidate.oldUri) && Is.string(candidate.newUri) && (candidate.options === undefined || (candidate.options.overwrite === undefined || Is.boolean(candidate.options.overwrite)) && (candidate.options.ignoreIfExists === undefined || Is.boolean(candidate.options.ignoreIfExists))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
      }
      RenameFile2.is = is;
    })(RenameFile || (exports2.RenameFile = RenameFile = {}));
    var DeleteFile;
    (function(DeleteFile2) {
      function create(uri, options, annotation) {
        var result = {
          kind: "delete",
          uri
        };
        if (options !== undefined && (options.recursive !== undefined || options.ignoreIfNotExists !== undefined)) {
          result.options = options;
        }
        if (annotation !== undefined) {
          result.annotationId = annotation;
        }
        return result;
      }
      DeleteFile2.create = create;
      function is(value) {
        var candidate = value;
        return candidate && candidate.kind === "delete" && Is.string(candidate.uri) && (candidate.options === undefined || (candidate.options.recursive === undefined || Is.boolean(candidate.options.recursive)) && (candidate.options.ignoreIfNotExists === undefined || Is.boolean(candidate.options.ignoreIfNotExists))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
      }
      DeleteFile2.is = is;
    })(DeleteFile || (exports2.DeleteFile = DeleteFile = {}));
    var WorkspaceEdit;
    (function(WorkspaceEdit2) {
      function is(value) {
        var candidate = value;
        return candidate && (candidate.changes !== undefined || candidate.documentChanges !== undefined) && (candidate.documentChanges === undefined || candidate.documentChanges.every(function(change) {
          if (Is.string(change.kind)) {
            return CreateFile.is(change) || RenameFile.is(change) || DeleteFile.is(change);
          } else {
            return TextDocumentEdit.is(change);
          }
        }));
      }
      WorkspaceEdit2.is = is;
    })(WorkspaceEdit || (exports2.WorkspaceEdit = WorkspaceEdit = {}));
    var TextEditChangeImpl = function() {
      function TextEditChangeImpl2(edits, changeAnnotations) {
        this.edits = edits;
        this.changeAnnotations = changeAnnotations;
      }
      TextEditChangeImpl2.prototype.insert = function(position, newText, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
          edit = TextEdit.insert(position, newText);
        } else if (ChangeAnnotationIdentifier.is(annotation)) {
          id = annotation;
          edit = AnnotatedTextEdit.insert(position, newText, annotation);
        } else {
          this.assertChangeAnnotations(this.changeAnnotations);
          id = this.changeAnnotations.manage(annotation);
          edit = AnnotatedTextEdit.insert(position, newText, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
          return id;
        }
      };
      TextEditChangeImpl2.prototype.replace = function(range, newText, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
          edit = TextEdit.replace(range, newText);
        } else if (ChangeAnnotationIdentifier.is(annotation)) {
          id = annotation;
          edit = AnnotatedTextEdit.replace(range, newText, annotation);
        } else {
          this.assertChangeAnnotations(this.changeAnnotations);
          id = this.changeAnnotations.manage(annotation);
          edit = AnnotatedTextEdit.replace(range, newText, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
          return id;
        }
      };
      TextEditChangeImpl2.prototype.delete = function(range, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
          edit = TextEdit.del(range);
        } else if (ChangeAnnotationIdentifier.is(annotation)) {
          id = annotation;
          edit = AnnotatedTextEdit.del(range, annotation);
        } else {
          this.assertChangeAnnotations(this.changeAnnotations);
          id = this.changeAnnotations.manage(annotation);
          edit = AnnotatedTextEdit.del(range, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
          return id;
        }
      };
      TextEditChangeImpl2.prototype.add = function(edit) {
        this.edits.push(edit);
      };
      TextEditChangeImpl2.prototype.all = function() {
        return this.edits;
      };
      TextEditChangeImpl2.prototype.clear = function() {
        this.edits.splice(0, this.edits.length);
      };
      TextEditChangeImpl2.prototype.assertChangeAnnotations = function(value) {
        if (value === undefined) {
          throw new Error("Text edit change is not configured to manage change annotations.");
        }
      };
      return TextEditChangeImpl2;
    }();
    var ChangeAnnotations = function() {
      function ChangeAnnotations2(annotations) {
        this._annotations = annotations === undefined ? Object.create(null) : annotations;
        this._counter = 0;
        this._size = 0;
      }
      ChangeAnnotations2.prototype.all = function() {
        return this._annotations;
      };
      Object.defineProperty(ChangeAnnotations2.prototype, "size", {
        get: function() {
          return this._size;
        },
        enumerable: false,
        configurable: true
      });
      ChangeAnnotations2.prototype.manage = function(idOrAnnotation, annotation) {
        var id;
        if (ChangeAnnotationIdentifier.is(idOrAnnotation)) {
          id = idOrAnnotation;
        } else {
          id = this.nextId();
          annotation = idOrAnnotation;
        }
        if (this._annotations[id] !== undefined) {
          throw new Error("Id ".concat(id, " is already in use."));
        }
        if (annotation === undefined) {
          throw new Error("No annotation provided for id ".concat(id));
        }
        this._annotations[id] = annotation;
        this._size++;
        return id;
      };
      ChangeAnnotations2.prototype.nextId = function() {
        this._counter++;
        return this._counter.toString();
      };
      return ChangeAnnotations2;
    }();
    var WorkspaceChange = function() {
      function WorkspaceChange2(workspaceEdit) {
        var _this = this;
        this._textEditChanges = Object.create(null);
        if (workspaceEdit !== undefined) {
          this._workspaceEdit = workspaceEdit;
          if (workspaceEdit.documentChanges) {
            this._changeAnnotations = new ChangeAnnotations(workspaceEdit.changeAnnotations);
            workspaceEdit.changeAnnotations = this._changeAnnotations.all();
            workspaceEdit.documentChanges.forEach(function(change) {
              if (TextDocumentEdit.is(change)) {
                var textEditChange = new TextEditChangeImpl(change.edits, _this._changeAnnotations);
                _this._textEditChanges[change.textDocument.uri] = textEditChange;
              }
            });
          } else if (workspaceEdit.changes) {
            Object.keys(workspaceEdit.changes).forEach(function(key) {
              var textEditChange = new TextEditChangeImpl(workspaceEdit.changes[key]);
              _this._textEditChanges[key] = textEditChange;
            });
          }
        } else {
          this._workspaceEdit = {};
        }
      }
      Object.defineProperty(WorkspaceChange2.prototype, "edit", {
        get: function() {
          this.initDocumentChanges();
          if (this._changeAnnotations !== undefined) {
            if (this._changeAnnotations.size === 0) {
              this._workspaceEdit.changeAnnotations = undefined;
            } else {
              this._workspaceEdit.changeAnnotations = this._changeAnnotations.all();
            }
          }
          return this._workspaceEdit;
        },
        enumerable: false,
        configurable: true
      });
      WorkspaceChange2.prototype.getTextEditChange = function(key) {
        if (OptionalVersionedTextDocumentIdentifier.is(key)) {
          this.initDocumentChanges();
          if (this._workspaceEdit.documentChanges === undefined) {
            throw new Error("Workspace edit is not configured for document changes.");
          }
          var textDocument = { uri: key.uri, version: key.version };
          var result = this._textEditChanges[textDocument.uri];
          if (!result) {
            var edits = [];
            var textDocumentEdit = {
              textDocument,
              edits
            };
            this._workspaceEdit.documentChanges.push(textDocumentEdit);
            result = new TextEditChangeImpl(edits, this._changeAnnotations);
            this._textEditChanges[textDocument.uri] = result;
          }
          return result;
        } else {
          this.initChanges();
          if (this._workspaceEdit.changes === undefined) {
            throw new Error("Workspace edit is not configured for normal text edit changes.");
          }
          var result = this._textEditChanges[key];
          if (!result) {
            var edits = [];
            this._workspaceEdit.changes[key] = edits;
            result = new TextEditChangeImpl(edits);
            this._textEditChanges[key] = result;
          }
          return result;
        }
      };
      WorkspaceChange2.prototype.initDocumentChanges = function() {
        if (this._workspaceEdit.documentChanges === undefined && this._workspaceEdit.changes === undefined) {
          this._changeAnnotations = new ChangeAnnotations;
          this._workspaceEdit.documentChanges = [];
          this._workspaceEdit.changeAnnotations = this._changeAnnotations.all();
        }
      };
      WorkspaceChange2.prototype.initChanges = function() {
        if (this._workspaceEdit.documentChanges === undefined && this._workspaceEdit.changes === undefined) {
          this._workspaceEdit.changes = Object.create(null);
        }
      };
      WorkspaceChange2.prototype.createFile = function(uri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
          throw new Error("Workspace edit is not configured for document changes.");
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
          annotation = optionsOrAnnotation;
        } else {
          options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
          operation = CreateFile.create(uri, options);
        } else {
          id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
          operation = CreateFile.create(uri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
          return id;
        }
      };
      WorkspaceChange2.prototype.renameFile = function(oldUri, newUri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
          throw new Error("Workspace edit is not configured for document changes.");
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
          annotation = optionsOrAnnotation;
        } else {
          options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
          operation = RenameFile.create(oldUri, newUri, options);
        } else {
          id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
          operation = RenameFile.create(oldUri, newUri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
          return id;
        }
      };
      WorkspaceChange2.prototype.deleteFile = function(uri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
          throw new Error("Workspace edit is not configured for document changes.");
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
          annotation = optionsOrAnnotation;
        } else {
          options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
          operation = DeleteFile.create(uri, options);
        } else {
          id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
          operation = DeleteFile.create(uri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
          return id;
        }
      };
      return WorkspaceChange2;
    }();
    exports2.WorkspaceChange = WorkspaceChange;
    var TextDocumentIdentifier;
    (function(TextDocumentIdentifier2) {
      function create(uri) {
        return { uri };
      }
      TextDocumentIdentifier2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri);
      }
      TextDocumentIdentifier2.is = is;
    })(TextDocumentIdentifier || (exports2.TextDocumentIdentifier = TextDocumentIdentifier = {}));
    var VersionedTextDocumentIdentifier;
    (function(VersionedTextDocumentIdentifier2) {
      function create(uri, version) {
        return { uri, version };
      }
      VersionedTextDocumentIdentifier2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && Is.integer(candidate.version);
      }
      VersionedTextDocumentIdentifier2.is = is;
    })(VersionedTextDocumentIdentifier || (exports2.VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier = {}));
    var OptionalVersionedTextDocumentIdentifier;
    (function(OptionalVersionedTextDocumentIdentifier2) {
      function create(uri, version) {
        return { uri, version };
      }
      OptionalVersionedTextDocumentIdentifier2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && (candidate.version === null || Is.integer(candidate.version));
      }
      OptionalVersionedTextDocumentIdentifier2.is = is;
    })(OptionalVersionedTextDocumentIdentifier || (exports2.OptionalVersionedTextDocumentIdentifier = OptionalVersionedTextDocumentIdentifier = {}));
    var TextDocumentItem;
    (function(TextDocumentItem2) {
      function create(uri, languageId, version, text) {
        return { uri, languageId, version, text };
      }
      TextDocumentItem2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && Is.string(candidate.languageId) && Is.integer(candidate.version) && Is.string(candidate.text);
      }
      TextDocumentItem2.is = is;
    })(TextDocumentItem || (exports2.TextDocumentItem = TextDocumentItem = {}));
    var MarkupKind;
    (function(MarkupKind2) {
      MarkupKind2.PlainText = "plaintext";
      MarkupKind2.Markdown = "markdown";
      function is(value) {
        var candidate = value;
        return candidate === MarkupKind2.PlainText || candidate === MarkupKind2.Markdown;
      }
      MarkupKind2.is = is;
    })(MarkupKind || (exports2.MarkupKind = MarkupKind = {}));
    var MarkupContent;
    (function(MarkupContent2) {
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(value) && MarkupKind.is(candidate.kind) && Is.string(candidate.value);
      }
      MarkupContent2.is = is;
    })(MarkupContent || (exports2.MarkupContent = MarkupContent = {}));
    var CompletionItemKind;
    (function(CompletionItemKind2) {
      CompletionItemKind2.Text = 1;
      CompletionItemKind2.Method = 2;
      CompletionItemKind2.Function = 3;
      CompletionItemKind2.Constructor = 4;
      CompletionItemKind2.Field = 5;
      CompletionItemKind2.Variable = 6;
      CompletionItemKind2.Class = 7;
      CompletionItemKind2.Interface = 8;
      CompletionItemKind2.Module = 9;
      CompletionItemKind2.Property = 10;
      CompletionItemKind2.Unit = 11;
      CompletionItemKind2.Value = 12;
      CompletionItemKind2.Enum = 13;
      CompletionItemKind2.Keyword = 14;
      CompletionItemKind2.Snippet = 15;
      CompletionItemKind2.Color = 16;
      CompletionItemKind2.File = 17;
      CompletionItemKind2.Reference = 18;
      CompletionItemKind2.Folder = 19;
      CompletionItemKind2.EnumMember = 20;
      CompletionItemKind2.Constant = 21;
      CompletionItemKind2.Struct = 22;
      CompletionItemKind2.Event = 23;
      CompletionItemKind2.Operator = 24;
      CompletionItemKind2.TypeParameter = 25;
    })(CompletionItemKind || (exports2.CompletionItemKind = CompletionItemKind = {}));
    var InsertTextFormat;
    (function(InsertTextFormat2) {
      InsertTextFormat2.PlainText = 1;
      InsertTextFormat2.Snippet = 2;
    })(InsertTextFormat || (exports2.InsertTextFormat = InsertTextFormat = {}));
    var CompletionItemTag;
    (function(CompletionItemTag2) {
      CompletionItemTag2.Deprecated = 1;
    })(CompletionItemTag || (exports2.CompletionItemTag = CompletionItemTag = {}));
    var InsertReplaceEdit;
    (function(InsertReplaceEdit2) {
      function create(newText, insert, replace) {
        return { newText, insert, replace };
      }
      InsertReplaceEdit2.create = create;
      function is(value) {
        var candidate = value;
        return candidate && Is.string(candidate.newText) && Range.is(candidate.insert) && Range.is(candidate.replace);
      }
      InsertReplaceEdit2.is = is;
    })(InsertReplaceEdit || (exports2.InsertReplaceEdit = InsertReplaceEdit = {}));
    var InsertTextMode;
    (function(InsertTextMode2) {
      InsertTextMode2.asIs = 1;
      InsertTextMode2.adjustIndentation = 2;
    })(InsertTextMode || (exports2.InsertTextMode = InsertTextMode = {}));
    var CompletionItemLabelDetails;
    (function(CompletionItemLabelDetails2) {
      function is(value) {
        var candidate = value;
        return candidate && (Is.string(candidate.detail) || candidate.detail === undefined) && (Is.string(candidate.description) || candidate.description === undefined);
      }
      CompletionItemLabelDetails2.is = is;
    })(CompletionItemLabelDetails || (exports2.CompletionItemLabelDetails = CompletionItemLabelDetails = {}));
    var CompletionItem;
    (function(CompletionItem2) {
      function create(label) {
        return { label };
      }
      CompletionItem2.create = create;
    })(CompletionItem || (exports2.CompletionItem = CompletionItem = {}));
    var CompletionList;
    (function(CompletionList2) {
      function create(items, isIncomplete) {
        return { items: items ? items : [], isIncomplete: !!isIncomplete };
      }
      CompletionList2.create = create;
    })(CompletionList || (exports2.CompletionList = CompletionList = {}));
    var MarkedString;
    (function(MarkedString2) {
      function fromPlainText(plainText) {
        return plainText.replace(/[\\`*_{}[\]()#+\-.!]/g, "\\$&");
      }
      MarkedString2.fromPlainText = fromPlainText;
      function is(value) {
        var candidate = value;
        return Is.string(candidate) || Is.objectLiteral(candidate) && Is.string(candidate.language) && Is.string(candidate.value);
      }
      MarkedString2.is = is;
    })(MarkedString || (exports2.MarkedString = MarkedString = {}));
    var Hover;
    (function(Hover2) {
      function is(value) {
        var candidate = value;
        return !!candidate && Is.objectLiteral(candidate) && (MarkupContent.is(candidate.contents) || MarkedString.is(candidate.contents) || Is.typedArray(candidate.contents, MarkedString.is)) && (value.range === undefined || Range.is(value.range));
      }
      Hover2.is = is;
    })(Hover || (exports2.Hover = Hover = {}));
    var ParameterInformation;
    (function(ParameterInformation2) {
      function create(label, documentation) {
        return documentation ? { label, documentation } : { label };
      }
      ParameterInformation2.create = create;
    })(ParameterInformation || (exports2.ParameterInformation = ParameterInformation = {}));
    var SignatureInformation;
    (function(SignatureInformation2) {
      function create(label, documentation) {
        var parameters = [];
        for (var _i = 2;_i < arguments.length; _i++) {
          parameters[_i - 2] = arguments[_i];
        }
        var result = { label };
        if (Is.defined(documentation)) {
          result.documentation = documentation;
        }
        if (Is.defined(parameters)) {
          result.parameters = parameters;
        } else {
          result.parameters = [];
        }
        return result;
      }
      SignatureInformation2.create = create;
    })(SignatureInformation || (exports2.SignatureInformation = SignatureInformation = {}));
    var DocumentHighlightKind;
    (function(DocumentHighlightKind2) {
      DocumentHighlightKind2.Text = 1;
      DocumentHighlightKind2.Read = 2;
      DocumentHighlightKind2.Write = 3;
    })(DocumentHighlightKind || (exports2.DocumentHighlightKind = DocumentHighlightKind = {}));
    var DocumentHighlight;
    (function(DocumentHighlight2) {
      function create(range, kind) {
        var result = { range };
        if (Is.number(kind)) {
          result.kind = kind;
        }
        return result;
      }
      DocumentHighlight2.create = create;
    })(DocumentHighlight || (exports2.DocumentHighlight = DocumentHighlight = {}));
    var SymbolKind;
    (function(SymbolKind2) {
      SymbolKind2.File = 1;
      SymbolKind2.Module = 2;
      SymbolKind2.Namespace = 3;
      SymbolKind2.Package = 4;
      SymbolKind2.Class = 5;
      SymbolKind2.Method = 6;
      SymbolKind2.Property = 7;
      SymbolKind2.Field = 8;
      SymbolKind2.Constructor = 9;
      SymbolKind2.Enum = 10;
      SymbolKind2.Interface = 11;
      SymbolKind2.Function = 12;
      SymbolKind2.Variable = 13;
      SymbolKind2.Constant = 14;
      SymbolKind2.String = 15;
      SymbolKind2.Number = 16;
      SymbolKind2.Boolean = 17;
      SymbolKind2.Array = 18;
      SymbolKind2.Object = 19;
      SymbolKind2.Key = 20;
      SymbolKind2.Null = 21;
      SymbolKind2.EnumMember = 22;
      SymbolKind2.Struct = 23;
      SymbolKind2.Event = 24;
      SymbolKind2.Operator = 25;
      SymbolKind2.TypeParameter = 26;
    })(SymbolKind || (exports2.SymbolKind = SymbolKind = {}));
    var SymbolTag;
    (function(SymbolTag2) {
      SymbolTag2.Deprecated = 1;
    })(SymbolTag || (exports2.SymbolTag = SymbolTag = {}));
    var SymbolInformation;
    (function(SymbolInformation2) {
      function create(name, kind, range, uri, containerName) {
        var result = {
          name,
          kind,
          location: { uri, range }
        };
        if (containerName) {
          result.containerName = containerName;
        }
        return result;
      }
      SymbolInformation2.create = create;
    })(SymbolInformation || (exports2.SymbolInformation = SymbolInformation = {}));
    var WorkspaceSymbol;
    (function(WorkspaceSymbol2) {
      function create(name, kind, uri, range) {
        return range !== undefined ? { name, kind, location: { uri, range } } : { name, kind, location: { uri } };
      }
      WorkspaceSymbol2.create = create;
    })(WorkspaceSymbol || (exports2.WorkspaceSymbol = WorkspaceSymbol = {}));
    var DocumentSymbol;
    (function(DocumentSymbol2) {
      function create(name, detail, kind, range, selectionRange, children) {
        var result = {
          name,
          detail,
          kind,
          range,
          selectionRange
        };
        if (children !== undefined) {
          result.children = children;
        }
        return result;
      }
      DocumentSymbol2.create = create;
      function is(value) {
        var candidate = value;
        return candidate && Is.string(candidate.name) && Is.number(candidate.kind) && Range.is(candidate.range) && Range.is(candidate.selectionRange) && (candidate.detail === undefined || Is.string(candidate.detail)) && (candidate.deprecated === undefined || Is.boolean(candidate.deprecated)) && (candidate.children === undefined || Array.isArray(candidate.children)) && (candidate.tags === undefined || Array.isArray(candidate.tags));
      }
      DocumentSymbol2.is = is;
    })(DocumentSymbol || (exports2.DocumentSymbol = DocumentSymbol = {}));
    var CodeActionKind;
    (function(CodeActionKind2) {
      CodeActionKind2.Empty = "";
      CodeActionKind2.QuickFix = "quickfix";
      CodeActionKind2.Refactor = "refactor";
      CodeActionKind2.RefactorExtract = "refactor.extract";
      CodeActionKind2.RefactorInline = "refactor.inline";
      CodeActionKind2.RefactorRewrite = "refactor.rewrite";
      CodeActionKind2.Source = "source";
      CodeActionKind2.SourceOrganizeImports = "source.organizeImports";
      CodeActionKind2.SourceFixAll = "source.fixAll";
    })(CodeActionKind || (exports2.CodeActionKind = CodeActionKind = {}));
    var CodeActionTriggerKind;
    (function(CodeActionTriggerKind2) {
      CodeActionTriggerKind2.Invoked = 1;
      CodeActionTriggerKind2.Automatic = 2;
    })(CodeActionTriggerKind || (exports2.CodeActionTriggerKind = CodeActionTriggerKind = {}));
    var CodeActionContext;
    (function(CodeActionContext2) {
      function create(diagnostics, only, triggerKind) {
        var result = { diagnostics };
        if (only !== undefined && only !== null) {
          result.only = only;
        }
        if (triggerKind !== undefined && triggerKind !== null) {
          result.triggerKind = triggerKind;
        }
        return result;
      }
      CodeActionContext2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.typedArray(candidate.diagnostics, Diagnostic.is) && (candidate.only === undefined || Is.typedArray(candidate.only, Is.string)) && (candidate.triggerKind === undefined || candidate.triggerKind === CodeActionTriggerKind.Invoked || candidate.triggerKind === CodeActionTriggerKind.Automatic);
      }
      CodeActionContext2.is = is;
    })(CodeActionContext || (exports2.CodeActionContext = CodeActionContext = {}));
    var CodeAction;
    (function(CodeAction2) {
      function create(title, kindOrCommandOrEdit, kind) {
        var result = { title };
        var checkKind = true;
        if (typeof kindOrCommandOrEdit === "string") {
          checkKind = false;
          result.kind = kindOrCommandOrEdit;
        } else if (Command.is(kindOrCommandOrEdit)) {
          result.command = kindOrCommandOrEdit;
        } else {
          result.edit = kindOrCommandOrEdit;
        }
        if (checkKind && kind !== undefined) {
          result.kind = kind;
        }
        return result;
      }
      CodeAction2.create = create;
      function is(value) {
        var candidate = value;
        return candidate && Is.string(candidate.title) && (candidate.diagnostics === undefined || Is.typedArray(candidate.diagnostics, Diagnostic.is)) && (candidate.kind === undefined || Is.string(candidate.kind)) && (candidate.edit !== undefined || candidate.command !== undefined) && (candidate.command === undefined || Command.is(candidate.command)) && (candidate.isPreferred === undefined || Is.boolean(candidate.isPreferred)) && (candidate.edit === undefined || WorkspaceEdit.is(candidate.edit));
      }
      CodeAction2.is = is;
    })(CodeAction || (exports2.CodeAction = CodeAction = {}));
    var CodeLens;
    (function(CodeLens2) {
      function create(range, data) {
        var result = { range };
        if (Is.defined(data)) {
          result.data = data;
        }
        return result;
      }
      CodeLens2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.undefined(candidate.command) || Command.is(candidate.command));
      }
      CodeLens2.is = is;
    })(CodeLens || (exports2.CodeLens = CodeLens = {}));
    var FormattingOptions;
    (function(FormattingOptions2) {
      function create(tabSize, insertSpaces) {
        return { tabSize, insertSpaces };
      }
      FormattingOptions2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.uinteger(candidate.tabSize) && Is.boolean(candidate.insertSpaces);
      }
      FormattingOptions2.is = is;
    })(FormattingOptions || (exports2.FormattingOptions = FormattingOptions = {}));
    var DocumentLink;
    (function(DocumentLink2) {
      function create(range, target, data) {
        return { range, target, data };
      }
      DocumentLink2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.undefined(candidate.target) || Is.string(candidate.target));
      }
      DocumentLink2.is = is;
    })(DocumentLink || (exports2.DocumentLink = DocumentLink = {}));
    var SelectionRange;
    (function(SelectionRange2) {
      function create(range, parent) {
        return { range, parent };
      }
      SelectionRange2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Range.is(candidate.range) && (candidate.parent === undefined || SelectionRange2.is(candidate.parent));
      }
      SelectionRange2.is = is;
    })(SelectionRange || (exports2.SelectionRange = SelectionRange = {}));
    var SemanticTokenTypes;
    (function(SemanticTokenTypes2) {
      SemanticTokenTypes2["namespace"] = "namespace";
      SemanticTokenTypes2["type"] = "type";
      SemanticTokenTypes2["class"] = "class";
      SemanticTokenTypes2["enum"] = "enum";
      SemanticTokenTypes2["interface"] = "interface";
      SemanticTokenTypes2["struct"] = "struct";
      SemanticTokenTypes2["typeParameter"] = "typeParameter";
      SemanticTokenTypes2["parameter"] = "parameter";
      SemanticTokenTypes2["variable"] = "variable";
      SemanticTokenTypes2["property"] = "property";
      SemanticTokenTypes2["enumMember"] = "enumMember";
      SemanticTokenTypes2["event"] = "event";
      SemanticTokenTypes2["function"] = "function";
      SemanticTokenTypes2["method"] = "method";
      SemanticTokenTypes2["macro"] = "macro";
      SemanticTokenTypes2["keyword"] = "keyword";
      SemanticTokenTypes2["modifier"] = "modifier";
      SemanticTokenTypes2["comment"] = "comment";
      SemanticTokenTypes2["string"] = "string";
      SemanticTokenTypes2["number"] = "number";
      SemanticTokenTypes2["regexp"] = "regexp";
      SemanticTokenTypes2["operator"] = "operator";
      SemanticTokenTypes2["decorator"] = "decorator";
    })(SemanticTokenTypes || (exports2.SemanticTokenTypes = SemanticTokenTypes = {}));
    var SemanticTokenModifiers;
    (function(SemanticTokenModifiers2) {
      SemanticTokenModifiers2["declaration"] = "declaration";
      SemanticTokenModifiers2["definition"] = "definition";
      SemanticTokenModifiers2["readonly"] = "readonly";
      SemanticTokenModifiers2["static"] = "static";
      SemanticTokenModifiers2["deprecated"] = "deprecated";
      SemanticTokenModifiers2["abstract"] = "abstract";
      SemanticTokenModifiers2["async"] = "async";
      SemanticTokenModifiers2["modification"] = "modification";
      SemanticTokenModifiers2["documentation"] = "documentation";
      SemanticTokenModifiers2["defaultLibrary"] = "defaultLibrary";
    })(SemanticTokenModifiers || (exports2.SemanticTokenModifiers = SemanticTokenModifiers = {}));
    var SemanticTokens;
    (function(SemanticTokens2) {
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && (candidate.resultId === undefined || typeof candidate.resultId === "string") && Array.isArray(candidate.data) && (candidate.data.length === 0 || typeof candidate.data[0] === "number");
      }
      SemanticTokens2.is = is;
    })(SemanticTokens || (exports2.SemanticTokens = SemanticTokens = {}));
    var InlineValueText;
    (function(InlineValueText2) {
      function create(range, text) {
        return { range, text };
      }
      InlineValueText2.create = create;
      function is(value) {
        var candidate = value;
        return candidate !== undefined && candidate !== null && Range.is(candidate.range) && Is.string(candidate.text);
      }
      InlineValueText2.is = is;
    })(InlineValueText || (exports2.InlineValueText = InlineValueText = {}));
    var InlineValueVariableLookup;
    (function(InlineValueVariableLookup2) {
      function create(range, variableName, caseSensitiveLookup) {
        return { range, variableName, caseSensitiveLookup };
      }
      InlineValueVariableLookup2.create = create;
      function is(value) {
        var candidate = value;
        return candidate !== undefined && candidate !== null && Range.is(candidate.range) && Is.boolean(candidate.caseSensitiveLookup) && (Is.string(candidate.variableName) || candidate.variableName === undefined);
      }
      InlineValueVariableLookup2.is = is;
    })(InlineValueVariableLookup || (exports2.InlineValueVariableLookup = InlineValueVariableLookup = {}));
    var InlineValueEvaluatableExpression;
    (function(InlineValueEvaluatableExpression2) {
      function create(range, expression) {
        return { range, expression };
      }
      InlineValueEvaluatableExpression2.create = create;
      function is(value) {
        var candidate = value;
        return candidate !== undefined && candidate !== null && Range.is(candidate.range) && (Is.string(candidate.expression) || candidate.expression === undefined);
      }
      InlineValueEvaluatableExpression2.is = is;
    })(InlineValueEvaluatableExpression || (exports2.InlineValueEvaluatableExpression = InlineValueEvaluatableExpression = {}));
    var InlineValueContext;
    (function(InlineValueContext2) {
      function create(frameId, stoppedLocation) {
        return { frameId, stoppedLocation };
      }
      InlineValueContext2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(value.stoppedLocation);
      }
      InlineValueContext2.is = is;
    })(InlineValueContext || (exports2.InlineValueContext = InlineValueContext = {}));
    var InlayHintKind;
    (function(InlayHintKind2) {
      InlayHintKind2.Type = 1;
      InlayHintKind2.Parameter = 2;
      function is(value) {
        return value === 1 || value === 2;
      }
      InlayHintKind2.is = is;
    })(InlayHintKind || (exports2.InlayHintKind = InlayHintKind = {}));
    var InlayHintLabelPart;
    (function(InlayHintLabelPart2) {
      function create(value) {
        return { value };
      }
      InlayHintLabelPart2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && (candidate.tooltip === undefined || Is.string(candidate.tooltip) || MarkupContent.is(candidate.tooltip)) && (candidate.location === undefined || Location.is(candidate.location)) && (candidate.command === undefined || Command.is(candidate.command));
      }
      InlayHintLabelPart2.is = is;
    })(InlayHintLabelPart || (exports2.InlayHintLabelPart = InlayHintLabelPart = {}));
    var InlayHint;
    (function(InlayHint2) {
      function create(position, label, kind) {
        var result = { position, label };
        if (kind !== undefined) {
          result.kind = kind;
        }
        return result;
      }
      InlayHint2.create = create;
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Position.is(candidate.position) && (Is.string(candidate.label) || Is.typedArray(candidate.label, InlayHintLabelPart.is)) && (candidate.kind === undefined || InlayHintKind.is(candidate.kind)) && candidate.textEdits === undefined || Is.typedArray(candidate.textEdits, TextEdit.is) && (candidate.tooltip === undefined || Is.string(candidate.tooltip) || MarkupContent.is(candidate.tooltip)) && (candidate.paddingLeft === undefined || Is.boolean(candidate.paddingLeft)) && (candidate.paddingRight === undefined || Is.boolean(candidate.paddingRight));
      }
      InlayHint2.is = is;
    })(InlayHint || (exports2.InlayHint = InlayHint = {}));
    var StringValue;
    (function(StringValue2) {
      function createSnippet(value) {
        return { kind: "snippet", value };
      }
      StringValue2.createSnippet = createSnippet;
    })(StringValue || (exports2.StringValue = StringValue = {}));
    var InlineCompletionItem;
    (function(InlineCompletionItem2) {
      function create(insertText, filterText, range, command) {
        return { insertText, filterText, range, command };
      }
      InlineCompletionItem2.create = create;
    })(InlineCompletionItem || (exports2.InlineCompletionItem = InlineCompletionItem = {}));
    var InlineCompletionList;
    (function(InlineCompletionList2) {
      function create(items) {
        return { items };
      }
      InlineCompletionList2.create = create;
    })(InlineCompletionList || (exports2.InlineCompletionList = InlineCompletionList = {}));
    var InlineCompletionTriggerKind;
    (function(InlineCompletionTriggerKind2) {
      InlineCompletionTriggerKind2.Invoked = 0;
      InlineCompletionTriggerKind2.Automatic = 1;
    })(InlineCompletionTriggerKind || (exports2.InlineCompletionTriggerKind = InlineCompletionTriggerKind = {}));
    var SelectedCompletionInfo;
    (function(SelectedCompletionInfo2) {
      function create(range, text) {
        return { range, text };
      }
      SelectedCompletionInfo2.create = create;
    })(SelectedCompletionInfo || (exports2.SelectedCompletionInfo = SelectedCompletionInfo = {}));
    var InlineCompletionContext;
    (function(InlineCompletionContext2) {
      function create(triggerKind, selectedCompletionInfo) {
        return { triggerKind, selectedCompletionInfo };
      }
      InlineCompletionContext2.create = create;
    })(InlineCompletionContext || (exports2.InlineCompletionContext = InlineCompletionContext = {}));
    var WorkspaceFolder;
    (function(WorkspaceFolder2) {
      function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && URI.is(candidate.uri) && Is.string(candidate.name);
      }
      WorkspaceFolder2.is = is;
    })(WorkspaceFolder || (exports2.WorkspaceFolder = WorkspaceFolder = {}));
    exports2.EOL = [`
`, `\r
`, "\r"];
    var TextDocument;
    (function(TextDocument2) {
      function create(uri, languageId, version, content) {
        return new FullTextDocument(uri, languageId, version, content);
      }
      TextDocument2.create = create;
      function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && (Is.undefined(candidate.languageId) || Is.string(candidate.languageId)) && Is.uinteger(candidate.lineCount) && Is.func(candidate.getText) && Is.func(candidate.positionAt) && Is.func(candidate.offsetAt) ? true : false;
      }
      TextDocument2.is = is;
      function applyEdits(document, edits) {
        var text = document.getText();
        var sortedEdits = mergeSort(edits, function(a, b) {
          var diff = a.range.start.line - b.range.start.line;
          if (diff === 0) {
            return a.range.start.character - b.range.start.character;
          }
          return diff;
        });
        var lastModifiedOffset = text.length;
        for (var i = sortedEdits.length - 1;i >= 0; i--) {
          var e = sortedEdits[i];
          var startOffset = document.offsetAt(e.range.start);
          var endOffset = document.offsetAt(e.range.end);
          if (endOffset <= lastModifiedOffset) {
            text = text.substring(0, startOffset) + e.newText + text.substring(endOffset, text.length);
          } else {
            throw new Error("Overlapping edit");
          }
          lastModifiedOffset = startOffset;
        }
        return text;
      }
      TextDocument2.applyEdits = applyEdits;
      function mergeSort(data, compare) {
        if (data.length <= 1) {
          return data;
        }
        var p = data.length / 2 | 0;
        var left = data.slice(0, p);
        var right = data.slice(p);
        mergeSort(left, compare);
        mergeSort(right, compare);
        var leftIdx = 0;
        var rightIdx = 0;
        var i = 0;
        while (leftIdx < left.length && rightIdx < right.length) {
          var ret = compare(left[leftIdx], right[rightIdx]);
          if (ret <= 0) {
            data[i++] = left[leftIdx++];
          } else {
            data[i++] = right[rightIdx++];
          }
        }
        while (leftIdx < left.length) {
          data[i++] = left[leftIdx++];
        }
        while (rightIdx < right.length) {
          data[i++] = right[rightIdx++];
        }
        return data;
      }
    })(TextDocument || (exports2.TextDocument = TextDocument = {}));
    var FullTextDocument = function() {
      function FullTextDocument2(uri, languageId, version, content) {
        this._uri = uri;
        this._languageId = languageId;
        this._version = version;
        this._content = content;
        this._lineOffsets = undefined;
      }
      Object.defineProperty(FullTextDocument2.prototype, "uri", {
        get: function() {
          return this._uri;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(FullTextDocument2.prototype, "languageId", {
        get: function() {
          return this._languageId;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(FullTextDocument2.prototype, "version", {
        get: function() {
          return this._version;
        },
        enumerable: false,
        configurable: true
      });
      FullTextDocument2.prototype.getText = function(range) {
        if (range) {
          var start = this.offsetAt(range.start);
          var end = this.offsetAt(range.end);
          return this._content.substring(start, end);
        }
        return this._content;
      };
      FullTextDocument2.prototype.update = function(event, version) {
        this._content = event.text;
        this._version = version;
        this._lineOffsets = undefined;
      };
      FullTextDocument2.prototype.getLineOffsets = function() {
        if (this._lineOffsets === undefined) {
          var lineOffsets = [];
          var text = this._content;
          var isLineStart = true;
          for (var i = 0;i < text.length; i++) {
            if (isLineStart) {
              lineOffsets.push(i);
              isLineStart = false;
            }
            var ch = text.charAt(i);
            isLineStart = ch === "\r" || ch === `
`;
            if (ch === "\r" && i + 1 < text.length && text.charAt(i + 1) === `
`) {
              i++;
            }
          }
          if (isLineStart && text.length > 0) {
            lineOffsets.push(text.length);
          }
          this._lineOffsets = lineOffsets;
        }
        return this._lineOffsets;
      };
      FullTextDocument2.prototype.positionAt = function(offset) {
        offset = Math.max(Math.min(offset, this._content.length), 0);
        var lineOffsets = this.getLineOffsets();
        var low = 0, high = lineOffsets.length;
        if (high === 0) {
          return Position.create(0, offset);
        }
        while (low < high) {
          var mid = Math.floor((low + high) / 2);
          if (lineOffsets[mid] > offset) {
            high = mid;
          } else {
            low = mid + 1;
          }
        }
        var line = low - 1;
        return Position.create(line, offset - lineOffsets[line]);
      };
      FullTextDocument2.prototype.offsetAt = function(position) {
        var lineOffsets = this.getLineOffsets();
        if (position.line >= lineOffsets.length) {
          return this._content.length;
        } else if (position.line < 0) {
          return 0;
        }
        var lineOffset = lineOffsets[position.line];
        var nextLineOffset = position.line + 1 < lineOffsets.length ? lineOffsets[position.line + 1] : this._content.length;
        return Math.max(Math.min(lineOffset + position.character, nextLineOffset), lineOffset);
      };
      Object.defineProperty(FullTextDocument2.prototype, "lineCount", {
        get: function() {
          return this.getLineOffsets().length;
        },
        enumerable: false,
        configurable: true
      });
      return FullTextDocument2;
    }();
    var Is;
    (function(Is2) {
      var toString = Object.prototype.toString;
      function defined(value) {
        return typeof value !== "undefined";
      }
      Is2.defined = defined;
      function undefined2(value) {
        return typeof value === "undefined";
      }
      Is2.undefined = undefined2;
      function boolean(value) {
        return value === true || value === false;
      }
      Is2.boolean = boolean;
      function string(value) {
        return toString.call(value) === "[object String]";
      }
      Is2.string = string;
      function number(value) {
        return toString.call(value) === "[object Number]";
      }
      Is2.number = number;
      function numberRange(value, min, max) {
        return toString.call(value) === "[object Number]" && min <= value && value <= max;
      }
      Is2.numberRange = numberRange;
      function integer2(value) {
        return toString.call(value) === "[object Number]" && -2147483648 <= value && value <= 2147483647;
      }
      Is2.integer = integer2;
      function uinteger2(value) {
        return toString.call(value) === "[object Number]" && 0 <= value && value <= 2147483647;
      }
      Is2.uinteger = uinteger2;
      function func(value) {
        return toString.call(value) === "[object Function]";
      }
      Is2.func = func;
      function objectLiteral(value) {
        return value !== null && typeof value === "object";
      }
      Is2.objectLiteral = objectLiteral;
      function typedArray(value, check) {
        return Array.isArray(value) && value.every(check);
      }
      Is2.typedArray = typedArray;
    })(Is || (Is = {}));
  });
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/messages.js
var require_messages2 = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ProtocolNotificationType = exports.ProtocolNotificationType0 = exports.ProtocolRequestType = exports.ProtocolRequestType0 = exports.RegistrationType = exports.MessageDirection = undefined;
  var vscode_jsonrpc_1 = require_main();
  var MessageDirection;
  (function(MessageDirection2) {
    MessageDirection2["clientToServer"] = "clientToServer";
    MessageDirection2["serverToClient"] = "serverToClient";
    MessageDirection2["both"] = "both";
  })(MessageDirection || (exports.MessageDirection = MessageDirection = {}));

  class RegistrationType {
    constructor(method) {
      this.method = method;
    }
  }
  exports.RegistrationType = RegistrationType;

  class ProtocolRequestType0 extends vscode_jsonrpc_1.RequestType0 {
    constructor(method) {
      super(method);
    }
  }
  exports.ProtocolRequestType0 = ProtocolRequestType0;

  class ProtocolRequestType extends vscode_jsonrpc_1.RequestType {
    constructor(method) {
      super(method, vscode_jsonrpc_1.ParameterStructures.byName);
    }
  }
  exports.ProtocolRequestType = ProtocolRequestType;

  class ProtocolNotificationType0 extends vscode_jsonrpc_1.NotificationType0 {
    constructor(method) {
      super(method);
    }
  }
  exports.ProtocolNotificationType0 = ProtocolNotificationType0;

  class ProtocolNotificationType extends vscode_jsonrpc_1.NotificationType {
    constructor(method) {
      super(method, vscode_jsonrpc_1.ParameterStructures.byName);
    }
  }
  exports.ProtocolNotificationType = ProtocolNotificationType;
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/utils/is.js
var require_is3 = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.objectLiteral = exports.typedArray = exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = undefined;
  function boolean(value) {
    return value === true || value === false;
  }
  exports.boolean = boolean;
  function string(value) {
    return typeof value === "string" || value instanceof String;
  }
  exports.string = string;
  function number(value) {
    return typeof value === "number" || value instanceof Number;
  }
  exports.number = number;
  function error(value) {
    return value instanceof Error;
  }
  exports.error = error;
  function func(value) {
    return typeof value === "function";
  }
  exports.func = func;
  function array(value) {
    return Array.isArray(value);
  }
  exports.array = array;
  function stringArray(value) {
    return array(value) && value.every((elem) => string(elem));
  }
  exports.stringArray = stringArray;
  function typedArray(value, check) {
    return Array.isArray(value) && value.every(check);
  }
  exports.typedArray = typedArray;
  function objectLiteral(value) {
    return value !== null && typeof value === "object";
  }
  exports.objectLiteral = objectLiteral;
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.implementation.js
var require_protocol_implementation = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ImplementationRequest = undefined;
  var messages_1 = require_messages2();
  var ImplementationRequest;
  (function(ImplementationRequest2) {
    ImplementationRequest2.method = "textDocument/implementation";
    ImplementationRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    ImplementationRequest2.type = new messages_1.ProtocolRequestType(ImplementationRequest2.method);
  })(ImplementationRequest || (exports.ImplementationRequest = ImplementationRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.typeDefinition.js
var require_protocol_typeDefinition = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.TypeDefinitionRequest = undefined;
  var messages_1 = require_messages2();
  var TypeDefinitionRequest;
  (function(TypeDefinitionRequest2) {
    TypeDefinitionRequest2.method = "textDocument/typeDefinition";
    TypeDefinitionRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    TypeDefinitionRequest2.type = new messages_1.ProtocolRequestType(TypeDefinitionRequest2.method);
  })(TypeDefinitionRequest || (exports.TypeDefinitionRequest = TypeDefinitionRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.workspaceFolder.js
var require_protocol_workspaceFolder = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.DidChangeWorkspaceFoldersNotification = exports.WorkspaceFoldersRequest = undefined;
  var messages_1 = require_messages2();
  var WorkspaceFoldersRequest;
  (function(WorkspaceFoldersRequest2) {
    WorkspaceFoldersRequest2.method = "workspace/workspaceFolders";
    WorkspaceFoldersRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    WorkspaceFoldersRequest2.type = new messages_1.ProtocolRequestType0(WorkspaceFoldersRequest2.method);
  })(WorkspaceFoldersRequest || (exports.WorkspaceFoldersRequest = WorkspaceFoldersRequest = {}));
  var DidChangeWorkspaceFoldersNotification;
  (function(DidChangeWorkspaceFoldersNotification2) {
    DidChangeWorkspaceFoldersNotification2.method = "workspace/didChangeWorkspaceFolders";
    DidChangeWorkspaceFoldersNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidChangeWorkspaceFoldersNotification2.type = new messages_1.ProtocolNotificationType(DidChangeWorkspaceFoldersNotification2.method);
  })(DidChangeWorkspaceFoldersNotification || (exports.DidChangeWorkspaceFoldersNotification = DidChangeWorkspaceFoldersNotification = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.configuration.js
var require_protocol_configuration = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ConfigurationRequest = undefined;
  var messages_1 = require_messages2();
  var ConfigurationRequest;
  (function(ConfigurationRequest2) {
    ConfigurationRequest2.method = "workspace/configuration";
    ConfigurationRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    ConfigurationRequest2.type = new messages_1.ProtocolRequestType(ConfigurationRequest2.method);
  })(ConfigurationRequest || (exports.ConfigurationRequest = ConfigurationRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.colorProvider.js
var require_protocol_colorProvider = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ColorPresentationRequest = exports.DocumentColorRequest = undefined;
  var messages_1 = require_messages2();
  var DocumentColorRequest;
  (function(DocumentColorRequest2) {
    DocumentColorRequest2.method = "textDocument/documentColor";
    DocumentColorRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentColorRequest2.type = new messages_1.ProtocolRequestType(DocumentColorRequest2.method);
  })(DocumentColorRequest || (exports.DocumentColorRequest = DocumentColorRequest = {}));
  var ColorPresentationRequest;
  (function(ColorPresentationRequest2) {
    ColorPresentationRequest2.method = "textDocument/colorPresentation";
    ColorPresentationRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    ColorPresentationRequest2.type = new messages_1.ProtocolRequestType(ColorPresentationRequest2.method);
  })(ColorPresentationRequest || (exports.ColorPresentationRequest = ColorPresentationRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.foldingRange.js
var require_protocol_foldingRange = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.FoldingRangeRefreshRequest = exports.FoldingRangeRequest = undefined;
  var messages_1 = require_messages2();
  var FoldingRangeRequest;
  (function(FoldingRangeRequest2) {
    FoldingRangeRequest2.method = "textDocument/foldingRange";
    FoldingRangeRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    FoldingRangeRequest2.type = new messages_1.ProtocolRequestType(FoldingRangeRequest2.method);
  })(FoldingRangeRequest || (exports.FoldingRangeRequest = FoldingRangeRequest = {}));
  var FoldingRangeRefreshRequest;
  (function(FoldingRangeRefreshRequest2) {
    FoldingRangeRefreshRequest2.method = `workspace/foldingRange/refresh`;
    FoldingRangeRefreshRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    FoldingRangeRefreshRequest2.type = new messages_1.ProtocolRequestType0(FoldingRangeRefreshRequest2.method);
  })(FoldingRangeRefreshRequest || (exports.FoldingRangeRefreshRequest = FoldingRangeRefreshRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.declaration.js
var require_protocol_declaration = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.DeclarationRequest = undefined;
  var messages_1 = require_messages2();
  var DeclarationRequest;
  (function(DeclarationRequest2) {
    DeclarationRequest2.method = "textDocument/declaration";
    DeclarationRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DeclarationRequest2.type = new messages_1.ProtocolRequestType(DeclarationRequest2.method);
  })(DeclarationRequest || (exports.DeclarationRequest = DeclarationRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.selectionRange.js
var require_protocol_selectionRange = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.SelectionRangeRequest = undefined;
  var messages_1 = require_messages2();
  var SelectionRangeRequest;
  (function(SelectionRangeRequest2) {
    SelectionRangeRequest2.method = "textDocument/selectionRange";
    SelectionRangeRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    SelectionRangeRequest2.type = new messages_1.ProtocolRequestType(SelectionRangeRequest2.method);
  })(SelectionRangeRequest || (exports.SelectionRangeRequest = SelectionRangeRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.progress.js
var require_protocol_progress = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.WorkDoneProgressCancelNotification = exports.WorkDoneProgressCreateRequest = exports.WorkDoneProgress = undefined;
  var vscode_jsonrpc_1 = require_main();
  var messages_1 = require_messages2();
  var WorkDoneProgress;
  (function(WorkDoneProgress2) {
    WorkDoneProgress2.type = new vscode_jsonrpc_1.ProgressType;
    function is(value) {
      return value === WorkDoneProgress2.type;
    }
    WorkDoneProgress2.is = is;
  })(WorkDoneProgress || (exports.WorkDoneProgress = WorkDoneProgress = {}));
  var WorkDoneProgressCreateRequest;
  (function(WorkDoneProgressCreateRequest2) {
    WorkDoneProgressCreateRequest2.method = "window/workDoneProgress/create";
    WorkDoneProgressCreateRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    WorkDoneProgressCreateRequest2.type = new messages_1.ProtocolRequestType(WorkDoneProgressCreateRequest2.method);
  })(WorkDoneProgressCreateRequest || (exports.WorkDoneProgressCreateRequest = WorkDoneProgressCreateRequest = {}));
  var WorkDoneProgressCancelNotification;
  (function(WorkDoneProgressCancelNotification2) {
    WorkDoneProgressCancelNotification2.method = "window/workDoneProgress/cancel";
    WorkDoneProgressCancelNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    WorkDoneProgressCancelNotification2.type = new messages_1.ProtocolNotificationType(WorkDoneProgressCancelNotification2.method);
  })(WorkDoneProgressCancelNotification || (exports.WorkDoneProgressCancelNotification = WorkDoneProgressCancelNotification = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.callHierarchy.js
var require_protocol_callHierarchy = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.CallHierarchyOutgoingCallsRequest = exports.CallHierarchyIncomingCallsRequest = exports.CallHierarchyPrepareRequest = undefined;
  var messages_1 = require_messages2();
  var CallHierarchyPrepareRequest;
  (function(CallHierarchyPrepareRequest2) {
    CallHierarchyPrepareRequest2.method = "textDocument/prepareCallHierarchy";
    CallHierarchyPrepareRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CallHierarchyPrepareRequest2.type = new messages_1.ProtocolRequestType(CallHierarchyPrepareRequest2.method);
  })(CallHierarchyPrepareRequest || (exports.CallHierarchyPrepareRequest = CallHierarchyPrepareRequest = {}));
  var CallHierarchyIncomingCallsRequest;
  (function(CallHierarchyIncomingCallsRequest2) {
    CallHierarchyIncomingCallsRequest2.method = "callHierarchy/incomingCalls";
    CallHierarchyIncomingCallsRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CallHierarchyIncomingCallsRequest2.type = new messages_1.ProtocolRequestType(CallHierarchyIncomingCallsRequest2.method);
  })(CallHierarchyIncomingCallsRequest || (exports.CallHierarchyIncomingCallsRequest = CallHierarchyIncomingCallsRequest = {}));
  var CallHierarchyOutgoingCallsRequest;
  (function(CallHierarchyOutgoingCallsRequest2) {
    CallHierarchyOutgoingCallsRequest2.method = "callHierarchy/outgoingCalls";
    CallHierarchyOutgoingCallsRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CallHierarchyOutgoingCallsRequest2.type = new messages_1.ProtocolRequestType(CallHierarchyOutgoingCallsRequest2.method);
  })(CallHierarchyOutgoingCallsRequest || (exports.CallHierarchyOutgoingCallsRequest = CallHierarchyOutgoingCallsRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.semanticTokens.js
var require_protocol_semanticTokens = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.SemanticTokensRefreshRequest = exports.SemanticTokensRangeRequest = exports.SemanticTokensDeltaRequest = exports.SemanticTokensRequest = exports.SemanticTokensRegistrationType = exports.TokenFormat = undefined;
  var messages_1 = require_messages2();
  var TokenFormat;
  (function(TokenFormat2) {
    TokenFormat2.Relative = "relative";
  })(TokenFormat || (exports.TokenFormat = TokenFormat = {}));
  var SemanticTokensRegistrationType;
  (function(SemanticTokensRegistrationType2) {
    SemanticTokensRegistrationType2.method = "textDocument/semanticTokens";
    SemanticTokensRegistrationType2.type = new messages_1.RegistrationType(SemanticTokensRegistrationType2.method);
  })(SemanticTokensRegistrationType || (exports.SemanticTokensRegistrationType = SemanticTokensRegistrationType = {}));
  var SemanticTokensRequest;
  (function(SemanticTokensRequest2) {
    SemanticTokensRequest2.method = "textDocument/semanticTokens/full";
    SemanticTokensRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    SemanticTokensRequest2.type = new messages_1.ProtocolRequestType(SemanticTokensRequest2.method);
    SemanticTokensRequest2.registrationMethod = SemanticTokensRegistrationType.method;
  })(SemanticTokensRequest || (exports.SemanticTokensRequest = SemanticTokensRequest = {}));
  var SemanticTokensDeltaRequest;
  (function(SemanticTokensDeltaRequest2) {
    SemanticTokensDeltaRequest2.method = "textDocument/semanticTokens/full/delta";
    SemanticTokensDeltaRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    SemanticTokensDeltaRequest2.type = new messages_1.ProtocolRequestType(SemanticTokensDeltaRequest2.method);
    SemanticTokensDeltaRequest2.registrationMethod = SemanticTokensRegistrationType.method;
  })(SemanticTokensDeltaRequest || (exports.SemanticTokensDeltaRequest = SemanticTokensDeltaRequest = {}));
  var SemanticTokensRangeRequest;
  (function(SemanticTokensRangeRequest2) {
    SemanticTokensRangeRequest2.method = "textDocument/semanticTokens/range";
    SemanticTokensRangeRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    SemanticTokensRangeRequest2.type = new messages_1.ProtocolRequestType(SemanticTokensRangeRequest2.method);
    SemanticTokensRangeRequest2.registrationMethod = SemanticTokensRegistrationType.method;
  })(SemanticTokensRangeRequest || (exports.SemanticTokensRangeRequest = SemanticTokensRangeRequest = {}));
  var SemanticTokensRefreshRequest;
  (function(SemanticTokensRefreshRequest2) {
    SemanticTokensRefreshRequest2.method = `workspace/semanticTokens/refresh`;
    SemanticTokensRefreshRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    SemanticTokensRefreshRequest2.type = new messages_1.ProtocolRequestType0(SemanticTokensRefreshRequest2.method);
  })(SemanticTokensRefreshRequest || (exports.SemanticTokensRefreshRequest = SemanticTokensRefreshRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.showDocument.js
var require_protocol_showDocument = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ShowDocumentRequest = undefined;
  var messages_1 = require_messages2();
  var ShowDocumentRequest;
  (function(ShowDocumentRequest2) {
    ShowDocumentRequest2.method = "window/showDocument";
    ShowDocumentRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    ShowDocumentRequest2.type = new messages_1.ProtocolRequestType(ShowDocumentRequest2.method);
  })(ShowDocumentRequest || (exports.ShowDocumentRequest = ShowDocumentRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.linkedEditingRange.js
var require_protocol_linkedEditingRange = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.LinkedEditingRangeRequest = undefined;
  var messages_1 = require_messages2();
  var LinkedEditingRangeRequest;
  (function(LinkedEditingRangeRequest2) {
    LinkedEditingRangeRequest2.method = "textDocument/linkedEditingRange";
    LinkedEditingRangeRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    LinkedEditingRangeRequest2.type = new messages_1.ProtocolRequestType(LinkedEditingRangeRequest2.method);
  })(LinkedEditingRangeRequest || (exports.LinkedEditingRangeRequest = LinkedEditingRangeRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.fileOperations.js
var require_protocol_fileOperations = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.WillDeleteFilesRequest = exports.DidDeleteFilesNotification = exports.DidRenameFilesNotification = exports.WillRenameFilesRequest = exports.DidCreateFilesNotification = exports.WillCreateFilesRequest = exports.FileOperationPatternKind = undefined;
  var messages_1 = require_messages2();
  var FileOperationPatternKind;
  (function(FileOperationPatternKind2) {
    FileOperationPatternKind2.file = "file";
    FileOperationPatternKind2.folder = "folder";
  })(FileOperationPatternKind || (exports.FileOperationPatternKind = FileOperationPatternKind = {}));
  var WillCreateFilesRequest;
  (function(WillCreateFilesRequest2) {
    WillCreateFilesRequest2.method = "workspace/willCreateFiles";
    WillCreateFilesRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    WillCreateFilesRequest2.type = new messages_1.ProtocolRequestType(WillCreateFilesRequest2.method);
  })(WillCreateFilesRequest || (exports.WillCreateFilesRequest = WillCreateFilesRequest = {}));
  var DidCreateFilesNotification;
  (function(DidCreateFilesNotification2) {
    DidCreateFilesNotification2.method = "workspace/didCreateFiles";
    DidCreateFilesNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidCreateFilesNotification2.type = new messages_1.ProtocolNotificationType(DidCreateFilesNotification2.method);
  })(DidCreateFilesNotification || (exports.DidCreateFilesNotification = DidCreateFilesNotification = {}));
  var WillRenameFilesRequest;
  (function(WillRenameFilesRequest2) {
    WillRenameFilesRequest2.method = "workspace/willRenameFiles";
    WillRenameFilesRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    WillRenameFilesRequest2.type = new messages_1.ProtocolRequestType(WillRenameFilesRequest2.method);
  })(WillRenameFilesRequest || (exports.WillRenameFilesRequest = WillRenameFilesRequest = {}));
  var DidRenameFilesNotification;
  (function(DidRenameFilesNotification2) {
    DidRenameFilesNotification2.method = "workspace/didRenameFiles";
    DidRenameFilesNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidRenameFilesNotification2.type = new messages_1.ProtocolNotificationType(DidRenameFilesNotification2.method);
  })(DidRenameFilesNotification || (exports.DidRenameFilesNotification = DidRenameFilesNotification = {}));
  var DidDeleteFilesNotification;
  (function(DidDeleteFilesNotification2) {
    DidDeleteFilesNotification2.method = "workspace/didDeleteFiles";
    DidDeleteFilesNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidDeleteFilesNotification2.type = new messages_1.ProtocolNotificationType(DidDeleteFilesNotification2.method);
  })(DidDeleteFilesNotification || (exports.DidDeleteFilesNotification = DidDeleteFilesNotification = {}));
  var WillDeleteFilesRequest;
  (function(WillDeleteFilesRequest2) {
    WillDeleteFilesRequest2.method = "workspace/willDeleteFiles";
    WillDeleteFilesRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    WillDeleteFilesRequest2.type = new messages_1.ProtocolRequestType(WillDeleteFilesRequest2.method);
  })(WillDeleteFilesRequest || (exports.WillDeleteFilesRequest = WillDeleteFilesRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.moniker.js
var require_protocol_moniker = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.MonikerRequest = exports.MonikerKind = exports.UniquenessLevel = undefined;
  var messages_1 = require_messages2();
  var UniquenessLevel;
  (function(UniquenessLevel2) {
    UniquenessLevel2.document = "document";
    UniquenessLevel2.project = "project";
    UniquenessLevel2.group = "group";
    UniquenessLevel2.scheme = "scheme";
    UniquenessLevel2.global = "global";
  })(UniquenessLevel || (exports.UniquenessLevel = UniquenessLevel = {}));
  var MonikerKind;
  (function(MonikerKind2) {
    MonikerKind2.$import = "import";
    MonikerKind2.$export = "export";
    MonikerKind2.local = "local";
  })(MonikerKind || (exports.MonikerKind = MonikerKind = {}));
  var MonikerRequest;
  (function(MonikerRequest2) {
    MonikerRequest2.method = "textDocument/moniker";
    MonikerRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    MonikerRequest2.type = new messages_1.ProtocolRequestType(MonikerRequest2.method);
  })(MonikerRequest || (exports.MonikerRequest = MonikerRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.typeHierarchy.js
var require_protocol_typeHierarchy = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.TypeHierarchySubtypesRequest = exports.TypeHierarchySupertypesRequest = exports.TypeHierarchyPrepareRequest = undefined;
  var messages_1 = require_messages2();
  var TypeHierarchyPrepareRequest;
  (function(TypeHierarchyPrepareRequest2) {
    TypeHierarchyPrepareRequest2.method = "textDocument/prepareTypeHierarchy";
    TypeHierarchyPrepareRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    TypeHierarchyPrepareRequest2.type = new messages_1.ProtocolRequestType(TypeHierarchyPrepareRequest2.method);
  })(TypeHierarchyPrepareRequest || (exports.TypeHierarchyPrepareRequest = TypeHierarchyPrepareRequest = {}));
  var TypeHierarchySupertypesRequest;
  (function(TypeHierarchySupertypesRequest2) {
    TypeHierarchySupertypesRequest2.method = "typeHierarchy/supertypes";
    TypeHierarchySupertypesRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    TypeHierarchySupertypesRequest2.type = new messages_1.ProtocolRequestType(TypeHierarchySupertypesRequest2.method);
  })(TypeHierarchySupertypesRequest || (exports.TypeHierarchySupertypesRequest = TypeHierarchySupertypesRequest = {}));
  var TypeHierarchySubtypesRequest;
  (function(TypeHierarchySubtypesRequest2) {
    TypeHierarchySubtypesRequest2.method = "typeHierarchy/subtypes";
    TypeHierarchySubtypesRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    TypeHierarchySubtypesRequest2.type = new messages_1.ProtocolRequestType(TypeHierarchySubtypesRequest2.method);
  })(TypeHierarchySubtypesRequest || (exports.TypeHierarchySubtypesRequest = TypeHierarchySubtypesRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.inlineValue.js
var require_protocol_inlineValue = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.InlineValueRefreshRequest = exports.InlineValueRequest = undefined;
  var messages_1 = require_messages2();
  var InlineValueRequest;
  (function(InlineValueRequest2) {
    InlineValueRequest2.method = "textDocument/inlineValue";
    InlineValueRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    InlineValueRequest2.type = new messages_1.ProtocolRequestType(InlineValueRequest2.method);
  })(InlineValueRequest || (exports.InlineValueRequest = InlineValueRequest = {}));
  var InlineValueRefreshRequest;
  (function(InlineValueRefreshRequest2) {
    InlineValueRefreshRequest2.method = `workspace/inlineValue/refresh`;
    InlineValueRefreshRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    InlineValueRefreshRequest2.type = new messages_1.ProtocolRequestType0(InlineValueRefreshRequest2.method);
  })(InlineValueRefreshRequest || (exports.InlineValueRefreshRequest = InlineValueRefreshRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.inlayHint.js
var require_protocol_inlayHint = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.InlayHintRefreshRequest = exports.InlayHintResolveRequest = exports.InlayHintRequest = undefined;
  var messages_1 = require_messages2();
  var InlayHintRequest;
  (function(InlayHintRequest2) {
    InlayHintRequest2.method = "textDocument/inlayHint";
    InlayHintRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    InlayHintRequest2.type = new messages_1.ProtocolRequestType(InlayHintRequest2.method);
  })(InlayHintRequest || (exports.InlayHintRequest = InlayHintRequest = {}));
  var InlayHintResolveRequest;
  (function(InlayHintResolveRequest2) {
    InlayHintResolveRequest2.method = "inlayHint/resolve";
    InlayHintResolveRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    InlayHintResolveRequest2.type = new messages_1.ProtocolRequestType(InlayHintResolveRequest2.method);
  })(InlayHintResolveRequest || (exports.InlayHintResolveRequest = InlayHintResolveRequest = {}));
  var InlayHintRefreshRequest;
  (function(InlayHintRefreshRequest2) {
    InlayHintRefreshRequest2.method = `workspace/inlayHint/refresh`;
    InlayHintRefreshRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    InlayHintRefreshRequest2.type = new messages_1.ProtocolRequestType0(InlayHintRefreshRequest2.method);
  })(InlayHintRefreshRequest || (exports.InlayHintRefreshRequest = InlayHintRefreshRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.diagnostic.js
var require_protocol_diagnostic = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.DiagnosticRefreshRequest = exports.WorkspaceDiagnosticRequest = exports.DocumentDiagnosticRequest = exports.DocumentDiagnosticReportKind = exports.DiagnosticServerCancellationData = undefined;
  var vscode_jsonrpc_1 = require_main();
  var Is = require_is3();
  var messages_1 = require_messages2();
  var DiagnosticServerCancellationData;
  (function(DiagnosticServerCancellationData2) {
    function is(value) {
      const candidate = value;
      return candidate && Is.boolean(candidate.retriggerRequest);
    }
    DiagnosticServerCancellationData2.is = is;
  })(DiagnosticServerCancellationData || (exports.DiagnosticServerCancellationData = DiagnosticServerCancellationData = {}));
  var DocumentDiagnosticReportKind;
  (function(DocumentDiagnosticReportKind2) {
    DocumentDiagnosticReportKind2.Full = "full";
    DocumentDiagnosticReportKind2.Unchanged = "unchanged";
  })(DocumentDiagnosticReportKind || (exports.DocumentDiagnosticReportKind = DocumentDiagnosticReportKind = {}));
  var DocumentDiagnosticRequest;
  (function(DocumentDiagnosticRequest2) {
    DocumentDiagnosticRequest2.method = "textDocument/diagnostic";
    DocumentDiagnosticRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentDiagnosticRequest2.type = new messages_1.ProtocolRequestType(DocumentDiagnosticRequest2.method);
    DocumentDiagnosticRequest2.partialResult = new vscode_jsonrpc_1.ProgressType;
  })(DocumentDiagnosticRequest || (exports.DocumentDiagnosticRequest = DocumentDiagnosticRequest = {}));
  var WorkspaceDiagnosticRequest;
  (function(WorkspaceDiagnosticRequest2) {
    WorkspaceDiagnosticRequest2.method = "workspace/diagnostic";
    WorkspaceDiagnosticRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    WorkspaceDiagnosticRequest2.type = new messages_1.ProtocolRequestType(WorkspaceDiagnosticRequest2.method);
    WorkspaceDiagnosticRequest2.partialResult = new vscode_jsonrpc_1.ProgressType;
  })(WorkspaceDiagnosticRequest || (exports.WorkspaceDiagnosticRequest = WorkspaceDiagnosticRequest = {}));
  var DiagnosticRefreshRequest;
  (function(DiagnosticRefreshRequest2) {
    DiagnosticRefreshRequest2.method = `workspace/diagnostic/refresh`;
    DiagnosticRefreshRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    DiagnosticRefreshRequest2.type = new messages_1.ProtocolRequestType0(DiagnosticRefreshRequest2.method);
  })(DiagnosticRefreshRequest || (exports.DiagnosticRefreshRequest = DiagnosticRefreshRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.notebook.js
var require_protocol_notebook = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.DidCloseNotebookDocumentNotification = exports.DidSaveNotebookDocumentNotification = exports.DidChangeNotebookDocumentNotification = exports.NotebookCellArrayChange = exports.DidOpenNotebookDocumentNotification = exports.NotebookDocumentSyncRegistrationType = exports.NotebookDocument = exports.NotebookCell = exports.ExecutionSummary = exports.NotebookCellKind = undefined;
  var vscode_languageserver_types_1 = require_main2();
  var Is = require_is3();
  var messages_1 = require_messages2();
  var NotebookCellKind;
  (function(NotebookCellKind2) {
    NotebookCellKind2.Markup = 1;
    NotebookCellKind2.Code = 2;
    function is(value) {
      return value === 1 || value === 2;
    }
    NotebookCellKind2.is = is;
  })(NotebookCellKind || (exports.NotebookCellKind = NotebookCellKind = {}));
  var ExecutionSummary;
  (function(ExecutionSummary2) {
    function create(executionOrder, success) {
      const result = { executionOrder };
      if (success === true || success === false) {
        result.success = success;
      }
      return result;
    }
    ExecutionSummary2.create = create;
    function is(value) {
      const candidate = value;
      return Is.objectLiteral(candidate) && vscode_languageserver_types_1.uinteger.is(candidate.executionOrder) && (candidate.success === undefined || Is.boolean(candidate.success));
    }
    ExecutionSummary2.is = is;
    function equals(one, other) {
      if (one === other) {
        return true;
      }
      if (one === null || one === undefined || other === null || other === undefined) {
        return false;
      }
      return one.executionOrder === other.executionOrder && one.success === other.success;
    }
    ExecutionSummary2.equals = equals;
  })(ExecutionSummary || (exports.ExecutionSummary = ExecutionSummary = {}));
  var NotebookCell;
  (function(NotebookCell2) {
    function create(kind, document) {
      return { kind, document };
    }
    NotebookCell2.create = create;
    function is(value) {
      const candidate = value;
      return Is.objectLiteral(candidate) && NotebookCellKind.is(candidate.kind) && vscode_languageserver_types_1.DocumentUri.is(candidate.document) && (candidate.metadata === undefined || Is.objectLiteral(candidate.metadata));
    }
    NotebookCell2.is = is;
    function diff(one, two) {
      const result = new Set;
      if (one.document !== two.document) {
        result.add("document");
      }
      if (one.kind !== two.kind) {
        result.add("kind");
      }
      if (one.executionSummary !== two.executionSummary) {
        result.add("executionSummary");
      }
      if ((one.metadata !== undefined || two.metadata !== undefined) && !equalsMetadata(one.metadata, two.metadata)) {
        result.add("metadata");
      }
      if ((one.executionSummary !== undefined || two.executionSummary !== undefined) && !ExecutionSummary.equals(one.executionSummary, two.executionSummary)) {
        result.add("executionSummary");
      }
      return result;
    }
    NotebookCell2.diff = diff;
    function equalsMetadata(one, other) {
      if (one === other) {
        return true;
      }
      if (one === null || one === undefined || other === null || other === undefined) {
        return false;
      }
      if (typeof one !== typeof other) {
        return false;
      }
      if (typeof one !== "object") {
        return false;
      }
      const oneArray = Array.isArray(one);
      const otherArray = Array.isArray(other);
      if (oneArray !== otherArray) {
        return false;
      }
      if (oneArray && otherArray) {
        if (one.length !== other.length) {
          return false;
        }
        for (let i = 0;i < one.length; i++) {
          if (!equalsMetadata(one[i], other[i])) {
            return false;
          }
        }
      }
      if (Is.objectLiteral(one) && Is.objectLiteral(other)) {
        const oneKeys = Object.keys(one);
        const otherKeys = Object.keys(other);
        if (oneKeys.length !== otherKeys.length) {
          return false;
        }
        oneKeys.sort();
        otherKeys.sort();
        if (!equalsMetadata(oneKeys, otherKeys)) {
          return false;
        }
        for (let i = 0;i < oneKeys.length; i++) {
          const prop = oneKeys[i];
          if (!equalsMetadata(one[prop], other[prop])) {
            return false;
          }
        }
      }
      return true;
    }
  })(NotebookCell || (exports.NotebookCell = NotebookCell = {}));
  var NotebookDocument;
  (function(NotebookDocument2) {
    function create(uri, notebookType, version, cells) {
      return { uri, notebookType, version, cells };
    }
    NotebookDocument2.create = create;
    function is(value) {
      const candidate = value;
      return Is.objectLiteral(candidate) && Is.string(candidate.uri) && vscode_languageserver_types_1.integer.is(candidate.version) && Is.typedArray(candidate.cells, NotebookCell.is);
    }
    NotebookDocument2.is = is;
  })(NotebookDocument || (exports.NotebookDocument = NotebookDocument = {}));
  var NotebookDocumentSyncRegistrationType;
  (function(NotebookDocumentSyncRegistrationType2) {
    NotebookDocumentSyncRegistrationType2.method = "notebookDocument/sync";
    NotebookDocumentSyncRegistrationType2.messageDirection = messages_1.MessageDirection.clientToServer;
    NotebookDocumentSyncRegistrationType2.type = new messages_1.RegistrationType(NotebookDocumentSyncRegistrationType2.method);
  })(NotebookDocumentSyncRegistrationType || (exports.NotebookDocumentSyncRegistrationType = NotebookDocumentSyncRegistrationType = {}));
  var DidOpenNotebookDocumentNotification;
  (function(DidOpenNotebookDocumentNotification2) {
    DidOpenNotebookDocumentNotification2.method = "notebookDocument/didOpen";
    DidOpenNotebookDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidOpenNotebookDocumentNotification2.type = new messages_1.ProtocolNotificationType(DidOpenNotebookDocumentNotification2.method);
    DidOpenNotebookDocumentNotification2.registrationMethod = NotebookDocumentSyncRegistrationType.method;
  })(DidOpenNotebookDocumentNotification || (exports.DidOpenNotebookDocumentNotification = DidOpenNotebookDocumentNotification = {}));
  var NotebookCellArrayChange;
  (function(NotebookCellArrayChange2) {
    function is(value) {
      const candidate = value;
      return Is.objectLiteral(candidate) && vscode_languageserver_types_1.uinteger.is(candidate.start) && vscode_languageserver_types_1.uinteger.is(candidate.deleteCount) && (candidate.cells === undefined || Is.typedArray(candidate.cells, NotebookCell.is));
    }
    NotebookCellArrayChange2.is = is;
    function create(start, deleteCount, cells) {
      const result = { start, deleteCount };
      if (cells !== undefined) {
        result.cells = cells;
      }
      return result;
    }
    NotebookCellArrayChange2.create = create;
  })(NotebookCellArrayChange || (exports.NotebookCellArrayChange = NotebookCellArrayChange = {}));
  var DidChangeNotebookDocumentNotification;
  (function(DidChangeNotebookDocumentNotification2) {
    DidChangeNotebookDocumentNotification2.method = "notebookDocument/didChange";
    DidChangeNotebookDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidChangeNotebookDocumentNotification2.type = new messages_1.ProtocolNotificationType(DidChangeNotebookDocumentNotification2.method);
    DidChangeNotebookDocumentNotification2.registrationMethod = NotebookDocumentSyncRegistrationType.method;
  })(DidChangeNotebookDocumentNotification || (exports.DidChangeNotebookDocumentNotification = DidChangeNotebookDocumentNotification = {}));
  var DidSaveNotebookDocumentNotification;
  (function(DidSaveNotebookDocumentNotification2) {
    DidSaveNotebookDocumentNotification2.method = "notebookDocument/didSave";
    DidSaveNotebookDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidSaveNotebookDocumentNotification2.type = new messages_1.ProtocolNotificationType(DidSaveNotebookDocumentNotification2.method);
    DidSaveNotebookDocumentNotification2.registrationMethod = NotebookDocumentSyncRegistrationType.method;
  })(DidSaveNotebookDocumentNotification || (exports.DidSaveNotebookDocumentNotification = DidSaveNotebookDocumentNotification = {}));
  var DidCloseNotebookDocumentNotification;
  (function(DidCloseNotebookDocumentNotification2) {
    DidCloseNotebookDocumentNotification2.method = "notebookDocument/didClose";
    DidCloseNotebookDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidCloseNotebookDocumentNotification2.type = new messages_1.ProtocolNotificationType(DidCloseNotebookDocumentNotification2.method);
    DidCloseNotebookDocumentNotification2.registrationMethod = NotebookDocumentSyncRegistrationType.method;
  })(DidCloseNotebookDocumentNotification || (exports.DidCloseNotebookDocumentNotification = DidCloseNotebookDocumentNotification = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.inlineCompletion.js
var require_protocol_inlineCompletion = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.InlineCompletionRequest = undefined;
  var messages_1 = require_messages2();
  var InlineCompletionRequest;
  (function(InlineCompletionRequest2) {
    InlineCompletionRequest2.method = "textDocument/inlineCompletion";
    InlineCompletionRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    InlineCompletionRequest2.type = new messages_1.ProtocolRequestType(InlineCompletionRequest2.method);
  })(InlineCompletionRequest || (exports.InlineCompletionRequest = InlineCompletionRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/protocol.js
var require_protocol = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.WorkspaceSymbolRequest = exports.CodeActionResolveRequest = exports.CodeActionRequest = exports.DocumentSymbolRequest = exports.DocumentHighlightRequest = exports.ReferencesRequest = exports.DefinitionRequest = exports.SignatureHelpRequest = exports.SignatureHelpTriggerKind = exports.HoverRequest = exports.CompletionResolveRequest = exports.CompletionRequest = exports.CompletionTriggerKind = exports.PublishDiagnosticsNotification = exports.WatchKind = exports.RelativePattern = exports.FileChangeType = exports.DidChangeWatchedFilesNotification = exports.WillSaveTextDocumentWaitUntilRequest = exports.WillSaveTextDocumentNotification = exports.TextDocumentSaveReason = exports.DidSaveTextDocumentNotification = exports.DidCloseTextDocumentNotification = exports.DidChangeTextDocumentNotification = exports.TextDocumentContentChangeEvent = exports.DidOpenTextDocumentNotification = exports.TextDocumentSyncKind = exports.TelemetryEventNotification = exports.LogMessageNotification = exports.ShowMessageRequest = exports.ShowMessageNotification = exports.MessageType = exports.DidChangeConfigurationNotification = exports.ExitNotification = exports.ShutdownRequest = exports.InitializedNotification = exports.InitializeErrorCodes = exports.InitializeRequest = exports.WorkDoneProgressOptions = exports.TextDocumentRegistrationOptions = exports.StaticRegistrationOptions = exports.PositionEncodingKind = exports.FailureHandlingKind = exports.ResourceOperationKind = exports.UnregistrationRequest = exports.RegistrationRequest = exports.DocumentSelector = exports.NotebookCellTextDocumentFilter = exports.NotebookDocumentFilter = exports.TextDocumentFilter = undefined;
  exports.MonikerRequest = exports.MonikerKind = exports.UniquenessLevel = exports.WillDeleteFilesRequest = exports.DidDeleteFilesNotification = exports.WillRenameFilesRequest = exports.DidRenameFilesNotification = exports.WillCreateFilesRequest = exports.DidCreateFilesNotification = exports.FileOperationPatternKind = exports.LinkedEditingRangeRequest = exports.ShowDocumentRequest = exports.SemanticTokensRegistrationType = exports.SemanticTokensRefreshRequest = exports.SemanticTokensRangeRequest = exports.SemanticTokensDeltaRequest = exports.SemanticTokensRequest = exports.TokenFormat = exports.CallHierarchyPrepareRequest = exports.CallHierarchyOutgoingCallsRequest = exports.CallHierarchyIncomingCallsRequest = exports.WorkDoneProgressCancelNotification = exports.WorkDoneProgressCreateRequest = exports.WorkDoneProgress = exports.SelectionRangeRequest = exports.DeclarationRequest = exports.FoldingRangeRefreshRequest = exports.FoldingRangeRequest = exports.ColorPresentationRequest = exports.DocumentColorRequest = exports.ConfigurationRequest = exports.DidChangeWorkspaceFoldersNotification = exports.WorkspaceFoldersRequest = exports.TypeDefinitionRequest = exports.ImplementationRequest = exports.ApplyWorkspaceEditRequest = exports.ExecuteCommandRequest = exports.PrepareRenameRequest = exports.RenameRequest = exports.PrepareSupportDefaultBehavior = exports.DocumentOnTypeFormattingRequest = exports.DocumentRangesFormattingRequest = exports.DocumentRangeFormattingRequest = exports.DocumentFormattingRequest = exports.DocumentLinkResolveRequest = exports.DocumentLinkRequest = exports.CodeLensRefreshRequest = exports.CodeLensResolveRequest = exports.CodeLensRequest = exports.WorkspaceSymbolResolveRequest = undefined;
  exports.InlineCompletionRequest = exports.DidCloseNotebookDocumentNotification = exports.DidSaveNotebookDocumentNotification = exports.DidChangeNotebookDocumentNotification = exports.NotebookCellArrayChange = exports.DidOpenNotebookDocumentNotification = exports.NotebookDocumentSyncRegistrationType = exports.NotebookDocument = exports.NotebookCell = exports.ExecutionSummary = exports.NotebookCellKind = exports.DiagnosticRefreshRequest = exports.WorkspaceDiagnosticRequest = exports.DocumentDiagnosticRequest = exports.DocumentDiagnosticReportKind = exports.DiagnosticServerCancellationData = exports.InlayHintRefreshRequest = exports.InlayHintResolveRequest = exports.InlayHintRequest = exports.InlineValueRefreshRequest = exports.InlineValueRequest = exports.TypeHierarchySupertypesRequest = exports.TypeHierarchySubtypesRequest = exports.TypeHierarchyPrepareRequest = undefined;
  var messages_1 = require_messages2();
  var vscode_languageserver_types_1 = require_main2();
  var Is = require_is3();
  var protocol_implementation_1 = require_protocol_implementation();
  Object.defineProperty(exports, "ImplementationRequest", { enumerable: true, get: function() {
    return protocol_implementation_1.ImplementationRequest;
  } });
  var protocol_typeDefinition_1 = require_protocol_typeDefinition();
  Object.defineProperty(exports, "TypeDefinitionRequest", { enumerable: true, get: function() {
    return protocol_typeDefinition_1.TypeDefinitionRequest;
  } });
  var protocol_workspaceFolder_1 = require_protocol_workspaceFolder();
  Object.defineProperty(exports, "WorkspaceFoldersRequest", { enumerable: true, get: function() {
    return protocol_workspaceFolder_1.WorkspaceFoldersRequest;
  } });
  Object.defineProperty(exports, "DidChangeWorkspaceFoldersNotification", { enumerable: true, get: function() {
    return protocol_workspaceFolder_1.DidChangeWorkspaceFoldersNotification;
  } });
  var protocol_configuration_1 = require_protocol_configuration();
  Object.defineProperty(exports, "ConfigurationRequest", { enumerable: true, get: function() {
    return protocol_configuration_1.ConfigurationRequest;
  } });
  var protocol_colorProvider_1 = require_protocol_colorProvider();
  Object.defineProperty(exports, "DocumentColorRequest", { enumerable: true, get: function() {
    return protocol_colorProvider_1.DocumentColorRequest;
  } });
  Object.defineProperty(exports, "ColorPresentationRequest", { enumerable: true, get: function() {
    return protocol_colorProvider_1.ColorPresentationRequest;
  } });
  var protocol_foldingRange_1 = require_protocol_foldingRange();
  Object.defineProperty(exports, "FoldingRangeRequest", { enumerable: true, get: function() {
    return protocol_foldingRange_1.FoldingRangeRequest;
  } });
  Object.defineProperty(exports, "FoldingRangeRefreshRequest", { enumerable: true, get: function() {
    return protocol_foldingRange_1.FoldingRangeRefreshRequest;
  } });
  var protocol_declaration_1 = require_protocol_declaration();
  Object.defineProperty(exports, "DeclarationRequest", { enumerable: true, get: function() {
    return protocol_declaration_1.DeclarationRequest;
  } });
  var protocol_selectionRange_1 = require_protocol_selectionRange();
  Object.defineProperty(exports, "SelectionRangeRequest", { enumerable: true, get: function() {
    return protocol_selectionRange_1.SelectionRangeRequest;
  } });
  var protocol_progress_1 = require_protocol_progress();
  Object.defineProperty(exports, "WorkDoneProgress", { enumerable: true, get: function() {
    return protocol_progress_1.WorkDoneProgress;
  } });
  Object.defineProperty(exports, "WorkDoneProgressCreateRequest", { enumerable: true, get: function() {
    return protocol_progress_1.WorkDoneProgressCreateRequest;
  } });
  Object.defineProperty(exports, "WorkDoneProgressCancelNotification", { enumerable: true, get: function() {
    return protocol_progress_1.WorkDoneProgressCancelNotification;
  } });
  var protocol_callHierarchy_1 = require_protocol_callHierarchy();
  Object.defineProperty(exports, "CallHierarchyIncomingCallsRequest", { enumerable: true, get: function() {
    return protocol_callHierarchy_1.CallHierarchyIncomingCallsRequest;
  } });
  Object.defineProperty(exports, "CallHierarchyOutgoingCallsRequest", { enumerable: true, get: function() {
    return protocol_callHierarchy_1.CallHierarchyOutgoingCallsRequest;
  } });
  Object.defineProperty(exports, "CallHierarchyPrepareRequest", { enumerable: true, get: function() {
    return protocol_callHierarchy_1.CallHierarchyPrepareRequest;
  } });
  var protocol_semanticTokens_1 = require_protocol_semanticTokens();
  Object.defineProperty(exports, "TokenFormat", { enumerable: true, get: function() {
    return protocol_semanticTokens_1.TokenFormat;
  } });
  Object.defineProperty(exports, "SemanticTokensRequest", { enumerable: true, get: function() {
    return protocol_semanticTokens_1.SemanticTokensRequest;
  } });
  Object.defineProperty(exports, "SemanticTokensDeltaRequest", { enumerable: true, get: function() {
    return protocol_semanticTokens_1.SemanticTokensDeltaRequest;
  } });
  Object.defineProperty(exports, "SemanticTokensRangeRequest", { enumerable: true, get: function() {
    return protocol_semanticTokens_1.SemanticTokensRangeRequest;
  } });
  Object.defineProperty(exports, "SemanticTokensRefreshRequest", { enumerable: true, get: function() {
    return protocol_semanticTokens_1.SemanticTokensRefreshRequest;
  } });
  Object.defineProperty(exports, "SemanticTokensRegistrationType", { enumerable: true, get: function() {
    return protocol_semanticTokens_1.SemanticTokensRegistrationType;
  } });
  var protocol_showDocument_1 = require_protocol_showDocument();
  Object.defineProperty(exports, "ShowDocumentRequest", { enumerable: true, get: function() {
    return protocol_showDocument_1.ShowDocumentRequest;
  } });
  var protocol_linkedEditingRange_1 = require_protocol_linkedEditingRange();
  Object.defineProperty(exports, "LinkedEditingRangeRequest", { enumerable: true, get: function() {
    return protocol_linkedEditingRange_1.LinkedEditingRangeRequest;
  } });
  var protocol_fileOperations_1 = require_protocol_fileOperations();
  Object.defineProperty(exports, "FileOperationPatternKind", { enumerable: true, get: function() {
    return protocol_fileOperations_1.FileOperationPatternKind;
  } });
  Object.defineProperty(exports, "DidCreateFilesNotification", { enumerable: true, get: function() {
    return protocol_fileOperations_1.DidCreateFilesNotification;
  } });
  Object.defineProperty(exports, "WillCreateFilesRequest", { enumerable: true, get: function() {
    return protocol_fileOperations_1.WillCreateFilesRequest;
  } });
  Object.defineProperty(exports, "DidRenameFilesNotification", { enumerable: true, get: function() {
    return protocol_fileOperations_1.DidRenameFilesNotification;
  } });
  Object.defineProperty(exports, "WillRenameFilesRequest", { enumerable: true, get: function() {
    return protocol_fileOperations_1.WillRenameFilesRequest;
  } });
  Object.defineProperty(exports, "DidDeleteFilesNotification", { enumerable: true, get: function() {
    return protocol_fileOperations_1.DidDeleteFilesNotification;
  } });
  Object.defineProperty(exports, "WillDeleteFilesRequest", { enumerable: true, get: function() {
    return protocol_fileOperations_1.WillDeleteFilesRequest;
  } });
  var protocol_moniker_1 = require_protocol_moniker();
  Object.defineProperty(exports, "UniquenessLevel", { enumerable: true, get: function() {
    return protocol_moniker_1.UniquenessLevel;
  } });
  Object.defineProperty(exports, "MonikerKind", { enumerable: true, get: function() {
    return protocol_moniker_1.MonikerKind;
  } });
  Object.defineProperty(exports, "MonikerRequest", { enumerable: true, get: function() {
    return protocol_moniker_1.MonikerRequest;
  } });
  var protocol_typeHierarchy_1 = require_protocol_typeHierarchy();
  Object.defineProperty(exports, "TypeHierarchyPrepareRequest", { enumerable: true, get: function() {
    return protocol_typeHierarchy_1.TypeHierarchyPrepareRequest;
  } });
  Object.defineProperty(exports, "TypeHierarchySubtypesRequest", { enumerable: true, get: function() {
    return protocol_typeHierarchy_1.TypeHierarchySubtypesRequest;
  } });
  Object.defineProperty(exports, "TypeHierarchySupertypesRequest", { enumerable: true, get: function() {
    return protocol_typeHierarchy_1.TypeHierarchySupertypesRequest;
  } });
  var protocol_inlineValue_1 = require_protocol_inlineValue();
  Object.defineProperty(exports, "InlineValueRequest", { enumerable: true, get: function() {
    return protocol_inlineValue_1.InlineValueRequest;
  } });
  Object.defineProperty(exports, "InlineValueRefreshRequest", { enumerable: true, get: function() {
    return protocol_inlineValue_1.InlineValueRefreshRequest;
  } });
  var protocol_inlayHint_1 = require_protocol_inlayHint();
  Object.defineProperty(exports, "InlayHintRequest", { enumerable: true, get: function() {
    return protocol_inlayHint_1.InlayHintRequest;
  } });
  Object.defineProperty(exports, "InlayHintResolveRequest", { enumerable: true, get: function() {
    return protocol_inlayHint_1.InlayHintResolveRequest;
  } });
  Object.defineProperty(exports, "InlayHintRefreshRequest", { enumerable: true, get: function() {
    return protocol_inlayHint_1.InlayHintRefreshRequest;
  } });
  var protocol_diagnostic_1 = require_protocol_diagnostic();
  Object.defineProperty(exports, "DiagnosticServerCancellationData", { enumerable: true, get: function() {
    return protocol_diagnostic_1.DiagnosticServerCancellationData;
  } });
  Object.defineProperty(exports, "DocumentDiagnosticReportKind", { enumerable: true, get: function() {
    return protocol_diagnostic_1.DocumentDiagnosticReportKind;
  } });
  Object.defineProperty(exports, "DocumentDiagnosticRequest", { enumerable: true, get: function() {
    return protocol_diagnostic_1.DocumentDiagnosticRequest;
  } });
  Object.defineProperty(exports, "WorkspaceDiagnosticRequest", { enumerable: true, get: function() {
    return protocol_diagnostic_1.WorkspaceDiagnosticRequest;
  } });
  Object.defineProperty(exports, "DiagnosticRefreshRequest", { enumerable: true, get: function() {
    return protocol_diagnostic_1.DiagnosticRefreshRequest;
  } });
  var protocol_notebook_1 = require_protocol_notebook();
  Object.defineProperty(exports, "NotebookCellKind", { enumerable: true, get: function() {
    return protocol_notebook_1.NotebookCellKind;
  } });
  Object.defineProperty(exports, "ExecutionSummary", { enumerable: true, get: function() {
    return protocol_notebook_1.ExecutionSummary;
  } });
  Object.defineProperty(exports, "NotebookCell", { enumerable: true, get: function() {
    return protocol_notebook_1.NotebookCell;
  } });
  Object.defineProperty(exports, "NotebookDocument", { enumerable: true, get: function() {
    return protocol_notebook_1.NotebookDocument;
  } });
  Object.defineProperty(exports, "NotebookDocumentSyncRegistrationType", { enumerable: true, get: function() {
    return protocol_notebook_1.NotebookDocumentSyncRegistrationType;
  } });
  Object.defineProperty(exports, "DidOpenNotebookDocumentNotification", { enumerable: true, get: function() {
    return protocol_notebook_1.DidOpenNotebookDocumentNotification;
  } });
  Object.defineProperty(exports, "NotebookCellArrayChange", { enumerable: true, get: function() {
    return protocol_notebook_1.NotebookCellArrayChange;
  } });
  Object.defineProperty(exports, "DidChangeNotebookDocumentNotification", { enumerable: true, get: function() {
    return protocol_notebook_1.DidChangeNotebookDocumentNotification;
  } });
  Object.defineProperty(exports, "DidSaveNotebookDocumentNotification", { enumerable: true, get: function() {
    return protocol_notebook_1.DidSaveNotebookDocumentNotification;
  } });
  Object.defineProperty(exports, "DidCloseNotebookDocumentNotification", { enumerable: true, get: function() {
    return protocol_notebook_1.DidCloseNotebookDocumentNotification;
  } });
  var protocol_inlineCompletion_1 = require_protocol_inlineCompletion();
  Object.defineProperty(exports, "InlineCompletionRequest", { enumerable: true, get: function() {
    return protocol_inlineCompletion_1.InlineCompletionRequest;
  } });
  var TextDocumentFilter;
  (function(TextDocumentFilter2) {
    function is(value) {
      const candidate = value;
      return Is.string(candidate) || (Is.string(candidate.language) || Is.string(candidate.scheme) || Is.string(candidate.pattern));
    }
    TextDocumentFilter2.is = is;
  })(TextDocumentFilter || (exports.TextDocumentFilter = TextDocumentFilter = {}));
  var NotebookDocumentFilter;
  (function(NotebookDocumentFilter2) {
    function is(value) {
      const candidate = value;
      return Is.objectLiteral(candidate) && (Is.string(candidate.notebookType) || Is.string(candidate.scheme) || Is.string(candidate.pattern));
    }
    NotebookDocumentFilter2.is = is;
  })(NotebookDocumentFilter || (exports.NotebookDocumentFilter = NotebookDocumentFilter = {}));
  var NotebookCellTextDocumentFilter;
  (function(NotebookCellTextDocumentFilter2) {
    function is(value) {
      const candidate = value;
      return Is.objectLiteral(candidate) && (Is.string(candidate.notebook) || NotebookDocumentFilter.is(candidate.notebook)) && (candidate.language === undefined || Is.string(candidate.language));
    }
    NotebookCellTextDocumentFilter2.is = is;
  })(NotebookCellTextDocumentFilter || (exports.NotebookCellTextDocumentFilter = NotebookCellTextDocumentFilter = {}));
  var DocumentSelector;
  (function(DocumentSelector2) {
    function is(value) {
      if (!Array.isArray(value)) {
        return false;
      }
      for (let elem of value) {
        if (!Is.string(elem) && !TextDocumentFilter.is(elem) && !NotebookCellTextDocumentFilter.is(elem)) {
          return false;
        }
      }
      return true;
    }
    DocumentSelector2.is = is;
  })(DocumentSelector || (exports.DocumentSelector = DocumentSelector = {}));
  var RegistrationRequest;
  (function(RegistrationRequest2) {
    RegistrationRequest2.method = "client/registerCapability";
    RegistrationRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    RegistrationRequest2.type = new messages_1.ProtocolRequestType(RegistrationRequest2.method);
  })(RegistrationRequest || (exports.RegistrationRequest = RegistrationRequest = {}));
  var UnregistrationRequest;
  (function(UnregistrationRequest2) {
    UnregistrationRequest2.method = "client/unregisterCapability";
    UnregistrationRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    UnregistrationRequest2.type = new messages_1.ProtocolRequestType(UnregistrationRequest2.method);
  })(UnregistrationRequest || (exports.UnregistrationRequest = UnregistrationRequest = {}));
  var ResourceOperationKind;
  (function(ResourceOperationKind2) {
    ResourceOperationKind2.Create = "create";
    ResourceOperationKind2.Rename = "rename";
    ResourceOperationKind2.Delete = "delete";
  })(ResourceOperationKind || (exports.ResourceOperationKind = ResourceOperationKind = {}));
  var FailureHandlingKind;
  (function(FailureHandlingKind2) {
    FailureHandlingKind2.Abort = "abort";
    FailureHandlingKind2.Transactional = "transactional";
    FailureHandlingKind2.TextOnlyTransactional = "textOnlyTransactional";
    FailureHandlingKind2.Undo = "undo";
  })(FailureHandlingKind || (exports.FailureHandlingKind = FailureHandlingKind = {}));
  var PositionEncodingKind;
  (function(PositionEncodingKind2) {
    PositionEncodingKind2.UTF8 = "utf-8";
    PositionEncodingKind2.UTF16 = "utf-16";
    PositionEncodingKind2.UTF32 = "utf-32";
  })(PositionEncodingKind || (exports.PositionEncodingKind = PositionEncodingKind = {}));
  var StaticRegistrationOptions;
  (function(StaticRegistrationOptions2) {
    function hasId(value) {
      const candidate = value;
      return candidate && Is.string(candidate.id) && candidate.id.length > 0;
    }
    StaticRegistrationOptions2.hasId = hasId;
  })(StaticRegistrationOptions || (exports.StaticRegistrationOptions = StaticRegistrationOptions = {}));
  var TextDocumentRegistrationOptions;
  (function(TextDocumentRegistrationOptions2) {
    function is(value) {
      const candidate = value;
      return candidate && (candidate.documentSelector === null || DocumentSelector.is(candidate.documentSelector));
    }
    TextDocumentRegistrationOptions2.is = is;
  })(TextDocumentRegistrationOptions || (exports.TextDocumentRegistrationOptions = TextDocumentRegistrationOptions = {}));
  var WorkDoneProgressOptions;
  (function(WorkDoneProgressOptions2) {
    function is(value) {
      const candidate = value;
      return Is.objectLiteral(candidate) && (candidate.workDoneProgress === undefined || Is.boolean(candidate.workDoneProgress));
    }
    WorkDoneProgressOptions2.is = is;
    function hasWorkDoneProgress(value) {
      const candidate = value;
      return candidate && Is.boolean(candidate.workDoneProgress);
    }
    WorkDoneProgressOptions2.hasWorkDoneProgress = hasWorkDoneProgress;
  })(WorkDoneProgressOptions || (exports.WorkDoneProgressOptions = WorkDoneProgressOptions = {}));
  var InitializeRequest;
  (function(InitializeRequest2) {
    InitializeRequest2.method = "initialize";
    InitializeRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    InitializeRequest2.type = new messages_1.ProtocolRequestType(InitializeRequest2.method);
  })(InitializeRequest || (exports.InitializeRequest = InitializeRequest = {}));
  var InitializeErrorCodes;
  (function(InitializeErrorCodes2) {
    InitializeErrorCodes2.unknownProtocolVersion = 1;
  })(InitializeErrorCodes || (exports.InitializeErrorCodes = InitializeErrorCodes = {}));
  var InitializedNotification;
  (function(InitializedNotification2) {
    InitializedNotification2.method = "initialized";
    InitializedNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    InitializedNotification2.type = new messages_1.ProtocolNotificationType(InitializedNotification2.method);
  })(InitializedNotification || (exports.InitializedNotification = InitializedNotification = {}));
  var ShutdownRequest;
  (function(ShutdownRequest2) {
    ShutdownRequest2.method = "shutdown";
    ShutdownRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    ShutdownRequest2.type = new messages_1.ProtocolRequestType0(ShutdownRequest2.method);
  })(ShutdownRequest || (exports.ShutdownRequest = ShutdownRequest = {}));
  var ExitNotification;
  (function(ExitNotification2) {
    ExitNotification2.method = "exit";
    ExitNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    ExitNotification2.type = new messages_1.ProtocolNotificationType0(ExitNotification2.method);
  })(ExitNotification || (exports.ExitNotification = ExitNotification = {}));
  var DidChangeConfigurationNotification;
  (function(DidChangeConfigurationNotification2) {
    DidChangeConfigurationNotification2.method = "workspace/didChangeConfiguration";
    DidChangeConfigurationNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidChangeConfigurationNotification2.type = new messages_1.ProtocolNotificationType(DidChangeConfigurationNotification2.method);
  })(DidChangeConfigurationNotification || (exports.DidChangeConfigurationNotification = DidChangeConfigurationNotification = {}));
  var MessageType;
  (function(MessageType2) {
    MessageType2.Error = 1;
    MessageType2.Warning = 2;
    MessageType2.Info = 3;
    MessageType2.Log = 4;
    MessageType2.Debug = 5;
  })(MessageType || (exports.MessageType = MessageType = {}));
  var ShowMessageNotification;
  (function(ShowMessageNotification2) {
    ShowMessageNotification2.method = "window/showMessage";
    ShowMessageNotification2.messageDirection = messages_1.MessageDirection.serverToClient;
    ShowMessageNotification2.type = new messages_1.ProtocolNotificationType(ShowMessageNotification2.method);
  })(ShowMessageNotification || (exports.ShowMessageNotification = ShowMessageNotification = {}));
  var ShowMessageRequest;
  (function(ShowMessageRequest2) {
    ShowMessageRequest2.method = "window/showMessageRequest";
    ShowMessageRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    ShowMessageRequest2.type = new messages_1.ProtocolRequestType(ShowMessageRequest2.method);
  })(ShowMessageRequest || (exports.ShowMessageRequest = ShowMessageRequest = {}));
  var LogMessageNotification;
  (function(LogMessageNotification2) {
    LogMessageNotification2.method = "window/logMessage";
    LogMessageNotification2.messageDirection = messages_1.MessageDirection.serverToClient;
    LogMessageNotification2.type = new messages_1.ProtocolNotificationType(LogMessageNotification2.method);
  })(LogMessageNotification || (exports.LogMessageNotification = LogMessageNotification = {}));
  var TelemetryEventNotification;
  (function(TelemetryEventNotification2) {
    TelemetryEventNotification2.method = "telemetry/event";
    TelemetryEventNotification2.messageDirection = messages_1.MessageDirection.serverToClient;
    TelemetryEventNotification2.type = new messages_1.ProtocolNotificationType(TelemetryEventNotification2.method);
  })(TelemetryEventNotification || (exports.TelemetryEventNotification = TelemetryEventNotification = {}));
  var TextDocumentSyncKind;
  (function(TextDocumentSyncKind2) {
    TextDocumentSyncKind2.None = 0;
    TextDocumentSyncKind2.Full = 1;
    TextDocumentSyncKind2.Incremental = 2;
  })(TextDocumentSyncKind || (exports.TextDocumentSyncKind = TextDocumentSyncKind = {}));
  var DidOpenTextDocumentNotification;
  (function(DidOpenTextDocumentNotification2) {
    DidOpenTextDocumentNotification2.method = "textDocument/didOpen";
    DidOpenTextDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidOpenTextDocumentNotification2.type = new messages_1.ProtocolNotificationType(DidOpenTextDocumentNotification2.method);
  })(DidOpenTextDocumentNotification || (exports.DidOpenTextDocumentNotification = DidOpenTextDocumentNotification = {}));
  var TextDocumentContentChangeEvent;
  (function(TextDocumentContentChangeEvent2) {
    function isIncremental(event) {
      let candidate = event;
      return candidate !== undefined && candidate !== null && typeof candidate.text === "string" && candidate.range !== undefined && (candidate.rangeLength === undefined || typeof candidate.rangeLength === "number");
    }
    TextDocumentContentChangeEvent2.isIncremental = isIncremental;
    function isFull(event) {
      let candidate = event;
      return candidate !== undefined && candidate !== null && typeof candidate.text === "string" && candidate.range === undefined && candidate.rangeLength === undefined;
    }
    TextDocumentContentChangeEvent2.isFull = isFull;
  })(TextDocumentContentChangeEvent || (exports.TextDocumentContentChangeEvent = TextDocumentContentChangeEvent = {}));
  var DidChangeTextDocumentNotification;
  (function(DidChangeTextDocumentNotification2) {
    DidChangeTextDocumentNotification2.method = "textDocument/didChange";
    DidChangeTextDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidChangeTextDocumentNotification2.type = new messages_1.ProtocolNotificationType(DidChangeTextDocumentNotification2.method);
  })(DidChangeTextDocumentNotification || (exports.DidChangeTextDocumentNotification = DidChangeTextDocumentNotification = {}));
  var DidCloseTextDocumentNotification;
  (function(DidCloseTextDocumentNotification2) {
    DidCloseTextDocumentNotification2.method = "textDocument/didClose";
    DidCloseTextDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidCloseTextDocumentNotification2.type = new messages_1.ProtocolNotificationType(DidCloseTextDocumentNotification2.method);
  })(DidCloseTextDocumentNotification || (exports.DidCloseTextDocumentNotification = DidCloseTextDocumentNotification = {}));
  var DidSaveTextDocumentNotification;
  (function(DidSaveTextDocumentNotification2) {
    DidSaveTextDocumentNotification2.method = "textDocument/didSave";
    DidSaveTextDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidSaveTextDocumentNotification2.type = new messages_1.ProtocolNotificationType(DidSaveTextDocumentNotification2.method);
  })(DidSaveTextDocumentNotification || (exports.DidSaveTextDocumentNotification = DidSaveTextDocumentNotification = {}));
  var TextDocumentSaveReason;
  (function(TextDocumentSaveReason2) {
    TextDocumentSaveReason2.Manual = 1;
    TextDocumentSaveReason2.AfterDelay = 2;
    TextDocumentSaveReason2.FocusOut = 3;
  })(TextDocumentSaveReason || (exports.TextDocumentSaveReason = TextDocumentSaveReason = {}));
  var WillSaveTextDocumentNotification;
  (function(WillSaveTextDocumentNotification2) {
    WillSaveTextDocumentNotification2.method = "textDocument/willSave";
    WillSaveTextDocumentNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    WillSaveTextDocumentNotification2.type = new messages_1.ProtocolNotificationType(WillSaveTextDocumentNotification2.method);
  })(WillSaveTextDocumentNotification || (exports.WillSaveTextDocumentNotification = WillSaveTextDocumentNotification = {}));
  var WillSaveTextDocumentWaitUntilRequest;
  (function(WillSaveTextDocumentWaitUntilRequest2) {
    WillSaveTextDocumentWaitUntilRequest2.method = "textDocument/willSaveWaitUntil";
    WillSaveTextDocumentWaitUntilRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    WillSaveTextDocumentWaitUntilRequest2.type = new messages_1.ProtocolRequestType(WillSaveTextDocumentWaitUntilRequest2.method);
  })(WillSaveTextDocumentWaitUntilRequest || (exports.WillSaveTextDocumentWaitUntilRequest = WillSaveTextDocumentWaitUntilRequest = {}));
  var DidChangeWatchedFilesNotification;
  (function(DidChangeWatchedFilesNotification2) {
    DidChangeWatchedFilesNotification2.method = "workspace/didChangeWatchedFiles";
    DidChangeWatchedFilesNotification2.messageDirection = messages_1.MessageDirection.clientToServer;
    DidChangeWatchedFilesNotification2.type = new messages_1.ProtocolNotificationType(DidChangeWatchedFilesNotification2.method);
  })(DidChangeWatchedFilesNotification || (exports.DidChangeWatchedFilesNotification = DidChangeWatchedFilesNotification = {}));
  var FileChangeType;
  (function(FileChangeType2) {
    FileChangeType2.Created = 1;
    FileChangeType2.Changed = 2;
    FileChangeType2.Deleted = 3;
  })(FileChangeType || (exports.FileChangeType = FileChangeType = {}));
  var RelativePattern;
  (function(RelativePattern2) {
    function is(value) {
      const candidate = value;
      return Is.objectLiteral(candidate) && (vscode_languageserver_types_1.URI.is(candidate.baseUri) || vscode_languageserver_types_1.WorkspaceFolder.is(candidate.baseUri)) && Is.string(candidate.pattern);
    }
    RelativePattern2.is = is;
  })(RelativePattern || (exports.RelativePattern = RelativePattern = {}));
  var WatchKind;
  (function(WatchKind2) {
    WatchKind2.Create = 1;
    WatchKind2.Change = 2;
    WatchKind2.Delete = 4;
  })(WatchKind || (exports.WatchKind = WatchKind = {}));
  var PublishDiagnosticsNotification;
  (function(PublishDiagnosticsNotification2) {
    PublishDiagnosticsNotification2.method = "textDocument/publishDiagnostics";
    PublishDiagnosticsNotification2.messageDirection = messages_1.MessageDirection.serverToClient;
    PublishDiagnosticsNotification2.type = new messages_1.ProtocolNotificationType(PublishDiagnosticsNotification2.method);
  })(PublishDiagnosticsNotification || (exports.PublishDiagnosticsNotification = PublishDiagnosticsNotification = {}));
  var CompletionTriggerKind;
  (function(CompletionTriggerKind2) {
    CompletionTriggerKind2.Invoked = 1;
    CompletionTriggerKind2.TriggerCharacter = 2;
    CompletionTriggerKind2.TriggerForIncompleteCompletions = 3;
  })(CompletionTriggerKind || (exports.CompletionTriggerKind = CompletionTriggerKind = {}));
  var CompletionRequest;
  (function(CompletionRequest2) {
    CompletionRequest2.method = "textDocument/completion";
    CompletionRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CompletionRequest2.type = new messages_1.ProtocolRequestType(CompletionRequest2.method);
  })(CompletionRequest || (exports.CompletionRequest = CompletionRequest = {}));
  var CompletionResolveRequest;
  (function(CompletionResolveRequest2) {
    CompletionResolveRequest2.method = "completionItem/resolve";
    CompletionResolveRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CompletionResolveRequest2.type = new messages_1.ProtocolRequestType(CompletionResolveRequest2.method);
  })(CompletionResolveRequest || (exports.CompletionResolveRequest = CompletionResolveRequest = {}));
  var HoverRequest;
  (function(HoverRequest2) {
    HoverRequest2.method = "textDocument/hover";
    HoverRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    HoverRequest2.type = new messages_1.ProtocolRequestType(HoverRequest2.method);
  })(HoverRequest || (exports.HoverRequest = HoverRequest = {}));
  var SignatureHelpTriggerKind;
  (function(SignatureHelpTriggerKind2) {
    SignatureHelpTriggerKind2.Invoked = 1;
    SignatureHelpTriggerKind2.TriggerCharacter = 2;
    SignatureHelpTriggerKind2.ContentChange = 3;
  })(SignatureHelpTriggerKind || (exports.SignatureHelpTriggerKind = SignatureHelpTriggerKind = {}));
  var SignatureHelpRequest;
  (function(SignatureHelpRequest2) {
    SignatureHelpRequest2.method = "textDocument/signatureHelp";
    SignatureHelpRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    SignatureHelpRequest2.type = new messages_1.ProtocolRequestType(SignatureHelpRequest2.method);
  })(SignatureHelpRequest || (exports.SignatureHelpRequest = SignatureHelpRequest = {}));
  var DefinitionRequest;
  (function(DefinitionRequest2) {
    DefinitionRequest2.method = "textDocument/definition";
    DefinitionRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DefinitionRequest2.type = new messages_1.ProtocolRequestType(DefinitionRequest2.method);
  })(DefinitionRequest || (exports.DefinitionRequest = DefinitionRequest = {}));
  var ReferencesRequest;
  (function(ReferencesRequest2) {
    ReferencesRequest2.method = "textDocument/references";
    ReferencesRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    ReferencesRequest2.type = new messages_1.ProtocolRequestType(ReferencesRequest2.method);
  })(ReferencesRequest || (exports.ReferencesRequest = ReferencesRequest = {}));
  var DocumentHighlightRequest;
  (function(DocumentHighlightRequest2) {
    DocumentHighlightRequest2.method = "textDocument/documentHighlight";
    DocumentHighlightRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentHighlightRequest2.type = new messages_1.ProtocolRequestType(DocumentHighlightRequest2.method);
  })(DocumentHighlightRequest || (exports.DocumentHighlightRequest = DocumentHighlightRequest = {}));
  var DocumentSymbolRequest;
  (function(DocumentSymbolRequest2) {
    DocumentSymbolRequest2.method = "textDocument/documentSymbol";
    DocumentSymbolRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentSymbolRequest2.type = new messages_1.ProtocolRequestType(DocumentSymbolRequest2.method);
  })(DocumentSymbolRequest || (exports.DocumentSymbolRequest = DocumentSymbolRequest = {}));
  var CodeActionRequest;
  (function(CodeActionRequest2) {
    CodeActionRequest2.method = "textDocument/codeAction";
    CodeActionRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CodeActionRequest2.type = new messages_1.ProtocolRequestType(CodeActionRequest2.method);
  })(CodeActionRequest || (exports.CodeActionRequest = CodeActionRequest = {}));
  var CodeActionResolveRequest;
  (function(CodeActionResolveRequest2) {
    CodeActionResolveRequest2.method = "codeAction/resolve";
    CodeActionResolveRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CodeActionResolveRequest2.type = new messages_1.ProtocolRequestType(CodeActionResolveRequest2.method);
  })(CodeActionResolveRequest || (exports.CodeActionResolveRequest = CodeActionResolveRequest = {}));
  var WorkspaceSymbolRequest;
  (function(WorkspaceSymbolRequest2) {
    WorkspaceSymbolRequest2.method = "workspace/symbol";
    WorkspaceSymbolRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    WorkspaceSymbolRequest2.type = new messages_1.ProtocolRequestType(WorkspaceSymbolRequest2.method);
  })(WorkspaceSymbolRequest || (exports.WorkspaceSymbolRequest = WorkspaceSymbolRequest = {}));
  var WorkspaceSymbolResolveRequest;
  (function(WorkspaceSymbolResolveRequest2) {
    WorkspaceSymbolResolveRequest2.method = "workspaceSymbol/resolve";
    WorkspaceSymbolResolveRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    WorkspaceSymbolResolveRequest2.type = new messages_1.ProtocolRequestType(WorkspaceSymbolResolveRequest2.method);
  })(WorkspaceSymbolResolveRequest || (exports.WorkspaceSymbolResolveRequest = WorkspaceSymbolResolveRequest = {}));
  var CodeLensRequest;
  (function(CodeLensRequest2) {
    CodeLensRequest2.method = "textDocument/codeLens";
    CodeLensRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CodeLensRequest2.type = new messages_1.ProtocolRequestType(CodeLensRequest2.method);
  })(CodeLensRequest || (exports.CodeLensRequest = CodeLensRequest = {}));
  var CodeLensResolveRequest;
  (function(CodeLensResolveRequest2) {
    CodeLensResolveRequest2.method = "codeLens/resolve";
    CodeLensResolveRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    CodeLensResolveRequest2.type = new messages_1.ProtocolRequestType(CodeLensResolveRequest2.method);
  })(CodeLensResolveRequest || (exports.CodeLensResolveRequest = CodeLensResolveRequest = {}));
  var CodeLensRefreshRequest;
  (function(CodeLensRefreshRequest2) {
    CodeLensRefreshRequest2.method = `workspace/codeLens/refresh`;
    CodeLensRefreshRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    CodeLensRefreshRequest2.type = new messages_1.ProtocolRequestType0(CodeLensRefreshRequest2.method);
  })(CodeLensRefreshRequest || (exports.CodeLensRefreshRequest = CodeLensRefreshRequest = {}));
  var DocumentLinkRequest;
  (function(DocumentLinkRequest2) {
    DocumentLinkRequest2.method = "textDocument/documentLink";
    DocumentLinkRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentLinkRequest2.type = new messages_1.ProtocolRequestType(DocumentLinkRequest2.method);
  })(DocumentLinkRequest || (exports.DocumentLinkRequest = DocumentLinkRequest = {}));
  var DocumentLinkResolveRequest;
  (function(DocumentLinkResolveRequest2) {
    DocumentLinkResolveRequest2.method = "documentLink/resolve";
    DocumentLinkResolveRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentLinkResolveRequest2.type = new messages_1.ProtocolRequestType(DocumentLinkResolveRequest2.method);
  })(DocumentLinkResolveRequest || (exports.DocumentLinkResolveRequest = DocumentLinkResolveRequest = {}));
  var DocumentFormattingRequest;
  (function(DocumentFormattingRequest2) {
    DocumentFormattingRequest2.method = "textDocument/formatting";
    DocumentFormattingRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentFormattingRequest2.type = new messages_1.ProtocolRequestType(DocumentFormattingRequest2.method);
  })(DocumentFormattingRequest || (exports.DocumentFormattingRequest = DocumentFormattingRequest = {}));
  var DocumentRangeFormattingRequest;
  (function(DocumentRangeFormattingRequest2) {
    DocumentRangeFormattingRequest2.method = "textDocument/rangeFormatting";
    DocumentRangeFormattingRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentRangeFormattingRequest2.type = new messages_1.ProtocolRequestType(DocumentRangeFormattingRequest2.method);
  })(DocumentRangeFormattingRequest || (exports.DocumentRangeFormattingRequest = DocumentRangeFormattingRequest = {}));
  var DocumentRangesFormattingRequest;
  (function(DocumentRangesFormattingRequest2) {
    DocumentRangesFormattingRequest2.method = "textDocument/rangesFormatting";
    DocumentRangesFormattingRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentRangesFormattingRequest2.type = new messages_1.ProtocolRequestType(DocumentRangesFormattingRequest2.method);
  })(DocumentRangesFormattingRequest || (exports.DocumentRangesFormattingRequest = DocumentRangesFormattingRequest = {}));
  var DocumentOnTypeFormattingRequest;
  (function(DocumentOnTypeFormattingRequest2) {
    DocumentOnTypeFormattingRequest2.method = "textDocument/onTypeFormatting";
    DocumentOnTypeFormattingRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    DocumentOnTypeFormattingRequest2.type = new messages_1.ProtocolRequestType(DocumentOnTypeFormattingRequest2.method);
  })(DocumentOnTypeFormattingRequest || (exports.DocumentOnTypeFormattingRequest = DocumentOnTypeFormattingRequest = {}));
  var PrepareSupportDefaultBehavior;
  (function(PrepareSupportDefaultBehavior2) {
    PrepareSupportDefaultBehavior2.Identifier = 1;
  })(PrepareSupportDefaultBehavior || (exports.PrepareSupportDefaultBehavior = PrepareSupportDefaultBehavior = {}));
  var RenameRequest;
  (function(RenameRequest2) {
    RenameRequest2.method = "textDocument/rename";
    RenameRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    RenameRequest2.type = new messages_1.ProtocolRequestType(RenameRequest2.method);
  })(RenameRequest || (exports.RenameRequest = RenameRequest = {}));
  var PrepareRenameRequest;
  (function(PrepareRenameRequest2) {
    PrepareRenameRequest2.method = "textDocument/prepareRename";
    PrepareRenameRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    PrepareRenameRequest2.type = new messages_1.ProtocolRequestType(PrepareRenameRequest2.method);
  })(PrepareRenameRequest || (exports.PrepareRenameRequest = PrepareRenameRequest = {}));
  var ExecuteCommandRequest;
  (function(ExecuteCommandRequest2) {
    ExecuteCommandRequest2.method = "workspace/executeCommand";
    ExecuteCommandRequest2.messageDirection = messages_1.MessageDirection.clientToServer;
    ExecuteCommandRequest2.type = new messages_1.ProtocolRequestType(ExecuteCommandRequest2.method);
  })(ExecuteCommandRequest || (exports.ExecuteCommandRequest = ExecuteCommandRequest = {}));
  var ApplyWorkspaceEditRequest;
  (function(ApplyWorkspaceEditRequest2) {
    ApplyWorkspaceEditRequest2.method = "workspace/applyEdit";
    ApplyWorkspaceEditRequest2.messageDirection = messages_1.MessageDirection.serverToClient;
    ApplyWorkspaceEditRequest2.type = new messages_1.ProtocolRequestType("workspace/applyEdit");
  })(ApplyWorkspaceEditRequest || (exports.ApplyWorkspaceEditRequest = ApplyWorkspaceEditRequest = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/connection.js
var require_connection2 = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.createProtocolConnection = undefined;
  var vscode_jsonrpc_1 = require_main();
  function createProtocolConnection(input, output, logger, options) {
    if (vscode_jsonrpc_1.ConnectionStrategy.is(options)) {
      options = { connectionStrategy: options };
    }
    return (0, vscode_jsonrpc_1.createMessageConnection)(input, output, logger, options);
  }
  exports.createProtocolConnection = createProtocolConnection;
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/common/api.js
var require_api2 = __commonJS((exports) => {
  var __createBinding = exports && exports.__createBinding || (Object.create ? function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() {
        return m[k];
      } };
    }
    Object.defineProperty(o, k2, desc);
  } : function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    o[k2] = m[k];
  });
  var __exportStar = exports && exports.__exportStar || function(m, exports2) {
    for (var p in m)
      if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports2, p))
        __createBinding(exports2, m, p);
  };
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.LSPErrorCodes = exports.createProtocolConnection = undefined;
  __exportStar(require_main(), exports);
  __exportStar(require_main2(), exports);
  __exportStar(require_messages2(), exports);
  __exportStar(require_protocol(), exports);
  var connection_1 = require_connection2();
  Object.defineProperty(exports, "createProtocolConnection", { enumerable: true, get: function() {
    return connection_1.createProtocolConnection;
  } });
  var LSPErrorCodes;
  (function(LSPErrorCodes2) {
    LSPErrorCodes2.lspReservedErrorRangeStart = -32899;
    LSPErrorCodes2.RequestFailed = -32803;
    LSPErrorCodes2.ServerCancelled = -32802;
    LSPErrorCodes2.ContentModified = -32801;
    LSPErrorCodes2.RequestCancelled = -32800;
    LSPErrorCodes2.lspReservedErrorRangeEnd = -32800;
  })(LSPErrorCodes || (exports.LSPErrorCodes = LSPErrorCodes = {}));
});

// ../../node_modules/.bun/vscode-languageserver-protocol@3.17.5/node_modules/vscode-languageserver-protocol/lib/node/main.js
var require_main3 = __commonJS((exports) => {
  var __createBinding = exports && exports.__createBinding || (Object.create ? function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() {
        return m[k];
      } };
    }
    Object.defineProperty(o, k2, desc);
  } : function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    o[k2] = m[k];
  });
  var __exportStar = exports && exports.__exportStar || function(m, exports2) {
    for (var p in m)
      if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports2, p))
        __createBinding(exports2, m, p);
  };
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.createProtocolConnection = undefined;
  var node_1 = require_main();
  __exportStar(require_main(), exports);
  __exportStar(require_api2(), exports);
  function createProtocolConnection(input, output, logger, options) {
    return (0, node_1.createMessageConnection)(input, output, logger, options);
  }
  exports.createProtocolConnection = createProtocolConnection;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/utils/uuid.js
var require_uuid = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.generateUuid = exports.parse = exports.isUUID = exports.v4 = exports.empty = undefined;

  class ValueUUID {
    constructor(_value) {
      this._value = _value;
    }
    asHex() {
      return this._value;
    }
    equals(other) {
      return this.asHex() === other.asHex();
    }
  }

  class V4UUID extends ValueUUID {
    static _oneOf(array) {
      return array[Math.floor(array.length * Math.random())];
    }
    static _randomHex() {
      return V4UUID._oneOf(V4UUID._chars);
    }
    constructor() {
      super([
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        "-",
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        "-",
        "4",
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        "-",
        V4UUID._oneOf(V4UUID._timeHighBits),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        "-",
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex(),
        V4UUID._randomHex()
      ].join(""));
    }
  }
  V4UUID._chars = ["0", "1", "2", "3", "4", "5", "6", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"];
  V4UUID._timeHighBits = ["8", "9", "a", "b"];
  exports.empty = new ValueUUID("00000000-0000-0000-0000-000000000000");
  function v4() {
    return new V4UUID;
  }
  exports.v4 = v4;
  var _UUIDPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
  function isUUID(value) {
    return _UUIDPattern.test(value);
  }
  exports.isUUID = isUUID;
  function parse(value) {
    if (!isUUID(value)) {
      throw new Error("invalid uuid");
    }
    return new ValueUUID(value);
  }
  exports.parse = parse;
  function generateUuid() {
    return v4().asHex();
  }
  exports.generateUuid = generateUuid;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/progress.js
var require_progress = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.attachPartialResult = exports.ProgressFeature = exports.attachWorkDone = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var uuid_1 = require_uuid();

  class WorkDoneProgressReporterImpl {
    constructor(_connection, _token) {
      this._connection = _connection;
      this._token = _token;
      WorkDoneProgressReporterImpl.Instances.set(this._token, this);
    }
    begin(title, percentage, message, cancellable) {
      let param = {
        kind: "begin",
        title,
        percentage,
        message,
        cancellable
      };
      this._connection.sendProgress(vscode_languageserver_protocol_1.WorkDoneProgress.type, this._token, param);
    }
    report(arg0, arg1) {
      let param = {
        kind: "report"
      };
      if (typeof arg0 === "number") {
        param.percentage = arg0;
        if (arg1 !== undefined) {
          param.message = arg1;
        }
      } else {
        param.message = arg0;
      }
      this._connection.sendProgress(vscode_languageserver_protocol_1.WorkDoneProgress.type, this._token, param);
    }
    done() {
      WorkDoneProgressReporterImpl.Instances.delete(this._token);
      this._connection.sendProgress(vscode_languageserver_protocol_1.WorkDoneProgress.type, this._token, { kind: "end" });
    }
  }
  WorkDoneProgressReporterImpl.Instances = new Map;

  class WorkDoneProgressServerReporterImpl extends WorkDoneProgressReporterImpl {
    constructor(connection, token) {
      super(connection, token);
      this._source = new vscode_languageserver_protocol_1.CancellationTokenSource;
    }
    get token() {
      return this._source.token;
    }
    done() {
      this._source.dispose();
      super.done();
    }
    cancel() {
      this._source.cancel();
    }
  }

  class NullProgressReporter {
    constructor() {}
    begin() {}
    report() {}
    done() {}
  }

  class NullProgressServerReporter extends NullProgressReporter {
    constructor() {
      super();
      this._source = new vscode_languageserver_protocol_1.CancellationTokenSource;
    }
    get token() {
      return this._source.token;
    }
    done() {
      this._source.dispose();
    }
    cancel() {
      this._source.cancel();
    }
  }
  function attachWorkDone(connection, params) {
    if (params === undefined || params.workDoneToken === undefined) {
      return new NullProgressReporter;
    }
    const token = params.workDoneToken;
    delete params.workDoneToken;
    return new WorkDoneProgressReporterImpl(connection, token);
  }
  exports.attachWorkDone = attachWorkDone;
  var ProgressFeature = (Base) => {
    return class extends Base {
      constructor() {
        super();
        this._progressSupported = false;
      }
      initialize(capabilities) {
        super.initialize(capabilities);
        if (capabilities?.window?.workDoneProgress === true) {
          this._progressSupported = true;
          this.connection.onNotification(vscode_languageserver_protocol_1.WorkDoneProgressCancelNotification.type, (params) => {
            let progress = WorkDoneProgressReporterImpl.Instances.get(params.token);
            if (progress instanceof WorkDoneProgressServerReporterImpl || progress instanceof NullProgressServerReporter) {
              progress.cancel();
            }
          });
        }
      }
      attachWorkDoneProgress(token) {
        if (token === undefined) {
          return new NullProgressReporter;
        } else {
          return new WorkDoneProgressReporterImpl(this.connection, token);
        }
      }
      createWorkDoneProgress() {
        if (this._progressSupported) {
          const token = (0, uuid_1.generateUuid)();
          return this.connection.sendRequest(vscode_languageserver_protocol_1.WorkDoneProgressCreateRequest.type, { token }).then(() => {
            const result = new WorkDoneProgressServerReporterImpl(this.connection, token);
            return result;
          });
        } else {
          return Promise.resolve(new NullProgressServerReporter);
        }
      }
    };
  };
  exports.ProgressFeature = ProgressFeature;
  var ResultProgress;
  (function(ResultProgress2) {
    ResultProgress2.type = new vscode_languageserver_protocol_1.ProgressType;
  })(ResultProgress || (ResultProgress = {}));

  class ResultProgressReporterImpl {
    constructor(_connection, _token) {
      this._connection = _connection;
      this._token = _token;
    }
    report(data) {
      this._connection.sendProgress(ResultProgress.type, this._token, data);
    }
  }
  function attachPartialResult(connection, params) {
    if (params === undefined || params.partialResultToken === undefined) {
      return;
    }
    const token = params.partialResultToken;
    delete params.partialResultToken;
    return new ResultProgressReporterImpl(connection, token);
  }
  exports.attachPartialResult = attachPartialResult;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/configuration.js
var require_configuration = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ConfigurationFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var Is = require_is();
  var ConfigurationFeature = (Base) => {
    return class extends Base {
      getConfiguration(arg) {
        if (!arg) {
          return this._getConfiguration({});
        } else if (Is.string(arg)) {
          return this._getConfiguration({ section: arg });
        } else {
          return this._getConfiguration(arg);
        }
      }
      _getConfiguration(arg) {
        let params = {
          items: Array.isArray(arg) ? arg : [arg]
        };
        return this.connection.sendRequest(vscode_languageserver_protocol_1.ConfigurationRequest.type, params).then((result) => {
          if (Array.isArray(result)) {
            return Array.isArray(arg) ? result : result[0];
          } else {
            return Array.isArray(arg) ? [] : null;
          }
        });
      }
    };
  };
  exports.ConfigurationFeature = ConfigurationFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/workspaceFolder.js
var require_workspaceFolder = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.WorkspaceFoldersFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var WorkspaceFoldersFeature = (Base) => {
    return class extends Base {
      constructor() {
        super();
        this._notificationIsAutoRegistered = false;
      }
      initialize(capabilities) {
        super.initialize(capabilities);
        let workspaceCapabilities = capabilities.workspace;
        if (workspaceCapabilities && workspaceCapabilities.workspaceFolders) {
          this._onDidChangeWorkspaceFolders = new vscode_languageserver_protocol_1.Emitter;
          this.connection.onNotification(vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type, (params) => {
            this._onDidChangeWorkspaceFolders.fire(params.event);
          });
        }
      }
      fillServerCapabilities(capabilities) {
        super.fillServerCapabilities(capabilities);
        const changeNotifications = capabilities.workspace?.workspaceFolders?.changeNotifications;
        this._notificationIsAutoRegistered = changeNotifications === true || typeof changeNotifications === "string";
      }
      getWorkspaceFolders() {
        return this.connection.sendRequest(vscode_languageserver_protocol_1.WorkspaceFoldersRequest.type);
      }
      get onDidChangeWorkspaceFolders() {
        if (!this._onDidChangeWorkspaceFolders) {
          throw new Error("Client doesn't support sending workspace folder change events.");
        }
        if (!this._notificationIsAutoRegistered && !this._unregistration) {
          this._unregistration = this.connection.client.register(vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type);
        }
        return this._onDidChangeWorkspaceFolders.event;
      }
    };
  };
  exports.WorkspaceFoldersFeature = WorkspaceFoldersFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/callHierarchy.js
var require_callHierarchy = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.CallHierarchyFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var CallHierarchyFeature = (Base) => {
    return class extends Base {
      get callHierarchy() {
        return {
          onPrepare: (handler) => {
            return this.connection.onRequest(vscode_languageserver_protocol_1.CallHierarchyPrepareRequest.type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), undefined);
            });
          },
          onIncomingCalls: (handler) => {
            const type = vscode_languageserver_protocol_1.CallHierarchyIncomingCallsRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          },
          onOutgoingCalls: (handler) => {
            const type = vscode_languageserver_protocol_1.CallHierarchyOutgoingCallsRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          }
        };
      }
    };
  };
  exports.CallHierarchyFeature = CallHierarchyFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/semanticTokens.js
var require_semanticTokens = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.SemanticTokensBuilder = exports.SemanticTokensDiff = exports.SemanticTokensFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var SemanticTokensFeature = (Base) => {
    return class extends Base {
      get semanticTokens() {
        return {
          refresh: () => {
            return this.connection.sendRequest(vscode_languageserver_protocol_1.SemanticTokensRefreshRequest.type);
          },
          on: (handler) => {
            const type = vscode_languageserver_protocol_1.SemanticTokensRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          },
          onDelta: (handler) => {
            const type = vscode_languageserver_protocol_1.SemanticTokensDeltaRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          },
          onRange: (handler) => {
            const type = vscode_languageserver_protocol_1.SemanticTokensRangeRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          }
        };
      }
    };
  };
  exports.SemanticTokensFeature = SemanticTokensFeature;

  class SemanticTokensDiff {
    constructor(originalSequence, modifiedSequence) {
      this.originalSequence = originalSequence;
      this.modifiedSequence = modifiedSequence;
    }
    computeDiff() {
      const originalLength = this.originalSequence.length;
      const modifiedLength = this.modifiedSequence.length;
      let startIndex = 0;
      while (startIndex < modifiedLength && startIndex < originalLength && this.originalSequence[startIndex] === this.modifiedSequence[startIndex]) {
        startIndex++;
      }
      if (startIndex < modifiedLength && startIndex < originalLength) {
        let originalEndIndex = originalLength - 1;
        let modifiedEndIndex = modifiedLength - 1;
        while (originalEndIndex >= startIndex && modifiedEndIndex >= startIndex && this.originalSequence[originalEndIndex] === this.modifiedSequence[modifiedEndIndex]) {
          originalEndIndex--;
          modifiedEndIndex--;
        }
        if (originalEndIndex < startIndex || modifiedEndIndex < startIndex) {
          originalEndIndex++;
          modifiedEndIndex++;
        }
        const deleteCount = originalEndIndex - startIndex + 1;
        const newData = this.modifiedSequence.slice(startIndex, modifiedEndIndex + 1);
        if (newData.length === 1 && newData[0] === this.originalSequence[originalEndIndex]) {
          return [
            { start: startIndex, deleteCount: deleteCount - 1 }
          ];
        } else {
          return [
            { start: startIndex, deleteCount, data: newData }
          ];
        }
      } else if (startIndex < modifiedLength) {
        return [
          { start: startIndex, deleteCount: 0, data: this.modifiedSequence.slice(startIndex) }
        ];
      } else if (startIndex < originalLength) {
        return [
          { start: startIndex, deleteCount: originalLength - startIndex }
        ];
      } else {
        return [];
      }
    }
  }
  exports.SemanticTokensDiff = SemanticTokensDiff;

  class SemanticTokensBuilder {
    constructor() {
      this._prevData = undefined;
      this.initialize();
    }
    initialize() {
      this._id = Date.now();
      this._prevLine = 0;
      this._prevChar = 0;
      this._data = [];
      this._dataLen = 0;
    }
    push(line, char, length, tokenType, tokenModifiers) {
      let pushLine = line;
      let pushChar = char;
      if (this._dataLen > 0) {
        pushLine -= this._prevLine;
        if (pushLine === 0) {
          pushChar -= this._prevChar;
        }
      }
      this._data[this._dataLen++] = pushLine;
      this._data[this._dataLen++] = pushChar;
      this._data[this._dataLen++] = length;
      this._data[this._dataLen++] = tokenType;
      this._data[this._dataLen++] = tokenModifiers;
      this._prevLine = line;
      this._prevChar = char;
    }
    get id() {
      return this._id.toString();
    }
    previousResult(id) {
      if (this.id === id) {
        this._prevData = this._data;
      }
      this.initialize();
    }
    build() {
      this._prevData = undefined;
      return {
        resultId: this.id,
        data: this._data
      };
    }
    canBuildEdits() {
      return this._prevData !== undefined;
    }
    buildEdits() {
      if (this._prevData !== undefined) {
        return {
          resultId: this.id,
          edits: new SemanticTokensDiff(this._prevData, this._data).computeDiff()
        };
      } else {
        return this.build();
      }
    }
  }
  exports.SemanticTokensBuilder = SemanticTokensBuilder;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/showDocument.js
var require_showDocument = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ShowDocumentFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var ShowDocumentFeature = (Base) => {
    return class extends Base {
      showDocument(params) {
        return this.connection.sendRequest(vscode_languageserver_protocol_1.ShowDocumentRequest.type, params);
      }
    };
  };
  exports.ShowDocumentFeature = ShowDocumentFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/fileOperations.js
var require_fileOperations = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.FileOperationsFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var FileOperationsFeature = (Base) => {
    return class extends Base {
      onDidCreateFiles(handler) {
        return this.connection.onNotification(vscode_languageserver_protocol_1.DidCreateFilesNotification.type, (params) => {
          handler(params);
        });
      }
      onDidRenameFiles(handler) {
        return this.connection.onNotification(vscode_languageserver_protocol_1.DidRenameFilesNotification.type, (params) => {
          handler(params);
        });
      }
      onDidDeleteFiles(handler) {
        return this.connection.onNotification(vscode_languageserver_protocol_1.DidDeleteFilesNotification.type, (params) => {
          handler(params);
        });
      }
      onWillCreateFiles(handler) {
        return this.connection.onRequest(vscode_languageserver_protocol_1.WillCreateFilesRequest.type, (params, cancel) => {
          return handler(params, cancel);
        });
      }
      onWillRenameFiles(handler) {
        return this.connection.onRequest(vscode_languageserver_protocol_1.WillRenameFilesRequest.type, (params, cancel) => {
          return handler(params, cancel);
        });
      }
      onWillDeleteFiles(handler) {
        return this.connection.onRequest(vscode_languageserver_protocol_1.WillDeleteFilesRequest.type, (params, cancel) => {
          return handler(params, cancel);
        });
      }
    };
  };
  exports.FileOperationsFeature = FileOperationsFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/linkedEditingRange.js
var require_linkedEditingRange = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.LinkedEditingRangeFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var LinkedEditingRangeFeature = (Base) => {
    return class extends Base {
      onLinkedEditingRange(handler) {
        return this.connection.onRequest(vscode_languageserver_protocol_1.LinkedEditingRangeRequest.type, (params, cancel) => {
          return handler(params, cancel, this.attachWorkDoneProgress(params), undefined);
        });
      }
    };
  };
  exports.LinkedEditingRangeFeature = LinkedEditingRangeFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/typeHierarchy.js
var require_typeHierarchy = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.TypeHierarchyFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var TypeHierarchyFeature = (Base) => {
    return class extends Base {
      get typeHierarchy() {
        return {
          onPrepare: (handler) => {
            return this.connection.onRequest(vscode_languageserver_protocol_1.TypeHierarchyPrepareRequest.type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), undefined);
            });
          },
          onSupertypes: (handler) => {
            const type = vscode_languageserver_protocol_1.TypeHierarchySupertypesRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          },
          onSubtypes: (handler) => {
            const type = vscode_languageserver_protocol_1.TypeHierarchySubtypesRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          }
        };
      }
    };
  };
  exports.TypeHierarchyFeature = TypeHierarchyFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/inlineValue.js
var require_inlineValue = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.InlineValueFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var InlineValueFeature = (Base) => {
    return class extends Base {
      get inlineValue() {
        return {
          refresh: () => {
            return this.connection.sendRequest(vscode_languageserver_protocol_1.InlineValueRefreshRequest.type);
          },
          on: (handler) => {
            return this.connection.onRequest(vscode_languageserver_protocol_1.InlineValueRequest.type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params));
            });
          }
        };
      }
    };
  };
  exports.InlineValueFeature = InlineValueFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/foldingRange.js
var require_foldingRange = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.FoldingRangeFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var FoldingRangeFeature = (Base) => {
    return class extends Base {
      get foldingRange() {
        return {
          refresh: () => {
            return this.connection.sendRequest(vscode_languageserver_protocol_1.FoldingRangeRefreshRequest.type);
          },
          on: (handler) => {
            const type = vscode_languageserver_protocol_1.FoldingRangeRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          }
        };
      }
    };
  };
  exports.FoldingRangeFeature = FoldingRangeFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/inlayHint.js
var require_inlayHint = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.InlayHintFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var InlayHintFeature = (Base) => {
    return class extends Base {
      get inlayHint() {
        return {
          refresh: () => {
            return this.connection.sendRequest(vscode_languageserver_protocol_1.InlayHintRefreshRequest.type);
          },
          on: (handler) => {
            return this.connection.onRequest(vscode_languageserver_protocol_1.InlayHintRequest.type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params));
            });
          },
          resolve: (handler) => {
            return this.connection.onRequest(vscode_languageserver_protocol_1.InlayHintResolveRequest.type, (params, cancel) => {
              return handler(params, cancel);
            });
          }
        };
      }
    };
  };
  exports.InlayHintFeature = InlayHintFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/diagnostic.js
var require_diagnostic = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.DiagnosticFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var DiagnosticFeature = (Base) => {
    return class extends Base {
      get diagnostics() {
        return {
          refresh: () => {
            return this.connection.sendRequest(vscode_languageserver_protocol_1.DiagnosticRefreshRequest.type);
          },
          on: (handler) => {
            return this.connection.onRequest(vscode_languageserver_protocol_1.DocumentDiagnosticRequest.type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(vscode_languageserver_protocol_1.DocumentDiagnosticRequest.partialResult, params));
            });
          },
          onWorkspace: (handler) => {
            return this.connection.onRequest(vscode_languageserver_protocol_1.WorkspaceDiagnosticRequest.type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(vscode_languageserver_protocol_1.WorkspaceDiagnosticRequest.partialResult, params));
            });
          }
        };
      }
    };
  };
  exports.DiagnosticFeature = DiagnosticFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/textDocuments.js
var require_textDocuments = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.TextDocuments = undefined;
  var vscode_languageserver_protocol_1 = require_main3();

  class TextDocuments {
    constructor(configuration) {
      this._configuration = configuration;
      this._syncedDocuments = new Map;
      this._onDidChangeContent = new vscode_languageserver_protocol_1.Emitter;
      this._onDidOpen = new vscode_languageserver_protocol_1.Emitter;
      this._onDidClose = new vscode_languageserver_protocol_1.Emitter;
      this._onDidSave = new vscode_languageserver_protocol_1.Emitter;
      this._onWillSave = new vscode_languageserver_protocol_1.Emitter;
    }
    get onDidOpen() {
      return this._onDidOpen.event;
    }
    get onDidChangeContent() {
      return this._onDidChangeContent.event;
    }
    get onWillSave() {
      return this._onWillSave.event;
    }
    onWillSaveWaitUntil(handler) {
      this._willSaveWaitUntil = handler;
    }
    get onDidSave() {
      return this._onDidSave.event;
    }
    get onDidClose() {
      return this._onDidClose.event;
    }
    get(uri) {
      return this._syncedDocuments.get(uri);
    }
    all() {
      return Array.from(this._syncedDocuments.values());
    }
    keys() {
      return Array.from(this._syncedDocuments.keys());
    }
    listen(connection) {
      connection.__textDocumentSync = vscode_languageserver_protocol_1.TextDocumentSyncKind.Incremental;
      const disposables = [];
      disposables.push(connection.onDidOpenTextDocument((event) => {
        const td = event.textDocument;
        const document = this._configuration.create(td.uri, td.languageId, td.version, td.text);
        this._syncedDocuments.set(td.uri, document);
        const toFire = Object.freeze({ document });
        this._onDidOpen.fire(toFire);
        this._onDidChangeContent.fire(toFire);
      }));
      disposables.push(connection.onDidChangeTextDocument((event) => {
        const td = event.textDocument;
        const changes = event.contentChanges;
        if (changes.length === 0) {
          return;
        }
        const { version } = td;
        if (version === null || version === undefined) {
          throw new Error(`Received document change event for ${td.uri} without valid version identifier`);
        }
        let syncedDocument = this._syncedDocuments.get(td.uri);
        if (syncedDocument !== undefined) {
          syncedDocument = this._configuration.update(syncedDocument, changes, version);
          this._syncedDocuments.set(td.uri, syncedDocument);
          this._onDidChangeContent.fire(Object.freeze({ document: syncedDocument }));
        }
      }));
      disposables.push(connection.onDidCloseTextDocument((event) => {
        let syncedDocument = this._syncedDocuments.get(event.textDocument.uri);
        if (syncedDocument !== undefined) {
          this._syncedDocuments.delete(event.textDocument.uri);
          this._onDidClose.fire(Object.freeze({ document: syncedDocument }));
        }
      }));
      disposables.push(connection.onWillSaveTextDocument((event) => {
        let syncedDocument = this._syncedDocuments.get(event.textDocument.uri);
        if (syncedDocument !== undefined) {
          this._onWillSave.fire(Object.freeze({ document: syncedDocument, reason: event.reason }));
        }
      }));
      disposables.push(connection.onWillSaveTextDocumentWaitUntil((event, token) => {
        let syncedDocument = this._syncedDocuments.get(event.textDocument.uri);
        if (syncedDocument !== undefined && this._willSaveWaitUntil) {
          return this._willSaveWaitUntil(Object.freeze({ document: syncedDocument, reason: event.reason }), token);
        } else {
          return [];
        }
      }));
      disposables.push(connection.onDidSaveTextDocument((event) => {
        let syncedDocument = this._syncedDocuments.get(event.textDocument.uri);
        if (syncedDocument !== undefined) {
          this._onDidSave.fire(Object.freeze({ document: syncedDocument }));
        }
      }));
      return vscode_languageserver_protocol_1.Disposable.create(() => {
        disposables.forEach((disposable) => disposable.dispose());
      });
    }
  }
  exports.TextDocuments = TextDocuments;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/notebook.js
var require_notebook = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.NotebookDocuments = exports.NotebookSyncFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var textDocuments_1 = require_textDocuments();
  var NotebookSyncFeature = (Base) => {
    return class extends Base {
      get synchronization() {
        return {
          onDidOpenNotebookDocument: (handler) => {
            return this.connection.onNotification(vscode_languageserver_protocol_1.DidOpenNotebookDocumentNotification.type, (params) => {
              handler(params);
            });
          },
          onDidChangeNotebookDocument: (handler) => {
            return this.connection.onNotification(vscode_languageserver_protocol_1.DidChangeNotebookDocumentNotification.type, (params) => {
              handler(params);
            });
          },
          onDidSaveNotebookDocument: (handler) => {
            return this.connection.onNotification(vscode_languageserver_protocol_1.DidSaveNotebookDocumentNotification.type, (params) => {
              handler(params);
            });
          },
          onDidCloseNotebookDocument: (handler) => {
            return this.connection.onNotification(vscode_languageserver_protocol_1.DidCloseNotebookDocumentNotification.type, (params) => {
              handler(params);
            });
          }
        };
      }
    };
  };
  exports.NotebookSyncFeature = NotebookSyncFeature;

  class CellTextDocumentConnection {
    onDidOpenTextDocument(handler) {
      this.openHandler = handler;
      return vscode_languageserver_protocol_1.Disposable.create(() => {
        this.openHandler = undefined;
      });
    }
    openTextDocument(params) {
      this.openHandler && this.openHandler(params);
    }
    onDidChangeTextDocument(handler) {
      this.changeHandler = handler;
      return vscode_languageserver_protocol_1.Disposable.create(() => {
        this.changeHandler = handler;
      });
    }
    changeTextDocument(params) {
      this.changeHandler && this.changeHandler(params);
    }
    onDidCloseTextDocument(handler) {
      this.closeHandler = handler;
      return vscode_languageserver_protocol_1.Disposable.create(() => {
        this.closeHandler = undefined;
      });
    }
    closeTextDocument(params) {
      this.closeHandler && this.closeHandler(params);
    }
    onWillSaveTextDocument() {
      return CellTextDocumentConnection.NULL_DISPOSE;
    }
    onWillSaveTextDocumentWaitUntil() {
      return CellTextDocumentConnection.NULL_DISPOSE;
    }
    onDidSaveTextDocument() {
      return CellTextDocumentConnection.NULL_DISPOSE;
    }
  }
  CellTextDocumentConnection.NULL_DISPOSE = Object.freeze({ dispose: () => {} });

  class NotebookDocuments {
    constructor(configurationOrTextDocuments) {
      if (configurationOrTextDocuments instanceof textDocuments_1.TextDocuments) {
        this._cellTextDocuments = configurationOrTextDocuments;
      } else {
        this._cellTextDocuments = new textDocuments_1.TextDocuments(configurationOrTextDocuments);
      }
      this.notebookDocuments = new Map;
      this.notebookCellMap = new Map;
      this._onDidOpen = new vscode_languageserver_protocol_1.Emitter;
      this._onDidChange = new vscode_languageserver_protocol_1.Emitter;
      this._onDidSave = new vscode_languageserver_protocol_1.Emitter;
      this._onDidClose = new vscode_languageserver_protocol_1.Emitter;
    }
    get cellTextDocuments() {
      return this._cellTextDocuments;
    }
    getCellTextDocument(cell) {
      return this._cellTextDocuments.get(cell.document);
    }
    getNotebookDocument(uri) {
      return this.notebookDocuments.get(uri);
    }
    getNotebookCell(uri) {
      const value = this.notebookCellMap.get(uri);
      return value && value[0];
    }
    findNotebookDocumentForCell(cell) {
      const key = typeof cell === "string" ? cell : cell.document;
      const value = this.notebookCellMap.get(key);
      return value && value[1];
    }
    get onDidOpen() {
      return this._onDidOpen.event;
    }
    get onDidSave() {
      return this._onDidSave.event;
    }
    get onDidChange() {
      return this._onDidChange.event;
    }
    get onDidClose() {
      return this._onDidClose.event;
    }
    listen(connection) {
      const cellTextDocumentConnection = new CellTextDocumentConnection;
      const disposables = [];
      disposables.push(this.cellTextDocuments.listen(cellTextDocumentConnection));
      disposables.push(connection.notebooks.synchronization.onDidOpenNotebookDocument((params) => {
        this.notebookDocuments.set(params.notebookDocument.uri, params.notebookDocument);
        for (const cellTextDocument of params.cellTextDocuments) {
          cellTextDocumentConnection.openTextDocument({ textDocument: cellTextDocument });
        }
        this.updateCellMap(params.notebookDocument);
        this._onDidOpen.fire(params.notebookDocument);
      }));
      disposables.push(connection.notebooks.synchronization.onDidChangeNotebookDocument((params) => {
        const notebookDocument = this.notebookDocuments.get(params.notebookDocument.uri);
        if (notebookDocument === undefined) {
          return;
        }
        notebookDocument.version = params.notebookDocument.version;
        const oldMetadata = notebookDocument.metadata;
        let metadataChanged = false;
        const change = params.change;
        if (change.metadata !== undefined) {
          metadataChanged = true;
          notebookDocument.metadata = change.metadata;
        }
        const opened = [];
        const closed = [];
        const data = [];
        const text = [];
        if (change.cells !== undefined) {
          const changedCells = change.cells;
          if (changedCells.structure !== undefined) {
            const array = changedCells.structure.array;
            notebookDocument.cells.splice(array.start, array.deleteCount, ...array.cells !== undefined ? array.cells : []);
            if (changedCells.structure.didOpen !== undefined) {
              for (const open of changedCells.structure.didOpen) {
                cellTextDocumentConnection.openTextDocument({ textDocument: open });
                opened.push(open.uri);
              }
            }
            if (changedCells.structure.didClose) {
              for (const close of changedCells.structure.didClose) {
                cellTextDocumentConnection.closeTextDocument({ textDocument: close });
                closed.push(close.uri);
              }
            }
          }
          if (changedCells.data !== undefined) {
            const cellUpdates = new Map(changedCells.data.map((cell) => [cell.document, cell]));
            for (let i = 0;i <= notebookDocument.cells.length; i++) {
              const change2 = cellUpdates.get(notebookDocument.cells[i].document);
              if (change2 !== undefined) {
                const old = notebookDocument.cells.splice(i, 1, change2);
                data.push({ old: old[0], new: change2 });
                cellUpdates.delete(change2.document);
                if (cellUpdates.size === 0) {
                  break;
                }
              }
            }
          }
          if (changedCells.textContent !== undefined) {
            for (const cellTextDocument of changedCells.textContent) {
              cellTextDocumentConnection.changeTextDocument({ textDocument: cellTextDocument.document, contentChanges: cellTextDocument.changes });
              text.push(cellTextDocument.document.uri);
            }
          }
        }
        this.updateCellMap(notebookDocument);
        const changeEvent = { notebookDocument };
        if (metadataChanged) {
          changeEvent.metadata = { old: oldMetadata, new: notebookDocument.metadata };
        }
        const added = [];
        for (const open of opened) {
          added.push(this.getNotebookCell(open));
        }
        const removed = [];
        for (const close of closed) {
          removed.push(this.getNotebookCell(close));
        }
        const textContent = [];
        for (const change2 of text) {
          textContent.push(this.getNotebookCell(change2));
        }
        if (added.length > 0 || removed.length > 0 || data.length > 0 || textContent.length > 0) {
          changeEvent.cells = { added, removed, changed: { data, textContent } };
        }
        if (changeEvent.metadata !== undefined || changeEvent.cells !== undefined) {
          this._onDidChange.fire(changeEvent);
        }
      }));
      disposables.push(connection.notebooks.synchronization.onDidSaveNotebookDocument((params) => {
        const notebookDocument = this.notebookDocuments.get(params.notebookDocument.uri);
        if (notebookDocument === undefined) {
          return;
        }
        this._onDidSave.fire(notebookDocument);
      }));
      disposables.push(connection.notebooks.synchronization.onDidCloseNotebookDocument((params) => {
        const notebookDocument = this.notebookDocuments.get(params.notebookDocument.uri);
        if (notebookDocument === undefined) {
          return;
        }
        this._onDidClose.fire(notebookDocument);
        for (const cellTextDocument of params.cellTextDocuments) {
          cellTextDocumentConnection.closeTextDocument({ textDocument: cellTextDocument });
        }
        this.notebookDocuments.delete(params.notebookDocument.uri);
        for (const cell of notebookDocument.cells) {
          this.notebookCellMap.delete(cell.document);
        }
      }));
      return vscode_languageserver_protocol_1.Disposable.create(() => {
        disposables.forEach((disposable) => disposable.dispose());
      });
    }
    updateCellMap(notebookDocument) {
      for (const cell of notebookDocument.cells) {
        this.notebookCellMap.set(cell.document, [cell, notebookDocument]);
      }
    }
  }
  exports.NotebookDocuments = NotebookDocuments;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/moniker.js
var require_moniker = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.MonikerFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var MonikerFeature = (Base) => {
    return class extends Base {
      get moniker() {
        return {
          on: (handler) => {
            const type = vscode_languageserver_protocol_1.MonikerRequest.type;
            return this.connection.onRequest(type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
            });
          }
        };
      }
    };
  };
  exports.MonikerFeature = MonikerFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/server.js
var require_server = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.createConnection = exports.combineFeatures = exports.combineNotebooksFeatures = exports.combineLanguagesFeatures = exports.combineWorkspaceFeatures = exports.combineWindowFeatures = exports.combineClientFeatures = exports.combineTracerFeatures = exports.combineTelemetryFeatures = exports.combineConsoleFeatures = exports._NotebooksImpl = exports._LanguagesImpl = exports.BulkUnregistration = exports.BulkRegistration = exports.ErrorMessageTracker = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var Is = require_is();
  var UUID = require_uuid();
  var progress_1 = require_progress();
  var configuration_1 = require_configuration();
  var workspaceFolder_1 = require_workspaceFolder();
  var callHierarchy_1 = require_callHierarchy();
  var semanticTokens_1 = require_semanticTokens();
  var showDocument_1 = require_showDocument();
  var fileOperations_1 = require_fileOperations();
  var linkedEditingRange_1 = require_linkedEditingRange();
  var typeHierarchy_1 = require_typeHierarchy();
  var inlineValue_1 = require_inlineValue();
  var foldingRange_1 = require_foldingRange();
  var inlayHint_1 = require_inlayHint();
  var diagnostic_1 = require_diagnostic();
  var notebook_1 = require_notebook();
  var moniker_1 = require_moniker();
  function null2Undefined(value) {
    if (value === null) {
      return;
    }
    return value;
  }

  class ErrorMessageTracker {
    constructor() {
      this._messages = Object.create(null);
    }
    add(message) {
      let count = this._messages[message];
      if (!count) {
        count = 0;
      }
      count++;
      this._messages[message] = count;
    }
    sendErrors(connection) {
      Object.keys(this._messages).forEach((message) => {
        connection.window.showErrorMessage(message);
      });
    }
  }
  exports.ErrorMessageTracker = ErrorMessageTracker;

  class RemoteConsoleImpl {
    constructor() {}
    rawAttach(connection) {
      this._rawConnection = connection;
    }
    attach(connection) {
      this._connection = connection;
    }
    get connection() {
      if (!this._connection) {
        throw new Error("Remote is not attached to a connection yet.");
      }
      return this._connection;
    }
    fillServerCapabilities(_capabilities) {}
    initialize(_capabilities) {}
    error(message) {
      this.send(vscode_languageserver_protocol_1.MessageType.Error, message);
    }
    warn(message) {
      this.send(vscode_languageserver_protocol_1.MessageType.Warning, message);
    }
    info(message) {
      this.send(vscode_languageserver_protocol_1.MessageType.Info, message);
    }
    log(message) {
      this.send(vscode_languageserver_protocol_1.MessageType.Log, message);
    }
    debug(message) {
      this.send(vscode_languageserver_protocol_1.MessageType.Debug, message);
    }
    send(type, message) {
      if (this._rawConnection) {
        this._rawConnection.sendNotification(vscode_languageserver_protocol_1.LogMessageNotification.type, { type, message }).catch(() => {
          (0, vscode_languageserver_protocol_1.RAL)().console.error(`Sending log message failed`);
        });
      }
    }
  }

  class _RemoteWindowImpl {
    constructor() {}
    attach(connection) {
      this._connection = connection;
    }
    get connection() {
      if (!this._connection) {
        throw new Error("Remote is not attached to a connection yet.");
      }
      return this._connection;
    }
    initialize(_capabilities) {}
    fillServerCapabilities(_capabilities) {}
    showErrorMessage(message, ...actions) {
      let params = { type: vscode_languageserver_protocol_1.MessageType.Error, message, actions };
      return this.connection.sendRequest(vscode_languageserver_protocol_1.ShowMessageRequest.type, params).then(null2Undefined);
    }
    showWarningMessage(message, ...actions) {
      let params = { type: vscode_languageserver_protocol_1.MessageType.Warning, message, actions };
      return this.connection.sendRequest(vscode_languageserver_protocol_1.ShowMessageRequest.type, params).then(null2Undefined);
    }
    showInformationMessage(message, ...actions) {
      let params = { type: vscode_languageserver_protocol_1.MessageType.Info, message, actions };
      return this.connection.sendRequest(vscode_languageserver_protocol_1.ShowMessageRequest.type, params).then(null2Undefined);
    }
  }
  var RemoteWindowImpl = (0, showDocument_1.ShowDocumentFeature)((0, progress_1.ProgressFeature)(_RemoteWindowImpl));
  var BulkRegistration;
  (function(BulkRegistration2) {
    function create() {
      return new BulkRegistrationImpl;
    }
    BulkRegistration2.create = create;
  })(BulkRegistration || (exports.BulkRegistration = BulkRegistration = {}));

  class BulkRegistrationImpl {
    constructor() {
      this._registrations = [];
      this._registered = new Set;
    }
    add(type, registerOptions) {
      const method = Is.string(type) ? type : type.method;
      if (this._registered.has(method)) {
        throw new Error(`${method} is already added to this registration`);
      }
      const id = UUID.generateUuid();
      this._registrations.push({
        id,
        method,
        registerOptions: registerOptions || {}
      });
      this._registered.add(method);
    }
    asRegistrationParams() {
      return {
        registrations: this._registrations
      };
    }
  }
  var BulkUnregistration;
  (function(BulkUnregistration2) {
    function create() {
      return new BulkUnregistrationImpl(undefined, []);
    }
    BulkUnregistration2.create = create;
  })(BulkUnregistration || (exports.BulkUnregistration = BulkUnregistration = {}));

  class BulkUnregistrationImpl {
    constructor(_connection, unregistrations) {
      this._connection = _connection;
      this._unregistrations = new Map;
      unregistrations.forEach((unregistration) => {
        this._unregistrations.set(unregistration.method, unregistration);
      });
    }
    get isAttached() {
      return !!this._connection;
    }
    attach(connection) {
      this._connection = connection;
    }
    add(unregistration) {
      this._unregistrations.set(unregistration.method, unregistration);
    }
    dispose() {
      let unregistrations = [];
      for (let unregistration of this._unregistrations.values()) {
        unregistrations.push(unregistration);
      }
      let params = {
        unregisterations: unregistrations
      };
      this._connection.sendRequest(vscode_languageserver_protocol_1.UnregistrationRequest.type, params).catch(() => {
        this._connection.console.info(`Bulk unregistration failed.`);
      });
    }
    disposeSingle(arg) {
      const method = Is.string(arg) ? arg : arg.method;
      const unregistration = this._unregistrations.get(method);
      if (!unregistration) {
        return false;
      }
      let params = {
        unregisterations: [unregistration]
      };
      this._connection.sendRequest(vscode_languageserver_protocol_1.UnregistrationRequest.type, params).then(() => {
        this._unregistrations.delete(method);
      }, (_error) => {
        this._connection.console.info(`Un-registering request handler for ${unregistration.id} failed.`);
      });
      return true;
    }
  }

  class RemoteClientImpl {
    attach(connection) {
      this._connection = connection;
    }
    get connection() {
      if (!this._connection) {
        throw new Error("Remote is not attached to a connection yet.");
      }
      return this._connection;
    }
    initialize(_capabilities) {}
    fillServerCapabilities(_capabilities) {}
    register(typeOrRegistrations, registerOptionsOrType, registerOptions) {
      if (typeOrRegistrations instanceof BulkRegistrationImpl) {
        return this.registerMany(typeOrRegistrations);
      } else if (typeOrRegistrations instanceof BulkUnregistrationImpl) {
        return this.registerSingle1(typeOrRegistrations, registerOptionsOrType, registerOptions);
      } else {
        return this.registerSingle2(typeOrRegistrations, registerOptionsOrType);
      }
    }
    registerSingle1(unregistration, type, registerOptions) {
      const method = Is.string(type) ? type : type.method;
      const id = UUID.generateUuid();
      let params = {
        registrations: [{ id, method, registerOptions: registerOptions || {} }]
      };
      if (!unregistration.isAttached) {
        unregistration.attach(this.connection);
      }
      return this.connection.sendRequest(vscode_languageserver_protocol_1.RegistrationRequest.type, params).then((_result) => {
        unregistration.add({ id, method });
        return unregistration;
      }, (_error) => {
        this.connection.console.info(`Registering request handler for ${method} failed.`);
        return Promise.reject(_error);
      });
    }
    registerSingle2(type, registerOptions) {
      const method = Is.string(type) ? type : type.method;
      const id = UUID.generateUuid();
      let params = {
        registrations: [{ id, method, registerOptions: registerOptions || {} }]
      };
      return this.connection.sendRequest(vscode_languageserver_protocol_1.RegistrationRequest.type, params).then((_result) => {
        return vscode_languageserver_protocol_1.Disposable.create(() => {
          this.unregisterSingle(id, method).catch(() => {
            this.connection.console.info(`Un-registering capability with id ${id} failed.`);
          });
        });
      }, (_error) => {
        this.connection.console.info(`Registering request handler for ${method} failed.`);
        return Promise.reject(_error);
      });
    }
    unregisterSingle(id, method) {
      let params = {
        unregisterations: [{ id, method }]
      };
      return this.connection.sendRequest(vscode_languageserver_protocol_1.UnregistrationRequest.type, params).catch(() => {
        this.connection.console.info(`Un-registering request handler for ${id} failed.`);
      });
    }
    registerMany(registrations) {
      let params = registrations.asRegistrationParams();
      return this.connection.sendRequest(vscode_languageserver_protocol_1.RegistrationRequest.type, params).then(() => {
        return new BulkUnregistrationImpl(this._connection, params.registrations.map((registration) => {
          return { id: registration.id, method: registration.method };
        }));
      }, (_error) => {
        this.connection.console.info(`Bulk registration failed.`);
        return Promise.reject(_error);
      });
    }
  }

  class _RemoteWorkspaceImpl {
    constructor() {}
    attach(connection) {
      this._connection = connection;
    }
    get connection() {
      if (!this._connection) {
        throw new Error("Remote is not attached to a connection yet.");
      }
      return this._connection;
    }
    initialize(_capabilities) {}
    fillServerCapabilities(_capabilities) {}
    applyEdit(paramOrEdit) {
      function isApplyWorkspaceEditParams(value) {
        return value && !!value.edit;
      }
      let params = isApplyWorkspaceEditParams(paramOrEdit) ? paramOrEdit : { edit: paramOrEdit };
      return this.connection.sendRequest(vscode_languageserver_protocol_1.ApplyWorkspaceEditRequest.type, params);
    }
  }
  var RemoteWorkspaceImpl = (0, fileOperations_1.FileOperationsFeature)((0, workspaceFolder_1.WorkspaceFoldersFeature)((0, configuration_1.ConfigurationFeature)(_RemoteWorkspaceImpl)));

  class TracerImpl {
    constructor() {
      this._trace = vscode_languageserver_protocol_1.Trace.Off;
    }
    attach(connection) {
      this._connection = connection;
    }
    get connection() {
      if (!this._connection) {
        throw new Error("Remote is not attached to a connection yet.");
      }
      return this._connection;
    }
    initialize(_capabilities) {}
    fillServerCapabilities(_capabilities) {}
    set trace(value) {
      this._trace = value;
    }
    log(message, verbose) {
      if (this._trace === vscode_languageserver_protocol_1.Trace.Off) {
        return;
      }
      this.connection.sendNotification(vscode_languageserver_protocol_1.LogTraceNotification.type, {
        message,
        verbose: this._trace === vscode_languageserver_protocol_1.Trace.Verbose ? verbose : undefined
      }).catch(() => {});
    }
  }

  class TelemetryImpl {
    constructor() {}
    attach(connection) {
      this._connection = connection;
    }
    get connection() {
      if (!this._connection) {
        throw new Error("Remote is not attached to a connection yet.");
      }
      return this._connection;
    }
    initialize(_capabilities) {}
    fillServerCapabilities(_capabilities) {}
    logEvent(data) {
      this.connection.sendNotification(vscode_languageserver_protocol_1.TelemetryEventNotification.type, data).catch(() => {
        this.connection.console.log(`Sending TelemetryEventNotification failed`);
      });
    }
  }

  class _LanguagesImpl {
    constructor() {}
    attach(connection) {
      this._connection = connection;
    }
    get connection() {
      if (!this._connection) {
        throw new Error("Remote is not attached to a connection yet.");
      }
      return this._connection;
    }
    initialize(_capabilities) {}
    fillServerCapabilities(_capabilities) {}
    attachWorkDoneProgress(params) {
      return (0, progress_1.attachWorkDone)(this.connection, params);
    }
    attachPartialResultProgress(_type, params) {
      return (0, progress_1.attachPartialResult)(this.connection, params);
    }
  }
  exports._LanguagesImpl = _LanguagesImpl;
  var LanguagesImpl = (0, foldingRange_1.FoldingRangeFeature)((0, moniker_1.MonikerFeature)((0, diagnostic_1.DiagnosticFeature)((0, inlayHint_1.InlayHintFeature)((0, inlineValue_1.InlineValueFeature)((0, typeHierarchy_1.TypeHierarchyFeature)((0, linkedEditingRange_1.LinkedEditingRangeFeature)((0, semanticTokens_1.SemanticTokensFeature)((0, callHierarchy_1.CallHierarchyFeature)(_LanguagesImpl)))))))));

  class _NotebooksImpl {
    constructor() {}
    attach(connection) {
      this._connection = connection;
    }
    get connection() {
      if (!this._connection) {
        throw new Error("Remote is not attached to a connection yet.");
      }
      return this._connection;
    }
    initialize(_capabilities) {}
    fillServerCapabilities(_capabilities) {}
    attachWorkDoneProgress(params) {
      return (0, progress_1.attachWorkDone)(this.connection, params);
    }
    attachPartialResultProgress(_type, params) {
      return (0, progress_1.attachPartialResult)(this.connection, params);
    }
  }
  exports._NotebooksImpl = _NotebooksImpl;
  var NotebooksImpl = (0, notebook_1.NotebookSyncFeature)(_NotebooksImpl);
  function combineConsoleFeatures(one, two) {
    return function(Base) {
      return two(one(Base));
    };
  }
  exports.combineConsoleFeatures = combineConsoleFeatures;
  function combineTelemetryFeatures(one, two) {
    return function(Base) {
      return two(one(Base));
    };
  }
  exports.combineTelemetryFeatures = combineTelemetryFeatures;
  function combineTracerFeatures(one, two) {
    return function(Base) {
      return two(one(Base));
    };
  }
  exports.combineTracerFeatures = combineTracerFeatures;
  function combineClientFeatures(one, two) {
    return function(Base) {
      return two(one(Base));
    };
  }
  exports.combineClientFeatures = combineClientFeatures;
  function combineWindowFeatures(one, two) {
    return function(Base) {
      return two(one(Base));
    };
  }
  exports.combineWindowFeatures = combineWindowFeatures;
  function combineWorkspaceFeatures(one, two) {
    return function(Base) {
      return two(one(Base));
    };
  }
  exports.combineWorkspaceFeatures = combineWorkspaceFeatures;
  function combineLanguagesFeatures(one, two) {
    return function(Base) {
      return two(one(Base));
    };
  }
  exports.combineLanguagesFeatures = combineLanguagesFeatures;
  function combineNotebooksFeatures(one, two) {
    return function(Base) {
      return two(one(Base));
    };
  }
  exports.combineNotebooksFeatures = combineNotebooksFeatures;
  function combineFeatures(one, two) {
    function combine(one2, two2, func) {
      if (one2 && two2) {
        return func(one2, two2);
      } else if (one2) {
        return one2;
      } else {
        return two2;
      }
    }
    let result = {
      __brand: "features",
      console: combine(one.console, two.console, combineConsoleFeatures),
      tracer: combine(one.tracer, two.tracer, combineTracerFeatures),
      telemetry: combine(one.telemetry, two.telemetry, combineTelemetryFeatures),
      client: combine(one.client, two.client, combineClientFeatures),
      window: combine(one.window, two.window, combineWindowFeatures),
      workspace: combine(one.workspace, two.workspace, combineWorkspaceFeatures),
      languages: combine(one.languages, two.languages, combineLanguagesFeatures),
      notebooks: combine(one.notebooks, two.notebooks, combineNotebooksFeatures)
    };
    return result;
  }
  exports.combineFeatures = combineFeatures;
  function createConnection(connectionFactory, watchDog, factories) {
    const logger = factories && factories.console ? new (factories.console(RemoteConsoleImpl)) : new RemoteConsoleImpl;
    const connection = connectionFactory(logger);
    logger.rawAttach(connection);
    const tracer = factories && factories.tracer ? new (factories.tracer(TracerImpl)) : new TracerImpl;
    const telemetry = factories && factories.telemetry ? new (factories.telemetry(TelemetryImpl)) : new TelemetryImpl;
    const client = factories && factories.client ? new (factories.client(RemoteClientImpl)) : new RemoteClientImpl;
    const remoteWindow = factories && factories.window ? new (factories.window(RemoteWindowImpl)) : new RemoteWindowImpl;
    const workspace = factories && factories.workspace ? new (factories.workspace(RemoteWorkspaceImpl)) : new RemoteWorkspaceImpl;
    const languages = factories && factories.languages ? new (factories.languages(LanguagesImpl)) : new LanguagesImpl;
    const notebooks = factories && factories.notebooks ? new (factories.notebooks(NotebooksImpl)) : new NotebooksImpl;
    const allRemotes = [logger, tracer, telemetry, client, remoteWindow, workspace, languages, notebooks];
    function asPromise(value) {
      if (value instanceof Promise) {
        return value;
      } else if (Is.thenable(value)) {
        return new Promise((resolve, reject) => {
          value.then((resolved) => resolve(resolved), (error) => reject(error));
        });
      } else {
        return Promise.resolve(value);
      }
    }
    let shutdownHandler = undefined;
    let initializeHandler = undefined;
    let exitHandler = undefined;
    let protocolConnection = {
      listen: () => connection.listen(),
      sendRequest: (type, ...params) => connection.sendRequest(Is.string(type) ? type : type.method, ...params),
      onRequest: (type, handler) => connection.onRequest(type, handler),
      sendNotification: (type, param) => {
        const method = Is.string(type) ? type : type.method;
        return connection.sendNotification(method, param);
      },
      onNotification: (type, handler) => connection.onNotification(type, handler),
      onProgress: connection.onProgress,
      sendProgress: connection.sendProgress,
      onInitialize: (handler) => {
        initializeHandler = handler;
        return {
          dispose: () => {
            initializeHandler = undefined;
          }
        };
      },
      onInitialized: (handler) => connection.onNotification(vscode_languageserver_protocol_1.InitializedNotification.type, handler),
      onShutdown: (handler) => {
        shutdownHandler = handler;
        return {
          dispose: () => {
            shutdownHandler = undefined;
          }
        };
      },
      onExit: (handler) => {
        exitHandler = handler;
        return {
          dispose: () => {
            exitHandler = undefined;
          }
        };
      },
      get console() {
        return logger;
      },
      get telemetry() {
        return telemetry;
      },
      get tracer() {
        return tracer;
      },
      get client() {
        return client;
      },
      get window() {
        return remoteWindow;
      },
      get workspace() {
        return workspace;
      },
      get languages() {
        return languages;
      },
      get notebooks() {
        return notebooks;
      },
      onDidChangeConfiguration: (handler) => connection.onNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, handler),
      onDidChangeWatchedFiles: (handler) => connection.onNotification(vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type, handler),
      __textDocumentSync: undefined,
      onDidOpenTextDocument: (handler) => connection.onNotification(vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type, handler),
      onDidChangeTextDocument: (handler) => connection.onNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, handler),
      onDidCloseTextDocument: (handler) => connection.onNotification(vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type, handler),
      onWillSaveTextDocument: (handler) => connection.onNotification(vscode_languageserver_protocol_1.WillSaveTextDocumentNotification.type, handler),
      onWillSaveTextDocumentWaitUntil: (handler) => connection.onRequest(vscode_languageserver_protocol_1.WillSaveTextDocumentWaitUntilRequest.type, handler),
      onDidSaveTextDocument: (handler) => connection.onNotification(vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type, handler),
      sendDiagnostics: (params) => connection.sendNotification(vscode_languageserver_protocol_1.PublishDiagnosticsNotification.type, params),
      onHover: (handler) => connection.onRequest(vscode_languageserver_protocol_1.HoverRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), undefined);
      }),
      onCompletion: (handler) => connection.onRequest(vscode_languageserver_protocol_1.CompletionRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onCompletionResolve: (handler) => connection.onRequest(vscode_languageserver_protocol_1.CompletionResolveRequest.type, handler),
      onSignatureHelp: (handler) => connection.onRequest(vscode_languageserver_protocol_1.SignatureHelpRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), undefined);
      }),
      onDeclaration: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DeclarationRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onDefinition: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DefinitionRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onTypeDefinition: (handler) => connection.onRequest(vscode_languageserver_protocol_1.TypeDefinitionRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onImplementation: (handler) => connection.onRequest(vscode_languageserver_protocol_1.ImplementationRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onReferences: (handler) => connection.onRequest(vscode_languageserver_protocol_1.ReferencesRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onDocumentHighlight: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DocumentHighlightRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onDocumentSymbol: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DocumentSymbolRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onWorkspaceSymbol: (handler) => connection.onRequest(vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onWorkspaceSymbolResolve: (handler) => connection.onRequest(vscode_languageserver_protocol_1.WorkspaceSymbolResolveRequest.type, handler),
      onCodeAction: (handler) => connection.onRequest(vscode_languageserver_protocol_1.CodeActionRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onCodeActionResolve: (handler) => connection.onRequest(vscode_languageserver_protocol_1.CodeActionResolveRequest.type, (params, cancel) => {
        return handler(params, cancel);
      }),
      onCodeLens: (handler) => connection.onRequest(vscode_languageserver_protocol_1.CodeLensRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onCodeLensResolve: (handler) => connection.onRequest(vscode_languageserver_protocol_1.CodeLensResolveRequest.type, (params, cancel) => {
        return handler(params, cancel);
      }),
      onDocumentFormatting: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DocumentFormattingRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), undefined);
      }),
      onDocumentRangeFormatting: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), undefined);
      }),
      onDocumentOnTypeFormatting: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type, (params, cancel) => {
        return handler(params, cancel);
      }),
      onRenameRequest: (handler) => connection.onRequest(vscode_languageserver_protocol_1.RenameRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), undefined);
      }),
      onPrepareRename: (handler) => connection.onRequest(vscode_languageserver_protocol_1.PrepareRenameRequest.type, (params, cancel) => {
        return handler(params, cancel);
      }),
      onDocumentLinks: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DocumentLinkRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onDocumentLinkResolve: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DocumentLinkResolveRequest.type, (params, cancel) => {
        return handler(params, cancel);
      }),
      onDocumentColor: (handler) => connection.onRequest(vscode_languageserver_protocol_1.DocumentColorRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onColorPresentation: (handler) => connection.onRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onFoldingRanges: (handler) => connection.onRequest(vscode_languageserver_protocol_1.FoldingRangeRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onSelectionRanges: (handler) => connection.onRequest(vscode_languageserver_protocol_1.SelectionRangeRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), (0, progress_1.attachPartialResult)(connection, params));
      }),
      onExecuteCommand: (handler) => connection.onRequest(vscode_languageserver_protocol_1.ExecuteCommandRequest.type, (params, cancel) => {
        return handler(params, cancel, (0, progress_1.attachWorkDone)(connection, params), undefined);
      }),
      dispose: () => connection.dispose()
    };
    for (let remote of allRemotes) {
      remote.attach(protocolConnection);
    }
    connection.onRequest(vscode_languageserver_protocol_1.InitializeRequest.type, (params) => {
      watchDog.initialize(params);
      if (Is.string(params.trace)) {
        tracer.trace = vscode_languageserver_protocol_1.Trace.fromString(params.trace);
      }
      for (let remote of allRemotes) {
        remote.initialize(params.capabilities);
      }
      if (initializeHandler) {
        let result = initializeHandler(params, new vscode_languageserver_protocol_1.CancellationTokenSource().token, (0, progress_1.attachWorkDone)(connection, params), undefined);
        return asPromise(result).then((value) => {
          if (value instanceof vscode_languageserver_protocol_1.ResponseError) {
            return value;
          }
          let result2 = value;
          if (!result2) {
            result2 = { capabilities: {} };
          }
          let capabilities = result2.capabilities;
          if (!capabilities) {
            capabilities = {};
            result2.capabilities = capabilities;
          }
          if (capabilities.textDocumentSync === undefined || capabilities.textDocumentSync === null) {
            capabilities.textDocumentSync = Is.number(protocolConnection.__textDocumentSync) ? protocolConnection.__textDocumentSync : vscode_languageserver_protocol_1.TextDocumentSyncKind.None;
          } else if (!Is.number(capabilities.textDocumentSync) && !Is.number(capabilities.textDocumentSync.change)) {
            capabilities.textDocumentSync.change = Is.number(protocolConnection.__textDocumentSync) ? protocolConnection.__textDocumentSync : vscode_languageserver_protocol_1.TextDocumentSyncKind.None;
          }
          for (let remote of allRemotes) {
            remote.fillServerCapabilities(capabilities);
          }
          return result2;
        });
      } else {
        let result = { capabilities: { textDocumentSync: vscode_languageserver_protocol_1.TextDocumentSyncKind.None } };
        for (let remote of allRemotes) {
          remote.fillServerCapabilities(result.capabilities);
        }
        return result;
      }
    });
    connection.onRequest(vscode_languageserver_protocol_1.ShutdownRequest.type, () => {
      watchDog.shutdownReceived = true;
      if (shutdownHandler) {
        return shutdownHandler(new vscode_languageserver_protocol_1.CancellationTokenSource().token);
      } else {
        return;
      }
    });
    connection.onNotification(vscode_languageserver_protocol_1.ExitNotification.type, () => {
      try {
        if (exitHandler) {
          exitHandler();
        }
      } finally {
        if (watchDog.shutdownReceived) {
          watchDog.exit(0);
        } else {
          watchDog.exit(1);
        }
      }
    });
    connection.onNotification(vscode_languageserver_protocol_1.SetTraceNotification.type, (params) => {
      tracer.trace = vscode_languageserver_protocol_1.Trace.fromString(params.value);
    });
    return protocolConnection;
  }
  exports.createConnection = createConnection;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/node/files.js
var require_files = __commonJS((exports) => {
  var __filename = "/Users/bromero/personal/vibe/node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/node/files.js";
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.resolveModulePath = exports.FileSystem = exports.resolveGlobalYarnPath = exports.resolveGlobalNodePath = exports.resolve = exports.uriToFilePath = undefined;
  var url = __require("url");
  var path = __require("path");
  var fs = __require("fs");
  var child_process_1 = __require("child_process");
  function uriToFilePath(uri) {
    let parsed = url.parse(uri);
    if (parsed.protocol !== "file:" || !parsed.path) {
      return;
    }
    let segments = parsed.path.split("/");
    for (var i = 0, len = segments.length;i < len; i++) {
      segments[i] = decodeURIComponent(segments[i]);
    }
    if (process.platform === "win32" && segments.length > 1) {
      let first = segments[0];
      let second = segments[1];
      if (first.length === 0 && second.length > 1 && second[1] === ":") {
        segments.shift();
      }
    }
    return path.normalize(segments.join("/"));
  }
  exports.uriToFilePath = uriToFilePath;
  function isWindows() {
    return process.platform === "win32";
  }
  function resolve(moduleName, nodePath, cwd, tracer) {
    const nodePathKey = "NODE_PATH";
    const app = [
      "var p = process;",
      "p.on('message',function(m){",
      "if(m.c==='e'){",
      "p.exit(0);",
      "}",
      "else if(m.c==='rs'){",
      "try{",
      "var r=require.resolve(m.a);",
      "p.send({c:'r',s:true,r:r});",
      "}",
      "catch(err){",
      "p.send({c:'r',s:false});",
      "}",
      "}",
      "});"
    ].join("");
    return new Promise((resolve2, reject) => {
      let env = process.env;
      let newEnv = Object.create(null);
      Object.keys(env).forEach((key) => newEnv[key] = env[key]);
      if (nodePath && fs.existsSync(nodePath)) {
        if (newEnv[nodePathKey]) {
          newEnv[nodePathKey] = nodePath + path.delimiter + newEnv[nodePathKey];
        } else {
          newEnv[nodePathKey] = nodePath;
        }
        if (tracer) {
          tracer(`NODE_PATH value is: ${newEnv[nodePathKey]}`);
        }
      }
      newEnv["ELECTRON_RUN_AS_NODE"] = "1";
      try {
        let cp = (0, child_process_1.fork)("", [], {
          cwd,
          env: newEnv,
          execArgv: ["-e", app]
        });
        if (cp.pid === undefined) {
          reject(new Error(`Starting process to resolve node module  ${moduleName} failed`));
          return;
        }
        cp.on("error", (error) => {
          reject(error);
        });
        cp.on("message", (message2) => {
          if (message2.c === "r") {
            cp.send({ c: "e" });
            if (message2.s) {
              resolve2(message2.r);
            } else {
              reject(new Error(`Failed to resolve module: ${moduleName}`));
            }
          }
        });
        let message = {
          c: "rs",
          a: moduleName
        };
        cp.send(message);
      } catch (error) {
        reject(error);
      }
    });
  }
  exports.resolve = resolve;
  function resolveGlobalNodePath(tracer) {
    let npmCommand = "npm";
    const env = Object.create(null);
    Object.keys(process.env).forEach((key) => env[key] = process.env[key]);
    env["NO_UPDATE_NOTIFIER"] = "true";
    const options = {
      encoding: "utf8",
      env
    };
    if (isWindows()) {
      npmCommand = "npm.cmd";
      options.shell = true;
    }
    let handler = () => {};
    try {
      process.on("SIGPIPE", handler);
      let stdout = (0, child_process_1.spawnSync)(npmCommand, ["config", "get", "prefix"], options).stdout;
      if (!stdout) {
        if (tracer) {
          tracer(`'npm config get prefix' didn't return a value.`);
        }
        return;
      }
      let prefix = stdout.trim();
      if (tracer) {
        tracer(`'npm config get prefix' value is: ${prefix}`);
      }
      if (prefix.length > 0) {
        if (isWindows()) {
          return path.join(prefix, "node_modules");
        } else {
          return path.join(prefix, "lib", "node_modules");
        }
      }
      return;
    } catch (err) {
      return;
    } finally {
      process.removeListener("SIGPIPE", handler);
    }
  }
  exports.resolveGlobalNodePath = resolveGlobalNodePath;
  function resolveGlobalYarnPath(tracer) {
    let yarnCommand = "yarn";
    let options = {
      encoding: "utf8"
    };
    if (isWindows()) {
      yarnCommand = "yarn.cmd";
      options.shell = true;
    }
    let handler = () => {};
    try {
      process.on("SIGPIPE", handler);
      let results = (0, child_process_1.spawnSync)(yarnCommand, ["global", "dir", "--json"], options);
      let stdout = results.stdout;
      if (!stdout) {
        if (tracer) {
          tracer(`'yarn global dir' didn't return a value.`);
          if (results.stderr) {
            tracer(results.stderr);
          }
        }
        return;
      }
      let lines = stdout.trim().split(/\r?\n/);
      for (let line of lines) {
        try {
          let yarn = JSON.parse(line);
          if (yarn.type === "log") {
            return path.join(yarn.data, "node_modules");
          }
        } catch (e) {}
      }
      return;
    } catch (err) {
      return;
    } finally {
      process.removeListener("SIGPIPE", handler);
    }
  }
  exports.resolveGlobalYarnPath = resolveGlobalYarnPath;
  var FileSystem;
  (function(FileSystem2) {
    let _isCaseSensitive = undefined;
    function isCaseSensitive() {
      if (_isCaseSensitive !== undefined) {
        return _isCaseSensitive;
      }
      if (process.platform === "win32") {
        _isCaseSensitive = false;
      } else {
        _isCaseSensitive = !fs.existsSync(__filename.toUpperCase()) || !fs.existsSync(__filename.toLowerCase());
      }
      return _isCaseSensitive;
    }
    FileSystem2.isCaseSensitive = isCaseSensitive;
    function isParent(parent, child) {
      if (isCaseSensitive()) {
        return path.normalize(child).indexOf(path.normalize(parent)) === 0;
      } else {
        return path.normalize(child).toLowerCase().indexOf(path.normalize(parent).toLowerCase()) === 0;
      }
    }
    FileSystem2.isParent = isParent;
  })(FileSystem || (exports.FileSystem = FileSystem = {}));
  function resolveModulePath(workspaceRoot, moduleName, nodePath, tracer) {
    if (nodePath) {
      if (!path.isAbsolute(nodePath)) {
        nodePath = path.join(workspaceRoot, nodePath);
      }
      return resolve(moduleName, nodePath, nodePath, tracer).then((value) => {
        if (FileSystem.isParent(nodePath, value)) {
          return value;
        } else {
          return Promise.reject(new Error(`Failed to load ${moduleName} from node path location.`));
        }
      }).then(undefined, (_error) => {
        return resolve(moduleName, resolveGlobalNodePath(tracer), workspaceRoot, tracer);
      });
    } else {
      return resolve(moduleName, resolveGlobalNodePath(tracer), workspaceRoot, tracer);
    }
  }
  exports.resolveModulePath = resolveModulePath;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/inlineCompletion.proposed.js
var require_inlineCompletion_proposed = __commonJS((exports) => {
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.InlineCompletionFeature = undefined;
  var vscode_languageserver_protocol_1 = require_main3();
  var InlineCompletionFeature = (Base) => {
    return class extends Base {
      get inlineCompletion() {
        return {
          on: (handler) => {
            return this.connection.onRequest(vscode_languageserver_protocol_1.InlineCompletionRequest.type, (params, cancel) => {
              return handler(params, cancel, this.attachWorkDoneProgress(params));
            });
          }
        };
      }
    };
  };
  exports.InlineCompletionFeature = InlineCompletionFeature;
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/common/api.js
var require_api3 = __commonJS((exports) => {
  var __createBinding = exports && exports.__createBinding || (Object.create ? function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() {
        return m[k];
      } };
    }
    Object.defineProperty(o, k2, desc);
  } : function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    o[k2] = m[k];
  });
  var __exportStar = exports && exports.__exportStar || function(m, exports2) {
    for (var p in m)
      if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports2, p))
        __createBinding(exports2, m, p);
  };
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.ProposedFeatures = exports.NotebookDocuments = exports.TextDocuments = exports.SemanticTokensBuilder = undefined;
  var semanticTokens_1 = require_semanticTokens();
  Object.defineProperty(exports, "SemanticTokensBuilder", { enumerable: true, get: function() {
    return semanticTokens_1.SemanticTokensBuilder;
  } });
  var ic = require_inlineCompletion_proposed();
  __exportStar(require_main3(), exports);
  var textDocuments_1 = require_textDocuments();
  Object.defineProperty(exports, "TextDocuments", { enumerable: true, get: function() {
    return textDocuments_1.TextDocuments;
  } });
  var notebook_1 = require_notebook();
  Object.defineProperty(exports, "NotebookDocuments", { enumerable: true, get: function() {
    return notebook_1.NotebookDocuments;
  } });
  __exportStar(require_server(), exports);
  var ProposedFeatures;
  (function(ProposedFeatures2) {
    ProposedFeatures2.all = {
      __brand: "features",
      languages: ic.InlineCompletionFeature
    };
  })(ProposedFeatures || (exports.ProposedFeatures = ProposedFeatures = {}));
});

// ../../node_modules/.bun/vscode-languageserver@9.0.1/node_modules/vscode-languageserver/lib/node/main.js
var require_main4 = __commonJS((exports) => {
  var __createBinding = exports && exports.__createBinding || (Object.create ? function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() {
        return m[k];
      } };
    }
    Object.defineProperty(o, k2, desc);
  } : function(o, m, k, k2) {
    if (k2 === undefined)
      k2 = k;
    o[k2] = m[k];
  });
  var __exportStar = exports && exports.__exportStar || function(m, exports2) {
    for (var p in m)
      if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports2, p))
        __createBinding(exports2, m, p);
  };
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.createConnection = exports.Files = undefined;
  var node_util_1 = __require("node:util");
  var Is = require_is();
  var server_1 = require_server();
  var fm = require_files();
  var node_1 = require_main3();
  __exportStar(require_main3(), exports);
  __exportStar(require_api3(), exports);
  var Files;
  (function(Files2) {
    Files2.uriToFilePath = fm.uriToFilePath;
    Files2.resolveGlobalNodePath = fm.resolveGlobalNodePath;
    Files2.resolveGlobalYarnPath = fm.resolveGlobalYarnPath;
    Files2.resolve = fm.resolve;
    Files2.resolveModulePath = fm.resolveModulePath;
  })(Files || (exports.Files = Files = {}));
  var _protocolConnection;
  function endProtocolConnection() {
    if (_protocolConnection === undefined) {
      return;
    }
    try {
      _protocolConnection.end();
    } catch (_err) {}
  }
  var _shutdownReceived = false;
  var exitTimer = undefined;
  function setupExitTimer() {
    const argName = "--clientProcessId";
    function runTimer(value) {
      try {
        let processId = parseInt(value);
        if (!isNaN(processId)) {
          exitTimer = setInterval(() => {
            try {
              process.kill(processId, 0);
            } catch (ex) {
              endProtocolConnection();
              process.exit(_shutdownReceived ? 0 : 1);
            }
          }, 3000);
        }
      } catch (e) {}
    }
    for (let i = 2;i < process.argv.length; i++) {
      let arg = process.argv[i];
      if (arg === argName && i + 1 < process.argv.length) {
        runTimer(process.argv[i + 1]);
        return;
      } else {
        let args = arg.split("=");
        if (args[0] === argName) {
          runTimer(args[1]);
        }
      }
    }
  }
  setupExitTimer();
  var watchDog = {
    initialize: (params) => {
      const processId = params.processId;
      if (Is.number(processId) && exitTimer === undefined) {
        setInterval(() => {
          try {
            process.kill(processId, 0);
          } catch (ex) {
            process.exit(_shutdownReceived ? 0 : 1);
          }
        }, 3000);
      }
    },
    get shutdownReceived() {
      return _shutdownReceived;
    },
    set shutdownReceived(value) {
      _shutdownReceived = value;
    },
    exit: (code) => {
      endProtocolConnection();
      process.exit(code);
    }
  };
  function createConnection(arg1, arg2, arg3, arg4) {
    let factories;
    let input;
    let output;
    let options;
    if (arg1 !== undefined && arg1.__brand === "features") {
      factories = arg1;
      arg1 = arg2;
      arg2 = arg3;
      arg3 = arg4;
    }
    if (node_1.ConnectionStrategy.is(arg1) || node_1.ConnectionOptions.is(arg1)) {
      options = arg1;
    } else {
      input = arg1;
      output = arg2;
      options = arg3;
    }
    return _createConnection(input, output, options, factories);
  }
  exports.createConnection = createConnection;
  function _createConnection(input, output, options, factories) {
    let stdio = false;
    if (!input && !output && process.argv.length > 2) {
      let port = undefined;
      let pipeName = undefined;
      let argv = process.argv.slice(2);
      for (let i = 0;i < argv.length; i++) {
        let arg = argv[i];
        if (arg === "--node-ipc") {
          input = new node_1.IPCMessageReader(process);
          output = new node_1.IPCMessageWriter(process);
          break;
        } else if (arg === "--stdio") {
          stdio = true;
          input = process.stdin;
          output = process.stdout;
          break;
        } else if (arg === "--socket") {
          port = parseInt(argv[i + 1]);
          break;
        } else if (arg === "--pipe") {
          pipeName = argv[i + 1];
          break;
        } else {
          var args = arg.split("=");
          if (args[0] === "--socket") {
            port = parseInt(args[1]);
            break;
          } else if (args[0] === "--pipe") {
            pipeName = args[1];
            break;
          }
        }
      }
      if (port) {
        let transport = (0, node_1.createServerSocketTransport)(port);
        input = transport[0];
        output = transport[1];
      } else if (pipeName) {
        let transport = (0, node_1.createServerPipeTransport)(pipeName);
        input = transport[0];
        output = transport[1];
      }
    }
    var commandLineMessage = "Use arguments of createConnection or set command line parameters: '--node-ipc', '--stdio' or '--socket={number}'";
    if (!input) {
      throw new Error("Connection input stream is not set. " + commandLineMessage);
    }
    if (!output) {
      throw new Error("Connection output stream is not set. " + commandLineMessage);
    }
    if (Is.func(input.read) && Is.func(input.on)) {
      let inputStream = input;
      inputStream.on("end", () => {
        endProtocolConnection();
        process.exit(_shutdownReceived ? 0 : 1);
      });
      inputStream.on("close", () => {
        endProtocolConnection();
        process.exit(_shutdownReceived ? 0 : 1);
      });
    }
    const connectionFactory = (logger) => {
      const result = (0, node_1.createProtocolConnection)(input, output, logger, options);
      if (stdio) {
        patchConsole(logger);
      }
      return result;
    };
    return (0, server_1.createConnection)(connectionFactory, watchDog, factories);
  }
  function patchConsole(logger) {
    function serialize(args) {
      return args.map((arg) => typeof arg === "string" ? arg : (0, node_util_1.inspect)(arg)).join(" ");
    }
    const counters = new Map;
    console.assert = function assert(assertion, ...args) {
      if (assertion) {
        return;
      }
      if (args.length === 0) {
        logger.error("Assertion failed");
      } else {
        const [message, ...rest] = args;
        logger.error(`Assertion failed: ${message} ${serialize(rest)}`);
      }
    };
    console.count = function count(label = "default") {
      const message = String(label);
      let counter = counters.get(message) ?? 0;
      counter += 1;
      counters.set(message, counter);
      logger.log(`${message}: ${message}`);
    };
    console.countReset = function countReset(label) {
      if (label === undefined) {
        counters.clear();
      } else {
        counters.delete(String(label));
      }
    };
    console.debug = function debug(...args) {
      logger.log(serialize(args));
    };
    console.dir = function dir(arg, options) {
      logger.log((0, node_util_1.inspect)(arg, options));
    };
    console.log = function log(...args) {
      logger.log(serialize(args));
    };
    console.error = function error(...args) {
      logger.error(serialize(args));
    };
    console.trace = function trace(...args) {
      const stack = new Error().stack.replace(/(.+\n){2}/, "");
      let message = "Trace";
      if (args.length !== 0) {
        message += `: ${serialize(args)}`;
      }
      logger.log(`${message}
${stack}`);
    };
    console.warn = function warn(...args) {
      logger.warn(serialize(args));
    };
  }
});

// src/server.ts
var import_node = __toESM(require_main4(), 1);

// ../../node_modules/.bun/vscode-languageserver-textdocument@1.0.12/node_modules/vscode-languageserver-textdocument/lib/esm/main.js
class FullTextDocument {
  constructor(uri, languageId, version, content) {
    this._uri = uri;
    this._languageId = languageId;
    this._version = version;
    this._content = content;
    this._lineOffsets = undefined;
  }
  get uri() {
    return this._uri;
  }
  get languageId() {
    return this._languageId;
  }
  get version() {
    return this._version;
  }
  getText(range) {
    if (range) {
      const start = this.offsetAt(range.start);
      const end = this.offsetAt(range.end);
      return this._content.substring(start, end);
    }
    return this._content;
  }
  update(changes, version) {
    for (const change of changes) {
      if (FullTextDocument.isIncremental(change)) {
        const range = getWellformedRange(change.range);
        const startOffset = this.offsetAt(range.start);
        const endOffset = this.offsetAt(range.end);
        this._content = this._content.substring(0, startOffset) + change.text + this._content.substring(endOffset, this._content.length);
        const startLine = Math.max(range.start.line, 0);
        const endLine = Math.max(range.end.line, 0);
        let lineOffsets = this._lineOffsets;
        const addedLineOffsets = computeLineOffsets(change.text, false, startOffset);
        if (endLine - startLine === addedLineOffsets.length) {
          for (let i = 0, len = addedLineOffsets.length;i < len; i++) {
            lineOffsets[i + startLine + 1] = addedLineOffsets[i];
          }
        } else {
          if (addedLineOffsets.length < 1e4) {
            lineOffsets.splice(startLine + 1, endLine - startLine, ...addedLineOffsets);
          } else {
            this._lineOffsets = lineOffsets = lineOffsets.slice(0, startLine + 1).concat(addedLineOffsets, lineOffsets.slice(endLine + 1));
          }
        }
        const diff = change.text.length - (endOffset - startOffset);
        if (diff !== 0) {
          for (let i = startLine + 1 + addedLineOffsets.length, len = lineOffsets.length;i < len; i++) {
            lineOffsets[i] = lineOffsets[i] + diff;
          }
        }
      } else if (FullTextDocument.isFull(change)) {
        this._content = change.text;
        this._lineOffsets = undefined;
      } else {
        throw new Error("Unknown change event received");
      }
    }
    this._version = version;
  }
  getLineOffsets() {
    if (this._lineOffsets === undefined) {
      this._lineOffsets = computeLineOffsets(this._content, true);
    }
    return this._lineOffsets;
  }
  positionAt(offset) {
    offset = Math.max(Math.min(offset, this._content.length), 0);
    const lineOffsets = this.getLineOffsets();
    let low = 0, high = lineOffsets.length;
    if (high === 0) {
      return { line: 0, character: offset };
    }
    while (low < high) {
      const mid = Math.floor((low + high) / 2);
      if (lineOffsets[mid] > offset) {
        high = mid;
      } else {
        low = mid + 1;
      }
    }
    const line = low - 1;
    offset = this.ensureBeforeEOL(offset, lineOffsets[line]);
    return { line, character: offset - lineOffsets[line] };
  }
  offsetAt(position) {
    const lineOffsets = this.getLineOffsets();
    if (position.line >= lineOffsets.length) {
      return this._content.length;
    } else if (position.line < 0) {
      return 0;
    }
    const lineOffset = lineOffsets[position.line];
    if (position.character <= 0) {
      return lineOffset;
    }
    const nextLineOffset = position.line + 1 < lineOffsets.length ? lineOffsets[position.line + 1] : this._content.length;
    const offset = Math.min(lineOffset + position.character, nextLineOffset);
    return this.ensureBeforeEOL(offset, lineOffset);
  }
  ensureBeforeEOL(offset, lineOffset) {
    while (offset > lineOffset && isEOL(this._content.charCodeAt(offset - 1))) {
      offset--;
    }
    return offset;
  }
  get lineCount() {
    return this.getLineOffsets().length;
  }
  static isIncremental(event) {
    const candidate = event;
    return candidate !== undefined && candidate !== null && typeof candidate.text === "string" && candidate.range !== undefined && (candidate.rangeLength === undefined || typeof candidate.rangeLength === "number");
  }
  static isFull(event) {
    const candidate = event;
    return candidate !== undefined && candidate !== null && typeof candidate.text === "string" && candidate.range === undefined && candidate.rangeLength === undefined;
  }
}
var TextDocument;
(function(TextDocument2) {
  function create(uri, languageId, version, content) {
    return new FullTextDocument(uri, languageId, version, content);
  }
  TextDocument2.create = create;
  function update(document, changes, version) {
    if (document instanceof FullTextDocument) {
      document.update(changes, version);
      return document;
    } else {
      throw new Error("TextDocument.update: document must be created by TextDocument.create");
    }
  }
  TextDocument2.update = update;
  function applyEdits(document, edits) {
    const text = document.getText();
    const sortedEdits = mergeSort(edits.map(getWellformedEdit), (a, b) => {
      const diff = a.range.start.line - b.range.start.line;
      if (diff === 0) {
        return a.range.start.character - b.range.start.character;
      }
      return diff;
    });
    let lastModifiedOffset = 0;
    const spans = [];
    for (const e of sortedEdits) {
      const startOffset = document.offsetAt(e.range.start);
      if (startOffset < lastModifiedOffset) {
        throw new Error("Overlapping edit");
      } else if (startOffset > lastModifiedOffset) {
        spans.push(text.substring(lastModifiedOffset, startOffset));
      }
      if (e.newText.length) {
        spans.push(e.newText);
      }
      lastModifiedOffset = document.offsetAt(e.range.end);
    }
    spans.push(text.substr(lastModifiedOffset));
    return spans.join("");
  }
  TextDocument2.applyEdits = applyEdits;
})(TextDocument || (TextDocument = {}));
function mergeSort(data, compare) {
  if (data.length <= 1) {
    return data;
  }
  const p = data.length / 2 | 0;
  const left = data.slice(0, p);
  const right = data.slice(p);
  mergeSort(left, compare);
  mergeSort(right, compare);
  let leftIdx = 0;
  let rightIdx = 0;
  let i = 0;
  while (leftIdx < left.length && rightIdx < right.length) {
    const ret = compare(left[leftIdx], right[rightIdx]);
    if (ret <= 0) {
      data[i++] = left[leftIdx++];
    } else {
      data[i++] = right[rightIdx++];
    }
  }
  while (leftIdx < left.length) {
    data[i++] = left[leftIdx++];
  }
  while (rightIdx < right.length) {
    data[i++] = right[rightIdx++];
  }
  return data;
}
function computeLineOffsets(text, isAtLineStart, textOffset = 0) {
  const result = isAtLineStart ? [textOffset] : [];
  for (let i = 0;i < text.length; i++) {
    const ch = text.charCodeAt(i);
    if (isEOL(ch)) {
      if (ch === 13 && i + 1 < text.length && text.charCodeAt(i + 1) === 10) {
        i++;
      }
      result.push(textOffset + i + 1);
    }
  }
  return result;
}
function isEOL(char) {
  return char === 13 || char === 10;
}
function getWellformedRange(range) {
  const start = range.start;
  const end = range.end;
  if (start.line > end.line || start.line === end.line && start.character > end.character) {
    return { start: end, end: start };
  }
  return range;
}
function getWellformedEdit(textEdit) {
  const range = getWellformedRange(textEdit.range);
  if (range !== textEdit.range) {
    return { newText: textEdit.newText, range };
  }
  return textEdit;
}

// src/document-manager.ts
var import_vscode_languageserver = __toESM(require_main4(), 1);

// ../vibe-lexer/dist/Vibe/Vibe/Basics.js
var not = (b) => b ? false : true;
var flip = (f) => (x) => (y) => f(y)(x);

// ../vibe-lexer/dist/Vibe/Vibe/Int.ffi.js
var intAdd = (a, b) => a + b | 0;
var intSub = (a, b) => a - b | 0;
var intMul = (a, b) => a * b | 0;
var intDiv = (a, b) => Math.trunc(a / b);
var intMod = (a, b) => a % b;
var numEq = (a, b) => a === b;
var numLt = (a, b) => a < b;
var numGt = (a, b) => a > b;
var numToString = (n) => n.toString();

// ../vibe-lexer/dist/Vibe/Vibe/Int.js
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

// ../vibe-lexer/dist/Vibe/Vibe/Float.ffi.js
var numToString2 = (n) => n.toString();

// ../vibe-lexer/dist/Vibe/Vibe/Float.js
var _toString2 = ($a0) => numToString2($a0);
var $dict_Show_Float = {
  toString: _toString2
};

// ../vibe-lexer/dist/Vibe/Vibe/String.ffi.js
var stringAppend = (a, b) => a + b;
var stringEq = (a, b) => a === b;
var parseInt2 = (just, nothing, s) => {
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

// ../vibe-lexer/dist/Vibe/Vibe/Result.js
var Ok = ($0) => ({ $tag: 0, $0 });
var Err = ($0) => ({ $tag: 1, $0 });

// ../vibe-lexer/dist/Vibe/Vibe/Maybe.js
var Just = ($0) => ({ $tag: 0, $0 });
var Nothing = { $tag: 1 };

// ../vibe-lexer/dist/Vibe/Vibe/String.js
var _append = ($a0) => ($a1) => stringAppend($a0, $a1);
var _eq2 = ($a0) => ($a1) => stringEq($a0, $a1);
var _parseInt = ($a0) => ($a1) => ($a2) => parseInt2($a0, $a1, $a2);
var _parseFloat = ($a0) => ($a1) => ($a2) => parseFloat($a0, $a1, $a2);
var length = ($recv) => $recv.length;
var _charAt = ($a0) => ($a1) => ($a2) => ($a3) => stringCharAt($a0, $a1, $a2, $a3);
var unsafeCharAt2 = ($a0) => ($a1) => unsafeCharAt($a0, $a1);
var _slice = ($recv) => ($a0) => ($a1) => $recv.slice($a0, $a1);
var slice = (start) => (end) => (s) => _slice(s)(start)(end);
var toList = ($a0) => stringToList($a0);
var charAt = _charAt(Just)(Nothing);
var toInt = _parseInt(Just)(Nothing);
var toFloat = _parseFloat(Just)(Nothing);
var $default_Eq_String__SLASH_EQ = (x) => (y) => not(_eq2(x)(y));
var $dict_Eq_String = {
  _EQ_EQ: _eq2,
  _SLASH_EQ: $default_Eq_String__SLASH_EQ
};
var $dict_Appendable_String = {
  _PLUS_PLUS: _append
};

// ../vibe-lexer/dist/Vibe/Vibe/Char.ffi.js
var charToString = (a) => a;
var charToCode = (c) => c.codePointAt(0);
var charOrd = (a, b) => a < b;
var charOrdGt = (a, b) => a > b;

// ../vibe-lexer/dist/Vibe/Vibe/Char.js
var _PIPE_PIPE2 = (a) => (b) => a || b();
var _AMP_AMP = (a) => (b) => a && b();
var _toString3 = ($a0) => charToString($a0);
var _lt2 = ($a0) => ($a1) => charOrd($a0, $a1);
var _gt2 = ($a0) => ($a1) => charOrdGt($a0, $a1);
var toCode = ($a0) => charToCode($a0);
var $default_Ord_Char__LT_EQ = (x) => (y) => _PIPE_PIPE2(_lt2(x)(y))(() => $dict_Eq_Char._EQ_EQ(x)(y));
var $default_Ord_Char__GT_EQ = (x) => (y) => _PIPE_PIPE2(_gt2(x)(y))(() => $dict_Eq_Char._EQ_EQ(x)(y));
var $dict_Ord_Char = {
  _LT: _lt2,
  _GT: _gt2,
  _LT_EQ: $default_Ord_Char__LT_EQ,
  _GT_EQ: $default_Ord_Char__GT_EQ
};
var isUpper = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("A"))(() => $dict_Ord_Char._LT_EQ(c)("Z"));
var isLower = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("a"))(() => $dict_Ord_Char._LT_EQ(c)("z"));
var isAlpha = (c) => _PIPE_PIPE2(isUpper(c))(() => isLower(c));
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

// ../vibe-lexer/dist/Vibe/Vibe/List.ffi.js
var listNth = (just, nothing, n, lst) => {
  if (n >= 0 && n < lst.length) {
    return just(lst[n]);
  }
  return nothing;
};

// ../vibe-lexer/dist/Vibe/Vibe/List.js
var _concat = ($recv) => ($a0) => $recv.concat($a0);
var _COLON_COLON = (x) => (xs) => _concat([x])(xs);
var _map = ($recv) => ($a0) => $recv.map($a0);
var map = flip(_map);
var _filter = ($recv) => ($a0) => $recv.filter($a0);
var filter = flip(_filter);
var _nth = ($a0) => ($a1) => ($a2) => ($a3) => listNth($a0, $a1, $a2, $a3);
var nth = _nth(Just)(Nothing);
// ../vibe-lexer/dist/VibeLexer/VibeLexer/Types.js
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
var Eof = { $tag: 24 };

// ../vibe-lexer/dist/Json/Json/Encode.js
var JsonString = ($0) => ({ $tag: 0, $0 });
var JsonInt = ($0) => ({ $tag: 1, $0 });
var JsonBool = ($0) => ({ $tag: 3, $0 });
var JsonArray = ($0) => ({ $tag: 5, $0 });
var JsonObject = ($0) => ({ $tag: 6, $0 });
var string = (s) => JsonString(s);
var int = (n) => JsonInt(n);
var bool = (b) => JsonBool(b);
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

// ../vibe-lexer/dist/VibeLexer/VibeLexer/Json.js
var tokenKindToString = (kind) => (($match_0) => {
  if ($match_0.$tag === 0) {
    return "LowerIdentifier";
  }
  if ($match_0.$tag === 1) {
    return "UpperIdentifier";
  }
  if ($match_0.$tag === 2) {
    return "Keyword";
  }
  if ($match_0.$tag === 3) {
    return "Number";
  }
  if ($match_0.$tag === 4) {
    return "String";
  }
  if ($match_0.$tag === 5) {
    return "Char";
  }
  if ($match_0.$tag === 6) {
    return "Operator";
  }
  if ($match_0.$tag === 7) {
    return "Range";
  }
  if ($match_0.$tag === 8) {
    return "Backslash";
  }
  if ($match_0.$tag === 9) {
    return "LParen";
  }
  if ($match_0.$tag === 10) {
    return "RParen";
  }
  if ($match_0.$tag === 11) {
    return "LBrace";
  }
  if ($match_0.$tag === 12) {
    return "RBrace";
  }
  if ($match_0.$tag === 13) {
    return "LBracket";
  }
  if ($match_0.$tag === 14) {
    return "RBracket";
  }
  if ($match_0.$tag === 15) {
    return "Comma";
  }
  if ($match_0.$tag === 16) {
    return "Dot";
  }
  if ($match_0.$tag === 17) {
    return "Colon";
  }
  if ($match_0.$tag === 18) {
    return "Equals";
  }
  if ($match_0.$tag === 19) {
    return "Pipe";
  }
  if ($match_0.$tag === 20) {
    return "Newline";
  }
  if ($match_0.$tag === 21) {
    return "BlockStart";
  }
  if ($match_0.$tag === 22) {
    return "BlockSep";
  }
  if ($match_0.$tag === 23) {
    return "BlockEnd";
  }
  if ($match_0.$tag === 24) {
    return "Eof";
  }
  throw new Error("Pattern match failed");
})(kind);
var encodePosition = (pos) => object([["offset", int(pos.offset)], ["line", int(pos.line)], ["column", int(pos.column)]]);
var encodeSpan = (span) => object([["start", encodePosition(span.start)], ["end", encodePosition(span.end)]]);
var encodeToken = (tok) => object([["kind", string(tokenKindToString(tok.kind))], ["lexeme", string(tok.lexeme)], ["span", encodeSpan(tok.span)]]);
var lexToJson = (lexFn) => (source) => (($match_1) => {
  if ($match_1.$tag === 0) {
    const tokens = $match_1.$0;
    return encode(0)(object([["ok", bool(true)], ["tokens", list(encodeToken)(tokens)]]));
  }
  if ($match_1.$tag === 1) {
    const err = $match_1.$0;
    return encode(0)(object([["ok", bool(false)], ["message", string(err.message)], ["span", encodeSpan(err.span)]]));
  }
  throw new Error("Pattern match failed");
})(lexFn(source));

// ../vibe-lexer/dist/VibeLexer/VibeLexer.js
var _PIPE_PIPE3 = (a) => (b) => a || b();
var _AMP_AMP2 = (a) => (b) => a && b();
var initState = (source) => ({ source, index: 0, line: 1, column: 1, sourceLen: length(source) });
var isAtEnd = (state) => $dict_Ord_Int._GT_EQ(state.index)(state.sourceLen);
var peek = (state) => charAt(state.index)(state.source);
var peekAt = (offset) => (state) => charAt($dict_Num_Int._PLUS(state.index)(offset))(state.source);
var position = (state) => ({ offset: state.index, line: state.line, column: state.column });
var advance = (state) => ((ch) => ((newIndex) => ((newState) => [ch, newState])((($match_0) => {
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
})(advance(state));
var skip2 = (state) => skip(skip(state));
var sliceFrom = (startOffset) => (state) => slice(startOffset)(state.index)(state.source);
var isIdentifierStart = (c) => _PIPE_PIPE3(isAlpha(c))(() => $dict_Eq_Char._EQ_EQ(c)("_"));
var isIdentifierPart = (c) => _PIPE_PIPE3(isIdentifierStart(c))(() => isDigit(c));
var isWhitespace = (c) => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(" "))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("\t"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(`
`))(() => $dict_Eq_Char._EQ_EQ(c)("\r"))));
var isOperatorChar = (c) => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("!"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("#"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("$"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("%"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("&"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("*"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("+"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("."))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("/"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("<"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("="))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(">"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("?"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("@"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("\\"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("^"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("|"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("~"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(":"))(() => $dict_Eq_Char._EQ_EQ(c)("-"))))))))))))))))))));
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
    if (isAtEnd(state)) {
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
      if (isAtEnd(state)) {
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
skipWhitespaceAndComments = (sawNl) => (state) => isAtEnd(state) ? Ok({ state, sawNewline: sawNl }) : (($match_9) => {
  if ($match_9.$tag === 1) {
    return Ok({ state, sawNewline: sawNl });
  }
  if ($match_9.$tag === 0) {
    const c = $match_9.$0;
    return skipWsDispatch(sawNl)(state)(c);
  }
  throw new Error("Pattern match failed");
})(peek(state));
skipWsDispatch = (sawNl) => (state) => (c) => isWhitespace(c) ? ((nl) => skipWhitespaceAndComments(nl)(skip(state)))(_PIPE_PIPE3(sawNl)(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(`
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
            return ((nl) => skipWhitespaceAndComments(nl)(s3))(_PIPE_PIPE3(sawNl)(() => $dict_Ord_Int._GT(s3.line)(lineBefore)));
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
        if (_PIPE_PIPE3(isIdentifierPart(c))(() => $dict_Eq_Char._EQ_EQ(c)("'"))) {
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
})(advance(state)))(state.index);
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
var isValidStringEscape = (esc) => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("n"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("r"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("t"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)('"'))(() => $dict_Eq_Char._EQ_EQ(esc)("\\")))));
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
var isValidCharEscape = (esc) => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("n"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("r"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("t"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("'"))(() => $dict_Eq_Char._EQ_EQ(esc)("\\")))));
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
var maybeInsertNewline = (tokens) => (sawNl) => (hasEmitted) => (state) => _AMP_AMP2(sawNl)(() => hasEmitted) ? ((nlPos) => ((nlToken) => _COLON_COLON(nlToken)(tokens))({ kind: Newline, lexeme: `
`, span: { start: nlPos, end: nlPos } }))(position(state)) : tokens;
var lexLoop = (loop) => {
  while (true) {
    if (isAtEnd(loop.state)) {
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
          if (isAtEnd(skipResult.state)) {
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
var reverse = (lst) => reverseHelper(lst)([]);
var lex = (source) => ((initial) => (($match_56) => {
  if ($match_56.$tag === 1) {
    const e = $match_56.$0;
    return Err(e);
  }
  if ($match_56.$tag === 0) {
    const loop = $match_56.$0;
    return ((endPos) => ((eofToken) => Ok(reverse(_COLON_COLON(eofToken)(loop.tokens))))({ kind: Eof, lexeme: "", span: { start: endPos, end: endPos } }))(position(loop.state));
  }
  throw new Error("Pattern match failed");
})(lexLoop(initial)))({ tokens: [], state: initState(source), sawNewline: false, hasEmittedToken: false });
var lexToJson2 = (source) => lexToJson(lex)(source);

// ../lexer/src/index.ts
class LexError extends Error {
  span;
  constructor(message, span) {
    super(message);
    this.span = span;
  }
}
function lex2(source) {
  const json = lexToJson2(source);
  const result = JSON.parse(json);
  if (!result.ok) {
    throw new LexError(result.message, result.span);
  }
  return result.tokens.map((t) => ({ kind: t.kind, lexeme: t.lexeme, span: t.span }));
}

// ../vibe-parser/dist/Vibe/Vibe/Basics.js
var _PIPE_GT2 = (x) => (f) => f(x);
var not2 = (b) => b ? false : true;
var identity2 = (x) => x;
var flip2 = (f) => (x) => (y) => f(y)(x);

// ../vibe-parser/dist/Vibe/Vibe/Int.ffi.js
var intAdd2 = (a, b) => a + b | 0;
var intSub2 = (a, b) => a - b | 0;
var intMul2 = (a, b) => a * b | 0;
var intDiv2 = (a, b) => Math.trunc(a / b);
var intMod2 = (a, b) => a % b;
var numEq3 = (a, b) => a === b;
var numLt3 = (a, b) => a < b;
var numGt3 = (a, b) => a > b;
var numToString3 = (n) => n.toString();

// ../vibe-parser/dist/Vibe/Vibe/Int.js
var _PIPE_PIPE4 = (a) => (b) => a || b();
var _add2 = ($a0) => ($a1) => intAdd2($a0, $a1);
var _sub2 = ($a0) => ($a1) => intSub2($a0, $a1);
var _mul2 = ($a0) => ($a1) => intMul2($a0, $a1);
var _div2 = ($a0) => ($a1) => intDiv2($a0, $a1);
var _mod2 = ($a0) => ($a1) => intMod2($a0, $a1);
var _eq3 = ($a0) => ($a1) => numEq3($a0, $a1);
var _lt3 = ($a0) => ($a1) => numLt3($a0, $a1);
var _gt3 = ($a0) => ($a1) => numGt3($a0, $a1);
var _toString4 = ($a0) => numToString3($a0);
var _negate2 = (x) => -x;
var $default_Ord_Int__LT_EQ2 = (x) => (y) => _PIPE_PIPE4(_lt3(x)(y))(() => $dict_Eq_Int2._EQ_EQ(x)(y));
var $default_Ord_Int__GT_EQ2 = (x) => (y) => _PIPE_PIPE4(_gt3(x)(y))(() => $dict_Eq_Int2._EQ_EQ(x)(y));
var $dict_Ord_Int2 = {
  _LT: _lt3,
  _GT: _gt3,
  _LT_EQ: $default_Ord_Int__LT_EQ2,
  _GT_EQ: $default_Ord_Int__GT_EQ2
};
var min = (a) => (b) => $dict_Ord_Int2._LT(a)(b) ? a : b;
var $default_Eq_Int__SLASH_EQ2 = (x) => (y) => not2(_eq3(x)(y));
var $dict_Eq_Int2 = {
  _EQ_EQ: _eq3,
  _SLASH_EQ: $default_Eq_Int__SLASH_EQ2
};
var $dict_Num_Int2 = {
  _PLUS: _add2,
  _MINUS: _sub2,
  _STAR: _mul2,
  negate: _negate2
};
var $dict_Integral_Int2 = {
  _SLASH_SLASH: _div2,
  _PERCENT: _mod2
};
var $dict_Show_Int2 = {
  toString: _toString4
};

// ../vibe-parser/dist/Vibe/Vibe/Float.ffi.js
var numToString4 = (n) => n.toString();

// ../vibe-parser/dist/Vibe/Vibe/Float.js
var _toString5 = ($a0) => numToString4($a0);
var $dict_Show_Float2 = {
  toString: _toString5
};

// ../vibe-parser/dist/Vibe/Vibe/String.ffi.js
var stringAppend2 = (a, b) => a + b;
var stringEq2 = (a, b) => a === b;
var parseInt3 = (just, nothing, s) => {
  const n = Number.parseInt(s, 10);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};
var parseFloat2 = (just, nothing, s) => {
  const n = Number.parseFloat(s);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};
var stringCharAt2 = (just, nothing, i, s) => {
  if (i >= 0 && i < s.length) {
    return just(s[i]);
  }
  return nothing;
};
var unsafeCharAt3 = (i, s) => s[i];
var stringToList2 = (s) => [...s];

// ../vibe-parser/dist/Vibe/Vibe/Result.js
var Ok2 = ($0) => ({ $tag: 0, $0 });
var Err2 = ($0) => ({ $tag: 1, $0 });

// ../vibe-parser/dist/Vibe/Vibe/Maybe.js
var Just2 = ($0) => ({ $tag: 0, $0 });
var Nothing2 = { $tag: 1 };

// ../vibe-parser/dist/Vibe/Vibe/String.js
var _append2 = ($a0) => ($a1) => stringAppend2($a0, $a1);
var _eq4 = ($a0) => ($a1) => stringEq2($a0, $a1);
var _parseInt2 = ($a0) => ($a1) => ($a2) => parseInt3($a0, $a1, $a2);
var _parseFloat2 = ($a0) => ($a1) => ($a2) => parseFloat2($a0, $a1, $a2);
var length2 = ($recv) => $recv.length;
var _charAt2 = ($a0) => ($a1) => ($a2) => ($a3) => stringCharAt2($a0, $a1, $a2, $a3);
var unsafeCharAt4 = ($a0) => ($a1) => unsafeCharAt3($a0, $a1);
var _slice2 = ($recv) => ($a0) => ($a1) => $recv.slice($a0, $a1);
var slice2 = (start) => (end) => (s) => _slice2(s)(start)(end);
var _startsWith = ($recv) => ($a0) => $recv.startsWith($a0);
var startsWith = (prefix) => (s) => _startsWith(s)(prefix);
var _endsWith = ($recv) => ($a0) => $recv.endsWith($a0);
var endsWith = (suffix) => (s) => _endsWith(s)(suffix);
var _includes = ($recv) => ($a0) => $recv.includes($a0);
var contains = (sub) => (s) => _includes(s)(sub);
var toList2 = ($a0) => stringToList2($a0);
var _joinStrings = ($recv) => ($a0) => $recv.join($a0);
var join2 = (sep) => (lst) => _joinStrings(lst)(sep);
var charAt2 = _charAt2(Just2)(Nothing2);
var toInt2 = _parseInt2(Just2)(Nothing2);
var toFloat2 = _parseFloat2(Just2)(Nothing2);
var isEmpty = (s) => $dict_Eq_Int2._EQ_EQ(length2(s))(0);
var $default_Eq_String__SLASH_EQ2 = (x) => (y) => not2(_eq4(x)(y));
var $dict_Eq_String2 = {
  _EQ_EQ: _eq4,
  _SLASH_EQ: $default_Eq_String__SLASH_EQ2
};
var $dict_Show_String2 = {
  toString: identity2
};
var $dict_Appendable_String2 = {
  _PLUS_PLUS: _append2
};

// ../vibe-parser/dist/Vibe/Vibe/Char.ffi.js
var charToString2 = (a) => a;
var charToCode2 = (c) => c.codePointAt(0);
var charOrd2 = (a, b) => a < b;
var charOrdGt2 = (a, b) => a > b;

// ../vibe-parser/dist/Vibe/Vibe/Char.js
var _PIPE_PIPE5 = (a) => (b) => a || b();
var _AMP_AMP3 = (a) => (b) => a && b();
var _toString6 = ($a0) => charToString2($a0);
var _lt4 = ($a0) => ($a1) => charOrd2($a0, $a1);
var _gt4 = ($a0) => ($a1) => charOrdGt2($a0, $a1);
var toCode2 = ($a0) => charToCode2($a0);
var $default_Ord_Char__LT_EQ2 = (x) => (y) => _PIPE_PIPE5(_lt4(x)(y))(() => $dict_Eq_Char2._EQ_EQ(x)(y));
var $default_Ord_Char__GT_EQ2 = (x) => (y) => _PIPE_PIPE5(_gt4(x)(y))(() => $dict_Eq_Char2._EQ_EQ(x)(y));
var $dict_Ord_Char2 = {
  _LT: _lt4,
  _GT: _gt4,
  _LT_EQ: $default_Ord_Char__LT_EQ2,
  _GT_EQ: $default_Ord_Char__GT_EQ2
};
var isUpper2 = (c) => _AMP_AMP3($dict_Ord_Char2._GT_EQ(c)("A"))(() => $dict_Ord_Char2._LT_EQ(c)("Z"));
var isLower2 = (c) => _AMP_AMP3($dict_Ord_Char2._GT_EQ(c)("a"))(() => $dict_Ord_Char2._LT_EQ(c)("z"));
var isAlpha2 = (c) => _PIPE_PIPE5(isUpper2(c))(() => isLower2(c));
var isDigit2 = (c) => _AMP_AMP3($dict_Ord_Char2._GT_EQ(c)("0"))(() => $dict_Ord_Char2._LT_EQ(c)("9"));
var $impl_Eq_Char__EQ_EQ2 = (x) => (y) => $dict_Eq_String2._EQ_EQ($dict_Show_Char2.toString(x))($dict_Show_Char2.toString(y));
var $default_Eq_Char__SLASH_EQ2 = (x) => (y) => not2($dict_Eq_Char2._EQ_EQ(x)(y));
var $dict_Eq_Char2 = {
  _EQ_EQ: $impl_Eq_Char__EQ_EQ2,
  _SLASH_EQ: $default_Eq_Char__SLASH_EQ2
};
var $dict_Show_Char2 = {
  toString: _toString6
};

// ../vibe-parser/dist/Vibe/Vibe/List.ffi.js
var listReverse2 = (lst) => [...lst].reverse();
var listNth2 = (just, nothing, n, lst) => {
  if (n >= 0 && n < lst.length) {
    return just(lst[n]);
  }
  return nothing;
};

// ../vibe-parser/dist/Vibe/Vibe/List.js
var _AMP_AMP4 = (a) => (b) => a && b();
var _concat2 = ($recv) => ($a0) => $recv.concat($a0);
var _COLON_COLON2 = (x) => (xs) => _concat2([x])(xs);
var _map2 = ($recv) => ($a0) => $recv.map($a0);
var map2 = flip2(_map2);
var _filter2 = ($recv) => ($a0) => $recv.filter($a0);
var filter2 = flip2(_filter2);
var head = (xs) => (($match_0) => {
  if (Array.isArray($match_0) && $match_0.length >= 1) {
    const x = $match_0[0];
    return Just2(x);
  }
  if (Array.isArray($match_0) && $match_0.length === 0) {
    return Nothing2;
  }
  throw new Error("Pattern match failed");
})(xs);
var last = (xs) => {
  while (true) {
    {
      const $match_2 = xs;
      if (Array.isArray($match_2) && $match_2.length === 0) {
        return Nothing2;
      }
      if (Array.isArray($match_2) && $match_2.length === 1) {
        const x = $match_2[0];
        return Just2(x);
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
var reverse2 = ($a0) => listReverse2($a0);
var _nth2 = ($a0) => ($a1) => ($a2) => ($a3) => listNth2($a0, $a1, $a2, $a3);
var nth2 = _nth2(Just2)(Nothing2);
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
    return _AMP_AMP4($dict_Eq._EQ_EQ(x)(y))(() => $dict_Eq._EQ_EQ(xtail)(ytail));
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})([xs, ys]);
var $default_Eq_List_v356__SLASH_EQ = ($dict_Eq) => (x) => (y) => not2($dict_Eq._EQ_EQ(x)(y));
var $impl_Show_List_v357_toString = ($dict_Show) => (lst) => ((elementStrings) => $dict_Appendable_String2._PLUS_PLUS("[")($dict_Appendable_String2._PLUS_PLUS(join2(", ")(elementStrings))("]")))(map2($dict_Show.toString)(lst));
var $dict_Eq_List_v3562 = ($dict_Eq) => ({
  _EQ_EQ: $impl_Eq_List_v356__EQ_EQ($dict_Eq),
  _SLASH_EQ: $default_Eq_List_v356__SLASH_EQ($dict_Eq)
});
var $dict_Show_List_v3572 = ($dict_Show) => ({
  toString: $impl_Show_List_v357_toString($dict_Show)
});
// ../vibe-parser/dist/Vibe/Vibe/Dict.ffi.js
var dictEmpty = new Map;
var dictInsert = (k, v, d) => {
  const m = new Map(d);
  m.set(k, v);
  return m;
};
var dictToList = (toPair, d) => {
  const result = [];
  d.forEach((v, k) => {
    result.push(toPair(k)(v));
  });
  return result;
};
var dictFromList = (getKey, getValue, lst) => {
  const m = new Map;
  for (const pair of lst) {
    m.set(getKey(pair), getValue(pair));
  }
  return m;
};

// ../vibe-parser/dist/Vibe/Vibe/Dict.js
var insert = ($a0) => ($a1) => ($a2) => dictInsert($a0, $a1, $a2);
var _has = ($recv) => ($a0) => $recv.has($a0);
var _unsafeGet = ($recv) => ($a0) => $recv.get($a0);
var get = (k) => (d) => _has(d)(k) ? Just2(_unsafeGet(d)(k)) : Nothing2;
var _toList = ($a0) => ($a1) => dictToList($a0, $a1);
var toList3 = _toList((k) => (v) => [k, v]);
var _fromList = ($a0) => ($a1) => ($a2) => dictFromList($a0, $a1, $a2);
var fromList = _fromList((pair) => (($match_0) => {
  {
    const k = $match_0[0];
    return k;
  }
  throw new Error("Pattern match failed");
})(pair))((pair) => (($match_1) => {
  {
    const v = $match_1[1];
    return v;
  }
  throw new Error("Pattern match failed");
})(pair));

// ../vibe-parser/dist/VibeLexer/VibeLexer/Types.js
var LowerIdentifier2 = { $tag: 0 };
var UpperIdentifier2 = { $tag: 1 };
var Keyword2 = { $tag: 2 };
var NumberToken2 = { $tag: 3 };
var StringToken2 = { $tag: 4 };
var CharToken2 = { $tag: 5 };
var Operator2 = { $tag: 6 };
var Range2 = { $tag: 7 };
var Backslash2 = { $tag: 8 };
var LParen2 = { $tag: 9 };
var RParen2 = { $tag: 10 };
var LBrace2 = { $tag: 11 };
var RBrace2 = { $tag: 12 };
var LBracket2 = { $tag: 13 };
var RBracket2 = { $tag: 14 };
var Comma2 = { $tag: 15 };
var Dot2 = { $tag: 16 };
var Colon2 = { $tag: 17 };
var Equals2 = { $tag: 18 };
var Pipe2 = { $tag: 19 };
var Newline2 = { $tag: 20 };
var BlockStart2 = { $tag: 21 };
var BlockSep2 = { $tag: 22 };
var BlockEnd2 = { $tag: 23 };
var Eof2 = { $tag: 24 };
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
var defaultOperatorInfo = { precedence: 9, associativity: AssocLeft };
var builtinRegistry = _PIPE_GT2(_PIPE_GT2(dictEmpty)(insert("&&")({ precedence: 3, associativity: AssocRight })))(insert("||")({ precedence: 2, associativity: AssocRight }));
var getOperatorInfo = (op) => (registry) => (($match_0) => {
  if ($match_0.$tag === 0) {
    const info = $match_0.$0;
    return info;
  }
  if ($match_0.$tag === 1) {
    return defaultOperatorInfo;
  }
  throw new Error("Pattern match failed");
})(get(op)(registry));
var insertOperator = insert;

// ../vibe-parser/dist/VibeParser/VibeParser/Parser.js
var _AMP_AMP5 = (a) => (b) => a && b();
var _PIPE_PIPE6 = (a) => (b) => a || b();
var PIAnnotation = ($0) => ($1) => ($2) => ({ $tag: 0, $0, $1, $2 });
var PIImpl = ($0) => ($1) => ($2) => ($3) => ({ $tag: 1, $0, $1, $2, $3 });
var initState2 = (tokens) => (registry) => ({ tokens, index: 0, registry });
var makeError = (msg) => (sp) => ({ message: msg, span: sp });
var eofToken = { kind: Eof2, lexeme: "", span: { start: { offset: 0, line: 0, column: 0 }, end: { offset: 0, line: 0, column: 0 } } };
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
})(nth2(n)(tokens));
var current = (state) => nthToken(state.index)(state.tokens);
var previous = (state) => nthToken($dict_Num_Int2._MINUS(state.index)(1))(state.tokens);
var advance2 = (state) => ({ ...state, index: $dict_Num_Int2._PLUS(state.index)(1) });
var peekAhead = (offset) => (state) => nth2($dict_Num_Int2._PLUS(state.index)(offset))(state.tokens);
var isAtEnd2 = (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(Eof2);
var peekKind = (kind) => (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(kind);
var peekKeyword = (kw) => (state) => ((tok) => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(kw)))(current(state));
var peekOperator = (op) => (state) => ((tok) => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(op)))(current(state));
var matchKind = (kind) => (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(kind) ? Just2(advance2(state)) : Nothing2;
var matchOperator = (op) => (state) => ((tok) => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(op)) ? Just2(advance2(state)) : Nothing2)(current(state));
var matchBlockSep = (state) => matchKind(BlockSep2)(state);
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
var expect = (kind) => (label) => (state) => ((tok) => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(kind) ? Ok2([tok, advance2(state)]) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected ")(label))(" but found "))(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
var expectKeyword = (kw) => (state) => ((tok) => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(kw)) ? Ok2([tok, advance2(state)]) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected keyword '")(kw))("' but found "))(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
var expectOperator = (op) => (state) => ((tok) => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(op)) ? Ok2([tok, advance2(state)]) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected operator '")(op))("' but found "))(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
var expectBlockStart = (state) => expect(BlockStart2)("indented block")(state);
var expectBlockEnd = (state) => expect(BlockEnd2)("end of indented block")(state);
var onSameLine = (span) => (tok) => $dict_Eq_Int2._EQ_EQ(span.end.line)(tok.span.start.line);
var continuesLayout = (baseIndent) => (lastEnd) => (next) => $dict_Eq_Int2._EQ_EQ(next.span.start.line)(lastEnd.line) ? true : $dict_Ord_Int2._GT_EQ(next.span.start.column)(baseIndent);
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
      const $match_13 = nth2(j)(tokens);
      if ($match_13.$tag === 1) {
        return Nothing2;
      }
      if ($match_13.$tag === 0) {
        const tok = $match_13.$0;
        if (isBlockToken(tok.kind)) {
          [j, remaining, tokens] = [$dict_Num_Int2._PLUS(j)(1), remaining, tokens];
          continue;
        } else {
          if ($dict_Eq_Int2._EQ_EQ(remaining)(0)) {
            return Just2(tok);
          } else {
            [j, remaining, tokens] = [$dict_Num_Int2._PLUS(j)(1), $dict_Num_Int2._MINUS(remaining)(1), tokens];
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
    if ($dict_Ord_Int2._GT(limit)(20)) {
      return false;
    } else {
      {
        const $match_14 = peekAheadSkipLayout(i)(state);
        if ($match_14.$tag === 1) {
          return false;
        }
        if ($match_14.$tag === 0) {
          const tok = $match_14.$0;
          if (_AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("=>"))) {
            return true;
          } else {
            if (_AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("where"))) {
              return false;
            } else {
              [i, limit, state] = [$dict_Num_Int2._PLUS(i)(1), $dict_Num_Int2._PLUS(limit)(1), state];
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
    if ($dict_Ord_Int2._GT(limit)(30)) {
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
                return $dict_Num_Int2._PLUS(parenDepth)(1);
              }
              if ($match_16.$tag === 10) {
                return $dict_Num_Int2._MINUS(parenDepth)(1);
              }
              {
                return parenDepth;
              }
              throw new Error("Pattern match failed");
            })(tok.kind);
            if (_AMP_AMP5($dict_Eq_Int2._EQ_EQ(newDepth)(0))(() => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("=>")))) {
              return true;
            } else {
              if (_AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("where"))) {
                return false;
              } else {
                [i, parenDepth, limit, state] = [$dict_Num_Int2._PLUS(i)(1), newDepth, $dict_Num_Int2._PLUS(limit)(1), state];
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
    if ($dict_Ord_Int2._GT(limit)(30)) {
      return false;
    } else {
      {
        const $match_17 = peekAheadSkipLayout(i)(state);
        if ($match_17.$tag === 1) {
          return false;
        }
        if ($match_17.$tag === 0) {
          const tok = $match_17.$0;
          if ($dict_Eq_Int2._SLASH_EQ(tok.span.start.line)(startLine)) {
            return false;
          } else {
            {
              const newDepth = (($match_18) => {
                if ($match_18.$tag === 9) {
                  return $dict_Num_Int2._PLUS(parenDepth)(1);
                }
                if ($match_18.$tag === 10) {
                  return $dict_Num_Int2._MINUS(parenDepth)(1);
                }
                {
                  return parenDepth;
                }
                throw new Error("Pattern match failed");
              })(tok.kind);
              if (_AMP_AMP5($dict_Eq_Int2._EQ_EQ(newDepth)(0))(() => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("=>")))) {
                return true;
              } else {
                if (_AMP_AMP5($dict_Eq_Int2._EQ_EQ(newDepth)(0))(() => _PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Equals2))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2)))) {
                  return false;
                } else {
                  [i, parenDepth, limit, startLine, state] = [$dict_Num_Int2._PLUS(i)(1), newDepth, $dict_Num_Int2._PLUS(limit)(1), startLine, state];
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
    if ($dict_Ord_Int2._GT(limit)(50)) {
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
                return $dict_Num_Int2._PLUS(parenDepth)(1);
              }
              if ($match_20.$tag === 10) {
                return $dict_Num_Int2._MINUS(parenDepth)(1);
              }
              {
                return parenDepth;
              }
              throw new Error("Pattern match failed");
            })(tok.kind);
            if (_AMP_AMP5($dict_Eq_Int2._EQ_EQ(newDepth)(0))(() => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("=>")))) {
              return true;
            } else {
              if (_AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Equals2))(() => $dict_Eq_Int2._EQ_EQ(newDepth)(0))) {
                return false;
              } else {
                if (_AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_Int2._EQ_EQ(newDepth)(0))) {
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
                      [i, parenDepth, limit, state] = [$dict_Num_Int2._PLUS(i)(1), newDepth, $dict_Num_Int2._PLUS(limit)(1), state];
                      continue;
                    }
                    throw new Error("Pattern match failed");
                  }
                } else {
                  [i, parenDepth, limit, state] = [$dict_Num_Int2._PLUS(i)(1), newDepth, $dict_Num_Int2._PLUS(limit)(1), state];
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
var peekDecorator = (state) => ((cur) => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(cur.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(cur.lexeme)("@")) ? (($match_22) => {
  if ($match_22.$tag === 1) {
    return false;
  }
  if ($match_22.$tag === 0) {
    const next = $match_22.$0;
    return _PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(next.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(next.kind)(Keyword2));
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : false)(current(state));
var intToStr = (n) => $dict_Show_Int2.toString(n);
var stringToInt = (s) => (($match_23) => {
  if ($match_23.$tag === 0) {
    const n = $match_23.$0;
    return n;
  }
  if ($match_23.$tag === 1) {
    return 0;
  }
  throw new Error("Pattern match failed");
})(toInt2(s));
var parseModuleNameParts = (name) => (end) => (state) => {
  while (true) {
    if (peekKind(Dot2)(state)) {
      {
        const s1 = advance2(state);
        {
          const $match_24 = expect(UpperIdentifier2)("module name segment")(s1);
          if ($match_24.$tag === 1) {
            const e = $match_24.$0;
            return Err2(e);
          }
          if ($match_24.$tag === 0) {
            const next = $match_24.$0[0];
            const s2 = $match_24.$0[1];
            [name, end, state] = [$dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(name)("."))(next.lexeme), next.span.end, s2];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
    } else {
      return Ok2([{ name, nameEnd: end }, state]);
    }
  }
};
var parseModuleName = (state) => (($match_25) => {
  if ($match_25.$tag === 1) {
    const e = $match_25.$0;
    return Err2(e);
  }
  if ($match_25.$tag === 0) {
    const first = $match_25.$0[0];
    const s1 = $match_25.$0[1];
    return parseModuleNameParts(first.lexeme)(first.span.end)(s1);
  }
  throw new Error("Pattern match failed");
})(expect(UpperIdentifier2)("module name")(state));
var parseExportMember = (state) => ((tok) => (($match_26) => {
  if ($match_26.$tag === 9) {
    return ((s1) => (($match_27) => {
      if ($match_27.$tag === 1) {
        const e = $match_27.$0;
        return Err2(e);
      }
      if ($match_27.$tag === 0) {
        const opTok = $match_27.$0[0];
        const s2 = $match_27.$0[1];
        return (($match_28) => {
          if ($match_28.$tag === 1) {
            const e = $match_28.$0;
            return Err2(e);
          }
          if ($match_28.$tag === 0) {
            const s3 = $match_28.$0[1];
            return Ok2([opTok.lexeme, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("closing paren for operator member")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator2)("operator in member list")(s1)))(advance2(state));
  }
  if ($match_26.$tag === 1) {
    return ((s1) => Ok2([tok.lexeme, s1]))(advance2(state));
  }
  if ($match_26.$tag === 0) {
    return ((s1) => Ok2([tok.lexeme, s1]))(advance2(state));
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Expected member name in export list, got ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var parseExportMembers = (state) => (($match_29) => {
  if ($match_29.$tag === 1) {
    const e = $match_29.$0;
    return Err2(e);
  }
  if ($match_29.$tag === 0) {
    const name = $match_29.$0[0];
    const s1 = $match_29.$0[1];
    return peekKind(Comma2)(s1) ? ((s2) => (($match_30) => {
      if ($match_30.$tag === 1) {
        const e = $match_30.$0;
        return Err2(e);
      }
      if ($match_30.$tag === 0) {
        const rest = $match_30.$0[0];
        const s3 = $match_30.$0[1];
        return Ok2([_COLON_COLON2(name)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseExportMembers(s2)))(advance2(s1)) : Ok2([[name], s1]);
  }
  throw new Error("Pattern match failed");
})(parseExportMember(state));
var parseExportSpec = (state) => ((tok) => (($match_31) => {
  if ($match_31.$tag === 9) {
    return ((s1) => (($match_32) => {
      if ($match_32.$tag === 1) {
        const e = $match_32.$0;
        return Err2(e);
      }
      if ($match_32.$tag === 0) {
        const opTok = $match_32.$0[0];
        const s2 = $match_32.$0[1];
        return (($match_33) => {
          if ($match_33.$tag === 1) {
            const e = $match_33.$0;
            return Err2(e);
          }
          if ($match_33.$tag === 0) {
            const cp = $match_33.$0[0];
            const s3 = $match_33.$0[1];
            return Ok2([ExportOperator(opTok.lexeme)({ start: tok.span.start, end: cp.span.end }), s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("closing paren for operator export")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator2)("operator in export")(s1)))(advance2(state));
  }
  if ($match_31.$tag === 0) {
    return ((s1) => Ok2([ExportValue(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_31.$tag === 1) {
    return ((nameTok) => ((s1) => peekKind(LParen2)(s1) ? ((s2) => peekKind(Range2)(s2) ? ((s3) => (($match_34) => {
      if ($match_34.$tag === 1) {
        const e = $match_34.$0;
        return Err2(e);
      }
      if ($match_34.$tag === 0) {
        const cp = $match_34.$0[0];
        const s4 = $match_34.$0[1];
        return Ok2([ExportTypeAll(nameTok.lexeme)({ start: nameTok.span.start, end: cp.span.end }), s4]);
      }
      throw new Error("Pattern match failed");
    })(expect(RParen2)("closing paren for type export")(s3)))(advance2(s2)) : (($match_35) => {
      if ($match_35.$tag === 1) {
        const e = $match_35.$0;
        return Err2(e);
      }
      if ($match_35.$tag === 0) {
        const members = $match_35.$0[0];
        const s3 = $match_35.$0[1];
        return (($match_36) => {
          if ($match_36.$tag === 1) {
            const e = $match_36.$0;
            return Err2(e);
          }
          if ($match_36.$tag === 0) {
            const cp = $match_36.$0[0];
            const s4 = $match_36.$0[1];
            return Ok2([ExportTypeSome(nameTok.lexeme)(members)({ start: nameTok.span.start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("closing paren for type export")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseExportMembers(s2)))(advance2(s1)) : Ok2([ExportValue(nameTok.lexeme)(nameTok.span), s1]))(advance2(state)))(tok);
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Expected export specification, got ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var parseExportSpecList = (state) => (($match_37) => {
  if ($match_37.$tag === 1) {
    const e = $match_37.$0;
    return Err2(e);
  }
  if ($match_37.$tag === 0) {
    const spec = $match_37.$0[0];
    const s1 = $match_37.$0[1];
    return peekKind(Comma2)(s1) ? ((s2) => (($match_38) => {
      if ($match_38.$tag === 1) {
        const e = $match_38.$0;
        return Err2(e);
      }
      if ($match_38.$tag === 0) {
        const rest = $match_38.$0[0];
        const s3 = $match_38.$0[1];
        return Ok2([_COLON_COLON2(spec)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseExportSpecList(s2)))(advance2(s1)) : Ok2([[spec], s1]);
  }
  throw new Error("Pattern match failed");
})(parseExportSpec(state));
var parseExposing = (state) => (($match_39) => {
  if ($match_39.$tag === 1) {
    const e = $match_39.$0;
    return Err2(e);
  }
  if ($match_39.$tag === 0) {
    const openParen = $match_39.$0[0];
    const s1 = $match_39.$0[1];
    return ((start) => peekKind(Range2)(s1) ? ((s2) => (($match_40) => {
      if ($match_40.$tag === 1) {
        const e = $match_40.$0;
        return Err2(e);
      }
      if ($match_40.$tag === 0) {
        const closeParen = $match_40.$0[0];
        const s3 = $match_40.$0[1];
        return Ok2([ExposeAll({ start, end: closeParen.span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RParen2)("close exposing (..)")(s2)))(advance2(s1)) : (($match_41) => {
      if ($match_41.$tag === 1) {
        const e = $match_41.$0;
        return Err2(e);
      }
      if ($match_41.$tag === 0) {
        const specs = $match_41.$0[0];
        const s2 = $match_41.$0[1];
        return (($match_42) => {
          if ($match_42.$tag === 1) {
            const e = $match_42.$0;
            return Err2(e);
          }
          if ($match_42.$tag === 0) {
            const closeParen = $match_42.$0[0];
            const s3 = $match_42.$0[1];
            return Ok2([ExposeExplicit(specs)({ start, end: closeParen.span.end }), s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close exposing list")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseExportSpecList(s1)))(openParen.span.start);
  }
  throw new Error("Pattern match failed");
})(expect(LParen2)("exposing list start")(state));
var parseModuleDeclaration = (state) => (($match_43) => {
  if ($match_43.$tag === 1) {
    const e = $match_43.$0;
    return Err2(e);
  }
  if ($match_43.$tag === 0) {
    const modTok = $match_43.$0[0];
    const s1 = $match_43.$0[1];
    return (($match_44) => {
      if ($match_44.$tag === 1) {
        const e = $match_44.$0;
        return Err2(e);
      }
      if ($match_44.$tag === 0) {
        const nameInfo = $match_44.$0[0];
        const s2 = $match_44.$0[1];
        return peekKeyword("exposing")(s2) ? (($match_45) => {
          if ($match_45.$tag === 1) {
            const e = $match_45.$0;
            return Err2(e);
          }
          if ($match_45.$tag === 0) {
            const s3 = $match_45.$0[1];
            return (($match_46) => {
              if ($match_46.$tag === 1) {
                const e = $match_46.$0;
                return Err2(e);
              }
              if ($match_46.$tag === 0) {
                const exp = $match_46.$0[0];
                const s4 = $match_46.$0[1];
                return Ok2([{ name: nameInfo.name, exposingClause: exp, hasExposing: true, span: { start: modTok.span.start, end: exposingSpan(exp).end } }, s4]);
              }
              throw new Error("Pattern match failed");
            })(parseExposing(s3));
          }
          throw new Error("Pattern match failed");
        })(expectKeyword("exposing")(s2)) : Ok2([{ name: nameInfo.name, exposingClause: ExposeAll({ start: nameInfo.nameEnd, end: nameInfo.nameEnd }), hasExposing: false, span: { start: modTok.span.start, end: nameInfo.nameEnd } }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseModuleName(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("module")(state));
var parseImportAlias = (state) => peekKeyword("as")(state) ? (($match_47) => {
  if ($match_47.$tag === 1) {
    const e = $match_47.$0;
    return Err2(e);
  }
  if ($match_47.$tag === 0) {
    const s1 = $match_47.$0[1];
    return (($match_48) => {
      if ($match_48.$tag === 1) {
        const e = $match_48.$0;
        return Err2(e);
      }
      if ($match_48.$tag === 0) {
        const aliasTok = $match_48.$0[0];
        const s2 = $match_48.$0[1];
        return Ok2([{ aliasName: aliasTok.lexeme, hasAlias: true }, s2]);
      }
      throw new Error("Pattern match failed");
    })(expect(UpperIdentifier2)("import alias")(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("as")(state)) : Ok2([{ aliasName: "", hasAlias: false }, state]);
var parseImportExposing = (state) => ((dummySpan) => peekKeyword("exposing")(state) ? (($match_49) => {
  if ($match_49.$tag === 1) {
    const e = $match_49.$0;
    return Err2(e);
  }
  if ($match_49.$tag === 0) {
    const s1 = $match_49.$0[1];
    return (($match_50) => {
      if ($match_50.$tag === 1) {
        const e = $match_50.$0;
        return Err2(e);
      }
      if ($match_50.$tag === 0) {
        const exp = $match_50.$0[0];
        const s2 = $match_50.$0[1];
        return Ok2([{ expClause: exp, hasExp: true }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseExposing(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("exposing")(state)) : Ok2([{ expClause: ExposeAll(dummySpan), hasExp: false }, state]))({ start: { offset: 0, line: 0, column: 0 }, end: { offset: 0, line: 0, column: 0 } });
var parseImport = (state) => (($match_51) => {
  if ($match_51.$tag === 1) {
    const e = $match_51.$0;
    return Err2(e);
  }
  if ($match_51.$tag === 0) {
    const importTok = $match_51.$0[0];
    const s1 = $match_51.$0[1];
    return (($match_52) => {
      if ($match_52.$tag === 1) {
        const e = $match_52.$0;
        return Err2(e);
      }
      if ($match_52.$tag === 0) {
        const nameInfo = $match_52.$0[0];
        const s2 = $match_52.$0[1];
        return (($match_53) => {
          if ($match_53.$tag === 1) {
            const e = $match_53.$0;
            return Err2(e);
          }
          if ($match_53.$tag === 0) {
            const aliasResult = $match_53.$0[0];
            const s3 = $match_53.$0[1];
            return (($match_54) => {
              if ($match_54.$tag === 1) {
                const e = $match_54.$0;
                return Err2(e);
              }
              if ($match_54.$tag === 0) {
                const expResult = $match_54.$0[0];
                const s4 = $match_54.$0[1];
                return ((endPos) => Ok2([{ moduleName: nameInfo.name, aliasName: aliasResult.aliasName, exposingClause: expResult.expClause, hasAlias: aliasResult.hasAlias, hasExposing: expResult.hasExp, span: { start: importTok.span.start, end: endPos } }, s4]))(expResult.hasExp ? exposingSpan(expResult.expClause).end : aliasResult.hasAlias ? previous(s3).span.end : nameInfo.nameEnd);
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
var parseImports = (state) => peekKeyword("import")(state) ? ((tok) => $dict_Eq_Int2._EQ_EQ(tok.span.start.column)(1) ? (($match_55) => {
  if ($match_55.$tag === 1) {
    const e = $match_55.$0;
    return Err2(e);
  }
  if ($match_55.$tag === 0) {
    const imp = $match_55.$0[0];
    const s1 = $match_55.$0[1];
    return (($match_56) => {
      if ($match_56.$tag === 1) {
        const e = $match_56.$0;
        return Err2(e);
      }
      if ($match_56.$tag === 0) {
        const rest = $match_56.$0[0];
        const s2 = $match_56.$0[1];
        return Ok2([_COLON_COLON2(imp)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseImports(s1));
  }
  throw new Error("Pattern match failed");
})(parseImport(state)) : Err2(makeError("import declaration must start at column 1")(tok.span)))(current(state)) : Ok2([[], state]);
var unquote = (lexeme) => _AMP_AMP5($dict_Ord_Int2._GT_EQ(length2(lexeme))(2))(() => _AMP_AMP5(startsWith('"')(lexeme))(() => endsWith('"')(lexeme))) ? slice2(1)($dict_Num_Int2._MINUS(length2(lexeme))(1))(lexeme) : lexeme;
var parseDecoratorArgs = (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(StringToken2) ? ((tok) => ((s1) => ((unquoted) => (($match_57) => {
  if ($match_57.$tag === 1) {
    const e = $match_57.$0;
    return Err2(e);
  }
  if ($match_57.$tag === 0) {
    const rest = $match_57.$0[0];
    const s2 = $match_57.$0[1];
    return Ok2([_COLON_COLON2(unquoted)(rest), s2]);
  }
  throw new Error("Pattern match failed");
})(parseDecoratorArgs(s1)))(unquote(tok.lexeme)))(advance2(state)))(current(state)) : Ok2([[], state]);
var parseDeclarationName = (state) => ((tok) => (($match_58) => {
  if ($match_58.$tag === 0) {
    return ((s1) => Ok2([{ declName: tok.lexeme, declSpan: tok.span }, s1]))(advance2(state));
  }
  if ($match_58.$tag === 9) {
    return ((s1) => (($match_59) => {
      if ($match_59.$tag === 1) {
        const e = $match_59.$0;
        return Err2(e);
      }
      if ($match_59.$tag === 0) {
        const opTok = $match_59.$0[0];
        const s2 = $match_59.$0[1];
        return (($match_60) => {
          if ($match_60.$tag === 1) {
            const e = $match_60.$0;
            return Err2(e);
          }
          if ($match_60.$tag === 0) {
            const cp = $match_60.$0[0];
            const s3 = $match_60.$0[1];
            return Ok2([{ declName: opTok.lexeme, declSpan: { start: tok.span.start, end: cp.span.end } }, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close operator name")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator2)("operator name")(s1)))(advance2(state));
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected declaration name but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
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
    if (peekKind(Dot2)(state)) {
      {
        const $match_62 = peekAhead(1)(state);
        if ($match_62.$tag === 1) {
          return Ok2([{ name, nameEnd: end }, state]);
        }
        if ($match_62.$tag === 0) {
          const next = $match_62.$0;
          if ($dict_Eq_TokenKind._EQ_EQ(next.kind)(UpperIdentifier2)) {
            {
              const s1 = advance2(state);
              {
                const $match_63 = expect(UpperIdentifier2)("type name part")(s1);
                if ($match_63.$tag === 1) {
                  const e = $match_63.$0;
                  return Err2(e);
                }
                if ($match_63.$tag === 0) {
                  const nextTok = $match_63.$0[0];
                  const s2 = $match_63.$0[1];
                  [name, end, state] = [$dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(name)("."))(nextTok.lexeme), nextTok.span.end, s2];
                  continue;
                }
                throw new Error("Pattern match failed");
              }
            }
          } else {
            return Ok2([{ name, nameEnd: end }, state]);
          }
        }
        throw new Error("Pattern match failed");
      }
    } else {
      return Ok2([{ name, nameEnd: end }, state]);
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
parseConstraints = (state) => peekKind(LParen2)(state) ? ((s1) => (($match_64) => {
  if ($match_64.$tag === 1) {
    const e = $match_64.$0;
    return Err2(e);
  }
  if ($match_64.$tag === 0) {
    const constraints = $match_64.$0[0];
    const s2 = $match_64.$0[1];
    return (($match_65) => {
      if ($match_65.$tag === 1) {
        const e = $match_65.$0;
        return Err2(e);
      }
      if ($match_65.$tag === 0) {
        const s3 = $match_65.$0[1];
        return (($match_66) => {
          if ($match_66.$tag === 1) {
            const e = $match_66.$0;
            return Err2(e);
          }
          if ($match_66.$tag === 0) {
            const s4 = $match_66.$0[1];
            return Ok2([constraints, s4]);
          }
          throw new Error("Pattern match failed");
        })(expectOperator("=>")(s3));
      }
      throw new Error("Pattern match failed");
    })(expect(RParen2)("close constraint list")(s2));
  }
  throw new Error("Pattern match failed");
})(parseConstraintList(s1)))(advance2(state)) : (($match_67) => {
  if ($match_67.$tag === 1) {
    const e = $match_67.$0;
    return Err2(e);
  }
  if ($match_67.$tag === 0) {
    const c = $match_67.$0[0];
    const s1 = $match_67.$0[1];
    return (($match_68) => {
      if ($match_68.$tag === 1) {
        const e = $match_68.$0;
        return Err2(e);
      }
      if ($match_68.$tag === 0) {
        const s2 = $match_68.$0[1];
        return Ok2([[c], s2]);
      }
      throw new Error("Pattern match failed");
    })(expectOperator("=>")(s1));
  }
  throw new Error("Pattern match failed");
})(parseConstraint(state));
parseConstraintList = (state) => (($match_69) => {
  if ($match_69.$tag === 1) {
    const e = $match_69.$0;
    return Err2(e);
  }
  if ($match_69.$tag === 0) {
    const c = $match_69.$0[0];
    const s1 = $match_69.$0[1];
    return peekKind(Comma2)(s1) ? ((s2) => (($match_70) => {
      if ($match_70.$tag === 1) {
        const e = $match_70.$0;
        return Err2(e);
      }
      if ($match_70.$tag === 0) {
        const rest = $match_70.$0[0];
        const s3 = $match_70.$0[1];
        return Ok2([_COLON_COLON2(c)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseConstraintList(s2)))(advance2(s1)) : Ok2([[c], s1]);
  }
  throw new Error("Pattern match failed");
})(parseConstraint(state));
parseConstraint = (state) => ((start) => (($match_71) => {
  if ($match_71.$tag === 1) {
    const e = $match_71.$0;
    return Err2(e);
  }
  if ($match_71.$tag === 0) {
    const protocolTok = $match_71.$0[0];
    const s1 = $match_71.$0[1];
    return (($match_72) => {
      if ($match_72.$tag === 1) {
        const e = $match_72.$0;
        return Err2(e);
      }
      if ($match_72.$tag === 0) {
        const typeArgs = $match_72.$0[0];
        const s2 = $match_72.$0[1];
        return isEmpty2(typeArgs) ? Err2(makeError("Expected at least one type argument in constraint")(current(s2).span)) : ((endPos) => Ok2([{ protocolName: protocolTok.lexeme, typeArgs, span: { start, end: endPos } }, s2]))((($match_73) => {
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
})(expect(UpperIdentifier2)("protocol name in constraint")(state)))(current(state).span.start);
parseConstraintTypeArgs = (state) => isTypeStart(current(state)) ? (($match_74) => {
  if ($match_74.$tag === 1) {
    const e = $match_74.$0;
    return Err2(e);
  }
  if ($match_74.$tag === 0) {
    const t = $match_74.$0[0];
    const s1 = $match_74.$0[1];
    return (($match_75) => {
      if ($match_75.$tag === 1) {
        const e = $match_75.$0;
        return Err2(e);
      }
      if ($match_75.$tag === 0) {
        const rest = $match_75.$0[0];
        const s2 = $match_75.$0[1];
        return Ok2([_COLON_COLON2(t)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseConstraintTypeArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeTerm(state)) : Ok2([[], state]);
parseTypeExpression = (state) => ((start) => peekConstraintContextInType(state) ? (($match_76) => {
  if ($match_76.$tag === 1) {
    const e = $match_76.$0;
    return Err2(e);
  }
  if ($match_76.$tag === 0) {
    const constraints = $match_76.$0[0];
    const s1 = $match_76.$0[1];
    return (($match_77) => {
      if ($match_77.$tag === 1) {
        const e = $match_77.$0;
        return Err2(e);
      }
      if ($match_77.$tag === 0) {
        const typ = $match_77.$0[0];
        const s2 = $match_77.$0[1];
        return Ok2([TQualified(constraints)(typ)({ start, end: typeSpan(typ).end }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeArrow(s1));
  }
  throw new Error("Pattern match failed");
})(parseConstraints(state)) : parseTypeArrow(state))(current(state).span.start);
parseTypeArrow = (state) => (($match_78) => {
  if ($match_78.$tag === 1) {
    const e = $match_78.$0;
    return Err2(e);
  }
  if ($match_78.$tag === 0) {
    const left = $match_78.$0[0];
    const s1 = $match_78.$0[1];
    return (($match_79) => {
      if ($match_79.$tag === 1) {
        return Ok2([left, s1]);
      }
      if ($match_79.$tag === 0) {
        const s2 = $match_79.$0;
        return (($match_80) => {
          if ($match_80.$tag === 1) {
            const e = $match_80.$0;
            return Err2(e);
          }
          if ($match_80.$tag === 0) {
            const right = $match_80.$0[0];
            const s3 = $match_80.$0[1];
            return Ok2([TFunction(left)(right)({ start: typeSpan(left).start, end: typeSpan(right).end }), s3]);
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
parseRecordType = (state) => ((start) => ((s1) => peekKind(RBrace2)(s1) ? ((s2) => Ok2([TRecord([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_82) => {
  if ($match_82.$tag === 1) {
    const e = $match_82.$0;
    return Err2(e);
  }
  if ($match_82.$tag === 0) {
    const fields = $match_82.$0[0];
    const s2 = $match_82.$0[1];
    return (($match_83) => {
      if ($match_83.$tag === 1) {
        const e = $match_83.$0;
        return Err2(e);
      }
      if ($match_83.$tag === 0) {
        const s3 = $match_83.$0[1];
        return Ok2([TRecord(fields)({ start, end: previous(s3).span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RBrace2)("close record type")(s2));
  }
  throw new Error("Pattern match failed");
})(parseRecordTypeFieldList(s1)))(advance2(state)))(current(state).span.start);
parseRecordTypeFieldList = (state) => (($match_84) => {
  if ($match_84.$tag === 1) {
    const e = $match_84.$0;
    return Err2(e);
  }
  if ($match_84.$tag === 0) {
    const firstField = $match_84.$0[0];
    const s1 = $match_84.$0[1];
    return (($match_85) => {
      if ($match_85.$tag === 1) {
        const e = $match_85.$0;
        return Err2(e);
      }
      if ($match_85.$tag === 0) {
        const rest = $match_85.$0[0];
        const s2 = $match_85.$0[1];
        return Ok2([_COLON_COLON2(firstField)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreRecordTypeFields(firstField)(s1));
  }
  throw new Error("Pattern match failed");
})(parseRecordTypeField(state));
parseMoreRecordTypeFields = (lastField) => (state) => peekKind(Comma2)(state) ? (($match_86) => {
  if ($match_86.$tag === 1) {
    return Ok2([[], state]);
  }
  if ($match_86.$tag === 0) {
    const next = $match_86.$0;
    return continuesLayout(lastField.span.start.column)(lastField.span.end)(next) ? ((s1) => (($match_87) => {
      if ($match_87.$tag === 1) {
        const e = $match_87.$0;
        return Err2(e);
      }
      if ($match_87.$tag === 0) {
        const field = $match_87.$0[0];
        const s2 = $match_87.$0[1];
        return (($match_88) => {
          if ($match_88.$tag === 1) {
            const e = $match_88.$0;
            return Err2(e);
          }
          if ($match_88.$tag === 0) {
            const rest = $match_88.$0[0];
            const s3 = $match_88.$0[1];
            return Ok2([_COLON_COLON2(field)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseMoreRecordTypeFields(field)(s2));
      }
      throw new Error("Pattern match failed");
    })(parseRecordTypeField(s1)))(advance2(state)) : Ok2([[], state]);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : Ok2([[], state]);
parseRecordTypeField = (state) => (($match_89) => {
  if ($match_89.$tag === 1) {
    const e = $match_89.$0;
    return Err2(e);
  }
  if ($match_89.$tag === 0) {
    const nameTok = $match_89.$0[0];
    const s1 = $match_89.$0[1];
    return (($match_90) => {
      if ($match_90.$tag === 1) {
        const e = $match_90.$0;
        return Err2(e);
      }
      if ($match_90.$tag === 0) {
        const s2 = $match_90.$0[1];
        return (($match_91) => {
          if ($match_91.$tag === 1) {
            const e = $match_91.$0;
            return Err2(e);
          }
          if ($match_91.$tag === 0) {
            const fieldType = $match_91.$0[0];
            const s3 = $match_91.$0[1];
            return Ok2([{ name: nameTok.lexeme, fieldType, span: { start: nameTok.span.start, end: typeSpan(fieldType).end } }, s3]);
          }
          throw new Error("Pattern match failed");
        })(parseTypeExpression(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Colon2)("':' after field name")(s1));
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier2)("record field name")(state));
parseParenOrTupleType = (state) => ((start) => ((s1) => peekKind(RParen2)(s1) ? ((s2) => Ok2([TRef("Unit")([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_92) => {
  if ($match_92.$tag === 1) {
    const e = $match_92.$0;
    return Err2(e);
  }
  if ($match_92.$tag === 0) {
    const first = $match_92.$0[0];
    const s2 = $match_92.$0[1];
    return (($match_93) => {
      if ($match_93.$tag === 1) {
        const e = $match_93.$0;
        return Err2(e);
      }
      if ($match_93.$tag === 0) {
        const moreTypes = $match_93.$0[0];
        const s3 = $match_93.$0[1];
        return (($match_94) => {
          if ($match_94.$tag === 1) {
            const e = $match_94.$0;
            return Err2(e);
          }
          if ($match_94.$tag === 0) {
            const cp = $match_94.$0[0];
            const s4 = $match_94.$0[1];
            return isEmpty2(moreTypes) ? Ok2([setTypeSpan(first)({ start, end: cp.span.end }), s4]) : Ok2([TTuple(_COLON_COLON2(first)(moreTypes))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close type group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleTypes(first)(s2));
  }
  throw new Error("Pattern match failed");
})(parseTypeExpression(s1)))(advance2(state)))(current(state).span.start);
parseMoreTupleTypes = (lastType) => (state) => peekKind(Comma2)(state) ? (($match_95) => {
  if ($match_95.$tag === 1) {
    return Ok2([[], state]);
  }
  if ($match_95.$tag === 0) {
    const next = $match_95.$0;
    return continuesLayout(typeSpan(lastType).start.column)(typeSpan(lastType).end)(next) ? ((s1) => (($match_96) => {
      if ($match_96.$tag === 1) {
        const e = $match_96.$0;
        return Err2(e);
      }
      if ($match_96.$tag === 0) {
        const t = $match_96.$0[0];
        const s2 = $match_96.$0[1];
        return (($match_97) => {
          if ($match_97.$tag === 1) {
            const e = $match_97.$0;
            return Err2(e);
          }
          if ($match_97.$tag === 0) {
            const rest = $match_97.$0[0];
            const s3 = $match_97.$0[1];
            return Ok2([_COLON_COLON2(t)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseMoreTupleTypes(t)(s2));
      }
      throw new Error("Pattern match failed");
    })(parseTypeExpression(s1)))(advance2(state)) : Ok2([[], state]);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : Ok2([[], state]);
parseTypeRef = (state) => ((tok) => _PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier2)) ? ((ident) => ((s1) => (($match_98) => {
  if ($match_98.$tag === 1) {
    const e = $match_98.$0;
    return Err2(e);
  }
  if ($match_98.$tag === 0) {
    const nameResult = $match_98.$0[0];
    const s2 = $match_98.$0[1];
    return ((isTypeCtor) => isTypeCtor ? (($match_99) => {
      if ($match_99.$tag === 1) {
        const e = $match_99.$0;
        return Err2(e);
      }
      if ($match_99.$tag === 0) {
        const args = $match_99.$0[0];
        const s3 = $match_99.$0[1];
        return ((endPos) => Ok2([TRef(nameResult.name)(args)({ start: ident.span.start, end: endPos }), s3]))((($match_100) => {
          if ($match_100.$tag === 0) {
            const a = $match_100.$0;
            return typeSpan(a).end;
          }
          if ($match_100.$tag === 1) {
            return nameResult.nameEnd;
          }
          throw new Error("Pattern match failed");
        })(last(args)));
      }
      throw new Error("Pattern match failed");
    })(parseTypeApplicationArgs({ start: ident.span.start, end: nameResult.nameEnd })(s2)) : Ok2([TRef(nameResult.name)([])({ start: ident.span.start, end: nameResult.nameEnd }), s2]))(_PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(ident.kind)(UpperIdentifier2))(() => contains(".")(nameResult.name)));
  }
  throw new Error("Pattern match failed");
})(parseQualifiedName(ident.lexeme)(ident.span.end)(s1)))(advance2(state)))(tok) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected type but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
parseTypeApplicationArgs = (lastSpan) => (state) => _AMP_AMP5(isTypeStart(current(state)))(() => onSameLine(lastSpan)(current(state))) ? (($match_101) => {
  if ($match_101.$tag === 1) {
    const e = $match_101.$0;
    return Err2(e);
  }
  if ($match_101.$tag === 0) {
    const arg = $match_101.$0[0];
    const s1 = $match_101.$0[1];
    return (($match_102) => {
      if ($match_102.$tag === 1) {
        const e = $match_102.$0;
        return Err2(e);
      }
      if ($match_102.$tag === 0) {
        const rest = $match_102.$0[0];
        const s2 = $match_102.$0[1];
        return Ok2([_COLON_COLON2(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeApplicationArgs(typeSpan(arg))(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok2([[], state]);
parseTypeAtom = (state) => ((tok) => (($match_103) => {
  if ($match_103.$tag === 11) {
    return parseRecordType(state);
  }
  if ($match_103.$tag === 9) {
    return parseParenOrTupleType(state);
  }
  {
    return _PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier2)) ? ((ident) => ((s1) => (($match_104) => {
      if ($match_104.$tag === 1) {
        const e = $match_104.$0;
        return Err2(e);
      }
      if ($match_104.$tag === 0) {
        const nameResult = $match_104.$0[0];
        const s2 = $match_104.$0[1];
        return Ok2([TRef(nameResult.name)([])({ start: ident.span.start, end: nameResult.nameEnd }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseQualifiedName(ident.lexeme)(ident.span.end)(s1)))(advance2(state)))(tok) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected type but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var skipNewlines = (state) => {
  while (true) {
    if ($dict_Eq_TokenKind._EQ_EQ(current(state).kind)(Newline2)) {
      state = advance2(state);
      continue;
    } else {
      return state;
    }
  }
};
var parseDecoratedDeclaration = (state) => (($match_105) => {
  if ($match_105.$tag === 1) {
    const e = $match_105.$0;
    return Err2(e);
  }
  if ($match_105.$tag === 0) {
    const atTok = $match_105.$0[0];
    const s1 = $match_105.$0[1];
    return ((decoratorTok) => _PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(decoratorTok.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(decoratorTok.kind)(Keyword2)) ? ((decorator) => ((s2) => (($match_106) => {
      if ($match_106.$tag === 1) {
        const e = $match_106.$0;
        return Err2(e);
      }
      if ($match_106.$tag === 0) {
        const args = $match_106.$0[0];
        const s3 = $match_106.$0[1];
        return ((s4) => (($match_107) => {
          if ($match_107.$tag === 1) {
            const e = $match_107.$0;
            return Err2(e);
          }
          if ($match_107.$tag === 0) {
            const nameResult = $match_107.$0[0];
            const s5 = $match_107.$0[1];
            return (($match_108) => {
              if ($match_108.$tag === 1) {
                const e = $match_108.$0;
                return Err2(e);
              }
              if ($match_108.$tag === 0) {
                const s6 = $match_108.$0[1];
                return (($match_109) => {
                  if ($match_109.$tag === 1) {
                    const e = $match_109.$0;
                    return Err2(e);
                  }
                  if ($match_109.$tag === 0) {
                    const annotation = $match_109.$0[0];
                    const s7 = $match_109.$0[1];
                    return Ok2([DDecorated(decorator)(args)(nameResult.declName)(annotation)({ start: atTok.span.start, end: typeSpan(annotation).end }), s7]);
                  }
                  throw new Error("Pattern match failed");
                })(parseTypeExpression(s6));
              }
              throw new Error("Pattern match failed");
            })(expect(Colon2)("type annotation")(s5));
          }
          throw new Error("Pattern match failed");
        })(parseDeclarationName(s4)))(skipNewlines(s3));
      }
      throw new Error("Pattern match failed");
    })(parseDecoratorArgs(s2)))(advance2(s1)))(decoratorTok.lexeme) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Expected decorator name but found ")(tokenKindStr(decoratorTok.kind)))(decoratorTok.span)))(current(s1));
  }
  throw new Error("Pattern match failed");
})(expectOperator("@")(state));
var parseTypeParams = (state) => peekKind(LowerIdentifier2)(state) ? ((tok) => ((s1) => (($match_110) => {
  if ($match_110.$tag === 1) {
    const e = $match_110.$0;
    return Err2(e);
  }
  if ($match_110.$tag === 0) {
    const rest = $match_110.$0[0];
    const s2 = $match_110.$0[1];
    return Ok2([_COLON_COLON2(tok.lexeme)(rest), s2]);
  }
  throw new Error("Pattern match failed");
})(parseTypeParams(s1)))(advance2(state)))(current(state)) : Ok2([[], state]);
var parseTypeAliasDecl = (typeTok) => (state) => (($match_111) => {
  if ($match_111.$tag === 1) {
    const e = $match_111.$0;
    return Err2(e);
  }
  if ($match_111.$tag === 0) {
    const s1 = $match_111.$0[1];
    return (($match_112) => {
      if ($match_112.$tag === 1) {
        const e = $match_112.$0;
        return Err2(e);
      }
      if ($match_112.$tag === 0) {
        const nameTok = $match_112.$0[0];
        const s2 = $match_112.$0[1];
        return (($match_113) => {
          if ($match_113.$tag === 1) {
            const e = $match_113.$0;
            return Err2(e);
          }
          if ($match_113.$tag === 0) {
            const params = $match_113.$0[0];
            const s3 = $match_113.$0[1];
            return (($match_114) => {
              if ($match_114.$tag === 1) {
                const e = $match_114.$0;
                return Err2(e);
              }
              if ($match_114.$tag === 0) {
                const s4 = $match_114.$0[1];
                return (($match_115) => {
                  if ($match_115.$tag === 1) {
                    const e = $match_115.$0;
                    return Err2(e);
                  }
                  if ($match_115.$tag === 0) {
                    const value = $match_115.$0[0];
                    const s5 = $match_115.$0[1];
                    return Ok2([DTypeAlias(nameTok.lexeme)(params)(value)({ start: typeTok.span.start, end: typeSpan(value).end }), s5]);
                  }
                  throw new Error("Pattern match failed");
                })(parseTypeExpression(s4));
              }
              throw new Error("Pattern match failed");
            })(expect(Equals2)("type alias definition")(s3));
          }
          throw new Error("Pattern match failed");
        })(parseTypeParams(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(UpperIdentifier2)("type alias name")(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("alias")(state));
var parseOptionalConstraintsForTypeDecl = (state) => peekConstraintContextInTypeDecl(state) ? parseConstraints(state) : Ok2([[], state]);
var parseTypeParamsWithLayout = (typeLine) => (typeCol) => (state) => peekKind(LowerIdentifier2)(state) ? ((tok) => _AMP_AMP5($dict_Eq_Int2._SLASH_EQ(tok.span.start.line)(typeLine))(() => $dict_Ord_Int2._LT_EQ(tok.span.start.column)(typeCol)) ? Ok2([[], state]) : ((s1) => (($match_116) => {
  if ($match_116.$tag === 1) {
    const e = $match_116.$0;
    return Err2(e);
  }
  if ($match_116.$tag === 0) {
    const rest = $match_116.$0[0];
    const s2 = $match_116.$0[1];
    return Ok2([_COLON_COLON2(tok.lexeme)(rest), s2]);
  }
  throw new Error("Pattern match failed");
})(parseTypeParamsWithLayout(typeLine)(typeCol)(s1)))(advance2(state)))(current(state)) : Ok2([[], state]);
var parseRecordTypeFields = (state) => (($match_117) => {
  if ($match_117.$tag === 1) {
    const e = $match_117.$0;
    return Err2(e);
  }
  if ($match_117.$tag === 0) {
    const s1 = $match_117.$0[1];
    return peekKind(RBrace2)(s1) ? ((s2) => Ok2([[], s2]))(advance2(s1)) : (($match_118) => {
      if ($match_118.$tag === 1) {
        const e = $match_118.$0;
        return Err2(e);
      }
      if ($match_118.$tag === 0) {
        const fields = $match_118.$0[0];
        const s2 = $match_118.$0[1];
        return (($match_119) => {
          if ($match_119.$tag === 1) {
            const e = $match_119.$0;
            return Err2(e);
          }
          if ($match_119.$tag === 0) {
            const s3 = $match_119.$0[1];
            return Ok2([fields, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RBrace2)("close record type '}'")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseRecordTypeFieldList(s1));
  }
  throw new Error("Pattern match failed");
})(expect(LBrace2)("record type start '{'")(state));
var parseConstructorArgs = (lastSpan) => (state) => _AMP_AMP5(isTypeStart(current(state)))(() => _AMP_AMP5(onSameLine(lastSpan)(current(state)))(() => not2(peekKind(Pipe2)(state)))) ? (($match_120) => {
  if ($match_120.$tag === 1) {
    const e = $match_120.$0;
    return Err2(e);
  }
  if ($match_120.$tag === 0) {
    const arg = $match_120.$0[0];
    const s1 = $match_120.$0[1];
    return (($match_121) => {
      if ($match_121.$tag === 1) {
        const e = $match_121.$0;
        return Err2(e);
      }
      if ($match_121.$tag === 0) {
        const rest = $match_121.$0[0];
        const s2 = $match_121.$0[1];
        return Ok2([_COLON_COLON2(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseConstructorArgs(typeSpan(arg))(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok2([[], state]);
var parseConstructorVariant = (state) => (($match_122) => {
  if ($match_122.$tag === 1) {
    const e = $match_122.$0;
    return Err2(e);
  }
  if ($match_122.$tag === 0) {
    const nameTok = $match_122.$0[0];
    const s1 = $match_122.$0[1];
    return (($match_123) => {
      if ($match_123.$tag === 1) {
        const e = $match_123.$0;
        return Err2(e);
      }
      if ($match_123.$tag === 0) {
        const args = $match_123.$0[0];
        const s2 = $match_123.$0[1];
        return ((endPos) => Ok2([{ name: nameTok.lexeme, args, span: { start: nameTok.span.start, end: endPos } }, s2]))((($match_124) => {
          if ($match_124.$tag === 0) {
            const a = $match_124.$0;
            return typeSpan(a).end;
          }
          if ($match_124.$tag === 1) {
            return nameTok.span.end;
          }
          throw new Error("Pattern match failed");
        })(last(args)));
      }
      throw new Error("Pattern match failed");
    })(parseConstructorArgs(nameTok.span)(s1));
  }
  throw new Error("Pattern match failed");
})(expect(UpperIdentifier2)("constructor name")(state));
var parseMoreConstructors = (equalsCol) => (equalsLine) => (state) => peekKind(Pipe2)(state) ? ((pipeTok) => _AMP_AMP5($dict_Eq_Int2._SLASH_EQ(pipeTok.span.start.line)(equalsLine))(() => $dict_Eq_Int2._SLASH_EQ(pipeTok.span.start.column)(equalsCol)) ? Err2(makeError("Constructor variant '|' must align with '='")(pipeTok.span)) : ((s1) => (($match_125) => {
  if ($match_125.$tag === 1) {
    const e = $match_125.$0;
    return Err2(e);
  }
  if ($match_125.$tag === 0) {
    const ctor = $match_125.$0[0];
    const s2 = $match_125.$0[1];
    return (($match_126) => {
      if ($match_126.$tag === 1) {
        const e = $match_126.$0;
        return Err2(e);
      }
      if ($match_126.$tag === 0) {
        const rest = $match_126.$0[0];
        const s3 = $match_126.$0[1];
        return Ok2([_COLON_COLON2(ctor)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreConstructors(equalsCol)(equalsLine)(s2));
  }
  throw new Error("Pattern match failed");
})(parseConstructorVariant(s1)))(advance2(state)))(current(state)) : Ok2([[], state]);
var parseConstructorVariants = (equalsCol) => (equalsLine) => (state) => (($match_127) => {
  if ($match_127.$tag === 1) {
    const e = $match_127.$0;
    return Err2(e);
  }
  if ($match_127.$tag === 0) {
    const first = $match_127.$0[0];
    const s1 = $match_127.$0[1];
    return (($match_128) => {
      if ($match_128.$tag === 1) {
        const e = $match_128.$0;
        return Err2(e);
      }
      if ($match_128.$tag === 0) {
        const rest = $match_128.$0[0];
        const s2 = $match_128.$0[1];
        return Ok2([_COLON_COLON2(first)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreConstructors(equalsCol)(equalsLine)(s1));
  }
  throw new Error("Pattern match failed");
})(parseConstructorVariant(state));
var parseTypeOrOpaqueDecl = (typeTok) => (state) => ((typeColumn) => ((typeLine) => (($match_129) => {
  if ($match_129.$tag === 1) {
    const e = $match_129.$0;
    return Err2(e);
  }
  if ($match_129.$tag === 0) {
    const constraints = $match_129.$0[0];
    const s1 = $match_129.$0[1];
    return (($match_130) => {
      if ($match_130.$tag === 1) {
        const e = $match_130.$0;
        return Err2(e);
      }
      if ($match_130.$tag === 0) {
        const nameTok = $match_130.$0[0];
        const s2 = $match_130.$0[1];
        return (($match_131) => {
          if ($match_131.$tag === 1) {
            const e = $match_131.$0;
            return Err2(e);
          }
          if ($match_131.$tag === 0) {
            const params = $match_131.$0[0];
            const s3 = $match_131.$0[1];
            return peekKind(Equals2)(s3) ? ((eqTok) => ((s4) => ((equalsCol) => ((equalsLine) => peekKind(LBrace2)(s4) ? (($match_132) => {
              if ($match_132.$tag === 1) {
                const e = $match_132.$0;
                return Err2(e);
              }
              if ($match_132.$tag === 0) {
                const fields = $match_132.$0[0];
                const s5 = $match_132.$0[1];
                return Ok2([DRecordType(nameTok.lexeme)(params)(constraints)(fields)({ start: typeTok.span.start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(parseRecordTypeFields(s4)) : (($match_133) => {
              if ($match_133.$tag === 1) {
                const e = $match_133.$0;
                return Err2(e);
              }
              if ($match_133.$tag === 0) {
                const ctors = $match_133.$0[0];
                const s5 = $match_133.$0[1];
                return ((endPos) => Ok2([DType(nameTok.lexeme)(params)(constraints)(ctors)({ start: typeTok.span.start, end: endPos }), s5]))((($match_134) => {
                  if ($match_134.$tag === 0) {
                    const c = $match_134.$0;
                    return c.span.end;
                  }
                  if ($match_134.$tag === 1) {
                    return eqTok.span.end;
                  }
                  throw new Error("Pattern match failed");
                })(last(ctors)));
              }
              throw new Error("Pattern match failed");
            })(parseConstructorVariants(equalsCol)(equalsLine)(s4)))(eqTok.span.start.line))(eqTok.span.start.column))(advance2(s3)))(current(s3)) : isEmpty2(constraints) ? ((endPos) => Ok2([DOpaqueType(nameTok.lexeme)(params)({ start: typeTok.span.start, end: endPos }), s3]))(isEmpty2(params) ? nameTok.span.end : previous(s3).span.end) : Err2(makeError("Opaque types cannot have constraints")({ start: typeTok.span.start, end: previous(s3).span.end }));
          }
          throw new Error("Pattern match failed");
        })(parseTypeParamsWithLayout(typeLine)(typeColumn)(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(UpperIdentifier2)("type name")(s1));
  }
  throw new Error("Pattern match failed");
})(parseOptionalConstraintsForTypeDecl(state)))(typeTok.span.start.line))(typeTok.span.start.column);
var parseTypeOrAliasDeclaration = (state) => (($match_135) => {
  if ($match_135.$tag === 1) {
    const e = $match_135.$0;
    return Err2(e);
  }
  if ($match_135.$tag === 0) {
    const typeTok = $match_135.$0[0];
    const s1 = $match_135.$0[1];
    return peekKeyword("alias")(s1) ? parseTypeAliasDecl(typeTok)(s1) : parseTypeOrOpaqueDecl(typeTok)(s1);
  }
  throw new Error("Pattern match failed");
})(expectKeyword("type")(state));
var parseOptionalConstraints = (hasConstraints) => (state) => hasConstraints ? parseConstraints(state) : Ok2([[], state]);
var parseProtocolMethodName = (state) => ((tok) => (($match_136) => {
  if ($match_136.$tag === 9) {
    return ((s1) => (($match_137) => {
      if ($match_137.$tag === 1) {
        const e = $match_137.$0;
        return Err2(e);
      }
      if ($match_137.$tag === 0) {
        const opTok = $match_137.$0[0];
        const s2 = $match_137.$0[1];
        return (($match_138) => {
          if ($match_138.$tag === 1) {
            const e = $match_138.$0;
            return Err2(e);
          }
          if ($match_138.$tag === 0) {
            const s3 = $match_138.$0[1];
            return Ok2([opTok.lexeme, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close paren after operator")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator2)("operator in protocol method")(s1)))(advance2(state));
  }
  if ($match_136.$tag === 0) {
    return ((s1) => Ok2([tok.lexeme, s1]))(advance2(state));
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Expected method name but found ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
var setPatSpan = (pat) => (span) => (($match_139) => {
  if ($match_139.$tag === 0) {
    const n = $match_139.$0;
    return PVar(n)(span);
  }
  if ($match_139.$tag === 1) {
    return PWildcard(span);
  }
  if ($match_139.$tag === 2) {
    const n = $match_139.$0;
    const a = $match_139.$1;
    return PConstructor(n)(a)(span);
  }
  if ($match_139.$tag === 3) {
    const e = $match_139.$0;
    return PTuple(e)(span);
  }
  if ($match_139.$tag === 4) {
    const e = $match_139.$0;
    return PList(e)(span);
  }
  if ($match_139.$tag === 5) {
    const h = $match_139.$0;
    const t = $match_139.$1;
    return PCons(h)(t)(span);
  }
  if ($match_139.$tag === 6) {
    const f = $match_139.$0;
    return PRecord(f)(span);
  }
  if ($match_139.$tag === 7) {
    const v = $match_139.$0;
    return PInt(v)(span);
  }
  if ($match_139.$tag === 8) {
    const v = $match_139.$0;
    return PFloat(v)(span);
  }
  if ($match_139.$tag === 9) {
    const v = $match_139.$0;
    return PString(v)(span);
  }
  if ($match_139.$tag === 10) {
    const v = $match_139.$0;
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
parseFunctionParamPattern = (state) => ((tok) => (($match_140) => {
  if ($match_140.$tag === 0) {
    return ((s1) => $dict_Eq_String2._EQ_EQ(tok.lexeme)("_") ? Ok2([PWildcard(tok.span), s1]) : Ok2([PVar(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_140.$tag === 1) {
    return parseFPCtorPattern(state);
  }
  if ($match_140.$tag === 9) {
    return parseFPTuplePattern(state);
  }
  if ($match_140.$tag === 11) {
    return parseFPRecordPattern(state);
  }
  if ($match_140.$tag === 13) {
    return Err2(makeError("List patterns are not allowed in function parameters")(tok.span));
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Expected function parameter pattern but found ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseFPCtorPattern = (state) => ((ctor) => ((s1) => (($match_141) => {
  if ($match_141.$tag === 1) {
    const e = $match_141.$0;
    return Err2(e);
  }
  if ($match_141.$tag === 0) {
    const args = $match_141.$0[0];
    const s2 = $match_141.$0[1];
    return ((endPos) => Ok2([PConstructor(ctor.lexeme)(args)({ start: ctor.span.start, end: endPos }), s2]))((($match_142) => {
      if ($match_142.$tag === 0) {
        const a = $match_142.$0;
        return patSpan(a).end;
      }
      if ($match_142.$tag === 1) {
        return ctor.span.end;
      }
      throw new Error("Pattern match failed");
    })(last(args)));
  }
  throw new Error("Pattern match failed");
})(parseFPCtorArgs(s1)))(advance2(state)))(current(state));
parseFPCtorArgs = (state) => isFunctionParamPatternStart(current(state)) ? (($match_143) => {
  if ($match_143.$tag === 1) {
    const e = $match_143.$0;
    return Err2(e);
  }
  if ($match_143.$tag === 0) {
    const pat = $match_143.$0[0];
    const s1 = $match_143.$0[1];
    return (($match_144) => {
      if ($match_144.$tag === 1) {
        const e = $match_144.$0;
        return Err2(e);
      }
      if ($match_144.$tag === 0) {
        const rest = $match_144.$0[0];
        const s2 = $match_144.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseFPCtorArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(state)) : Ok2([[], state]);
parseFPTuplePattern = (state) => ((start) => ((s1) => peekKind(RParen2)(s1) ? ((s2) => Ok2([PTuple([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_145) => {
  if ($match_145.$tag === 1) {
    const e = $match_145.$0;
    return Err2(e);
  }
  if ($match_145.$tag === 0) {
    const first = $match_145.$0[0];
    const s2 = $match_145.$0[1];
    return (($match_146) => {
      if ($match_146.$tag === 1) {
        const e = $match_146.$0;
        return Err2(e);
      }
      if ($match_146.$tag === 0) {
        const more = $match_146.$0[0];
        const s3 = $match_146.$0[1];
        return (($match_147) => {
          if ($match_147.$tag === 1) {
            const e = $match_147.$0;
            return Err2(e);
          }
          if ($match_147.$tag === 0) {
            const cp = $match_147.$0[0];
            const s4 = $match_147.$0[1];
            return isEmpty2(more) ? Ok2([setPatSpan(first)({ start, end: cp.span.end }), s4]) : Ok2([PTuple(_COLON_COLON2(first)(more))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close pattern group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseFPTupleRest(first)(s2));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(s1)))(advance2(state)))(current(state).span.start);
parseFPTupleRest = (lastPat) => (state) => peekKind(Comma2)(state) ? (($match_148) => {
  if ($match_148.$tag === 1) {
    return Ok2([[], state]);
  }
  if ($match_148.$tag === 0) {
    const next = $match_148.$0;
    return continuesLayout(patSpan(lastPat).start.column)(patSpan(lastPat).end)(next) ? ((s1) => (($match_149) => {
      if ($match_149.$tag === 1) {
        const e = $match_149.$0;
        return Err2(e);
      }
      if ($match_149.$tag === 0) {
        const pat = $match_149.$0[0];
        const s2 = $match_149.$0[1];
        return (($match_150) => {
          if ($match_150.$tag === 1) {
            const e = $match_150.$0;
            return Err2(e);
          }
          if ($match_150.$tag === 0) {
            const rest = $match_150.$0[0];
            const s3 = $match_150.$0[1];
            return Ok2([_COLON_COLON2(pat)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseFPTupleRest(pat)(s2));
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParamPattern(s1)))(advance2(state)) : Ok2([[], state]);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : Ok2([[], state]);
parseFPRecordPattern = (state) => ((start) => ((s1) => peekKind(RBrace2)(s1) ? ((s2) => Ok2([PRecord([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_151) => {
  if ($match_151.$tag === 1) {
    const e = $match_151.$0;
    return Err2(e);
  }
  if ($match_151.$tag === 0) {
    const fields = $match_151.$0[0];
    const s2 = $match_151.$0[1];
    return (($match_152) => {
      if ($match_152.$tag === 1) {
        const e = $match_152.$0;
        return Err2(e);
      }
      if ($match_152.$tag === 0) {
        const s3 = $match_152.$0[1];
        return Ok2([PRecord(fields)({ start, end: previous(s3).span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RBrace2)("close record pattern")(s2));
  }
  throw new Error("Pattern match failed");
})(parseRecordFieldPatterns(s1)))(advance2(state)))(current(state).span.start);
parseRecordFieldPatterns = (state) => (($match_153) => {
  if ($match_153.$tag === 1) {
    const e = $match_153.$0;
    return Err2(e);
  }
  if ($match_153.$tag === 0) {
    const field = $match_153.$0[0];
    const s1 = $match_153.$0[1];
    return peekKind(Comma2)(s1) ? ((s2) => (($match_154) => {
      if ($match_154.$tag === 1) {
        const e = $match_154.$0;
        return Err2(e);
      }
      if ($match_154.$tag === 0) {
        const rest = $match_154.$0[0];
        const s3 = $match_154.$0[1];
        return Ok2([_COLON_COLON2(field)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseRecordFieldPatterns(s2)))(advance2(s1)) : Ok2([[field], s1]);
  }
  throw new Error("Pattern match failed");
})(parseRecordFieldPattern(state));
parseRecordFieldPattern = (state) => (($match_155) => {
  if ($match_155.$tag === 1) {
    const e = $match_155.$0;
    return Err2(e);
  }
  if ($match_155.$tag === 0) {
    const nameTok = $match_155.$0[0];
    const s1 = $match_155.$0[1];
    return peekKind(Equals2)(s1) ? ((s2) => (($match_156) => {
      if ($match_156.$tag === 1) {
        const e = $match_156.$0;
        return Err2(e);
      }
      if ($match_156.$tag === 0) {
        const pat = $match_156.$0[0];
        const s3 = $match_156.$0[1];
        return Ok2([{ name: nameTok.lexeme, pattern: pat }, s3]);
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParamPattern(s2)))(advance2(s1)) : Ok2([{ name: nameTok.lexeme, pattern: PVar(nameTok.lexeme)(nameTok.span) }, s1]);
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier2)("record field name")(state));
var parseFunctionParams = (state) => isFunctionParamPatternStart(current(state)) ? (($match_157) => {
  if ($match_157.$tag === 1) {
    const e = $match_157.$0;
    return Err2(e);
  }
  if ($match_157.$tag === 0) {
    const pat = $match_157.$0[0];
    const s1 = $match_157.$0[1];
    return (($match_158) => {
      if ($match_158.$tag === 1) {
        const e = $match_158.$0;
        return Err2(e);
      }
      if ($match_158.$tag === 0) {
        const rest = $match_158.$0[0];
        const s2 = $match_158.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParams(s1));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(state)) : Ok2([[], state]);
var declToValueDecl = (decl) => (($match_159) => {
  if ($match_159.$tag === 0) {
    const vd = $match_159.$0;
    return vd;
  }
  if ($match_159.$tag === 1) {
    const ta = $match_159.$0;
    return { name: ta.name, args: [], body: EUnit(ta.span), span: ta.span };
  }
  {
    return { name: "", args: [], body: EUnit(eofToken.span), span: eofToken.span };
  }
  throw new Error("Pattern match failed");
})(decl);
var parseQualifiedCtorName = (name) => (end) => (state) => {
  while (true) {
    if (peekKind(Dot2)(state)) {
      {
        const $match_160 = peekAhead(1)(state);
        if ($match_160.$tag === 1) {
          return Ok2([{ name, nameEnd: end }, state]);
        }
        if ($match_160.$tag === 0) {
          const next = $match_160.$0;
          if ($dict_Eq_TokenKind._EQ_EQ(next.kind)(UpperIdentifier2)) {
            {
              const s1 = advance2(state);
              {
                const $match_161 = expect(UpperIdentifier2)("constructor name part")(s1);
                if ($match_161.$tag === 1) {
                  const e = $match_161.$0;
                  return Err2(e);
                }
                if ($match_161.$tag === 0) {
                  const nextTok = $match_161.$0[0];
                  const s2 = $match_161.$0[1];
                  [name, end, state] = [$dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(name)("."))(nextTok.lexeme), nextTok.span.end, s2];
                  continue;
                }
                throw new Error("Pattern match failed");
              }
            }
          } else {
            return Ok2([{ name, nameEnd: end }, state]);
          }
        }
        throw new Error("Pattern match failed");
      }
    } else {
      return Ok2([{ name, nameEnd: end }, state]);
    }
  }
};
var parsePattern;
var parsePrimaryPattern;
var parseConstructorPattern;
var parseConstructorPatternArgs;
var parseListPattern;
var parsePatternList;
var parseParenOrTuplePattern;
var parseTuplePatternRest;
parsePattern = (state) => (($match_162) => {
  if ($match_162.$tag === 1) {
    const e = $match_162.$0;
    return Err2(e);
  }
  if ($match_162.$tag === 0) {
    const primary = $match_162.$0[0];
    const s1 = $match_162.$0[1];
    return peekOperator("::")(s1) ? ((s2) => (($match_163) => {
      if ($match_163.$tag === 1) {
        const e = $match_163.$0;
        return Err2(e);
      }
      if ($match_163.$tag === 0) {
        const tail = $match_163.$0[0];
        const s3 = $match_163.$0[1];
        return Ok2([PCons(primary)(tail)({ start: patSpan(primary).start, end: patSpan(tail).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parsePattern(s2)))(advance2(s1)) : Ok2([primary, s1]);
  }
  throw new Error("Pattern match failed");
})(parsePrimaryPattern(state));
parsePrimaryPattern = (state) => ((tok) => (($match_164) => {
  if ($match_164.$tag === 0) {
    return ((s1) => $dict_Eq_String2._EQ_EQ(tok.lexeme)("_") ? Ok2([PWildcard(tok.span), s1]) : Ok2([PVar(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_164.$tag === 3) {
    return ((s1) => contains(".")(tok.lexeme) ? Ok2([PFloat(tok.lexeme)(tok.span), s1]) : Ok2([PInt(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_164.$tag === 4) {
    return ((s1) => Ok2([PString(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_164.$tag === 5) {
    return ((s1) => Ok2([PChar(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_164.$tag === 1) {
    return parseConstructorPattern(state);
  }
  if ($match_164.$tag === 13) {
    return parseListPattern(state);
  }
  if ($match_164.$tag === 9) {
    return parseParenOrTuplePattern(state);
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected pattern but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseConstructorPattern = (state) => ((ctor) => ((s1) => (($match_165) => {
  if ($match_165.$tag === 1) {
    const e = $match_165.$0;
    return Err2(e);
  }
  if ($match_165.$tag === 0) {
    const nameResult = $match_165.$0[0];
    const s2 = $match_165.$0[1];
    return (($match_166) => {
      if ($match_166.$tag === 1) {
        const e = $match_166.$0;
        return Err2(e);
      }
      if ($match_166.$tag === 0) {
        const args = $match_166.$0[0];
        const s3 = $match_166.$0[1];
        return ((endPos) => Ok2([PConstructor(nameResult.name)(args)({ start: ctor.span.start, end: endPos }), s3]))((($match_167) => {
          if ($match_167.$tag === 0) {
            const a = $match_167.$0;
            return patSpan(a).end;
          }
          if ($match_167.$tag === 1) {
            return nameResult.nameEnd;
          }
          throw new Error("Pattern match failed");
        })(last(args)));
      }
      throw new Error("Pattern match failed");
    })(parseConstructorPatternArgs(s2));
  }
  throw new Error("Pattern match failed");
})(parseQualifiedCtorName(ctor.lexeme)(ctor.span.end)(s1)))(advance2(state)))(current(state));
parseConstructorPatternArgs = (state) => isPatternStart(current(state)) ? (($match_168) => {
  if ($match_168.$tag === 1) {
    const e = $match_168.$0;
    return Err2(e);
  }
  if ($match_168.$tag === 0) {
    const pat = $match_168.$0[0];
    const s1 = $match_168.$0[1];
    return (($match_169) => {
      if ($match_169.$tag === 1) {
        const e = $match_169.$0;
        return Err2(e);
      }
      if ($match_169.$tag === 0) {
        const rest = $match_169.$0[0];
        const s2 = $match_169.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseConstructorPatternArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parsePrimaryPattern(state)) : Ok2([[], state]);
parseListPattern = (state) => ((start) => ((s1) => peekKind(RBracket2)(s1) ? ((s2) => Ok2([PList([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_170) => {
  if ($match_170.$tag === 1) {
    const e = $match_170.$0;
    return Err2(e);
  }
  if ($match_170.$tag === 0) {
    const elements = $match_170.$0[0];
    const s2 = $match_170.$0[1];
    return (($match_171) => {
      if ($match_171.$tag === 1) {
        const e = $match_171.$0;
        return Err2(e);
      }
      if ($match_171.$tag === 0) {
        const s3 = $match_171.$0[1];
        return Ok2([PList(elements)({ start, end: previous(s3).span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RBracket2)("close list pattern")(s2));
  }
  throw new Error("Pattern match failed");
})(parsePatternList(s1)))(advance2(state)))(current(state).span.start);
parsePatternList = (state) => (($match_172) => {
  if ($match_172.$tag === 1) {
    const e = $match_172.$0;
    return Err2(e);
  }
  if ($match_172.$tag === 0) {
    const pat = $match_172.$0[0];
    const s1 = $match_172.$0[1];
    return peekKind(Comma2)(s1) ? ((s2) => (($match_173) => {
      if ($match_173.$tag === 1) {
        const e = $match_173.$0;
        return Err2(e);
      }
      if ($match_173.$tag === 0) {
        const rest = $match_173.$0[0];
        const s3 = $match_173.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parsePatternList(s2)))(advance2(s1)) : Ok2([[pat], s1]);
  }
  throw new Error("Pattern match failed");
})(parsePattern(state));
parseParenOrTuplePattern = (state) => ((start) => ((s1) => peekKind(RParen2)(s1) ? ((s2) => Ok2([PTuple([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_174) => {
  if ($match_174.$tag === 1) {
    const e = $match_174.$0;
    return Err2(e);
  }
  if ($match_174.$tag === 0) {
    const first = $match_174.$0[0];
    const s2 = $match_174.$0[1];
    return (($match_175) => {
      if ($match_175.$tag === 1) {
        const e = $match_175.$0;
        return Err2(e);
      }
      if ($match_175.$tag === 0) {
        const more = $match_175.$0[0];
        const s3 = $match_175.$0[1];
        return (($match_176) => {
          if ($match_176.$tag === 1) {
            const e = $match_176.$0;
            return Err2(e);
          }
          if ($match_176.$tag === 0) {
            const cp = $match_176.$0[0];
            const s4 = $match_176.$0[1];
            return isEmpty2(more) ? Ok2([setPatSpan(first)({ start, end: cp.span.end }), s4]) : Ok2([PTuple(_COLON_COLON2(first)(more))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close pattern group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseTuplePatternRest(first)(s2));
  }
  throw new Error("Pattern match failed");
})(parsePattern(s1)))(advance2(state)))(current(state).span.start);
parseTuplePatternRest = (lastPat) => (state) => peekKind(Comma2)(state) ? (($match_177) => {
  if ($match_177.$tag === 1) {
    return Ok2([[], state]);
  }
  if ($match_177.$tag === 0) {
    const next = $match_177.$0;
    return continuesLayout(patSpan(lastPat).start.column)(patSpan(lastPat).end)(next) ? ((s1) => (($match_178) => {
      if ($match_178.$tag === 1) {
        const e = $match_178.$0;
        return Err2(e);
      }
      if ($match_178.$tag === 0) {
        const pat = $match_178.$0[0];
        const s2 = $match_178.$0[1];
        return (($match_179) => {
          if ($match_179.$tag === 1) {
            const e = $match_179.$0;
            return Err2(e);
          }
          if ($match_179.$tag === 0) {
            const rest = $match_179.$0[0];
            const s3 = $match_179.$0[1];
            return Ok2([_COLON_COLON2(pat)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseTuplePatternRest(pat)(s2));
      }
      throw new Error("Pattern match failed");
    })(parsePattern(s1)))(advance2(state)) : Ok2([[], state]);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : Ok2([[], state]);
var parseLambdaArgs = (state) => (($match_180) => {
  if ($match_180.$tag === 0) {
    const s1 = $match_180.$0;
    return Ok2([[], s1]);
  }
  if ($match_180.$tag === 1) {
    return isPatternStart(current(state)) ? (($match_181) => {
      if ($match_181.$tag === 1) {
        const e = $match_181.$0;
        return Err2(e);
      }
      if ($match_181.$tag === 0) {
        const pat = $match_181.$0[0];
        const s2 = $match_181.$0[1];
        return (($match_182) => {
          if ($match_182.$tag === 1) {
            const e = $match_182.$0;
            return Err2(e);
          }
          if ($match_182.$tag === 0) {
            const rest = $match_182.$0[0];
            const s3 = $match_182.$0[1];
            return Ok2([_COLON_COLON2(pat)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseLambdaArgs(s2));
      }
      throw new Error("Pattern match failed");
    })(parsePattern(state)) : Err2(makeError("Expected lambda argument")(current(state).span));
  }
  throw new Error("Pattern match failed");
})(matchOperator("->")(state));
var parseFieldAccesses = (expr) => (state) => {
  while (true) {
    {
      const $match_183 = matchKind(Dot2)(state);
      if ($match_183.$tag === 1) {
        return Ok2([expr, state]);
      }
      if ($match_183.$tag === 0) {
        const s1 = $match_183.$0;
        {
          const tok = current(s1);
          if (_PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier2))) {
            {
              const field = tok;
              {
                const s2 = advance2(s1);
                [expr, state] = [EFieldAccess(expr)(field.lexeme)({ start: exprSpan(expr).start, end: field.span.end }), s2];
                continue;
              }
            }
          } else {
            return Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Expected field name but found ")(tokenKindStr(tok.kind)))(tok.span));
          }
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var noDefaultMethod = (name) => (typeExpr) => (span) => ({ name, methodType: typeExpr, defaultArgs: [], defaultBody: EUnit(span), hasDefault: false, hasType: true, span });
var mergeProtocolItems = (items) => (($match_184) => {
  if (Array.isArray($match_184) && $match_184.length === 0) {
    return [];
  }
  if (Array.isArray($match_184) && $match_184.length >= 1 && $match_184[0].$tag === 0) {
    const name = $match_184[0].$0;
    const typeExpr = $match_184[0].$1;
    const span = $match_184[0].$2;
    const rest = $match_184.slice(1);
    return (($match_185) => {
      if (Array.isArray($match_185) && $match_185.length >= 1 && $match_185[0].$tag === 1) {
        const implName = $match_185[0].$0;
        const args = $match_185[0].$1;
        const body = $match_185[0].$2;
        const implSpan = $match_185[0].$3;
        const rest2 = $match_185.slice(1);
        return $dict_Eq_String2._EQ_EQ(name)(implName) ? _COLON_COLON2({ name, methodType: typeExpr, defaultArgs: args, defaultBody: body, hasDefault: true, hasType: true, span: { start: span.start, end: implSpan.end } })(mergeProtocolItems(rest2)) : _COLON_COLON2(noDefaultMethod(name)(typeExpr)(span))(mergeProtocolItems(rest));
      }
      {
        return _COLON_COLON2(noDefaultMethod(name)(typeExpr)(span))(mergeProtocolItems(rest));
      }
      throw new Error("Pattern match failed");
    })(rest);
  }
  if (Array.isArray($match_184) && $match_184.length >= 1 && $match_184[0].$tag === 1) {
    const name = $match_184[0].$0;
    const args = $match_184[0].$1;
    const body = $match_184[0].$2;
    const span = $match_184[0].$3;
    const rest = $match_184.slice(1);
    return ((dummyType) => _COLON_COLON2({ name, methodType: dummyType, defaultArgs: args, defaultBody: body, hasDefault: true, hasType: false, span })(mergeProtocolItems(rest)))(TRef("Unknown")([])(span));
  }
  throw new Error("Pattern match failed");
})(items);
var parseImplementationTypeArgs = (state) => isTypeStart(current(state)) ? (($match_186) => {
  if ($match_186.$tag === 1) {
    const e = $match_186.$0;
    return Err2(e);
  }
  if ($match_186.$tag === 0) {
    const t = $match_186.$0[0];
    const s1 = $match_186.$0[1];
    return (($match_187) => {
      if ($match_187.$tag === 1) {
        const e = $match_187.$0;
        return Err2(e);
      }
      if ($match_187.$tag === 0) {
        const rest = $match_187.$0[0];
        const s2 = $match_187.$0[1];
        return Ok2([_COLON_COLON2(t)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseImplementationTypeArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok2([[], state]);
var parseInfixOperator = (state) => peekKind(LParen2)(state) ? ((s1) => (($match_188) => {
  if ($match_188.$tag === 1) {
    const e = $match_188.$0;
    return Err2(e);
  }
  if ($match_188.$tag === 0) {
    const opTok = $match_188.$0[0];
    const s2 = $match_188.$0[1];
    return (($match_189) => {
      if ($match_189.$tag === 1) {
        const e = $match_189.$0;
        return Err2(e);
      }
      if ($match_189.$tag === 0) {
        const cp = $match_189.$0[0];
        const s3 = $match_189.$0[1];
        return Ok2([{ opName: opTok.lexeme, opEnd: cp.span.end }, s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RParen2)("close paren")(s2));
  }
  throw new Error("Pattern match failed");
})(expect(Operator2)("operator")(s1)))(advance2(state)) : (($match_190) => {
  if ($match_190.$tag === 1) {
    const e = $match_190.$0;
    return Err2(e);
  }
  if ($match_190.$tag === 0) {
    const opTok = $match_190.$0[0];
    const s1 = $match_190.$0[1];
    return Ok2([{ opName: opTok.lexeme, opEnd: opTok.span.end }, s1]);
  }
  throw new Error("Pattern match failed");
})(expect(Operator2)("operator")(state));
var parseInfixDeclaration = (state) => ((fixityTok) => ((start) => ((s1) => (($match_191) => {
  if ($match_191.$tag === 1) {
    const e = $match_191.$0;
    return Err2(e);
  }
  if ($match_191.$tag === 0) {
    const precTok = $match_191.$0[0];
    const s2 = $match_191.$0[1];
    return (($match_192) => {
      if ($match_192.$tag === 1) {
        const e = $match_192.$0;
        return Err2(e);
      }
      if ($match_192.$tag === 0) {
        const opResult = $match_192.$0[0];
        const s3 = $match_192.$0[1];
        return ((assocStr) => Ok2([DInfix(assocStr)(stringToInt(precTok.lexeme))(opResult.opName)({ start, end: opResult.opEnd }), s3]))((($match_193) => {
          if ($match_193 === "infixl") {
            return "left";
          }
          if ($match_193 === "infixr") {
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
})(expect(NumberToken2)("precedence number")(s1)))(advance2(state)))(fixityTok.span.start))(current(state));
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
parseDeclaration = (state) => peekDecorator(state) ? parseDecoratedDeclaration(state) : peekKeyword("type")(state) ? parseTypeOrAliasDeclaration(state) : peekKeyword("protocol")(state) ? parseProtocolDeclaration(state) : peekKeyword("implement")(state) ? parseImplementationDeclaration(state) : _PIPE_PIPE6(peekKeyword("infix")(state))(() => _PIPE_PIPE6(peekKeyword("infixl")(state))(() => peekKeyword("infixr")(state))) ? parseInfixDeclaration(state) : parseAnnotationOrValue(state);
parseAnnotationOrValue = (state) => (($match_194) => {
  if ($match_194.$tag === 1) {
    const e = $match_194.$0;
    return Err2(e);
  }
  if ($match_194.$tag === 0) {
    const nameResult = $match_194.$0[0];
    const s1 = $match_194.$0[1];
    return peekKind(Colon2)(s1) ? ((s2) => (($match_195) => {
      if ($match_195.$tag === 1) {
        const e = $match_195.$0;
        return Err2(e);
      }
      if ($match_195.$tag === 0) {
        const annotation = $match_195.$0[0];
        const s3 = $match_195.$0[1];
        return Ok2([DTypeAnnotation({ name: nameResult.declName, annotation, span: { start: nameResult.declSpan.start, end: typeSpan(annotation).end } }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeExpression(s2)))(advance2(s1)) : (($match_196) => {
      if ($match_196.$tag === 1) {
        const e = $match_196.$0;
        return Err2(e);
      }
      if ($match_196.$tag === 0) {
        const result = $match_196.$0[0];
        const s2 = $match_196.$0[1];
        return Ok2([DValue({ name: nameResult.declName, args: result.mArgs, body: result.mBody, span: { start: nameResult.declSpan.start, end: exprSpan(result.mBody).end } }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMethodBody(Just2(nameResult.declSpan.start.column))(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclarationName(state));
parseMethodBody = (declColumn) => (state) => (($match_197) => {
  if ($match_197.$tag === 1) {
    const e = $match_197.$0;
    return Err2(e);
  }
  if ($match_197.$tag === 0) {
    const args = $match_197.$0[0];
    const s1 = $match_197.$0[1];
    return (($match_198) => {
      if ($match_198.$tag === 1) {
        const e = $match_198.$0;
        return Err2(e);
      }
      if ($match_198.$tag === 0) {
        const eqTok = $match_198.$0[0];
        const s2 = $match_198.$0[1];
        return (($match_199) => {
          if ($match_199.$tag === 1) {
            return (($match_200) => {
              if ($match_200.$tag === 1) {
                const e = $match_200.$0;
                return Err2(e);
              }
              if ($match_200.$tag === 0) {
                const body = $match_200.$0[0];
                const s3 = $match_200.$0[1];
                return Ok2([{ mArgs: args, mBody: body }, s3]);
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing2)(s2));
          }
          if ($match_199.$tag === 0) {
            const col = $match_199.$0;
            return ((bodyTok) => _AMP_AMP5($dict_Eq_Int2._SLASH_EQ(bodyTok.span.start.line)(eqTok.span.start.line))(() => $dict_Ord_Int2._LT_EQ(bodyTok.span.start.column)(col)) ? Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Function body must be indented past column ")(intToStr(col)))(bodyTok.span)) : (($match_201) => {
              if ($match_201.$tag === 1) {
                const e = $match_201.$0;
                return Err2(e);
              }
              if ($match_201.$tag === 0) {
                const body = $match_201.$0[0];
                const s3 = $match_201.$0[1];
                return Ok2([{ mArgs: args, mBody: body }, s3]);
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing2)(s2)))(current(s2));
          }
          throw new Error("Pattern match failed");
        })(declColumn);
      }
      throw new Error("Pattern match failed");
    })(expect(Equals2)("'=' after parameters")(s1));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParams(state));
parseProtocolDeclaration = (state) => (($match_202) => {
  if ($match_202.$tag === 1) {
    const e = $match_202.$0;
    return Err2(e);
  }
  if ($match_202.$tag === 0) {
    const protocolTok = $match_202.$0[0];
    const s1 = $match_202.$0[1];
    return ((hasConstraints) => (($match_203) => {
      if ($match_203.$tag === 1) {
        const e = $match_203.$0;
        return Err2(e);
      }
      if ($match_203.$tag === 0) {
        const constraints = $match_203.$0[0];
        const s2 = $match_203.$0[1];
        return (($match_204) => {
          if ($match_204.$tag === 1) {
            const e = $match_204.$0;
            return Err2(e);
          }
          if ($match_204.$tag === 0) {
            const nameTok = $match_204.$0[0];
            const s3 = $match_204.$0[1];
            return (($match_205) => {
              if ($match_205.$tag === 1) {
                const e = $match_205.$0;
                return Err2(e);
              }
              if ($match_205.$tag === 0) {
                const params = $match_205.$0[0];
                const s4 = $match_205.$0[1];
                return (($match_206) => {
                  if ($match_206.$tag === 1) {
                    const e = $match_206.$0;
                    return Err2(e);
                  }
                  if ($match_206.$tag === 0) {
                    const s5 = $match_206.$0[1];
                    return (($match_207) => {
                      if ($match_207.$tag === 1) {
                        const e = $match_207.$0;
                        return Err2(e);
                      }
                      if ($match_207.$tag === 0) {
                        const s6 = $match_207.$0[1];
                        return (($match_208) => {
                          if ($match_208.$tag === 1) {
                            const e = $match_208.$0;
                            return Err2(e);
                          }
                          if ($match_208.$tag === 0) {
                            const items = $match_208.$0[0];
                            const s7 = $match_208.$0[1];
                            return (($match_209) => {
                              if ($match_209.$tag === 1) {
                                const e = $match_209.$0;
                                return Err2(e);
                              }
                              if ($match_209.$tag === 0) {
                                const s8 = $match_209.$0[1];
                                return ((methods) => ((endPos) => Ok2([DProtocol(nameTok.lexeme)(params)(constraints)(methods)({ start: protocolTok.span.start, end: endPos }), s8]))((($match_210) => {
                                  if ($match_210.$tag === 0) {
                                    const m = $match_210.$0;
                                    return m.span.end;
                                  }
                                  if ($match_210.$tag === 1) {
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
        })(expect(UpperIdentifier2)("protocol name")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseOptionalConstraints(hasConstraints)(s1)))(peekConstraintContextInProtocol(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("protocol")(state));
parseProtocolItems = (state) => (($match_211) => {
  if ($match_211.$tag === 1) {
    const e = $match_211.$0;
    return Err2(e);
  }
  if ($match_211.$tag === 0) {
    const item = $match_211.$0[0];
    const s1 = $match_211.$0[1];
    return (($match_212) => {
      if ($match_212.$tag === 1) {
        return Ok2([[item], s1]);
      }
      if ($match_212.$tag === 0) {
        const s2 = $match_212.$0;
        return (($match_213) => {
          if ($match_213.$tag === 1) {
            const e = $match_213.$0;
            return Err2(e);
          }
          if ($match_213.$tag === 0) {
            const rest = $match_213.$0[0];
            const s3 = $match_213.$0[1];
            return Ok2([_COLON_COLON2(item)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseProtocolItems(s2));
      }
      throw new Error("Pattern match failed");
    })(matchBlockSep(s1));
  }
  throw new Error("Pattern match failed");
})(parseProtocolItem(state));
parseProtocolItem = (state) => ((start) => (($match_214) => {
  if ($match_214.$tag === 1) {
    const e = $match_214.$0;
    return Err2(e);
  }
  if ($match_214.$tag === 0) {
    const methodName = $match_214.$0[0];
    const s1 = $match_214.$0[1];
    return peekKind(Colon2)(s1) ? ((s2) => (($match_215) => {
      if ($match_215.$tag === 1) {
        const e = $match_215.$0;
        return Err2(e);
      }
      if ($match_215.$tag === 0) {
        const methodType = $match_215.$0[0];
        const s3 = $match_215.$0[1];
        return Ok2([PIAnnotation(methodName)(methodType)({ start, end: typeSpan(methodType).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeExpression(s2)))(advance2(s1)) : (($match_216) => {
      if ($match_216.$tag === 1) {
        const e = $match_216.$0;
        return Err2(e);
      }
      if ($match_216.$tag === 0) {
        const args = $match_216.$0[0];
        const s2 = $match_216.$0[1];
        return (($match_217) => {
          if ($match_217.$tag === 1) {
            const e = $match_217.$0;
            return Err2(e);
          }
          if ($match_217.$tag === 0) {
            const s3 = $match_217.$0[1];
            return (($match_218) => {
              if ($match_218.$tag === 1) {
                const e = $match_218.$0;
                return Err2(e);
              }
              if ($match_218.$tag === 0) {
                const body = $match_218.$0[0];
                const s4 = $match_218.$0[1];
                return Ok2([PIImpl(methodName)(args)(body)({ start, end: exprSpan(body).end }), s4]);
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing2)(s3));
          }
          throw new Error("Pattern match failed");
        })(expect(Equals2)("'=' after parameters")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParams(s1));
  }
  throw new Error("Pattern match failed");
})(parseProtocolMethodName(state)))(current(state).span.start);
parseImplementationDeclaration = (state) => (($match_219) => {
  if ($match_219.$tag === 1) {
    const e = $match_219.$0;
    return Err2(e);
  }
  if ($match_219.$tag === 0) {
    const implTok = $match_219.$0[0];
    const s1 = $match_219.$0[1];
    return ((hasConstraints) => (($match_220) => {
      if ($match_220.$tag === 1) {
        const e = $match_220.$0;
        return Err2(e);
      }
      if ($match_220.$tag === 0) {
        const constraints = $match_220.$0[0];
        const s2 = $match_220.$0[1];
        return (($match_221) => {
          if ($match_221.$tag === 1) {
            const e = $match_221.$0;
            return Err2(e);
          }
          if ($match_221.$tag === 0) {
            const protocolTok = $match_221.$0[0];
            const s3 = $match_221.$0[1];
            return (($match_222) => {
              if ($match_222.$tag === 1) {
                const e = $match_222.$0;
                return Err2(e);
              }
              if ($match_222.$tag === 0) {
                const typeArgs = $match_222.$0[0];
                const s4 = $match_222.$0[1];
                return (($match_223) => {
                  if ($match_223.$tag === 1) {
                    const e = $match_223.$0;
                    return Err2(e);
                  }
                  if ($match_223.$tag === 0) {
                    const methods = $match_223.$0[0];
                    const s5 = $match_223.$0[1];
                    return ((endPos) => Ok2([DImplementation(constraints)(protocolTok.lexeme)(typeArgs)(methods)({ start: implTok.span.start, end: endPos }), s5]))((($match_224) => {
                      if ($match_224.$tag === 0) {
                        const m = $match_224.$0;
                        return m.span.end;
                      }
                      if ($match_224.$tag === 1) {
                        return (($match_225) => {
                          if ($match_225.$tag === 0) {
                            const t = $match_225.$0;
                            return typeSpan(t).end;
                          }
                          if ($match_225.$tag === 1) {
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
        })(expect(UpperIdentifier2)("protocol name")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseOptionalConstraints(hasConstraints)(s1)))(peekConstraintContext(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("implement")(state));
parseImplementationMethods = (state) => peekKeyword("where")(state) ? (($match_226) => {
  if ($match_226.$tag === 1) {
    const e = $match_226.$0;
    return Err2(e);
  }
  if ($match_226.$tag === 0) {
    const s1 = $match_226.$0[1];
    return (($match_227) => {
      if ($match_227.$tag === 1) {
        const e = $match_227.$0;
        return Err2(e);
      }
      if ($match_227.$tag === 0) {
        const s2 = $match_227.$0[1];
        return (($match_228) => {
          if ($match_228.$tag === 1) {
            const e = $match_228.$0;
            return Err2(e);
          }
          if ($match_228.$tag === 0) {
            const methods = $match_228.$0[0];
            const s3 = $match_228.$0[1];
            return (($match_229) => {
              if ($match_229.$tag === 1) {
                const e = $match_229.$0;
                return Err2(e);
              }
              if ($match_229.$tag === 0) {
                const s4 = $match_229.$0[1];
                return Ok2([methods, s4]);
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
})(expectKeyword("where")(state)) : Ok2([[], state]);
parseImplMethodList = (state) => (($match_230) => {
  if ($match_230.$tag === 1) {
    const e = $match_230.$0;
    return Err2(e);
  }
  if ($match_230.$tag === 0) {
    const method = $match_230.$0[0];
    const s1 = $match_230.$0[1];
    return (($match_231) => {
      if ($match_231.$tag === 1) {
        return Ok2([[method], s1]);
      }
      if ($match_231.$tag === 0) {
        const s2 = $match_231.$0;
        return (($match_232) => {
          if ($match_232.$tag === 1) {
            const e = $match_232.$0;
            return Err2(e);
          }
          if ($match_232.$tag === 0) {
            const rest = $match_232.$0[0];
            const s3 = $match_232.$0[1];
            return Ok2([_COLON_COLON2(method)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseImplMethodList(s2));
      }
      throw new Error("Pattern match failed");
    })(matchBlockSep(s1));
  }
  throw new Error("Pattern match failed");
})(parseImplMethod(state));
parseImplMethod = (state) => (($match_233) => {
  if ($match_233.$tag === 1) {
    const e = $match_233.$0;
    return Err2(e);
  }
  if ($match_233.$tag === 0) {
    const nameResult = $match_233.$0[0];
    const s1 = $match_233.$0[1];
    return (($match_234) => {
      if ($match_234.$tag === 1) {
        const e = $match_234.$0;
        return Err2(e);
      }
      if ($match_234.$tag === 0) {
        const result = $match_234.$0[0];
        const s2 = $match_234.$0[1];
        return Ok2([{ name: nameResult.declName, implArgs: result.mArgs, implementation: result.mBody, span: { start: nameResult.declSpan.start, end: exprSpan(result.mBody).end } }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMethodBody(Nothing2)(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclarationName(state));
parseExpression = (baseIndentFloor) => (state) => parseBinaryExpression(0)(baseIndentFloor)(state);
parseBinaryExpression = (minPrec) => (baseIndentFloor) => (state) => (($match_235) => {
  if ($match_235.$tag === 1) {
    const e = $match_235.$0;
    return Err2(e);
  }
  if ($match_235.$tag === 0) {
    const left = $match_235.$0[0];
    const s1 = $match_235.$0[1];
    return parseBinExprLoop(left)(minPrec)(baseIndentFloor)(s1);
  }
  throw new Error("Pattern match failed");
})(parseUnary(baseIndentFloor)(state));
parseBinExprLoop = (left) => (minPrec) => (baseIndentFloor) => (state) => {
  while (true) {
    {
      const opTok = current(state);
      if ($dict_Eq_TokenKind._EQ_EQ(opTok.kind)(Operator2)) {
        if ($dict_Eq_String2._EQ_EQ(opTok.lexeme)("@")) {
          return Ok2([left, state]);
        } else {
          {
            const info = getOperatorInfo(opTok.lexeme)(state.registry);
            {
              const prec = info.precedence;
              if ($dict_Ord_Int2._LT(prec)(minPrec)) {
                return Ok2([left, state]);
              } else {
                {
                  const s1 = advance2(state);
                  {
                    const nextMin = (($match_236) => {
                      if ($match_236.$tag === 1) {
                        return prec;
                      }
                      {
                        return $dict_Num_Int2._PLUS(prec)(1);
                      }
                      throw new Error("Pattern match failed");
                    })(info.associativity);
                    {
                      const $match_237 = parseBinaryExpression(nextMin)(baseIndentFloor)(s1);
                      if ($match_237.$tag === 1) {
                        const e = $match_237.$0;
                        return Err2(e);
                      }
                      if ($match_237.$tag === 0) {
                        const right = $match_237.$0[0];
                        const s2 = $match_237.$0[1];
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
        return Ok2([left, state]);
      }
    }
  }
};
parseUnary = (baseIndentFloor) => (state) => ((tok) => _AMP_AMP5($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("-")) ? (($match_238) => {
  if ($match_238.$tag === 1) {
    return parseApplication(baseIndentFloor)(state);
  }
  if ($match_238.$tag === 0) {
    const nextTok = $match_238.$0;
    return _PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(LowerIdentifier2))(() => _PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(UpperIdentifier2))(() => _PIPE_PIPE6($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(NumberToken2))(() => $dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(LParen2)))) ? ((s1) => (($match_239) => {
      if ($match_239.$tag === 1) {
        const e = $match_239.$0;
        return Err2(e);
      }
      if ($match_239.$tag === 0) {
        const operand = $match_239.$0[0];
        const s2 = $match_239.$0[1];
        return Ok2([EUnary("-")(operand)({ start: tok.span.start, end: exprSpan(operand).end }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseUnary(baseIndentFloor)(s1)))(advance2(state)) : parseApplication(baseIndentFloor)(state);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : parseApplication(baseIndentFloor)(state))(current(state));
parseApplication = (baseIndentFloor) => (state) => (($match_240) => {
  if ($match_240.$tag === 1) {
    const e = $match_240.$0;
    return Err2(e);
  }
  if ($match_240.$tag === 0) {
    const callee = $match_240.$0[0];
    const s1 = $match_240.$0[1];
    return ((baseIndent) => ((effectiveIndent) => (($match_241) => {
      if ($match_241.$tag === 1) {
        const e = $match_241.$0;
        return Err2(e);
      }
      if ($match_241.$tag === 0) {
        const args = $match_241.$0[0];
        const s2 = $match_241.$0[1];
        return isEmpty2(args) ? Ok2([callee, s2]) : ((endPos) => Ok2([EApply(callee)(args)({ start: exprSpan(callee).start, end: endPos }), s2]))((($match_242) => {
          if ($match_242.$tag === 0) {
            const a = $match_242.$0;
            return exprSpan(a).end;
          }
          if ($match_242.$tag === 1) {
            return exprSpan(callee).end;
          }
          throw new Error("Pattern match failed");
        })(last(args)));
      }
      throw new Error("Pattern match failed");
    })(parseApplicationArgs(effectiveIndent)(exprSpan(callee).end)(s1)))((($match_243) => {
      if ($match_243.$tag === 1) {
        return baseIndent;
      }
      if ($match_243.$tag === 0) {
        const floor = $match_243.$0;
        return min(baseIndent)(floor);
      }
      throw new Error("Pattern match failed");
    })(baseIndentFloor)))(exprSpan(callee).start.column);
  }
  throw new Error("Pattern match failed");
})(parsePrimaryWithAccess(state));
parseApplicationArgs = (effectiveIndent) => (lastEnd) => (state) => isExpressionStart(current(state)) ? continuesLayout(effectiveIndent)(lastEnd)(current(state)) ? (($match_244) => {
  if ($match_244.$tag === 1) {
    const e = $match_244.$0;
    return Err2(e);
  }
  if ($match_244.$tag === 0) {
    const arg = $match_244.$0[0];
    const s1 = $match_244.$0[1];
    return (($match_245) => {
      if ($match_245.$tag === 1) {
        const e = $match_245.$0;
        return Err2(e);
      }
      if ($match_245.$tag === 0) {
        const rest = $match_245.$0[0];
        const s2 = $match_245.$0[1];
        return Ok2([_COLON_COLON2(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseApplicationArgs(effectiveIndent)(exprSpan(arg).end)(s1));
  }
  throw new Error("Pattern match failed");
})(parsePrimaryWithAccess(state)) : Ok2([[], state]) : Ok2([[], state]);
parsePrimaryWithAccess = (state) => (($match_246) => {
  if ($match_246.$tag === 1) {
    const e = $match_246.$0;
    return Err2(e);
  }
  if ($match_246.$tag === 0) {
    const expr = $match_246.$0[0];
    const s1 = $match_246.$0[1];
    return parseFieldAccesses(expr)(s1);
  }
  throw new Error("Pattern match failed");
})(parsePrimary(state));
parsePrimary = (state) => ((tok) => peekKeyword("if")(state) ? parseIf(state) : peekKeyword("let")(state) ? parseLetIn(state) : peekKeyword("case")(state) ? parseCase(state) : (($match_247) => {
  if ($match_247.$tag === 8) {
    return parseLambda(state);
  }
  if ($match_247.$tag === 0) {
    return ((s1) => Ok2([EVar(tok.lexeme)("lower")(tok.span), s1]))(advance2(state));
  }
  if ($match_247.$tag === 1) {
    return ((s1) => Ok2([EVar(tok.lexeme)("upper")(tok.span), s1]))(advance2(state));
  }
  if ($match_247.$tag === 3) {
    return ((s1) => contains(".")(tok.lexeme) ? Ok2([EFloat(tok.lexeme)(tok.span), s1]) : Ok2([EInt(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_247.$tag === 4) {
    return ((s1) => Ok2([EString(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_247.$tag === 5) {
    return ((s1) => Ok2([EChar(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_247.$tag === 9) {
    return parseParenExpr(state);
  }
  if ($match_247.$tag === 13) {
    return parseListExpr(state);
  }
  if ($match_247.$tag === 11) {
    return parseRecordExpr(state);
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected expression but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseLambda = (state) => ((start) => ((lambdaCol) => ((s1) => (($match_248) => {
  if ($match_248.$tag === 1) {
    const e = $match_248.$0;
    return Err2(e);
  }
  if ($match_248.$tag === 0) {
    const args = $match_248.$0[0];
    const s2 = $match_248.$0[1];
    return ((bodyTok) => _AMP_AMP5($dict_Eq_Int2._SLASH_EQ(bodyTok.span.start.line)(previous(s2).span.start.line))(() => $dict_Ord_Int2._LT_EQ(bodyTok.span.start.column)(lambdaCol)) ? Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Lambda body must be indented past column ")(intToStr(lambdaCol)))(bodyTok.span)) : (($match_249) => {
      if ($match_249.$tag === 1) {
        const e = $match_249.$0;
        return Err2(e);
      }
      if ($match_249.$tag === 0) {
        const body = $match_249.$0[0];
        const s3 = $match_249.$0[1];
        return Ok2([ELambda(args)(body)({ start, end: exprSpan(body).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Nothing2)(s2)))(current(s2));
  }
  throw new Error("Pattern match failed");
})(parseLambdaArgs(s1)))(advance2(state)))(start.column))(current(state).span.start);
parseIf = (state) => (($match_250) => {
  if ($match_250.$tag === 1) {
    const e = $match_250.$0;
    return Err2(e);
  }
  if ($match_250.$tag === 0) {
    const ifTok = $match_250.$0[0];
    const s1 = $match_250.$0[1];
    return (($match_251) => {
      if ($match_251.$tag === 1) {
        const e = $match_251.$0;
        return Err2(e);
      }
      if ($match_251.$tag === 0) {
        const condition = $match_251.$0[0];
        const s2 = $match_251.$0[1];
        return (($match_252) => {
          if ($match_252.$tag === 1) {
            const e = $match_252.$0;
            return Err2(e);
          }
          if ($match_252.$tag === 0) {
            const s3 = $match_252.$0[1];
            return (($match_253) => {
              if ($match_253.$tag === 1) {
                const e = $match_253.$0;
                return Err2(e);
              }
              if ($match_253.$tag === 0) {
                const thenBranch = $match_253.$0[0];
                const s4 = $match_253.$0[1];
                return (($match_254) => {
                  if ($match_254.$tag === 1) {
                    const e = $match_254.$0;
                    return Err2(e);
                  }
                  if ($match_254.$tag === 0) {
                    const s5 = $match_254.$0[1];
                    return (($match_255) => {
                      if ($match_255.$tag === 1) {
                        const e = $match_255.$0;
                        return Err2(e);
                      }
                      if ($match_255.$tag === 0) {
                        const elseBranch = $match_255.$0[0];
                        const s6 = $match_255.$0[1];
                        return Ok2([EIf(condition)(thenBranch)(elseBranch)({ start: ifTok.span.start, end: exprSpan(elseBranch).end }), s6]);
                      }
                      throw new Error("Pattern match failed");
                    })(parseExpression(Nothing2)(s5));
                  }
                  throw new Error("Pattern match failed");
                })(expectKeyword("else")(s4));
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing2)(s3));
          }
          throw new Error("Pattern match failed");
        })(expectKeyword("then")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Nothing2)(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("if")(state));
parseLetIn = (state) => (($match_256) => {
  if ($match_256.$tag === 1) {
    const e = $match_256.$0;
    return Err2(e);
  }
  if ($match_256.$tag === 0) {
    const letTok = $match_256.$0[0];
    const s1 = $match_256.$0[1];
    return (($match_257) => {
      if ($match_257.$tag === 1) {
        const e = $match_257.$0;
        return Err2(e);
      }
      if ($match_257.$tag === 0) {
        const s2 = $match_257.$0[1];
        return (($match_258) => {
          if ($match_258.$tag === 1) {
            const e = $match_258.$0;
            return Err2(e);
          }
          if ($match_258.$tag === 0) {
            const bindings = $match_258.$0[0];
            const s3 = $match_258.$0[1];
            return (($match_259) => {
              if ($match_259.$tag === 1) {
                const e = $match_259.$0;
                return Err2(e);
              }
              if ($match_259.$tag === 0) {
                const s4 = $match_259.$0[1];
                return (($match_260) => {
                  if ($match_260.$tag === 1) {
                    const e = $match_260.$0;
                    return Err2(e);
                  }
                  if ($match_260.$tag === 0) {
                    const s5 = $match_260.$0[1];
                    return (($match_261) => {
                      if ($match_261.$tag === 1) {
                        const e = $match_261.$0;
                        return Err2(e);
                      }
                      if ($match_261.$tag === 0) {
                        const body = $match_261.$0[0];
                        const s6 = $match_261.$0[1];
                        return Ok2([ELetIn(bindings)(body)({ start: letTok.span.start, end: exprSpan(body).end }), s6]);
                      }
                      throw new Error("Pattern match failed");
                    })(parseExpression(Nothing2)(s5));
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
parseLetBindings = (state) => (($match_262) => {
  if ($match_262.$tag === 1) {
    const e = $match_262.$0;
    return Err2(e);
  }
  if ($match_262.$tag === 0) {
    const binding = $match_262.$0[0];
    const s1 = $match_262.$0[1];
    return (($match_263) => {
      if ($match_263.$tag === 1) {
        return Ok2([[binding], s1]);
      }
      if ($match_263.$tag === 0) {
        const s2 = $match_263.$0;
        return (($match_264) => {
          if ($match_264.$tag === 1) {
            const e = $match_264.$0;
            return Err2(e);
          }
          if ($match_264.$tag === 0) {
            const rest = $match_264.$0[0];
            const s3 = $match_264.$0[1];
            return Ok2([_COLON_COLON2(binding)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseLetBindings(s2));
      }
      throw new Error("Pattern match failed");
    })(matchBlockSep(s1));
  }
  throw new Error("Pattern match failed");
})(parseLetBinding(state));
parseLetBinding = (state) => (($match_265) => {
  if ($match_265.$tag === 1) {
    const e = $match_265.$0;
    return Err2(e);
  }
  if ($match_265.$tag === 0) {
    const decl = $match_265.$0[0];
    const s1 = $match_265.$0[1];
    return Ok2([declToValueDecl(decl), s1]);
  }
  throw new Error("Pattern match failed");
})(parseDeclaration(state));
parseCase = (state) => (($match_266) => {
  if ($match_266.$tag === 1) {
    const e = $match_266.$0;
    return Err2(e);
  }
  if ($match_266.$tag === 0) {
    const caseTok = $match_266.$0[0];
    const s1 = $match_266.$0[1];
    return (($match_267) => {
      if ($match_267.$tag === 1) {
        const e = $match_267.$0;
        return Err2(e);
      }
      if ($match_267.$tag === 0) {
        const discriminant = $match_267.$0[0];
        const s2 = $match_267.$0[1];
        return (($match_268) => {
          if ($match_268.$tag === 1) {
            const e = $match_268.$0;
            return Err2(e);
          }
          if ($match_268.$tag === 0) {
            const s3 = $match_268.$0[1];
            return (($match_269) => {
              if ($match_269.$tag === 1) {
                const e = $match_269.$0;
                return Err2(e);
              }
              if ($match_269.$tag === 0) {
                const s4 = $match_269.$0[1];
                return (($match_270) => {
                  if ($match_270.$tag === 1) {
                    const e = $match_270.$0;
                    return Err2(e);
                  }
                  if ($match_270.$tag === 0) {
                    const branches = $match_270.$0[0];
                    const s5 = $match_270.$0[1];
                    return (($match_271) => {
                      if ($match_271.$tag === 1) {
                        const e = $match_271.$0;
                        return Err2(e);
                      }
                      if ($match_271.$tag === 0) {
                        const s6 = $match_271.$0[1];
                        return ((endPos) => Ok2([ECase(discriminant)(branches)({ start: caseTok.span.start, end: endPos }), s6]))((($match_272) => {
                          if ($match_272.$tag === 0) {
                            const b = $match_272.$0;
                            return b.span.end;
                          }
                          if ($match_272.$tag === 1) {
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
    })(parseExpression(Nothing2)(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("case")(state));
parseCaseBranches = (state) => (($match_273) => {
  if ($match_273.$tag === 1) {
    const e = $match_273.$0;
    return Err2(e);
  }
  if ($match_273.$tag === 0) {
    const branch = $match_273.$0[0];
    const s1 = $match_273.$0[1];
    return (($match_274) => {
      if ($match_274.$tag === 1) {
        return Ok2([[branch], s1]);
      }
      if ($match_274.$tag === 0) {
        const s2 = $match_274.$0;
        return (($match_275) => {
          if ($match_275.$tag === 1) {
            const e = $match_275.$0;
            return Err2(e);
          }
          if ($match_275.$tag === 0) {
            const rest = $match_275.$0[0];
            const s3 = $match_275.$0[1];
            return Ok2([_COLON_COLON2(branch)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseCaseBranches(s2));
      }
      throw new Error("Pattern match failed");
    })(matchBlockSep(s1));
  }
  throw new Error("Pattern match failed");
})(parseCaseBranch(state));
parseCaseBranch = (state) => (($match_276) => {
  if ($match_276.$tag === 1) {
    const e = $match_276.$0;
    return Err2(e);
  }
  if ($match_276.$tag === 0) {
    const pat = $match_276.$0[0];
    const s1 = $match_276.$0[1];
    return (($match_277) => {
      if ($match_277.$tag === 1) {
        const e = $match_277.$0;
        return Err2(e);
      }
      if ($match_277.$tag === 0) {
        const s2 = $match_277.$0[1];
        return ((branchIndent) => (($match_278) => {
          if ($match_278.$tag === 1) {
            const e = $match_278.$0;
            return Err2(e);
          }
          if ($match_278.$tag === 0) {
            const body = $match_278.$0[0];
            const s3 = $match_278.$0[1];
            return Ok2([{ pattern: pat, body, span: { start: patSpan(pat).start, end: exprSpan(body).end } }, s3]);
          }
          throw new Error("Pattern match failed");
        })(parseExpression(Just2(branchIndent))(s2)))($dict_Num_Int2._PLUS(patSpan(pat).start.column)(1));
      }
      throw new Error("Pattern match failed");
    })(expectOperator("->")(s1));
  }
  throw new Error("Pattern match failed");
})(parsePattern(state));
parseParenExpr = (state) => ((start) => ((s1) => peekKind(RParen2)(s1) ? ((s2) => Ok2([EUnit({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : peekKind(Operator2)(s1) ? (($match_279) => {
  if ($match_279.$tag === 0) {
    const next = $match_279.$0;
    return $dict_Eq_TokenKind._EQ_EQ(next.kind)(RParen2) ? ((opTok) => ((s2) => ((s3) => Ok2([EVar(opTok.lexeme)("operator")({ start, end: previous(s3).span.end }), s3]))(advance2(s2)))(advance2(s1)))(current(s1)) : parseParenExprContents(start)(s1);
  }
  if ($match_279.$tag === 1) {
    return parseParenExprContents(start)(s1);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(s1)) : parseParenExprContents(start)(s1))(advance2(state)))(current(state).span.start);
parseParenExprContents = (start) => (state) => (($match_280) => {
  if ($match_280.$tag === 1) {
    const e = $match_280.$0;
    return Err2(e);
  }
  if ($match_280.$tag === 0) {
    const first = $match_280.$0[0];
    const s1 = $match_280.$0[1];
    return (($match_281) => {
      if ($match_281.$tag === 1) {
        const e = $match_281.$0;
        return Err2(e);
      }
      if ($match_281.$tag === 0) {
        const more = $match_281.$0[0];
        const s2 = $match_281.$0[1];
        return (($match_282) => {
          if ($match_282.$tag === 1) {
            const e = $match_282.$0;
            return Err2(e);
          }
          if ($match_282.$tag === 0) {
            const cp = $match_282.$0[0];
            const s3 = $match_282.$0[1];
            return isEmpty2(more) ? Ok2([EParen(first)({ start, end: cp.span.end }), s3]) : Ok2([ETuple(_COLON_COLON2(first)(more))({ start, end: cp.span.end }), s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close group")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleExprs(first)(s1));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing2)(state));
parseMoreTupleExprs = (lastExpr) => (state) => peekKind(Comma2)(state) ? (($match_283) => {
  if ($match_283.$tag === 1) {
    return Ok2([[], state]);
  }
  if ($match_283.$tag === 0) {
    const next = $match_283.$0;
    return continuesLayout(exprSpan(lastExpr).start.column)(exprSpan(lastExpr).end)(next) ? ((s1) => (($match_284) => {
      if ($match_284.$tag === 1) {
        const e = $match_284.$0;
        return Err2(e);
      }
      if ($match_284.$tag === 0) {
        const expr = $match_284.$0[0];
        const s2 = $match_284.$0[1];
        return (($match_285) => {
          if ($match_285.$tag === 1) {
            const e2 = $match_285.$0;
            return Err2(e2);
          }
          if ($match_285.$tag === 0) {
            const rest = $match_285.$0[0];
            const s3 = $match_285.$0[1];
            return Ok2([_COLON_COLON2(expr)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseMoreTupleExprs(expr)(s2));
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Nothing2)(s1)))(advance2(state)) : Ok2([[], state]);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : Ok2([[], state]);
parseListExpr = (state) => ((start) => ((s1) => peekKind(RBracket2)(s1) ? ((s2) => Ok2([EList([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_286) => {
  if ($match_286.$tag === 1) {
    const e = $match_286.$0;
    return Err2(e);
  }
  if ($match_286.$tag === 0) {
    const first = $match_286.$0[0];
    const s2 = $match_286.$0[1];
    return (($match_287) => {
      if ($match_287.$tag === 0) {
        const s3 = $match_287.$0;
        return (($match_288) => {
          if ($match_288.$tag === 1) {
            const e = $match_288.$0;
            return Err2(e);
          }
          if ($match_288.$tag === 0) {
            const endExpr = $match_288.$0[0];
            const s4 = $match_288.$0[1];
            return (($match_289) => {
              if ($match_289.$tag === 1) {
                const e = $match_289.$0;
                return Err2(e);
              }
              if ($match_289.$tag === 0) {
                const s5 = $match_289.$0[1];
                return Ok2([EListRange(first)(endExpr)({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBracket2)("close list range")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseExpression(Nothing2)(s3));
      }
      if ($match_287.$tag === 1) {
        return (($match_290) => {
          if ($match_290.$tag === 1) {
            const e = $match_290.$0;
            return Err2(e);
          }
          if ($match_290.$tag === 0) {
            const more = $match_290.$0[0];
            const s4 = $match_290.$0[1];
            return (($match_291) => {
              if ($match_291.$tag === 1) {
                const e = $match_291.$0;
                return Err2(e);
              }
              if ($match_291.$tag === 0) {
                const s5 = $match_291.$0[1];
                return Ok2([EList(_COLON_COLON2(first)(more))({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBracket2)("close list")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseMoreListExprs(first)(s2));
      }
      throw new Error("Pattern match failed");
    })(matchKind(Range2)(s2));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing2)(s1)))(advance2(state)))(current(state).span.start);
parseMoreListExprs = (lastExpr) => (state) => peekKind(Comma2)(state) ? (($match_292) => {
  if ($match_292.$tag === 1) {
    return Ok2([[], state]);
  }
  if ($match_292.$tag === 0) {
    const next = $match_292.$0;
    return continuesLayout(exprSpan(lastExpr).start.column)(exprSpan(lastExpr).end)(next) ? ((s1) => (($match_293) => {
      if ($match_293.$tag === 1) {
        const e = $match_293.$0;
        return Err2(e);
      }
      if ($match_293.$tag === 0) {
        const expr = $match_293.$0[0];
        const s2 = $match_293.$0[1];
        return (($match_294) => {
          if ($match_294.$tag === 1) {
            const e2 = $match_294.$0;
            return Err2(e2);
          }
          if ($match_294.$tag === 0) {
            const rest = $match_294.$0[0];
            const s3 = $match_294.$0[1];
            return Ok2([_COLON_COLON2(expr)(rest), s3]);
          }
          throw new Error("Pattern match failed");
        })(parseMoreListExprs(expr)(s2));
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Nothing2)(s1)))(advance2(state)) : Ok2([[], state]);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : Ok2([[], state]);
parseRecordExpr = (state) => ((start) => ((s1) => peekKind(RBrace2)(s1) ? ((s2) => Ok2([ERecord([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_295) => {
  if ($match_295.$tag === 1) {
    const e = $match_295.$0;
    return Err2(e);
  }
  if ($match_295.$tag === 0) {
    const head2 = $match_295.$0[0];
    const s2 = $match_295.$0[1];
    return (($match_296) => {
      if ($match_296.$tag === 0) {
        const s3 = $match_296.$0;
        return (($match_297) => {
          if ($match_297.$tag === 1) {
            const e = $match_297.$0;
            return Err2(e);
          }
          if ($match_297.$tag === 0) {
            const fields = $match_297.$0[0];
            const s4 = $match_297.$0[1];
            return (($match_298) => {
              if ($match_298.$tag === 1) {
                const e = $match_298.$0;
                return Err2(e);
              }
              if ($match_298.$tag === 0) {
                const s5 = $match_298.$0[1];
                return Ok2([ERecordUpdate(head2.lexeme)(fields)({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBrace2)("close record update")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldList(Nothing2)(head2.span.start.column)(s3));
      }
      if ($match_296.$tag === 1) {
        return (($match_299) => {
          if ($match_299.$tag === 1) {
            const e = $match_299.$0;
            return Err2(e);
          }
          if ($match_299.$tag === 0) {
            const fields = $match_299.$0[0];
            const s4 = $match_299.$0[1];
            return (($match_300) => {
              if ($match_300.$tag === 1) {
                const e = $match_300.$0;
                return Err2(e);
              }
              if ($match_300.$tag === 0) {
                const s5 = $match_300.$0[1];
                return Ok2([ERecord(fields)({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBrace2)("close record")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldList(Just2(head2))(head2.span.start.column)(s2));
      }
      throw new Error("Pattern match failed");
    })(matchKind(Pipe2)(s2));
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier2)("record field or base")(s1)))(advance2(state)))(current(state).span.start);
parseRecordFieldList = (firstFieldToken) => (baseIndent) => (state) => (($match_301) => {
  if ($match_301.$tag === 0) {
    const tok = $match_301.$0;
    return (($match_302) => {
      if ($match_302.$tag === 1) {
        const e = $match_302.$0;
        return Err2(e);
      }
      if ($match_302.$tag === 0) {
        const firstField = $match_302.$0[0];
        const s1 = $match_302.$0[1];
        return (($match_303) => {
          if ($match_303.$tag === 1) {
            const e = $match_303.$0;
            return Err2(e);
          }
          if ($match_303.$tag === 0) {
            const rest = $match_303.$0[0];
            const s2 = $match_303.$0[1];
            return Ok2([_COLON_COLON2(firstField)(rest), s2]);
          }
          throw new Error("Pattern match failed");
        })(parseMoreRecordFields(baseIndent)(firstField)(s1));
      }
      throw new Error("Pattern match failed");
    })(parseRecordFieldValue(tok)(baseIndent)(state));
  }
  if ($match_301.$tag === 1) {
    return (($match_304) => {
      if ($match_304.$tag === 1) {
        const e = $match_304.$0;
        return Err2(e);
      }
      if ($match_304.$tag === 0) {
        const tok = $match_304.$0[0];
        const s1 = $match_304.$0[1];
        return (($match_305) => {
          if ($match_305.$tag === 1) {
            const e = $match_305.$0;
            return Err2(e);
          }
          if ($match_305.$tag === 0) {
            const firstField = $match_305.$0[0];
            const s2 = $match_305.$0[1];
            return (($match_306) => {
              if ($match_306.$tag === 1) {
                const e = $match_306.$0;
                return Err2(e);
              }
              if ($match_306.$tag === 0) {
                const rest = $match_306.$0[0];
                const s3 = $match_306.$0[1];
                return Ok2([_COLON_COLON2(firstField)(rest), s3]);
              }
              throw new Error("Pattern match failed");
            })(parseMoreRecordFields(baseIndent)(firstField)(s2));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldValue(tok)(baseIndent)(s1));
      }
      throw new Error("Pattern match failed");
    })(expect(LowerIdentifier2)("record field name")(state));
  }
  throw new Error("Pattern match failed");
})(firstFieldToken);
parseRecordFieldValue = (fieldTok) => (baseIndent) => (state) => (($match_307) => {
  if ($match_307.$tag === 1) {
    const e = $match_307.$0;
    return Err2(e);
  }
  if ($match_307.$tag === 0) {
    const s1 = $match_307.$0[1];
    return (($match_308) => {
      if ($match_308.$tag === 1) {
        const e = $match_308.$0;
        return Err2(e);
      }
      if ($match_308.$tag === 0) {
        const value = $match_308.$0[0];
        const s2 = $match_308.$0[1];
        return Ok2([{ name: fieldTok.lexeme, value, span: { start: fieldTok.span.start, end: exprSpan(value).end } }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Just2(baseIndent))(s1));
  }
  throw new Error("Pattern match failed");
})(expect(Equals2)("record field assignment")(state));
parseMoreRecordFields = (baseIndent) => (lastField) => (state) => peekKind(Comma2)(state) ? (($match_309) => {
  if ($match_309.$tag === 1) {
    return Ok2([[], state]);
  }
  if ($match_309.$tag === 0) {
    const next = $match_309.$0;
    return continuesLayout(baseIndent)(lastField.span.end)(next) ? ((s1) => (($match_310) => {
      if ($match_310.$tag === 1) {
        const e = $match_310.$0;
        return Err2(e);
      }
      if ($match_310.$tag === 0) {
        const fieldTok = $match_310.$0[0];
        const s2 = $match_310.$0[1];
        return (($match_311) => {
          if ($match_311.$tag === 1) {
            const e = $match_311.$0;
            return Err2(e);
          }
          if ($match_311.$tag === 0) {
            const field = $match_311.$0[0];
            const s3 = $match_311.$0[1];
            return (($match_312) => {
              if ($match_312.$tag === 1) {
                const e = $match_312.$0;
                return Err2(e);
              }
              if ($match_312.$tag === 0) {
                const rest = $match_312.$0[0];
                const s4 = $match_312.$0[1];
                return Ok2([_COLON_COLON2(field)(rest), s4]);
              }
              throw new Error("Pattern match failed");
            })(parseMoreRecordFields(baseIndent)(field)(s3));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldValue(fieldTok)(baseIndent)(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(LowerIdentifier2)("record field name")(s1)))(advance2(state)) : Ok2([[], state]);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : Ok2([[], state]);
var parseDeclarations = (state) => isAtEnd2(state) ? Ok2([[], state]) : ((tok) => $dict_Eq_Int2._EQ_EQ(tok.span.start.column)(1) ? (($match_313) => {
  if ($match_313.$tag === 1) {
    const e = $match_313.$0;
    return Err2(e);
  }
  if ($match_313.$tag === 0) {
    const decl = $match_313.$0[0];
    const s1 = $match_313.$0[1];
    return (($match_314) => {
      if ($match_314.$tag === 1) {
        const e = $match_314.$0;
        return Err2(e);
      }
      if ($match_314.$tag === 0) {
        const rest = $match_314.$0[0];
        const s2 = $match_314.$0[1];
        return Ok2([_COLON_COLON2(decl)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseDeclarations(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclaration(state)) : Err2(makeError("top-level declaration must start at column 1")(tok.span)))(current(state));
var parseProgram = (state) => ((tok) => $dict_Eq_Int2._EQ_EQ(tok.span.start.column)(1) ? (($match_315) => {
  if ($match_315.$tag === 1) {
    const e = $match_315.$0;
    return Err2(e);
  }
  if ($match_315.$tag === 0) {
    const modDecl = $match_315.$0[0];
    const s1 = $match_315.$0[1];
    return (($match_316) => {
      if ($match_316.$tag === 1) {
        const e = $match_316.$0;
        return Err2(e);
      }
      if ($match_316.$tag === 0) {
        const imports = $match_316.$0[0];
        const s2 = $match_316.$0[1];
        return (($match_317) => {
          if ($match_317.$tag === 1) {
            const e = $match_317.$0;
            return Err2(e);
          }
          if ($match_317.$tag === 0) {
            const decls = $match_317.$0[0];
            const s3 = $match_317.$0[1];
            return Ok2([{ moduleDecl: modDecl, imports, declarations: decls }, s3]);
          }
          throw new Error("Pattern match failed");
        })(parseDeclarations(s2));
      }
      throw new Error("Pattern match failed");
    })(parseImports(s1));
  }
  throw new Error("Pattern match failed");
})(parseModuleDeclaration(state)) : Err2(makeError("module declaration must start at column 1")(tok.span)))(current(state));
var parseTokens = (tokens) => (registry) => ((state) => (($match_318) => {
  if ($match_318.$tag === 1) {
    const e = $match_318.$0;
    return Err2(e);
  }
  if ($match_318.$tag === 0) {
    const prog = $match_318.$0[0];
    return Ok2(prog);
  }
  throw new Error("Pattern match failed");
})(parseProgram(state)))(initState2(tokens)(registry));

// ../vibe-parser/dist/VibeParser/VibeParser/Layout.js
var initState3 = { output: [], stack: [], bracketDepth: 0 };
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
var stackTop = (stack) => head(stack);
var stackPop = (stack) => (($match_1) => {
  if (Array.isArray($match_1) && $match_1.length >= 1) {
    const rest = $match_1.slice(1);
    return rest;
  }
  if (Array.isArray($match_1) && $match_1.length === 0) {
    return [];
  }
  throw new Error("Pattern match failed");
})(stack);
var isLetContext = (ctx) => $dict_Eq_String2._EQ_EQ(ctx.keyword)("let");
var hasLetContext = (stack) => any(isLetContext)(stack);
var skipNewlines2 = (tokens) => {
  while (true) {
    {
      const $match_2 = tokens;
      if (Array.isArray($match_2) && $match_2.length === 0) {
        return [];
      }
      if (Array.isArray($match_2) && $match_2.length >= 1) {
        const tok = $match_2[0];
        const rest = $match_2.slice(1);
        {
          const $match_3 = tok.kind;
          if ($match_3.$tag === 20) {
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
      const $match_4 = tokens;
      if (Array.isArray($match_4) && $match_4.length === 0) {
        return Nothing2;
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
            return Just2(tok);
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
      const $match_6 = stackTop(state.stack);
      if ($match_6.$tag === 1) {
        return state;
      }
      if ($match_6.$tag === 0) {
        const ctx = $match_6.$0;
        if ($dict_Ord_Int2._LT(col)(ctx.column)) {
          [col, span, state] = [col, span, { ...state, output: _COLON_COLON2(makeVirtual(BlockEnd2)(span))(state.output), stack: stackPop(state.stack) }];
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
      const $match_7 = stackTop(state.stack);
      if ($match_7.$tag === 1) {
        return state;
      }
      if ($match_7.$tag === 0) {
        const ctx = $match_7.$0;
        if ($dict_Ord_Int2._GT_EQ(ctx.bracketDepth)(state.bracketDepth)) {
          [state, span] = [{ ...state, output: _COLON_COLON2(makeVirtual(BlockEnd2)(span))(state.output), stack: stackPop(state.stack) }, span];
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
      const $match_8 = stackTop(state.stack);
      if ($match_8.$tag === 1) {
        return state;
      }
      if ($match_8.$tag === 0) {
        const ctx = $match_8.$0;
        {
          const newState = { ...state, output: _COLON_COLON2(makeVirtual(BlockEnd2)(span))(state.output), stack: stackPop(state.stack) };
          if ($dict_Eq_String2._EQ_EQ(ctx.keyword)("let")) {
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
      const $match_9 = stackTop(state.stack);
      if ($match_9.$tag === 1) {
        return state;
      }
      if ($match_9.$tag === 0) {
        [span, state] = [span, { ...state, output: _COLON_COLON2(makeVirtual(BlockEnd2)(span))(state.output), stack: stackPop(state.stack) }];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
};
var handleEof = (tok) => (state) => ((closed) => ({ ...closed, output: _COLON_COLON2(tok)(closed.output) }))(closeAllContexts(tok.span)(state));
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
      const $match_10 = tokens;
      if (Array.isArray($match_10) && $match_10.length === 0) {
        return state;
      }
      if (Array.isArray($match_10) && $match_10.length >= 1) {
        const tok = $match_10[0];
        const rest = $match_10.slice(1);
        {
          const $match_11 = tok.kind;
          if ($match_11.$tag === 24) {
            return handleEof(tok)(state);
          }
          if ($match_11.$tag === 20) {
            return handleNewline(rest)(state);
          }
          if ($match_11.$tag === 9) {
            return handleOpenBracket(tok)(rest)(state);
          }
          if ($match_11.$tag === 13) {
            return handleOpenBracket(tok)(rest)(state);
          }
          if ($match_11.$tag === 11) {
            return handleOpenBracket(tok)(rest)(state);
          }
          if ($match_11.$tag === 10) {
            return handleCloseBracket(tok)(rest)(state);
          }
          if ($match_11.$tag === 14) {
            return handleCloseBracket(tok)(rest)(state);
          }
          if ($match_11.$tag === 12) {
            return handleCloseBracket(tok)(rest)(state);
          }
          if ($match_11.$tag === 2) {
            return handleKeyword(tok)(rest)(state);
          }
          {
            [tokens, state] = [rest, { ...state, output: _COLON_COLON2(tok)(state.output) }];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
handleNewline = (rest) => (state) => (($match_12) => {
  if ($match_12.$tag === 1) {
    return processTokens(rest)(state);
  }
  if ($match_12.$tag === 0) {
    const nextTok = $match_12.$0;
    return (($match_13) => {
      if ($match_13.$tag === 24) {
        return processTokens(rest)(state);
      }
      {
        return ((col) => ((closed) => ((withSep) => processTokens(rest)(withSep))((($match_14) => {
          if ($match_14.$tag === 1) {
            return closed;
          }
          if ($match_14.$tag === 0) {
            const ctx = $match_14.$0;
            return $dict_Eq_Int2._EQ_EQ(col)(ctx.column) ? { ...closed, output: _COLON_COLON2(makeVirtual(BlockSep2)(nextTok.span))(closed.output) } : closed;
          }
          throw new Error("Pattern match failed");
        })(stackTop(closed.stack))))(closeContextsBelow(col)(nextTok.span)(state)))(nextTok.span.start.column);
      }
      throw new Error("Pattern match failed");
    })(nextTok.kind);
  }
  throw new Error("Pattern match failed");
})(nextReal(rest));
handleOpenBracket = (tok) => (rest) => (state) => processTokens(rest)({ ...state, output: _COLON_COLON2(tok)(state.output), bracketDepth: $dict_Num_Int2._PLUS(state.bracketDepth)(1) });
handleCloseBracket = (tok) => (rest) => (state) => ((closed) => ((newDepth) => processTokens(rest)({ ...closed, output: _COLON_COLON2(tok)(closed.output), bracketDepth: newDepth }))($dict_Ord_Int2._GT(closed.bracketDepth)(0) ? $dict_Num_Int2._MINUS(closed.bracketDepth)(1) : 0))(closeContextsForBracket(state)(tok.span));
handleKeyword = (tok) => (rest) => (state) => $dict_Eq_String2._EQ_EQ(tok.lexeme)("in") ? handleIn(tok)(rest)(state) : isLayoutKeyword(tok.lexeme) ? handleLayoutKeyword(tok)(rest)(state) : processTokens(rest)({ ...state, output: _COLON_COLON2(tok)(state.output) });
handleIn = (tok) => (rest) => (state) => ((closed) => processTokens(rest)({ ...closed, output: _COLON_COLON2(tok)(closed.output) }))(hasLetContext(state.stack) ? closeForIn(state)(tok.span) : state);
handleLayoutKeyword = (tok) => (rest) => (state) => ((stateWithTok) => ((remaining) => (($match_15) => {
  if (Array.isArray($match_15) && $match_15.length === 0) {
    return processTokens(remaining)(stateWithTok);
  }
  if (Array.isArray($match_15) && $match_15.length >= 1) {
    const nextTok = $match_15[0];
    return (($match_16) => {
      if ($match_16.$tag === 24) {
        return processTokens(remaining)(stateWithTok);
      }
      {
        return ((ctx) => ((newState) => processTokens(remaining)(newState))({ ...stateWithTok, stack: _COLON_COLON2(ctx)(stateWithTok.stack), output: _COLON_COLON2(makeVirtual(BlockStart2)(nextTok.span))(stateWithTok.output) }))({ column: nextTok.span.start.column, keyword: tok.lexeme, bracketDepth: stateWithTok.bracketDepth });
      }
      throw new Error("Pattern match failed");
    })(nextTok.kind);
  }
  throw new Error("Pattern match failed");
})(remaining))(skipNewlines2(rest)))({ ...state, output: _COLON_COLON2(tok)(state.output) });
var insertLayoutTokens = (tokens) => ((finalState) => reverse2(finalState.output))(processTokens(tokens)(initState3));

// ../vibe-parser/dist/Json/Json/Encode.js
var JsonString2 = ($0) => ({ $tag: 0, $0 });
var JsonInt2 = ($0) => ({ $tag: 1, $0 });
var JsonFloat = ($0) => ({ $tag: 2, $0 });
var JsonBool2 = ($0) => ({ $tag: 3, $0 });
var JsonNull = { $tag: 4 };
var JsonArray2 = ($0) => ({ $tag: 5, $0 });
var JsonObject2 = ($0) => ({ $tag: 6, $0 });
var string2 = (s) => JsonString2(s);
var int2 = (n) => JsonInt2(n);
var bool2 = (b) => JsonBool2(b);
var $null = { $tag: 4 };
var hexDigit2 = (n) => (($match_0) => {
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
var hexByte2 = (n) => ((hi) => ((lo) => $dict_Appendable_String2._PLUS_PLUS(hexDigit2(hi))(hexDigit2(lo)))($dict_Integral_Int2._PERCENT(n)(16)))($dict_Integral_Int2._SLASH_SLASH(n)(16));
var escapeControl2 = (code) => $dict_Appendable_String2._PLUS_PLUS("\\u00")(hexByte2(code));
var escapeChar2 = (c) => (($match_1) => {
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
    return ((code) => $dict_Ord_Int2._LT(code)(32) ? escapeControl2(code) : $dict_Show_Char2.toString(c))(toCode2(c));
  }
  throw new Error("Pattern match failed");
})(c);
var escapeChars2 = (chars) => ((go) => go(chars)(""))((cs) => (acc) => {
  while (true) {
    {
      const $match_2 = cs;
      if (Array.isArray($match_2) && $match_2.length === 0) {
        return acc;
      }
      if (Array.isArray($match_2) && $match_2.length >= 1) {
        const c = $match_2[0];
        const rest = $match_2.slice(1);
        [cs, acc] = [rest, $dict_Appendable_String2._PLUS_PLUS(acc)(escapeChar2(c))];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
var renderString2 = (s) => $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS('"')(escapeChars2(toList2(s))))('"');
var renderFloat2 = (n) => ((s) => (($match_3) => {
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
})(s))($dict_Show_Float2.toString(n));
var repeatSpaces2 = (n) => $dict_Ord_Int2._LT_EQ(n)(0) ? "" : $dict_Appendable_String2._PLUS_PLUS(" ")(repeatSpaces2($dict_Num_Int2._MINUS(n)(1)));
var makeIndent2 = (indent) => (depth) => repeatSpaces2($dict_Num_Int2._STAR(indent)(depth));
var renderValue2;
var renderArray2;
var renderObject2;
var joinValues2;
var joinValuesIndented2;
var joinPairs2;
var joinPairsIndented2;
renderValue2 = (indent) => (depth) => (val) => (($match_4) => {
  if ($match_4.$tag === 0) {
    const s = $match_4.$0;
    return renderString2(s);
  }
  if ($match_4.$tag === 1) {
    const n = $match_4.$0;
    return $dict_Show_Int2.toString(n);
  }
  if ($match_4.$tag === 2) {
    const n = $match_4.$0;
    return renderFloat2(n);
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
    return renderArray2(indent)(depth)(items);
  }
  if ($match_4.$tag === 6) {
    const pairs = $match_4.$0;
    return renderObject2(indent)(depth)(pairs);
  }
  throw new Error("Pattern match failed");
})(val);
renderArray2 = (indent) => (depth) => (items) => (($match_5) => {
  if (Array.isArray($match_5) && $match_5.length === 0) {
    return "[]";
  }
  {
    return $dict_Eq_Int2._EQ_EQ(indent)(0) ? $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("[")(joinValues2(indent)(depth)(items)))("]") : ((newDepth) => ((pad) => ((innerPad) => $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(`[
`)(pad))(joinValuesIndented2(indent)(newDepth)(items)))(`
`))(innerPad))("]"))(makeIndent2(indent)(depth)))(makeIndent2(indent)(newDepth)))($dict_Num_Int2._PLUS(depth)(1));
  }
  throw new Error("Pattern match failed");
})(items);
renderObject2 = (indent) => (depth) => (pairs) => (($match_6) => {
  if (Array.isArray($match_6) && $match_6.length === 0) {
    return "{}";
  }
  {
    return $dict_Eq_Int2._EQ_EQ(indent)(0) ? $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("{")(joinPairs2(indent)(depth)(pairs)))("}") : ((newDepth) => ((pad) => ((innerPad) => $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(`{
`)(pad))(joinPairsIndented2(indent)(newDepth)(pairs)))(`
`))(innerPad))("}"))(makeIndent2(indent)(depth)))(makeIndent2(indent)(newDepth)))($dict_Num_Int2._PLUS(depth)(1));
  }
  throw new Error("Pattern match failed");
})(pairs);
joinValues2 = (indent) => (depth) => (items) => ((go) => go(items)(""))((xs) => (acc) => {
  while (true) {
    {
      const $match_7 = xs;
      if (Array.isArray($match_7) && $match_7.length === 0) {
        return acc;
      }
      if (Array.isArray($match_7) && $match_7.length === 1) {
        const x = $match_7[0];
        return $dict_Appendable_String2._PLUS_PLUS(acc)(renderValue2(indent)(depth)(x));
      }
      if (Array.isArray($match_7) && $match_7.length >= 1) {
        const x = $match_7[0];
        const rest = $match_7.slice(1);
        [xs, acc] = [rest, $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(acc)(renderValue2(indent)(depth)(x)))(",")];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
joinValuesIndented2 = (indent) => (depth) => (items) => ((pad) => ((go) => go(items)(""))((xs) => (acc) => {
  while (true) {
    {
      const $match_8 = xs;
      if (Array.isArray($match_8) && $match_8.length === 0) {
        return acc;
      }
      if (Array.isArray($match_8) && $match_8.length === 1) {
        const x = $match_8[0];
        return $dict_Appendable_String2._PLUS_PLUS(acc)(renderValue2(indent)(depth)(x));
      }
      if (Array.isArray($match_8) && $match_8.length >= 1) {
        const x = $match_8[0];
        const rest = $match_8.slice(1);
        [xs, acc] = [rest, $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(acc)(renderValue2(indent)(depth)(x)))(`,
`))(pad)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
}))(makeIndent2(indent)(depth));
joinPairs2 = (indent) => (depth) => (pairs) => ((go) => go(pairs)(""))((ps) => (acc) => {
  while (true) {
    {
      const $match_9 = ps;
      if (Array.isArray($match_9) && $match_9.length === 0) {
        return acc;
      }
      if (Array.isArray($match_9) && $match_9.length === 1) {
        const k = $match_9[0][0];
        const v = $match_9[0][1];
        return $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(acc)(renderString2(k)))(":"))(renderValue2(indent)(depth)(v));
      }
      if (Array.isArray($match_9) && $match_9.length >= 1) {
        const k = $match_9[0][0];
        const v = $match_9[0][1];
        const rest = $match_9.slice(1);
        [ps, acc] = [rest, $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(acc)(renderString2(k)))(":"))(renderValue2(indent)(depth)(v)))(",")];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
joinPairsIndented2 = (indent) => (depth) => (pairs) => ((pad) => ((go) => go(pairs)(""))((ps) => (acc) => {
  while (true) {
    {
      const $match_10 = ps;
      if (Array.isArray($match_10) && $match_10.length === 0) {
        return acc;
      }
      if (Array.isArray($match_10) && $match_10.length === 1) {
        const k = $match_10[0][0];
        const v = $match_10[0][1];
        return $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(acc)(renderString2(k)))(": "))(renderValue2(indent)(depth)(v));
      }
      if (Array.isArray($match_10) && $match_10.length >= 1) {
        const k = $match_10[0][0];
        const v = $match_10[0][1];
        const rest = $match_10.slice(1);
        [ps, acc] = [rest, $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(acc)(renderString2(k)))(": "))(renderValue2(indent)(depth)(v)))(`,
`))(pad)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
}))(makeIndent2(indent)(depth));
var encode2 = (indent) => (val) => renderValue2(indent)(0)(val);
var list2 = (encode3) => (items) => JsonArray2(map2(encode3)(items));
var object2 = (pairs) => JsonObject2(pairs);

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
var _AMP_AMP6 = (a) => (b) => a && b();
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
var arrayGet = _arrayGet(Just2)(Nothing2);
var objectGet = _objectGet(Just2)(Nothing2);
var jsonParse2 = _jsonParse(Ok2)(Err2);
var decodeString_ = (json) => _isString(json) ? Ok2(_jsonToString(json)) : Err2(Failure("Expecting a STRING"));
var string3 = Decoder(decodeString_);
var decodeInt_ = (json) => _isInt(json) ? Ok2(_toInt(json)) : Err2(Failure("Expecting an INT"));
var int3 = Decoder(decodeInt_);
var decodeFloat_ = (json) => _isNumber(json) ? Ok2(_toFloat(json)) : Err2(Failure("Expecting a FLOAT"));
var float = Decoder(decodeFloat_);
var decodeBool_ = (json) => _isBool(json) ? Ok2(_toBool(json)) : Err2(Failure("Expecting a BOOL"));
var bool3 = Decoder(decodeBool_);
var decodeListItems = (decoder) => (json) => (idx) => (len) => $dict_Ord_Int2._GT_EQ(idx)(len) ? Ok2([]) : (($match_5) => {
  if ($match_5.$tag === 1) {
    return Err2(Index(idx)(Failure("Index out of bounds")));
  }
  if ($match_5.$tag === 0) {
    const item = $match_5.$0;
    return (($match_6) => {
      if ($match_6.$tag === 1) {
        const e = $match_6.$0;
        return Err2(Index(idx)(e));
      }
      if ($match_6.$tag === 0) {
        const val = $match_6.$0;
        return (($match_7) => {
          if ($match_7.$tag === 1) {
            const e = $match_7.$0;
            return Err2(e);
          }
          if ($match_7.$tag === 0) {
            const rest = $match_7.$0;
            return Ok2(_COLON_COLON2(val)(rest));
          }
          throw new Error("Pattern match failed");
        })(decodeListItems(decoder)(json)($dict_Num_Int2._PLUS(idx)(1))(len));
      }
      throw new Error("Pattern match failed");
    })(runDecoder(decoder)(item));
  }
  throw new Error("Pattern match failed");
})(arrayGet(idx)(json));
var decodeList_ = (decoder) => (json) => _isArray(json) ? decodeListItems(decoder)(json)(0)(_arrayLength(json)) : Err2(Failure("Expecting an ARRAY"));
var list3 = (decoder) => Decoder(decodeList_(decoder));
var decodeField_ = (key) => (decoder) => (json) => _isObject(json) ? (($match_12) => {
  if ($match_12.$tag === 1) {
    return Err2(Field(key)(Failure("Missing field")));
  }
  if ($match_12.$tag === 0) {
    const sub = $match_12.$0;
    return (($match_13) => {
      if ($match_13.$tag === 1) {
        const e = $match_13.$0;
        return Err2(Field(key)(e));
      }
      if ($match_13.$tag === 0) {
        const val = $match_13.$0;
        return Ok2(val);
      }
      throw new Error("Pattern match failed");
    })(runDecoder(decoder)(sub));
  }
  throw new Error("Pattern match failed");
})(objectGet(key)(json)) : Err2(Failure("Expecting an OBJECT"));
var field = (key) => (decoder) => Decoder(decodeField_(key)(decoder));
var decodeMap3_ = (f) => (da) => (db) => (dc) => (json) => (($match_19) => {
  if ($match_19.$tag === 1) {
    const e = $match_19.$0;
    return Err2(e);
  }
  if ($match_19.$tag === 0) {
    const a = $match_19.$0;
    return (($match_20) => {
      if ($match_20.$tag === 1) {
        const e = $match_20.$0;
        return Err2(e);
      }
      if ($match_20.$tag === 0) {
        const b = $match_20.$0;
        return (($match_21) => {
          if ($match_21.$tag === 1) {
            const e = $match_21.$0;
            return Err2(e);
          }
          if ($match_21.$tag === 0) {
            const c = $match_21.$0;
            return Ok2(f(a)(b)(c));
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
var decodeSucceed_ = (val) => (_) => Ok2(val);
var succeed = (val) => Decoder(decodeSucceed_(val));
var decodeAndThen_ = (callback) => (decoder) => (json) => (($match_31) => {
  if ($match_31.$tag === 1) {
    const e = $match_31.$0;
    return Err2(e);
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
jsonToValue = (json) => _isNull(json) ? JsonNull : _isBool(json) ? JsonBool2(_toBool(json)) : _isInt(json) ? JsonInt2(_toInt(json)) : _isNumber(json) ? JsonFloat(_toFloat(json)) : _isString(json) ? JsonString2(_jsonToString(json)) : _isArray(json) ? JsonArray2(convertArray(json)(0)(_arrayLength(json))) : _isObject(json) ? JsonObject2(convertObject(json)(_objectKeys(json))) : JsonNull;
convertArray = (json) => (idx) => (len) => $dict_Ord_Int2._GT_EQ(idx)(len) ? [] : (($match_32) => {
  if ($match_32.$tag === 1) {
    return [];
  }
  if ($match_32.$tag === 0) {
    const item = $match_32.$0;
    return _COLON_COLON2(jsonToValue(item))(convertArray(json)($dict_Num_Int2._PLUS(idx)(1))(len));
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
            return _COLON_COLON2([k, jsonToValue(item)])(convertObject(json)(rest));
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var decodeValue_ = (json) => Ok2(jsonToValue(json));
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
          return $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("At ")(context))(": "))(msg);
        }
      }
      if ($match_35.$tag === 0) {
        const key = $match_35.$0;
        const inner = $match_35.$1;
        {
          const newContext = isEmpty(context) ? $dict_Appendable_String2._PLUS_PLUS(".")(key) : $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(context)("."))(key);
          [context, err] = [newContext, inner];
          continue;
        }
      }
      if ($match_35.$tag === 1) {
        const idx = $match_35.$0;
        const inner = $match_35.$1;
        {
          const newContext = isEmpty(context) ? $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("[")($dict_Show_Int2.toString(idx)))("]") : $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS(context)("["))($dict_Show_Int2.toString(idx)))("]");
          [context, err] = [newContext, inner];
          continue;
        }
      }
      if ($match_35.$tag === 2) {
        const errors = $match_35.$0;
        {
          const prefix = isEmpty(context) ? `One of the following failed:
` : $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("At ")(context))(`, one of the following failed:
`);
          return $dict_Appendable_String2._PLUS_PLUS(prefix)(formatOneOf(errors)(1));
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
    return $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("  (")($dict_Show_Int2.toString(n)))(") "))(errorToString(e)))(`
`))(formatOneOf(rest)($dict_Num_Int2._PLUS(n)(1)));
  }
  throw new Error("Pattern match failed");
})(errors);
var $impl_Eq_Error__EQ_EQ;
var $dict_Eq_Error;
$impl_Eq_Error__EQ_EQ = (x_impl) => (y_impl) => (($match_39) => {
  if ($match_39[0].$tag === 0 && $match_39[1].$tag === 0) {
    const a_0 = $match_39[0].$0;
    const a_1 = $match_39[0].$1;
    const b_0 = $match_39[1].$0;
    const b_1 = $match_39[1].$1;
    return _AMP_AMP6($dict_Eq_String2._EQ_EQ(a_0)(b_0))(() => $dict_Eq_Error._EQ_EQ(a_1)(b_1));
  }
  if ($match_39[0].$tag === 1 && $match_39[1].$tag === 1) {
    const a_0 = $match_39[0].$0;
    const a_1 = $match_39[0].$1;
    const b_0 = $match_39[1].$0;
    const b_1 = $match_39[1].$1;
    return _AMP_AMP6($dict_Eq_Int2._EQ_EQ(a_0)(b_0))(() => $dict_Eq_Error._EQ_EQ(a_1)(b_1));
  }
  if ($match_39[0].$tag === 2 && $match_39[1].$tag === 2) {
    const a_0 = $match_39[0].$0;
    const b_0 = $match_39[1].$0;
    return $dict_Eq_List_v3562($dict_Eq_Error)._EQ_EQ(a_0)(b_0);
  }
  if ($match_39[0].$tag === 3 && $match_39[1].$tag === 3) {
    const a_0 = $match_39[0].$0;
    const b_0 = $match_39[1].$0;
    return $dict_Eq_String2._EQ_EQ(a_0)(b_0);
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
    return $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Field(")($dict_Show_String2.toString(a0)))(", "))($dict_Show_Error.toString(a1)))(")");
  }
  if ($match_40.$tag === 1) {
    const a0 = $match_40.$0;
    const a1 = $match_40.$1;
    return $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Index(")($dict_Show_Int2.toString(a0)))(", "))($dict_Show_Error.toString(a1)))(")");
  }
  if ($match_40.$tag === 2) {
    const a0 = $match_40.$0;
    return $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("OneOf(")($dict_Show_List_v3572($dict_Show_Error).toString(a0)))(")");
  }
  if ($match_40.$tag === 3) {
    const a0 = $match_40.$0;
    return $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Failure(")($dict_Show_String2.toString(a0)))(")");
  }
  throw new Error("Pattern match failed");
})(x_impl);
$dict_Show_Error = {
  toString: $impl_Show_Error_toString
};

// ../vibe-parser/dist/VibeLexer/VibeLexer.js
var _PIPE_PIPE7 = (a) => (b) => a || b();
var _AMP_AMP7 = (a) => (b) => a && b();
var initState4 = (source) => ({ source, index: 0, line: 1, column: 1, sourceLen: length2(source) });
var isAtEnd3 = (state) => $dict_Ord_Int2._GT_EQ(state.index)(state.sourceLen);
var peek2 = (state) => charAt2(state.index)(state.source);
var peekAt2 = (offset) => (state) => charAt2($dict_Num_Int2._PLUS(state.index)(offset))(state.source);
var position2 = (state) => ({ offset: state.index, line: state.line, column: state.column });
var advance3 = (state) => ((ch) => ((newIndex) => ((newState) => [ch, newState])((($match_0) => {
  if ($match_0 === `
`) {
    return { ...state, index: newIndex, line: $dict_Num_Int2._PLUS(state.line)(1), column: 1 };
  }
  if ($match_0 === "\t") {
    return { ...state, index: newIndex, column: $dict_Num_Int2._PLUS($dict_Num_Int2._STAR($dict_Integral_Int2._SLASH_SLASH($dict_Num_Int2._MINUS(state.column)(1))(8))(8))(9) };
  }
  {
    return { ...state, index: newIndex, column: $dict_Num_Int2._PLUS(state.column)(1) };
  }
  throw new Error("Pattern match failed");
})(ch)))($dict_Num_Int2._PLUS(state.index)(1)))(unsafeCharAt4(state.index)(state.source));
var skip3 = (state) => (($match_1) => {
  {
    const s = $match_1[1];
    return s;
  }
  throw new Error("Pattern match failed");
})(advance3(state));
var skip22 = (state) => skip3(skip3(state));
var sliceFrom2 = (startOffset) => (state) => slice2(startOffset)(state.index)(state.source);
var isIdentifierStart2 = (c) => _PIPE_PIPE7(isAlpha2(c))(() => $dict_Eq_Char2._EQ_EQ(c)("_"));
var isIdentifierPart2 = (c) => _PIPE_PIPE7(isIdentifierStart2(c))(() => isDigit2(c));
var isWhitespace2 = (c) => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)(" "))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("\t"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)(`
`))(() => $dict_Eq_Char2._EQ_EQ(c)("\r"))));
var isOperatorChar2 = (c) => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("!"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("#"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("$"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("%"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("&"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("*"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("+"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("."))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("/"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("<"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("="))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)(">"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("?"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("@"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("\\"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("^"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("|"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)("~"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)(":"))(() => $dict_Eq_Char2._EQ_EQ(c)("-"))))))))))))))))))));
var isKeyword2 = (word) => (($match_2) => {
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
var skipLineComment2 = (state) => {
  while (true) {
    if (isAtEnd3(state)) {
      return state;
    } else {
      {
        const $match_4 = peek2(state);
        if ($match_4.$tag === 1) {
          return state;
        }
        if ($match_4.$tag === 0) {
          const c = $match_4.$0;
          if ($dict_Eq_Char2._EQ_EQ(c)(`
`)) {
            return state;
          } else {
            state = skip3(state);
            continue;
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var skipBlockComment2;
var skipBlockCommentPair2;
skipBlockComment2 = (depth) => (state) => {
  while (true) {
    if ($dict_Eq_Int2._EQ_EQ(depth)(0)) {
      return Ok2(state);
    } else {
      if (isAtEnd3(state)) {
        return Err2({ message: "Unterminated block comment", span: { start: position2(state), end: position2(state) } });
      } else {
        {
          const $match_5 = [peek2(state), peekAt2(1)(state)];
          if ($match_5[0].$tag === 0 && $match_5[1].$tag === 0) {
            const c1 = $match_5[0].$0;
            const c2 = $match_5[1].$0;
            return skipBlockCommentPair2(depth)(state)(c1)(c2);
          }
          {
            [depth, state] = [depth, skip3(state)];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
    }
  }
};
skipBlockCommentPair2 = (depth) => (state) => (c1) => (c2) => (($match_6) => {
  if ($match_6 === "{") {
    return (($match_7) => {
      if ($match_7 === "-") {
        return skipBlockComment2($dict_Num_Int2._PLUS(depth)(1))(skip22(state));
      }
      {
        return skipBlockComment2(depth)(skip3(state));
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  if ($match_6 === "-") {
    return (($match_8) => {
      if ($match_8 === "}") {
        return skipBlockComment2($dict_Num_Int2._MINUS(depth)(1))(skip22(state));
      }
      {
        return skipBlockComment2(depth)(skip3(state));
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return skipBlockComment2(depth)(skip3(state));
  }
  throw new Error("Pattern match failed");
})(c1);
var skipWhitespaceAndComments2;
var skipWsDispatch2;
var skipWsMaybeLine2;
var skipWsMaybeBlock2;
skipWhitespaceAndComments2 = (sawNl) => (state) => isAtEnd3(state) ? Ok2({ state, sawNewline: sawNl }) : (($match_9) => {
  if ($match_9.$tag === 1) {
    return Ok2({ state, sawNewline: sawNl });
  }
  if ($match_9.$tag === 0) {
    const c = $match_9.$0;
    return skipWsDispatch2(sawNl)(state)(c);
  }
  throw new Error("Pattern match failed");
})(peek2(state));
skipWsDispatch2 = (sawNl) => (state) => (c) => isWhitespace2(c) ? ((nl) => skipWhitespaceAndComments2(nl)(skip3(state)))(_PIPE_PIPE7(sawNl)(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(c)(`
`))(() => $dict_Eq_Char2._EQ_EQ(c)("\r")))) : (($match_10) => {
  if ($match_10 === "-") {
    return skipWsMaybeLine2(sawNl)(state);
  }
  if ($match_10 === "{") {
    return skipWsMaybeBlock2(sawNl)(state);
  }
  {
    return Ok2({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(c);
skipWsMaybeLine2 = (sawNl) => (state) => (($match_11) => {
  if ($match_11.$tag === 0) {
    const c2 = $match_11.$0;
    return (($match_12) => {
      if ($match_12 === "-") {
        return ((s1) => skipWhitespaceAndComments2(true)(s1))(skipLineComment2(state));
      }
      {
        return Ok2({ state, sawNewline: sawNl });
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return Ok2({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
skipWsMaybeBlock2 = (sawNl) => (state) => (($match_13) => {
  if ($match_13.$tag === 0) {
    const c2 = $match_13.$0;
    return (($match_14) => {
      if ($match_14 === "-") {
        return ((lineBefore) => ((s2) => (($match_15) => {
          if ($match_15.$tag === 1) {
            const e = $match_15.$0;
            return Err2(e);
          }
          if ($match_15.$tag === 0) {
            const s3 = $match_15.$0;
            return ((nl) => skipWhitespaceAndComments2(nl)(s3))(_PIPE_PIPE7(sawNl)(() => $dict_Ord_Int2._GT(s3.line)(lineBefore)));
          }
          throw new Error("Pattern match failed");
        })(skipBlockComment2(1)(s2)))(skip22(state)))(state.line);
      }
      {
        return Ok2({ state, sawNewline: sawNl });
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return Ok2({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
var identKind2 = (text) => (isUpper3) => isKeyword2(text) ? Keyword2 : isUpper3 ? UpperIdentifier2 : LowerIdentifier2;
var consumeIdentifierChars2 = (state) => {
  while (true) {
    {
      const $match_16 = peek2(state);
      if ($match_16.$tag === 1) {
        return state;
      }
      if ($match_16.$tag === 0) {
        const c = $match_16.$0;
        if (_PIPE_PIPE7(isIdentifierPart2(c))(() => $dict_Eq_Char2._EQ_EQ(c)("'"))) {
          state = skip3(state);
          continue;
        } else {
          return state;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var readIdentifierOrKeyword2 = (state) => (startPos) => ((startIdx) => (($match_17) => {
  {
    const firstChar = $match_17[0];
    const s1 = $match_17[1];
    return ((isUpper3) => ((s2) => ((text) => ((endPos) => ((span) => ((kind) => [{ kind, lexeme: text, span }, s2])(identKind2(text)(isUpper3)))({ start: startPos, end: endPos }))(position2(s2)))(sliceFrom2(startIdx)(s2)))(consumeIdentifierChars2(s1)))(isUpper2(firstChar));
  }
  throw new Error("Pattern match failed");
})(advance3(state)))(state.index);
var makeNumberToken2 = (state) => (startIdx) => (startPos) => ((text) => ((endPos) => [{ kind: NumberToken2, lexeme: text, span: { start: startPos, end: endPos } }, state])(position2(state)))(sliceFrom2(startIdx)(state));
var consumeDigits2 = (state) => {
  while (true) {
    {
      const $match_18 = peek2(state);
      if ($match_18.$tag === 1) {
        return state;
      }
      if ($match_18.$tag === 0) {
        const c = $match_18.$0;
        if (isDigit2(c)) {
          state = skip3(state);
          continue;
        } else {
          return state;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var readNumberAfterDot2 = (s1) => (startIdx) => (startPos) => (($match_19) => {
  if ($match_19.$tag === 0) {
    const d = $match_19.$0;
    return isDigit2(d) ? ((s2) => ((s3) => makeNumberToken2(s3)(startIdx)(startPos))(consumeDigits2(s2)))(skip3(s1)) : makeNumberToken2(s1)(startIdx)(startPos);
  }
  {
    return makeNumberToken2(s1)(startIdx)(startPos);
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(s1));
var readNumberAfterInt2 = (s1) => (startIdx) => (startPos) => (($match_20) => {
  if ($match_20.$tag === 0) {
    const c = $match_20.$0;
    return (($match_21) => {
      if ($match_21 === ".") {
        return readNumberAfterDot2(s1)(startIdx)(startPos);
      }
      {
        return makeNumberToken2(s1)(startIdx)(startPos);
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return makeNumberToken2(s1)(startIdx)(startPos);
  }
  throw new Error("Pattern match failed");
})(peek2(s1));
var readNumber2 = (state) => (startPos) => ((startIdx) => ((s1) => readNumberAfterInt2(s1)(startIdx)(startPos))(consumeDigits2(state)))(state.index);
var isValidStringEscape2 = (esc) => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(esc)("n"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(esc)("r"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(esc)("t"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(esc)('"'))(() => $dict_Eq_Char2._EQ_EQ(esc)("\\")))));
var readStringBody2;
var readStringEscape2;
readStringBody2 = (state) => (startIdx) => (startPos) => {
  while (true) {
    {
      const $match_22 = peek2(state);
      if ($match_22.$tag === 1) {
        return Err2({ message: "Unterminated string literal", span: { start: startPos, end: position2(state) } });
      }
      if ($match_22.$tag === 0) {
        const c = $match_22.$0;
        {
          const $match_23 = c;
          if ($match_23 === '"') {
            {
              const s1 = skip3(state);
              {
                const text = sliceFrom2(startIdx)(s1);
                {
                  const endPos = position2(s1);
                  return Ok2([{ kind: StringToken2, lexeme: text, span: { start: startPos, end: endPos } }, s1]);
                }
              }
            }
          }
          if ($match_23 === `
`) {
            return Err2({ message: "Unterminated string literal", span: { start: startPos, end: position2(state) } });
          }
          if ($match_23 === "\\") {
            return readStringEscape2(state)(startIdx)(startPos);
          }
          {
            [state, startIdx, startPos] = [skip3(state), startIdx, startPos];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
readStringEscape2 = (state) => (startIdx) => (startPos) => ((s1) => (($match_24) => {
  if ($match_24.$tag === 1) {
    return Err2({ message: "Unterminated string literal", span: { start: startPos, end: position2(s1) } });
  }
  if ($match_24.$tag === 0) {
    const esc = $match_24.$0;
    return isValidStringEscape2(esc) ? readStringBody2(skip3(s1))(startIdx)(startPos) : Err2({ message: "Invalid escape in string", span: { start: startPos, end: position2(s1) } });
  }
  throw new Error("Pattern match failed");
})(peek2(s1)))(skip3(state));
var readString2 = (state) => (startPos) => ((startIdx) => ((s1) => readStringBody2(s1)(startIdx)(startPos))(skip3(state)))(state.index);
var isValidCharEscape2 = (esc) => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(esc)("n"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(esc)("r"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(esc)("t"))(() => _PIPE_PIPE7($dict_Eq_Char2._EQ_EQ(esc)("'"))(() => $dict_Eq_Char2._EQ_EQ(esc)("\\")))));
var expectClosingQuote2 = (state) => (startIdx) => (startPos) => (($match_25) => {
  if ($match_25.$tag === 0) {
    const c = $match_25.$0;
    return (($match_26) => {
      if ($match_26 === "'") {
        return ((s1) => ((text) => ((endPos) => Ok2([{ kind: CharToken2, lexeme: text, span: { start: startPos, end: endPos } }, s1]))(position2(s1)))(sliceFrom2(startIdx)(s1)))(skip3(state));
      }
      {
        return Err2({ message: "Char literal must contain exactly one character", span: { start: startPos, end: position2(state) } });
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Err2({ message: "Char literal must contain exactly one character", span: { start: startPos, end: position2(state) } });
  }
  throw new Error("Pattern match failed");
})(peek2(state));
var readCharEscape2 = (s1) => (startIdx) => (startPos) => ((s2) => (($match_27) => {
  if ($match_27.$tag === 1) {
    return Err2({ message: "Unterminated char literal", span: { start: startPos, end: position2(s2) } });
  }
  if ($match_27.$tag === 0) {
    const esc = $match_27.$0;
    return isValidCharEscape2(esc) ? ((s3) => expectClosingQuote2(s3)(startIdx)(startPos))(skip3(s2)) : Err2({ message: "Invalid escape in char literal", span: { start: startPos, end: position2(s2) } });
  }
  throw new Error("Pattern match failed");
})(peek2(s2)))(skip3(s1));
var readChar2 = (state) => (startPos) => ((startIdx) => ((s1) => (($match_28) => {
  if ($match_28.$tag === 1) {
    return Err2({ message: "Unterminated char literal", span: { start: startPos, end: position2(s1) } });
  }
  if ($match_28.$tag === 0) {
    const c = $match_28.$0;
    return (($match_29) => {
      if ($match_29 === `
`) {
        return Err2({ message: "Unterminated char literal", span: { start: startPos, end: position2(s1) } });
      }
      if ($match_29 === "\\") {
        return readCharEscape2(s1)(startIdx)(startPos);
      }
      {
        return ((s2) => expectClosingQuote2(s2)(startIdx)(startPos))(skip3(s1));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  throw new Error("Pattern match failed");
})(peek2(s1)))(skip3(state)))(state.index);
var makeSimpleToken2 = (state) => (startPos) => (kind) => ((startIdx) => ((s1) => ((text) => ((endPos) => [{ kind, lexeme: text, span: { start: startPos, end: endPos } }, s1])(position2(s1)))(sliceFrom2(startIdx)(s1)))(skip3(state)))(state.index);
var makeTwoCharToken2 = (state) => (startPos) => (startIdx) => (kind) => ((s2) => ((text) => ((endPos) => [{ kind, lexeme: text, span: { start: startPos, end: endPos } }, s2])(position2(s2)))(sliceFrom2(startIdx)(s2)))(skip22(state));
var readDot2 = (state) => (startPos) => (startIdx) => (($match_30) => {
  if ($match_30.$tag === 0) {
    const c = $match_30.$0;
    return (($match_31) => {
      if ($match_31 === ".") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Range2)));
      }
      {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(Dot2)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok2(Just2(makeSimpleToken2(state)(startPos)(Dot2)));
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
var readColon2 = (state) => (startPos) => (startIdx) => (($match_32) => {
  if ($match_32.$tag === 0) {
    const c = $match_32.$0;
    return (($match_33) => {
      if ($match_33 === ":") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(Colon2)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok2(Just2(makeSimpleToken2(state)(startPos)(Colon2)));
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
var readEquals2 = (state) => (startPos) => (startIdx) => (($match_34) => {
  if ($match_34.$tag === 0) {
    const c = $match_34.$0;
    return (($match_35) => {
      if ($match_35 === ">") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      if ($match_35 === "=") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(Equals2)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok2(Just2(makeSimpleToken2(state)(startPos)(Equals2)));
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
var readPipe2 = (state) => (startPos) => (startIdx) => (($match_36) => {
  if ($match_36.$tag === 0) {
    const c = $match_36.$0;
    return (($match_37) => {
      if ($match_37 === ">") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      if ($match_37 === "|") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(Pipe2)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok2(Just2(makeSimpleToken2(state)(startPos)(Pipe2)));
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
var consumeOperator2;
var consumeOperatorCheck2;
consumeOperator2 = (state) => (($match_38) => {
  if ($match_38.$tag === 1) {
    return state;
  }
  if ($match_38.$tag === 0) {
    const c = $match_38.$0;
    return isOperatorChar2(c) ? consumeOperatorCheck2(state)(c) : state;
  }
  throw new Error("Pattern match failed");
})(peek2(state));
consumeOperatorCheck2 = (state) => (c) => (($match_39) => {
  if ($match_39 === "-") {
    return (($match_40) => {
      if ($match_40.$tag === 0) {
        const c2 = $match_40.$0;
        return (($match_41) => {
          if ($match_41 === ">") {
            return state;
          }
          {
            return consumeOperator2(skip3(state));
          }
          throw new Error("Pattern match failed");
        })(c2);
      }
      {
        return consumeOperator2(skip3(state));
      }
      throw new Error("Pattern match failed");
    })(peekAt2(1)(state));
  }
  {
    return consumeOperator2(skip3(state));
  }
  throw new Error("Pattern match failed");
})(c);
var readLAngle2 = (state) => (startPos) => (startIdx) => (($match_42) => {
  if ($match_42.$tag === 0) {
    const c = $match_42.$0;
    return (($match_43) => {
      if ($match_43 === "|") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      if ($match_43 === "=") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok2(Just2([{ kind: Operator2, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position2(s1)))(sliceFrom2(startIdx)(s1)))(consumeOperator2(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok2(Just2([{ kind: Operator2, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position2(s1)))(sliceFrom2(startIdx)(s1)))(consumeOperator2(state));
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
var readDash2 = (state) => (startPos) => (startIdx) => (($match_44) => {
  if ($match_44.$tag === 0) {
    const c = $match_44.$0;
    return (($match_45) => {
      if ($match_45 === ">") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      if ($match_45 === "=") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok2(Just2([{ kind: Operator2, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position2(s1)))(sliceFrom2(startIdx)(s1)))(consumeOperator2(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok2(Just2([{ kind: Operator2, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position2(s1)))(sliceFrom2(startIdx)(s1)))(consumeOperator2(state));
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
var readGt2 = (state) => (startPos) => (startIdx) => (($match_46) => {
  if ($match_46.$tag === 0) {
    const c = $match_46.$0;
    return (($match_47) => {
      if ($match_47 === "=") {
        return Ok2(Just2(makeTwoCharToken2(state)(startPos)(startIdx)(Operator2)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok2(Just2([{ kind: Operator2, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position2(s1)))(sliceFrom2(startIdx)(s1)))(consumeOperator2(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok2(Just2([{ kind: Operator2, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position2(s1)))(sliceFrom2(startIdx)(s1)))(consumeOperator2(state));
  }
  throw new Error("Pattern match failed");
})(peekAt2(1)(state));
var readGenericOperator2 = (state) => (startPos) => (startIdx) => (c) => isOperatorChar2(c) ? ((s1) => ((text) => ((endPos) => Ok2(Just2([{ kind: Operator2, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position2(s1)))(sliceFrom2(startIdx)(s1)))(consumeOperator2(state)) : Ok2(Nothing2);
var readPunctuationOrOperator2 = (state) => (startPos) => ((startIdx) => (($match_48) => {
  if ($match_48.$tag === 1) {
    return Ok2(Nothing2);
  }
  if ($match_48.$tag === 0) {
    const c = $match_48.$0;
    return (($match_49) => {
      if ($match_49 === "(") {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(LParen2)));
      }
      if ($match_49 === ")") {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(RParen2)));
      }
      if ($match_49 === "{") {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(LBrace2)));
      }
      if ($match_49 === "}") {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(RBrace2)));
      }
      if ($match_49 === "[") {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(LBracket2)));
      }
      if ($match_49 === "]") {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(RBracket2)));
      }
      if ($match_49 === ",") {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(Comma2)));
      }
      if ($match_49 === ".") {
        return readDot2(state)(startPos)(startIdx);
      }
      if ($match_49 === ":") {
        return readColon2(state)(startPos)(startIdx);
      }
      if ($match_49 === "=") {
        return readEquals2(state)(startPos)(startIdx);
      }
      if ($match_49 === "|") {
        return readPipe2(state)(startPos)(startIdx);
      }
      if ($match_49 === "<") {
        return readLAngle2(state)(startPos)(startIdx);
      }
      if ($match_49 === "-") {
        return readDash2(state)(startPos)(startIdx);
      }
      if ($match_49 === ">") {
        return readGt2(state)(startPos)(startIdx);
      }
      if ($match_49 === "\\") {
        return Ok2(Just2(makeSimpleToken2(state)(startPos)(Backslash2)));
      }
      {
        return readGenericOperator2(state)(startPos)(startIdx)(c);
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  throw new Error("Pattern match failed");
})(peek2(state)))(state.index);
var readTokenDispatch2 = (state) => (startPos) => (c) => isIdentifierStart2(c) ? Ok2(readIdentifierOrKeyword2(state)(startPos)) : isDigit2(c) ? Ok2(readNumber2(state)(startPos)) : (($match_50) => {
  if ($match_50 === '"') {
    return readString2(state)(startPos);
  }
  if ($match_50 === "'") {
    return readChar2(state)(startPos);
  }
  {
    return (($match_51) => {
      if ($match_51.$tag === 1) {
        const e = $match_51.$0;
        return Err2(e);
      }
      if ($match_51.$tag === 0 && $match_51.$0.$tag === 1) {
        return Err2({ message: $dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Unexpected character '")($dict_Show_Char2.toString(c)))("'"), span: { start: startPos, end: startPos } });
      }
      if ($match_51.$tag === 0 && $match_51.$0.$tag === 0) {
        const result = $match_51.$0.$0;
        return Ok2(result);
      }
      throw new Error("Pattern match failed");
    })(readPunctuationOrOperator2(state)(startPos));
  }
  throw new Error("Pattern match failed");
})(c);
var readToken2 = (state) => (startPos) => (($match_52) => {
  if ($match_52.$tag === 1) {
    return Err2({ message: "Unexpected end of input", span: { start: startPos, end: startPos } });
  }
  if ($match_52.$tag === 0) {
    const c = $match_52.$0;
    return readTokenDispatch2(state)(startPos)(c);
  }
  throw new Error("Pattern match failed");
})(peek2(state));
var maybeInsertNewline2 = (tokens) => (sawNl) => (hasEmitted) => (state) => _AMP_AMP7(sawNl)(() => hasEmitted) ? ((nlPos) => ((nlToken) => _COLON_COLON2(nlToken)(tokens))({ kind: Newline2, lexeme: `
`, span: { start: nlPos, end: nlPos } }))(position2(state)) : tokens;
var lexLoop2 = (loop) => {
  while (true) {
    if (isAtEnd3(loop.state)) {
      return Ok2(loop);
    } else {
      {
        const $match_53 = skipWhitespaceAndComments2(loop.sawNewline)(loop.state);
        if ($match_53.$tag === 1) {
          const e = $match_53.$0;
          return Err2(e);
        }
        if ($match_53.$tag === 0) {
          const skipResult = $match_53.$0;
          if (isAtEnd3(skipResult.state)) {
            return Ok2({ ...loop, state: skipResult.state, sawNewline: skipResult.sawNewline });
          } else {
            {
              const tokens1 = maybeInsertNewline2(loop.tokens)(skipResult.sawNewline)(loop.hasEmittedToken)(skipResult.state);
              {
                const startPos = position2(skipResult.state);
                {
                  const $match_54 = readToken2(skipResult.state)(startPos);
                  if ($match_54.$tag === 1) {
                    const e = $match_54.$0;
                    return Err2(e);
                  }
                  if ($match_54.$tag === 0) {
                    const tok = $match_54.$0[0];
                    const newState = $match_54.$0[1];
                    loop = { tokens: _COLON_COLON2(tok)(tokens1), state: newState, sawNewline: false, hasEmittedToken: true };
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
var reverseHelper2 = (lst) => (acc) => {
  while (true) {
    {
      const $match_55 = lst;
      if (Array.isArray($match_55) && $match_55.length === 0) {
        return acc;
      }
      if (Array.isArray($match_55) && $match_55.length >= 1) {
        const x = $match_55[0];
        const xs = $match_55.slice(1);
        [lst, acc] = [xs, _COLON_COLON2(x)(acc)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
};
var reverse3 = (lst) => reverseHelper2(lst)([]);
var lex3 = (source) => ((initial) => (($match_56) => {
  if ($match_56.$tag === 1) {
    const e = $match_56.$0;
    return Err2(e);
  }
  if ($match_56.$tag === 0) {
    const loop = $match_56.$0;
    return ((endPos) => ((eofToken2) => Ok2(reverse3(_COLON_COLON2(eofToken2)(loop.tokens))))({ kind: Eof2, lexeme: "", span: { start: endPos, end: endPos } }))(position2(loop.state));
  }
  throw new Error("Pattern match failed");
})(lexLoop2(initial)))({ tokens: [], state: initState4(source), sawNewline: false, hasEmittedToken: false });

// ../vibe-parser/dist/VibeParser/VibeParser/Json.js
var encodePosition2 = (pos) => object2([["offset", int2(pos.offset)], ["line", int2(pos.line)], ["column", int2(pos.column)]]);
var encodeSpan2 = (span) => object2([["start", encodePosition2(span.start)], ["end", encodePosition2(span.end)]]);
var encodeExportSpec = (spec) => (($match_0) => {
  if ($match_0.$tag === 0) {
    const name = $match_0.$0;
    const span = $match_0.$1;
    return object2([["kind", string2("ExportValue")], ["name", string2(name)], ["span", encodeSpan2(span)]]);
  }
  if ($match_0.$tag === 1) {
    const name = $match_0.$0;
    const span = $match_0.$1;
    return object2([["kind", string2("ExportOperator")], ["operator", string2(name)], ["span", encodeSpan2(span)]]);
  }
  if ($match_0.$tag === 2) {
    const name = $match_0.$0;
    const span = $match_0.$1;
    return object2([["kind", string2("ExportTypeAll")], ["name", string2(name)], ["span", encodeSpan2(span)]]);
  }
  if ($match_0.$tag === 3) {
    const name = $match_0.$0;
    const members = $match_0.$1;
    const span = $match_0.$2;
    return object2([["kind", string2("ExportTypeSome")], ["name", string2(name)], ["members", list2(string2)(members)], ["span", encodeSpan2(span)]]);
  }
  throw new Error("Pattern match failed");
})(spec);
var encodeExposing = (exp) => (($match_1) => {
  if ($match_1.$tag === 0) {
    const span = $match_1.$0;
    return object2([["kind", string2("All")], ["span", encodeSpan2(span)]]);
  }
  if ($match_1.$tag === 1) {
    const specs = $match_1.$0;
    const span = $match_1.$1;
    return object2([["kind", string2("Explicit")], ["exports", list2(encodeExportSpec)(specs)], ["span", encodeSpan2(span)]]);
  }
  throw new Error("Pattern match failed");
})(exp);
var encodeModuleDecl = (m) => ((exposingVal) => object2([["name", string2(m.name)], ["exposing", exposingVal], ["span", encodeSpan2(m.span)]]))(m.hasExposing ? encodeExposing(m.exposingClause) : $null);
var encodeImport = (imp) => ((aliasVal) => ((exposingVal) => object2([["moduleName", string2(imp.moduleName)], ["alias", aliasVal], ["exposing", exposingVal], ["span", encodeSpan2(imp.span)]]))(imp.hasExposing ? encodeExposing(imp.exposingClause) : $null))(imp.hasAlias ? string2(imp.aliasName) : $null);
var encodePattern;
var encodeRecordFieldPattern;
encodePattern = (pat) => (($match_2) => {
  if ($match_2.$tag === 0) {
    const name = $match_2.$0;
    const span = $match_2.$1;
    return object2([["kind", string2("VarPattern")], ["name", string2(name)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 1) {
    const span = $match_2.$0;
    return object2([["kind", string2("WildcardPattern")], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 2) {
    const name = $match_2.$0;
    const args = $match_2.$1;
    const span = $match_2.$2;
    return object2([["kind", string2("ConstructorPattern")], ["name", string2(name)], ["args", list2(encodePattern)(args)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 3) {
    const elems = $match_2.$0;
    const span = $match_2.$1;
    return object2([["kind", string2("TuplePattern")], ["elements", list2(encodePattern)(elems)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 4) {
    const elems = $match_2.$0;
    const span = $match_2.$1;
    return object2([["kind", string2("ListPattern")], ["elements", list2(encodePattern)(elems)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 5) {
    const head2 = $match_2.$0;
    const tail = $match_2.$1;
    const span = $match_2.$2;
    return object2([["kind", string2("ConsPattern")], ["head", encodePattern(head2)], ["tail", encodePattern(tail)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 6) {
    const fields = $match_2.$0;
    const span = $match_2.$1;
    return object2([["kind", string2("RecordPattern")], ["fields", list2(encodeRecordFieldPattern)(fields)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 7) {
    const value2 = $match_2.$0;
    const span = $match_2.$1;
    return object2([["kind", string2("IntPattern")], ["value", string2(value2)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 8) {
    const value2 = $match_2.$0;
    const span = $match_2.$1;
    return object2([["kind", string2("FloatPattern")], ["value", string2(value2)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 9) {
    const value2 = $match_2.$0;
    const span = $match_2.$1;
    return object2([["kind", string2("StringPattern")], ["value", string2(value2)], ["span", encodeSpan2(span)]]);
  }
  if ($match_2.$tag === 10) {
    const value2 = $match_2.$0;
    const span = $match_2.$1;
    return object2([["kind", string2("CharPattern")], ["value", string2(value2)], ["span", encodeSpan2(span)]]);
  }
  throw new Error("Pattern match failed");
})(pat);
encodeRecordFieldPattern = (rfp) => object2([["name", string2(rfp.name)], ["pattern", encodePattern(rfp.pattern)]]);
var encodeExpr;
var encodeRecordField;
var encodeCaseBranch;
var encodeValueDecl;
encodeExpr = (expr) => (($match_3) => {
  if ($match_3.$tag === 0) {
    const name = $match_3.$0;
    const ns = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("Var")], ["name", string2(name)], ["namespace", string2(ns)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 1) {
    const value2 = $match_3.$0;
    const span = $match_3.$1;
    return object2([["kind", string2("Number")], ["value", string2(value2)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 2) {
    const value2 = $match_3.$0;
    const span = $match_3.$1;
    return object2([["kind", string2("Number")], ["value", string2(value2)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 3) {
    const value2 = $match_3.$0;
    const span = $match_3.$1;
    return object2([["kind", string2("String")], ["value", string2(value2)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 4) {
    const value2 = $match_3.$0;
    const span = $match_3.$1;
    return object2([["kind", string2("Char")], ["value", string2(value2)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 5) {
    const args = $match_3.$0;
    const body = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("Lambda")], ["args", list2(encodePattern)(args)], ["body", encodeExpr(body)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 6) {
    const callee = $match_3.$0;
    const args = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("Apply")], ["callee", encodeExpr(callee)], ["args", list2(encodeExpr)(args)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 7) {
    const cond = $match_3.$0;
    const thenBranch = $match_3.$1;
    const elseBranch = $match_3.$2;
    const span = $match_3.$3;
    return object2([["kind", string2("If")], ["condition", encodeExpr(cond)], ["thenBranch", encodeExpr(thenBranch)], ["elseBranch", encodeExpr(elseBranch)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 8) {
    const bindings = $match_3.$0;
    const body = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("LetIn")], ["bindings", list2(encodeValueDecl)(bindings)], ["body", encodeExpr(body)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 9) {
    const discriminant = $match_3.$0;
    const branches = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("Case")], ["discriminant", encodeExpr(discriminant)], ["branches", list2(encodeCaseBranch)(branches)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 10) {
    const left = $match_3.$0;
    const op = $match_3.$1;
    const right = $match_3.$2;
    const span = $match_3.$3;
    return object2([["kind", string2("Infix")], ["left", encodeExpr(left)], ["operator", string2(op)], ["right", encodeExpr(right)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 11) {
    const op = $match_3.$0;
    const operand = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("Unary")], ["operator", string2(op)], ["operand", encodeExpr(operand)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 12) {
    const inner = $match_3.$0;
    const span = $match_3.$1;
    return object2([["kind", string2("Paren")], ["expression", encodeExpr(inner)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 13) {
    const elems = $match_3.$0;
    const span = $match_3.$1;
    return object2([["kind", string2("Tuple")], ["elements", list2(encodeExpr)(elems)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 14) {
    const span = $match_3.$0;
    return object2([["kind", string2("Unit")], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 15) {
    const elems = $match_3.$0;
    const span = $match_3.$1;
    return object2([["kind", string2("List")], ["elements", list2(encodeExpr)(elems)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 16) {
    const start = $match_3.$0;
    const end = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("ListRange")], ["start", encodeExpr(start)], ["end", encodeExpr(end)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 17) {
    const fields = $match_3.$0;
    const span = $match_3.$1;
    return object2([["kind", string2("Record")], ["fields", list2(encodeRecordField)(fields)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 18) {
    const base = $match_3.$0;
    const fields = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("RecordUpdate")], ["base", string2(base)], ["fields", list2(encodeRecordField)(fields)], ["span", encodeSpan2(span)]]);
  }
  if ($match_3.$tag === 19) {
    const target = $match_3.$0;
    const field2 = $match_3.$1;
    const span = $match_3.$2;
    return object2([["kind", string2("FieldAccess")], ["target", encodeExpr(target)], ["field", string2(field2)], ["span", encodeSpan2(span)]]);
  }
  throw new Error("Pattern match failed");
})(expr);
encodeRecordField = (rf) => object2([["name", string2(rf.name)], ["value", encodeExpr(rf.value)], ["span", encodeSpan2(rf.span)]]);
encodeCaseBranch = (cb) => object2([["pattern", encodePattern(cb.pattern)], ["body", encodeExpr(cb.body)], ["span", encodeSpan2(cb.span)]]);
encodeValueDecl = (vd) => object2([["kind", string2("ValueDeclaration")], ["name", string2(vd.name)], ["args", list2(encodePattern)(vd.args)], ["body", encodeExpr(vd.body)], ["span", encodeSpan2(vd.span)]]);
var encodeTypeExpr;
var encodeConstraint;
var encodeRecordFieldType;
encodeTypeExpr = (te) => (($match_4) => {
  if ($match_4.$tag === 0) {
    const name = $match_4.$0;
    const args = $match_4.$1;
    const span = $match_4.$2;
    return object2([["kind", string2("TypeRef")], ["name", string2(name)], ["args", list2(encodeTypeExpr)(args)], ["span", encodeSpan2(span)]]);
  }
  if ($match_4.$tag === 1) {
    const from = $match_4.$0;
    const to = $match_4.$1;
    const span = $match_4.$2;
    return object2([["kind", string2("FunctionType")], ["from", encodeTypeExpr(from)], ["to", encodeTypeExpr(to)], ["span", encodeSpan2(span)]]);
  }
  if ($match_4.$tag === 2) {
    const elems = $match_4.$0;
    const span = $match_4.$1;
    return object2([["kind", string2("TupleType")], ["elements", list2(encodeTypeExpr)(elems)], ["span", encodeSpan2(span)]]);
  }
  if ($match_4.$tag === 3) {
    const fields = $match_4.$0;
    const span = $match_4.$1;
    return object2([["kind", string2("RecordType")], ["fields", list2(encodeRecordFieldType)(fields)], ["span", encodeSpan2(span)]]);
  }
  if ($match_4.$tag === 4) {
    const constraints = $match_4.$0;
    const typ = $match_4.$1;
    const span = $match_4.$2;
    return object2([["kind", string2("QualifiedType")], ["constraints", list2(encodeConstraint)(constraints)], ["type", encodeTypeExpr(typ)], ["span", encodeSpan2(span)]]);
  }
  throw new Error("Pattern match failed");
})(te);
encodeConstraint = (c) => object2([["protocolName", string2(c.protocolName)], ["typeArgs", list2(encodeTypeExpr)(c.typeArgs)], ["span", encodeSpan2(c.span)]]);
encodeRecordFieldType = (rft) => object2([["name", string2(rft.name)], ["type", encodeTypeExpr(rft.fieldType)], ["span", encodeSpan2(rft.span)]]);
var encodeConstructor = (cv) => object2([["name", string2(cv.name)], ["args", list2(encodeTypeExpr)(cv.args)], ["span", encodeSpan2(cv.span)]]);
var encodeProtocolMethod = (pm) => ((typeVal) => ((defaultVal) => object2([["name", string2(pm.name)], ["type", typeVal], ["defaultImpl", defaultVal], ["span", encodeSpan2(pm.span)]]))(pm.hasDefault ? object2([["args", list2(encodePattern)(pm.defaultArgs)], ["body", encodeExpr(pm.defaultBody)]]) : $null))(pm.hasType ? encodeTypeExpr(pm.methodType) : $null);
var encodeMethodImpl = (mi) => ((argsVal) => object2([["name", string2(mi.name)], ["args", argsVal], ["implementation", encodeExpr(mi.implementation)], ["span", encodeSpan2(mi.span)]]))((($match_5) => {
  if (Array.isArray($match_5) && $match_5.length === 0) {
    return $null;
  }
  {
    return list2(encodePattern)(mi.implArgs);
  }
  throw new Error("Pattern match failed");
})(mi.implArgs));
var encodeDeclaration = (decl) => (($match_6) => {
  if ($match_6.$tag === 0) {
    const vd = $match_6.$0;
    return object2([["kind", string2("ValueDeclaration")], ["name", string2(vd.name)], ["args", list2(encodePattern)(vd.args)], ["body", encodeExpr(vd.body)], ["span", encodeSpan2(vd.span)]]);
  }
  if ($match_6.$tag === 1) {
    const ta = $match_6.$0;
    return object2([["kind", string2("TypeAnnotationDeclaration")], ["name", string2(ta.name)], ["annotation", encodeTypeExpr(ta.annotation)], ["span", encodeSpan2(ta.span)]]);
  }
  if ($match_6.$tag === 2) {
    const decorator = $match_6.$0;
    const args = $match_6.$1;
    const name = $match_6.$2;
    const annotation = $match_6.$3;
    const span = $match_6.$4;
    return object2([["kind", string2("DecoratedDeclaration")], ["decorator", string2(decorator)], ["args", list2(string2)(args)], ["name", string2(name)], ["annotation", encodeTypeExpr(annotation)], ["span", encodeSpan2(span)]]);
  }
  if ($match_6.$tag === 3) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const constraints = $match_6.$2;
    const ctors = $match_6.$3;
    const span = $match_6.$4;
    return object2([["kind", string2("TypeDeclaration")], ["name", string2(name)], ["params", list2(string2)(params)], ["constraints", list2(encodeConstraint)(constraints)], ["constructors", list2(encodeConstructor)(ctors)], ["span", encodeSpan2(span)]]);
  }
  if ($match_6.$tag === 4) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const value2 = $match_6.$2;
    const span = $match_6.$3;
    return object2([["kind", string2("TypeAliasDeclaration")], ["name", string2(name)], ["params", list2(string2)(params)], ["value", encodeTypeExpr(value2)], ["span", encodeSpan2(span)]]);
  }
  if ($match_6.$tag === 5) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const span = $match_6.$2;
    return object2([["kind", string2("OpaqueTypeDeclaration")], ["name", string2(name)], ["params", list2(string2)(params)], ["span", encodeSpan2(span)]]);
  }
  if ($match_6.$tag === 6) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const constraints = $match_6.$2;
    const fields = $match_6.$3;
    const span = $match_6.$4;
    return object2([["kind", string2("TypeDeclaration")], ["name", string2(name)], ["params", list2(string2)(params)], ["constraints", list2(encodeConstraint)(constraints)], ["recordFields", list2(encodeRecordFieldType)(fields)], ["span", encodeSpan2(span)]]);
  }
  if ($match_6.$tag === 7) {
    const name = $match_6.$0;
    const params = $match_6.$1;
    const constraints = $match_6.$2;
    const methods = $match_6.$3;
    const span = $match_6.$4;
    return object2([["kind", string2("ProtocolDeclaration")], ["name", string2(name)], ["params", list2(string2)(params)], ["constraints", list2(encodeConstraint)(constraints)], ["methods", list2(encodeProtocolMethod)(methods)], ["span", encodeSpan2(span)]]);
  }
  if ($match_6.$tag === 8) {
    const constraints = $match_6.$0;
    const protoName = $match_6.$1;
    const typeArgs = $match_6.$2;
    const methods = $match_6.$3;
    const span = $match_6.$4;
    return object2([["kind", string2("ImplementationDeclaration")], ["constraints", list2(encodeConstraint)(constraints)], ["protocolName", string2(protoName)], ["typeArgs", list2(encodeTypeExpr)(typeArgs)], ["methods", list2(encodeMethodImpl)(methods)], ["span", encodeSpan2(span)]]);
  }
  if ($match_6.$tag === 9) {
    const assoc = $match_6.$0;
    const prec = $match_6.$1;
    const name = $match_6.$2;
    const span = $match_6.$3;
    return ((fixity) => object2([["kind", string2("InfixDeclaration")], ["fixity", string2(fixity)], ["precedence", int2(prec)], ["operator", string2(name)], ["span", encodeSpan2(span)]]))((($match_7) => {
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
var encodeProgram = (prog) => object2([["module", encodeModuleDecl(prog.moduleDecl)], ["imports", list2(encodeImport)(prog.imports)], ["declarations", list2(encodeDeclaration)(prog.declarations)]]);
var encodeAssociativity = (assoc) => (($match_9) => {
  if ($match_9.$tag === 0) {
    return string2("left");
  }
  if ($match_9.$tag === 1) {
    return string2("right");
  }
  if ($match_9.$tag === 2) {
    return string2("none");
  }
  throw new Error("Pattern match failed");
})(assoc);
var encodeRegistryEntry = (entry) => (($match_10) => {
  {
    const op = $match_10[0];
    const info = $match_10[1];
    return object2([["op", string2(op)], ["precedence", int2(info.precedence)], ["associativity", encodeAssociativity(info.associativity)]]);
  }
  throw new Error("Pattern match failed");
})(entry);
var encodeRegistry = (registry) => list2(encodeRegistryEntry)(toList3(registry));

// ../vibe-parser/dist/VibeParser/VibeParser.js
var _AMP_AMP8 = (a) => (b) => a && b();
var _PIPE_PIPE8 = (a) => (b) => a || b();
var findOperator = (tokens) => (($match_1) => {
  if (Array.isArray($match_1) && $match_1.length === 0) {
    return Nothing2;
  }
  if (Array.isArray($match_1) && $match_1.length >= 1) {
    const tok = $match_1[0];
    const rest = $match_1.slice(1);
    return $dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2) ? Just2([tok.lexeme, rest]) : $dict_Eq_TokenKind._EQ_EQ(tok.kind)(LParen2) ? (($match_2) => {
      if (Array.isArray($match_2) && $match_2.length >= 1) {
        const opTok = $match_2[0];
        const rest2 = $match_2.slice(1);
        return $dict_Eq_TokenKind._EQ_EQ(opTok.kind)(Operator2) ? (($match_3) => {
          if (Array.isArray($match_3) && $match_3.length >= 1) {
            const closeParen = $match_3[0];
            const rest3 = $match_3.slice(1);
            return $dict_Eq_TokenKind._EQ_EQ(closeParen.kind)(RParen2) ? Just2([opTok.lexeme, rest3]) : Nothing2;
          }
          {
            return Nothing2;
          }
          throw new Error("Pattern match failed");
        })(rest2) : Nothing2;
      }
      {
        return Nothing2;
      }
      throw new Error("Pattern match failed");
    })(rest) : Nothing2;
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
})(toInt2(s));
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
        if (_AMP_AMP8($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => _PIPE_PIPE8($dict_Eq_String2._EQ_EQ(tok.lexeme)("infix"))(() => _PIPE_PIPE8($dict_Eq_String2._EQ_EQ(tok.lexeme)("infixl"))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("infixr"))))) {
          {
            const $match_6 = rest;
            if (Array.isArray($match_6) && $match_6.length >= 1) {
              const precTok = $match_6[0];
              const opRest = $match_6.slice(1);
              if ($dict_Eq_TokenKind._EQ_EQ(precTok.kind)(NumberToken2)) {
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
var encodeError = (message) => (span) => encode2(0)(object2([["ok", bool2(false)], ["message", string2(message)], ["span", encodeSpan2(span)]]));
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
        return encode2(0)(object2([["ok", bool2(true)], ["program", encodeProgram(program)], ["registry", encodeRegistry(registry)]]));
      }
      throw new Error("Pattern match failed");
    })(parseTokens(layoutTokens)(registry)))(insertLayoutTokens(tokens)))(collectInfixFromTokens(tokens)(builtinRegistry));
  }
  throw new Error("Pattern match failed");
})(lex3(source));
var assocDecoder = ((decode) => andThen(decode)(string3))((s) => (($match_13) => {
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
var registryEntryDecoder = map3((op) => (p) => (a) => [op, { precedence: p, associativity: a }])(field("op")(string3))(field("precedence")(int3))(field("associativity")(assocDecoder));
var registryDecoder = list3(registryEntryDecoder);

// ../parser/src/index.ts
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
  const map4 = new Map;
  for (const entry of entries) {
    map4.set(entry.op, {
      precedence: entry.precedence,
      associativity: entry.associativity
    });
  }
  return map4;
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

// ../syntax/src/operators.ts
var OPERATOR_CHARS = new Set(Array.from("!#$%&*+./<=>?@%\\^|~:-"));
var BUILTIN_OPERATORS = [
  {
    symbol: "&&",
    fixity: { associativity: "right", precedence: 3 },
    isShortCircuit: true,
    helper: { name: "_AMP_AMP", impl: "(a) => (b) => a && b()" }
  },
  {
    symbol: "||",
    fixity: { associativity: "right", precedence: 2 },
    isShortCircuit: true,
    helper: { name: "_PIPE_PIPE", impl: "(a) => (b) => a || b()" }
  }
];
var SHORT_CIRCUIT_OPERATORS = new Set(BUILTIN_OPERATORS.filter((op) => op.isShortCircuit).map((op) => op.symbol));
var BUILTIN_OPERATOR_FIXITY = Object.fromEntries(BUILTIN_OPERATORS.map((op) => [op.symbol, op.fixity]));
var SHORT_CIRCUIT_HELPERS = Object.fromEntries(BUILTIN_OPERATORS.filter((op) => op.helper).map((op) => [
  op.symbol,
  op.helper
]));
// ../syntax/src/index.ts
var KEYWORDS = [
  "if",
  "then",
  "else",
  "let",
  "in",
  "case",
  "of",
  "type",
  "alias",
  "module",
  "import",
  "exposing",
  "as",
  "port",
  "infix",
  "infixl",
  "infixr",
  "protocol",
  "implement",
  "where"
];
var KEYWORD_SET = new Set(KEYWORDS);
var BUILTIN_MODULE_NAME = "__builtin__";

// ../semantics/src/errors.ts
class SemanticError extends Error {
  span;
  filePath;
  constructor(message, span, filePath) {
    super(message);
    this.span = span;
    this.filePath = filePath;
  }
}
class MultipleSemanticErrors extends Error {
  errors;
  constructor(errors) {
    const count = errors.length;
    const summary = count === 1 ? errors[0].message : `${count} semantic error(s) found`;
    super(summary);
    this.errors = errors;
    this.name = "MultipleSemanticErrors";
  }
}
// ../semantics/src/types.ts
class Scope {
  _parent;
  _symbols;
  constructor(parent) {
    this._parent = parent;
    this._symbols = new Map;
  }
  get parent() {
    return this._parent;
  }
  get symbols() {
    return this._symbols;
  }
  define(name, scheme) {
    this._symbols.set(name, scheme);
  }
  has(name) {
    return this._symbols.has(name);
  }
  get(name) {
    return this._symbols.get(name);
  }
  lookup(name) {
    if (this._symbols.has(name)) {
      return this._symbols.get(name);
    }
    if (this._parent) {
      return this._parent.lookup(name);
    }
    return;
  }
  child() {
    return new Scope(this);
  }
  getLocalNames() {
    return Array.from(this._symbols.keys());
  }
}

class RegistryManager {
  adts = {};
  constructors = {};
  constructorTypes = {};
  typeAliases = {};
  records = {};
  opaqueTypes = {};
  protocols = {};
  instances = [];
  operators = new Map;
  values = {};
  typeSchemes = {};
  types = {};
  annotations = {};
  localInstances = [];
  infixDeclarations = [];
  localProtocolMethods = new Set;
  importedValues = new Map;
  registerADT(info) {
    this.adts[info.name] = info;
  }
  registerConstructor(name, info) {
    this.constructors[name] = info;
  }
  registerConstructorType(name, scheme) {
    this.constructorTypes[name] = scheme;
  }
  registerTypeAlias(info) {
    this.typeAliases[info.name] = info;
  }
  registerRecord(info) {
    this.records[info.name] = info;
  }
  registerOpaqueType(info) {
    this.opaqueTypes[info.name] = info;
  }
  registerProtocol(info) {
    this.protocols[info.name] = info;
  }
  registerInstance(info) {
    this.instances.push(info);
  }
  registerOperator(name, info) {
    this.operators.set(name, info);
  }
  registerValue(name, info) {
    this.values[name] = info;
  }
  registerTypeScheme(name, scheme) {
    this.typeSchemes[name] = scheme;
  }
  registerType(name, type) {
    this.types[name] = type;
  }
  hasADT(name) {
    return name in this.adts;
  }
  hasConstructor(name) {
    return name in this.constructors;
  }
  hasTypeAlias(name) {
    return name in this.typeAliases;
  }
  hasOpaqueType(name) {
    return name in this.opaqueTypes;
  }
  hasProtocol(name) {
    return name in this.protocols;
  }
  hasRecord(name) {
    return name in this.records;
  }
  getADT(name) {
    return this.adts[name];
  }
  getConstructor(name) {
    return this.constructors[name];
  }
  getConstructorType(name) {
    return this.constructorTypes[name];
  }
  getTypeAlias(name) {
    return this.typeAliases[name];
  }
  getRecord(name) {
    return this.records[name];
  }
  getOpaqueType(name) {
    return this.opaqueTypes[name];
  }
  getProtocol(name) {
    return this.protocols[name];
  }
  registerAnnotation(name, ann) {
    this.annotations[name] = ann;
  }
  getAnnotation(name) {
    return this.annotations[name];
  }
  registerLocalInstance(info) {
    this.localInstances.push(info);
  }
  registerInfixDeclaration(decl) {
    this.infixDeclarations.push(decl);
  }
  registerLocalProtocolMethod(name) {
    this.localProtocolMethods.add(name);
  }
  hasLocalProtocolMethod(name) {
    return this.localProtocolMethods.has(name);
  }
  registerImportedValue(name, moduleName) {
    this.importedValues.set(name, moduleName);
  }
  getImportedValueSource(name) {
    return this.importedValues.get(name);
  }
  isValueImported(name) {
    return this.importedValues.has(name);
  }
}
// ../semantics/src/utils.ts
var nextTypeVarId = 0;
function freshType() {
  return { kind: "var", id: nextTypeVarId++ };
}
function listType(element) {
  return { kind: "con", name: "List", args: [element] };
}
function fn(a, b, c) {
  if (c) {
    return fn(a, fn(b, c));
  }
  return { kind: "fun", from: a, to: b };
}
function fnChain(args, result) {
  return args.reduceRight((acc, arg) => fn(arg, acc), result);
}
function typesEqual(t1, t2) {
  if (t1.kind !== t2.kind)
    return false;
  switch (t1.kind) {
    case "var":
      return t2.id === t1.id;
    case "con":
      return t2.name === t1.name && t1.args.length === t2.args.length && t1.args.every((a, i) => typesEqual(a, t2.args[i]));
    case "fun":
      return typesEqual(t1.from, t2.from) && typesEqual(t1.to, t2.to);
    case "tuple":
      return t1.elements.length === t2.elements.length && t1.elements.every((e, i) => typesEqual(e, t2.elements[i]));
    case "record": {
      const r2 = t2;
      const keys1 = Object.keys(t1.fields).sort();
      const keys2 = Object.keys(r2.fields).sort();
      return keys1.length === keys2.length && keys1.every((k, i) => k === keys2[i] && typesEqual(t1.fields[k], r2.fields[k]));
    }
    case "error":
      return true;
  }
}
function applySubstitution(type, substitution) {
  if (type.kind === "var") {
    const replacement = substitution.get(type.id);
    return replacement ? applySubstitution(replacement, substitution) : type;
  }
  if (type.kind === "fun") {
    return fn(applySubstitution(type.from, substitution), applySubstitution(type.to, substitution));
  }
  if (type.kind === "tuple") {
    return {
      kind: "tuple",
      elements: type.elements.map((t) => applySubstitution(t, substitution))
    };
  }
  if (type.kind === "record") {
    const fields = {};
    for (const [k, v] of Object.entries(type.fields)) {
      fields[k] = applySubstitution(v, substitution);
    }
    return { kind: "record", fields };
  }
  if (type.kind === "con") {
    return {
      kind: "con",
      name: type.name,
      args: type.args.map((t) => applySubstitution(t, substitution))
    };
  }
  return type;
}
function applySubstitutionToConstraints(constraints, substitution) {
  return constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((t) => applySubstitution(t, substitution))
  }));
}
function getFreeTypeVars(type, substitution) {
  const concrete = applySubstitution(type, substitution);
  if (concrete.kind === "var") {
    return new Set([concrete.id]);
  }
  if (concrete.kind === "con") {
    const result = new Set;
    for (const arg of concrete.args) {
      for (const v of getFreeTypeVars(arg, substitution)) {
        result.add(v);
      }
    }
    return result;
  }
  if (concrete.kind === "fun") {
    const result = new Set;
    for (const v of getFreeTypeVars(concrete.from, substitution)) {
      result.add(v);
    }
    for (const v of getFreeTypeVars(concrete.to, substitution)) {
      result.add(v);
    }
    return result;
  }
  if (concrete.kind === "tuple") {
    const result = new Set;
    for (const el of concrete.elements) {
      for (const v of getFreeTypeVars(el, substitution)) {
        result.add(v);
      }
    }
    return result;
  }
  if (concrete.kind === "record") {
    const result = new Set;
    for (const fieldType of Object.values(concrete.fields)) {
      for (const v of getFreeTypeVars(fieldType, substitution)) {
        result.add(v);
      }
    }
    return result;
  }
  return new Set;
}
function getFreeTypeVarsInScope(scope, substitution) {
  const result = new Set;
  for (const scheme of scope.symbols.values()) {
    const typeFree = getFreeTypeVars(scheme.type, substitution);
    for (const v of typeFree) {
      if (!scheme.vars.has(v)) {
        result.add(v);
      }
    }
  }
  if (scope.parent) {
    for (const v of getFreeTypeVarsInScope(scope.parent, substitution)) {
      result.add(v);
    }
  }
  return result;
}
function collectTypeVarIds(type) {
  const ids = new Set;
  collectTypeVarIdsHelper(type, ids);
  return ids;
}
function collectTypeVarIdsHelper(type, ids) {
  switch (type.kind) {
    case "var":
      ids.add(type.id);
      break;
    case "fun":
      collectTypeVarIdsHelper(type.from, ids);
      collectTypeVarIdsHelper(type.to, ids);
      break;
    case "tuple":
      for (const el of type.elements) {
        collectTypeVarIdsHelper(el, ids);
      }
      break;
    case "con":
      for (const arg of type.args) {
        collectTypeVarIdsHelper(arg, ids);
      }
      break;
    case "record":
      for (const v of Object.values(type.fields)) {
        collectTypeVarIdsHelper(v, ids);
      }
      break;
  }
}
function collectTypeVarIdsOrdered(type, result, seen) {
  switch (type.kind) {
    case "var":
      if (!seen.has(type.id)) {
        seen.add(type.id);
        result.push(type.id);
      }
      break;
    case "con":
      for (const arg of type.args) {
        collectTypeVarIdsOrdered(arg, result, seen);
      }
      break;
    case "fun":
      collectTypeVarIdsOrdered(type.from, result, seen);
      collectTypeVarIdsOrdered(type.to, result, seen);
      break;
    case "tuple":
      for (const el of type.elements) {
        collectTypeVarIdsOrdered(el, result, seen);
      }
      break;
    case "record":
      for (const v of Object.values(type.fields)) {
        collectTypeVarIdsOrdered(v, result, seen);
      }
      break;
  }
}
var LETTERS = "abcdefghijklmnopqrstuvwxyz";
function formatType(type) {
  switch (type.kind) {
    case "var":
      return `t${type.id}`;
    case "con":
      if (type.args.length === 0) {
        return type.name;
      }
      return `${type.name} ${type.args.map(formatType).join(" ")}`;
    case "fun":
      const from = type.from.kind === "fun" ? `(${formatType(type.from)})` : formatType(type.from);
      return `${from} -> ${formatType(type.to)}`;
    case "tuple":
      return `(${type.elements.map(formatType).join(", ")})`;
    case "record":
      const fields = Object.entries(type.fields).map(([k, v]) => `${k}: ${formatType(v)}`).join(", ");
      return `{ ${fields} }`;
    case "error":
      return "<error>";
  }
}
function buildNormalizedNames(scheme) {
  const ids = [];
  const seen = new Set;
  for (const c of scheme.constraints) {
    for (const t of c.typeArgs) {
      collectTypeVarIdsOrdered(t, ids, seen);
    }
  }
  collectTypeVarIdsOrdered(scheme.type, ids, seen);
  const map4 = new Map;
  for (let i = 0;i < ids.length; i++) {
    map4.set(ids[i], i < LETTERS.length ? LETTERS[i] : `t${ids[i]}`);
  }
  return map4;
}
function formatTypeForDisplay(type, paramNames) {
  switch (type.kind) {
    case "var":
      return paramNames.get(type.id) ?? `t${type.id}`;
    case "con":
      if (type.args.length === 0)
        return type.name;
      return `${type.name} ${type.args.map((a) => formatTypeArgForDisplay(a, paramNames)).join(" ")}`;
    case "fun": {
      const from = formatTypeArgForDisplay(type.from, paramNames);
      return `${from} -> ${formatTypeForDisplay(type.to, paramNames)}`;
    }
    case "tuple":
      return `(${type.elements.map((e) => formatTypeForDisplay(e, paramNames)).join(", ")})`;
    case "record": {
      const fields = Object.entries(type.fields).map(([k, v]) => `${k} : ${formatTypeForDisplay(v, paramNames)}`).join(", ");
      return `{ ${fields} }`;
    }
    case "error":
      return "<error>";
  }
}
function formatTypeArgForDisplay(type, paramNames) {
  if (type.kind === "fun" || type.kind === "con" && type.args.length > 0) {
    return `(${formatTypeForDisplay(type, paramNames)})`;
  }
  return formatTypeForDisplay(type, paramNames);
}
function formatConstraintsForDisplay(constraints, paramNames) {
  if (!constraints || constraints.length === 0)
    return "";
  const parts = constraints.map((c) => {
    const args = c.typeArgs.map((t) => formatTypeForDisplay(t, paramNames)).join(" ");
    return `${c.protocolName} ${args}`;
  });
  if (parts.length === 1)
    return parts[0] ?? "";
  return `(${parts.join(", ")})`;
}
function formatTypeSchemeForDisplay(scheme) {
  if (!scheme || !scheme.type)
    return "<unknown type>";
  const names = scheme.paramNames ?? buildNormalizedNames(scheme);
  const constraintStr = formatConstraintsForDisplay(scheme.constraints, names);
  const typeStr = formatTypeForDisplay(scheme.type, names);
  return constraintStr ? `${constraintStr} => ${typeStr}` : typeStr;
}
function createConstraintContext() {
  return { constraints: [] };
}
function addConstraint(ctx, constraint) {
  const isDuplicate = ctx.constraints.some((c) => c.protocolName === constraint.protocolName && c.typeArgs.length === constraint.typeArgs.length && c.typeArgs.every((t, i) => typesEqual(t, constraint.typeArgs[i])));
  if (!isDuplicate) {
    ctx.constraints.push(constraint);
  }
}
function flattenFunctionParams(type) {
  const params = [];
  let current2 = type;
  while (current2.kind === "fun") {
    params.push(current2.from);
    current2 = current2.to;
  }
  return params;
}
function applyVarSubstitution(type, subst) {
  switch (type.kind) {
    case "var": {
      const replacement = subst.get(type.id);
      return replacement ?? type;
    }
    case "fun":
      return {
        kind: "fun",
        from: applyVarSubstitution(type.from, subst),
        to: applyVarSubstitution(type.to, subst)
      };
    case "tuple":
      return {
        kind: "tuple",
        elements: type.elements.map((el) => applyVarSubstitution(el, subst))
      };
    case "con":
      return {
        kind: "con",
        name: type.name,
        args: type.args.map((arg) => applyVarSubstitution(arg, subst))
      };
    case "record":
      const fields = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = applyVarSubstitution(v, subst);
      }
      return { kind: "record", fields };
    default:
      return type;
  }
}
function applyTypeSubstitution(type, substitution) {
  switch (type.kind) {
    case "var": {
      const mapped = substitution.get(type.id);
      return mapped ?? type;
    }
    case "con":
      return {
        kind: "con",
        name: type.name,
        args: type.args.map((arg) => applyTypeSubstitution(arg, substitution))
      };
    case "fun":
      return {
        kind: "fun",
        from: applyTypeSubstitution(type.from, substitution),
        to: applyTypeSubstitution(type.to, substitution)
      };
    case "tuple":
      return {
        kind: "tuple",
        elements: type.elements.map((el) => applyTypeSubstitution(el, substitution))
      };
    case "record": {
      const fields = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = applyTypeSubstitution(v, substitution);
      }
      return { kind: "record", fields };
    }
    case "error":
      return type;
  }
}
// ../semantics/src/builtins.ts
var BUILTIN_CONSTRUCTORS = {
  True: 0,
  False: 0,
  Unit: 0,
  Int: 0,
  Float: 0,
  String: 0,
  Char: 0
};
var BOOL_TYPE = { kind: "con", name: "Bool", args: [] };
var INFIX_TYPES = {
  "&&": {
    kind: "fun",
    from: BOOL_TYPE,
    to: { kind: "fun", from: BOOL_TYPE, to: BOOL_TYPE }
  },
  "||": {
    kind: "fun",
    from: BOOL_TYPE,
    to: { kind: "fun", from: BOOL_TYPE, to: BOOL_TYPE }
  }
};
var BUILTIN_SPAN = {
  start: { offset: 0, line: 0, column: 0 },
  end: { offset: 0, line: 0, column: 0 }
};

// ../semantics/src/exhaustiveness.ts
function checkExhaustiveness(branches, scrutineeType, adts, constructors, resolveConstructor) {
  const resolver = resolveConstructor || ((name) => {
    const info = constructors[name];
    if (info) {
      const adt = adts[info.parentType];
      if (adt)
        return { name, info, adt };
    }
    return;
  });
  const matrix = branches.map((p) => [p]);
  const witness = isUseful(matrix, [{ kind: "WildcardPattern", span: createDummySpan() }], adts, constructors, resolver);
  if (witness) {
    return { exhaustive: false, missing: patternToString(witness[0]) };
  }
  return { exhaustive: true };
}
function isUseful(matrix, vector, adts, constructors, resolver) {
  if (matrix.length === 0) {
    return vector;
  }
  if (vector.length === 0) {
    return null;
  }
  const p = vector[0];
  const restVector = vector.slice(1);
  if (isConstructorLike(p)) {
    const { name, args } = getConstructorDecomposition(p);
    const resolved = resolver(name);
    const canonicalName = resolved ? resolved.name : name;
    const specializedMatrix = specialize(matrix, canonicalName, args.length, adts, constructors, resolver);
    const witness = isUseful(specializedMatrix, [...args, ...restVector], adts, constructors, resolver);
    if (witness) {
      const witnessArgs = witness.slice(0, args.length);
      const witnessRest = witness.slice(args.length);
      return [reconstructConstructor(p, witnessArgs), ...witnessRest];
    }
    return null;
  }
  if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
    const { names: usedConstructors, adt } = getUsedConstructors(matrix, resolver);
    const typeInfo = inferTypeInfo(usedConstructors, adt, adts, constructors, resolver);
    if (typeInfo.isComplete) {
      for (const ctor of typeInfo.allConstructors) {
        const arity = getConstructorArity(ctor, constructors, adts, resolver, typeInfo.adtModuleName);
        const specializedMatrix = specialize(matrix, ctor, arity, adts, constructors, resolver);
        const dummyArgs = Array(arity).fill({ kind: "WildcardPattern", span: createDummySpan() });
        const witness = isUseful(specializedMatrix, [...dummyArgs, ...restVector], adts, constructors, resolver);
        if (witness) {
          const witnessArgs = witness.slice(0, arity);
          const witnessRest = witness.slice(arity);
          const ctorPattern = createConstructorPattern(ctor, witnessArgs);
          return [ctorPattern, ...witnessRest];
        }
      }
      return null;
    } else {
      const defaultMatrix = specializeDefault(matrix);
      const witness = isUseful(defaultMatrix, restVector, adts, constructors, resolver);
      if (witness) {
        const missingCtor = pickMissingConstructor(typeInfo.allConstructors, usedConstructors);
        if (missingCtor) {
          const arity = getConstructorArity(missingCtor, constructors, adts, resolver, typeInfo.adtModuleName);
          const args = Array(arity).fill({ kind: "WildcardPattern", span: createDummySpan() });
          return [createConstructorPattern(missingCtor, args), ...witness];
        } else {
          return [p, ...witness];
        }
      }
      return null;
    }
  }
  return null;
}
function isConstructorLike(p) {
  return p.kind === "ConstructorPattern" || p.kind === "TuplePattern" || p.kind === "ListPattern" || p.kind === "ConsPattern" || p.kind === "IntPattern" || p.kind === "FloatPattern" || p.kind === "StringPattern" || p.kind === "CharPattern";
}
function getConstructorDecomposition(p) {
  switch (p.kind) {
    case "ConstructorPattern":
      return { name: p.name, args: p.args };
    case "TuplePattern":
      return { name: `Tuple${p.elements.length}`, args: p.elements };
    case "ConsPattern":
      return { name: "::", args: [p.head, p.tail] };
    case "ListPattern":
      if (p.elements.length === 0)
        return { name: "[]", args: [] };
      return convertListToCons(p.elements);
    case "IntPattern":
      return { name: `Int:${p.value}`, args: [] };
    case "FloatPattern":
      return { name: `Float:${p.value}`, args: [] };
    case "StringPattern":
      return { name: `String:${p.value}`, args: [] };
    case "CharPattern":
      return { name: `Char:${p.value}`, args: [] };
    default:
      throw new Error(`Unexpected pattern kind in decomposition: ${p.kind}`);
  }
}
function convertListToCons(elements) {
  if (elements.length === 0) {
    return { name: "[]", args: [] };
  }
  const [head2, ...tail] = elements;
  const tailPattern = { kind: "ListPattern", elements: tail, span: createDummySpan() };
  return { name: "::", args: [head2, tailPattern] };
}
function reconstructConstructor(original, args) {
  if (original.kind === "ConstructorPattern") {
    return { ...original, args };
  }
  if (original.kind === "TuplePattern") {
    return { ...original, elements: args };
  }
  if (original.kind === "ConsPattern") {
    return { ...original, head: args[0], tail: args[1] };
  }
  if (original.kind === "ListPattern") {
    return { kind: "ConsPattern", head: args[0], tail: args[1], span: createDummySpan() };
  }
  return original;
}
function createConstructorPattern(name, args) {
  if (name.startsWith("Tuple")) {
    return { kind: "TuplePattern", elements: args, span: createDummySpan() };
  }
  if (name === "::") {
    return { kind: "ConsPattern", head: args[0], tail: args[1], span: createDummySpan() };
  }
  if (name === "[]") {
    return { kind: "ListPattern", elements: [], span: createDummySpan() };
  }
  return { kind: "ConstructorPattern", name, args, span: createDummySpan() };
}
function specialize(matrix, ctorName, arity, adts, constructors, resolver) {
  const newMatrix = [];
  for (const row of matrix) {
    if (row.length === 0)
      continue;
    const p = row[0];
    const rest = row.slice(1);
    if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
      const newArgs = Array(arity).fill({ kind: "WildcardPattern", span: p.span });
      newMatrix.push([...newArgs, ...rest]);
    } else if (isConstructorLike(p)) {
      const { name, args } = getConstructorDecomposition(p);
      const resolved = resolver(name);
      const canonicalName = resolved ? resolved.name : name;
      if (canonicalName === ctorName) {
        newMatrix.push([...args, ...rest]);
      }
    }
  }
  return newMatrix;
}
function specializeDefault(matrix) {
  const newMatrix = [];
  for (const row of matrix) {
    if (row.length === 0)
      continue;
    const p = row[0];
    const rest = row.slice(1);
    if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
      newMatrix.push(rest);
    }
  }
  return newMatrix;
}
function getUsedConstructors(matrix, resolver) {
  const used = new Set;
  let adtInfo;
  for (const row of matrix) {
    if (row.length > 0) {
      const p = row[0];
      if (isConstructorLike(p)) {
        const { name } = getConstructorDecomposition(p);
        const resolved = resolver(name);
        if (resolved) {
          used.add(resolved.name);
          if (!adtInfo && resolved.adt)
            adtInfo = resolved.adt;
        } else {
          used.add(name);
        }
      }
    }
  }
  return { names: used, adt: adtInfo };
}
function inferTypeInfo(usedConstructors, adtContext, adts, constructors, resolver) {
  if (usedConstructors.size === 0) {
    return { isComplete: false, allConstructors: [] };
  }
  const firstCtor = usedConstructors.values().next().value;
  if (firstCtor.startsWith("Tuple")) {
    return { isComplete: true, allConstructors: [firstCtor] };
  }
  if (firstCtor === "::" || firstCtor === "[]") {
    return { isComplete: true, allConstructors: ["[]", "::"] };
  }
  if (adtContext) {
    return {
      isComplete: true,
      allConstructors: adtContext.constructors,
      adtModuleName: adtContext.moduleName
    };
  }
  const resolved = resolver(firstCtor);
  if (resolved) {
    const { info: info2, adt } = resolved;
    return {
      isComplete: true,
      allConstructors: adt.constructors,
      adtModuleName: adt.moduleName
    };
  }
  const info = constructors[firstCtor];
  if (info) {
    const adt = adts[info.parentType];
    if (adt) {
      const all = adt.constructors;
      return { isComplete: true, allConstructors: all, adtModuleName: adt.moduleName };
    }
  }
  return { isComplete: false, allConstructors: [] };
}
function getConstructorArity(name, constructors, adts, resolver, moduleName) {
  if (name === "::")
    return 2;
  if (name === "[]")
    return 0;
  if (name.startsWith("Tuple")) {
    const match = name.match(/^Tuple(\d+)$/);
    if (match)
      return parseInt(match[1], 10);
    return 0;
  }
  if (moduleName) {
    const resolved2 = resolver(name, moduleName);
    if (resolved2)
      return resolved2.info.arity;
  }
  const info = constructors[name];
  if (info)
    return info.arity;
  const resolved = resolver(name);
  if (resolved)
    return resolved.info.arity;
  return 0;
}
function pickMissingConstructor(all, used) {
  for (const c of all) {
    if (!used.has(c))
      return c;
  }
  return;
}
function createDummySpan() {
  return { start: { offset: 0, line: 0, column: 0 }, end: { offset: 0, line: 0, column: 0 } };
}
function patternToString(p) {
  switch (p.kind) {
    case "WildcardPattern":
      return "_";
    case "VarPattern":
      return p.name;
    case "ConstructorPattern":
      if (p.args.length === 0)
        return p.name;
      return `(${p.name} ${p.args.map(patternToString).join(" ")})`;
    case "TuplePattern":
      return `(${p.elements.map(patternToString).join(", ")})`;
    case "ListPattern":
      return `[${p.elements.map(patternToString).join(", ")}]`;
    case "ConsPattern":
      return `(${patternToString(p.head)} :: ${patternToString(p.tail)})`;
    case "RecordPattern":
      const fields = p.fields.map((f) => `${f.name}${f.pattern ? " = " + patternToString(f.pattern) : ""}`);
      return `{ ${fields.join(", ")} }`;
    case "IntPattern":
      return p.value;
    case "FloatPattern":
      return p.value;
    case "StringPattern":
      return p.value;
    case "CharPattern":
      return p.value;
  }
}

// ../semantics/src/index.ts
function makeLambda(args, body, span) {
  if (args.length === 0) {
    return body;
  }
  return {
    kind: "Lambda",
    args,
    body,
    span
  };
}
var ERROR_TYPE = { kind: "error" };
function invertContext(context) {
  const result = new Map;
  for (const [name, typeVar] of context) {
    result.set(typeVar.id, name);
  }
  return result;
}
function resolveParamNames(paramNames, substitution) {
  const result = new Map;
  for (const [id, name] of paramNames) {
    const resolved = applySubstitution({ kind: "var", id }, substitution);
    if (resolved.kind === "var") {
      result.set(resolved.id, name);
    }
  }
  return result;
}

class SemanticAnalyzer {
  program;
  options;
  currentErrors = [];
  errorSignatures = new Set;
  currentConstraintContext = createConstraintContext();
  _pendingProtocolUsages = [];
  _resolvedProtocolUsages = new Map;
  _registry = new RegistryManager;
  _globalScope = new Scope;
  _substitution = new Map;
  _dependencies = new Map;
  constructor(program, options) {
    this.program = program;
    this.options = options;
    this._dependencies = options.dependencies ?? new Map;
  }
  analyze() {
    this.ensureModuleNameConsistency();
    this.initializeBuiltinADTs();
    this.initializeBuiltinOpaqueTypes();
    this.validateImports();
    this.seedBuiltinOperators();
    this.mergeImportedModules();
    this.registerInfixDeclarations();
    this.registerADTTypeDeclarations();
    this.registerOpaqueTypeDeclarations();
    const typeAliasDecls = this.registerTypeAliasDeclarations();
    this.validateTypeAliasDeclarations(typeAliasDecls);
    this.registerProtocolDeclarations();
    this.registerImplementationDeclarations();
    _recordRegistry = this.records;
    _protocolMethodUsages = this._resolvedProtocolUsages;
    _currentModuleName = this.getModuleName();
    this.autoImplementProtocols();
    this.registerValueDeclarations();
    this.validateAnnotationsAndSeedGlobalNames();
    this.inferValueDeclarations();
    this.validateImplementationMethodExpressions();
    this.validateImplementationMethodTypes();
    this.validateInstanceConstraintSatisfiability();
    this.validateConcreteConstraintInstances();
    this.validateProtocolDefaultImplementations();
    const result = this.buildSemanticModule();
    if (this.getErrors().length > 0) {
      throw new MultipleSemanticErrors(this.getErrors());
    }
    return result;
  }
  getFilePath() {
    return this.options.fileContext.filePath;
  }
  getSrcDir() {
    return this.options.fileContext.srcDir;
  }
  getDeclarations() {
    return this.program.declarations;
  }
  getModule() {
    return this.program.module;
  }
  getModuleName() {
    return this.getModule().name;
  }
  get registry() {
    return this._registry;
  }
  get adts() {
    return this._registry.adts;
  }
  get constructors() {
    return this._registry.constructors;
  }
  get constructorTypes() {
    return this._registry.constructorTypes;
  }
  get typeAliases() {
    return this._registry.typeAliases;
  }
  get records() {
    return this._registry.records;
  }
  get opaqueTypes() {
    return this._registry.opaqueTypes;
  }
  get protocols() {
    return this._registry.protocols;
  }
  get instances() {
    return this._registry.instances;
  }
  get operators() {
    return this._registry.operators;
  }
  get values() {
    return this._registry.values;
  }
  get typeSchemes() {
    return this._registry.typeSchemes;
  }
  get types() {
    return this._registry.types;
  }
  get annotations() {
    return this._registry.annotations;
  }
  get localInstances() {
    return this._registry.localInstances;
  }
  get infixDeclarations() {
    return this._registry.infixDeclarations;
  }
  get localProtocolMethods() {
    return this._registry.localProtocolMethods;
  }
  get importedValues() {
    return this._registry.importedValues;
  }
  get globalScope() {
    return this._globalScope;
  }
  createScope() {
    return this._globalScope.child();
  }
  get substitution() {
    return this._substitution;
  }
  get imports() {
    return this.program.imports;
  }
  get dependencies() {
    return this._dependencies;
  }
  getErrors() {
    return this.currentErrors;
  }
  addError(message, span) {
    const signature = `${this.getFilePath()}:${span.start.line}:${span.start.column}`;
    if (!this.errorSignatures.has(signature)) {
      this.errorSignatures.add(signature);
      this.currentErrors.push(new SemanticError(message, span, this.getFilePath()));
    }
  }
  resetErrors() {
    this.currentErrors = [];
    this.errorSignatures = new Set;
  }
  getCollectedConstraints() {
    return this.currentConstraintContext.constraints;
  }
  resetConstraintContext() {
    this.currentConstraintContext = createConstraintContext();
    this._pendingProtocolUsages = [];
  }
  getConstraintContext() {
    return this.currentConstraintContext;
  }
  analyzeExpr(exprArg, contextArg) {
    const { constructors, adts, typeAliases, opaqueTypes, records } = this;
    const { imports, dependencies } = this;
    let expr = exprArg;
    let scope = contextArg.scope;
    let substitution = contextArg.substitution;
    let expectedType = contextArg.expectedType ?? null;
    while (true) {
      switch (expr.kind) {
        case "Var": {
          const { type: resolved, constraints } = this.lookupSymbolWithConstraints(scope, expr.name, expr.span, substitution);
          for (const constraint of constraints) {
            addConstraint(this.getConstraintContext(), constraint);
            this._pendingProtocolUsages.push({ node: expr, constraint });
          }
          return applySubstitution(resolved, substitution);
        }
        case "Number": {
          const hasDecimal = expr.value.includes(".");
          const typeName = hasDecimal ? "Float" : "Int";
          const opaque = opaqueTypes[typeName];
          if (!opaque && !adts[typeName]) {
            throw new SemanticError(`Type '${typeName}' not found. Make sure the prelude is imported.`, expr.span, this.getFilePath());
          }
          return { kind: "con", name: typeName, args: [] };
        }
        case "String": {
          const opaque = opaqueTypes["String"];
          if (!opaque && !adts["String"]) {
            throw new SemanticError("Type 'String' not found. Make sure the prelude is imported.", expr.span, this.getFilePath());
          }
          return { kind: "con", name: "String", args: [] };
        }
        case "Char": {
          const opaque = opaqueTypes["Char"];
          if (!opaque && !adts["Char"]) {
            throw new SemanticError("Type 'Char' not found. Make sure the prelude is imported.", expr.span, this.getFilePath());
          }
          return { kind: "con", name: "Char", args: [] };
        }
        case "Unit": {
          const opaque = opaqueTypes["Unit"];
          if (!opaque && !adts["Unit"]) {
            throw new SemanticError("Type 'Unit' not found. Make sure the prelude is imported.", expr.span, this.getFilePath());
          }
          return { kind: "con", name: "Unit", args: [] };
        }
        case "Tuple": {
          const elements = expr.elements.map((el) => this.analyzeExpr(el, { scope, substitution }));
          return { kind: "tuple", elements };
        }
        case "List": {
          if (expr.elements.length === 0) {
            return listType(freshType());
          }
          const first = this.analyzeExpr(expr.elements[0], {
            scope,
            substitution,
            expectedType: null
          });
          for (const el of expr.elements.slice(1)) {
            const elType = this.analyzeExpr(el, { scope, substitution });
            this.unify(first, elType, el.span, substitution);
          }
          return listType(applySubstitution(first, substitution));
        }
        case "ListRange": {
          const startType = this.analyzeExpr(expr.start, {
            scope,
            substitution
          });
          const endType = this.analyzeExpr(expr.end, { scope, substitution });
          this.unify(startType, endType, expr.span, substitution);
          return listType(applySubstitution(startType, substitution));
        }
        case "Record": {
          const fields = {};
          for (const field2 of expr.fields) {
            if (Object.hasOwn(fields, field2.name)) {
              throw new SemanticError(`Duplicate record field '${field2.name}'`, field2.span, this.getFilePath());
            }
            fields[field2.name] = this.analyzeExpr(field2.value, {
              scope,
              substitution
            });
          }
          const literalFieldNames = new Set(Object.keys(fields));
          let targetRecord;
          if (expectedType) {
            const expected = applySubstitution(expectedType, substitution);
            if (expected.kind === "con") {
              const rec = this.resolveRecordInfo(expected.name);
              if (rec) {
                targetRecord = rec;
              }
            }
          }
          if (!targetRecord) {
            const candidates = this.findRecordsByFieldNames(literalFieldNames);
            if (candidates.length === 0) {
              const shape = Object.entries(fields).map(([k, v]) => `${k} : ${formatType(v)}`).join(", ");
              throw new SemanticError(`No record type in scope has shape { ${shape} }. ` + `Define a named record type with these fields.`, expr.span, this.getFilePath());
            }
            if (candidates.length > 1) {
              const names = candidates.map((r) => r.name).join(", ");
              throw new SemanticError(`Ambiguous record literal — multiple types in scope match this shape: ${names}. ` + `Add a type annotation to disambiguate.`, expr.span, this.getFilePath());
            }
            targetRecord = candidates[0];
          }
          const recordFieldNames = new Set(targetRecord.fields.map((f) => f.name));
          for (const fname of literalFieldNames) {
            if (!recordFieldNames.has(fname)) {
              throw new SemanticError(`Field '${fname}' is not a field of record type '${targetRecord.name}'`, expr.fields.find((f) => f.name === fname).span, this.getFilePath());
            }
          }
          for (const fname of recordFieldNames) {
            if (!literalFieldNames.has(fname)) {
              throw new SemanticError(`Missing field '${fname}' for record type '${targetRecord.name}'`, expr.span, this.getFilePath());
            }
          }
          const paramVars = targetRecord.params.map(() => freshType());
          const resolveCtx = new Map;
          targetRecord.params.forEach((p, i) => {
            resolveCtx.set(p, paramVars[i]);
          });
          for (const fieldInfo of targetRecord.fields) {
            const declaredFieldType = this.typeFromAnnotation(fieldInfo.typeExpr, resolveCtx);
            this.unify(fields[fieldInfo.name], declaredFieldType, expr.fields.find((f) => f.name === fieldInfo.name).span, substitution);
          }
          const resolvedArgs = paramVars.map((v) => applySubstitution(v, substitution));
          return { kind: "con", name: targetRecord.name, args: resolvedArgs };
        }
        case "RecordUpdate": {
          const baseType = this.lookupSymbol(scope, expr.base, expr.span, substitution);
          const concreteBase = applySubstitution(baseType, substitution);
          if (concreteBase.kind === "con") {
            const recordInfo = this.resolveRecordInfo(concreteBase.name);
            if (recordInfo) {
              const paramVars = recordInfo.params.map(() => freshType());
              const resolveCtx = new Map;
              recordInfo.params.forEach((p, i) => {
                resolveCtx.set(p, paramVars[i]);
              });
              paramVars.forEach((v, i) => {
                if (concreteBase.args[i]) {
                  this.unify(v, concreteBase.args[i], expr.span, substitution);
                }
              });
              const recordFieldNames = new Set(recordInfo.fields.map((f) => f.name));
              for (const field2 of expr.fields) {
                if (!recordFieldNames.has(field2.name)) {
                  throw new SemanticError(`Record '${concreteBase.name}' has no field '${field2.name}'`, field2.span, this.getFilePath());
                }
                const fieldInfo = recordInfo.fields.find((f) => f.name === field2.name);
                const declaredFieldType = this.typeFromAnnotation(fieldInfo.typeExpr, resolveCtx);
                const fieldType = this.analyzeExpr(field2.value, {
                  scope,
                  substitution
                });
                this.unify(fieldType, declaredFieldType, field2.span, substitution);
              }
              const resolvedArgs = paramVars.map((v) => applySubstitution(v, substitution));
              return {
                kind: "con",
                name: concreteBase.name,
                args: resolvedArgs
              };
            }
          }
          if (concreteBase.kind !== "record") {
            throw new SemanticError(`Cannot update non-record '${expr.base}'`, expr.span, this.getFilePath());
          }
          const updatedFields = {
            ...concreteBase.fields
          };
          for (const field2 of expr.fields) {
            if (!updatedFields[field2.name]) {
              throw new SemanticError(`Record '${expr.base}' has no field '${field2.name}'`, field2.span, this.getFilePath());
            }
            const fieldType = this.analyzeExpr(field2.value, {
              scope,
              substitution
            });
            this.unify(updatedFields[field2.name], fieldType, field2.span, substitution);
            updatedFields[field2.name] = applySubstitution(updatedFields[field2.name], substitution);
          }
          return { kind: "record", fields: updatedFields };
        }
        case "FieldAccess": {
          const fieldExpr = expr;
          const moduleAccess = tryResolveModuleFieldAccess(fieldExpr, imports, dependencies, substitution);
          if (moduleAccess) {
            return moduleAccess;
          }
          const targetType = this.analyzeExpr(fieldExpr.target, {
            scope,
            substitution
          });
          const concrete = applySubstitution(targetType, substitution);
          if (concrete.kind === "error") {
            return ERROR_TYPE;
          }
          let fieldType;
          if (concrete.kind === "record") {
            fieldType = concrete.fields[fieldExpr.field];
          } else if (concrete.kind === "con") {
            const recordInfo = this.resolveRecordInfo(concrete.name);
            if (recordInfo) {
              const fieldInfo = recordInfo.fields.find((f) => f.name === fieldExpr.field);
              if (fieldInfo) {
                const resolveCtx = new Map;
                const freshVars = [];
                recordInfo.params.forEach((p) => {
                  const v = { kind: "var", id: freshType().id };
                  resolveCtx.set(p, v);
                  freshVars.push(v);
                });
                const genericFieldType = this.typeFromAnnotation(fieldInfo.typeExpr, resolveCtx);
                const instSub = new Map;
                freshVars.forEach((v, i) => {
                  instSub.set(v.id, concrete.args[i]);
                });
                fieldType = applySubstitution(genericFieldType, instSub);
              }
            }
          }
          if (!fieldType) {
            if (concrete.kind !== "record" && (concrete.kind !== "con" || !this.resolveRecordInfo(concrete.name))) {
              throw new SemanticError(`Cannot access field '${fieldExpr.field}' on non-record value '${formatType(concrete)}'`, fieldExpr.span, this.getFilePath());
            }
            throw new SemanticError(`Record has no field '${fieldExpr.field}'`, fieldExpr.span, this.getFilePath());
          }
          return applySubstitution(fieldType, substitution);
        }
        case "Lambda": {
          const paramTypes = expr.args.map(() => freshType());
          const fnScope = scope.child();
          this.bindPatterns(fnScope, expr.args, paramTypes, substitution);
          const bodyType = this.analyzeExpr(expr.body, {
            scope: fnScope,
            substitution
          });
          return fnChain(paramTypes, bodyType);
        }
        case "Apply": {
          let calleeType = this.analyzeExpr(expr.callee, {
            scope,
            substitution
          });
          for (const arg of expr.args) {
            const argType = this.analyzeExpr(arg, { scope, substitution });
            const resultType = freshType();
            this.unify(calleeType, fn(argType, resultType), expr.span, substitution);
            this.validateConstraintsEagerly(substitution, arg.span);
            calleeType = applySubstitution(resultType, substitution);
          }
          return applySubstitution(calleeType, substitution);
        }
        case "If": {
          const condType = this.analyzeExpr(expr.condition, {
            scope,
            substitution
          });
          const boolAdt = adts["Bool"];
          if (!boolAdt) {
            throw new SemanticError("Type 'Bool' not found. Make sure the prelude is imported.", expr.condition.span, this.getFilePath());
          }
          const tBool = { kind: "con", name: "Bool", args: [] };
          this.unify(condType, tBool, expr.condition.span, substitution);
          const thenType = this.analyzeExpr(expr.thenBranch, {
            scope,
            substitution
          });
          const elseType = this.analyzeExpr(expr.elseBranch, {
            scope,
            substitution
          });
          this.unify(thenType, elseType, expr.span, substitution);
          return applySubstitution(thenType, substitution);
        }
        case "LetIn": {
          const letScope = scope.child();
          for (const binding of expr.bindings) {
            if (letScope.symbols.has(binding.name)) {
              throw new SemanticError(`Duplicate let-binding '${binding.name}'`, binding.span, this.getFilePath());
            }
            const seeded = this.seedValueType(binding);
            this.declareSymbol(letScope, binding.name, { vars: new Set, constraints: [], type: seeded }, binding.span);
          }
          for (const binding of expr.bindings) {
            const declared = this.lookupSymbol(letScope, binding.name, binding.span, substitution);
            const inferred = this.analyzeValueDeclaration(binding, letScope, substitution, declared);
            const generalizedScheme = this.generalize(inferred, scope, substitution);
            letScope.symbols.set(binding.name, generalizedScheme);
          }
          expr = expr.body;
          scope = letScope;
          continue;
        }
        case "Case": {
          const caseExpr = expr;
          const discriminantType = this.analyzeExpr(caseExpr.discriminant, {
            scope,
            substitution
          });
          const branchTypes = [];
          caseExpr.branches.forEach((branch, index) => {
            const branchScope = new Scope(scope);
            const patternType = this.bindPattern(branch.pattern, branchScope, new Set, freshType(), substitution);
            this.unify(discriminantType, patternType, branch.pattern.span, substitution);
            const bodyType = this.analyzeExpr(branch.body, {
              scope: branchScope,
              substitution,
              expectedType
            });
            branchTypes.push(bodyType);
            if (branch.pattern.kind === "WildcardPattern") {
              if (index !== caseExpr.branches.length - 1) {
                throw new SemanticError("Wildcard pattern makes following branches unreachable", branch.pattern.span, this.getFilePath());
              }
            }
            if (branch.pattern.kind === "ConstructorPattern") {
              this.validateConstructorArity(branch.pattern);
            }
          });
          if (branchTypes.length === 0) {
            throw new SemanticError("Case expression has no branches", caseExpr.span, this.getFilePath());
          }
          const firstType = branchTypes[0];
          for (const bt of branchTypes.slice(1)) {
            this.unify(firstType, bt, caseExpr.span, substitution);
          }
          const patterns = caseExpr.branches.map((b) => b.pattern);
          const result = checkExhaustiveness(patterns, discriminantType, adts, constructors, (name) => resolveQualifiedConstructor(name, constructors, adts, imports, dependencies) || undefined);
          if (!result.exhaustive) {
            throw new SemanticError(`Non-exhaustive case expression (missing: ${result.missing})`, caseExpr.span, this.getFilePath());
          }
          return applySubstitution(firstType, substitution);
        }
        case "Infix": {
          const opType = INFIX_TYPES[expr.operator];
          if (opType) {
            const leftType = this.analyzeExpr(expr.left, {
              scope,
              substitution
            });
            const rightType = this.analyzeExpr(expr.right, {
              scope,
              substitution
            });
            const expected = applySubstitution(opType, substitution);
            const params = flattenFunctionParams(expected);
            if (params.length < 2) {
              throw new SemanticError("Invalid operator type", expr.span, this.getFilePath());
            }
            this.unify(params[0], leftType, expr.left.span, substitution);
            this.unify(params[1], rightType, expr.right.span, substitution);
            return applySubstitution(extractAnnotationReturn(expected, 2), substitution);
          }
          const callee = {
            kind: "Var",
            name: expr.operator,
            namespace: "lower",
            span: expr.span
          };
          const applyExpr = {
            kind: "Apply",
            callee,
            args: [expr.left, expr.right],
            span: expr.span
          };
          const beforeLen = this._pendingProtocolUsages.length;
          const result = this.analyzeExpr(applyExpr, {
            scope,
            substitution,
            expectedType
          });
          for (let i = beforeLen;i < this._pendingProtocolUsages.length; i++) {
            if (this._pendingProtocolUsages[i].node === callee) {
              this._pendingProtocolUsages[i].node = expr;
            }
          }
          return result;
        }
        case "Paren":
          expr = expr.expression;
          continue;
        case "Unary": {
          const operandType = this.analyzeExpr(expr.operand, {
            scope,
            substitution
          });
          const concreteType = applySubstitution(operandType, substitution);
          if (concreteType.kind === "con") {
            if (concreteType.name === "Int" || concreteType.name === "Float") {
              return concreteType;
            }
          }
          if (concreteType.kind === "var") {
            throw new SemanticError(`Unary negation requires a concrete numeric type (Int or Float), but got an unknown type. Add a type annotation to disambiguate.`, expr.span, this.getFilePath());
          }
          throw new SemanticError(`Unary negation is only allowed for Int and Float, but got '${formatType(concreteType)}'`, expr.span, this.getFilePath());
        }
        default: {
          const _exhaustive = expr;
          throw new SemanticError("Unsupported expression", expr.span, this.getFilePath());
        }
      }
    }
  }
  seedValueType(decl) {
    if (decl.kind === "DecoratedDeclaration") {
      return this.typeFromAnnotation(decl.annotation, new Map);
    }
    const argTypes = decl.args.map(() => freshType());
    const resultType = freshType();
    return fnChain(argTypes, resultType);
  }
  typeFromAnnotation(annotation, context = new Map) {
    const { adts, typeAliases, records, imports, dependencies } = this;
    function resolve(name) {
      return resolveQualifiedType(name, adts, typeAliases, {}, records, imports, dependencies);
    }
    switch (annotation.kind) {
      case "TypeRef": {
        if (isTypeVariable(annotation.name)) {
          let typeVar = context.get(annotation.name);
          if (!typeVar) {
            typeVar = freshType();
            context.set(annotation.name, typeVar);
          }
          if (annotation.args.length > 0) {} else {
            return typeVar;
          }
        }
        const resolved = resolve(annotation.name);
        if (resolved && resolved.kind === "alias") {
          const aliasInfo = resolved.info;
          if (annotation.args.length !== aliasInfo.params.length) {} else {
            const argTypes = [];
            for (let i = 0;i < aliasInfo.params.length; i++) {
              const argType = this.typeFromAnnotation(annotation.args[i], context);
              argTypes.push(argType);
            }
            const aliasContext = new Map(context);
            const substitutionMap = new Map;
            for (let i = 0;i < aliasInfo.params.length; i++) {
              const paramName = aliasInfo.params[i];
              const fresh = freshType();
              aliasContext.set(paramName, fresh);
              substitutionMap.set(fresh.id, argTypes[i]);
            }
            const expandedType = this.typeFromAnnotation(aliasInfo.value, aliasContext);
            return applySubstitution(expandedType, substitutionMap);
          }
        }
        if (resolved && resolved.kind === "record") {
          const recordInfo = resolved.info;
          if (annotation.args.length === recordInfo.params.length) {
            const argTypes = [];
            for (let i = 0;i < recordInfo.params.length; i++) {
              argTypes.push(this.typeFromAnnotation(annotation.args[i], context));
            }
            return {
              kind: "con",
              name: recordInfo.name,
              args: argTypes
            };
          }
        }
        const canonicalName = resolved ? resolved.name : annotation.name;
        return {
          kind: "con",
          name: canonicalName,
          args: annotation.args.map((arg) => this.typeFromAnnotation(arg, context))
        };
      }
      case "FunctionType": {
        const from = this.typeFromAnnotation(annotation.from, context);
        const to = this.typeFromAnnotation(annotation.to, context);
        return fn(from, to);
      }
      case "TupleType": {
        return {
          kind: "tuple",
          elements: annotation.elements.map((el) => this.typeFromAnnotation(el, context))
        };
      }
      case "RecordType": {
        throw new SemanticError(`Record types cannot be used directly in type annotations. ` + `Define a named record type using 'type RecordName = { ... }' and reference it by name.`, annotation.span, this.getFilePath());
      }
      case "QualifiedType": {
        return this.typeFromAnnotation(annotation.type, context);
      }
    }
  }
  validateConstructorArity(pattern) {
    const ctorInfo = this.constructors[pattern.name];
    if (ctorInfo) {
      if (ctorInfo.arity !== pattern.args.length) {
        throw new SemanticError(`Constructor '${pattern.name}' expects ${ctorInfo.arity} argument(s), got ${pattern.args.length}`, pattern.span, this.getFilePath());
      }
      return;
    }
    const expected = BUILTIN_CONSTRUCTORS[pattern.name];
    if (expected !== undefined && expected !== pattern.args.length) {
      throw new SemanticError(`Constructor '${pattern.name}' expects ${expected} argument(s)`, pattern.span, this.getFilePath());
    }
  }
  bindPatterns(scope, patterns, paramTypes, substitution = this.substitution) {
    if (patterns.length !== paramTypes.length) {
      throw new Error("Internal arity mismatch during pattern binding");
    }
    const seen = new Set;
    patterns.forEach((pattern, idx) => {
      const paramType = paramTypes[idx];
      this.bindPattern(pattern, scope, seen, paramType, substitution);
      if (pattern.kind === "VarPattern") {
        scope.symbols.set(pattern.name, {
          vars: new Set,
          constraints: [],
          type: paramType
        });
      }
    });
  }
  bindPattern(pattern, scope, seen = new Set, expected, substitution = this.substitution) {
    const { constructors, adts, imports, dependencies } = this;
    const bind = (p, exp) => this.bindPattern(p, scope, seen, exp);
    switch (pattern.kind) {
      case "VarPattern": {
        if (seen.has(pattern.name)) {
          throw new SemanticError(`Duplicate pattern variable '${pattern.name}'`, pattern.span, this.getFilePath());
        }
        seen.add(pattern.name);
        this.declareSymbol(scope, pattern.name, { vars: new Set, constraints: [], type: expected }, pattern.span);
        return expected;
      }
      case "WildcardPattern":
        return expected;
      case "TuplePattern": {
        const subTypes = pattern.elements.map(() => freshType());
        this.unify({ kind: "tuple", elements: subTypes }, expected, pattern.span, substitution);
        pattern.elements.forEach((el, idx) => bind(el, subTypes[idx]));
        return applySubstitution({ kind: "tuple", elements: subTypes }, substitution);
      }
      case "ConstructorPattern": {
        this.validateConstructorArity(pattern);
        const resolved = resolveQualifiedConstructor(pattern.name, constructors, adts, imports, dependencies);
        const ctorInfo = resolved ? resolved.info : undefined;
        if (ctorInfo) {
          const paramTypeVars = new Map;
          for (const param of ctorInfo.parentParams) {
            paramTypeVars.set(param, freshType());
          }
          const resultType = {
            kind: "con",
            name: ctorInfo.parentType,
            args: ctorInfo.parentParams.map((p) => paramTypeVars.get(p))
          };
          const argTypes = ctorInfo.argTypes.map((argExpr) => this.constructorArgToType(argExpr, paramTypeVars));
          this.unify(resultType, expected, pattern.span, substitution);
          if (pattern.args.length !== argTypes.length) {
            throw new SemanticError(`Constructor '${pattern.name}' expects ${argTypes.length} argument(s), got ${pattern.args.length}`, pattern.span, this.getFilePath());
          }
          pattern.args.forEach((arg, idx) => {
            const argType = applySubstitution(argTypes[idx], substitution);
            bind(arg, argType);
          });
          return applySubstitution(resultType, substitution);
        } else {
          const argTypes = pattern.args.map(() => freshType());
          const constructed = fnChain(argTypes, freshType());
          this.unify(constructed, expected, pattern.span, substitution);
          pattern.args.forEach((arg, idx) => bind(arg, argTypes[idx]));
          return applySubstitution(expected, substitution);
        }
      }
      case "ListPattern": {
        const elemType = freshType();
        const lt = listType(elemType);
        this.unify(lt, expected, pattern.span, substitution);
        pattern.elements.forEach((el) => bind(el, applySubstitution(elemType, substitution)));
        return applySubstitution(lt, substitution);
      }
      case "ConsPattern": {
        const elemType = freshType();
        const lt = listType(elemType);
        this.unify(lt, expected, pattern.span, substitution);
        bind(pattern.head, applySubstitution(elemType, substitution));
        bind(pattern.tail, applySubstitution(lt, substitution));
        return applySubstitution(lt, substitution);
      }
      case "RecordPattern": {
        const fieldTypes = {};
        for (const field2 of pattern.fields) {
          fieldTypes[field2.name] = freshType();
        }
        const recordType = {
          kind: "record",
          fields: fieldTypes
        };
        this.unify(recordType, expected, pattern.span, substitution);
        for (const field2 of pattern.fields) {
          const fieldType = fieldTypes[field2.name];
          const appliedFieldType = applySubstitution(fieldType, substitution);
          if (field2.pattern) {
            bind(field2.pattern, appliedFieldType);
          } else {
            if (seen.has(field2.name)) {
              throw new SemanticError(`Duplicate pattern variable '${field2.name}'`, pattern.span, this.getFilePath());
            }
            seen.add(field2.name);
            this.declareSymbol(scope, field2.name, { vars: new Set, constraints: [], type: appliedFieldType }, pattern.span);
          }
        }
        return applySubstitution(recordType, substitution);
      }
      default:
        return expected;
    }
  }
  constructorArgToType(expr, paramTypeVars) {
    switch (expr.kind) {
      case "TypeRef": {
        const typeVar = paramTypeVars.get(expr.name);
        if (typeVar && expr.args.length === 0) {
          return typeVar;
        }
        const args = expr.args.map((arg) => this.constructorArgToType(arg, paramTypeVars));
        return {
          kind: "con",
          name: expr.name,
          args
        };
      }
      case "FunctionType": {
        return {
          kind: "fun",
          from: this.constructorArgToType(expr.from, paramTypeVars),
          to: this.constructorArgToType(expr.to, paramTypeVars)
        };
      }
      case "TupleType": {
        return {
          kind: "tuple",
          elements: expr.elements.map((el) => this.constructorArgToType(el, paramTypeVars))
        };
      }
      case "RecordType": {
        const sortedFields = [...expr.fields].sort((a, b) => a.name.localeCompare(b.name));
        const fields = {};
        for (const field2 of sortedFields) {
          fields[field2.name] = this.constructorArgToType(field2.type, paramTypeVars);
        }
        return {
          kind: "record",
          fields
        };
      }
      case "QualifiedType": {
        throw new SemanticError("Constructor arguments cannot have constraints", expr.span, this.getFilePath());
      }
    }
  }
  validateFunctionParamPatterns(patterns) {
    for (const pattern of patterns) {
      this.validateFunctionParamPattern(pattern);
    }
  }
  validateFunctionParamPattern(pattern) {
    const { constructors, adts } = this;
    switch (pattern.kind) {
      case "VarPattern":
      case "WildcardPattern":
        return;
      case "TuplePattern":
        for (const element of pattern.elements) {
          this.validateFunctionParamPattern(element);
        }
        return;
      case "RecordPattern":
        for (const field2 of pattern.fields) {
          if (field2.pattern) {
            this.validateFunctionParamPattern(field2.pattern);
          }
        }
        return;
      case "ConstructorPattern": {
        const ctorInfo = constructors[pattern.name];
        if (!ctorInfo) {
          return;
        }
        const adtInfo = adts[ctorInfo.parentType];
        if (!adtInfo) {
          return;
        }
        if (adtInfo.constructors.length > 1) {
          const constructorNames = adtInfo.constructors.join(", ");
          throw new SemanticError(`Constructor pattern '${pattern.name}' is not allowed in function parameters. ` + `The type '${ctorInfo.parentType}' has multiple constructors (${constructorNames}). ` + `Use a case expression in the function body instead.`, pattern.span, this.getFilePath());
        }
        for (const arg of pattern.args) {
          this.validateFunctionParamPattern(arg);
        }
        return;
      }
      case "ListPattern":
      case "ConsPattern":
        throw new SemanticError(`List patterns are not allowed in function parameters. ` + `Use a case expression in the function body instead.`, pattern.span, this.getFilePath());
    }
  }
  resolveRecordInfo(name) {
    if (this.records[name])
      return this.records[name];
    for (const [, dep] of this.dependencies) {
      if (dep.records[name])
        return dep.records[name];
    }
    return;
  }
  findRecordsByFieldNames(fieldNames) {
    const results = [];
    const seen = new Set;
    const check = (rec) => {
      if (seen.has(rec.name))
        return;
      seen.add(rec.name);
      const recFieldNames = new Set(rec.fields.map((f) => f.name));
      if (recFieldNames.size === fieldNames.size && [...fieldNames].every((f) => recFieldNames.has(f))) {
        results.push(rec);
      }
    };
    for (const rec of Object.values(this.records)) {
      check(rec);
    }
    return results;
  }
  analyzeValueDeclaration(decl, scope, substitution, declaredType, annotationType) {
    this.validateFunctionParamPatterns(decl.args);
    const paramTypes = annotationType ? extractAnnotationParams(annotationType, decl.args.length, decl.span) : decl.args.map(() => freshType());
    const returnType = annotationType ? extractAnnotationReturn(annotationType, decl.args.length) : freshType();
    const expected = fnChain(paramTypes, returnType);
    this.unify(expected, declaredType, decl.span, substitution);
    const fnScope = scope.child();
    this.bindPatterns(fnScope, decl.args, paramTypes, substitution);
    const bodyType = this.analyzeExpr(decl.body, {
      scope: fnScope,
      substitution,
      expectedType: returnType
    });
    this.unify(bodyType, returnType, decl.body.span, substitution);
    return applySubstitution(expected, substitution);
  }
  unify(a, b, span, substitution = this.substitution) {
    const left = applySubstitution(a, substitution);
    const right = applySubstitution(b, substitution);
    if (left.kind === "error" || right.kind === "error") {
      return;
    }
    if (left.kind === "var") {
      if (!typesEqual(left, right)) {
        if (occursIn(left.id, right, substitution)) {
          throw new SemanticError("Recursive type detected", span, this.getFilePath());
        }
        substitution.set(left.id, right);
      }
      return;
    }
    if (right.kind === "var") {
      return this.unify(right, left, span, substitution);
    }
    if (left.kind === "con" && right.kind === "con") {
      if (left.name !== right.name || left.args.length !== right.args.length) {
        throw new SemanticError(`Type mismatch: cannot unify '${formatType(left)}' with '${formatType(right)}'`, span, this.getFilePath());
      }
      left.args.forEach((arg, idx) => this.unify(arg, right.args[idx], span, substitution));
      return;
    }
    if (left.kind === "fun" && right.kind === "fun") {
      this.unify(left.from, right.from, span, substitution);
      this.unify(left.to, right.to, span, substitution);
      return;
    }
    if (left.kind === "tuple" && right.kind === "tuple") {
      if (left.elements.length !== right.elements.length) {
        throw new SemanticError("Tuple length mismatch", span, this.getFilePath());
      }
      left.elements.forEach((el, idx) => this.unify(el, right.elements[idx], span, substitution));
      return;
    }
    if (left.kind === "record" && right.kind === "record") {
      const shared = Object.keys(left.fields).filter((k) => right.fields[k] !== undefined);
      for (const key of shared) {
        this.unify(left.fields[key], right.fields[key], span, substitution);
      }
      return;
    }
    throw new SemanticError(`Type mismatch: cannot unify '${formatType(left)}' with '${formatType(right)}'`, span, this.getFilePath());
  }
  importExportSpec(spec, depModule, imp) {
    switch (spec.kind) {
      case "ExportValue": {
        const name = spec.name;
        if (!isExportedFromModule(depModule, name, "value")) {
          if (isExportedFromModule(depModule, name, "type") && depModule.adts[name]) {
            const depADT = depModule.adts[name];
            this.checkTypeCollision(name, imp.moduleName, this.adts[name], imp.span, "type");
            this.adts[name] = depADT;
            return;
          }
          if (depModule.typeAliases[name]) {
            this.checkTypeCollision(name, imp.moduleName, this.typeAliases[name], imp.span, "type alias");
            this.typeAliases[name] = depModule.typeAliases[name];
            return;
          }
          if (depModule.opaqueTypes[name]) {
            this.opaqueTypes[name] = depModule.opaqueTypes[name];
            return;
          }
          if (depModule.records[name]) {
            this.checkTypeCollision(name, imp.moduleName, this.records[name], imp.span, "record type");
            this.records[name] = depModule.records[name];
            return;
          }
          if (isExportedFromModule(depModule, name, "protocol") && depModule.protocols[name]) {
            this.checkTypeCollision(name, imp.moduleName, this.protocols[name], imp.span, "protocol");
            this.protocols[name] = depModule.protocols[name];
            return;
          }
          throw new SemanticError(`Cannot import '${name}' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
        }
        const depValue = depModule.values[name];
        if (depValue && depValue.type) {
          const importedType = depValue.type;
          const scheme = this.generalize(importedType, this.globalScope, this.substitution);
          this.globalScope.define(name, scheme);
          this.importedValues.set(name, imp.moduleName);
        }
        if (isExportedFromModule(depModule, name, "constructor")) {
          const depConstructor = depModule.constructors[name];
          if (depConstructor) {
            this.constructors[name] = depConstructor;
            const ctorScheme = depModule.constructorTypes[name];
            if (ctorScheme) {
              this.globalScope.define(name, ctorScheme);
              this.constructorTypes[name] = ctorScheme;
            }
          }
        }
        break;
      }
      case "ExportOperator": {
        const op = spec.operator;
        if (!isExportedFromModule(depModule, op, "operator")) {
          throw new SemanticError(`Cannot import operator '${op}' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
        }
        const depValue = depModule.values[op];
        if (depValue && depValue.type) {
          const importedType = depValue.type;
          const scheme = this.generalize(importedType, this.globalScope, this.substitution);
          this.globalScope.define(op, scheme);
        }
        const opInfo = depModule.operators.get(op);
        if (opInfo) {
          this.operators.set(op, opInfo);
          this.importedValues.set(op, imp.moduleName);
        }
        break;
      }
      case "ExportTypeAll": {
        const name = spec.name;
        const depADT = depModule.adts[name];
        if (depADT) {
          if (!isExportedFromModule(depModule, name, "type")) {
            throw new SemanticError(`Cannot import type '${name}(..)' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
          }
          this.checkTypeCollision(name, imp.moduleName, this.adts[name], imp.span, "type");
          this.adts[name] = depADT;
          for (const ctorName of depADT.constructors) {
            const ctor = depModule.constructors[ctorName];
            if (ctor) {
              this.constructors[ctorName] = ctor;
              const ctorScheme = depModule.constructorTypes[ctorName];
              if (ctorScheme) {
                this.globalScope.define(ctorName, ctorScheme);
                this.constructorTypes[ctorName] = ctorScheme;
              }
            }
          }
          return;
        }
        const depProtocol = depModule.protocols[name];
        if (depProtocol) {
          if (!isExportedFromModule(depModule, name, "protocol")) {
            throw new SemanticError(`Cannot import protocol '${name}(..)' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
          }
          this.checkTypeCollision(name, imp.moduleName, this.protocols[name], imp.span, "protocol");
          this.protocols[name] = depProtocol;
          const methodSchemes = addProtocolMethodsToScope(depProtocol, this.globalScope);
          for (const [methodName, scheme] of methodSchemes) {
            this.typeSchemes[methodName] = scheme;
          }
          return;
        }
        const depRecord = depModule.records[name];
        if (depRecord) {
          throw new SemanticError(`Record type '${name}' cannot use (..) syntax - records have no constructors. Use '${name}' instead`, spec.span, this.getFilePath());
        }
        throw new SemanticError(`Cannot import '${name}(..)' from module '${imp.moduleName}' - it is not a type or protocol`, spec.span, this.getFilePath());
      }
      case "ExportTypeSome": {
        const name = spec.name;
        const members = spec.members;
        const depADT = depModule.adts[name];
        if (depADT) {
          if (!isExportedFromModule(depModule, name, "type")) {
            throw new SemanticError(`Cannot import type '${name}' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
          }
          this.checkTypeCollision(name, imp.moduleName, this.adts[name], imp.span, "type");
          this.adts[name] = depADT;
          for (const ctorName of members) {
            if (!depADT.constructors.includes(ctorName)) {
              throw new SemanticError(`Constructor '${ctorName}' is not defined in type '${name}'`, spec.span, this.getFilePath());
            }
            const ctor = depModule.constructors[ctorName];
            if (ctor) {
              this.constructors[ctorName] = ctor;
              const ctorScheme = depModule.constructorTypes[ctorName];
              if (ctorScheme) {
                this.globalScope.define(ctorName, ctorScheme);
                this.constructorTypes[ctorName] = ctorScheme;
              }
            }
          }
          return;
        }
        const depProtocol = depModule.protocols[name];
        if (depProtocol) {
          if (!isExportedFromModule(depModule, name, "protocol")) {
            throw new SemanticError(`Cannot import protocol '${name}' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
          }
          this.checkTypeCollision(name, imp.moduleName, this.protocols[name], imp.span, "protocol");
          this.protocols[name] = depProtocol;
          for (const methodName of members) {
            const methodInfo = depProtocol.methods.get(methodName);
            if (!methodInfo) {
              throw new SemanticError(`Method '${methodName}' is not defined in protocol '${name}'`, spec.span, this.getFilePath());
            }
            const constraintTypeVar = freshType();
            const methodType = methodInfo.type;
            const scheme = {
              vars: new Set([
                constraintTypeVar.kind === "var" ? constraintTypeVar.id : -1
              ]),
              constraints: [
                { protocolName: name, typeArgs: [constraintTypeVar] }
              ],
              type: methodType
            };
            this.globalScope.define(methodName, scheme);
          }
          return;
        }
        throw new SemanticError(`Cannot import '${name}(...)' from module '${imp.moduleName}' - it is not a type or protocol`, spec.span, this.getFilePath());
      }
    }
  }
  registerValue(decl) {
    if (Object.hasOwn(this.values, decl.name)) {
      throw new SemanticError(`Duplicate definition for '${decl.name}'`, decl.span, this.getFilePath());
    }
    const hasBuiltinAnnotation = decl.kind === "DecoratedDeclaration";
    let externalTarget;
    if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
      externalTarget = {
        modulePath: decl.args[0],
        exportName: decl.args[1],
        span: decl.span
      };
    }
    this.values[decl.name] = {
      declaration: decl,
      annotation: hasBuiltinAnnotation ? decl.annotation : undefined,
      externalTarget
    };
  }
  createChildScope(parent) {
    return (parent ?? this._globalScope).child();
  }
  computeExpectedModuleName() {
    const normalizedFilePath = this.getFilePath().replace(/\\/g, "/");
    const rawSrcDir = this.getSrcDir();
    const normalizedSrcDir = rawSrcDir.replace(/\\/g, "/").replace(/\/$/, "");
    if (!normalizedSrcDir || !normalizedFilePath.startsWith(normalizedSrcDir + "/")) {
      const fileName = normalizedFilePath.split("/").pop() ?? "";
      return fileName.replace(/\.vibe$/, "");
    }
    const relativePath = normalizedFilePath.slice(normalizedSrcDir.length + 1).replace(/\.vibe$/, "");
    return relativePath.replace(/\//g, ".");
  }
  ensureModuleNameConsistency() {
    const expectedModuleName = this.computeExpectedModuleName();
    const declaredModuleName = this.program.module.name;
    if (declaredModuleName !== expectedModuleName) {
      throw new SemanticError(`Module name '${declaredModuleName}' does not match file path.
` + `Expected: module ${expectedModuleName} [exposing (..)]
` + `File path: ${this.getFilePath()}`, this.program.module.span, this.getFilePath());
    }
  }
  initializeBuiltinADTs() {
    this.adts["Bool"] = {
      name: "Bool",
      params: [],
      constructors: ["True", "False"],
      constraints: [],
      span: BUILTIN_SPAN
    };
    this.constructors["True"] = {
      arity: 0,
      argTypes: [],
      parentType: "Bool",
      parentParams: [],
      moduleName: BUILTIN_MODULE_NAME,
      span: BUILTIN_SPAN
    };
    this.constructorTypes["True"] = {
      vars: new Set,
      constraints: [],
      type: { kind: "con", name: "Bool", args: [] }
    };
    this.constructors["False"] = {
      arity: 0,
      argTypes: [],
      parentType: "Bool",
      parentParams: [],
      moduleName: BUILTIN_MODULE_NAME,
      span: BUILTIN_SPAN
    };
    this.constructorTypes["False"] = {
      vars: new Set,
      constraints: [],
      type: { kind: "con", name: "Bool", args: [] }
    };
    this.adts["List"] = {
      name: "List",
      params: ["a"],
      constructors: [],
      constraints: [],
      span: BUILTIN_SPAN
    };
    this.globalScope.define("True", this.constructorTypes["True"]);
    this.globalScope.define("False", this.constructorTypes["False"]);
  }
  initializeBuiltinOpaqueTypes() {
    this.opaqueTypes["Unit"] = {
      name: "Unit",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
    this.opaqueTypes["Int"] = {
      name: "Int",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
    this.opaqueTypes["Float"] = {
      name: "Float",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
    this.opaqueTypes["String"] = {
      name: "String",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
    this.opaqueTypes["Char"] = {
      name: "Char",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
  }
  validateImports() {
    const byModule = new Map;
    const byAlias = new Map;
    for (const imp of this.imports) {
      const duplicateModule = byModule.get(imp.moduleName);
      if (duplicateModule) {
        throw new SemanticError(`Duplicate import of module '${imp.moduleName}'`, imp.span, this.getFilePath());
      }
      byModule.set(imp.moduleName, imp);
      if (imp.alias) {
        const duplicateAlias = byAlias.get(imp.alias);
        if (duplicateAlias) {
          throw new SemanticError(`Duplicate import alias '${imp.alias}'`, imp.span, this.getFilePath());
        }
        byAlias.set(imp.alias, imp);
      }
    }
  }
  seedBuiltinOperators() {
    for (const [op, ty] of Object.entries(INFIX_TYPES)) {
      this.globalScope.define(op, {
        vars: new Set,
        constraints: [],
        type: ty
      });
    }
    for (const [op, fixity] of Object.entries(BUILTIN_OPERATOR_FIXITY)) {
      this.operators.set(op, fixity);
    }
  }
  mergeImportedModules() {
    for (const imp of this.imports) {
      const depModule = this.dependencies.get(imp.moduleName);
      if (!depModule) {
        throw new SemanticError("Unresolved module import", imp.span, this.getFilePath());
      }
      const currentModuleName = this.getModuleName();
      for (const instance of depModule.instances) {
        if (instance.moduleName === currentModuleName)
          continue;
        const isDuplicate = this.instances.some((existing) => existing.protocolName === instance.protocolName && existing.moduleName === instance.moduleName && typeArgsEqual(existing.typeArgs, instance.typeArgs));
        if (!isDuplicate) {
          this.instances.push(instance);
        }
      }
      if (imp.alias) {
        this.globalScope.define(imp.alias, {
          vars: new Set,
          constraints: [],
          type: freshType()
        });
      }
      if (!imp.alias) {
        const moduleParts = imp.moduleName.split(".");
        const rootModule = moduleParts[0];
        if (!this.globalScope.has(rootModule)) {
          this.globalScope.define(rootModule, {
            vars: new Set,
            constraints: [],
            type: freshType()
          });
        }
      }
      if (imp.exposing?.kind === "Explicit") {
        for (const spec of imp.exposing.exports) {
          this.importExportSpec(spec, depModule, imp);
        }
      }
      if (imp.exposing?.kind === "All") {
        for (const [name, depValue] of Object.entries(depModule.values)) {
          if (depValue.type && isExportedFromModule(depModule, name, "value")) {
            const importedType = depValue.type;
            const scheme = this.generalize(importedType, this.globalScope, this.substitution);
            this.globalScope.define(name, scheme);
            this.importedValues.set(name, imp.moduleName);
          }
        }
        for (const [name] of depModule.exports.reExportedValues) {
          if (this.globalScope.has(name))
            continue;
          const scheme = depModule.typeSchemes[name];
          if (scheme) {
            this.globalScope.define(name, scheme);
            this.importedValues.set(name, imp.moduleName);
          }
        }
        for (const [name, ctor] of Object.entries(depModule.constructors)) {
          if (isExportedFromModule(depModule, name, "constructor")) {
            this.constructors[name] = ctor;
            const ctorScheme = depModule.constructorTypes[name];
            if (ctorScheme) {
              this.globalScope.define(name, ctorScheme);
              this.constructorTypes[name] = ctorScheme;
            }
          }
        }
        for (const [name, adt] of Object.entries(depModule.adts)) {
          if (isExportedFromModule(depModule, name, "type")) {
            this.checkTypeCollision(name, imp.moduleName, this.adts[name], imp.span, "type");
            this.adts[name] = adt;
          }
        }
        for (const [name, alias] of Object.entries(depModule.typeAliases)) {
          if (isExportedFromModule(depModule, name, "type")) {
            this.checkTypeCollision(name, imp.moduleName, this.typeAliases[name], imp.span, "type alias");
            this.typeAliases[name] = alias;
          }
        }
        for (const [name, opaque] of Object.entries(depModule.opaqueTypes)) {
          if (isExportedFromModule(depModule, name, "type")) {
            if (Object.hasOwn(this.opaqueTypes, name) && this.opaqueTypes[name].moduleName !== BUILTIN_MODULE_NAME && this.opaqueTypes[name].moduleName !== imp.moduleName) {
              throw new SemanticError(`Opaque type '${name}' conflicts with opaque type from module '${this.opaqueTypes[name].moduleName}'. ` + `Consider using a different name or qualified imports.`, imp.span, this.getFilePath());
            }
            this.opaqueTypes[name] = opaque;
          }
        }
        for (const [name, rec] of Object.entries(depModule.records)) {
          if (isExportedFromModule(depModule, name, "type")) {
            this.checkTypeCollision(name, imp.moduleName, this.records[name], imp.span, "record type");
            this.records[name] = rec;
          }
        }
        for (const [name, protocol] of Object.entries(depModule.protocols)) {
          if (isExportedFromModule(depModule, name, "protocol")) {
            this.checkTypeCollision(name, imp.moduleName, this.protocols[name], imp.span, "protocol");
            this.protocols[name] = protocol;
            const methodSchemes = addProtocolMethodsToScope(protocol, this.globalScope);
            for (const [methodName, scheme] of methodSchemes) {
              this.typeSchemes[methodName] = scheme;
            }
          }
        }
        for (const [op, info] of depModule.operators) {
          if (isExportedFromModule(depModule, op, "operator")) {
            this.operators.set(op, info);
            this.importedValues.set(op, imp.moduleName);
          }
        }
      }
    }
    const importedModuleNames = new Set(this.imports.map((imp) => imp.moduleName));
    const thisModuleName = this.getModuleName();
    for (const [depName, depModule] of this.dependencies) {
      if (importedModuleNames.has(depName))
        continue;
      for (const instance of depModule.instances) {
        if (instance.moduleName === thisModuleName)
          continue;
        const isDuplicate = this.instances.some((existing) => existing.protocolName === instance.protocolName && existing.moduleName === instance.moduleName && typeArgsEqual(existing.typeArgs, instance.typeArgs));
        if (!isDuplicate) {
          this.instances.push(instance);
        }
      }
    }
  }
  registerInfixDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "InfixDeclaration") {
        this.registerInfixDeclaration(decl);
        continue;
      }
    }
  }
  registerInfixDeclaration(decl) {
    if (this.importedValues.has(decl.operator)) {
      const sourceModule = this.importedValues.get(decl.operator);
      throw new SemanticError(`Cannot declare fixity for imported operator '${decl.operator}' from module '${sourceModule}'. ` + `Fixity is an intrinsic property of the operator and cannot be redefined.`, decl.span, this.getFilePath());
    }
    if (this.operators.has(decl.operator)) {
      throw new SemanticError(`Duplicate infix declaration for operator '${decl.operator}'`, decl.span, this.getFilePath());
    }
    const associativity = decl.fixity === "infixl" ? "left" : decl.fixity === "infixr" ? "right" : "none";
    if (decl.precedence < 0 || decl.precedence > 9) {
      throw new SemanticError(`Precedence must be between 0 and 9, got ${decl.precedence}`, decl.span, this.getFilePath());
    }
    this.operators.set(decl.operator, {
      precedence: decl.precedence,
      associativity
    });
    this.infixDeclarations.push(decl);
  }
  registerRecordTypeDeclaration(decl) {
    if (this.records[decl.name]) {
      throw new SemanticError(`Duplicate record type declaration for '${decl.name}'`, decl.span, this.getFilePath());
    }
    if (this.adts[decl.name]) {
      throw new SemanticError(`Record type '${decl.name}' conflicts with ADT '${decl.name}'`, decl.span, this.getFilePath());
    }
    if (this.typeAliases[decl.name]) {
      throw new SemanticError(`Record type '${decl.name}' conflicts with type alias '${decl.name}'`, decl.span, this.getFilePath());
    }
    if (this.opaqueTypes[decl.name]) {
      throw new SemanticError(`Record type '${decl.name}' conflicts with opaque type '${decl.name}'`, decl.span, this.getFilePath());
    }
    if (decl.constructors && decl.constructors.length > 0) {
      throw new SemanticError(`Type '${decl.name}' cannot have both constructors and record fields. ` + `Use 'type' for ADTs or record types separately.`, decl.span, this.getFilePath());
    }
    if (!decl.recordFields) {
      throw new SemanticError(`Record type '${decl.name}' is missing field definitions`, decl.span, this.getFilePath());
    }
    const fieldNames = new Set;
    for (const field2 of decl.recordFields) {
      if (fieldNames.has(field2.name)) {
        throw new SemanticError(`Duplicate field '${field2.name}' in record type '${decl.name}'`, field2.span, this.getFilePath());
      }
      fieldNames.add(field2.name);
    }
    const paramTypeVars = new Map;
    for (const param of decl.params) {
      paramTypeVars.set(param, freshType());
    }
    const semanticConstraints = [];
    if (decl.constraints) {
      for (const c of decl.constraints) {
        semanticConstraints.push({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => this.constructorArgToType(t, paramTypeVars))
        });
      }
    }
    for (const field2 of decl.recordFields) {
      if (containsRecordType(field2.type)) {
        throw new SemanticError(`Record types cannot be used directly in type annotations. ` + `Define a named record type using 'type RecordName = { ... }' and reference it by name.`, field2.span, this.getFilePath());
      }
    }
    const fields = decl.recordFields.map((field2) => ({
      name: field2.name,
      typeExpr: field2.type,
      span: field2.span
    }));
    this.records[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      constraints: semanticConstraints,
      fields,
      span: decl.span
    };
  }
  registerTypeDeclaration(decl) {
    const existingADT = this.adts[decl.name];
    if (existingADT) {
      if (existingADT.moduleName && existingADT.moduleName !== this.getModuleName()) {
        throw new SemanticError(`Type '${decl.name}' conflicts with type from module '${existingADT.moduleName}'. ` + `Consider using a different name or qualified imports.`, decl.span, this.getFilePath());
      }
      throw new SemanticError(`Duplicate type declaration for '${decl.name}'`, decl.span, this.getFilePath());
    }
    const paramSet = new Set;
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(`Duplicate type parameter '${param}' in type '${decl.name}'`, decl.span, this.getFilePath());
      }
      paramSet.add(param);
    }
    if (!decl.constructors || decl.constructors.length === 0) {
      if (decl.recordFields) {
        this.registerRecordTypeDeclaration(decl);
        return;
      }
      throw new SemanticError(`Type '${decl.name}' must have at least one constructor`, decl.span, this.getFilePath());
    }
    const paramTypeVars = new Map;
    for (const param of decl.params) {
      paramTypeVars.set(param, freshType());
    }
    const semanticConstraints = [];
    if (decl.constraints) {
      for (const c of decl.constraints) {
        semanticConstraints.push({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => this.constructorArgToType(t, paramTypeVars))
        });
      }
    }
    const constructorNames = decl.constructors.map((c) => c.name);
    this.adts[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      constructors: constructorNames,
      constraints: semanticConstraints,
      span: decl.span
    };
    const resultType = {
      kind: "con",
      name: decl.name,
      args: decl.params.map((p) => paramTypeVars.get(p))
    };
    for (const ctor of decl.constructors) {
      const existingCtor = this.constructors[ctor.name];
      if (existingCtor && existingCtor.moduleName === this.getModuleName()) {
        throw new SemanticError(`Duplicate constructor '${ctor.name}' (constructor names must be unique within a module)`, ctor.span, this.getFilePath());
      }
      this.constructors[ctor.name] = {
        arity: ctor.args.length,
        argTypes: ctor.args,
        parentType: decl.name,
        parentParams: decl.params,
        moduleName: this.getModuleName(),
        span: ctor.span
      };
      const ctorType = this.buildConstructorType(ctor, resultType, paramTypeVars);
      const quantifiedVars = new Set;
      for (const tv of paramTypeVars.values()) {
        quantifiedVars.add(tv.id);
      }
      const ctorScheme = {
        vars: quantifiedVars,
        constraints: semanticConstraints,
        type: ctorType,
        paramNames: invertContext(paramTypeVars)
      };
      this.globalScope.define(ctor.name, ctorScheme);
      this.constructorTypes[ctor.name] = ctorScheme;
    }
  }
  buildConstructorType(ctor, resultType, paramTypeVars) {
    if (ctor.args.length === 0) {
      return resultType;
    }
    const argTypes = ctor.args.map((argExpr) => this.constructorArgToType(argExpr, paramTypeVars));
    return fnChain(argTypes, resultType);
  }
  registerADTTypeDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "TypeDeclaration") {
        this.registerTypeDeclaration(decl);
        continue;
      }
    }
  }
  registerOpaqueTypeDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "OpaqueTypeDeclaration") {
        this.registerOpaqueType(decl);
        continue;
      }
    }
  }
  registerOpaqueType(decl) {
    if (Object.hasOwn(this.opaqueTypes, decl.name)) {
      const existing = this.opaqueTypes[decl.name];
      if (existing.moduleName && existing.moduleName !== this.getModuleName()) {
        throw new SemanticError(`Opaque type '${decl.name}' conflicts with opaque type from module '${existing.moduleName}'. ` + `Consider using a different name or qualified imports.`, decl.span, this.getFilePath());
      }
      throw new SemanticError(`Duplicate opaque type declaration for '${decl.name}'`, decl.span, this.getFilePath());
    }
    const paramSet = new Set;
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(`Duplicate type parameter '${param}' in opaque type '${decl.name}'`, decl.span, this.getFilePath());
      }
      paramSet.add(param);
    }
    this.opaqueTypes[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      span: decl.span
    };
  }
  registerTypeAliasDeclarations() {
    const typeAliasDecls = [];
    for (const decl of this.program.declarations) {
      if (decl.kind === "TypeAliasDeclaration") {
        this.registerTypeAliasWithoutValidation(decl);
        typeAliasDecls.push(decl);
        continue;
      }
    }
    return typeAliasDecls;
  }
  registerTypeAliasWithoutValidation(decl) {
    const existingAlias = this.typeAliases[decl.name];
    if (existingAlias) {
      if (existingAlias.moduleName && existingAlias.moduleName !== this.getModuleName()) {
        throw new SemanticError(`Type alias '${decl.name}' conflicts with type alias from module '${existingAlias.moduleName}'. ` + `Consider using a different name or qualified imports.`, decl.span, this.getFilePath());
      }
      throw new SemanticError(`Duplicate type alias '${decl.name}'`, decl.span, this.getFilePath());
    }
    const paramSet = new Set;
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(`Duplicate type parameter '${param}' in type alias '${decl.name}'`, decl.span, this.getFilePath());
      }
      paramSet.add(param);
    }
    this.typeAliases[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      value: decl.value,
      span: decl.span
    };
  }
  validateTypeAliasDeclarations(typeAliasDecls) {
    for (const decl of typeAliasDecls) {
      this.validateTypeAliasReferences(decl);
    }
  }
  validateTypeAliasReferences(decl) {
    if (decl.value.kind === "RecordType") {
      throw new SemanticError(`Type alias '${decl.name}' cannot directly define a record type. ` + `Use 'type ${decl.name} = { ... }' instead of 'type alias'.`, decl.value.span, this.getFilePath());
    }
    const paramSet = new Set(decl.params);
    const validationErrors = validateTypeExpr(this, decl.value, paramSet, decl.span);
    if (validationErrors.length > 0) {
      const err = validationErrors[0];
      const message = err.suggestion ? `${err.message}. ${err.suggestion}` : err.message;
      throw new SemanticError(message, err.span, this.getFilePath());
    }
  }
  registerProtocolDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "ProtocolDeclaration") {
        this.registerProtocol(decl);
        for (const method of decl.methods) {
          this.localProtocolMethods.add(method.name);
        }
        continue;
      }
    }
  }
  registerProtocol(decl) {
    const { protocols, globalScope, substitution } = this;
    const existingProtocol = protocols[decl.name];
    if (existingProtocol) {
      if (existingProtocol.moduleName && existingProtocol.moduleName !== this.getModuleName()) {
        throw new SemanticError(`Protocol '${decl.name}' conflicts with protocol from module '${existingProtocol.moduleName}'. ` + `Consider using a different name or qualified imports.`, decl.span, this.getFilePath());
      }
      throw new SemanticError(`Duplicate protocol '${decl.name}'`, decl.span, this.getFilePath());
    }
    const paramSet = new Set;
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(`Duplicate type parameter '${param}' in protocol '${decl.name}'`, decl.span, this.getFilePath());
      }
      paramSet.add(param);
    }
    if (decl.methods.length === 0) {
      throw new SemanticError(`Protocol '${decl.name}' must have at least one method`, decl.span, this.getFilePath());
    }
    const sharedTypeVarCtx = new Map;
    for (const param of decl.params) {
      sharedTypeVarCtx.set(param, freshType());
    }
    const protocolConstraint = {
      protocolName: decl.name,
      typeArgs: decl.params.map((p) => sharedTypeVarCtx.get(p))
    };
    const quantifiedVars = new Set;
    for (const tv of sharedTypeVarCtx.values()) {
      quantifiedVars.add(tv.id);
    }
    const methods = new Map;
    const methodNames = new Set;
    for (const method of decl.methods) {
      if (methodNames.has(method.name)) {
        throw new SemanticError(`Duplicate method '${method.name}' in protocol '${decl.name}'`, method.span, this.getFilePath());
      }
      methodNames.add(method.name);
      let methodType;
      if (method.type) {
        methodType = this.typeFromAnnotation(method.type, sharedTypeVarCtx);
      } else if (method.defaultImpl) {
        const lambdaExpr = makeLambda(method.defaultImpl.args, method.defaultImpl.body, method.span);
        const tempScope = this.createChildScope();
        const inferredType = this.analyzeExpr(lambdaExpr, {
          scope: tempScope,
          substitution
        });
        methodType = applySubstitution(inferredType, substitution);
        methodType = substituteProtocolVars(methodType, sharedTypeVarCtx);
      } else {
        throw new SemanticError(`Protocol method '${method.name}' must have either a type annotation or a default implementation`, method.span, this.getFilePath());
      }
      const methodInfo = {
        type: methodType,
        span: method.span
      };
      if (method.defaultImpl) {
        methodInfo.defaultImpl = {
          args: method.defaultImpl.args,
          body: method.defaultImpl.body
        };
      }
      methods.set(method.name, methodInfo);
      if (!globalScope.symbols.has(method.name)) {
        const allConstraints = [
          protocolConstraint,
          ...decl.constraints.map((c) => ({
            protocolName: c.protocolName,
            typeArgs: c.typeArgs.map((ta) => this.typeFromAnnotation(ta, sharedTypeVarCtx))
          }))
        ];
        const scheme = {
          vars: new Set(quantifiedVars),
          constraints: allConstraints,
          type: methodType
        };
        globalScope.symbols.set(method.name, scheme);
      }
    }
    const superclassConstraints = decl.constraints.map((c) => ({
      protocolName: c.protocolName,
      typeArgs: c.typeArgs.map((ta) => this.typeFromAnnotation(ta, sharedTypeVarCtx))
    }));
    protocols[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      superclassConstraints,
      methods,
      span: decl.span
    };
  }
  registerImplementationDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "ImplementationDeclaration") {
        this.registerImplementation(decl);
        continue;
      }
    }
  }
  registerImplementation(decl) {
    const protocol = this.protocols[decl.protocolName];
    if (!protocol) {
      throw new SemanticError(`Unknown protocol '${decl.protocolName}'`, decl.span, this.getFilePath());
    }
    if (decl.typeArgs.length !== protocol.params.length) {
      throw new SemanticError(`Protocol '${decl.protocolName}' expects ${protocol.params.length} type argument(s), but got ${decl.typeArgs.length}`, decl.span, this.getFilePath());
    }
    const typeVarCtx = new Map;
    const typeArgs = [];
    for (const typeArg of decl.typeArgs) {
      typeArgs.push(this.typeFromAnnotation(typeArg, typeVarCtx));
    }
    const constraints = [];
    for (const astConstraint of decl.constraints) {
      if (!this.protocols[astConstraint.protocolName]) {
        throw new SemanticError(`Unknown protocol '${astConstraint.protocolName}' in constraint`, decl.span, this.getFilePath());
      }
      const constraintTypeArgs = [];
      for (const typeArg of astConstraint.typeArgs) {
        constraintTypeArgs.push(this.typeFromAnnotation(typeArg, typeVarCtx));
      }
      constraints.push({
        protocolName: astConstraint.protocolName,
        typeArgs: constraintTypeArgs
      });
    }
    const implementedMethods = new Set(decl.methods.map((m) => m.name));
    const allMethods = new Set(protocol.methods.keys());
    if (decl.methods.length === 0 && (decl.protocolName === "Eq" || decl.protocolName === "Show")) {
      if (typeArgs.length === 1 && typeArgs[0].kind === "con") {
        const typeName = typeArgs[0].name;
        const typeDecl = this.getDeclarations().find((d) => (d.kind === "TypeDeclaration" || d.kind === "TypeAliasDeclaration") && d.name === typeName);
        if (typeDecl && typeDecl.kind === "TypeDeclaration") {
          let synthetic;
          if (decl.protocolName === "Eq") {
            synthetic = this.generateEqImplementation(typeDecl, protocol);
          } else {
            synthetic = this.generateShowImplementation(typeDecl);
          }
          if (synthetic) {
            synthetic.methods.forEach((impl, name) => {
              const methodInfo = protocol.methods.get(name);
              decl.methods.push({
                name,
                implementation: impl,
                span: decl.span
              });
              implementedMethods.add(name);
            });
          }
        }
      }
    }
    for (const methodName of allMethods) {
      const methodInfo = protocol.methods.get(methodName);
      if (!implementedMethods.has(methodName) && !methodInfo.defaultImpl) {
        throw new SemanticError(`Instance is missing implementation for method '${methodName}'`, decl.span, this.getFilePath());
      }
    }
    for (const implemented of implementedMethods) {
      if (!allMethods.has(implemented)) {
        throw new SemanticError(`Method '${implemented}' is not part of protocol '${decl.protocolName}'`, decl.span, this.getFilePath());
      }
    }
    const methods = new Map;
    const explicitMethods = new Set;
    for (const method of decl.methods) {
      if (method.args && method.args.length > 0) {
        const lambda = makeLambda(method.args, method.implementation, method.span);
        methods.set(method.name, lambda);
      } else {
        methods.set(method.name, method.implementation);
      }
      explicitMethods.add(method.name);
    }
    for (const [methodName, methodInfo] of protocol.methods) {
      if (!methods.has(methodName) && methodInfo.defaultImpl) {
        const defaultLambda = makeLambda(methodInfo.defaultImpl.args, methodInfo.defaultImpl.body, methodInfo.span);
        methods.set(methodName, defaultLambda);
      }
    }
    const instanceInfo = {
      protocolName: decl.protocolName,
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods,
      span: decl.span
    };
    for (const existing of this.instances) {
      if (existing.protocolName !== decl.protocolName)
        continue;
      if (instancesOverlap(existing, instanceInfo, this.instances)) {
        throw new SemanticError(`Overlapping implementation for protocol '${decl.protocolName}'`, decl.span, this.getFilePath());
      }
    }
    this.instances.push(instanceInfo);
    this.localInstances.push(instanceInfo);
  }
  generateShowImplementation(decl) {
    const typeArgs = [
      {
        kind: "con",
        name: decl.name,
        args: decl.params.map((param) => ({
          kind: "var",
          id: freshType().id
        }))
      }
    ];
    const paramMap = new Map;
    const headType = typeArgs[0];
    decl.params.forEach((p, i) => {
      paramMap.set(p, headType.args[i]);
    });
    const constraints = decl.params.map((p) => ({
      protocolName: "Show",
      typeArgs: [paramMap.get(p)]
    }));
    const methods = new Map;
    const span = decl.span;
    let body;
    const str = (s) => ({
      kind: "String",
      value: `"${s}"`,
      span
    });
    const append = (a, b) => ({
      kind: "Infix",
      left: a,
      operator: "++",
      right: b,
      span
    });
    const toStringWithUsage = (val, valType) => {
      const callee = {
        kind: "Var",
        name: "toString",
        namespace: "lower",
        span
      };
      const node = {
        kind: "Apply",
        callee,
        args: [val],
        span
      };
      this._resolvedProtocolUsages.set(callee, {
        protocolName: "Show",
        typeArgs: [valType]
      });
      return node;
    };
    if (decl.recordFields) {
      let expr = str(`${decl.name} { `);
      decl.recordFields.forEach((field2, i) => {
        if (i > 0)
          expr = append(expr, str(", "));
        expr = append(expr, str(`${field2.name} = `));
        const fieldAccess = {
          kind: "FieldAccess",
          target: { kind: "Var", name: "x_impl", namespace: "lower", span },
          field: field2.name,
          span
        };
        const fieldType = this.typeFromAnnotation(field2.type, paramMap);
        this.ensureNestedTupleInstances("Show", fieldType);
        expr = append(expr, toStringWithUsage(fieldAccess, fieldType));
      });
      expr = append(expr, str(" }"));
      body = expr;
    } else if (decl.constructors) {
      const branches = decl.constructors.map((ctor) => {
        const args = ctor.args.map((_, i) => `a${i}`);
        const pattern = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: args.map((a) => ({ kind: "VarPattern", name: a, span })),
          span
        };
        let expr;
        if (args.length === 0) {
          expr = str(`${ctor.name}`);
        } else {
          expr = str(`${ctor.name}(`);
          args.forEach((a, i) => {
            if (i > 0)
              expr = append(expr, str(", "));
            const argType = this.typeFromAnnotation(ctor.args[i], paramMap);
            this.ensureNestedTupleInstances("Show", argType);
            expr = append(expr, toStringWithUsage({ kind: "Var", name: a, namespace: "lower", span }, argType));
          });
          expr = append(expr, str(")"));
        }
        return { pattern, body: expr, span };
      });
      body = {
        kind: "Case",
        discriminant: { kind: "Var", name: "x_impl", namespace: "lower", span },
        branches,
        span
      };
    } else {
      body = str(`${decl.name}`);
    }
    methods.set("toString", {
      kind: "Lambda",
      args: [{ kind: "VarPattern", name: "x_impl", span }],
      body,
      span
    });
    return {
      protocolName: "Show",
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods: new Set(["toString"]),
      span
    };
  }
  ensureNestedTupleInstances(protocolName, type) {
    if (type.kind === "tuple") {
      for (const elem of type.elements) {
        this.ensureNestedTupleInstances(protocolName, elem);
      }
      if (!findInstanceForTypeInternal(protocolName, type, this.instances)) {
        const instance = generateSyntheticInstance(protocolName, type, this._resolvedProtocolUsages);
        this.instances.push(instance);
      }
    } else if (type.kind === "con") {
      for (const arg of type.args) {
        this.ensureNestedTupleInstances(protocolName, arg);
      }
    }
  }
  generateEqImplementation(decl, protocol) {
    const typeParams = new Set(decl.params);
    if (decl.recordFields) {
      for (const field2 of decl.recordFields) {
        this.validateTypeImplementsEq(field2.type, decl.span, typeParams, new Set, decl.name);
      }
    }
    if (decl.constructors) {
      for (const ctor of decl.constructors) {
        for (const arg of ctor.args) {
          this.validateTypeImplementsEq(arg, decl.span, typeParams, new Set, decl.name);
        }
      }
    }
    const typeArgs = [
      {
        kind: "con",
        name: decl.name,
        args: decl.params.map((param) => ({
          kind: "var",
          id: freshType().id
        }))
      }
    ];
    const paramMap = new Map;
    const headType = typeArgs[0];
    if (headType && headType.kind === "con") {
      const typeCon = headType;
      decl.params.forEach((p, i) => {
        paramMap.set(p, typeCon.args[i]);
      });
    }
    const constraints = decl.params.map((p) => {
      const tvar = paramMap.get(p);
      if (!tvar)
        throw new Error("Type variable missing");
      return {
        protocolName: "Eq",
        typeArgs: [tvar]
      };
    });
    const methods = new Map;
    const xVar = "x_impl";
    const yVar = "y_impl";
    const span = decl.span;
    let body;
    if (decl.recordFields) {
      const checks = decl.recordFields.map((field2) => {
        const node = {
          kind: "Infix",
          left: {
            kind: "FieldAccess",
            target: { kind: "Var", name: xVar, namespace: "lower", span },
            field: field2.name,
            span
          },
          operator: "==",
          right: {
            kind: "FieldAccess",
            target: { kind: "Var", name: yVar, namespace: "lower", span },
            field: field2.name,
            span
          },
          span
        };
        const fieldType = this.typeFromAnnotation(field2.type, paramMap);
        this.ensureNestedTupleInstances("Eq", fieldType);
        this._resolvedProtocolUsages.set(node, {
          protocolName: "Eq",
          typeArgs: [fieldType]
        });
        return node;
      });
      if (checks.length === 0) {
        body = { kind: "Var", name: "True", namespace: "upper", span };
      } else {
        body = checks.reduce((acc, check) => ({
          kind: "Infix",
          left: acc,
          operator: "&&",
          right: check,
          span
        }));
      }
    } else if (decl.constructors) {
      const branches = [];
      const hasMultipleConstructors = decl.constructors.length > 1;
      for (const ctor of decl.constructors) {
        const argsX = ctor.args.map((_, i) => ({
          kind: "VarPattern",
          name: `a_${i}`,
          span
        }));
        const argsY = ctor.args.map((_, i) => ({
          kind: "VarPattern",
          name: `b_${i}`,
          span
        }));
        const patX = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: argsX,
          span
        };
        const patY = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: argsY,
          span
        };
        const pattern = {
          kind: "TuplePattern",
          elements: [patX, patY],
          span
        };
        const checks = ctor.args.map((arg, i) => {
          const node = {
            kind: "Infix",
            left: { kind: "Var", name: `a_${i}`, namespace: "lower", span },
            operator: "==",
            right: { kind: "Var", name: `b_${i}`, namespace: "lower", span },
            span
          };
          const argType = this.typeFromAnnotation(arg, paramMap);
          this.ensureNestedTupleInstances("Eq", argType);
          this._resolvedProtocolUsages.set(node, {
            protocolName: "Eq",
            typeArgs: [argType]
          });
          return node;
        });
        let branchBody;
        if (checks.length === 0) {
          branchBody = { kind: "Var", name: "True", namespace: "upper", span };
        } else {
          branchBody = checks.reduce((acc, check) => ({
            kind: "Infix",
            left: acc,
            operator: "&&",
            right: check,
            operatorInfo: {
              precedence: 3,
              associativity: "right"
            },
            span
          }));
        }
        branches.push({ pattern, body: branchBody, span });
      }
      if (hasMultipleConstructors || decl.constructors.length === 0) {
        branches.push({
          pattern: { kind: "WildcardPattern", span },
          body: { kind: "Var", name: "False", namespace: "upper", span },
          span
        });
      }
      body = {
        kind: "Case",
        discriminant: {
          kind: "Tuple",
          elements: [
            { kind: "Var", name: xVar, namespace: "lower", span },
            { kind: "Var", name: yVar, namespace: "lower", span }
          ],
          span
        },
        branches,
        span
      };
    } else {
      body = { kind: "Var", name: "True", namespace: "upper", span };
    }
    methods.set("==", {
      kind: "Lambda",
      args: [
        { kind: "VarPattern", name: xVar, span },
        { kind: "VarPattern", name: yVar, span }
      ],
      body,
      span
    });
    return {
      protocolName: protocol.name,
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods: new Set(["=="]),
      span: decl.span
    };
  }
  validateTypeImplementsEq(type, declSpan, typeParams, checkedTypes, derivingTypeName) {
    const typeKey = JSON.stringify(type);
    if (checkedTypes.has(typeKey))
      return;
    checkedTypes.add(typeKey);
    switch (type.kind) {
      case "FunctionType":
        throw new SemanticError(`Type mismatch: cannot unify 'Int' with 'Int -> Int'. No instance of Eq for function type.`, declSpan, this.getFilePath());
      case "TypeRef": {
        const firstChar = type.name.charAt(0);
        const isTypeVariable = firstChar === firstChar.toLowerCase() && firstChar !== firstChar.toUpperCase();
        if (isTypeVariable && typeParams.has(type.name))
          return;
        if (derivingTypeName && type.name === derivingTypeName)
          return;
        const typeForLookup = {
          kind: "con",
          name: type.name,
          args: []
        };
        if (findInstanceForTypeInternal("Eq", typeForLookup, this.instances)) {
          return;
        }
        if (type.name === "List" && type.args.length === 1) {
          this.validateTypeImplementsEq(type.args[0], declSpan, typeParams, checkedTypes, derivingTypeName);
          return;
        }
        const localDecl = this.getDeclarations().find((d) => (d.kind === "TypeDeclaration" || d.kind === "TypeAliasDeclaration") && d.name === type.name);
        if (localDecl) {
          if (localDecl.kind === "TypeDeclaration") {
            const hasExplicit = this.getDeclarations().some((d) => d.kind === "ImplementationDeclaration" && d.protocolName === "Eq" && d.typeArgs.length > 0 && d.typeArgs[0].kind === "TypeRef" && d.typeArgs[0].name === type.name);
            if (hasExplicit)
              return;
            throw new SemanticError(`Type '${type.name}' does not implement 'Eq'. Implicit 'Eq' requires all fields to implement 'Eq'.`, declSpan, this.getFilePath());
          }
        }
        type.args.forEach((arg) => this.validateTypeImplementsEq(arg, declSpan, typeParams, checkedTypes, derivingTypeName));
        return;
      }
      case "TupleType":
        type.elements.forEach((elem) => this.validateTypeImplementsEq(elem, declSpan, typeParams, checkedTypes, derivingTypeName));
        return;
      case "RecordType":
        type.fields.forEach((f) => this.validateTypeImplementsEq(f.type, declSpan, typeParams, checkedTypes, derivingTypeName));
        return;
      case "QualifiedType":
        this.validateTypeImplementsEq(type.type, declSpan, typeParams, checkedTypes, derivingTypeName);
        return;
      default:
        return;
    }
  }
  autoImplementProtocols() {
    for (const decl of this.getDeclarations()) {
      if (decl.kind === "TypeDeclaration") {
        const eqInstance = this.autoImplementProtocolForType("Eq", decl);
        if (eqInstance) {
          this.instances.push(eqInstance);
          this.localInstances.push(eqInstance);
        }
        const showInstance = this.autoImplementProtocolForType("Show", decl);
        if (showInstance) {
          this.instances.push(showInstance);
          this.localInstances.push(showInstance);
        }
      }
    }
  }
  autoImplementProtocolForType(protocolName, decl) {
    const protocol = this.protocols[protocolName];
    if (!protocol)
      return;
    const isVibe = protocol.moduleName === "Vibe" || protocol.moduleName === "Vibe.Basics";
    if (!isVibe)
      return;
    if (!this.canDeclImplementProtocol(decl, protocolName)) {
      return;
    }
    for (const existing of this.instances) {
      if (existing.protocolName !== protocolName)
        continue;
      if (existing.typeArgs.length === 0)
        continue;
      const typeArg = existing.typeArgs[0];
      if (typeArg?.kind === "con" && typeArg.name === decl.name) {
        return;
      }
    }
    if (protocolName === "Eq") {
      return this.generateEqImplementation(decl, protocol);
    } else if (protocolName === "Show") {
      return this.generateShowImplementation(decl);
    }
    return;
  }
  registerValueDeclarations() {
    for (const decl of this.getDeclarations()) {
      if (decl.kind === "ValueDeclaration" || decl.kind === "DecoratedDeclaration") {
        if (decl.kind === "DecoratedDeclaration") {
          this.validateDecoratedDeclaration(decl);
        }
        this.registerValue(decl);
        continue;
      }
      if (decl.kind === "TypeAnnotationDeclaration") {
        if (Object.hasOwn(this.annotations, decl.name)) {
          throw new SemanticError(`Duplicate type annotation for '${decl.name}'`, decl.span, this.getFilePath());
        }
        this.annotations[decl.name] = decl;
      }
    }
  }
  canDeclImplementProtocol(decl, protocolName) {
    const typeParams = new Set(decl.params);
    if (decl.recordFields) {
      for (const field2 of decl.recordFields) {
        if (!this.canTypeImplementProtocol(field2.type, protocolName, typeParams)) {
          return false;
        }
      }
    }
    if (decl.constructors) {
      for (const ctor of decl.constructors) {
        for (const arg of ctor.args) {
          if (!this.canTypeImplementProtocol(arg, protocolName, typeParams)) {
            return false;
          }
        }
      }
    }
    return true;
  }
  canTypeImplementProtocol(type, protocolName, typeParams, checkedTypes = new Set) {
    const typeKey = JSON.stringify(type);
    if (checkedTypes.has(typeKey))
      return true;
    checkedTypes.add(typeKey);
    const typeMatchesInstanceArg = (instanceArg, typeName) => {
      return instanceArg.kind === "con" && instanceArg.name === typeName;
    };
    switch (type.kind) {
      case "FunctionType":
        const hasFunctionInstance = this.instances.some((inst) => inst.protocolName === protocolName && inst.typeArgs.length > 0 && inst.typeArgs[0].kind === "fun");
        return hasFunctionInstance;
      case "TypeRef": {
        const firstChar = type.name.charAt(0);
        if (firstChar === firstChar.toLowerCase() && firstChar !== firstChar.toUpperCase()) {
          return true;
        }
        const hasInstance = this.instances.some((inst) => inst.protocolName === protocolName && inst.typeArgs.length > 0 && typeMatchesInstanceArg(inst.typeArgs[0], type.name));
        if (hasInstance) {
          return type.args.every((arg) => this.canTypeImplementProtocol(arg, protocolName, typeParams, checkedTypes));
        }
        const protocol = this.protocols[protocolName];
        if (!protocol) {
          return false;
        }
        const allMethodsHaveDefaults = Array.from(protocol.methods.values()).every((method) => method.defaultImpl !== undefined);
        if (allMethodsHaveDefaults) {
          return true;
        }
        const localDecl = this.getDeclarations().find((d) => (d.kind === "TypeDeclaration" || d.kind === "TypeAliasDeclaration") && d.name === type.name);
        if (localDecl && localDecl.kind === "TypeDeclaration") {
          return type.args.every((arg) => this.canTypeImplementProtocol(arg, protocolName, typeParams, checkedTypes));
        }
        if (localDecl && localDecl.kind === "TypeAliasDeclaration") {
          return type.args.every((arg) => this.canTypeImplementProtocol(arg, protocolName, typeParams, checkedTypes));
        }
        return false;
      }
      case "TupleType":
        return type.elements.every((elem) => this.canTypeImplementProtocol(elem, protocolName, typeParams, checkedTypes));
      case "RecordType":
        return type.fields.every((f) => this.canTypeImplementProtocol(f.type, protocolName, typeParams, checkedTypes));
      case "QualifiedType":
        return this.canTypeImplementProtocol(type.type, protocolName, typeParams, checkedTypes);
      default:
        return false;
    }
  }
  validateInfixDeclarationsHaveDefinitions() {
    for (const decl of this.infixDeclarations) {
      const op = decl.operator;
      const hasLocalDefinition = Object.hasOwn(this.values, op);
      const isLocalProtocolMethod = this.localProtocolMethods.has(op);
      if (!hasLocalDefinition && !isLocalProtocolMethod) {
        throw new SemanticError(`Infix declaration for operator '${op}' has no corresponding function definition. ` + `Define the operator in this module or remove the fixity declaration.`, decl.span, this.getFilePath());
      }
    }
  }
  validateDecoratedDeclaration(decl) {
    const validDecorators = ["external", "get", "call", "val", "import"];
    if (!validDecorators.includes(decl.decorator)) {
      throw new SemanticError(`Unknown decorator '@${decl.decorator}'. Valid decorators are: @external, @get, @call, @val, @import`, decl.span, this.getFilePath());
    }
    switch (decl.decorator) {
      case "external":
        if (decl.args.length !== 2) {
          throw new SemanticError(`@external requires exactly 2 string arguments (module path and export name), got ${decl.args.length}`, decl.span, this.getFilePath());
        }
        break;
      case "get":
      case "call":
      case "val":
        if (decl.args.length !== 1) {
          throw new SemanticError(`@${decl.decorator} requires exactly 1 string argument (property key), got ${decl.args.length}`, decl.span, this.getFilePath());
        }
        break;
      case "import":
        if (decl.args.length !== 1) {
          throw new SemanticError(`@import requires exactly 1 string argument (module path), got ${decl.args.length}`, decl.span, this.getFilePath());
        }
        break;
    }
  }
  isFFIDeclaration(decl) {
    return decl.kind === "DecoratedDeclaration";
  }
  validateDecoratedPropertyType(decl, type) {
    const variant = decl.decorator;
    if (variant === "val") {
      return;
    }
    const params = flattenFunctionParams(type);
    if (variant === "get") {
      if (params.length !== 1) {
        throw new SemanticError(`@get declaration '${decl.name}' must have type A -> B (exactly one argument), got ${params.length} argument(s)`, decl.span, this.getFilePath());
      }
    } else if (variant === "call") {
      if (params.length < 1) {
        throw new SemanticError(`@call declaration '${decl.name}' must have at least one argument`, decl.span, this.getFilePath());
      }
    }
  }
  validateAnnotationsAndSeedGlobalNames() {
    this.validateInfixDeclarationsHaveDefinitions();
    for (const [name, ann] of Object.entries(this.annotations)) {
      if (!Object.hasOwn(this.values, name)) {
        throw new SemanticError(`Type annotation for '${name}' has no matching definition`, ann.span, this.getFilePath());
      }
      const value2 = this.values[name];
      if (value2.declaration.kind === "DecoratedDeclaration") {
        throw new SemanticError(`Decorated declaration '${name}' already includes a type annotation`, ann.span, this.getFilePath());
      }
      value2.annotation = ann.annotation;
    }
    for (const [name, info] of Object.entries(this.values)) {
      const annotationExpr = info.annotation ?? (info.declaration.kind === "DecoratedDeclaration" ? info.declaration.annotation : undefined);
      let annotationType;
      let annotatedConstraints;
      if (annotationExpr) {
        const typeVars = collectTypeVariables(annotationExpr);
        const validationErrors = validateTypeExpr(this, annotationExpr, typeVars, info.declaration.span);
        if (validationErrors.length > 0) {
          const err = validationErrors[0];
          const message = err.suggestion ? `${err.message}. ${err.suggestion}` : err.message;
          throw new SemanticError(message, err.span, this.getFilePath());
        }
        const result = this.typeFromAnnotationWithConstraints(annotationExpr, new Map);
        annotationType = result.type;
        annotatedConstraints = result.constraints.length > 0 ? result.constraints : undefined;
        if (annotatedConstraints) {
          info.annotatedConstraints = annotatedConstraints;
        }
        if (result.paramNames.size > 0) {
          info.annotatedParamNames = result.paramNames;
        }
      }
      const seeded = annotationType ?? this.seedValueType(info.declaration);
      this.declareSymbol(this.globalScope, name, { vars: new Set, constraints: [], type: seeded }, info.declaration.span);
      this.types[name] = seeded;
    }
  }
  typeFromAnnotationWithConstraints(annotation, context = new Map) {
    const { protocols } = this;
    if (annotation.kind === "QualifiedType") {
      const constraints = [];
      for (const astConstraint of annotation.constraints) {
        const protocol = protocols[astConstraint.protocolName];
        if (!protocol) {
          throw new SemanticError(`Unknown protocol '${astConstraint.protocolName}' in type constraint`, astConstraint.span, this.getFilePath());
        }
        if (astConstraint.typeArgs.length !== protocol.params.length) {
          throw new SemanticError(`Protocol '${astConstraint.protocolName}' expects ${protocol.params.length} type argument(s), but constraint has ${astConstraint.typeArgs.length}`, astConstraint.span, this.getFilePath());
        }
        const constraintTypeArgs = [];
        for (const typeArg of astConstraint.typeArgs) {
          constraintTypeArgs.push(this.typeFromAnnotation(typeArg, context));
        }
        for (let i = 0;i < constraintTypeArgs.length; i++) {
          const typeArg = constraintTypeArgs[i];
          if (typeArg.kind !== "var") {
            throw new SemanticError(`Constraint '${astConstraint.protocolName}' must be applied to type variables, not concrete types`, astConstraint.span, this.getFilePath());
          }
        }
        constraints.push({
          protocolName: astConstraint.protocolName,
          typeArgs: constraintTypeArgs
        });
      }
      const innerResult = this.typeFromAnnotationWithConstraints(annotation.type, context);
      const paramNames2 = invertContext(context);
      return {
        type: innerResult.type,
        constraints: [...constraints, ...innerResult.constraints],
        paramNames: paramNames2
      };
    }
    const type = this.typeFromAnnotation(annotation, context);
    const paramNames = invertContext(context);
    return { type, constraints: [], paramNames };
  }
  inferValueDeclarations() {
    this.concretizeInstanceTypeArgs();
    const depGraph = buildDependencyGraph(this.values);
    const sccs = computeSCCs(depGraph);
    for (const scc of sccs) {
      const externals = [];
      const valueDecls = [];
      for (const name of scc) {
        const info = this.values[name];
        if (this.isFFIDeclaration(info.declaration)) {
          externals.push(name);
        } else {
          valueDecls.push(name);
        }
      }
      for (const name of externals) {
        const info = this.values[name];
        const decl = info.declaration;
        if (decl.kind === "DecoratedDeclaration") {
          const result = this.typeFromAnnotationWithConstraints(decl.annotation, new Map);
          info.type = result.type;
          if (result.constraints.length > 0) {
            info.annotatedConstraints = result.constraints;
          }
          if (decl.decorator === "get" || decl.decorator === "call") {
            this.validateDecoratedPropertyType(decl, info.type);
          }
          const freeVars = getFreeTypeVars(info.type, this.substitution);
          const scheme = {
            vars: freeVars,
            constraints: result.constraints,
            type: info.type,
            paramNames: result.paramNames.size > 0 ? result.paramNames : undefined
          };
          this.globalScope.symbols.set(info.declaration.name, scheme);
          this.typeSchemes[name] = scheme;
        }
      }
      if (valueDecls.length > 0) {
        this.resetConstraintContext();
        const inferredTypes = new Map;
        for (const name of valueDecls) {
          const info = this.values[name];
          if (info.declaration.kind !== "ValueDeclaration")
            continue;
          const declaredType = this.types[name];
          const annotationType = info.annotation ? this.typeFromAnnotation(info.annotation, new Map) : undefined;
          const inferred = this.analyzeValueDeclaration(info.declaration, this.globalScope, this.substitution, declaredType, annotationType);
          inferredTypes.set(name, inferred);
          this.types[name] = inferred;
          info.type = inferred;
        }
        for (const name of valueDecls) {
          const info = this.values[name];
          const inferred = inferredTypes.get(name);
          const generalizedScheme = this.generalizeWithAnnotatedConstraints(inferred, new Scope, this.substitution, info.annotatedConstraints, info.declaration.span);
          if (info.annotatedParamNames) {
            generalizedScheme.paramNames = resolveParamNames(info.annotatedParamNames, this.substitution);
          }
          this.globalScope.define(name, generalizedScheme);
          this.typeSchemes[name] = generalizedScheme;
        }
        for (const usage of this._pendingProtocolUsages) {
          const resolvedArgs = usage.constraint.typeArgs.map((t) => applySubstitution(t, this.substitution));
          this._resolvedProtocolUsages.set(usage.node, {
            protocolName: usage.constraint.protocolName,
            typeArgs: resolvedArgs
          });
        }
      }
    }
  }
  generalizeWithAnnotatedConstraints(type, scope, substitution, annotatedConstraints, span) {
    const typeFreeVars = getFreeTypeVars(type, substitution);
    const scopeFreeVars = getFreeTypeVarsInScope(scope, substitution);
    const quantified = new Set;
    for (const v of typeFreeVars) {
      if (!scopeFreeVars.has(v)) {
        quantified.add(v);
      }
    }
    const rawConstraints = this.getCollectedConstraints();
    const resolvedConstraints = applySubstitutionToConstraints(rawConstraints, substitution);
    for (const c of resolvedConstraints) {
      const resolvedTypeArgs = c.typeArgs.map((t) => applySubstitution(t, substitution));
      const isFullyPolymorphic = resolvedTypeArgs.every((t) => {
        if (t.kind === "var" && quantified.has(t.id)) {
          return true;
        }
        return false;
      });
      if (isFullyPolymorphic) {
        continue;
      }
      const resolvedConstraint = {
        protocolName: c.protocolName,
        typeArgs: resolvedTypeArgs
      };
      const lookupResult = validateConstraintSatisfiable(resolvedConstraint, this.instances);
      if (!lookupResult.found) {
        const typeArgsStr = resolvedTypeArgs.map((t) => formatType(t)).join(", ");
        if (lookupResult.reason === "unsatisfied-constraint") {
          throw new SemanticError(`No instance of '${c.protocolName}' for type(s) '${typeArgsStr}'. ` + `The instance requires '${lookupResult.constraint}' for '${lookupResult.forType}', ` + `but no such instance exists.`, span, this.getFilePath());
        } else {
          throw new SemanticError(`No instance of '${c.protocolName}' for type(s) '${typeArgsStr}'. ` + `Add an implementation: implement ${c.protocolName} ${typeArgsStr} where ...`, span, this.getFilePath());
        }
      }
    }
    const inferredRelevantConstraints = [];
    for (const c of resolvedConstraints) {
      const typeArgFreeVars = [];
      for (const t of c.typeArgs) {
        const freeVars = getFreeTypeVars(t, substitution);
        for (const v of freeVars) {
          typeArgFreeVars.push(v);
        }
      }
      const hasQuantifiedVar = typeArgFreeVars.some((v) => quantified.has(v));
      const hasUnquantifiedVar = typeArgFreeVars.some((v) => !quantified.has(v));
      if (typeArgFreeVars.length === 0) {
        continue;
      }
      if (hasUnquantifiedVar && !hasQuantifiedVar) {
        const typeArgsStr = c.typeArgs.map((t) => formatType(t)).join(", ");
        throw new SemanticError(`Ambiguous type variable in '${c.protocolName}' constraint. ` + `The type '${typeArgsStr}' contains type variable(s) that do not appear ` + `in the expression's type, so they cannot be determined. ` + `Consider adding a type annotation to make the type concrete.`, span, this.getFilePath());
      }
      if (hasQuantifiedVar) {
        inferredRelevantConstraints.push(c);
      }
    }
    let allConstraints = [...inferredRelevantConstraints];
    if (annotatedConstraints && annotatedConstraints.length > 0) {
      const resolvedAnnotated = applySubstitutionToConstraints(annotatedConstraints, substitution);
      for (const c of resolvedAnnotated) {
        for (const typeArg of c.typeArgs) {
          const freeVars = getFreeTypeVars(typeArg, substitution);
          let hasQuantifiedVar = false;
          for (const v of freeVars) {
            if (quantified.has(v)) {
              hasQuantifiedVar = true;
            }
          }
          if (freeVars.size === 0) {
            throw new SemanticError(`Constraint '${c.protocolName}' is on a concrete type, which is not allowed in type annotations`, span, this.getFilePath());
          }
          if (!hasQuantifiedVar) {
            throw new SemanticError(`Constraint '${c.protocolName}' references type variables not used in the function type`, span, this.getFilePath());
          }
        }
        allConstraints.push(c);
      }
    }
    const uniqueConstraints = [];
    for (const c of allConstraints) {
      const isDuplicate = uniqueConstraints.some((uc) => uc.protocolName === c.protocolName && uc.typeArgs.length === c.typeArgs.length && uc.typeArgs.every((t, i) => typesEqual(t, c.typeArgs[i])));
      if (!isDuplicate) {
        uniqueConstraints.push(c);
      }
    }
    const constraintsToRemove = [];
    for (const c of uniqueConstraints) {
      const resolvedTypeArgs = c.typeArgs.map((t) => applySubstitution(t, substitution));
      const varPositions = [];
      const concretePositions = [];
      for (let i = 0;i < resolvedTypeArgs.length; i++) {
        const arg = resolvedTypeArgs[i];
        if (arg.kind === "var" && quantified.has(arg.id)) {
          varPositions.push(i);
        } else if (arg.kind !== "var") {
          concretePositions.push(i);
        }
      }
      if (concretePositions.length > 0 && varPositions.length > 0) {
        const matchingInstances = this.instances.filter((inst) => {
          if (inst.protocolName !== c.protocolName)
            return false;
          if (inst.typeArgs.length !== resolvedTypeArgs.length)
            return false;
          for (const pos of concretePositions) {
            const instArg = inst.typeArgs[pos];
            const constraintArg = resolvedTypeArgs[pos];
            if (!instanceTypeMatches(instArg, constraintArg))
              return false;
          }
          for (const pos of varPositions) {
            const instArg = inst.typeArgs[pos];
            if (instArg.kind === "var")
              return false;
          }
          return true;
        });
        if (matchingInstances.length === 1) {
          const matchingInst = matchingInstances[0];
          for (const pos of varPositions) {
            const typeVar = resolvedTypeArgs[pos];
            const instType = matchingInst.typeArgs[pos];
            if (typeVar.kind === "var") {
              substitution.set(typeVar.id, instType);
              quantified.delete(typeVar.id);
            }
          }
          constraintsToRemove.push(c);
        }
      }
    }
    const finalConstraints = uniqueConstraints.filter((c) => !constraintsToRemove.includes(c));
    const finalType = applySubstitution(type, substitution);
    const finalTypeFreeVars = getFreeTypeVars(finalType, substitution);
    for (const c of finalConstraints) {
      for (const typeArg of c.typeArgs) {
        const resolvedTypeArg = applySubstitution(typeArg, substitution);
        const constraintFreeVars = getFreeTypeVars(resolvedTypeArg, substitution);
        for (const v of constraintFreeVars) {
          if (!finalTypeFreeVars.has(v)) {
            throw new SemanticError(`Ambiguous type variable in '${c.protocolName}' constraint. ` + `The type variable does not appear in the expression's type, ` + `so it cannot be determined. Consider adding a type annotation ` + `to make the type concrete.`, span, this.getFilePath());
          }
        }
      }
    }
    return { vars: quantified, constraints: finalConstraints, type: finalType };
  }
  validateImplementationMethodExpressions() {
    for (const instance of this.localInstances) {
      for (const methodName of instance.explicitMethods) {
        const methodExpr = instance.methods.get(methodName);
        if (methodExpr) {
          this.validateExpressionIdentifiers(methodExpr, this.globalScope, instance.protocolName, methodName);
        }
      }
    }
  }
  validateExpressionIdentifiers(expr, scope, protocolName, methodName) {
    const validate = (e, s = scope) => this.validateExpressionIdentifiers(e, s, protocolName, methodName);
    switch (expr.kind) {
      case "Var": {
        const name = expr.name;
        if (this.constructors[name]) {
          return;
        }
        if (!symbolExists(scope, name)) {
          this.addError(`Undefined name '${name}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span);
        }
        return;
      }
      case "Lambda": {
        const childScope = new Scope(scope);
        for (const arg of expr.args) {
          bindPatternNames(arg, childScope);
        }
        validate(expr.body, childScope);
        return;
      }
      case "Apply": {
        validate(expr.callee);
        for (const arg of expr.args) {
          validate(arg);
        }
        return;
      }
      case "If": {
        validate(expr.condition);
        validate(expr.thenBranch);
        validate(expr.elseBranch);
        return;
      }
      case "LetIn": {
        const childScope = new Scope(scope);
        for (const binding of expr.bindings) {
          validate(binding.body);
          childScope.symbols.set(binding.name, {
            vars: new Set,
            constraints: [],
            type: { kind: "var", id: -1 }
          });
        }
        validate(expr.body, childScope);
        return;
      }
      case "Case": {
        validate(expr.discriminant);
        for (const branch of expr.branches) {
          const branchScope = new Scope(scope);
          bindPatternNames(branch.pattern, branchScope);
          validate(branch.body, branchScope);
        }
        return;
      }
      case "Infix": {
        if (!symbolExists(scope, expr.operator)) {
          throw new SemanticError(`Undefined operator '${expr.operator}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
        }
        validate(expr.left);
        validate(expr.right);
        return;
      }
      case "Unary": {
        validate(expr.operand);
        return;
      }
      case "Paren": {
        validate(expr.expression);
        return;
      }
      case "Tuple": {
        for (const element of expr.elements) {
          validate(element);
        }
        return;
      }
      case "List": {
        for (const element of expr.elements) {
          validate(element);
        }
        return;
      }
      case "ListRange": {
        validate(expr.start);
        validate(expr.end);
        return;
      }
      case "Record": {
        for (const field2 of expr.fields) {
          validate(field2.value);
        }
        return;
      }
      case "RecordUpdate": {
        if (!symbolExists(scope, expr.base)) {
          this.addError(`Undefined name '${expr.base}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span);
        }
        for (const field2 of expr.fields) {
          validate(field2.value);
        }
        return;
      }
      case "FieldAccess": {
        const resolved = this.validateModuleFieldAccess(expr, protocolName, methodName);
        if (!resolved) {
          validate(expr.target);
        }
        return;
      }
      case "Number":
      case "String":
      case "Char":
      case "Unit":
        return;
    }
  }
  validateModuleFieldAccess(expr, protocolName, methodName) {
    const { imports, dependencies } = this;
    const parts = [];
    let current2 = expr;
    while (current2.kind === "FieldAccess") {
      parts.unshift(current2.field);
      current2 = current2.target;
    }
    if (current2.kind !== "Var") {
      return false;
    }
    const baseName = current2.name;
    parts.unshift(baseName);
    for (const imp of imports) {
      if (imp.alias && baseName === imp.alias && parts.length >= 2) {
        const depModule = dependencies.get(imp.moduleName);
        if (!depModule) {
          throw new SemanticError(`Module '${imp.moduleName}' (aliased as '${imp.alias}') not found in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
        }
        const fieldName = parts[1];
        const valueInfo = depModule.values[fieldName];
        if (valueInfo) {
          if (!isExportedFromModule(depModule, fieldName, "value")) {
            throw new SemanticError(`'${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
          }
          return true;
        }
        const ctorInfo = depModule.constructors[fieldName];
        if (ctorInfo) {
          if (!isExportedFromModule(depModule, fieldName, "constructor")) {
            throw new SemanticError(`Constructor '${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
          }
          return true;
        }
        throw new SemanticError(`'${fieldName}' is not defined in module '${imp.moduleName}' (aliased as '${imp.alias}') in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
      }
      const importParts = imp.moduleName.split(".");
      if (!imp.alias && importParts.length <= parts.length - 1) {
        let matches = true;
        for (let i = 0;i < importParts.length; i++) {
          if (importParts[i] !== parts[i]) {
            matches = false;
            break;
          }
        }
        if (matches) {
          const depModule = dependencies.get(imp.moduleName);
          if (!depModule) {
            throw new SemanticError(`Module '${imp.moduleName}' not found in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
          }
          const fieldParts = parts.slice(importParts.length);
          if (fieldParts.length === 1) {
            const fieldName = fieldParts[0];
            const valueInfo = depModule.values[fieldName];
            if (valueInfo) {
              if (!isExportedFromModule(depModule, fieldName, "value")) {
                throw new SemanticError(`'${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
              }
              return true;
            }
            const ctorInfo = depModule.constructors[fieldName];
            if (ctorInfo) {
              if (!isExportedFromModule(depModule, fieldName, "constructor")) {
                throw new SemanticError(`Constructor '${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
              }
              return true;
            }
            throw new SemanticError(`'${fieldName}' is not defined in module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
          }
        }
      }
    }
    return false;
  }
  validateInstanceConstraintSatisfiability() {
    for (const instance of this.localInstances) {
      if (instance.constraints.length === 0)
        continue;
      for (const constraint of instance.constraints) {
        const constraintProtocol = this.protocols[constraint.protocolName];
        if (!constraintProtocol) {
          throw new SemanticError(`Instance constraint references unknown protocol '${constraint.protocolName}'`, instance.span, this.getFilePath());
        }
      }
    }
  }
  validateProtocolDefaultImplementations() {
    for (const [protocolName, protocol] of Object.entries(this.protocols)) {
      if (protocol.moduleName !== this.getModuleName())
        continue;
      for (const [methodName, methodInfo] of protocol.methods) {
        if (methodInfo.defaultImpl) {
          const methodScope = new Scope(this.globalScope);
          for (const arg of methodInfo.defaultImpl.args) {
            bindPatternNames(arg, methodScope);
          }
          this.validateExpressionIdentifiers(methodInfo.defaultImpl.body, methodScope, protocolName, methodName);
        }
      }
    }
  }
  computeModuleExports() {
    const moduleDecl = this.getModule();
    const {
      values,
      operators,
      adts,
      typeAliases,
      opaqueTypes,
      records,
      protocols,
      importedValues
    } = this;
    const exports = {
      values: new Set,
      operators: new Set,
      types: new Map,
      protocols: new Map,
      exportsAll: false,
      reExportedValues: new Map
    };
    if (!moduleDecl.exposing) {
      return exports;
    }
    const exposing = moduleDecl.exposing;
    if (exposing.kind === "All") {
      exports.exportsAll = true;
      for (const name of Object.keys(values)) {
        exports.values.add(name);
      }
      for (const op of operators.keys()) {
        exports.operators.add(op);
      }
      for (const [name, adt] of Object.entries(adts)) {
        exports.types.set(name, {
          allConstructors: true,
          constructors: new Set(adt.constructors)
        });
      }
      for (const name of Object.keys(typeAliases)) {
        exports.types.set(name, { allConstructors: false });
      }
      for (const name of Object.keys(opaqueTypes)) {
        exports.types.set(name, { allConstructors: false });
      }
      for (const name of Object.keys(records)) {
        exports.types.set(name, { allConstructors: false });
      }
      for (const [name, protocol] of Object.entries(protocols)) {
        exports.protocols.set(name, {
          allMethods: true,
          methods: new Set(protocol.methods.keys())
        });
      }
      return exports;
    }
    for (const spec of exposing.exports) {
      switch (spec.kind) {
        case "ExportValue": {
          const name = spec.name;
          if (Object.hasOwn(values, name)) {
            exports.values.add(name);
            continue;
          }
          if (importedValues.has(name)) {
            exports.reExportedValues.set(name, importedValues.get(name));
            continue;
          }
          if (Object.hasOwn(adts, name)) {
            exports.types.set(name, {
              allConstructors: false,
              constructors: new Set
            });
            continue;
          }
          if (Object.hasOwn(typeAliases, name)) {
            exports.types.set(name, { allConstructors: false });
            continue;
          }
          if (Object.hasOwn(opaqueTypes, name)) {
            exports.types.set(name, { allConstructors: false });
            continue;
          }
          if (Object.hasOwn(records, name)) {
            exports.types.set(name, { allConstructors: false });
            continue;
          }
          if (Object.hasOwn(protocols, name)) {
            exports.protocols.set(name, {
              allMethods: false,
              methods: new Set
            });
            continue;
          }
          throw new SemanticError(`Module exposes '${name}' which is not defined`, spec.span, this.getFilePath());
        }
        case "ExportOperator": {
          const op = spec.operator;
          if (!Object.hasOwn(values, op) && !operators.has(op) && !importedValues.has(op)) {
            throw new SemanticError(`Module exposes operator '${op}' which is not defined`, spec.span, this.getFilePath());
          }
          exports.operators.add(op);
          if (Object.hasOwn(values, op)) {
            exports.values.add(op);
          } else if (importedValues.has(op)) {
            exports.reExportedValues.set(op, importedValues.get(op));
          }
          break;
        }
        case "ExportTypeAll": {
          const name = spec.name;
          if (Object.hasOwn(adts, name)) {
            const adt = adts[name];
            exports.types.set(name, {
              allConstructors: true,
              constructors: new Set(adt.constructors)
            });
            continue;
          }
          if (Object.hasOwn(protocols, name)) {
            const protocol = protocols[name];
            exports.protocols.set(name, {
              allMethods: true,
              methods: new Set(protocol.methods.keys())
            });
            continue;
          }
          if (Object.hasOwn(typeAliases, name)) {
            throw new SemanticError(`Type alias '${name}' cannot use (..) syntax - type aliases have no constructors`, spec.span, this.getFilePath());
          }
          if (Object.hasOwn(opaqueTypes, name)) {
            throw new SemanticError(`Opaque type '${name}' cannot use (..) syntax - opaque types have no constructors`, spec.span, this.getFilePath());
          }
          if (Object.hasOwn(records, name)) {
            throw new SemanticError(`Record type '${name}' cannot use (..) syntax - records have no constructors. Use '${name}' instead`, spec.span, this.getFilePath());
          }
          throw new SemanticError(`Module exposes '${name}(..)' but '${name}' is not a type or protocol`, spec.span, this.getFilePath());
        }
        case "ExportTypeSome": {
          const name = spec.name;
          const members = spec.members;
          if (Object.hasOwn(adts, name)) {
            const adt = adts[name];
            const exportedCtors = new Set;
            for (const memberName of members) {
              if (!adt.constructors.includes(memberName)) {
                throw new SemanticError(`Constructor '${memberName}' is not defined in type '${name}'`, spec.span, this.getFilePath());
              }
              exportedCtors.add(memberName);
            }
            exports.types.set(name, {
              allConstructors: false,
              constructors: exportedCtors
            });
            continue;
          }
          if (Object.hasOwn(protocols, name)) {
            const protocol = protocols[name];
            const exportedMethods = new Set;
            for (const memberName of members) {
              if (!protocol.methods.has(memberName)) {
                throw new SemanticError(`Method '${memberName}' is not defined in protocol '${name}'`, spec.span, this.getFilePath());
              }
              exportedMethods.add(memberName);
              exports.values.add(memberName);
            }
            exports.protocols.set(name, {
              allMethods: false,
              methods: exportedMethods
            });
            continue;
          }
          throw new SemanticError(`Module exposes '${name}(...)' but '${name}' is not a type or protocol`, spec.span, this.getFilePath());
        }
      }
    }
    return exports;
  }
  buildSemanticModule() {
    const exports = this.computeModuleExports();
    for (const [name] of exports.reExportedValues) {
      const scheme = this.globalScope.lookup(name);
      if (scheme && !this.typeSchemes[name]) {
        this.typeSchemes[name] = scheme;
      }
    }
    return {
      values: this.values,
      annotations: this.annotations,
      module: this.program.module,
      imports: this.imports,
      types: this.types,
      typeSchemes: this.typeSchemes,
      adts: this.adts,
      constructors: this.constructors,
      constructorTypes: this.constructorTypes,
      typeAliases: this.typeAliases,
      records: this.records,
      opaqueTypes: this.opaqueTypes,
      protocols: this.protocols,
      instances: this.instances,
      operators: this.operators,
      infixDeclarations: this.infixDeclarations,
      exports,
      errors: this.getErrors(),
      importedValues: this.importedValues,
      protocolMethodUsages: this._resolvedProtocolUsages
    };
  }
  concretizeInstanceTypeArgs() {
    for (const instance of this.localInstances) {
      const protocol = this.protocols[instance.protocolName];
      if (!protocol)
        continue;
      const hasTypeVars = instance.typeArgs.some((t) => t.kind === "var");
      if (!hasTypeVars)
        continue;
      for (const methodName of instance.explicitMethods) {
        const methodExpr = instance.methods.get(methodName);
        const protocolMethodInfo = protocol.methods.get(methodName);
        if (!methodExpr || !protocolMethodInfo)
          continue;
        const expectedType = substituteTypeParams(protocolMethodInfo.type, protocol.params, instance.typeArgs);
        const inferSubstitution = new Map(this._substitution);
        const tempScope = this.createChildScope();
        try {
          const inferredType = this.analyzeExpr(methodExpr, {
            scope: tempScope,
            substitution: inferSubstitution
          });
          this.unify(inferredType, expectedType, methodExpr.span, inferSubstitution);
          for (let i = 0;i < instance.typeArgs.length; i++) {
            const typeArg = instance.typeArgs[i];
            const resolved = applySubstitution(typeArg, inferSubstitution);
            if (resolved.kind !== "var" && typeArg.kind === "var") {
              instance.typeArgs[i] = resolved;
            }
          }
          const constraintsToRemove = [];
          for (const constraint of instance.constraints) {
            let allConcrete = true;
            for (let i = 0;i < constraint.typeArgs.length; i++) {
              const typeArg = constraint.typeArgs[i];
              const resolved = applySubstitution(typeArg, inferSubstitution);
              if (resolved.kind !== "var" && typeArg.kind === "var") {
                constraint.typeArgs[i] = resolved;
              }
              if (constraint.typeArgs[i].kind === "var") {
                allConcrete = false;
              }
            }
            if (allConcrete) {
              const isSatisfied = findInstanceForConstraint(constraint.protocolName, constraint.typeArgs, this.instances);
              if (isSatisfied) {
                constraintsToRemove.push(constraint);
              }
            }
          }
          for (const toRemove of constraintsToRemove) {
            const idx = instance.constraints.indexOf(toRemove);
            if (idx !== -1) {
              instance.constraints.splice(idx, 1);
            }
          }
        } catch {}
      }
    }
  }
  validateImplementationMethodTypes() {
    for (const instance of this.localInstances) {
      const protocol = this.protocols[instance.protocolName];
      if (!protocol)
        continue;
      const paramSubstitution = new Map;
      const paramNameToId = new Map;
      for (let i = 0;i < protocol.params.length; i++) {
        const paramName = protocol.params[i];
        const typeArg = instance.typeArgs[i];
        if (typeArg) {
          const paramVar = freshType();
          paramNameToId.set(paramName, paramVar.id);
          paramSubstitution.set(paramVar.id, typeArg);
        }
      }
      for (const methodName of instance.explicitMethods) {
        const methodExpr = instance.methods.get(methodName);
        const protocolMethodInfo = protocol.methods.get(methodName);
        if (!methodExpr || !protocolMethodInfo)
          continue;
        const expectedType = substituteTypeParams(protocolMethodInfo.type, protocol.params, instance.typeArgs);
        const inferSubstitution = new Map(this._substitution);
        const tempScope = this.createChildScope();
        let inferredType;
        if (methodExpr.kind === "Lambda" && expectedType.kind === "fun") {
          const expectedParamTypes = [];
          let currentType = expectedType;
          while (currentType.kind === "fun" && expectedParamTypes.length < methodExpr.args.length) {
            expectedParamTypes.push(currentType.from);
            currentType = currentType.to;
          }
          this.bindPatterns(tempScope, methodExpr.args, expectedParamTypes, inferSubstitution);
          const bodyType = this.analyzeExpr(methodExpr.body, {
            scope: tempScope,
            substitution: inferSubstitution
          });
          inferredType = fnChain(expectedParamTypes, bodyType);
        } else {
          inferredType = this.analyzeExpr(methodExpr, {
            scope: tempScope,
            substitution: inferSubstitution,
            expectedType
          });
        }
        try {
          this.unify(inferredType, expectedType, methodExpr.span, inferSubstitution);
        } catch (e) {
          if (e instanceof SemanticError) {
            throw new SemanticError(`Implementation of '${methodName}' for '${instance.protocolName}' has type '${formatType(applySubstitution(inferredType, inferSubstitution))}' but protocol expects '${formatType(expectedType)}'`, methodExpr.span, this.getFilePath());
          }
          throw e;
        }
        for (const constraint of instance.constraints) {
          for (const constraintTypeArg of constraint.typeArgs) {
            const resolvedType = applySubstitution(constraintTypeArg, inferSubstitution);
            if (resolvedType.kind === "con") {
              const hasInstance = findInstanceForTypeInternal(constraint.protocolName, resolvedType, this.instances);
              if (!hasInstance) {
                throw new SemanticError(`Implementation of '${methodName}' for '${instance.protocolName}' ` + `requires '${constraint.protocolName}' constraint on type parameter, ` + `but the implementation uses type '${formatType(resolvedType)}' ` + `which does not implement '${constraint.protocolName}'`, methodExpr.span, this.getFilePath());
              }
            }
          }
        }
        for (let i = 0;i < instance.typeArgs.length; i++) {
          const typeArg = instance.typeArgs[i];
          const resolved = applySubstitution(typeArg, inferSubstitution);
          if (resolved.kind !== "var" && typeArg.kind === "var") {
            instance.typeArgs[i] = resolved;
          }
        }
        for (const constraint of instance.constraints) {
          for (let i = 0;i < constraint.typeArgs.length; i++) {
            const typeArg = constraint.typeArgs[i];
            const resolved = applySubstitution(typeArg, inferSubstitution);
            if (resolved.kind !== "var" && typeArg.kind === "var") {
              constraint.typeArgs[i] = resolved;
            }
          }
        }
      }
    }
  }
  validateConcreteConstraintInstances() {
    for (const [valueName, valueInfo] of Object.entries(this.values)) {
      if (valueName.startsWith("$"))
        continue;
      if (valueInfo.collectedConstraints) {
        for (const constraint of valueInfo.collectedConstraints) {
          const resolvedTypeArgs = constraint.typeArgs.map((t) => applySubstitution(t, this._substitution));
          for (const typeArg of resolvedTypeArgs) {
            if (typeArg.kind === "con") {
              const hasInstance = findInstanceForTypeInternal(constraint.protocolName, typeArg, this.instances);
              if (!hasInstance) {
                const span = valueInfo.span ?? valueInfo.declaration.span;
                throw new SemanticError(`No instance of '${constraint.protocolName}' for type '${formatType(typeArg)}'. ` + `Add an implementation: implement ${constraint.protocolName} ${formatType(typeArg)} where ...`, span, this.getFilePath());
              }
            }
          }
        }
      }
    }
  }
  validateConstraintsEagerly(substitution, span) {
    const constraints = this.getCollectedConstraints();
    if (constraints.length === 0)
      return;
    for (const c of constraints) {
      const resolvedTypeArgs = c.typeArgs.map((t) => applySubstitution(t, substitution));
      const hasConcreteNonVarArg = resolvedTypeArgs.some((t) => {
        if (t.kind === "var")
          return false;
        if (t.kind === "fun")
          return true;
        if (t.kind === "tuple")
          return true;
        if (t.kind === "record")
          return true;
        if (t.kind === "con") {
          return false;
        }
        return false;
      });
      if (!hasConcreteNonVarArg)
        continue;
      const resolvedConstraint = {
        protocolName: c.protocolName,
        typeArgs: resolvedTypeArgs
      };
      const satisfiability = checkConstraintSatisfiability(resolvedConstraint, this.instances);
      if (!satisfiability.possible) {
        for (const typeArg of resolvedTypeArgs) {
          if (typeArg.kind === "fun") {
            throw new SemanticError(`Type mismatch: cannot unify '${formatType(typeArg.from)}' with '${formatType(typeArg)}'`, span, this.getFilePath());
          }
        }
        const typeArgsStr = resolvedTypeArgs.map((t) => formatType(t)).join(", ");
        throw new SemanticError(`Type mismatch: expression cannot be used as a function (constraint '${c.protocolName}' on '${typeArgsStr}' cannot be satisfied)`, span, this.getFilePath());
      }
    }
  }
  checkTypeCollision(name, importingFrom, existing, span, kind) {
    if (!existing)
      return;
    if (kind === "protocol") {
      return;
    }
    if (existing.moduleName !== undefined && existing.moduleName !== importingFrom) {
      throw new SemanticError(`${kind === "type" ? "Type" : kind === "type alias" ? "Type alias" : kind === "record type" ? "Record type" : "Protocol"} '${name}' conflicts with ${kind} from module '${existing.moduleName}'. ` + `Consider using qualified imports or aliasing one of them.`, span, this.getFilePath());
    }
  }
  declareSymbol(scope, name, scheme, span) {
    if (scope.has(name)) {
      throw new SemanticError(`Duplicate definition for '${name}'`, span, this.getFilePath());
    }
    scope.define(name, scheme);
  }
  lookupSymbolWithConstraints(scope, name, span, substitution) {
    const sub = substitution ?? this._substitution;
    const scheme = scope.lookup(name);
    if (scheme) {
      const { type, constraints } = instantiateWithConstraints(scheme, sub);
      return { type, constraints };
    }
    this.addError(`Undefined name '${name}'`, span);
    return { type: ERROR_TYPE, constraints: [] };
  }
  lookupSymbol(scope, name, span, substitution) {
    return this.lookupSymbolWithConstraints(scope, name, span, substitution).type;
  }
  generalize(type, scope, substitution) {
    const sub = substitution ?? this._substitution;
    const typeFreeVars = getFreeTypeVars(type, sub);
    const scopeFreeVars = getFreeTypeVarsInScope(scope, sub);
    const quantified = new Set;
    for (const v of typeFreeVars) {
      if (!scopeFreeVars.has(v)) {
        quantified.add(v);
      }
    }
    const rawConstraints = this.getCollectedConstraints();
    const resolvedConstraints = applySubstitutionToConstraints(rawConstraints, sub);
    const relevantConstraints = resolvedConstraints.filter((c) => {
      return c.typeArgs.some((t) => {
        const freeVars = getFreeTypeVars(t, sub);
        for (const v of freeVars) {
          if (quantified.has(v))
            return true;
        }
        return false;
      });
    });
    const uniqueConstraints = [];
    for (const c of relevantConstraints) {
      const isDuplicate = uniqueConstraints.some((uc) => uc.protocolName === c.protocolName && uc.typeArgs.length === c.typeArgs.length && uc.typeArgs.every((t, i) => typesEqual(t, c.typeArgs[i])));
      if (!isDuplicate) {
        uniqueConstraints.push(c);
      }
    }
    return { vars: quantified, constraints: uniqueConstraints, type };
  }
}
function collectFreeVars(expr, bound = new Set) {
  const free = new Set;
  const worklist = [[expr, bound]];
  while (worklist.length > 0) {
    const [e, localBound] = worklist.pop();
    switch (e.kind) {
      case "Var":
        if (!localBound.has(e.name)) {
          free.add(e.name);
        }
        break;
      case "Number":
      case "String":
      case "Char":
      case "Unit":
        break;
      case "Paren":
        worklist.push([e.expression, localBound]);
        break;
      case "Tuple":
        for (const el of e.elements)
          worklist.push([el, localBound]);
        break;
      case "List":
        for (const el of e.elements)
          worklist.push([el, localBound]);
        break;
      case "ListRange":
        worklist.push([e.start, localBound]);
        worklist.push([e.end, localBound]);
        break;
      case "Record":
        for (const f of e.fields)
          worklist.push([f.value, localBound]);
        break;
      case "RecordUpdate":
        if (!localBound.has(e.base)) {
          free.add(e.base);
        }
        for (const f of e.fields)
          worklist.push([f.value, localBound]);
        break;
      case "FieldAccess":
        worklist.push([e.target, localBound]);
        break;
      case "Apply":
        worklist.push([e.callee, localBound]);
        for (const arg of e.args)
          worklist.push([arg, localBound]);
        break;
      case "Infix":
        worklist.push([e.left, localBound]);
        worklist.push([e.right, localBound]);
        if (!localBound.has(e.operator)) {
          free.add(e.operator);
        }
        break;
      case "Unary":
        worklist.push([e.operand, localBound]);
        break;
      case "Lambda": {
        const lambdaBound = new Set(localBound);
        for (const p of e.args)
          collectPatternVars(p, lambdaBound);
        worklist.push([e.body, lambdaBound]);
        break;
      }
      case "LetIn": {
        const letBound = new Set(localBound);
        for (const binding of e.bindings) {
          letBound.add(binding.name);
          for (const p of binding.args)
            collectPatternVars(p, letBound);
        }
        for (const binding of e.bindings) {
          worklist.push([binding.body, letBound]);
        }
        worklist.push([e.body, letBound]);
        break;
      }
      case "If":
        worklist.push([e.condition, localBound]);
        worklist.push([e.thenBranch, localBound]);
        worklist.push([e.elseBranch, localBound]);
        break;
      case "Case":
        worklist.push([e.discriminant, localBound]);
        for (const branch of e.branches) {
          const branchBound = new Set(localBound);
          collectPatternVars(branch.pattern, branchBound);
          worklist.push([branch.body, branchBound]);
        }
        break;
    }
  }
  return free;
}
function collectPatternVars(pattern, bound) {
  switch (pattern.kind) {
    case "VarPattern":
      bound.add(pattern.name);
      break;
    case "WildcardPattern":
      break;
    case "ConstructorPattern":
      pattern.args.forEach((p) => collectPatternVars(p, bound));
      break;
    case "TuplePattern":
      pattern.elements.forEach((p) => collectPatternVars(p, bound));
      break;
    case "ListPattern":
      pattern.elements.forEach((p) => collectPatternVars(p, bound));
      break;
    case "ConsPattern":
      collectPatternVars(pattern.head, bound);
      collectPatternVars(pattern.tail, bound);
      break;
  }
}
function buildDependencyGraph(values) {
  const graph = new Map;
  const valueNames = new Set(Object.keys(values));
  for (const [name, info] of Object.entries(values)) {
    const deps = new Set;
    if (info.declaration.kind === "ValueDeclaration") {
      const bound = new Set;
      info.declaration.args.forEach((p) => collectPatternVars(p, bound));
      const freeVars = collectFreeVars(info.declaration.body, bound);
      for (const v of freeVars) {
        if (valueNames.has(v) && v !== name) {
          deps.add(v);
        }
      }
    }
    graph.set(name, deps);
  }
  return graph;
}
function computeSCCs(graph) {
  const index = new Map;
  const lowlink = new Map;
  const onStack = new Set;
  const stack = [];
  const sccs = [];
  let currentIndex = 0;
  function strongConnect(v) {
    index.set(v, currentIndex);
    lowlink.set(v, currentIndex);
    currentIndex++;
    stack.push(v);
    onStack.add(v);
    const deps = graph.get(v) ?? new Set;
    for (const w of deps) {
      if (!index.has(w)) {
        strongConnect(w);
        lowlink.set(v, Math.min(lowlink.get(v), lowlink.get(w)));
      } else if (onStack.has(w)) {
        lowlink.set(v, Math.min(lowlink.get(v), index.get(w)));
      }
    }
    if (lowlink.get(v) === index.get(v)) {
      const scc = [];
      let w;
      do {
        w = stack.pop();
        onStack.delete(w);
        scc.push(w);
      } while (w !== v);
      sccs.push(scc);
    }
  }
  for (const v of graph.keys()) {
    if (!index.has(v)) {
      strongConnect(v);
    }
  }
  return sccs;
}
function isExportedFromModule(depModule, itemName, itemKind) {
  const exports = depModule.exports;
  if (exports.exportsAll) {
    return true;
  }
  switch (itemKind) {
    case "value":
      return exports.values.has(itemName);
    case "operator":
      return exports.operators.has(itemName) || exports.values.has(itemName);
    case "type":
      return exports.types.has(itemName);
    case "constructor": {
      for (const [, typeExport] of exports.types) {
        if (typeExport.constructors?.has(itemName)) {
          return true;
        }
      }
      return false;
    }
    case "protocol":
      return exports.protocols.has(itemName);
    case "method": {
      for (const [, protocolExport] of exports.protocols) {
        if (protocolExport.methods?.has(itemName)) {
          return true;
        }
      }
      return false;
    }
  }
}
function analyze(program, options) {
  return new SemanticAnalyzer(program, options).analyze();
}
function containsRecordType(expr) {
  switch (expr.kind) {
    case "RecordType":
      return true;
    case "TypeRef":
      return expr.args.some(containsRecordType);
    case "FunctionType":
      return containsRecordType(expr.from) || containsRecordType(expr.to);
    case "TupleType":
      return expr.elements.some(containsRecordType);
    case "QualifiedType":
      return containsRecordType(expr.type);
  }
}
function collectTypeVariables(expr) {
  const vars = new Set;
  function collect(e) {
    switch (e.kind) {
      case "TypeRef": {
        const name = e.name;
        if (name.length > 0 && name[0] === name[0].toLowerCase()) {
          vars.add(name);
        }
        for (const arg of e.args) {
          collect(arg);
        }
        break;
      }
      case "FunctionType":
        collect(e.from);
        collect(e.to);
        break;
      case "TupleType":
        for (const el of e.elements) {
          collect(el);
        }
        break;
      case "RecordType":
        for (const field2 of e.fields) {
          collect(field2.type);
        }
        break;
      case "QualifiedType":
        collect(e.type);
        for (const constraint of e.constraints) {
          for (const arg of constraint.typeArgs) {
            collect(arg);
          }
        }
        break;
    }
  }
  collect(expr);
  return vars;
}
function validateTypeExpr(analyzer, expr, definedParams, parentSpan) {
  const { adts, typeAliases, opaqueTypes, records, imports, dependencies } = analyzer;
  const errors = [];
  function resolve(name) {
    return resolveQualifiedType(name, adts, typeAliases, opaqueTypes, records, imports, dependencies);
  }
  function findCaseSuggestion(name) {
    const nameLower = name.toLowerCase();
    for (const adtName of Object.keys(adts)) {
      if (adtName.toLowerCase() === nameLower && adtName !== name) {
        return adtName;
      }
    }
    for (const aliasName of Object.keys(typeAliases)) {
      if (aliasName.toLowerCase() === nameLower && aliasName !== name) {
        return aliasName;
      }
    }
    for (const opaqueName of Object.keys(opaqueTypes)) {
      if (opaqueName.toLowerCase() === nameLower && opaqueName !== name) {
        return opaqueName;
      }
    }
    for (const recordName of Object.keys(records)) {
      if (recordName.toLowerCase() === nameLower && recordName !== name) {
        return recordName;
      }
    }
    return;
  }
  function validate(e) {
    switch (e.kind) {
      case "TypeRef": {
        const name = e.name;
        const isLowercase = name.charAt(0) === name.charAt(0).toLowerCase();
        if (definedParams.has(name)) {
          if (e.args.length > 0) {
            errors.push({
              message: `Type parameter '${name}' cannot take type arguments`,
              span: e.span ?? parentSpan
            });
          }
          return;
        }
        const adtByName = adts[name];
        if (adtByName) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        if (name === "List") {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        if (typeAliases[name]) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        if (opaqueTypes[name]) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        if (records[name]) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        const resolved = resolve(name);
        if (resolved) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        const suggestion = findCaseSuggestion(name);
        if (isLowercase && name.length === 1) {
          errors.push({
            message: `Type variable '${name}' is not defined in this context`,
            span: e.span ?? parentSpan,
            suggestion: `Add '${name}' as a type parameter to the type alias`
          });
          return;
        }
        if (suggestion) {
          errors.push({
            message: `Type '${name}' is not defined`,
            span: e.span ?? parentSpan,
            suggestion: `Did you mean '${suggestion}'?`
          });
        } else {
          errors.push({
            message: `Type '${name}' is not defined`,
            span: e.span ?? parentSpan
          });
        }
        for (const arg of e.args) {
          validate(arg);
        }
        break;
      }
      case "FunctionType": {
        validate(e.from);
        validate(e.to);
        break;
      }
      case "TupleType": {
        for (const el of e.elements) {
          validate(el);
        }
        break;
      }
      case "RecordType": {
        for (const field2 of e.fields) {
          validate(field2.type);
        }
        break;
      }
      case "QualifiedType": {
        validate(e.type);
        break;
      }
    }
  }
  validate(expr);
  return errors;
}
function addProtocolMethodsToScope(protocol, scope) {
  const sharedTypeVarCtx = new Map;
  for (const param of protocol.params) {
    sharedTypeVarCtx.set(param, freshType());
  }
  const protocolConstraint = {
    protocolName: protocol.name,
    typeArgs: protocol.params.map((p) => sharedTypeVarCtx.get(p))
  };
  const quantifiedVars = new Set;
  for (const tv of sharedTypeVarCtx.values()) {
    quantifiedVars.add(tv.id);
  }
  const paramNames = invertContext(sharedTypeVarCtx);
  const methodSchemes = new Map;
  for (const [methodName, methodInfo] of protocol.methods) {
    if (!scope.symbols.has(methodName)) {
      const refreshedType = refreshType(methodInfo.type, sharedTypeVarCtx);
      const scheme = {
        vars: new Set(quantifiedVars),
        constraints: [protocolConstraint],
        type: refreshedType,
        paramNames
      };
      scope.symbols.set(methodName, scheme);
      methodSchemes.set(methodName, scheme);
    }
  }
  return methodSchemes;
}
function refreshType(type, newVarMap) {
  const oldVarIds = collectTypeVarIds(type);
  const newVars = Array.from(newVarMap.values());
  const varSubst = new Map;
  const oldVarArray = Array.from(oldVarIds);
  for (let i = 0;i < Math.min(oldVarArray.length, newVars.length); i++) {
    varSubst.set(oldVarArray[i], newVars[i]);
  }
  return applyVarSubstitution(type, varSubst);
}
function substituteProtocolVars(type, protocolVarCtx) {
  return type;
}
function findInstanceForConstraint(protocolName, typeArgs, instances) {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    if (inst.typeArgs.length !== typeArgs.length)
      continue;
    let allMatch = true;
    for (let i = 0;i < typeArgs.length; i++) {
      if (!instanceTypeMatches(inst.typeArgs[i], typeArgs[i])) {
        allMatch = false;
        break;
      }
    }
    if (allMatch)
      return true;
  }
  if (typeArgs.length > 0) {
    if (trySynthesizeInstance(protocolName, typeArgs[0], instances)) {
      return true;
    }
  }
  return false;
}
function substituteTypeParams(type, params, typeArgs) {
  const varIds = [];
  collectTypeVarIdsOrdered(type, varIds, new Set);
  const substitution = new Map;
  for (let i = 0;i < Math.min(varIds.length, typeArgs.length); i++) {
    substitution.set(varIds[i], typeArgs[i]);
  }
  return applyTypeSubstitution(type, substitution);
}
function instanceTypeMatches(instType, concreteType) {
  if (instType.kind === "var") {
    return true;
  }
  if (instType.kind !== concreteType.kind) {
    return false;
  }
  if (instType.kind === "con" && concreteType.kind === "con") {
    if (instType.name !== concreteType.name) {
      return false;
    }
    if (instType.args.length !== concreteType.args.length) {
      return false;
    }
    for (let i = 0;i < instType.args.length; i++) {
      if (!instanceTypeMatches(instType.args[i], concreteType.args[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "fun" && concreteType.kind === "fun") {
    return instanceTypeMatches(instType.from, concreteType.from) && instanceTypeMatches(instType.to, concreteType.to);
  }
  if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    if (instType.elements.length !== concreteType.elements.length) {
      return false;
    }
    for (let i = 0;i < instType.elements.length; i++) {
      if (!instanceTypeMatches(instType.elements[i], concreteType.elements[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "record" && concreteType.kind === "record") {
    const instKeys = Object.keys(instType.fields);
    const concreteKeys = Object.keys(concreteType.fields);
    if (instKeys.length !== concreteKeys.length) {
      return false;
    }
    for (const key of instKeys) {
      if (!(key in concreteType.fields)) {
        return false;
      }
      if (!instanceTypeMatches(instType.fields[key], concreteType.fields[key])) {
        return false;
      }
    }
    return true;
  }
  return typesEqual(instType, concreteType);
}
function checkConstraintSatisfiability(constraint, instances) {
  const hasFreeVars = constraint.typeArgs.some((t) => {
    const freeVars = getFreeTypeVars(t, new Map);
    return freeVars.size > 0;
  });
  if (!hasFreeVars) {
    const result = validateConstraintSatisfiable(constraint, instances);
    return { possible: result.found };
  }
  for (const typeArg of constraint.typeArgs) {
    const shape = getTypeShape(typeArg);
    if (shape === "fun") {
      const hasFunctionInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName)
          return false;
        return inst.typeArgs.some((t) => t.kind === "fun" || t.kind === "var");
      });
      if (!hasFunctionInstance) {
        return { possible: false };
      }
    }
    if (shape === "tuple") {
      if (constraint.protocolName === "Eq" || constraint.protocolName === "Show") {
        return { possible: true };
      }
      const hasTupleInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName)
          return false;
        return inst.typeArgs.some((t) => t.kind === "tuple" || t.kind === "var");
      });
      if (!hasTupleInstance) {
        return { possible: false };
      }
    }
    if (shape === "record") {
      if (constraint.protocolName === "Eq" || constraint.protocolName === "Show") {
        return { possible: true };
      }
      const hasRecordInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName)
          return false;
        return inst.typeArgs.some((t) => t.kind === "record" || t.kind === "var");
      });
      if (!hasRecordInstance) {
        return { possible: false };
      }
    }
    if (shape === "con" && typeArg.kind === "con") {
      const hasMatchingInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName)
          return false;
        return inst.typeArgs.some((t) => t.kind === "var" || t.kind === "con" && t.name === typeArg.name);
      });
      if (!hasMatchingInstance) {
        return { possible: false };
      }
    }
  }
  return { possible: true };
}
function getTypeShape(type) {
  switch (type.kind) {
    case "var":
      return "var";
    case "con":
      return "con";
    case "fun":
      return "fun";
    case "tuple":
      return "tuple";
    case "record":
      return "record";
    default:
      return "con";
  }
}
function validateConstraintSatisfiable(constraint, instances) {
  let firstUnsatisfiedConstraint = null;
  for (const inst of instances) {
    if (inst.protocolName !== constraint.protocolName)
      continue;
    if (inst.typeArgs.length !== constraint.typeArgs.length)
      continue;
    const instSubstitution = new Map;
    let allArgsMatch = true;
    for (let i = 0;i < inst.typeArgs.length; i++) {
      const instArg = inst.typeArgs[i];
      const constraintArg = constraint.typeArgs[i];
      if (!matchTypeArgForInstance(instArg, constraintArg, instSubstitution)) {
        allArgsMatch = false;
        break;
      }
    }
    if (!allArgsMatch)
      continue;
    if (inst.constraints.length === 0) {
      return { found: true };
    }
    let allConstraintsSatisfied = true;
    for (const instConstraint of inst.constraints) {
      const substitutedTypeArgs = instConstraint.typeArgs.map((t) => applySubstitution(t, instSubstitution));
      const substitutedConstraint = {
        protocolName: instConstraint.protocolName,
        typeArgs: substitutedTypeArgs
      };
      const canBeSatisfied = checkConstraintSatisfiability(substitutedConstraint, instances);
      if (!canBeSatisfied.possible) {
        allConstraintsSatisfied = false;
        if (!firstUnsatisfiedConstraint) {
          firstUnsatisfiedConstraint = {
            constraint: instConstraint.protocolName,
            forType: substitutedTypeArgs.map((t) => formatType(t)).join(", ")
          };
        }
        break;
      }
    }
    if (allConstraintsSatisfied) {
      return { found: true };
    }
  }
  if (firstUnsatisfiedConstraint) {
    return {
      found: false,
      reason: "unsatisfied-constraint",
      constraint: firstUnsatisfiedConstraint.constraint,
      forType: firstUnsatisfiedConstraint.forType
    };
  }
  if (constraint.typeArgs.length === 1) {
    if (trySynthesizeInstance(constraint.protocolName, constraint.typeArgs[0], instances)) {
      return { found: true };
    }
  }
  return { found: false, reason: "no-instance" };
}
function matchTypeArgForInstance(instArg, constraintArg, substitution) {
  if (instArg.kind === "var") {
    const existing = substitution.get(instArg.id);
    if (existing) {
      return typesEqual(existing, constraintArg);
    }
    substitution.set(instArg.id, constraintArg);
    return true;
  }
  if (constraintArg.kind === "var") {
    return true;
  }
  if (instArg.kind !== constraintArg.kind)
    return false;
  if (instArg.kind === "con" && constraintArg.kind === "con") {
    if (instArg.name !== constraintArg.name)
      return false;
    if (instArg.args.length !== constraintArg.args.length)
      return false;
    for (let i = 0;i < instArg.args.length; i++) {
      if (!matchTypeArgForInstance(instArg.args[i], constraintArg.args[i], substitution)) {
        return false;
      }
    }
    return true;
  }
  if (instArg.kind === "fun" && constraintArg.kind === "fun") {
    return matchTypeArgForInstance(instArg.from, constraintArg.from, substitution) && matchTypeArgForInstance(instArg.to, constraintArg.to, substitution);
  }
  if (instArg.kind === "tuple" && constraintArg.kind === "tuple") {
    if (instArg.elements.length !== constraintArg.elements.length)
      return false;
    for (let i = 0;i < instArg.elements.length; i++) {
      if (!matchTypeArgForInstance(instArg.elements[i], constraintArg.elements[i], substitution)) {
        return false;
      }
    }
    return true;
  }
  if (instArg.kind === "record" && constraintArg.kind === "record") {
    const instKeys = Object.keys(instArg.fields).sort();
    const constraintKeys = Object.keys(constraintArg.fields).sort();
    if (instKeys.length !== constraintKeys.length)
      return false;
    for (let i = 0;i < instKeys.length; i++) {
      const key = instKeys[i];
      if (key !== constraintKeys[i])
        return false;
      if (!matchTypeArgForInstance(instArg.fields[key], constraintArg.fields[key], substitution)) {
        return false;
      }
    }
    return true;
  }
  return typesEqual(instArg, constraintArg);
}
function findInstanceForTypeWithReason(protocolName, concreteType, instances) {
  let hasPolymorphicInstance = false;
  let firstUnsatisfiedConstraint = null;
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    const instTypeArg = inst.typeArgs[0];
    if (!instTypeArg)
      continue;
    let matchesStructure = false;
    if (instTypeArg.kind === concreteType.kind) {
      if (instTypeArg.kind === "con" && concreteType.kind === "con") {
        matchesStructure = instTypeArg.name === concreteType.name && instTypeArg.args.length === concreteType.args.length;
      } else if (instTypeArg.kind === "tuple" && concreteType.kind === "tuple") {
        matchesStructure = instTypeArg.elements.length === concreteType.elements.length;
      } else if (instTypeArg.kind === "record" && concreteType.kind === "record") {
        const k1 = Object.keys(instTypeArg.fields).sort();
        const k2 = Object.keys(concreteType.fields).sort();
        matchesStructure = k1.length === k2.length && k1.every((k, i) => k === k2[i]);
      } else {
        matchesStructure = true;
      }
    }
    if (matchesStructure) {
      if (instanceTypeMatches(instTypeArg, concreteType)) {
        if (inst.constraints.length === 0) {
          return { found: true };
        }
        const typeVarSubst = new Map;
        buildTypeVarSubstitution(instTypeArg, concreteType, typeVarSubst);
        let allConstraintsSatisfied = true;
        for (const constraint of inst.constraints) {
          const substitutedTypeArgs = constraint.typeArgs.map((t) => applySubstitutionToType(t, typeVarSubst));
          for (const typeArg of substitutedTypeArgs) {
            const constraintResult = findInstanceForTypeWithReason(constraint.protocolName, typeArg, instances);
            if (!constraintResult.found) {
              allConstraintsSatisfied = false;
              hasPolymorphicInstance = true;
              if (!firstUnsatisfiedConstraint) {
                firstUnsatisfiedConstraint = {
                  constraint: constraint.protocolName,
                  forType: formatType(typeArg)
                };
              }
              break;
            }
          }
          if (!allConstraintsSatisfied)
            break;
        }
        if (allConstraintsSatisfied) {
          return { found: true };
        }
      }
    }
    if (instTypeArg.kind === "var") {
      hasPolymorphicInstance = true;
      if (inst.constraints.length === 0) {
        return { found: true };
      }
      let allConstraintsSatisfied = true;
      for (const constraint of inst.constraints) {
        const constraintResult = findInstanceForTypeWithReason(constraint.protocolName, concreteType, instances);
        if (!constraintResult.found) {
          allConstraintsSatisfied = false;
          if (!firstUnsatisfiedConstraint) {
            firstUnsatisfiedConstraint = {
              constraint: constraint.protocolName,
              forType: formatType(concreteType)
            };
          }
          break;
        }
      }
      if (allConstraintsSatisfied) {
        return { found: true };
      }
    }
  }
  if (trySynthesizeInstance(protocolName, concreteType, instances)) {
    return { found: true };
  }
  if (hasPolymorphicInstance && firstUnsatisfiedConstraint) {
    return {
      found: false,
      reason: "unsatisfied-constraint",
      constraint: firstUnsatisfiedConstraint.constraint,
      forType: firstUnsatisfiedConstraint.forType
    };
  }
  return { found: false, reason: "no-instance" };
}
function buildTypeVarSubstitution(instType, concreteType, subst) {
  if (instType.kind === "var") {
    subst.set(instType.id, concreteType);
    return;
  }
  if (instType.kind === "con" && concreteType.kind === "con") {
    for (let i = 0;i < instType.args.length; i++) {
      buildTypeVarSubstitution(instType.args[i], concreteType.args[i], subst);
    }
  } else if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    for (let i = 0;i < instType.elements.length; i++) {
      buildTypeVarSubstitution(instType.elements[i], concreteType.elements[i], subst);
    }
  } else if (instType.kind === "fun" && concreteType.kind === "fun") {
    buildTypeVarSubstitution(instType.from, concreteType.from, subst);
    buildTypeVarSubstitution(instType.to, concreteType.to, subst);
  } else if (instType.kind === "record" && concreteType.kind === "record") {
    for (const key of Object.keys(instType.fields)) {
      if (key in concreteType.fields) {
        buildTypeVarSubstitution(instType.fields[key], concreteType.fields[key], subst);
      }
    }
  }
}
function applySubstitutionToType(type, subst) {
  if (type.kind === "var") {
    return subst.get(type.id) ?? type;
  }
  if (type.kind === "con") {
    return {
      ...type,
      args: type.args.map((a) => applySubstitutionToType(a, subst))
    };
  }
  if (type.kind === "tuple") {
    return {
      ...type,
      elements: type.elements.map((e) => applySubstitutionToType(e, subst))
    };
  }
  if (type.kind === "fun") {
    return {
      ...type,
      from: applySubstitutionToType(type.from, subst),
      to: applySubstitutionToType(type.to, subst)
    };
  }
  if (type.kind === "record") {
    const newFields = {};
    for (const [k, v] of Object.entries(type.fields)) {
      newFields[k] = applySubstitutionToType(v, subst);
    }
    return { ...type, fields: newFields };
  }
  return type;
}
function findInstanceForTypeInternal(protocolName, concreteType, instances) {
  return findInstanceForTypeWithReason(protocolName, concreteType, instances).found;
}
var syntheticSpan = {
  start: { offset: 0, line: 0, column: 0 },
  end: { offset: 0, line: 0, column: 0 }
};
var _recordRegistry = {};
var _protocolMethodUsages;
var _currentModuleName;
function getRecordFieldTypes(type) {
  if (type.kind === "record") {
    return type.fields;
  }
  if (type.kind === "con") {
    const info = _recordRegistry[type.name];
    if (info) {
      const fields = {};
      const resolveCtx = new Map;
      info.params.forEach((p, i) => {
        const v = type.args[i]?.kind === "var" ? type.args[i] : freshType();
        resolveCtx.set(p, v);
      });
      for (const field2 of info.fields) {
        fields[field2.name] = resolveTypeExprForRecord(field2.typeExpr, resolveCtx);
      }
      return fields;
    }
  }
  return;
}
function resolveTypeExprForRecord(typeExpr, context) {
  switch (typeExpr.kind) {
    case "TypeRef": {
      const tv = context.get(typeExpr.name);
      if (tv)
        return tv;
      return {
        kind: "con",
        name: typeExpr.name,
        args: typeExpr.args.map((a) => resolveTypeExprForRecord(a, context))
      };
    }
    case "FunctionType":
      return {
        kind: "fun",
        from: resolveTypeExprForRecord(typeExpr.from, context),
        to: resolveTypeExprForRecord(typeExpr.to, context)
      };
    case "TupleType":
      return {
        kind: "tuple",
        elements: typeExpr.elements.map((e) => resolveTypeExprForRecord(e, context))
      };
    default:
      return freshType();
  }
}
function trySynthesizeInstance(protocolName, type, instances) {
  if (protocolName !== "Eq" && protocolName !== "Show")
    return false;
  if (type.kind === "tuple") {
    for (const elem of type.elements) {
      if (!findInstanceForTypeInternal(protocolName, elem, instances)) {
        return false;
      }
    }
    const instance = generateSyntheticInstance(protocolName, type, _protocolMethodUsages);
    instances.push(instance);
    return true;
  }
  const fieldTypes = getRecordFieldTypes(type);
  if (fieldTypes) {
    for (const fieldType of Object.values(fieldTypes)) {
      if (!findInstanceForTypeInternal(protocolName, fieldType, instances)) {
        return false;
      }
    }
    const instance = generateSyntheticInstance(protocolName, type, _protocolMethodUsages);
    instances.push(instance);
    return true;
  }
  return false;
}
function generateSyntheticInstance(protocolName, type, usages) {
  const methods = new Map;
  const span = syntheticSpan;
  if (protocolName === "Eq") {
    methods.set("==", generateSyntheticEq(type, usages));
  } else if (protocolName === "Show") {
    methods.set("toString", generateSyntheticShow(type, usages));
  }
  return {
    protocolName,
    moduleName: _currentModuleName ?? "Synthetic",
    typeArgs: [type],
    constraints: [],
    methods,
    explicitMethods: new Set(methods.keys()),
    span
  };
}
function generateSyntheticEq(type, usages) {
  const span = syntheticSpan;
  if (type.kind === "tuple") {
    const arity = type.elements.length;
    const xVars = Array.from({ length: arity }, (_, i) => `x${i}`);
    const yVars = Array.from({ length: arity }, (_, i) => `y${i}`);
    let body = { kind: "Var", name: "True", namespace: "upper", span };
    if (arity > 0) {
      const checks = xVars.map((xv, i) => {
        const node = {
          kind: "Infix",
          left: { kind: "Var", name: xv, namespace: "lower", span },
          operator: "==",
          right: { kind: "Var", name: yVars[i], namespace: "lower", span },
          span
        };
        if (usages) {
          usages.set(node, {
            protocolName: "Eq",
            typeArgs: [type.elements[i]]
          });
        }
        return node;
      });
      body = checks.reduce((acc, check) => ({
        kind: "Infix",
        left: acc,
        operator: "&&",
        right: check,
        span
      }));
    }
    const pattern = {
      kind: "TuplePattern",
      elements: [
        {
          kind: "TuplePattern",
          elements: xVars.map((v) => ({ kind: "VarPattern", name: v, span })),
          span
        },
        {
          kind: "TuplePattern",
          elements: yVars.map((v) => ({ kind: "VarPattern", name: v, span })),
          span
        }
      ],
      span
    };
    return {
      kind: "Lambda",
      args: [
        { kind: "VarPattern", name: "x_impl", span },
        { kind: "VarPattern", name: "y_impl", span }
      ],
      body: {
        kind: "Case",
        discriminant: {
          kind: "Tuple",
          elements: [
            { kind: "Var", name: "x_impl", namespace: "lower", span },
            { kind: "Var", name: "y_impl", namespace: "lower", span }
          ],
          span
        },
        branches: [{ pattern, body, span }],
        span
      },
      span
    };
  } else {
    const fieldTypes = getRecordFieldTypes(type);
    if (fieldTypes) {
      const fields = Object.keys(fieldTypes).sort();
      let body = { kind: "Var", name: "True", namespace: "upper", span };
      if (fields.length > 0) {
        const checks = fields.map((f) => {
          const node = {
            kind: "Infix",
            left: {
              kind: "FieldAccess",
              target: { kind: "Var", name: "x_impl", namespace: "lower", span },
              field: f,
              span
            },
            operator: "==",
            right: {
              kind: "FieldAccess",
              target: { kind: "Var", name: "y_impl", namespace: "lower", span },
              field: f,
              span
            },
            span
          };
          if (usages) {
            usages.set(node, {
              protocolName: "Eq",
              typeArgs: [fieldTypes[f]]
            });
          }
          return node;
        });
        body = checks.reduce((acc, check) => ({
          kind: "Infix",
          left: acc,
          operator: "&&",
          right: check,
          span
        }));
      }
      return {
        kind: "Lambda",
        args: [
          { kind: "VarPattern", name: "x_impl", span },
          { kind: "VarPattern", name: "y_impl", span }
        ],
        body,
        span
      };
    }
  }
  throw new Error("Unsupported type for synthetic Eq");
}
function generateSyntheticShow(type, usages) {
  const span = syntheticSpan;
  if (type.kind === "tuple") {
    const arity = type.elements.length;
    const vars = Array.from({ length: arity }, (_, i) => `x${i}`);
    const str = (s) => ({
      kind: "String",
      value: `"${s}"`,
      span
    });
    const append = (a, b) => ({
      kind: "Infix",
      left: a,
      operator: "++",
      right: b,
      span
    });
    const toStringCall = (valName, elemType) => {
      const callee = {
        kind: "Var",
        name: "toString",
        namespace: "lower",
        span
      };
      const node = {
        kind: "Apply",
        callee,
        args: [{ kind: "Var", name: valName, namespace: "lower", span }],
        span
      };
      if (usages) {
        usages.set(callee, { protocolName: "Show", typeArgs: [elemType] });
      }
      return node;
    };
    let body = str("(");
    vars.forEach((v, i) => {
      if (i > 0)
        body = append(body, str(", "));
      body = append(body, toStringCall(v, type.elements[i]));
    });
    body = append(body, str(")"));
    const pattern = {
      kind: "TuplePattern",
      elements: vars.map((v) => ({ kind: "VarPattern", name: v, span })),
      span
    };
    return {
      kind: "Lambda",
      args: [{ kind: "VarPattern", name: "x_impl", span }],
      body: {
        kind: "Case",
        discriminant: { kind: "Var", name: "x_impl", namespace: "lower", span },
        branches: [{ pattern, body, span }],
        span
      },
      span
    };
  } else {
    const fieldTypes = getRecordFieldTypes(type);
    if (fieldTypes) {
      const fields = Object.keys(fieldTypes).sort();
      const typeName = type.kind === "con" ? type.name : null;
      const str = (s) => ({
        kind: "String",
        value: `"${s}"`,
        span
      });
      const append = (a, b) => ({
        kind: "Infix",
        left: a,
        operator: "++",
        right: b,
        span
      });
      const toStringField = (f) => {
        const callee = {
          kind: "Var",
          name: "toString",
          namespace: "lower",
          span
        };
        const node = {
          kind: "Apply",
          callee,
          args: [
            {
              kind: "FieldAccess",
              target: { kind: "Var", name: "x_impl", namespace: "lower", span },
              field: f,
              span
            }
          ],
          span
        };
        if (usages) {
          usages.set(callee, {
            protocolName: "Show",
            typeArgs: [fieldTypes[f]]
          });
        }
        return node;
      };
      let body = str(typeName ? `${typeName} { ` : "{ ");
      fields.forEach((f, i) => {
        if (i > 0)
          body = append(body, str(", "));
        body = append(body, str(`${f} = `));
        body = append(body, toStringField(f));
      });
      body = append(body, str(" }"));
      return {
        kind: "Lambda",
        args: [{ kind: "VarPattern", name: "x_impl", span }],
        body,
        span
      };
    }
  }
  throw new Error("Unsupported type for synthetic Show");
}
function symbolExists(scope, name) {
  return scope.lookup(name) !== undefined;
}
function bindPatternNames(pattern, scope) {
  switch (pattern.kind) {
    case "VarPattern":
      scope.define(pattern.name, {
        vars: new Set,
        constraints: [],
        type: { kind: "var", id: -1 }
      });
      return;
    case "WildcardPattern":
      return;
    case "ConstructorPattern":
      for (const arg of pattern.args) {
        bindPatternNames(arg, scope);
      }
      return;
    case "TuplePattern":
      for (const element of pattern.elements) {
        bindPatternNames(element, scope);
      }
      return;
    case "ListPattern":
      for (const element of pattern.elements) {
        bindPatternNames(element, scope);
      }
      return;
    case "ConsPattern":
      bindPatternNames(pattern.head, scope);
      bindPatternNames(pattern.tail, scope);
      return;
    case "RecordPattern":
      for (const field2 of pattern.fields) {
        if (field2.pattern) {
          bindPatternNames(field2.pattern, scope);
        }
      }
      return;
  }
}
function typeArgsEqual(args1, args2) {
  if (args1.length !== args2.length)
    return false;
  return args1.every((t, i) => typesEqual(t, args2[i]));
}
function instancesOverlap(inst1, inst2, instances) {
  if (!typesOverlap(inst1.typeArgs, inst2.typeArgs)) {
    return false;
  }
  for (let i = 0;i < inst1.typeArgs.length; i++) {
    const type1 = inst1.typeArgs[i];
    const type2 = inst2.typeArgs[i];
    if (type1.kind === "var" && type2.kind === "con") {
      const constraintsForVar = inst1.constraints.filter((c) => c.typeArgs.some((t) => t.kind === "var" && t.id === type1.id));
      for (const constraint of constraintsForVar) {
        const substitutedType = substituteTypeInConstraint(constraint, type1.id, type2);
        if (!canSatisfyConstraint(substitutedType, constraint.protocolName, instances)) {
          return false;
        }
      }
    }
    if (type2.kind === "var" && type1.kind === "con") {
      const constraintsForVar = inst2.constraints.filter((c) => c.typeArgs.some((t) => t.kind === "var" && t.id === type2.id));
      for (const constraint of constraintsForVar) {
        const substitutedType = substituteTypeInConstraint(constraint, type2.id, type1);
        if (!canSatisfyConstraint(substitutedType, constraint.protocolName, instances)) {
          return false;
        }
      }
    }
  }
  return true;
}
function substituteTypeInConstraint(constraint, varId, replacement) {
  for (const typeArg of constraint.typeArgs) {
    if (typeArg.kind === "var" && typeArg.id === varId) {
      return replacement;
    }
    if (typeArg.kind === "con") {
      const substitutedArgs = typeArg.args.map((arg) => arg.kind === "var" && arg.id === varId ? replacement : arg);
      return { kind: "con", name: typeArg.name, args: substitutedArgs };
    }
  }
  return constraint.typeArgs[0] || replacement;
}
function canSatisfyConstraint(concreteType, protocolName, instances) {
  if (concreteType.kind !== "con") {
    return true;
  }
  return findInstanceForTypeInternal(protocolName, concreteType, instances);
}
function typesOverlap(types1, types2) {
  if (types1.length !== types2.length)
    return false;
  for (let i = 0;i < types1.length; i++) {
    if (!typeOverlaps(types1[i], types2[i])) {
      return false;
    }
  }
  return true;
}
function typeOverlaps(type1, type2) {
  if (type1.kind === "var" || type2.kind === "var")
    return true;
  if (type1.kind === "con" && type2.kind === "con") {
    if (type1.name !== type2.name)
      return false;
    return typesOverlap(type1.args, type2.args);
  }
  if (type1.kind === "fun" && type2.kind === "fun") {
    return typeOverlaps(type1.from, type2.from) && typeOverlaps(type1.to, type2.to);
  }
  if (type1.kind === "tuple" && type2.kind === "tuple") {
    return typesOverlap(type1.elements, type2.elements);
  }
  if (type1.kind === "record" && type2.kind === "record") {
    const fields1 = Object.keys(type1.fields);
    const fields2 = Object.keys(type2.fields);
    if (fields1.length !== fields2.length)
      return false;
    for (const field2 of fields1) {
      if (!type2.fields[field2])
        return false;
      if (!typeOverlaps(type1.fields[field2], type2.fields[field2])) {
        return false;
      }
    }
    return true;
  }
  return false;
}
function resolveModuleField(depModule, field2, substitution) {
  if (Object.hasOwn(depModule.typeSchemes, field2)) {
    const scheme = depModule.typeSchemes[field2];
    return instantiate(scheme, substitution);
  }
  if (Object.hasOwn(depModule.values, field2)) {
    const valueInfo = depModule.values[field2];
    const valueType = valueInfo.type || depModule.types[field2];
    if (valueType) {
      return valueType;
    }
  }
  if (Object.hasOwn(depModule.constructorTypes, field2)) {
    const ctorScheme = depModule.constructorTypes[field2];
    return instantiate(ctorScheme, substitution);
  }
  return null;
}
function tryResolveModuleFieldAccess(expr, imports, dependencies, substitution) {
  const parts = [];
  let current2 = expr;
  while (current2.kind === "FieldAccess") {
    parts.unshift(current2.field);
    current2 = current2.target;
  }
  if (current2.kind !== "Var") {
    return null;
  }
  const baseName = current2.name;
  parts.unshift(baseName);
  for (const imp of imports) {
    const importParts = imp.moduleName.split(".");
    if (imp.alias && baseName === imp.alias && parts.length >= 2) {
      const depModule = dependencies.get(imp.moduleName);
      if (!depModule) {
        continue;
      }
      const fieldParts = parts.slice(1);
      if (fieldParts.length === 1) {
        const field2 = fieldParts[0];
        const resolved = resolveModuleField(depModule, field2, substitution);
        if (resolved)
          return resolved;
      }
    }
    if (importParts.length <= parts.length - 1) {
      let matches = true;
      for (let i = 0;i < importParts.length; i++) {
        if (importParts[i] !== parts[i]) {
          matches = false;
          break;
        }
      }
      if (matches) {
        const depModule = dependencies.get(imp.moduleName);
        if (!depModule) {
          return null;
        }
        const fieldParts = parts.slice(importParts.length);
        if (fieldParts.length === 1) {
          const field2 = fieldParts[0];
          const resolved = resolveModuleField(depModule, field2, substitution);
          if (resolved)
            return resolved;
        }
      }
    }
  }
  return null;
}
function instantiateWithConstraints(scheme, substitution) {
  if (scheme.vars.size === 0) {
    return { type: scheme.type, constraints: scheme.constraints };
  }
  const instantiationMap = new Map;
  for (const varId of scheme.vars) {
    instantiationMap.set(varId, freshType());
  }
  const instantiatedType = instantiateType(scheme.type, instantiationMap, substitution);
  const instantiatedConstraints = scheme.constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((t) => instantiateType(t, instantiationMap, substitution))
  }));
  return { type: instantiatedType, constraints: instantiatedConstraints };
}
function instantiate(scheme, substitution) {
  return instantiateWithConstraints(scheme, substitution).type;
}
function instantiateType(type, instantiationMap, substitution) {
  const concrete = applySubstitution(type, substitution);
  if (concrete.kind === "var") {
    const instantiated = instantiationMap.get(concrete.id);
    if (instantiated) {
      return instantiated;
    }
    return concrete;
  }
  if (concrete.kind === "con") {
    return {
      kind: "con",
      name: concrete.name,
      args: concrete.args.map((arg) => instantiateType(arg, instantiationMap, substitution))
    };
  }
  if (concrete.kind === "fun") {
    return {
      kind: "fun",
      from: instantiateType(concrete.from, instantiationMap, substitution),
      to: instantiateType(concrete.to, instantiationMap, substitution)
    };
  }
  if (concrete.kind === "tuple") {
    return {
      kind: "tuple",
      elements: concrete.elements.map((el) => instantiateType(el, instantiationMap, substitution))
    };
  }
  if (concrete.kind === "record") {
    const fields = {};
    for (const [key, fieldType] of Object.entries(concrete.fields)) {
      fields[key] = instantiateType(fieldType, instantiationMap, substitution);
    }
    return { kind: "record", fields };
  }
  return concrete;
}
function extractAnnotationParams(annotation, argCount, span) {
  const params = flattenFunctionParams(annotation);
  const result = params.slice(0, argCount);
  while (result.length < argCount) {
    result.push(freshType());
  }
  return result;
}
function extractAnnotationReturn(annotation, argCount) {
  let result = annotation;
  for (let i = 0;i < argCount; i++) {
    if (result.kind !== "fun") {
      return result;
    }
    result = result.to;
  }
  return result;
}
function isTypeVariable(name) {
  if (name.length === 0)
    return false;
  const firstChar = name[0];
  return firstChar === firstChar.toLowerCase();
}
function occursIn(id, type, substitution) {
  const concrete = applySubstitution(type, substitution);
  if (concrete.kind === "var") {
    return concrete.id === id;
  }
  switch (concrete.kind) {
    case "fun":
      return occursIn(id, concrete.from, substitution) || occursIn(id, concrete.to, substitution);
    case "tuple":
      return concrete.elements.some((t) => occursIn(id, t, substitution));
    case "record":
      return Object.values(concrete.fields).some((t) => occursIn(id, t, substitution));
    case "con":
      return concrete.args.some((t) => occursIn(id, t, substitution));
    default:
      return false;
  }
}
function resolveQualifiedType(name, adts, typeAliases, opaqueTypes, records, imports, dependencies) {
  if (adts[name])
    return { kind: "adt", name, info: adts[name] };
  if (typeAliases[name])
    return { kind: "alias", name, info: typeAliases[name] };
  if (opaqueTypes[name])
    return { kind: "opaque", name, info: opaqueTypes[name] };
  if (records[name])
    return { kind: "record", name, info: records[name] };
  const parts = name.split(".");
  if (parts.length > 1) {
    const typeName = parts.pop();
    const moduleName = parts.join(".");
    for (const imp of imports) {
      if (imp.alias === moduleName || imp.moduleName === moduleName) {
        const depModule = dependencies.get(imp.moduleName);
        if (depModule) {
          if (depModule.adts[typeName])
            return {
              kind: "adt",
              name: typeName,
              info: depModule.adts[typeName]
            };
          if (depModule.typeAliases[typeName])
            return {
              kind: "alias",
              name: typeName,
              info: depModule.typeAliases[typeName]
            };
          if (depModule.opaqueTypes[typeName])
            return {
              kind: "opaque",
              name: typeName,
              info: depModule.opaqueTypes[typeName]
            };
          if (depModule.records[typeName])
            return {
              kind: "record",
              name: typeName,
              info: depModule.records[typeName]
            };
        }
      }
    }
  }
  return null;
}
function resolveQualifiedConstructor(name, constructors, adts, imports, dependencies, moduleContext) {
  if (moduleContext) {
    const depModule = dependencies.get(moduleContext);
    if (depModule && depModule.constructors[name]) {
      const info = depModule.constructors[name];
      const adt = depModule.adts[info.parentType];
      if (adt) {
        return { name, info, adt };
      }
    }
  }
  if (constructors[name]) {
    const info = constructors[name];
    const adt = adts[info.parentType];
    if (adt) {
      return { name, info, adt };
    }
  }
  const parts = name.split(".");
  if (parts.length > 1) {
    const ctorName = parts.pop();
    const moduleName = parts.join(".");
    for (const imp of imports) {
      if (imp.alias === moduleName || imp.moduleName === moduleName) {
        const depModule = dependencies.get(imp.moduleName);
        if (depModule) {
          if (depModule.constructors[ctorName]) {
            const info = depModule.constructors[ctorName];
            const adt = depModule.adts[info.parentType];
            if (adt) {
              return { name: ctorName, info, adt };
            }
          }
        }
      }
    }
  }
  return null;
}

// ../module-resolver/src/index.ts
import fs2 from "node:fs";
import path2 from "node:path";

// ../config/src/index.ts
import fs from "node:fs";
import path from "node:path";
var DEFAULT_CONFIG_NAME = "vibe.json";
function loadConfig(options = {}) {
  const cwd = options.cwd ?? process.cwd();
  const configPath = resolveConfigPath(options.path, cwd);
  if (!fs.existsSync(configPath)) {
    throw new Error(`No ${DEFAULT_CONFIG_NAME} found at ${configPath}`);
  }
  const rawText = fs.readFileSync(configPath, "utf8");
  let rawConfig;
  try {
    rawConfig = JSON.parse(rawText);
  } catch (error) {
    throw new Error(`Invalid JSON in ${configPath}: ${error.message}`);
  }
  const config = validateConfig(rawConfig, configPath);
  const rootDir = path.dirname(configPath);
  return {
    ...config,
    rootDir,
    configPath,
    srcDir: path.resolve(rootDir, config.src),
    distDir: path.resolve(rootDir, config.dist)
  };
}
function resolveConfigPath(explicitPath, cwd) {
  if (explicitPath) {
    return path.isAbsolute(explicitPath) ? explicitPath : path.resolve(cwd, explicitPath);
  }
  return path.join(cwd, DEFAULT_CONFIG_NAME);
}
function validateConfig(config, sourcePath) {
  if (!config || typeof config !== "object") {
    throw new Error(`Config at ${sourcePath} must be an object`);
  }
  const shape = config;
  const name = expectString(shape, "name", sourcePath);
  const src = expectString(shape, "src", sourcePath);
  const dist = expectString(shape, "dist", sourcePath);
  const packages = expectStringArray(shape, "packages", sourcePath);
  return { name, src, dist, packages };
}
function expectString(config, key, sourcePath) {
  const value2 = config[key];
  if (typeof value2 !== "string" || value2.trim() === "") {
    throw new Error(`Config field "${key}" in ${sourcePath} must be a non-empty string`);
  }
  return value2;
}
function expectStringArray(config, key, sourcePath) {
  const value2 = config[key];
  if (!Array.isArray(value2)) {
    throw new Error(`Config field "${key}" in ${sourcePath} must be an array of package names`);
  }
  const items = value2.map((entry) => {
    if (typeof entry !== "string" || entry.trim() === "") {
      throw new Error(`Config field "${key}" in ${sourcePath} must only contain non-empty strings`);
    }
    return entry;
  });
  return items;
}

// ../module-resolver/src/index.ts
function resolveModule(input) {
  const { config, moduleName, preferDist = false } = input;
  const relativePath = moduleNameToRelativePath(moduleName);
  const candidates = [
    {
      packageName: config.name,
      baseDir: preferDist ? config.distDir : config.srcDir,
      srcDir: config.srcDir
    }
  ];
  for (const pkgName of config.packages) {
    const pkgRoot = resolvePackageRoot(pkgName, config.rootDir);
    if (!pkgRoot) {
      continue;
    }
    const pkgConfigPath = path2.join(pkgRoot, DEFAULT_CONFIG_NAME);
    let pkgSrcDir = path2.join(pkgRoot, "src");
    let pkgDistDir = path2.join(pkgRoot, "dist");
    let vibePackageName = pkgName;
    if (fs2.existsSync(pkgConfigPath)) {
      const pkgConfig = loadConfig({ path: pkgConfigPath });
      pkgSrcDir = pkgConfig.srcDir;
      pkgDistDir = pkgConfig.distDir;
      vibePackageName = pkgConfig.name;
    }
    candidates.push({
      packageName: vibePackageName,
      baseDir: preferDist ? pkgDistDir : pkgSrcDir,
      srcDir: pkgSrcDir
    });
  }
  for (const candidate of candidates) {
    const fullPath = path2.join(candidate.baseDir, relativePath);
    if (fs2.existsSync(fullPath)) {
      return {
        moduleName,
        packageName: candidate.packageName,
        filePath: fullPath,
        srcDir: candidate.srcDir
      };
    }
  }
  throw new Error(`Module "${moduleName}" not found in configured packages`);
}
function moduleNameToRelativePath(moduleName) {
  const segments = moduleName.split(".");
  if (segments.length === 0) {
    throw new Error("Module name must not be empty");
  }
  for (const segment of segments) {
    if (!/^[A-Z][A-Za-z0-9_]*$/.test(segment)) {
      throw new Error(`Invalid module segment "${segment}" in module name "${moduleName}"`);
    }
  }
  return path2.join(...segments) + ".vibe";
}
function resolvePackageRoot(pkgName, startDir) {
  const fromNodeModules = findUpward(startDir, (dir) => {
    const candidate = path2.join(dir, "node_modules", pkgName);
    return fs2.existsSync(path2.join(candidate, "package.json")) ? candidate : null;
  });
  if (fromNodeModules) {
    return fromNodeModules;
  }
  const workspaceRoot = findWorkspaceRoot(startDir);
  if (workspaceRoot) {
    const folderName = pkgName.includes("/") ? pkgName.split("/").at(-1) ?? pkgName : pkgName;
    const candidate = path2.join(workspaceRoot, "packages", folderName);
    if (fs2.existsSync(path2.join(candidate, "package.json"))) {
      return candidate;
    }
  }
  return null;
}
function findWorkspaceRoot(startDir) {
  return findUpward(startDir, (dir) => {
    const pkgDir = path2.join(dir, "packages");
    return fs2.existsSync(pkgDir) && fs2.statSync(pkgDir).isDirectory() ? dir : null;
  });
}
function findUpward(startDir, matcher) {
  let dir = path2.resolve(startDir);
  while (true) {
    const match = matcher(dir);
    if (match) {
      return match;
    }
    const parent = path2.dirname(dir);
    if (parent === dir) {
      return null;
    }
    dir = parent;
  }
}
function discoverSourceModules(srcDir) {
  const modules = [];
  function scan(dir, prefix) {
    if (!fs2.existsSync(dir)) {
      return;
    }
    const entries = fs2.readdirSync(dir, { withFileTypes: true });
    for (const entry of entries) {
      if (entry.isDirectory()) {
        const newPrefix = prefix ? `${prefix}.${entry.name}` : entry.name;
        scan(path2.join(dir, entry.name), newPrefix);
      } else if (entry.isFile() && entry.name.endsWith(".vibe")) {
        const baseName = entry.name.slice(0, -5);
        const moduleName = prefix ? `${prefix}.${baseName}` : baseName;
        modules.push(moduleName);
      }
    }
  }
  scan(srcDir, "");
  return modules;
}

// src/types.ts
var DEFAULT_CONFIG = {
  semanticTokens: true,
  completion: true,
  hover: true,
  goToDefinition: true,
  diagnostics: true,
  maxDiagnostics: 100,
  debounceDelay: 150
};

// src/document-manager.ts
import * as fs3 from "node:fs";
import * as path3 from "node:path";

class DocumentManager {
  documents = new Map;
  projectContext = null;
  pendingAnalysis = new Map;
  debounceDelay = 150;
  loadedModules = new Map;
  configCache = new Map;
  updateDocument(document) {
    const uri = document.uri;
    const version = document.version;
    const content = document.getText();
    const existing = this.documents.get(uri);
    if (existing && existing.version === version) {
      return existing;
    }
    const cache = {
      uri,
      version,
      content,
      diagnostics: [],
      lastAnalyzed: Date.now()
    };
    this.documents.set(uri, cache);
    this.analyzeDocument(cache);
    return cache;
  }
  getDocument(uri) {
    return this.documents.get(uri);
  }
  removeDocument(uri) {
    this.documents.delete(uri);
    const timer = this.pendingAnalysis.get(uri);
    if (timer) {
      clearTimeout(timer);
      this.pendingAnalysis.delete(uri);
    }
  }
  getAllDocuments() {
    return Array.from(this.documents.values());
  }
  resolveConfigForDocument(documentUri) {
    try {
      const filePath = documentUri.startsWith("file://") ? decodeURIComponent(documentUri.slice(7)) : documentUri;
      let dir = path3.dirname(filePath);
      while (dir && dir !== path3.dirname(dir)) {
        const vibeConfigPath = path3.join(dir, "vibe.json");
        const packageJsonPath = path3.join(dir, "package.json");
        if (fs3.existsSync(vibeConfigPath)) {
          const cached = this.configCache.get(vibeConfigPath);
          if (cached)
            return cached;
          const config = loadConfig({ path: vibeConfigPath });
          this.configCache.set(vibeConfigPath, config);
          return config;
        }
        if (fs3.existsSync(packageJsonPath)) {
          try {
            const pkgJson = JSON.parse(fs3.readFileSync(packageJsonPath, "utf8"));
            if (pkgJson.vibe) {
              const cached = this.configCache.get(packageJsonPath);
              if (cached)
                return cached;
              const config = loadConfig({ path: packageJsonPath });
              this.configCache.set(packageJsonPath, config);
              return config;
            }
          } catch {}
        }
        dir = path3.dirname(dir);
      }
    } catch {}
    return null;
  }
  loadModule(moduleName, config) {
    if (this.loadedModules.has(moduleName)) {
      return this.loadedModules.get(moduleName) || null;
    }
    try {
      const resolved = resolveModule({
        config,
        moduleName,
        preferDist: false
      });
      const source = fs3.readFileSync(resolved.filePath, "utf8");
      const { program } = parseWithInfix(source);
      const moduleDeps = new Map;
      if (program.imports) {
        for (const imp of program.imports) {
          if (!this.loadedModules.has(imp.moduleName)) {
            const depModule = this.loadModule(imp.moduleName, config);
            if (depModule) {
              moduleDeps.set(imp.moduleName, depModule);
            }
          } else {
            const cachedModule = this.loadedModules.get(imp.moduleName);
            if (cachedModule) {
              moduleDeps.set(imp.moduleName, cachedModule);
            }
          }
        }
      }
      for (const [name, mod] of this.loadedModules) {
        if (!moduleDeps.has(name)) {
          moduleDeps.set(name, mod);
        }
      }
      const analyzed = analyze(program, {
        dependencies: moduleDeps,
        fileContext: {
          filePath: resolved.filePath,
          srcDir: resolved.srcDir
        }
      });
      this.loadedModules.set(moduleName, analyzed);
      return analyzed;
    } catch {
      return null;
    }
  }
  loadDependencies(ast, config) {
    const dependencies = new Map;
    if (ast.imports) {
      for (const imp of ast.imports) {
        if (dependencies.has(imp.moduleName))
          continue;
        const cached = this.loadedModules.get(imp.moduleName);
        if (cached) {
          dependencies.set(imp.moduleName, cached);
        } else {
          const loaded = this.loadModule(imp.moduleName, config);
          if (loaded) {
            dependencies.set(imp.moduleName, loaded);
          }
        }
      }
    }
    const currentModule = ast.module?.name;
    const allSourceModules = this.discoverAllPackageModules(config);
    let pending = allSourceModules.filter((m) => m !== currentModule && !dependencies.has(m));
    for (let pass = 0;pass < 2 && pending.length > 0; pass++) {
      const stillPending = [];
      for (const moduleName of pending) {
        if (dependencies.has(moduleName))
          continue;
        const cached = this.loadedModules.get(moduleName);
        if (cached) {
          dependencies.set(moduleName, cached);
        } else {
          const loaded = this.loadModule(moduleName, config);
          if (loaded) {
            dependencies.set(moduleName, loaded);
          } else {
            stillPending.push(moduleName);
          }
        }
      }
      pending = stillPending;
    }
    return dependencies;
  }
  discoverAllPackageModules(config) {
    const seen = new Set;
    const result = [];
    const addModules = (srcDir) => {
      for (const name of discoverSourceModules(srcDir)) {
        if (!seen.has(name)) {
          seen.add(name);
          result.push(name);
        }
      }
    };
    addModules(config.srcDir);
    for (const pkgName of config.packages) {
      try {
        const pkgRoot = this.resolvePackageRoot(pkgName, config.rootDir);
        if (!pkgRoot)
          continue;
        const pkgConfigPath = path3.join(pkgRoot, "vibe.json");
        if (!fs3.existsSync(pkgConfigPath))
          continue;
        const pkgConfig = loadConfig({ path: pkgConfigPath });
        addModules(pkgConfig.srcDir);
      } catch {}
    }
    return result;
  }
  resolvePackageRoot(pkgName, startDir) {
    let dir = path3.resolve(startDir);
    while (true) {
      const candidate = path3.join(dir, "node_modules", pkgName);
      if (fs3.existsSync(path3.join(candidate, "package.json"))) {
        return fs3.realpathSync(candidate);
      }
      const parent = path3.dirname(dir);
      if (parent === dir)
        break;
      dir = parent;
    }
    return null;
  }
  analyzeDocument(cache) {
    const config = this.resolveConfigForDocument(cache.uri);
    const diagnostics = [];
    try {
      cache.tokens = lex2(cache.content);
    } catch (error) {
      if (error instanceof LexError) {
        diagnostics.push(this.createDiagnostic(error.message, error.span, import_vscode_languageserver.DiagnosticSeverity.Error));
      } else {
        diagnostics.push(this.createDiagnostic(`Lexer error: ${String(error)}`, this.defaultSpan(), import_vscode_languageserver.DiagnosticSeverity.Error));
      }
      cache.diagnostics = diagnostics;
      return;
    }
    try {
      const { program } = parseWithInfix(cache.content);
      cache.parseResult = { ast: program, errors: [] };
    } catch (error) {
      const parseError = this.extractParseError(error);
      cache.parseResult = { errors: [parseError] };
      diagnostics.push(this.createDiagnostic(parseError.message, parseError.span, import_vscode_languageserver.DiagnosticSeverity.Error));
      cache.diagnostics = diagnostics;
      return;
    }
    if (cache.parseResult?.ast) {
      try {
        const dependencies = config ? this.loadDependencies(cache.parseResult.ast, config) : new Map;
        const docFilePath = cache.uri.startsWith("file://") ? decodeURIComponent(cache.uri.slice(7)) : cache.uri;
        const analyzeOptions = {
          dependencies,
          fileContext: {
            filePath: docFilePath,
            srcDir: config?.srcDir || ""
          }
        };
        const semanticModule = analyze(cache.parseResult.ast, analyzeOptions);
        cache.semanticResult = { module: semanticModule, errors: [] };
      } catch (error) {
        if (error instanceof MultipleSemanticErrors) {
          const errorInfos = error.errors.map((e) => ({
            message: e.message,
            span: e.span
          }));
          cache.semanticResult = { errors: errorInfos };
          for (const e of error.errors) {
            diagnostics.push(this.createDiagnostic(e.message, e.span, import_vscode_languageserver.DiagnosticSeverity.Error));
          }
        } else if (error instanceof SemanticError) {
          cache.semanticResult = {
            errors: [{ message: error.message, span: error.span }]
          };
          diagnostics.push(this.createDiagnostic(error.message, error.span, import_vscode_languageserver.DiagnosticSeverity.Error));
        } else {
          const msg = error instanceof Error ? error.message : String(error);
          cache.semanticResult = {
            errors: [{ message: msg, span: this.defaultSpan() }]
          };
          diagnostics.push(this.createDiagnostic(`Semantic error: ${msg}`, this.defaultSpan(), import_vscode_languageserver.DiagnosticSeverity.Error));
        }
      }
    }
    cache.diagnostics = diagnostics;
    cache.lastAnalyzed = Date.now();
  }
  createDiagnostic(message, span, severity) {
    return {
      severity,
      range: {
        start: this.spanToPosition(span.start),
        end: this.spanToPosition(span.end)
      },
      message,
      source: "vibe"
    };
  }
  spanToPosition(pos) {
    return {
      line: Math.max(0, pos.line - 1),
      character: Math.max(0, pos.column - 1)
    };
  }
  extractParseError(error) {
    if (error instanceof Error) {
      const lineMatch = error.message.match(/line (\d+)/i);
      const colMatch = error.message.match(/column (\d+)/i);
      const line = lineMatch && lineMatch[1] ? parseInt(lineMatch[1], 10) : 1;
      const column = colMatch && colMatch[1] ? parseInt(colMatch[1], 10) : 1;
      return {
        message: error.message,
        span: {
          start: { offset: 0, line, column },
          end: { offset: 0, line, column: column + 1 }
        }
      };
    }
    return {
      message: String(error),
      span: this.defaultSpan()
    };
  }
  defaultSpan() {
    return {
      start: { offset: 0, line: 1, column: 1 },
      end: { offset: 0, line: 1, column: 1 }
    };
  }
  getSymbolsAtPosition(uri) {
    const cache = this.documents.get(uri);
    if (!cache?.semanticResult?.module) {
      return [];
    }
    const symbols = [];
    const module = cache.semanticResult.module;
    for (const [name, valueInfo] of Object.entries(module.values)) {
      const typeScheme = module.typeSchemes[name];
      symbols.push({
        name,
        kind: this.isFunction(typeScheme) ? "function" /* Function */ : "value" /* Value */,
        type: typeScheme,
        definitionSpan: valueInfo.declaration.span
      });
    }
    for (const [name, adtInfo] of Object.entries(module.adts)) {
      symbols.push({
        name,
        kind: "type" /* Type */,
        documentation: `type ${name}${adtInfo.params.length > 0 ? " " + adtInfo.params.join(" ") : ""}`,
        definitionSpan: adtInfo.span,
        moduleName: adtInfo.moduleName
      });
      for (const ctorName of adtInfo.constructors) {
        const ctorInfo = module.constructors[ctorName];
        const ctorType = module.constructorTypes[ctorName];
        if (ctorInfo) {
          symbols.push({
            name: ctorName,
            kind: "constructor" /* Constructor */,
            type: ctorType,
            definitionSpan: ctorInfo.span,
            moduleName: ctorInfo.moduleName
          });
        }
      }
    }
    for (const [name, aliasInfo] of Object.entries(module.typeAliases)) {
      symbols.push({
        name,
        kind: "typeAlias" /* TypeAlias */,
        definitionSpan: aliasInfo.span,
        moduleName: aliasInfo.moduleName
      });
    }
    for (const [name, protocolInfo] of Object.entries(module.protocols)) {
      symbols.push({
        name,
        kind: "protocol" /* Protocol */,
        documentation: `protocol ${name} ${protocolInfo.params.join(" ")}`,
        definitionSpan: protocolInfo.span,
        moduleName: protocolInfo.moduleName
      });
    }
    return symbols;
  }
  isFunction(scheme) {
    if (!scheme)
      return false;
    return scheme.type.kind === "fun";
  }
  findDefinition(uri, line, character) {
    const cache = this.documents.get(uri);
    if (!cache?.parseResult?.ast) {
      return;
    }
    const targetLine = line + 1;
    const targetColumn = character + 1;
    const name = this.findIdentifierAtPosition(cache.content, targetLine, targetColumn);
    if (!name) {
      return;
    }
    const symbols = this.getSymbolsAtPosition(uri);
    const symbol = symbols.find((s) => s.name === name);
    if (symbol?.definitionSpan) {
      return { uri, span: symbol.definitionSpan };
    }
    return;
  }
  findIdentifierAtPosition(content, line, column) {
    const lines = content.split(`
`);
    if (line < 1 || line > lines.length) {
      return;
    }
    const lineText = lines[line - 1];
    if (!lineText || column < 1 || column > lineText.length + 1) {
      return;
    }
    const idx = column - 1;
    let start = idx;
    let end = idx;
    while (start > 0) {
      const char = lineText[start - 1];
      if (!char || !/[a-zA-Z0-9_']/.test(char))
        break;
      start--;
    }
    while (end < lineText.length) {
      const char = lineText[end];
      if (!char || !/[a-zA-Z0-9_']/.test(char))
        break;
      end++;
    }
    if (start === end) {
      return;
    }
    return lineText.slice(start, end);
  }
  getHoverInfo(uri, line, character) {
    const cache = this.documents.get(uri);
    if (!cache?.semanticResult?.module) {
      return;
    }
    const targetLine = line + 1;
    const targetColumn = character + 1;
    const name = this.findIdentifierAtPosition(cache.content, targetLine, targetColumn);
    if (!name) {
      return;
    }
    const module = cache.semanticResult.module;
    if (module.typeSchemes[name]) {
      const typeStr = this.formatTypeScheme(module.typeSchemes[name]);
      const valueEntry = module.values[name];
      if (!valueEntry && module.instances.length > 0) {
        const concreteType = this.inferProtocolMethodType(name, module);
        if (concreteType) {
          return {
            name,
            type: concreteType,
            span: this.defaultSpan()
          };
        }
      }
      return {
        name,
        type: typeStr,
        span: valueEntry?.declaration?.span || this.defaultSpan()
      };
    }
    if (module.constructorTypes[name]) {
      const typeStr = this.formatTypeScheme(module.constructorTypes[name]);
      return {
        name,
        type: typeStr,
        span: module.constructors[name]?.span || this.defaultSpan()
      };
    }
    if (module.adts[name]) {
      const adt = module.adts[name];
      return {
        name,
        type: `type ${name}${adt.params.length > 0 ? " " + adt.params.join(" ") : ""}`,
        span: adt.span
      };
    }
    if (module.typeAliases[name]) {
      const alias = module.typeAliases[name];
      return {
        name,
        type: `type alias ${name}${alias.params.length > 0 ? " " + alias.params.join(" ") : ""}`,
        span: alias.span
      };
    }
    if (module.protocols[name]) {
      const protocol = module.protocols[name];
      return {
        name,
        type: `protocol ${name} ${protocol.params.join(" ")}`,
        span: protocol.span
      };
    }
    return;
  }
  formatTypeScheme(scheme) {
    return formatTypeSchemeForDisplay(scheme);
  }
  inferProtocolMethodType(methodName, module) {
    for (const instance of module.instances) {
      const protocol = module.protocols[instance.protocolName];
      if (!protocol)
        continue;
      const methodInfo = protocol.methods.get(methodName);
      if (methodInfo) {
        const tempScheme = {
          vars: new Set,
          constraints: [],
          type: methodInfo.type
        };
        const names = buildNormalizedNames(tempScheme);
        return formatTypeForDisplay(methodInfo.type, names);
      }
    }
    return;
  }
  formatType(type) {
    if (!type) {
      return "<unknown>";
    }
    const tempScheme = {
      vars: new Set,
      constraints: [],
      type
    };
    const names = buildNormalizedNames(tempScheme);
    return formatTypeForDisplay(type, names);
  }
}

// src/semantic-tokens.ts
var import_vscode_languageserver2 = __toESM(require_main4(), 1);
var TOKEN_TYPES = [
  import_vscode_languageserver2.SemanticTokenTypes.namespace,
  import_vscode_languageserver2.SemanticTokenTypes.type,
  import_vscode_languageserver2.SemanticTokenTypes.class,
  import_vscode_languageserver2.SemanticTokenTypes.enum,
  import_vscode_languageserver2.SemanticTokenTypes.interface,
  import_vscode_languageserver2.SemanticTokenTypes.typeParameter,
  import_vscode_languageserver2.SemanticTokenTypes.parameter,
  import_vscode_languageserver2.SemanticTokenTypes.variable,
  import_vscode_languageserver2.SemanticTokenTypes.property,
  import_vscode_languageserver2.SemanticTokenTypes.function,
  import_vscode_languageserver2.SemanticTokenTypes.keyword,
  import_vscode_languageserver2.SemanticTokenTypes.string,
  import_vscode_languageserver2.SemanticTokenTypes.number,
  import_vscode_languageserver2.SemanticTokenTypes.operator,
  import_vscode_languageserver2.SemanticTokenTypes.comment,
  import_vscode_languageserver2.SemanticTokenTypes.macro
];
var TOKEN_MODIFIERS = [
  import_vscode_languageserver2.SemanticTokenModifiers.declaration,
  import_vscode_languageserver2.SemanticTokenModifiers.definition,
  import_vscode_languageserver2.SemanticTokenModifiers.readonly,
  import_vscode_languageserver2.SemanticTokenModifiers.static,
  import_vscode_languageserver2.SemanticTokenModifiers.defaultLibrary
];
var SEMANTIC_TOKENS_LEGEND = {
  tokenTypes: TOKEN_TYPES,
  tokenModifiers: TOKEN_MODIFIERS
};
var TokenTypeIndex = {
  Namespace: 0,
  Type: 1,
  Protocol: 2,
  Constructor: 3,
  TypeAlias: 4,
  TypeParameter: 5,
  Parameter: 6,
  Variable: 7,
  Property: 8,
  Function: 9,
  Keyword: 10,
  String: 11,
  Number: 12,
  Operator: 13,
  Comment: 14,
  External: 15
};
var TokenModifierFlags = {
  Declaration: 1 << 0,
  Definition: 1 << 1,
  Readonly: 1 << 2,
  Static: 1 << 3,
  DefaultLibrary: 1 << 4
};
function provideSemanticTokens(cache) {
  const tokens = [];
  if (cache.parseResult?.ast) {
    extractASTTokens(cache.parseResult.ast, cache.semanticResult?.module, cache.content, tokens);
  }
  tokens.sort((a, b) => {
    if (a.line !== b.line)
      return a.line - b.line;
    return a.char - b.char;
  });
  return encodeTokens(tokens);
}
function extractASTTokens(ast, semantics, content, tokens) {
  if (ast.module) {
    addToken(tokens, ast.module.span, TokenTypeIndex.Namespace, TokenModifierFlags.Declaration);
  }
  if (ast.imports) {
    for (const imp of ast.imports) {
      if (!imp)
        continue;
      const modSpan = getModuleNameSpan(imp.span, imp.moduleName, content);
      addToken(tokens, modSpan, TokenTypeIndex.Namespace, 0);
    }
  }
  if (ast.declarations) {
    for (const decl of ast.declarations) {
      if (!decl)
        continue;
      extractDeclarationTokens(decl, semantics, content, tokens);
    }
  }
}
function extractDeclarationTokens(decl, semantics, content, tokens) {
  if (!decl)
    return;
  switch (decl.kind) {
    case "ValueDeclaration": {
      const isFunction = decl.args && decl.args.length > 0 || semantics?.typeSchemes[decl.name]?.type.kind === "fun";
      const typeIdx = isFunction ? TokenTypeIndex.Function : TokenTypeIndex.Variable;
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(tokens, nameSpan, typeIdx, TokenModifierFlags.Declaration | TokenModifierFlags.Definition | TokenModifierFlags.Readonly);
      if (decl.args) {
        for (const arg of decl.args) {
          if (arg) {
            extractPatternTokens(arg, content, tokens, true);
          }
        }
      }
      if (decl.body) {
        extractExprTokens(decl.body, semantics, content, tokens);
      }
      break;
    }
    case "TypeAnnotationDeclaration": {
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      const isFunction = decl.annotation?.kind === "FunctionType" || decl.annotation?.kind === "QualifiedType";
      addToken(tokens, nameSpan, isFunction ? TokenTypeIndex.Function : TokenTypeIndex.Variable, TokenModifierFlags.Declaration);
      if (decl.annotation) {
        extractTypeExprTokens(decl.annotation, content, tokens);
      }
      break;
    }
    case "TypeDeclaration": {
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(tokens, nameSpan, TokenTypeIndex.Type, TokenModifierFlags.Declaration | TokenModifierFlags.Definition);
      if (decl.constructors) {
        for (const ctor of decl.constructors) {
          addToken(tokens, ctor.span, TokenTypeIndex.Constructor, TokenModifierFlags.Declaration);
          for (const arg of ctor.args) {
            extractTypeExprTokens(arg, content, tokens);
          }
        }
      }
      if (decl.recordFields) {
        for (const field2 of decl.recordFields) {
          const fieldSpan = getNameSpan(field2.span, field2.name, content);
          addToken(tokens, fieldSpan, TokenTypeIndex.Property, 0);
          extractTypeExprTokens(field2.type, content, tokens);
        }
      }
      break;
    }
    case "TypeAliasDeclaration": {
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(tokens, nameSpan, TokenTypeIndex.TypeAlias, TokenModifierFlags.Declaration | TokenModifierFlags.Definition);
      extractTypeExprTokens(decl.value, content, tokens);
      break;
    }
    case "OpaqueTypeDeclaration": {
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(tokens, nameSpan, TokenTypeIndex.Type, TokenModifierFlags.Declaration | TokenModifierFlags.Definition);
      break;
    }
    case "ProtocolDeclaration": {
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(tokens, nameSpan, TokenTypeIndex.Protocol, TokenModifierFlags.Declaration | TokenModifierFlags.Definition);
      for (const method of decl.methods) {
        const methodSpan = getNameSpan(method.span, method.name, content);
        addToken(tokens, methodSpan, TokenTypeIndex.Function, TokenModifierFlags.Declaration);
        if (method.type) {
          extractTypeExprTokens(method.type, content, tokens);
        }
        if (method.defaultImpl) {
          for (const arg of method.defaultImpl.args) {
            extractPatternTokens(arg, content, tokens, true);
          }
          extractExprTokens(method.defaultImpl.body, semantics, content, tokens);
        }
      }
      break;
    }
    case "ImplementationDeclaration": {
      const protocolSpan = getNameSpan(decl.span, decl.protocolName, content);
      addToken(tokens, protocolSpan, TokenTypeIndex.Protocol, 0);
      for (const typeArg of decl.typeArgs) {
        extractTypeExprTokens(typeArg, content, tokens);
      }
      for (const method of decl.methods) {
        const methodSpan = getNameSpan(method.span, method.name, content);
        addToken(tokens, methodSpan, TokenTypeIndex.Function, TokenModifierFlags.Definition);
        extractExprTokens(method.implementation, semantics, content, tokens);
      }
      break;
    }
    case "DecoratedDeclaration": {
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(tokens, nameSpan, TokenTypeIndex.External, TokenModifierFlags.Declaration | TokenModifierFlags.Definition);
      extractTypeExprTokens(decl.annotation, content, tokens);
      break;
    }
    case "InfixDeclaration": {
      const opSpan = getNameSpan(decl.span, decl.operator, content);
      addToken(tokens, opSpan, TokenTypeIndex.Operator, TokenModifierFlags.Declaration);
      break;
    }
  }
}
function extractExprTokens(expr, semantics, content, tokens) {
  if (!expr || !expr.kind)
    return;
  switch (expr.kind) {
    case "Var": {
      let typeIdx;
      if (expr.namespace === "upper") {
        typeIdx = TokenTypeIndex.Constructor;
      } else if (semantics?.typeSchemes[expr.name]?.type?.kind === "fun") {
        typeIdx = TokenTypeIndex.Function;
      } else {
        typeIdx = TokenTypeIndex.Variable;
      }
      addToken(tokens, expr.span, typeIdx, TokenModifierFlags.Readonly);
      break;
    }
    case "Number":
      addToken(tokens, expr.span, TokenTypeIndex.Number, 0);
      break;
    case "String":
      addToken(tokens, expr.span, TokenTypeIndex.String, 0);
      break;
    case "Char":
      addToken(tokens, expr.span, TokenTypeIndex.String, 0);
      break;
    case "Lambda":
      if (expr.args) {
        for (const arg of expr.args) {
          if (arg) {
            extractPatternTokens(arg, content, tokens, true);
          }
        }
      }
      if (expr.body) {
        extractExprTokens(expr.body, semantics, content, tokens);
      }
      break;
    case "Apply":
      if (expr.callee) {
        extractExprTokens(expr.callee, semantics, content, tokens);
      }
      if (expr.args) {
        for (const arg of expr.args) {
          if (arg) {
            extractExprTokens(arg, semantics, content, tokens);
          }
        }
      }
      break;
    case "If":
      if (expr.condition) {
        extractExprTokens(expr.condition, semantics, content, tokens);
      }
      if (expr.thenBranch) {
        extractExprTokens(expr.thenBranch, semantics, content, tokens);
      }
      if (expr.elseBranch) {
        extractExprTokens(expr.elseBranch, semantics, content, tokens);
      }
      break;
    case "LetIn":
      for (const binding of expr.bindings) {
        const nameSpan = getNameSpan(binding.span, binding.name, content);
        const isFunction = binding.args.length > 0;
        addToken(tokens, nameSpan, isFunction ? TokenTypeIndex.Function : TokenTypeIndex.Variable, TokenModifierFlags.Declaration | TokenModifierFlags.Readonly);
        for (const arg of binding.args) {
          extractPatternTokens(arg, content, tokens, true);
        }
        extractExprTokens(binding.body, semantics, content, tokens);
      }
      extractExprTokens(expr.body, semantics, content, tokens);
      break;
    case "Case":
      extractExprTokens(expr.discriminant, semantics, content, tokens);
      for (const branch of expr.branches) {
        extractPatternTokens(branch.pattern, content, tokens, false);
        extractExprTokens(branch.body, semantics, content, tokens);
      }
      break;
    case "Infix":
      extractExprTokens(expr.left, semantics, content, tokens);
      addToken(tokens, estimateOperatorSpan(expr), TokenTypeIndex.Operator, 0);
      extractExprTokens(expr.right, semantics, content, tokens);
      break;
    case "Paren":
      extractExprTokens(expr.expression, semantics, content, tokens);
      break;
    case "Tuple":
      for (const el of expr.elements) {
        extractExprTokens(el, semantics, content, tokens);
      }
      break;
    case "Unit":
      break;
    case "List":
      for (const el of expr.elements) {
        extractExprTokens(el, semantics, content, tokens);
      }
      break;
    case "ListRange":
      extractExprTokens(expr.start, semantics, content, tokens);
      extractExprTokens(expr.end, semantics, content, tokens);
      break;
    case "Record":
      for (const field2 of expr.fields) {
        const fieldSpan2 = getNameSpan(field2.span, field2.name, content);
        addToken(tokens, fieldSpan2, TokenTypeIndex.Property, 0);
        extractExprTokens(field2.value, semantics, content, tokens);
      }
      break;
    case "RecordUpdate": {
      const baseSpan = getNameSpan(expr.span, expr.base, content);
      addToken(tokens, baseSpan, TokenTypeIndex.Variable, 0);
      for (const field2 of expr.fields) {
        const fieldSpan2 = getNameSpan(field2.span, field2.name, content);
        addToken(tokens, fieldSpan2, TokenTypeIndex.Property, 0);
        extractExprTokens(field2.value, semantics, content, tokens);
      }
      break;
    }
    case "FieldAccess":
      extractExprTokens(expr.target, semantics, content, tokens);
      const fieldSpan = estimateFieldSpan(expr);
      addToken(tokens, fieldSpan, TokenTypeIndex.Property, 0);
      break;
  }
}
function extractPatternTokens(pattern, content, tokens, isParameter) {
  if (!pattern || !pattern.kind)
    return;
  switch (pattern.kind) {
    case "VarPattern":
      addToken(tokens, pattern.span, isParameter ? TokenTypeIndex.Parameter : TokenTypeIndex.Variable, TokenModifierFlags.Declaration | TokenModifierFlags.Readonly);
      break;
    case "WildcardPattern":
      break;
    case "ConstructorPattern":
      const ctorSpan = getNameSpan(pattern.span, pattern.name, content);
      addToken(tokens, ctorSpan, TokenTypeIndex.Constructor, 0);
      for (const arg of pattern.args) {
        extractPatternTokens(arg, content, tokens, false);
      }
      break;
    case "TuplePattern":
      for (const el of pattern.elements) {
        extractPatternTokens(el, content, tokens, false);
      }
      break;
    case "ListPattern":
      for (const el of pattern.elements) {
        extractPatternTokens(el, content, tokens, false);
      }
      break;
    case "ConsPattern":
      extractPatternTokens(pattern.head, content, tokens, false);
      extractPatternTokens(pattern.tail, content, tokens, false);
      break;
  }
}
function extractTypeExprTokens(typeExpr, content, tokens) {
  if (!typeExpr || !typeExpr.kind)
    return;
  switch (typeExpr.kind) {
    case "TypeRef": {
      const firstChar = typeExpr.name[0];
      const isTypeVar = firstChar && firstChar === firstChar.toLowerCase();
      const typeIdx = isTypeVar ? TokenTypeIndex.TypeParameter : TokenTypeIndex.Type;
      const nameSpan = getNameSpan(typeExpr.span, typeExpr.name, content);
      addToken(tokens, nameSpan, typeIdx, 0);
      for (const arg of typeExpr.args) {
        extractTypeExprTokens(arg, content, tokens);
      }
      break;
    }
    case "FunctionType":
      extractTypeExprTokens(typeExpr.from, content, tokens);
      extractTypeExprTokens(typeExpr.to, content, tokens);
      break;
    case "TupleType":
      for (const el of typeExpr.elements) {
        extractTypeExprTokens(el, content, tokens);
      }
      break;
    case "RecordType":
      for (const field2 of typeExpr.fields) {
        const fieldSpan = estimateRecordFieldSpan(typeExpr.span, field2.name);
        addToken(tokens, fieldSpan, TokenTypeIndex.Property, 0);
        extractTypeExprTokens(field2.type, content, tokens);
      }
      break;
    case "QualifiedType":
      for (const constraint of typeExpr.constraints) {
        const protocolSpan = getNameSpan(constraint.span, constraint.protocolName, content);
        addToken(tokens, protocolSpan, TokenTypeIndex.Protocol, 0);
        for (const arg of constraint.typeArgs) {
          extractTypeExprTokens(arg, content, tokens);
        }
      }
      extractTypeExprTokens(typeExpr.type, content, tokens);
      break;
  }
}
function addToken(tokens, span, type, modifiers) {
  const length3 = span.end.offset - span.start.offset;
  if (length3 <= 0)
    return;
  tokens.push({
    line: Math.max(0, span.start.line - 1),
    char: Math.max(0, span.start.column - 1),
    length: length3,
    type,
    modifiers
  });
}
function encodeTokens(tokens) {
  const data = [];
  let prevLine = 0;
  let prevChar = 0;
  for (const token of tokens) {
    const deltaLine = token.line - prevLine;
    const deltaChar = deltaLine === 0 ? token.char - prevChar : token.char;
    data.push(deltaLine, deltaChar, token.length, token.type, token.modifiers);
    prevLine = token.line;
    prevChar = token.char;
  }
  return data;
}
function getNameSpan(fullSpan, name, content) {
  const spanText = content.substring(fullSpan.start.offset, fullSpan.end.offset);
  const nameIndex = spanText.indexOf(name);
  if (nameIndex === -1) {
    return {
      start: fullSpan.start,
      end: {
        offset: fullSpan.start.offset + name.length,
        line: fullSpan.start.line,
        column: fullSpan.start.column + name.length
      }
    };
  }
  const nameStart = fullSpan.start.offset + nameIndex;
  const textBeforeName = spanText.substring(0, nameIndex);
  const newlinesBefore = (textBeforeName.match(/\n/g) || []).length;
  let startColumn;
  if (newlinesBefore > 0) {
    const lastNewlineIndex = textBeforeName.lastIndexOf(`
`);
    startColumn = textBeforeName.length - lastNewlineIndex;
  } else {
    startColumn = fullSpan.start.column + nameIndex;
  }
  return {
    start: {
      offset: nameStart,
      line: fullSpan.start.line + newlinesBefore,
      column: startColumn
    },
    end: {
      offset: nameStart + name.length,
      line: fullSpan.start.line + newlinesBefore,
      column: startColumn + name.length
    }
  };
}
function getModuleNameSpan(fullSpan, moduleName, content) {
  return getNameSpan(fullSpan, moduleName, content);
}
function estimateOperatorSpan(expr) {
  const leftEnd = expr.left.span.end;
  return {
    start: {
      offset: leftEnd.offset + 1,
      line: leftEnd.line,
      column: leftEnd.column + 1
    },
    end: {
      offset: leftEnd.offset + 1 + expr.operator.length,
      line: leftEnd.line,
      column: leftEnd.column + 1 + expr.operator.length
    }
  };
}
function estimateFieldSpan(expr) {
  const targetEnd = expr.target.span.end;
  return {
    start: {
      offset: targetEnd.offset + 1,
      line: targetEnd.line,
      column: targetEnd.column + 1
    },
    end: {
      offset: targetEnd.offset + 1 + expr.field.length,
      line: targetEnd.line,
      column: targetEnd.column + 1 + expr.field.length
    }
  };
}
function estimateRecordFieldSpan(recordSpan, fieldName) {
  return {
    start: recordSpan.start,
    end: {
      offset: recordSpan.start.offset + fieldName.length,
      line: recordSpan.start.line,
      column: recordSpan.start.column + fieldName.length
    }
  };
}

// src/server.ts
var connection = import_node.createConnection(import_node.ProposedFeatures.all);
var documents = new import_node.TextDocuments(TextDocument);
var documentManager = new DocumentManager;
var config = { ...DEFAULT_CONFIG };
var hasWorkspaceFolderCapability = false;
var hasDiagnosticRelatedInformationCapability = false;
var hasSemanticTokensCapability = false;
connection.onInitialize((params) => {
  const capabilities = params.capabilities;
  hasWorkspaceFolderCapability = !!capabilities.workspace?.workspaceFolders;
  hasDiagnosticRelatedInformationCapability = !!capabilities.textDocument?.publishDiagnostics?.relatedInformation;
  hasSemanticTokensCapability = !!capabilities.textDocument?.semanticTokens;
  const result = {
    capabilities: {
      textDocumentSync: import_node.TextDocumentSyncKind.Incremental,
      completionProvider: {
        resolveProvider: true,
        triggerCharacters: [".", ":"]
      },
      hoverProvider: true,
      definitionProvider: true,
      documentSymbolProvider: true,
      semanticTokensProvider: hasSemanticTokensCapability ? {
        legend: SEMANTIC_TOKENS_LEGEND,
        full: true,
        range: false
      } : undefined,
      workspace: hasWorkspaceFolderCapability ? {
        workspaceFolders: {
          supported: true,
          changeNotifications: true
        }
      } : undefined
    },
    serverInfo: {
      name: "vibe-language-server",
      version: "0.1.0"
    }
  };
  return result;
});
connection.onInitialized(() => {
  connection.console.log("Vibe Language Server initialized");
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders((_event) => {
      connection.console.log("Workspace folder change event received");
    });
  }
});
documents.onDidOpen((event) => {
  const doc = event.document;
  connection.console.log(`Document opened: ${doc.uri}`);
  const cache = documentManager.updateDocument(doc);
  sendDiagnostics(doc.uri, cache.diagnostics);
});
documents.onDidChangeContent((event) => {
  const doc = event.document;
  const cache = documentManager.updateDocument(doc);
  sendDiagnostics(doc.uri, cache.diagnostics);
});
documents.onDidClose((event) => {
  const uri = event.document.uri;
  connection.console.log(`Document closed: ${uri}`);
  documentManager.removeDocument(uri);
  connection.sendDiagnostics({ uri, diagnostics: [] });
});
function sendDiagnostics(uri, diagnostics) {
  connection.sendDiagnostics({
    uri,
    diagnostics: diagnostics.slice(0, config.maxDiagnostics)
  });
}
connection.onCompletion((params) => {
  const uri = params.textDocument.uri;
  const symbols = documentManager.getSymbolsAtPosition(uri);
  return symbols.map((symbol) => ({
    label: symbol.name,
    kind: symbolKindToCompletionKind(symbol.kind),
    detail: symbol.type ? documentManager.formatTypeScheme(symbol.type) : undefined,
    documentation: symbol.documentation,
    data: { uri, name: symbol.name }
  }));
});
connection.onCompletionResolve((item) => {
  return item;
});
function symbolKindToCompletionKind(kind) {
  switch (kind) {
    case "function" /* Function */:
      return import_node.CompletionItemKind.Function;
    case "value" /* Value */:
      return import_node.CompletionItemKind.Variable;
    case "constructor" /* Constructor */:
      return import_node.CompletionItemKind.Constructor;
    case "type" /* Type */:
      return import_node.CompletionItemKind.Class;
    case "typeAlias" /* TypeAlias */:
      return import_node.CompletionItemKind.Interface;
    case "opaqueType" /* OpaqueType */:
      return import_node.CompletionItemKind.Class;
    case "protocol" /* Protocol */:
      return import_node.CompletionItemKind.Interface;
    case "typeVariable" /* TypeVariable */:
      return import_node.CompletionItemKind.TypeParameter;
    case "parameter" /* Parameter */:
      return import_node.CompletionItemKind.Variable;
    case "field" /* Field */:
      return import_node.CompletionItemKind.Field;
    case "operator" /* Operator */:
      return import_node.CompletionItemKind.Operator;
    case "module" /* Module */:
      return import_node.CompletionItemKind.Module;
    default:
      return import_node.CompletionItemKind.Text;
  }
}
connection.onHover((params) => {
  const uri = params.textDocument.uri;
  const { line, character } = params.position;
  const hoverInfo = documentManager.getHoverInfo(uri, line, character);
  if (!hoverInfo) {
    return null;
  }
  const content = [];
  if (hoverInfo.type) {
    content.push("```vibe");
    content.push(`${hoverInfo.name} : ${hoverInfo.type}`);
    content.push("```");
  } else {
    content.push(`**${hoverInfo.name}**`);
  }
  return {
    contents: {
      kind: import_node.MarkupKind.Markdown,
      value: content.join(`
`)
    },
    range: {
      start: {
        line: Math.max(0, hoverInfo.span.start.line - 1),
        character: Math.max(0, hoverInfo.span.start.column - 1)
      },
      end: {
        line: Math.max(0, hoverInfo.span.end.line - 1),
        character: Math.max(0, hoverInfo.span.end.column - 1)
      }
    }
  };
});
connection.onDefinition((params) => {
  const uri = params.textDocument.uri;
  const { line, character } = params.position;
  const definition = documentManager.findDefinition(uri, line, character);
  if (!definition) {
    return null;
  }
  return import_node.Location.create(definition.uri, {
    start: {
      line: Math.max(0, definition.span.start.line - 1),
      character: Math.max(0, definition.span.start.column - 1)
    },
    end: {
      line: Math.max(0, definition.span.end.line - 1),
      character: Math.max(0, definition.span.end.column - 1)
    }
  });
});
connection.onDocumentSymbol((params) => {
  const uri = params.textDocument.uri;
  const symbols = documentManager.getSymbolsAtPosition(uri);
  return symbols.map((symbol) => ({
    name: symbol.name,
    kind: symbolKindToDocumentSymbolKind(symbol.kind),
    range: symbol.definitionSpan ? {
      start: {
        line: Math.max(0, symbol.definitionSpan.start.line - 1),
        character: Math.max(0, symbol.definitionSpan.start.column - 1)
      },
      end: {
        line: Math.max(0, symbol.definitionSpan.end.line - 1),
        character: Math.max(0, symbol.definitionSpan.end.column - 1)
      }
    } : {
      start: { line: 0, character: 0 },
      end: { line: 0, character: 0 }
    },
    selectionRange: symbol.definitionSpan ? {
      start: {
        line: Math.max(0, symbol.definitionSpan.start.line - 1),
        character: Math.max(0, symbol.definitionSpan.start.column - 1)
      },
      end: {
        line: Math.max(0, symbol.definitionSpan.end.line - 1),
        character: Math.max(0, symbol.definitionSpan.end.column - 1)
      }
    } : {
      start: { line: 0, character: 0 },
      end: { line: 0, character: 0 }
    }
  }));
});
function symbolKindToDocumentSymbolKind(kind) {
  switch (kind) {
    case "function" /* Function */:
      return 12;
    case "value" /* Value */:
      return 13;
    case "constructor" /* Constructor */:
      return 9;
    case "type" /* Type */:
      return 5;
    case "typeAlias" /* TypeAlias */:
      return 11;
    case "opaqueType" /* OpaqueType */:
      return 5;
    case "protocol" /* Protocol */:
      return 11;
    case "typeVariable" /* TypeVariable */:
      return 26;
    case "parameter" /* Parameter */:
      return 13;
    case "field" /* Field */:
      return 8;
    case "operator" /* Operator */:
      return 25;
    case "module" /* Module */:
      return 2;
    default:
      return 13;
  }
}
connection.languages.semanticTokens.on((params) => {
  const uri = params.textDocument.uri;
  const cache = documentManager.getDocument(uri);
  if (!cache) {
    return { data: [] };
  }
  const data = provideSemanticTokens(cache);
  return { data };
});
connection.onDidChangeConfiguration((change) => {
  if (change.settings?.vibe) {
    config = { ...DEFAULT_CONFIG, ...change.settings.vibe };
  }
});
documents.listen(connection);
connection.listen();
