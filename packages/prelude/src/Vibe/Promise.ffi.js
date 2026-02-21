class PromiseWrapper {
  #promise;
  constructor(promise) {
    this.#promise = promise;
  }

  static wrap(value) {
    return value instanceof Promise ? new PromiseWrapper(value) : value;
  }

  static unwrap(value) {
    return value instanceof PromiseWrapper ? value.#promise : value;
  }
}

export const createPromise = (executor) => {
  return new Promise((resolve) =>
    executor((value) => {
      resolve(PromiseWrapper.wrap(value));
    }),
  );
};

export const flatMapPromise = (fn) => (promise) => {
  return promise.then((value) => fn(PromiseWrapper.unwrap(value)));
};

export const mapPromise = (fn) => (promise) => {
  return promise.then((value) =>
    PromiseWrapper.wrap(fn(PromiseWrapper.unwrap(value))),
  );
};

export const catchPromise = (fn) => (promise) => {
  return promise.catch((error) => PromiseWrapper.wrap(fn(error)));
};
