export const processArgv = process.argv;

export const processCwd = (_unit) => {
  return process.cwd();
};

export const processExit = (code) => {
  process.exit(code);
};

export const processGetEnv = (just, nothing, name) => {
  const value = process.env[name];
  return value !== undefined ? just(value) : nothing;
};

export const processSetEnv = (name, value) => {
  process.env[name] = value;
};

export const processPlatform = process.platform;
