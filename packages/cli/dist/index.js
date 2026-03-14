// @bun
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
var __require = import.meta.require;

// ../../node_modules/.bun/commander@12.1.0/node_modules/commander/lib/error.js
var require_error = __commonJS((exports) => {
  class CommanderError extends Error {
    constructor(exitCode, code, message) {
      super(message);
      Error.captureStackTrace(this, this.constructor);
      this.name = this.constructor.name;
      this.code = code;
      this.exitCode = exitCode;
      this.nestedError = undefined;
    }
  }

  class InvalidArgumentError extends CommanderError {
    constructor(message) {
      super(1, "commander.invalidArgument", message);
      Error.captureStackTrace(this, this.constructor);
      this.name = this.constructor.name;
    }
  }
  exports.CommanderError = CommanderError;
  exports.InvalidArgumentError = InvalidArgumentError;
});

// ../../node_modules/.bun/commander@12.1.0/node_modules/commander/lib/argument.js
var require_argument = __commonJS((exports) => {
  var { InvalidArgumentError } = require_error();

  class Argument {
    constructor(name, description) {
      this.description = description || "";
      this.variadic = false;
      this.parseArg = undefined;
      this.defaultValue = undefined;
      this.defaultValueDescription = undefined;
      this.argChoices = undefined;
      switch (name[0]) {
        case "<":
          this.required = true;
          this._name = name.slice(1, -1);
          break;
        case "[":
          this.required = false;
          this._name = name.slice(1, -1);
          break;
        default:
          this.required = true;
          this._name = name;
          break;
      }
      if (this._name.length > 3 && this._name.slice(-3) === "...") {
        this.variadic = true;
        this._name = this._name.slice(0, -3);
      }
    }
    name() {
      return this._name;
    }
    _concatValue(value, previous) {
      if (previous === this.defaultValue || !Array.isArray(previous)) {
        return [value];
      }
      return previous.concat(value);
    }
    default(value, description) {
      this.defaultValue = value;
      this.defaultValueDescription = description;
      return this;
    }
    argParser(fn) {
      this.parseArg = fn;
      return this;
    }
    choices(values) {
      this.argChoices = values.slice();
      this.parseArg = (arg, previous) => {
        if (!this.argChoices.includes(arg)) {
          throw new InvalidArgumentError(`Allowed choices are ${this.argChoices.join(", ")}.`);
        }
        if (this.variadic) {
          return this._concatValue(arg, previous);
        }
        return arg;
      };
      return this;
    }
    argRequired() {
      this.required = true;
      return this;
    }
    argOptional() {
      this.required = false;
      return this;
    }
  }
  function humanReadableArgName(arg) {
    const nameOutput = arg.name() + (arg.variadic === true ? "..." : "");
    return arg.required ? "<" + nameOutput + ">" : "[" + nameOutput + "]";
  }
  exports.Argument = Argument;
  exports.humanReadableArgName = humanReadableArgName;
});

// ../../node_modules/.bun/commander@12.1.0/node_modules/commander/lib/help.js
var require_help = __commonJS((exports) => {
  var { humanReadableArgName } = require_argument();

  class Help {
    constructor() {
      this.helpWidth = undefined;
      this.sortSubcommands = false;
      this.sortOptions = false;
      this.showGlobalOptions = false;
    }
    visibleCommands(cmd) {
      const visibleCommands = cmd.commands.filter((cmd2) => !cmd2._hidden);
      const helpCommand = cmd._getHelpCommand();
      if (helpCommand && !helpCommand._hidden) {
        visibleCommands.push(helpCommand);
      }
      if (this.sortSubcommands) {
        visibleCommands.sort((a, b) => {
          return a.name().localeCompare(b.name());
        });
      }
      return visibleCommands;
    }
    compareOptions(a, b) {
      const getSortKey = (option) => {
        return option.short ? option.short.replace(/^-/, "") : option.long.replace(/^--/, "");
      };
      return getSortKey(a).localeCompare(getSortKey(b));
    }
    visibleOptions(cmd) {
      const visibleOptions = cmd.options.filter((option) => !option.hidden);
      const helpOption = cmd._getHelpOption();
      if (helpOption && !helpOption.hidden) {
        const removeShort = helpOption.short && cmd._findOption(helpOption.short);
        const removeLong = helpOption.long && cmd._findOption(helpOption.long);
        if (!removeShort && !removeLong) {
          visibleOptions.push(helpOption);
        } else if (helpOption.long && !removeLong) {
          visibleOptions.push(cmd.createOption(helpOption.long, helpOption.description));
        } else if (helpOption.short && !removeShort) {
          visibleOptions.push(cmd.createOption(helpOption.short, helpOption.description));
        }
      }
      if (this.sortOptions) {
        visibleOptions.sort(this.compareOptions);
      }
      return visibleOptions;
    }
    visibleGlobalOptions(cmd) {
      if (!this.showGlobalOptions)
        return [];
      const globalOptions = [];
      for (let ancestorCmd = cmd.parent;ancestorCmd; ancestorCmd = ancestorCmd.parent) {
        const visibleOptions = ancestorCmd.options.filter((option) => !option.hidden);
        globalOptions.push(...visibleOptions);
      }
      if (this.sortOptions) {
        globalOptions.sort(this.compareOptions);
      }
      return globalOptions;
    }
    visibleArguments(cmd) {
      if (cmd._argsDescription) {
        cmd.registeredArguments.forEach((argument) => {
          argument.description = argument.description || cmd._argsDescription[argument.name()] || "";
        });
      }
      if (cmd.registeredArguments.find((argument) => argument.description)) {
        return cmd.registeredArguments;
      }
      return [];
    }
    subcommandTerm(cmd) {
      const args = cmd.registeredArguments.map((arg) => humanReadableArgName(arg)).join(" ");
      return cmd._name + (cmd._aliases[0] ? "|" + cmd._aliases[0] : "") + (cmd.options.length ? " [options]" : "") + (args ? " " + args : "");
    }
    optionTerm(option) {
      return option.flags;
    }
    argumentTerm(argument) {
      return argument.name();
    }
    longestSubcommandTermLength(cmd, helper2) {
      return helper2.visibleCommands(cmd).reduce((max, command) => {
        return Math.max(max, helper2.subcommandTerm(command).length);
      }, 0);
    }
    longestOptionTermLength(cmd, helper2) {
      return helper2.visibleOptions(cmd).reduce((max, option) => {
        return Math.max(max, helper2.optionTerm(option).length);
      }, 0);
    }
    longestGlobalOptionTermLength(cmd, helper2) {
      return helper2.visibleGlobalOptions(cmd).reduce((max, option) => {
        return Math.max(max, helper2.optionTerm(option).length);
      }, 0);
    }
    longestArgumentTermLength(cmd, helper2) {
      return helper2.visibleArguments(cmd).reduce((max, argument) => {
        return Math.max(max, helper2.argumentTerm(argument).length);
      }, 0);
    }
    commandUsage(cmd) {
      let cmdName = cmd._name;
      if (cmd._aliases[0]) {
        cmdName = cmdName + "|" + cmd._aliases[0];
      }
      let ancestorCmdNames = "";
      for (let ancestorCmd = cmd.parent;ancestorCmd; ancestorCmd = ancestorCmd.parent) {
        ancestorCmdNames = ancestorCmd.name() + " " + ancestorCmdNames;
      }
      return ancestorCmdNames + cmdName + " " + cmd.usage();
    }
    commandDescription(cmd) {
      return cmd.description();
    }
    subcommandDescription(cmd) {
      return cmd.summary() || cmd.description();
    }
    optionDescription(option) {
      const extraInfo = [];
      if (option.argChoices) {
        extraInfo.push(`choices: ${option.argChoices.map((choice) => JSON.stringify(choice)).join(", ")}`);
      }
      if (option.defaultValue !== undefined) {
        const showDefault = option.required || option.optional || option.isBoolean() && typeof option.defaultValue === "boolean";
        if (showDefault) {
          extraInfo.push(`default: ${option.defaultValueDescription || JSON.stringify(option.defaultValue)}`);
        }
      }
      if (option.presetArg !== undefined && option.optional) {
        extraInfo.push(`preset: ${JSON.stringify(option.presetArg)}`);
      }
      if (option.envVar !== undefined) {
        extraInfo.push(`env: ${option.envVar}`);
      }
      if (extraInfo.length > 0) {
        return `${option.description} (${extraInfo.join(", ")})`;
      }
      return option.description;
    }
    argumentDescription(argument) {
      const extraInfo = [];
      if (argument.argChoices) {
        extraInfo.push(`choices: ${argument.argChoices.map((choice) => JSON.stringify(choice)).join(", ")}`);
      }
      if (argument.defaultValue !== undefined) {
        extraInfo.push(`default: ${argument.defaultValueDescription || JSON.stringify(argument.defaultValue)}`);
      }
      if (extraInfo.length > 0) {
        const extraDescripton = `(${extraInfo.join(", ")})`;
        if (argument.description) {
          return `${argument.description} ${extraDescripton}`;
        }
        return extraDescripton;
      }
      return argument.description;
    }
    formatHelp(cmd, helper2) {
      const termWidth = helper2.padWidth(cmd, helper2);
      const helpWidth = helper2.helpWidth || 80;
      const itemIndentWidth = 2;
      const itemSeparatorWidth = 2;
      function formatItem(term, description) {
        if (description) {
          const fullText = `${term.padEnd(termWidth + itemSeparatorWidth)}${description}`;
          return helper2.wrap(fullText, helpWidth - itemIndentWidth, termWidth + itemSeparatorWidth);
        }
        return term;
      }
      function formatList(textArray) {
        return textArray.join(`
`).replace(/^/gm, " ".repeat(itemIndentWidth));
      }
      let output = [`Usage: ${helper2.commandUsage(cmd)}`, ""];
      const commandDescription = helper2.commandDescription(cmd);
      if (commandDescription.length > 0) {
        output = output.concat([
          helper2.wrap(commandDescription, helpWidth, 0),
          ""
        ]);
      }
      const argumentList = helper2.visibleArguments(cmd).map((argument) => {
        return formatItem(helper2.argumentTerm(argument), helper2.argumentDescription(argument));
      });
      if (argumentList.length > 0) {
        output = output.concat(["Arguments:", formatList(argumentList), ""]);
      }
      const optionList = helper2.visibleOptions(cmd).map((option) => {
        return formatItem(helper2.optionTerm(option), helper2.optionDescription(option));
      });
      if (optionList.length > 0) {
        output = output.concat(["Options:", formatList(optionList), ""]);
      }
      if (this.showGlobalOptions) {
        const globalOptionList = helper2.visibleGlobalOptions(cmd).map((option) => {
          return formatItem(helper2.optionTerm(option), helper2.optionDescription(option));
        });
        if (globalOptionList.length > 0) {
          output = output.concat([
            "Global Options:",
            formatList(globalOptionList),
            ""
          ]);
        }
      }
      const commandList = helper2.visibleCommands(cmd).map((cmd2) => {
        return formatItem(helper2.subcommandTerm(cmd2), helper2.subcommandDescription(cmd2));
      });
      if (commandList.length > 0) {
        output = output.concat(["Commands:", formatList(commandList), ""]);
      }
      return output.join(`
`);
    }
    padWidth(cmd, helper2) {
      return Math.max(helper2.longestOptionTermLength(cmd, helper2), helper2.longestGlobalOptionTermLength(cmd, helper2), helper2.longestSubcommandTermLength(cmd, helper2), helper2.longestArgumentTermLength(cmd, helper2));
    }
    wrap(str, width, indent, minColumnWidth = 40) {
      const indents = " \\f\\t\\v\xA0\u1680\u2000-\u200A\u202F\u205F\u3000\uFEFF";
      const manualIndent = new RegExp(`[\\n][${indents}]+`);
      if (str.match(manualIndent))
        return str;
      const columnWidth = width - indent;
      if (columnWidth < minColumnWidth)
        return str;
      const leadingStr = str.slice(0, indent);
      const columnText = str.slice(indent).replace(`\r
`, `
`);
      const indentString = " ".repeat(indent);
      const zeroWidthSpace = "\u200B";
      const breaks = `\\s${zeroWidthSpace}`;
      const regex = new RegExp(`
|.{1,${columnWidth - 1}}([${breaks}]|$)|[^${breaks}]+?([${breaks}]|$)`, "g");
      const lines = columnText.match(regex) || [];
      return leadingStr + lines.map((line, i) => {
        if (line === `
`)
          return "";
        return (i > 0 ? indentString : "") + line.trimEnd();
      }).join(`
`);
    }
  }
  exports.Help = Help;
});

// ../../node_modules/.bun/commander@12.1.0/node_modules/commander/lib/option.js
var require_option = __commonJS((exports) => {
  var { InvalidArgumentError } = require_error();

  class Option {
    constructor(flags, description) {
      this.flags = flags;
      this.description = description || "";
      this.required = flags.includes("<");
      this.optional = flags.includes("[");
      this.variadic = /\w\.\.\.[>\]]$/.test(flags);
      this.mandatory = false;
      const optionFlags = splitOptionFlags(flags);
      this.short = optionFlags.shortFlag;
      this.long = optionFlags.longFlag;
      this.negate = false;
      if (this.long) {
        this.negate = this.long.startsWith("--no-");
      }
      this.defaultValue = undefined;
      this.defaultValueDescription = undefined;
      this.presetArg = undefined;
      this.envVar = undefined;
      this.parseArg = undefined;
      this.hidden = false;
      this.argChoices = undefined;
      this.conflictsWith = [];
      this.implied = undefined;
    }
    default(value, description) {
      this.defaultValue = value;
      this.defaultValueDescription = description;
      return this;
    }
    preset(arg) {
      this.presetArg = arg;
      return this;
    }
    conflicts(names) {
      this.conflictsWith = this.conflictsWith.concat(names);
      return this;
    }
    implies(impliedOptionValues) {
      let newImplied = impliedOptionValues;
      if (typeof impliedOptionValues === "string") {
        newImplied = { [impliedOptionValues]: true };
      }
      this.implied = Object.assign(this.implied || {}, newImplied);
      return this;
    }
    env(name) {
      this.envVar = name;
      return this;
    }
    argParser(fn) {
      this.parseArg = fn;
      return this;
    }
    makeOptionMandatory(mandatory = true) {
      this.mandatory = !!mandatory;
      return this;
    }
    hideHelp(hide = true) {
      this.hidden = !!hide;
      return this;
    }
    _concatValue(value, previous) {
      if (previous === this.defaultValue || !Array.isArray(previous)) {
        return [value];
      }
      return previous.concat(value);
    }
    choices(values) {
      this.argChoices = values.slice();
      this.parseArg = (arg, previous) => {
        if (!this.argChoices.includes(arg)) {
          throw new InvalidArgumentError(`Allowed choices are ${this.argChoices.join(", ")}.`);
        }
        if (this.variadic) {
          return this._concatValue(arg, previous);
        }
        return arg;
      };
      return this;
    }
    name() {
      if (this.long) {
        return this.long.replace(/^--/, "");
      }
      return this.short.replace(/^-/, "");
    }
    attributeName() {
      return camelcase(this.name().replace(/^no-/, ""));
    }
    is(arg) {
      return this.short === arg || this.long === arg;
    }
    isBoolean() {
      return !this.required && !this.optional && !this.negate;
    }
  }

  class DualOptions {
    constructor(options) {
      this.positiveOptions = new Map;
      this.negativeOptions = new Map;
      this.dualOptions = new Set;
      options.forEach((option) => {
        if (option.negate) {
          this.negativeOptions.set(option.attributeName(), option);
        } else {
          this.positiveOptions.set(option.attributeName(), option);
        }
      });
      this.negativeOptions.forEach((value, key) => {
        if (this.positiveOptions.has(key)) {
          this.dualOptions.add(key);
        }
      });
    }
    valueFromOption(value, option) {
      const optionKey = option.attributeName();
      if (!this.dualOptions.has(optionKey))
        return true;
      const preset = this.negativeOptions.get(optionKey).presetArg;
      const negativeValue = preset !== undefined ? preset : false;
      return option.negate === (negativeValue === value);
    }
  }
  function camelcase(str) {
    return str.split("-").reduce((str2, word) => {
      return str2 + word[0].toUpperCase() + word.slice(1);
    });
  }
  function splitOptionFlags(flags) {
    let shortFlag;
    let longFlag;
    const flagParts = flags.split(/[ |,]+/);
    if (flagParts.length > 1 && !/^[[<]/.test(flagParts[1]))
      shortFlag = flagParts.shift();
    longFlag = flagParts.shift();
    if (!shortFlag && /^-[^-]$/.test(longFlag)) {
      shortFlag = longFlag;
      longFlag = undefined;
    }
    return { shortFlag, longFlag };
  }
  exports.Option = Option;
  exports.DualOptions = DualOptions;
});

// ../../node_modules/.bun/commander@12.1.0/node_modules/commander/lib/suggestSimilar.js
var require_suggestSimilar = __commonJS((exports) => {
  var maxDistance = 3;
  function editDistance(a, b) {
    if (Math.abs(a.length - b.length) > maxDistance)
      return Math.max(a.length, b.length);
    const d = [];
    for (let i = 0;i <= a.length; i++) {
      d[i] = [i];
    }
    for (let j = 0;j <= b.length; j++) {
      d[0][j] = j;
    }
    for (let j = 1;j <= b.length; j++) {
      for (let i = 1;i <= a.length; i++) {
        let cost = 1;
        if (a[i - 1] === b[j - 1]) {
          cost = 0;
        } else {
          cost = 1;
        }
        d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost);
        if (i > 1 && j > 1 && a[i - 1] === b[j - 2] && a[i - 2] === b[j - 1]) {
          d[i][j] = Math.min(d[i][j], d[i - 2][j - 2] + 1);
        }
      }
    }
    return d[a.length][b.length];
  }
  function suggestSimilar(word, candidates) {
    if (!candidates || candidates.length === 0)
      return "";
    candidates = Array.from(new Set(candidates));
    const searchingOptions = word.startsWith("--");
    if (searchingOptions) {
      word = word.slice(2);
      candidates = candidates.map((candidate) => candidate.slice(2));
    }
    let similar = [];
    let bestDistance = maxDistance;
    const minSimilarity = 0.4;
    candidates.forEach((candidate) => {
      if (candidate.length <= 1)
        return;
      const distance = editDistance(word, candidate);
      const length = Math.max(word.length, candidate.length);
      const similarity = (length - distance) / length;
      if (similarity > minSimilarity) {
        if (distance < bestDistance) {
          bestDistance = distance;
          similar = [candidate];
        } else if (distance === bestDistance) {
          similar.push(candidate);
        }
      }
    });
    similar.sort((a, b) => a.localeCompare(b));
    if (searchingOptions) {
      similar = similar.map((candidate) => `--${candidate}`);
    }
    if (similar.length > 1) {
      return `
(Did you mean one of ${similar.join(", ")}?)`;
    }
    if (similar.length === 1) {
      return `
(Did you mean ${similar[0]}?)`;
    }
    return "";
  }
  exports.suggestSimilar = suggestSimilar;
});

// ../../node_modules/.bun/commander@12.1.0/node_modules/commander/lib/command.js
var require_command = __commonJS((exports) => {
  var EventEmitter = __require("events").EventEmitter;
  var childProcess = __require("child_process");
  var path = __require("path");
  var fs = __require("fs");
  var process2 = __require("process");
  var { Argument, humanReadableArgName } = require_argument();
  var { CommanderError } = require_error();
  var { Help } = require_help();
  var { Option, DualOptions } = require_option();
  var { suggestSimilar } = require_suggestSimilar();

  class Command extends EventEmitter {
    constructor(name) {
      super();
      this.commands = [];
      this.options = [];
      this.parent = null;
      this._allowUnknownOption = false;
      this._allowExcessArguments = true;
      this.registeredArguments = [];
      this._args = this.registeredArguments;
      this.args = [];
      this.rawArgs = [];
      this.processedArgs = [];
      this._scriptPath = null;
      this._name = name || "";
      this._optionValues = {};
      this._optionValueSources = {};
      this._storeOptionsAsProperties = false;
      this._actionHandler = null;
      this._executableHandler = false;
      this._executableFile = null;
      this._executableDir = null;
      this._defaultCommandName = null;
      this._exitCallback = null;
      this._aliases = [];
      this._combineFlagAndOptionalValue = true;
      this._description = "";
      this._summary = "";
      this._argsDescription = undefined;
      this._enablePositionalOptions = false;
      this._passThroughOptions = false;
      this._lifeCycleHooks = {};
      this._showHelpAfterError = false;
      this._showSuggestionAfterError = true;
      this._outputConfiguration = {
        writeOut: (str) => process2.stdout.write(str),
        writeErr: (str) => process2.stderr.write(str),
        getOutHelpWidth: () => process2.stdout.isTTY ? process2.stdout.columns : undefined,
        getErrHelpWidth: () => process2.stderr.isTTY ? process2.stderr.columns : undefined,
        outputError: (str, write) => write(str)
      };
      this._hidden = false;
      this._helpOption = undefined;
      this._addImplicitHelpCommand = undefined;
      this._helpCommand = undefined;
      this._helpConfiguration = {};
    }
    copyInheritedSettings(sourceCommand) {
      this._outputConfiguration = sourceCommand._outputConfiguration;
      this._helpOption = sourceCommand._helpOption;
      this._helpCommand = sourceCommand._helpCommand;
      this._helpConfiguration = sourceCommand._helpConfiguration;
      this._exitCallback = sourceCommand._exitCallback;
      this._storeOptionsAsProperties = sourceCommand._storeOptionsAsProperties;
      this._combineFlagAndOptionalValue = sourceCommand._combineFlagAndOptionalValue;
      this._allowExcessArguments = sourceCommand._allowExcessArguments;
      this._enablePositionalOptions = sourceCommand._enablePositionalOptions;
      this._showHelpAfterError = sourceCommand._showHelpAfterError;
      this._showSuggestionAfterError = sourceCommand._showSuggestionAfterError;
      return this;
    }
    _getCommandAndAncestors() {
      const result = [];
      for (let command = this;command; command = command.parent) {
        result.push(command);
      }
      return result;
    }
    command(nameAndArgs, actionOptsOrExecDesc, execOpts) {
      let desc = actionOptsOrExecDesc;
      let opts = execOpts;
      if (typeof desc === "object" && desc !== null) {
        opts = desc;
        desc = null;
      }
      opts = opts || {};
      const [, name, args] = nameAndArgs.match(/([^ ]+) *(.*)/);
      const cmd = this.createCommand(name);
      if (desc) {
        cmd.description(desc);
        cmd._executableHandler = true;
      }
      if (opts.isDefault)
        this._defaultCommandName = cmd._name;
      cmd._hidden = !!(opts.noHelp || opts.hidden);
      cmd._executableFile = opts.executableFile || null;
      if (args)
        cmd.arguments(args);
      this._registerCommand(cmd);
      cmd.parent = this;
      cmd.copyInheritedSettings(this);
      if (desc)
        return this;
      return cmd;
    }
    createCommand(name) {
      return new Command(name);
    }
    createHelp() {
      return Object.assign(new Help, this.configureHelp());
    }
    configureHelp(configuration) {
      if (configuration === undefined)
        return this._helpConfiguration;
      this._helpConfiguration = configuration;
      return this;
    }
    configureOutput(configuration) {
      if (configuration === undefined)
        return this._outputConfiguration;
      Object.assign(this._outputConfiguration, configuration);
      return this;
    }
    showHelpAfterError(displayHelp = true) {
      if (typeof displayHelp !== "string")
        displayHelp = !!displayHelp;
      this._showHelpAfterError = displayHelp;
      return this;
    }
    showSuggestionAfterError(displaySuggestion = true) {
      this._showSuggestionAfterError = !!displaySuggestion;
      return this;
    }
    addCommand(cmd, opts) {
      if (!cmd._name) {
        throw new Error(`Command passed to .addCommand() must have a name
- specify the name in Command constructor or using .name()`);
      }
      opts = opts || {};
      if (opts.isDefault)
        this._defaultCommandName = cmd._name;
      if (opts.noHelp || opts.hidden)
        cmd._hidden = true;
      this._registerCommand(cmd);
      cmd.parent = this;
      cmd._checkForBrokenPassThrough();
      return this;
    }
    createArgument(name, description) {
      return new Argument(name, description);
    }
    argument(name, description, fn, defaultValue) {
      const argument = this.createArgument(name, description);
      if (typeof fn === "function") {
        argument.default(defaultValue).argParser(fn);
      } else {
        argument.default(fn);
      }
      this.addArgument(argument);
      return this;
    }
    arguments(names) {
      names.trim().split(/ +/).forEach((detail) => {
        this.argument(detail);
      });
      return this;
    }
    addArgument(argument) {
      const previousArgument = this.registeredArguments.slice(-1)[0];
      if (previousArgument && previousArgument.variadic) {
        throw new Error(`only the last argument can be variadic '${previousArgument.name()}'`);
      }
      if (argument.required && argument.defaultValue !== undefined && argument.parseArg === undefined) {
        throw new Error(`a default value for a required argument is never used: '${argument.name()}'`);
      }
      this.registeredArguments.push(argument);
      return this;
    }
    helpCommand(enableOrNameAndArgs, description) {
      if (typeof enableOrNameAndArgs === "boolean") {
        this._addImplicitHelpCommand = enableOrNameAndArgs;
        return this;
      }
      enableOrNameAndArgs = enableOrNameAndArgs ?? "help [command]";
      const [, helpName, helpArgs] = enableOrNameAndArgs.match(/([^ ]+) *(.*)/);
      const helpDescription = description ?? "display help for command";
      const helpCommand = this.createCommand(helpName);
      helpCommand.helpOption(false);
      if (helpArgs)
        helpCommand.arguments(helpArgs);
      if (helpDescription)
        helpCommand.description(helpDescription);
      this._addImplicitHelpCommand = true;
      this._helpCommand = helpCommand;
      return this;
    }
    addHelpCommand(helpCommand, deprecatedDescription) {
      if (typeof helpCommand !== "object") {
        this.helpCommand(helpCommand, deprecatedDescription);
        return this;
      }
      this._addImplicitHelpCommand = true;
      this._helpCommand = helpCommand;
      return this;
    }
    _getHelpCommand() {
      const hasImplicitHelpCommand = this._addImplicitHelpCommand ?? (this.commands.length && !this._actionHandler && !this._findCommand("help"));
      if (hasImplicitHelpCommand) {
        if (this._helpCommand === undefined) {
          this.helpCommand(undefined, undefined);
        }
        return this._helpCommand;
      }
      return null;
    }
    hook(event, listener) {
      const allowedValues = ["preSubcommand", "preAction", "postAction"];
      if (!allowedValues.includes(event)) {
        throw new Error(`Unexpected value for event passed to hook : '${event}'.
Expecting one of '${allowedValues.join("', '")}'`);
      }
      if (this._lifeCycleHooks[event]) {
        this._lifeCycleHooks[event].push(listener);
      } else {
        this._lifeCycleHooks[event] = [listener];
      }
      return this;
    }
    exitOverride(fn) {
      if (fn) {
        this._exitCallback = fn;
      } else {
        this._exitCallback = (err) => {
          if (err.code !== "commander.executeSubCommandAsync") {
            throw err;
          } else {}
        };
      }
      return this;
    }
    _exit(exitCode, code, message) {
      if (this._exitCallback) {
        this._exitCallback(new CommanderError(exitCode, code, message));
      }
      process2.exit(exitCode);
    }
    action(fn) {
      const listener = (args) => {
        const expectedArgsCount = this.registeredArguments.length;
        const actionArgs = args.slice(0, expectedArgsCount);
        if (this._storeOptionsAsProperties) {
          actionArgs[expectedArgsCount] = this;
        } else {
          actionArgs[expectedArgsCount] = this.opts();
        }
        actionArgs.push(this);
        return fn.apply(this, actionArgs);
      };
      this._actionHandler = listener;
      return this;
    }
    createOption(flags, description) {
      return new Option(flags, description);
    }
    _callParseArg(target, value, previous, invalidArgumentMessage) {
      try {
        return target.parseArg(value, previous);
      } catch (err) {
        if (err.code === "commander.invalidArgument") {
          const message = `${invalidArgumentMessage} ${err.message}`;
          this.error(message, { exitCode: err.exitCode, code: err.code });
        }
        throw err;
      }
    }
    _registerOption(option) {
      const matchingOption = option.short && this._findOption(option.short) || option.long && this._findOption(option.long);
      if (matchingOption) {
        const matchingFlag = option.long && this._findOption(option.long) ? option.long : option.short;
        throw new Error(`Cannot add option '${option.flags}'${this._name && ` to command '${this._name}'`} due to conflicting flag '${matchingFlag}'
-  already used by option '${matchingOption.flags}'`);
      }
      this.options.push(option);
    }
    _registerCommand(command) {
      const knownBy = (cmd) => {
        return [cmd.name()].concat(cmd.aliases());
      };
      const alreadyUsed = knownBy(command).find((name) => this._findCommand(name));
      if (alreadyUsed) {
        const existingCmd = knownBy(this._findCommand(alreadyUsed)).join("|");
        const newCmd = knownBy(command).join("|");
        throw new Error(`cannot add command '${newCmd}' as already have command '${existingCmd}'`);
      }
      this.commands.push(command);
    }
    addOption(option) {
      this._registerOption(option);
      const oname = option.name();
      const name = option.attributeName();
      if (option.negate) {
        const positiveLongFlag = option.long.replace(/^--no-/, "--");
        if (!this._findOption(positiveLongFlag)) {
          this.setOptionValueWithSource(name, option.defaultValue === undefined ? true : option.defaultValue, "default");
        }
      } else if (option.defaultValue !== undefined) {
        this.setOptionValueWithSource(name, option.defaultValue, "default");
      }
      const handleOptionValue = (val, invalidValueMessage, valueSource) => {
        if (val == null && option.presetArg !== undefined) {
          val = option.presetArg;
        }
        const oldValue = this.getOptionValue(name);
        if (val !== null && option.parseArg) {
          val = this._callParseArg(option, val, oldValue, invalidValueMessage);
        } else if (val !== null && option.variadic) {
          val = option._concatValue(val, oldValue);
        }
        if (val == null) {
          if (option.negate) {
            val = false;
          } else if (option.isBoolean() || option.optional) {
            val = true;
          } else {
            val = "";
          }
        }
        this.setOptionValueWithSource(name, val, valueSource);
      };
      this.on("option:" + oname, (val) => {
        const invalidValueMessage = `error: option '${option.flags}' argument '${val}' is invalid.`;
        handleOptionValue(val, invalidValueMessage, "cli");
      });
      if (option.envVar) {
        this.on("optionEnv:" + oname, (val) => {
          const invalidValueMessage = `error: option '${option.flags}' value '${val}' from env '${option.envVar}' is invalid.`;
          handleOptionValue(val, invalidValueMessage, "env");
        });
      }
      return this;
    }
    _optionEx(config, flags, description, fn, defaultValue) {
      if (typeof flags === "object" && flags instanceof Option) {
        throw new Error("To add an Option object use addOption() instead of option() or requiredOption()");
      }
      const option = this.createOption(flags, description);
      option.makeOptionMandatory(!!config.mandatory);
      if (typeof fn === "function") {
        option.default(defaultValue).argParser(fn);
      } else if (fn instanceof RegExp) {
        const regex = fn;
        fn = (val, def) => {
          const m = regex.exec(val);
          return m ? m[0] : def;
        };
        option.default(defaultValue).argParser(fn);
      } else {
        option.default(fn);
      }
      return this.addOption(option);
    }
    option(flags, description, parseArg, defaultValue) {
      return this._optionEx({}, flags, description, parseArg, defaultValue);
    }
    requiredOption(flags, description, parseArg, defaultValue) {
      return this._optionEx({ mandatory: true }, flags, description, parseArg, defaultValue);
    }
    combineFlagAndOptionalValue(combine = true) {
      this._combineFlagAndOptionalValue = !!combine;
      return this;
    }
    allowUnknownOption(allowUnknown = true) {
      this._allowUnknownOption = !!allowUnknown;
      return this;
    }
    allowExcessArguments(allowExcess = true) {
      this._allowExcessArguments = !!allowExcess;
      return this;
    }
    enablePositionalOptions(positional = true) {
      this._enablePositionalOptions = !!positional;
      return this;
    }
    passThroughOptions(passThrough = true) {
      this._passThroughOptions = !!passThrough;
      this._checkForBrokenPassThrough();
      return this;
    }
    _checkForBrokenPassThrough() {
      if (this.parent && this._passThroughOptions && !this.parent._enablePositionalOptions) {
        throw new Error(`passThroughOptions cannot be used for '${this._name}' without turning on enablePositionalOptions for parent command(s)`);
      }
    }
    storeOptionsAsProperties(storeAsProperties = true) {
      if (this.options.length) {
        throw new Error("call .storeOptionsAsProperties() before adding options");
      }
      if (Object.keys(this._optionValues).length) {
        throw new Error("call .storeOptionsAsProperties() before setting option values");
      }
      this._storeOptionsAsProperties = !!storeAsProperties;
      return this;
    }
    getOptionValue(key) {
      if (this._storeOptionsAsProperties) {
        return this[key];
      }
      return this._optionValues[key];
    }
    setOptionValue(key, value) {
      return this.setOptionValueWithSource(key, value, undefined);
    }
    setOptionValueWithSource(key, value, source) {
      if (this._storeOptionsAsProperties) {
        this[key] = value;
      } else {
        this._optionValues[key] = value;
      }
      this._optionValueSources[key] = source;
      return this;
    }
    getOptionValueSource(key) {
      return this._optionValueSources[key];
    }
    getOptionValueSourceWithGlobals(key) {
      let source;
      this._getCommandAndAncestors().forEach((cmd) => {
        if (cmd.getOptionValueSource(key) !== undefined) {
          source = cmd.getOptionValueSource(key);
        }
      });
      return source;
    }
    _prepareUserArgs(argv, parseOptions) {
      if (argv !== undefined && !Array.isArray(argv)) {
        throw new Error("first parameter to parse must be array or undefined");
      }
      parseOptions = parseOptions || {};
      if (argv === undefined && parseOptions.from === undefined) {
        if (process2.versions?.electron) {
          parseOptions.from = "electron";
        }
        const execArgv = process2.execArgv ?? [];
        if (execArgv.includes("-e") || execArgv.includes("--eval") || execArgv.includes("-p") || execArgv.includes("--print")) {
          parseOptions.from = "eval";
        }
      }
      if (argv === undefined) {
        argv = process2.argv;
      }
      this.rawArgs = argv.slice();
      let userArgs;
      switch (parseOptions.from) {
        case undefined:
        case "node":
          this._scriptPath = argv[1];
          userArgs = argv.slice(2);
          break;
        case "electron":
          if (process2.defaultApp) {
            this._scriptPath = argv[1];
            userArgs = argv.slice(2);
          } else {
            userArgs = argv.slice(1);
          }
          break;
        case "user":
          userArgs = argv.slice(0);
          break;
        case "eval":
          userArgs = argv.slice(1);
          break;
        default:
          throw new Error(`unexpected parse option { from: '${parseOptions.from}' }`);
      }
      if (!this._name && this._scriptPath)
        this.nameFromFilename(this._scriptPath);
      this._name = this._name || "program";
      return userArgs;
    }
    parse(argv, parseOptions) {
      const userArgs = this._prepareUserArgs(argv, parseOptions);
      this._parseCommand([], userArgs);
      return this;
    }
    async parseAsync(argv, parseOptions) {
      const userArgs = this._prepareUserArgs(argv, parseOptions);
      await this._parseCommand([], userArgs);
      return this;
    }
    _executeSubCommand(subcommand, args) {
      args = args.slice();
      let launchWithNode = false;
      const sourceExt = [".js", ".ts", ".tsx", ".mjs", ".cjs"];
      function findFile(baseDir, baseName) {
        const localBin = path.resolve(baseDir, baseName);
        if (fs.existsSync(localBin))
          return localBin;
        if (sourceExt.includes(path.extname(baseName)))
          return;
        const foundExt = sourceExt.find((ext) => fs.existsSync(`${localBin}${ext}`));
        if (foundExt)
          return `${localBin}${foundExt}`;
        return;
      }
      this._checkForMissingMandatoryOptions();
      this._checkForConflictingOptions();
      let executableFile = subcommand._executableFile || `${this._name}-${subcommand._name}`;
      let executableDir = this._executableDir || "";
      if (this._scriptPath) {
        let resolvedScriptPath;
        try {
          resolvedScriptPath = fs.realpathSync(this._scriptPath);
        } catch (err) {
          resolvedScriptPath = this._scriptPath;
        }
        executableDir = path.resolve(path.dirname(resolvedScriptPath), executableDir);
      }
      if (executableDir) {
        let localFile = findFile(executableDir, executableFile);
        if (!localFile && !subcommand._executableFile && this._scriptPath) {
          const legacyName = path.basename(this._scriptPath, path.extname(this._scriptPath));
          if (legacyName !== this._name) {
            localFile = findFile(executableDir, `${legacyName}-${subcommand._name}`);
          }
        }
        executableFile = localFile || executableFile;
      }
      launchWithNode = sourceExt.includes(path.extname(executableFile));
      let proc;
      if (process2.platform !== "win32") {
        if (launchWithNode) {
          args.unshift(executableFile);
          args = incrementNodeInspectorPort(process2.execArgv).concat(args);
          proc = childProcess.spawn(process2.argv[0], args, { stdio: "inherit" });
        } else {
          proc = childProcess.spawn(executableFile, args, { stdio: "inherit" });
        }
      } else {
        args.unshift(executableFile);
        args = incrementNodeInspectorPort(process2.execArgv).concat(args);
        proc = childProcess.spawn(process2.execPath, args, { stdio: "inherit" });
      }
      if (!proc.killed) {
        const signals = ["SIGUSR1", "SIGUSR2", "SIGTERM", "SIGINT", "SIGHUP"];
        signals.forEach((signal) => {
          process2.on(signal, () => {
            if (proc.killed === false && proc.exitCode === null) {
              proc.kill(signal);
            }
          });
        });
      }
      const exitCallback = this._exitCallback;
      proc.on("close", (code) => {
        code = code ?? 1;
        if (!exitCallback) {
          process2.exit(code);
        } else {
          exitCallback(new CommanderError(code, "commander.executeSubCommandAsync", "(close)"));
        }
      });
      proc.on("error", (err) => {
        if (err.code === "ENOENT") {
          const executableDirMessage = executableDir ? `searched for local subcommand relative to directory '${executableDir}'` : "no directory for search for local subcommand, use .executableDir() to supply a custom directory";
          const executableMissing = `'${executableFile}' does not exist
 - if '${subcommand._name}' is not meant to be an executable command, remove description parameter from '.command()' and use '.description()' instead
 - if the default executable name is not suitable, use the executableFile option to supply a custom name or path
 - ${executableDirMessage}`;
          throw new Error(executableMissing);
        } else if (err.code === "EACCES") {
          throw new Error(`'${executableFile}' not executable`);
        }
        if (!exitCallback) {
          process2.exit(1);
        } else {
          const wrappedError = new CommanderError(1, "commander.executeSubCommandAsync", "(error)");
          wrappedError.nestedError = err;
          exitCallback(wrappedError);
        }
      });
      this.runningCommand = proc;
    }
    _dispatchSubcommand(commandName, operands, unknown) {
      const subCommand = this._findCommand(commandName);
      if (!subCommand)
        this.help({ error: true });
      let promiseChain;
      promiseChain = this._chainOrCallSubCommandHook(promiseChain, subCommand, "preSubcommand");
      promiseChain = this._chainOrCall(promiseChain, () => {
        if (subCommand._executableHandler) {
          this._executeSubCommand(subCommand, operands.concat(unknown));
        } else {
          return subCommand._parseCommand(operands, unknown);
        }
      });
      return promiseChain;
    }
    _dispatchHelpCommand(subcommandName) {
      if (!subcommandName) {
        this.help();
      }
      const subCommand = this._findCommand(subcommandName);
      if (subCommand && !subCommand._executableHandler) {
        subCommand.help();
      }
      return this._dispatchSubcommand(subcommandName, [], [this._getHelpOption()?.long ?? this._getHelpOption()?.short ?? "--help"]);
    }
    _checkNumberOfArguments() {
      this.registeredArguments.forEach((arg, i) => {
        if (arg.required && this.args[i] == null) {
          this.missingArgument(arg.name());
        }
      });
      if (this.registeredArguments.length > 0 && this.registeredArguments[this.registeredArguments.length - 1].variadic) {
        return;
      }
      if (this.args.length > this.registeredArguments.length) {
        this._excessArguments(this.args);
      }
    }
    _processArguments() {
      const myParseArg = (argument, value, previous) => {
        let parsedValue = value;
        if (value !== null && argument.parseArg) {
          const invalidValueMessage = `error: command-argument value '${value}' is invalid for argument '${argument.name()}'.`;
          parsedValue = this._callParseArg(argument, value, previous, invalidValueMessage);
        }
        return parsedValue;
      };
      this._checkNumberOfArguments();
      const processedArgs = [];
      this.registeredArguments.forEach((declaredArg, index) => {
        let value = declaredArg.defaultValue;
        if (declaredArg.variadic) {
          if (index < this.args.length) {
            value = this.args.slice(index);
            if (declaredArg.parseArg) {
              value = value.reduce((processed, v) => {
                return myParseArg(declaredArg, v, processed);
              }, declaredArg.defaultValue);
            }
          } else if (value === undefined) {
            value = [];
          }
        } else if (index < this.args.length) {
          value = this.args[index];
          if (declaredArg.parseArg) {
            value = myParseArg(declaredArg, value, declaredArg.defaultValue);
          }
        }
        processedArgs[index] = value;
      });
      this.processedArgs = processedArgs;
    }
    _chainOrCall(promise, fn) {
      if (promise && promise.then && typeof promise.then === "function") {
        return promise.then(() => fn());
      }
      return fn();
    }
    _chainOrCallHooks(promise, event) {
      let result = promise;
      const hooks = [];
      this._getCommandAndAncestors().reverse().filter((cmd) => cmd._lifeCycleHooks[event] !== undefined).forEach((hookedCommand) => {
        hookedCommand._lifeCycleHooks[event].forEach((callback) => {
          hooks.push({ hookedCommand, callback });
        });
      });
      if (event === "postAction") {
        hooks.reverse();
      }
      hooks.forEach((hookDetail) => {
        result = this._chainOrCall(result, () => {
          return hookDetail.callback(hookDetail.hookedCommand, this);
        });
      });
      return result;
    }
    _chainOrCallSubCommandHook(promise, subCommand, event) {
      let result = promise;
      if (this._lifeCycleHooks[event] !== undefined) {
        this._lifeCycleHooks[event].forEach((hook) => {
          result = this._chainOrCall(result, () => {
            return hook(this, subCommand);
          });
        });
      }
      return result;
    }
    _parseCommand(operands, unknown) {
      const parsed = this.parseOptions(unknown);
      this._parseOptionsEnv();
      this._parseOptionsImplied();
      operands = operands.concat(parsed.operands);
      unknown = parsed.unknown;
      this.args = operands.concat(unknown);
      if (operands && this._findCommand(operands[0])) {
        return this._dispatchSubcommand(operands[0], operands.slice(1), unknown);
      }
      if (this._getHelpCommand() && operands[0] === this._getHelpCommand().name()) {
        return this._dispatchHelpCommand(operands[1]);
      }
      if (this._defaultCommandName) {
        this._outputHelpIfRequested(unknown);
        return this._dispatchSubcommand(this._defaultCommandName, operands, unknown);
      }
      if (this.commands.length && this.args.length === 0 && !this._actionHandler && !this._defaultCommandName) {
        this.help({ error: true });
      }
      this._outputHelpIfRequested(parsed.unknown);
      this._checkForMissingMandatoryOptions();
      this._checkForConflictingOptions();
      const checkForUnknownOptions = () => {
        if (parsed.unknown.length > 0) {
          this.unknownOption(parsed.unknown[0]);
        }
      };
      const commandEvent = `command:${this.name()}`;
      if (this._actionHandler) {
        checkForUnknownOptions();
        this._processArguments();
        let promiseChain;
        promiseChain = this._chainOrCallHooks(promiseChain, "preAction");
        promiseChain = this._chainOrCall(promiseChain, () => this._actionHandler(this.processedArgs));
        if (this.parent) {
          promiseChain = this._chainOrCall(promiseChain, () => {
            this.parent.emit(commandEvent, operands, unknown);
          });
        }
        promiseChain = this._chainOrCallHooks(promiseChain, "postAction");
        return promiseChain;
      }
      if (this.parent && this.parent.listenerCount(commandEvent)) {
        checkForUnknownOptions();
        this._processArguments();
        this.parent.emit(commandEvent, operands, unknown);
      } else if (operands.length) {
        if (this._findCommand("*")) {
          return this._dispatchSubcommand("*", operands, unknown);
        }
        if (this.listenerCount("command:*")) {
          this.emit("command:*", operands, unknown);
        } else if (this.commands.length) {
          this.unknownCommand();
        } else {
          checkForUnknownOptions();
          this._processArguments();
        }
      } else if (this.commands.length) {
        checkForUnknownOptions();
        this.help({ error: true });
      } else {
        checkForUnknownOptions();
        this._processArguments();
      }
    }
    _findCommand(name) {
      if (!name)
        return;
      return this.commands.find((cmd) => cmd._name === name || cmd._aliases.includes(name));
    }
    _findOption(arg) {
      return this.options.find((option) => option.is(arg));
    }
    _checkForMissingMandatoryOptions() {
      this._getCommandAndAncestors().forEach((cmd) => {
        cmd.options.forEach((anOption) => {
          if (anOption.mandatory && cmd.getOptionValue(anOption.attributeName()) === undefined) {
            cmd.missingMandatoryOptionValue(anOption);
          }
        });
      });
    }
    _checkForConflictingLocalOptions() {
      const definedNonDefaultOptions = this.options.filter((option) => {
        const optionKey = option.attributeName();
        if (this.getOptionValue(optionKey) === undefined) {
          return false;
        }
        return this.getOptionValueSource(optionKey) !== "default";
      });
      const optionsWithConflicting = definedNonDefaultOptions.filter((option) => option.conflictsWith.length > 0);
      optionsWithConflicting.forEach((option) => {
        const conflictingAndDefined = definedNonDefaultOptions.find((defined) => option.conflictsWith.includes(defined.attributeName()));
        if (conflictingAndDefined) {
          this._conflictingOption(option, conflictingAndDefined);
        }
      });
    }
    _checkForConflictingOptions() {
      this._getCommandAndAncestors().forEach((cmd) => {
        cmd._checkForConflictingLocalOptions();
      });
    }
    parseOptions(argv) {
      const operands = [];
      const unknown = [];
      let dest = operands;
      const args = argv.slice();
      function maybeOption(arg) {
        return arg.length > 1 && arg[0] === "-";
      }
      let activeVariadicOption = null;
      while (args.length) {
        const arg = args.shift();
        if (arg === "--") {
          if (dest === unknown)
            dest.push(arg);
          dest.push(...args);
          break;
        }
        if (activeVariadicOption && !maybeOption(arg)) {
          this.emit(`option:${activeVariadicOption.name()}`, arg);
          continue;
        }
        activeVariadicOption = null;
        if (maybeOption(arg)) {
          const option = this._findOption(arg);
          if (option) {
            if (option.required) {
              const value = args.shift();
              if (value === undefined)
                this.optionMissingArgument(option);
              this.emit(`option:${option.name()}`, value);
            } else if (option.optional) {
              let value = null;
              if (args.length > 0 && !maybeOption(args[0])) {
                value = args.shift();
              }
              this.emit(`option:${option.name()}`, value);
            } else {
              this.emit(`option:${option.name()}`);
            }
            activeVariadicOption = option.variadic ? option : null;
            continue;
          }
        }
        if (arg.length > 2 && arg[0] === "-" && arg[1] !== "-") {
          const option = this._findOption(`-${arg[1]}`);
          if (option) {
            if (option.required || option.optional && this._combineFlagAndOptionalValue) {
              this.emit(`option:${option.name()}`, arg.slice(2));
            } else {
              this.emit(`option:${option.name()}`);
              args.unshift(`-${arg.slice(2)}`);
            }
            continue;
          }
        }
        if (/^--[^=]+=/.test(arg)) {
          const index = arg.indexOf("=");
          const option = this._findOption(arg.slice(0, index));
          if (option && (option.required || option.optional)) {
            this.emit(`option:${option.name()}`, arg.slice(index + 1));
            continue;
          }
        }
        if (maybeOption(arg)) {
          dest = unknown;
        }
        if ((this._enablePositionalOptions || this._passThroughOptions) && operands.length === 0 && unknown.length === 0) {
          if (this._findCommand(arg)) {
            operands.push(arg);
            if (args.length > 0)
              unknown.push(...args);
            break;
          } else if (this._getHelpCommand() && arg === this._getHelpCommand().name()) {
            operands.push(arg);
            if (args.length > 0)
              operands.push(...args);
            break;
          } else if (this._defaultCommandName) {
            unknown.push(arg);
            if (args.length > 0)
              unknown.push(...args);
            break;
          }
        }
        if (this._passThroughOptions) {
          dest.push(arg);
          if (args.length > 0)
            dest.push(...args);
          break;
        }
        dest.push(arg);
      }
      return { operands, unknown };
    }
    opts() {
      if (this._storeOptionsAsProperties) {
        const result = {};
        const len = this.options.length;
        for (let i = 0;i < len; i++) {
          const key = this.options[i].attributeName();
          result[key] = key === this._versionOptionName ? this._version : this[key];
        }
        return result;
      }
      return this._optionValues;
    }
    optsWithGlobals() {
      return this._getCommandAndAncestors().reduce((combinedOptions, cmd) => Object.assign(combinedOptions, cmd.opts()), {});
    }
    error(message, errorOptions) {
      this._outputConfiguration.outputError(`${message}
`, this._outputConfiguration.writeErr);
      if (typeof this._showHelpAfterError === "string") {
        this._outputConfiguration.writeErr(`${this._showHelpAfterError}
`);
      } else if (this._showHelpAfterError) {
        this._outputConfiguration.writeErr(`
`);
        this.outputHelp({ error: true });
      }
      const config = errorOptions || {};
      const exitCode = config.exitCode || 1;
      const code = config.code || "commander.error";
      this._exit(exitCode, code, message);
    }
    _parseOptionsEnv() {
      this.options.forEach((option) => {
        if (option.envVar && option.envVar in process2.env) {
          const optionKey = option.attributeName();
          if (this.getOptionValue(optionKey) === undefined || ["default", "config", "env"].includes(this.getOptionValueSource(optionKey))) {
            if (option.required || option.optional) {
              this.emit(`optionEnv:${option.name()}`, process2.env[option.envVar]);
            } else {
              this.emit(`optionEnv:${option.name()}`);
            }
          }
        }
      });
    }
    _parseOptionsImplied() {
      const dualHelper = new DualOptions(this.options);
      const hasCustomOptionValue = (optionKey) => {
        return this.getOptionValue(optionKey) !== undefined && !["default", "implied"].includes(this.getOptionValueSource(optionKey));
      };
      this.options.filter((option) => option.implied !== undefined && hasCustomOptionValue(option.attributeName()) && dualHelper.valueFromOption(this.getOptionValue(option.attributeName()), option)).forEach((option) => {
        Object.keys(option.implied).filter((impliedKey) => !hasCustomOptionValue(impliedKey)).forEach((impliedKey) => {
          this.setOptionValueWithSource(impliedKey, option.implied[impliedKey], "implied");
        });
      });
    }
    missingArgument(name) {
      const message = `error: missing required argument '${name}'`;
      this.error(message, { code: "commander.missingArgument" });
    }
    optionMissingArgument(option) {
      const message = `error: option '${option.flags}' argument missing`;
      this.error(message, { code: "commander.optionMissingArgument" });
    }
    missingMandatoryOptionValue(option) {
      const message = `error: required option '${option.flags}' not specified`;
      this.error(message, { code: "commander.missingMandatoryOptionValue" });
    }
    _conflictingOption(option, conflictingOption) {
      const findBestOptionFromValue = (option2) => {
        const optionKey = option2.attributeName();
        const optionValue = this.getOptionValue(optionKey);
        const negativeOption = this.options.find((target) => target.negate && optionKey === target.attributeName());
        const positiveOption = this.options.find((target) => !target.negate && optionKey === target.attributeName());
        if (negativeOption && (negativeOption.presetArg === undefined && optionValue === false || negativeOption.presetArg !== undefined && optionValue === negativeOption.presetArg)) {
          return negativeOption;
        }
        return positiveOption || option2;
      };
      const getErrorMessage = (option2) => {
        const bestOption = findBestOptionFromValue(option2);
        const optionKey = bestOption.attributeName();
        const source = this.getOptionValueSource(optionKey);
        if (source === "env") {
          return `environment variable '${bestOption.envVar}'`;
        }
        return `option '${bestOption.flags}'`;
      };
      const message = `error: ${getErrorMessage(option)} cannot be used with ${getErrorMessage(conflictingOption)}`;
      this.error(message, { code: "commander.conflictingOption" });
    }
    unknownOption(flag) {
      if (this._allowUnknownOption)
        return;
      let suggestion = "";
      if (flag.startsWith("--") && this._showSuggestionAfterError) {
        let candidateFlags = [];
        let command = this;
        do {
          const moreFlags = command.createHelp().visibleOptions(command).filter((option) => option.long).map((option) => option.long);
          candidateFlags = candidateFlags.concat(moreFlags);
          command = command.parent;
        } while (command && !command._enablePositionalOptions);
        suggestion = suggestSimilar(flag, candidateFlags);
      }
      const message = `error: unknown option '${flag}'${suggestion}`;
      this.error(message, { code: "commander.unknownOption" });
    }
    _excessArguments(receivedArgs) {
      if (this._allowExcessArguments)
        return;
      const expected = this.registeredArguments.length;
      const s = expected === 1 ? "" : "s";
      const forSubcommand = this.parent ? ` for '${this.name()}'` : "";
      const message = `error: too many arguments${forSubcommand}. Expected ${expected} argument${s} but got ${receivedArgs.length}.`;
      this.error(message, { code: "commander.excessArguments" });
    }
    unknownCommand() {
      const unknownName = this.args[0];
      let suggestion = "";
      if (this._showSuggestionAfterError) {
        const candidateNames = [];
        this.createHelp().visibleCommands(this).forEach((command) => {
          candidateNames.push(command.name());
          if (command.alias())
            candidateNames.push(command.alias());
        });
        suggestion = suggestSimilar(unknownName, candidateNames);
      }
      const message = `error: unknown command '${unknownName}'${suggestion}`;
      this.error(message, { code: "commander.unknownCommand" });
    }
    version(str, flags, description) {
      if (str === undefined)
        return this._version;
      this._version = str;
      flags = flags || "-V, --version";
      description = description || "output the version number";
      const versionOption = this.createOption(flags, description);
      this._versionOptionName = versionOption.attributeName();
      this._registerOption(versionOption);
      this.on("option:" + versionOption.name(), () => {
        this._outputConfiguration.writeOut(`${str}
`);
        this._exit(0, "commander.version", str);
      });
      return this;
    }
    description(str, argsDescription) {
      if (str === undefined && argsDescription === undefined)
        return this._description;
      this._description = str;
      if (argsDescription) {
        this._argsDescription = argsDescription;
      }
      return this;
    }
    summary(str) {
      if (str === undefined)
        return this._summary;
      this._summary = str;
      return this;
    }
    alias(alias) {
      if (alias === undefined)
        return this._aliases[0];
      let command = this;
      if (this.commands.length !== 0 && this.commands[this.commands.length - 1]._executableHandler) {
        command = this.commands[this.commands.length - 1];
      }
      if (alias === command._name)
        throw new Error("Command alias can't be the same as its name");
      const matchingCommand = this.parent?._findCommand(alias);
      if (matchingCommand) {
        const existingCmd = [matchingCommand.name()].concat(matchingCommand.aliases()).join("|");
        throw new Error(`cannot add alias '${alias}' to command '${this.name()}' as already have command '${existingCmd}'`);
      }
      command._aliases.push(alias);
      return this;
    }
    aliases(aliases) {
      if (aliases === undefined)
        return this._aliases;
      aliases.forEach((alias) => this.alias(alias));
      return this;
    }
    usage(str) {
      if (str === undefined) {
        if (this._usage)
          return this._usage;
        const args = this.registeredArguments.map((arg) => {
          return humanReadableArgName(arg);
        });
        return [].concat(this.options.length || this._helpOption !== null ? "[options]" : [], this.commands.length ? "[command]" : [], this.registeredArguments.length ? args : []).join(" ");
      }
      this._usage = str;
      return this;
    }
    name(str) {
      if (str === undefined)
        return this._name;
      this._name = str;
      return this;
    }
    nameFromFilename(filename) {
      this._name = path.basename(filename, path.extname(filename));
      return this;
    }
    executableDir(path2) {
      if (path2 === undefined)
        return this._executableDir;
      this._executableDir = path2;
      return this;
    }
    helpInformation(contextOptions) {
      const helper2 = this.createHelp();
      if (helper2.helpWidth === undefined) {
        helper2.helpWidth = contextOptions && contextOptions.error ? this._outputConfiguration.getErrHelpWidth() : this._outputConfiguration.getOutHelpWidth();
      }
      return helper2.formatHelp(this, helper2);
    }
    _getHelpContext(contextOptions) {
      contextOptions = contextOptions || {};
      const context = { error: !!contextOptions.error };
      let write;
      if (context.error) {
        write = (arg) => this._outputConfiguration.writeErr(arg);
      } else {
        write = (arg) => this._outputConfiguration.writeOut(arg);
      }
      context.write = contextOptions.write || write;
      context.command = this;
      return context;
    }
    outputHelp(contextOptions) {
      let deprecatedCallback;
      if (typeof contextOptions === "function") {
        deprecatedCallback = contextOptions;
        contextOptions = undefined;
      }
      const context = this._getHelpContext(contextOptions);
      this._getCommandAndAncestors().reverse().forEach((command) => command.emit("beforeAllHelp", context));
      this.emit("beforeHelp", context);
      let helpInformation = this.helpInformation(context);
      if (deprecatedCallback) {
        helpInformation = deprecatedCallback(helpInformation);
        if (typeof helpInformation !== "string" && !Buffer.isBuffer(helpInformation)) {
          throw new Error("outputHelp callback must return a string or a Buffer");
        }
      }
      context.write(helpInformation);
      if (this._getHelpOption()?.long) {
        this.emit(this._getHelpOption().long);
      }
      this.emit("afterHelp", context);
      this._getCommandAndAncestors().forEach((command) => command.emit("afterAllHelp", context));
    }
    helpOption(flags, description) {
      if (typeof flags === "boolean") {
        if (flags) {
          this._helpOption = this._helpOption ?? undefined;
        } else {
          this._helpOption = null;
        }
        return this;
      }
      flags = flags ?? "-h, --help";
      description = description ?? "display help for command";
      this._helpOption = this.createOption(flags, description);
      return this;
    }
    _getHelpOption() {
      if (this._helpOption === undefined) {
        this.helpOption(undefined, undefined);
      }
      return this._helpOption;
    }
    addHelpOption(option) {
      this._helpOption = option;
      return this;
    }
    help(contextOptions) {
      this.outputHelp(contextOptions);
      let exitCode = process2.exitCode || 0;
      if (exitCode === 0 && contextOptions && typeof contextOptions !== "function" && contextOptions.error) {
        exitCode = 1;
      }
      this._exit(exitCode, "commander.help", "(outputHelp)");
    }
    addHelpText(position, text) {
      const allowedValues = ["beforeAll", "before", "after", "afterAll"];
      if (!allowedValues.includes(position)) {
        throw new Error(`Unexpected value for position to addHelpText.
Expecting one of '${allowedValues.join("', '")}'`);
      }
      const helpEvent = `${position}Help`;
      this.on(helpEvent, (context) => {
        let helpStr;
        if (typeof text === "function") {
          helpStr = text({ error: context.error, command: context.command });
        } else {
          helpStr = text;
        }
        if (helpStr) {
          context.write(`${helpStr}
`);
        }
      });
      return this;
    }
    _outputHelpIfRequested(args) {
      const helpOption = this._getHelpOption();
      const helpRequested = helpOption && args.find((arg) => helpOption.is(arg));
      if (helpRequested) {
        this.outputHelp();
        this._exit(0, "commander.helpDisplayed", "(outputHelp)");
      }
    }
  }
  function incrementNodeInspectorPort(args) {
    return args.map((arg) => {
      if (!arg.startsWith("--inspect")) {
        return arg;
      }
      let debugOption;
      let debugHost = "127.0.0.1";
      let debugPort = "9229";
      let match;
      if ((match = arg.match(/^(--inspect(-brk)?)$/)) !== null) {
        debugOption = match[1];
      } else if ((match = arg.match(/^(--inspect(-brk|-port)?)=([^:]+)$/)) !== null) {
        debugOption = match[1];
        if (/^\d+$/.test(match[3])) {
          debugPort = match[3];
        } else {
          debugHost = match[3];
        }
      } else if ((match = arg.match(/^(--inspect(-brk|-port)?)=([^:]+):(\d+)$/)) !== null) {
        debugOption = match[1];
        debugHost = match[3];
        debugPort = match[4];
      }
      if (debugOption && debugPort !== "0") {
        return `${debugOption}=${debugHost}:${parseInt(debugPort) + 1}`;
      }
      return arg;
    });
  }
  exports.Command = Command;
});

// ../../node_modules/.bun/commander@12.1.0/node_modules/commander/index.js
var require_commander = __commonJS((exports) => {
  var { Argument } = require_argument();
  var { Command } = require_command();
  var { CommanderError, InvalidArgumentError } = require_error();
  var { Help } = require_help();
  var { Option } = require_option();
  exports.program = new Command;
  exports.createCommand = (name) => new Command(name);
  exports.createOption = (flags, description) => new Option(flags, description);
  exports.createArgument = (name, description) => new Argument(name, description);
  exports.Command = Command;
  exports.Option = Option;
  exports.Argument = Argument;
  exports.Help = Help;
  exports.CommanderError = CommanderError;
  exports.InvalidArgumentError = InvalidArgumentError;
  exports.InvalidOptionArgumentError = InvalidArgumentError;
});

// src/index.ts
import fs4 from "fs";
import path4 from "path";

// ../../node_modules/.bun/commander@12.1.0/node_modules/commander/esm.mjs
var import__ = __toESM(require_commander(), 1);
var {
  program,
  createCommand,
  createArgument,
  createOption,
  CommanderError,
  InvalidArgumentError,
  InvalidOptionArgumentError,
  Command,
  Argument,
  Option,
  Help
} = import__.default;

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
var parseFloat2 = (just, nothing, s) => {
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
var _parseFloat = ($a0) => ($a1) => ($a2) => parseFloat2($a0, $a1, $a2);
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
var stringLt2 = (a, b) => a < b;
var stringGt2 = (a, b) => a > b;
var parseInt3 = (just, nothing, s) => {
  const n = Number.parseInt(s, 10);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};
var parseFloat3 = (just, nothing, s) => {
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
var _PIPE_PIPE5 = (a) => (b) => a || b();
var _append2 = ($a0) => ($a1) => stringAppend2($a0, $a1);
var _eq4 = ($a0) => ($a1) => stringEq2($a0, $a1);
var _lt4 = ($a0) => ($a1) => stringLt2($a0, $a1);
var _gt4 = ($a0) => ($a1) => stringGt2($a0, $a1);
var _parseInt2 = ($a0) => ($a1) => ($a2) => parseInt3($a0, $a1, $a2);
var _parseFloat2 = ($a0) => ($a1) => ($a2) => parseFloat3($a0, $a1, $a2);
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
var $default_Ord_String__LT_EQ = (x) => (y) => _PIPE_PIPE5(_lt4(x)(y))(() => $dict_Eq_String2._EQ_EQ(x)(y));
var $default_Ord_String__GT_EQ = (x) => (y) => _PIPE_PIPE5(_gt4(x)(y))(() => $dict_Eq_String2._EQ_EQ(x)(y));
var $dict_Eq_String2 = {
  _EQ_EQ: _eq4,
  _SLASH_EQ: $default_Eq_String__SLASH_EQ2
};
var $dict_Ord_String2 = {
  _LT: _lt4,
  _GT: _gt4,
  _LT_EQ: $default_Ord_String__LT_EQ,
  _GT_EQ: $default_Ord_String__GT_EQ
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
var _PIPE_PIPE6 = (a) => (b) => a || b();
var _AMP_AMP3 = (a) => (b) => a && b();
var _toString6 = ($a0) => charToString2($a0);
var _lt5 = ($a0) => ($a1) => charOrd2($a0, $a1);
var _gt5 = ($a0) => ($a1) => charOrdGt2($a0, $a1);
var toCode2 = ($a0) => charToCode2($a0);
var $default_Ord_Char__LT_EQ2 = (x) => (y) => _PIPE_PIPE6(_lt5(x)(y))(() => $dict_Eq_Char2._EQ_EQ(x)(y));
var $default_Ord_Char__GT_EQ2 = (x) => (y) => _PIPE_PIPE6(_gt5(x)(y))(() => $dict_Eq_Char2._EQ_EQ(x)(y));
var $dict_Ord_Char2 = {
  _LT: _lt5,
  _GT: _gt5,
  _LT_EQ: $default_Ord_Char__LT_EQ2,
  _GT_EQ: $default_Ord_Char__GT_EQ2
};
var isUpper2 = (c) => _AMP_AMP3($dict_Ord_Char2._GT_EQ(c)("A"))(() => $dict_Ord_Char2._LT_EQ(c)("Z"));
var isLower2 = (c) => _AMP_AMP3($dict_Ord_Char2._GT_EQ(c)("a"))(() => $dict_Ord_Char2._LT_EQ(c)("z"));
var isAlpha2 = (c) => _PIPE_PIPE6(isUpper2(c))(() => isLower2(c));
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
// ../vibe-parser/dist/Vibe/Vibe/Dict.js
var _AMP_AMP5 = (a) => (b) => a && b();
var RBNode = ($0) => ($1) => ($2) => ($3) => ($4) => ({ $tag: 1, $0, $1, $2, $3, $4 });
var empty = { $tag: 0 };
var get = ($dict_Ord) => (targetKey) => (dict) => (($match_0) => {
  if ($match_0.$tag === 0) {
    return Nothing2;
  }
  if ($match_0.$tag === 1) {
    const key = $match_0.$1;
    const value = $match_0.$2;
    const left = $match_0.$3;
    const right = $match_0.$4;
    return $dict_Ord._LT(targetKey)(key) ? get($dict_Ord)(targetKey)(left) : $dict_Ord._GT(targetKey)(key) ? get($dict_Ord)(targetKey)(right) : Just2(value);
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
var toList3 = (dict) => _foldr((key) => (value) => (list2) => _COLON_COLON2([key, value])(list2))([])(dict);
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
var $impl_Eq_NColor__EQ_EQ = (x_impl) => (y_impl) => (($match_37) => {
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
})([x_impl, y_impl]);
var $dict_Eq_NColor = {
  _EQ_EQ: $impl_Eq_NColor__EQ_EQ
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
    return _AMP_AMP5(_AMP_AMP5(_AMP_AMP5(_AMP_AMP5($dict_Eq_NColor._EQ_EQ(a_0)(b_0))(() => $dict_Eq._EQ_EQ(a_1)(b_1)))(() => $dict_Eq._EQ_EQ(a_2)(b_2)))(() => $dict_Eq._EQ_EQ(a_3)(b_3)))(() => $dict_Eq._EQ_EQ(a_4)(b_4));
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
var emptyRegistry = empty;
var defaultOperatorInfo = { precedence: 9, associativity: AssocLeft };
var builtinRegistry = _PIPE_GT2(_PIPE_GT2(empty)(insert($dict_Ord_String2)("&&")({ precedence: 3, associativity: AssocRight })))(insert($dict_Ord_String2)("||")({ precedence: 2, associativity: AssocRight }));
var getOperatorInfo = (op) => (registry) => (($match_0) => {
  if ($match_0.$tag === 0) {
    const info = $match_0.$0;
    return info;
  }
  if ($match_0.$tag === 1) {
    return defaultOperatorInfo;
  }
  throw new Error("Pattern match failed");
})(get($dict_Ord_String2)(op)(registry));
var insertOperator = insert($dict_Ord_String2);
var mergeRegistries = (base) => (override) => union($dict_Ord_String2)(override)(base);

// ../vibe-parser/dist/VibeParser/VibeParser/Parser.js
var _AMP_AMP6 = (a) => (b) => a && b();
var _PIPE_PIPE7 = (a) => (b) => a || b();
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
var peekKeyword = (kw) => (state) => ((tok) => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(kw)))(current(state));
var peekOperator = (op) => (state) => ((tok) => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(op)))(current(state));
var matchKind = (kind) => (state) => $dict_Eq_TokenKind._EQ_EQ(current(state).kind)(kind) ? Just2(advance2(state)) : Nothing2;
var matchOperator = (op) => (state) => ((tok) => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(op)) ? Just2(advance2(state)) : Nothing2)(current(state));
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
var expectKeyword = (kw) => (state) => ((tok) => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(kw)) ? Ok2([tok, advance2(state)]) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected keyword '")(kw))("' but found "))(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
var expectOperator = (op) => (state) => ((tok) => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)(op)) ? Ok2([tok, advance2(state)]) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected operator '")(op))("' but found "))(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
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
          if (_AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("=>"))) {
            return true;
          } else {
            if (_AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("where"))) {
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
            if (_AMP_AMP6($dict_Eq_Int2._EQ_EQ(newDepth)(0))(() => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("=>")))) {
              return true;
            } else {
              if (_AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("where"))) {
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
              if (_AMP_AMP6($dict_Eq_Int2._EQ_EQ(newDepth)(0))(() => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("=>")))) {
                return true;
              } else {
                if (_AMP_AMP6($dict_Eq_Int2._EQ_EQ(newDepth)(0))(() => _PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Equals2))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2)))) {
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
            if (_AMP_AMP6($dict_Eq_Int2._EQ_EQ(newDepth)(0))(() => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("=>")))) {
              return true;
            } else {
              if (_AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Equals2))(() => $dict_Eq_Int2._EQ_EQ(newDepth)(0))) {
                return false;
              } else {
                if (_AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => $dict_Eq_Int2._EQ_EQ(newDepth)(0))) {
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
var peekDecorator = (state) => ((cur) => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(cur.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(cur.lexeme)("@")) ? (($match_22) => {
  if ($match_22.$tag === 1) {
    return false;
  }
  if ($match_22.$tag === 0) {
    const next = $match_22.$0;
    return _PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(next.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(next.kind)(Keyword2));
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
var unquote = (lexeme) => _AMP_AMP6($dict_Ord_Int2._GT_EQ(length2(lexeme))(2))(() => _AMP_AMP6(startsWith('"')(lexeme))(() => endsWith('"')(lexeme))) ? slice2(1)($dict_Num_Int2._MINUS(length2(lexeme))(1))(lexeme) : lexeme;
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
    })(parseMoreRecordTypeFields(s1));
  }
  throw new Error("Pattern match failed");
})(parseRecordTypeField(state));
parseMoreRecordTypeFields = (state) => peekKind(Comma2)(state) ? ((s1) => (($match_86) => {
  if ($match_86.$tag === 1) {
    const e = $match_86.$0;
    return Err2(e);
  }
  if ($match_86.$tag === 0) {
    const field = $match_86.$0[0];
    const s2 = $match_86.$0[1];
    return (($match_87) => {
      if ($match_87.$tag === 1) {
        const e = $match_87.$0;
        return Err2(e);
      }
      if ($match_87.$tag === 0) {
        const rest = $match_87.$0[0];
        const s3 = $match_87.$0[1];
        return Ok2([_COLON_COLON2(field)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreRecordTypeFields(s2));
  }
  throw new Error("Pattern match failed");
})(parseRecordTypeField(s1)))(advance2(state)) : Ok2([[], state]);
parseRecordTypeField = (state) => (($match_88) => {
  if ($match_88.$tag === 1) {
    const e = $match_88.$0;
    return Err2(e);
  }
  if ($match_88.$tag === 0) {
    const nameTok = $match_88.$0[0];
    const s1 = $match_88.$0[1];
    return (($match_89) => {
      if ($match_89.$tag === 1) {
        const e = $match_89.$0;
        return Err2(e);
      }
      if ($match_89.$tag === 0) {
        const s2 = $match_89.$0[1];
        return (($match_90) => {
          if ($match_90.$tag === 1) {
            const e = $match_90.$0;
            return Err2(e);
          }
          if ($match_90.$tag === 0) {
            const fieldType = $match_90.$0[0];
            const s3 = $match_90.$0[1];
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
parseParenOrTupleType = (state) => ((start) => ((s1) => peekKind(RParen2)(s1) ? ((s2) => Ok2([TRef("Unit")([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_91) => {
  if ($match_91.$tag === 1) {
    const e = $match_91.$0;
    return Err2(e);
  }
  if ($match_91.$tag === 0) {
    const first = $match_91.$0[0];
    const s2 = $match_91.$0[1];
    return (($match_92) => {
      if ($match_92.$tag === 1) {
        const e = $match_92.$0;
        return Err2(e);
      }
      if ($match_92.$tag === 0) {
        const moreTypes = $match_92.$0[0];
        const s3 = $match_92.$0[1];
        return (($match_93) => {
          if ($match_93.$tag === 1) {
            const e = $match_93.$0;
            return Err2(e);
          }
          if ($match_93.$tag === 0) {
            const cp = $match_93.$0[0];
            const s4 = $match_93.$0[1];
            return isEmpty2(moreTypes) ? Ok2([setTypeSpan(first)({ start, end: cp.span.end }), s4]) : Ok2([TTuple(_COLON_COLON2(first)(moreTypes))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close type group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleTypes(s2));
  }
  throw new Error("Pattern match failed");
})(parseTypeExpression(s1)))(advance2(state)))(current(state).span.start);
parseMoreTupleTypes = (state) => peekKind(Comma2)(state) ? ((s1) => (($match_94) => {
  if ($match_94.$tag === 1) {
    const e = $match_94.$0;
    return Err2(e);
  }
  if ($match_94.$tag === 0) {
    const t = $match_94.$0[0];
    const s2 = $match_94.$0[1];
    return (($match_95) => {
      if ($match_95.$tag === 1) {
        const e = $match_95.$0;
        return Err2(e);
      }
      if ($match_95.$tag === 0) {
        const rest = $match_95.$0[0];
        const s3 = $match_95.$0[1];
        return Ok2([_COLON_COLON2(t)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleTypes(s2));
  }
  throw new Error("Pattern match failed");
})(parseTypeExpression(s1)))(advance2(state)) : Ok2([[], state]);
parseTypeRef = (state) => ((tok) => _PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier2)) ? ((ident) => ((s1) => (($match_96) => {
  if ($match_96.$tag === 1) {
    const e = $match_96.$0;
    return Err2(e);
  }
  if ($match_96.$tag === 0) {
    const nameResult = $match_96.$0[0];
    const s2 = $match_96.$0[1];
    return ((isTypeCtor) => isTypeCtor ? (($match_97) => {
      if ($match_97.$tag === 1) {
        const e = $match_97.$0;
        return Err2(e);
      }
      if ($match_97.$tag === 0) {
        const args = $match_97.$0[0];
        const s3 = $match_97.$0[1];
        return ((endPos) => Ok2([TRef(nameResult.name)(args)({ start: ident.span.start, end: endPos }), s3]))((($match_98) => {
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
    })(parseTypeApplicationArgs({ start: ident.span.start, end: nameResult.nameEnd })(s2)) : Ok2([TRef(nameResult.name)([])({ start: ident.span.start, end: nameResult.nameEnd }), s2]))(_PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(ident.kind)(UpperIdentifier2))(() => contains(".")(nameResult.name)));
  }
  throw new Error("Pattern match failed");
})(parseQualifiedName(ident.lexeme)(ident.span.end)(s1)))(advance2(state)))(tok) : Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected type but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span)))(current(state));
parseTypeApplicationArgs = (lastSpan) => (state) => _AMP_AMP6(isTypeStart(current(state)))(() => onSameLine(lastSpan)(current(state))) ? (($match_99) => {
  if ($match_99.$tag === 1) {
    const e = $match_99.$0;
    return Err2(e);
  }
  if ($match_99.$tag === 0) {
    const arg = $match_99.$0[0];
    const s1 = $match_99.$0[1];
    return (($match_100) => {
      if ($match_100.$tag === 1) {
        const e = $match_100.$0;
        return Err2(e);
      }
      if ($match_100.$tag === 0) {
        const rest = $match_100.$0[0];
        const s2 = $match_100.$0[1];
        return Ok2([_COLON_COLON2(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeApplicationArgs(typeSpan(arg))(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok2([[], state]);
parseTypeAtom = (state) => ((tok) => (($match_101) => {
  if ($match_101.$tag === 11) {
    return parseRecordType(state);
  }
  if ($match_101.$tag === 9) {
    return parseParenOrTupleType(state);
  }
  {
    return _PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier2)) ? ((ident) => ((s1) => (($match_102) => {
      if ($match_102.$tag === 1) {
        const e = $match_102.$0;
        return Err2(e);
      }
      if ($match_102.$tag === 0) {
        const nameResult = $match_102.$0[0];
        const s2 = $match_102.$0[1];
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
var parseDecoratedDeclaration = (state) => (($match_103) => {
  if ($match_103.$tag === 1) {
    const e = $match_103.$0;
    return Err2(e);
  }
  if ($match_103.$tag === 0) {
    const atTok = $match_103.$0[0];
    const s1 = $match_103.$0[1];
    return ((decoratorTok) => _PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(decoratorTok.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(decoratorTok.kind)(Keyword2)) ? ((decorator) => ((s2) => (($match_104) => {
      if ($match_104.$tag === 1) {
        const e = $match_104.$0;
        return Err2(e);
      }
      if ($match_104.$tag === 0) {
        const args = $match_104.$0[0];
        const s3 = $match_104.$0[1];
        return ((s4) => (($match_105) => {
          if ($match_105.$tag === 1) {
            const e = $match_105.$0;
            return Err2(e);
          }
          if ($match_105.$tag === 0) {
            const nameResult = $match_105.$0[0];
            const s5 = $match_105.$0[1];
            return (($match_106) => {
              if ($match_106.$tag === 1) {
                const e = $match_106.$0;
                return Err2(e);
              }
              if ($match_106.$tag === 0) {
                const s6 = $match_106.$0[1];
                return (($match_107) => {
                  if ($match_107.$tag === 1) {
                    const e = $match_107.$0;
                    return Err2(e);
                  }
                  if ($match_107.$tag === 0) {
                    const annotation = $match_107.$0[0];
                    const s7 = $match_107.$0[1];
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
var parseTypeParams = (state) => peekKind(LowerIdentifier2)(state) ? ((tok) => ((s1) => (($match_108) => {
  if ($match_108.$tag === 1) {
    const e = $match_108.$0;
    return Err2(e);
  }
  if ($match_108.$tag === 0) {
    const rest = $match_108.$0[0];
    const s2 = $match_108.$0[1];
    return Ok2([_COLON_COLON2(tok.lexeme)(rest), s2]);
  }
  throw new Error("Pattern match failed");
})(parseTypeParams(s1)))(advance2(state)))(current(state)) : Ok2([[], state]);
var parseTypeAliasDecl = (typeTok) => (state) => (($match_109) => {
  if ($match_109.$tag === 1) {
    const e = $match_109.$0;
    return Err2(e);
  }
  if ($match_109.$tag === 0) {
    const s1 = $match_109.$0[1];
    return (($match_110) => {
      if ($match_110.$tag === 1) {
        const e = $match_110.$0;
        return Err2(e);
      }
      if ($match_110.$tag === 0) {
        const nameTok = $match_110.$0[0];
        const s2 = $match_110.$0[1];
        return (($match_111) => {
          if ($match_111.$tag === 1) {
            const e = $match_111.$0;
            return Err2(e);
          }
          if ($match_111.$tag === 0) {
            const params = $match_111.$0[0];
            const s3 = $match_111.$0[1];
            return (($match_112) => {
              if ($match_112.$tag === 1) {
                const e = $match_112.$0;
                return Err2(e);
              }
              if ($match_112.$tag === 0) {
                const s4 = $match_112.$0[1];
                return (($match_113) => {
                  if ($match_113.$tag === 1) {
                    const e = $match_113.$0;
                    return Err2(e);
                  }
                  if ($match_113.$tag === 0) {
                    const value = $match_113.$0[0];
                    const s5 = $match_113.$0[1];
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
var parseTypeParamsWithLayout = (typeLine) => (typeCol) => (state) => peekKind(LowerIdentifier2)(state) ? ((tok) => _AMP_AMP6($dict_Eq_Int2._SLASH_EQ(tok.span.start.line)(typeLine))(() => $dict_Ord_Int2._LT_EQ(tok.span.start.column)(typeCol)) ? Ok2([[], state]) : ((s1) => (($match_114) => {
  if ($match_114.$tag === 1) {
    const e = $match_114.$0;
    return Err2(e);
  }
  if ($match_114.$tag === 0) {
    const rest = $match_114.$0[0];
    const s2 = $match_114.$0[1];
    return Ok2([_COLON_COLON2(tok.lexeme)(rest), s2]);
  }
  throw new Error("Pattern match failed");
})(parseTypeParamsWithLayout(typeLine)(typeCol)(s1)))(advance2(state)))(current(state)) : Ok2([[], state]);
var parseRecordTypeFields = (state) => (($match_115) => {
  if ($match_115.$tag === 1) {
    const e = $match_115.$0;
    return Err2(e);
  }
  if ($match_115.$tag === 0) {
    const s1 = $match_115.$0[1];
    return peekKind(RBrace2)(s1) ? ((s2) => Ok2([[], s2]))(advance2(s1)) : (($match_116) => {
      if ($match_116.$tag === 1) {
        const e = $match_116.$0;
        return Err2(e);
      }
      if ($match_116.$tag === 0) {
        const fields = $match_116.$0[0];
        const s2 = $match_116.$0[1];
        return (($match_117) => {
          if ($match_117.$tag === 1) {
            const e = $match_117.$0;
            return Err2(e);
          }
          if ($match_117.$tag === 0) {
            const s3 = $match_117.$0[1];
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
var parseConstructorArgs = (lastSpan) => (state) => _AMP_AMP6(isTypeStart(current(state)))(() => _AMP_AMP6(onSameLine(lastSpan)(current(state)))(() => not2(peekKind(Pipe2)(state)))) ? (($match_118) => {
  if ($match_118.$tag === 1) {
    const e = $match_118.$0;
    return Err2(e);
  }
  if ($match_118.$tag === 0) {
    const arg = $match_118.$0[0];
    const s1 = $match_118.$0[1];
    return (($match_119) => {
      if ($match_119.$tag === 1) {
        const e = $match_119.$0;
        return Err2(e);
      }
      if ($match_119.$tag === 0) {
        const rest = $match_119.$0[0];
        const s2 = $match_119.$0[1];
        return Ok2([_COLON_COLON2(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseConstructorArgs(typeSpan(arg))(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok2([[], state]);
var parseConstructorVariant = (state) => (($match_120) => {
  if ($match_120.$tag === 1) {
    const e = $match_120.$0;
    return Err2(e);
  }
  if ($match_120.$tag === 0) {
    const nameTok = $match_120.$0[0];
    const s1 = $match_120.$0[1];
    return (($match_121) => {
      if ($match_121.$tag === 1) {
        const e = $match_121.$0;
        return Err2(e);
      }
      if ($match_121.$tag === 0) {
        const args = $match_121.$0[0];
        const s2 = $match_121.$0[1];
        return ((endPos) => Ok2([{ name: nameTok.lexeme, args, span: { start: nameTok.span.start, end: endPos } }, s2]))((($match_122) => {
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
})(expect(UpperIdentifier2)("constructor name")(state));
var parseMoreConstructors = (equalsCol) => (equalsLine) => (state) => peekKind(Pipe2)(state) ? ((pipeTok) => _AMP_AMP6($dict_Eq_Int2._SLASH_EQ(pipeTok.span.start.line)(equalsLine))(() => $dict_Eq_Int2._SLASH_EQ(pipeTok.span.start.column)(equalsCol)) ? Err2(makeError("Constructor variant '|' must align with '='")(pipeTok.span)) : ((s1) => (($match_123) => {
  if ($match_123.$tag === 1) {
    const e = $match_123.$0;
    return Err2(e);
  }
  if ($match_123.$tag === 0) {
    const ctor = $match_123.$0[0];
    const s2 = $match_123.$0[1];
    return (($match_124) => {
      if ($match_124.$tag === 1) {
        const e = $match_124.$0;
        return Err2(e);
      }
      if ($match_124.$tag === 0) {
        const rest = $match_124.$0[0];
        const s3 = $match_124.$0[1];
        return Ok2([_COLON_COLON2(ctor)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreConstructors(equalsCol)(equalsLine)(s2));
  }
  throw new Error("Pattern match failed");
})(parseConstructorVariant(s1)))(advance2(state)))(current(state)) : Ok2([[], state]);
var parseConstructorVariants = (equalsCol) => (equalsLine) => (state) => (($match_125) => {
  if ($match_125.$tag === 1) {
    const e = $match_125.$0;
    return Err2(e);
  }
  if ($match_125.$tag === 0) {
    const first = $match_125.$0[0];
    const s1 = $match_125.$0[1];
    return (($match_126) => {
      if ($match_126.$tag === 1) {
        const e = $match_126.$0;
        return Err2(e);
      }
      if ($match_126.$tag === 0) {
        const rest = $match_126.$0[0];
        const s2 = $match_126.$0[1];
        return Ok2([_COLON_COLON2(first)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreConstructors(equalsCol)(equalsLine)(s1));
  }
  throw new Error("Pattern match failed");
})(parseConstructorVariant(state));
var parseTypeOrOpaqueDecl = (typeTok) => (state) => ((typeColumn) => ((typeLine) => (($match_127) => {
  if ($match_127.$tag === 1) {
    const e = $match_127.$0;
    return Err2(e);
  }
  if ($match_127.$tag === 0) {
    const constraints = $match_127.$0[0];
    const s1 = $match_127.$0[1];
    return (($match_128) => {
      if ($match_128.$tag === 1) {
        const e = $match_128.$0;
        return Err2(e);
      }
      if ($match_128.$tag === 0) {
        const nameTok = $match_128.$0[0];
        const s2 = $match_128.$0[1];
        return (($match_129) => {
          if ($match_129.$tag === 1) {
            const e = $match_129.$0;
            return Err2(e);
          }
          if ($match_129.$tag === 0) {
            const params = $match_129.$0[0];
            const s3 = $match_129.$0[1];
            return peekKind(Equals2)(s3) ? ((eqTok) => ((s4) => ((equalsCol) => ((equalsLine) => peekKind(LBrace2)(s4) ? (($match_130) => {
              if ($match_130.$tag === 1) {
                const e = $match_130.$0;
                return Err2(e);
              }
              if ($match_130.$tag === 0) {
                const fields = $match_130.$0[0];
                const s5 = $match_130.$0[1];
                return Ok2([DRecordType(nameTok.lexeme)(params)(constraints)(fields)({ start: typeTok.span.start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(parseRecordTypeFields(s4)) : (($match_131) => {
              if ($match_131.$tag === 1) {
                const e = $match_131.$0;
                return Err2(e);
              }
              if ($match_131.$tag === 0) {
                const ctors = $match_131.$0[0];
                const s5 = $match_131.$0[1];
                return ((endPos) => Ok2([DType(nameTok.lexeme)(params)(constraints)(ctors)({ start: typeTok.span.start, end: endPos }), s5]))((($match_132) => {
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
var parseTypeOrAliasDeclaration = (state) => (($match_133) => {
  if ($match_133.$tag === 1) {
    const e = $match_133.$0;
    return Err2(e);
  }
  if ($match_133.$tag === 0) {
    const typeTok = $match_133.$0[0];
    const s1 = $match_133.$0[1];
    return peekKeyword("alias")(s1) ? parseTypeAliasDecl(typeTok)(s1) : parseTypeOrOpaqueDecl(typeTok)(s1);
  }
  throw new Error("Pattern match failed");
})(expectKeyword("type")(state));
var parseOptionalConstraints = (hasConstraints) => (state) => hasConstraints ? parseConstraints(state) : Ok2([[], state]);
var parseProtocolMethodName = (state) => ((tok) => (($match_134) => {
  if ($match_134.$tag === 9) {
    return ((s1) => (($match_135) => {
      if ($match_135.$tag === 1) {
        const e = $match_135.$0;
        return Err2(e);
      }
      if ($match_135.$tag === 0) {
        const opTok = $match_135.$0[0];
        const s2 = $match_135.$0[1];
        return (($match_136) => {
          if ($match_136.$tag === 1) {
            const e = $match_136.$0;
            return Err2(e);
          }
          if ($match_136.$tag === 0) {
            const s3 = $match_136.$0[1];
            return Ok2([opTok.lexeme, s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close paren after operator")(s2));
      }
      throw new Error("Pattern match failed");
    })(expect(Operator2)("operator in protocol method")(s1)))(advance2(state));
  }
  if ($match_134.$tag === 0) {
    return ((s1) => Ok2([tok.lexeme, s1]))(advance2(state));
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Expected method name but found ")(tokenKindStr(tok.kind)))(tok.span));
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
    return ((s1) => $dict_Eq_String2._EQ_EQ(tok.lexeme)("_") ? Ok2([PWildcard(tok.span), s1]) : Ok2([PVar(tok.lexeme)(tok.span), s1]))(advance2(state));
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
    return Err2(makeError("List patterns are not allowed in function parameters")(tok.span));
  }
  {
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Expected function parameter pattern but found ")(tokenKindStr(tok.kind)))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseFPCtorPattern = (state) => ((ctor) => ((s1) => (($match_139) => {
  if ($match_139.$tag === 1) {
    const e = $match_139.$0;
    return Err2(e);
  }
  if ($match_139.$tag === 0) {
    const args = $match_139.$0[0];
    const s2 = $match_139.$0[1];
    return ((endPos) => Ok2([PConstructor(ctor.lexeme)(args)({ start: ctor.span.start, end: endPos }), s2]))((($match_140) => {
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
})(parseFPCtorArgs(s1)))(advance2(state)))(current(state));
parseFPCtorArgs = (state) => isFunctionParamPatternStart(current(state)) ? (($match_141) => {
  if ($match_141.$tag === 1) {
    const e = $match_141.$0;
    return Err2(e);
  }
  if ($match_141.$tag === 0) {
    const pat = $match_141.$0[0];
    const s1 = $match_141.$0[1];
    return (($match_142) => {
      if ($match_142.$tag === 1) {
        const e = $match_142.$0;
        return Err2(e);
      }
      if ($match_142.$tag === 0) {
        const rest = $match_142.$0[0];
        const s2 = $match_142.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseFPCtorArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(state)) : Ok2([[], state]);
parseFPTuplePattern = (state) => ((start) => ((s1) => peekKind(RParen2)(s1) ? ((s2) => Ok2([PTuple([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_143) => {
  if ($match_143.$tag === 1) {
    const e = $match_143.$0;
    return Err2(e);
  }
  if ($match_143.$tag === 0) {
    const first = $match_143.$0[0];
    const s2 = $match_143.$0[1];
    return (($match_144) => {
      if ($match_144.$tag === 1) {
        const e = $match_144.$0;
        return Err2(e);
      }
      if ($match_144.$tag === 0) {
        const more = $match_144.$0[0];
        const s3 = $match_144.$0[1];
        return (($match_145) => {
          if ($match_145.$tag === 1) {
            const e = $match_145.$0;
            return Err2(e);
          }
          if ($match_145.$tag === 0) {
            const cp = $match_145.$0[0];
            const s4 = $match_145.$0[1];
            return isEmpty2(more) ? Ok2([setPatSpan(first)({ start, end: cp.span.end }), s4]) : Ok2([PTuple(_COLON_COLON2(first)(more))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close pattern group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseFPTupleRest(s2));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(s1)))(advance2(state)))(current(state).span.start);
parseFPTupleRest = (state) => peekKind(Comma2)(state) ? ((s1) => (($match_146) => {
  if ($match_146.$tag === 1) {
    const e = $match_146.$0;
    return Err2(e);
  }
  if ($match_146.$tag === 0) {
    const pat = $match_146.$0[0];
    const s2 = $match_146.$0[1];
    return (($match_147) => {
      if ($match_147.$tag === 1) {
        const e = $match_147.$0;
        return Err2(e);
      }
      if ($match_147.$tag === 0) {
        const rest = $match_147.$0[0];
        const s3 = $match_147.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseFPTupleRest(s2));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(s1)))(advance2(state)) : Ok2([[], state]);
parseFPRecordPattern = (state) => ((start) => ((s1) => peekKind(RBrace2)(s1) ? ((s2) => Ok2([PRecord([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_148) => {
  if ($match_148.$tag === 1) {
    const e = $match_148.$0;
    return Err2(e);
  }
  if ($match_148.$tag === 0) {
    const fields = $match_148.$0[0];
    const s2 = $match_148.$0[1];
    return (($match_149) => {
      if ($match_149.$tag === 1) {
        const e = $match_149.$0;
        return Err2(e);
      }
      if ($match_149.$tag === 0) {
        const s3 = $match_149.$0[1];
        return Ok2([PRecord(fields)({ start, end: previous(s3).span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RBrace2)("close record pattern")(s2));
  }
  throw new Error("Pattern match failed");
})(parseRecordFieldPatterns(s1)))(advance2(state)))(current(state).span.start);
parseRecordFieldPatterns = (state) => (($match_150) => {
  if ($match_150.$tag === 1) {
    const e = $match_150.$0;
    return Err2(e);
  }
  if ($match_150.$tag === 0) {
    const field = $match_150.$0[0];
    const s1 = $match_150.$0[1];
    return peekKind(Comma2)(s1) ? ((s2) => (($match_151) => {
      if ($match_151.$tag === 1) {
        const e = $match_151.$0;
        return Err2(e);
      }
      if ($match_151.$tag === 0) {
        const rest = $match_151.$0[0];
        const s3 = $match_151.$0[1];
        return Ok2([_COLON_COLON2(field)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseRecordFieldPatterns(s2)))(advance2(s1)) : Ok2([[field], s1]);
  }
  throw new Error("Pattern match failed");
})(parseRecordFieldPattern(state));
parseRecordFieldPattern = (state) => (($match_152) => {
  if ($match_152.$tag === 1) {
    const e = $match_152.$0;
    return Err2(e);
  }
  if ($match_152.$tag === 0) {
    const nameTok = $match_152.$0[0];
    const s1 = $match_152.$0[1];
    return peekKind(Equals2)(s1) ? ((s2) => (($match_153) => {
      if ($match_153.$tag === 1) {
        const e = $match_153.$0;
        return Err2(e);
      }
      if ($match_153.$tag === 0) {
        const pat = $match_153.$0[0];
        const s3 = $match_153.$0[1];
        return Ok2([{ name: nameTok.lexeme, pattern: pat }, s3]);
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParamPattern(s2)))(advance2(s1)) : Ok2([{ name: nameTok.lexeme, pattern: PVar(nameTok.lexeme)(nameTok.span) }, s1]);
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier2)("record field name")(state));
var parseFunctionParams = (state) => isFunctionParamPatternStart(current(state)) ? (($match_154) => {
  if ($match_154.$tag === 1) {
    const e = $match_154.$0;
    return Err2(e);
  }
  if ($match_154.$tag === 0) {
    const pat = $match_154.$0[0];
    const s1 = $match_154.$0[1];
    return (($match_155) => {
      if ($match_155.$tag === 1) {
        const e = $match_155.$0;
        return Err2(e);
      }
      if ($match_155.$tag === 0) {
        const rest = $match_155.$0[0];
        const s2 = $match_155.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseFunctionParams(s1));
  }
  throw new Error("Pattern match failed");
})(parseFunctionParamPattern(state)) : Ok2([[], state]);
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
    if (peekKind(Dot2)(state)) {
      {
        const $match_157 = peekAhead(1)(state);
        if ($match_157.$tag === 1) {
          return Ok2([{ name, nameEnd: end }, state]);
        }
        if ($match_157.$tag === 0) {
          const next = $match_157.$0;
          if ($dict_Eq_TokenKind._EQ_EQ(next.kind)(UpperIdentifier2)) {
            {
              const s1 = advance2(state);
              {
                const $match_158 = expect(UpperIdentifier2)("constructor name part")(s1);
                if ($match_158.$tag === 1) {
                  const e = $match_158.$0;
                  return Err2(e);
                }
                if ($match_158.$tag === 0) {
                  const nextTok = $match_158.$0[0];
                  const s2 = $match_158.$0[1];
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
var parseAtomicPattern;
var parseListPattern;
var parsePatternList;
var parseParenOrTuplePattern;
var parseTuplePatternRest;
parsePattern = (state) => (($match_159) => {
  if ($match_159.$tag === 1) {
    const e = $match_159.$0;
    return Err2(e);
  }
  if ($match_159.$tag === 0) {
    const primary = $match_159.$0[0];
    const s1 = $match_159.$0[1];
    return peekOperator("::")(s1) ? ((s2) => (($match_160) => {
      if ($match_160.$tag === 1) {
        const e = $match_160.$0;
        return Err2(e);
      }
      if ($match_160.$tag === 0) {
        const tail = $match_160.$0[0];
        const s3 = $match_160.$0[1];
        return Ok2([PCons(primary)(tail)({ start: patSpan(primary).start, end: patSpan(tail).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parsePattern(s2)))(advance2(s1)) : Ok2([primary, s1]);
  }
  throw new Error("Pattern match failed");
})(parsePrimaryPattern(state));
parsePrimaryPattern = (state) => ((tok) => (($match_161) => {
  if ($match_161.$tag === 0) {
    return ((s1) => $dict_Eq_String2._EQ_EQ(tok.lexeme)("_") ? Ok2([PWildcard(tok.span), s1]) : Ok2([PVar(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_161.$tag === 3) {
    return ((s1) => contains(".")(tok.lexeme) ? Ok2([PFloat(tok.lexeme)(tok.span), s1]) : Ok2([PInt(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_161.$tag === 4) {
    return ((s1) => Ok2([PString(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_161.$tag === 5) {
    return ((s1) => Ok2([PChar(tok.lexeme)(tok.span), s1]))(advance2(state));
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
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected pattern but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseConstructorPattern = (state) => ((ctor) => ((s1) => (($match_162) => {
  if ($match_162.$tag === 1) {
    const e = $match_162.$0;
    return Err2(e);
  }
  if ($match_162.$tag === 0) {
    const nameResult = $match_162.$0[0];
    const s2 = $match_162.$0[1];
    return (($match_163) => {
      if ($match_163.$tag === 1) {
        const e = $match_163.$0;
        return Err2(e);
      }
      if ($match_163.$tag === 0) {
        const args = $match_163.$0[0];
        const s3 = $match_163.$0[1];
        return ((endPos) => Ok2([PConstructor(nameResult.name)(args)({ start: ctor.span.start, end: endPos }), s3]))((($match_164) => {
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
})(parseQualifiedCtorName(ctor.lexeme)(ctor.span.end)(s1)))(advance2(state)))(current(state));
parseConstructorPatternArgs = (state) => isPatternStart(current(state)) ? (($match_165) => {
  if ($match_165.$tag === 1) {
    const e = $match_165.$0;
    return Err2(e);
  }
  if ($match_165.$tag === 0) {
    const pat = $match_165.$0[0];
    const s1 = $match_165.$0[1];
    return (($match_166) => {
      if ($match_166.$tag === 1) {
        const e = $match_166.$0;
        return Err2(e);
      }
      if ($match_166.$tag === 0) {
        const rest = $match_166.$0[0];
        const s2 = $match_166.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseConstructorPatternArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseAtomicPattern(state)) : Ok2([[], state]);
parseAtomicPattern = (state) => ((tok) => (($match_167) => {
  if ($match_167.$tag === 1) {
    return ((s1) => (($match_168) => {
      if ($match_168.$tag === 1) {
        const e = $match_168.$0;
        return Err2(e);
      }
      if ($match_168.$tag === 0) {
        const nameResult = $match_168.$0[0];
        const s2 = $match_168.$0[1];
        return Ok2([PConstructor(nameResult.name)([])({ start: tok.span.start, end: nameResult.nameEnd }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseQualifiedCtorName(tok.lexeme)(tok.span.end)(s1)))(advance2(state));
  }
  {
    return parsePrimaryPattern(state);
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseListPattern = (state) => ((start) => ((s1) => peekKind(RBracket2)(s1) ? ((s2) => Ok2([PList([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_169) => {
  if ($match_169.$tag === 1) {
    const e = $match_169.$0;
    return Err2(e);
  }
  if ($match_169.$tag === 0) {
    const elements = $match_169.$0[0];
    const s2 = $match_169.$0[1];
    return (($match_170) => {
      if ($match_170.$tag === 1) {
        const e = $match_170.$0;
        return Err2(e);
      }
      if ($match_170.$tag === 0) {
        const s3 = $match_170.$0[1];
        return Ok2([PList(elements)({ start, end: previous(s3).span.end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RBracket2)("close list pattern")(s2));
  }
  throw new Error("Pattern match failed");
})(parsePatternList(s1)))(advance2(state)))(current(state).span.start);
parsePatternList = (state) => (($match_171) => {
  if ($match_171.$tag === 1) {
    const e = $match_171.$0;
    return Err2(e);
  }
  if ($match_171.$tag === 0) {
    const pat = $match_171.$0[0];
    const s1 = $match_171.$0[1];
    return peekKind(Comma2)(s1) ? ((s2) => (($match_172) => {
      if ($match_172.$tag === 1) {
        const e = $match_172.$0;
        return Err2(e);
      }
      if ($match_172.$tag === 0) {
        const rest = $match_172.$0[0];
        const s3 = $match_172.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parsePatternList(s2)))(advance2(s1)) : Ok2([[pat], s1]);
  }
  throw new Error("Pattern match failed");
})(parsePattern(state));
parseParenOrTuplePattern = (state) => ((start) => ((s1) => peekKind(RParen2)(s1) ? ((s2) => Ok2([PTuple([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_173) => {
  if ($match_173.$tag === 1) {
    const e = $match_173.$0;
    return Err2(e);
  }
  if ($match_173.$tag === 0) {
    const first = $match_173.$0[0];
    const s2 = $match_173.$0[1];
    return (($match_174) => {
      if ($match_174.$tag === 1) {
        const e = $match_174.$0;
        return Err2(e);
      }
      if ($match_174.$tag === 0) {
        const more = $match_174.$0[0];
        const s3 = $match_174.$0[1];
        return (($match_175) => {
          if ($match_175.$tag === 1) {
            const e = $match_175.$0;
            return Err2(e);
          }
          if ($match_175.$tag === 0) {
            const cp = $match_175.$0[0];
            const s4 = $match_175.$0[1];
            return isEmpty2(more) ? Ok2([setPatSpan(first)({ start, end: cp.span.end }), s4]) : Ok2([PTuple(_COLON_COLON2(first)(more))({ start, end: cp.span.end }), s4]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close pattern group")(s3));
      }
      throw new Error("Pattern match failed");
    })(parseTuplePatternRest(s2));
  }
  throw new Error("Pattern match failed");
})(parsePattern(s1)))(advance2(state)))(current(state).span.start);
parseTuplePatternRest = (state) => peekKind(Comma2)(state) ? ((s1) => (($match_176) => {
  if ($match_176.$tag === 1) {
    const e = $match_176.$0;
    return Err2(e);
  }
  if ($match_176.$tag === 0) {
    const pat = $match_176.$0[0];
    const s2 = $match_176.$0[1];
    return (($match_177) => {
      if ($match_177.$tag === 1) {
        const e = $match_177.$0;
        return Err2(e);
      }
      if ($match_177.$tag === 0) {
        const rest = $match_177.$0[0];
        const s3 = $match_177.$0[1];
        return Ok2([_COLON_COLON2(pat)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseTuplePatternRest(s2));
  }
  throw new Error("Pattern match failed");
})(parsePattern(s1)))(advance2(state)) : Ok2([[], state]);
var parseLambdaArgs = (state) => (($match_178) => {
  if ($match_178.$tag === 0) {
    const s1 = $match_178.$0;
    return Ok2([[], s1]);
  }
  if ($match_178.$tag === 1) {
    return isPatternStart(current(state)) ? (($match_179) => {
      if ($match_179.$tag === 1) {
        const e = $match_179.$0;
        return Err2(e);
      }
      if ($match_179.$tag === 0) {
        const pat = $match_179.$0[0];
        const s2 = $match_179.$0[1];
        return (($match_180) => {
          if ($match_180.$tag === 1) {
            const e = $match_180.$0;
            return Err2(e);
          }
          if ($match_180.$tag === 0) {
            const rest = $match_180.$0[0];
            const s3 = $match_180.$0[1];
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
      const $match_181 = matchKind(Dot2)(state);
      if ($match_181.$tag === 1) {
        return Ok2([expr, state]);
      }
      if ($match_181.$tag === 0) {
        const s1 = $match_181.$0;
        {
          const tok = current(s1);
          if (_PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(tok.kind)(LowerIdentifier2))(() => $dict_Eq_TokenKind._EQ_EQ(tok.kind)(UpperIdentifier2))) {
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
        return $dict_Eq_String2._EQ_EQ(name)(implName) ? _COLON_COLON2({ name, methodType: typeExpr, defaultArgs: args, defaultBody: body, hasDefault: true, hasType: true, span: { start: span.start, end: implSpan.end } })(mergeProtocolItems(rest2)) : _COLON_COLON2(noDefaultMethod(name)(typeExpr)(span))(mergeProtocolItems(rest));
      }
      {
        return _COLON_COLON2(noDefaultMethod(name)(typeExpr)(span))(mergeProtocolItems(rest));
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
    return ((dummyType) => _COLON_COLON2({ name, methodType: dummyType, defaultArgs: args, defaultBody: body, hasDefault: true, hasType: false, span })(mergeProtocolItems(rest)))(TRef("Unknown")([])(span));
  }
  throw new Error("Pattern match failed");
})(items);
var parseImplementationTypeArgs = (state) => isTypeStart(current(state)) ? (($match_184) => {
  if ($match_184.$tag === 1) {
    const e = $match_184.$0;
    return Err2(e);
  }
  if ($match_184.$tag === 0) {
    const t = $match_184.$0[0];
    const s1 = $match_184.$0[1];
    return (($match_185) => {
      if ($match_185.$tag === 1) {
        const e = $match_185.$0;
        return Err2(e);
      }
      if ($match_185.$tag === 0) {
        const rest = $match_185.$0[0];
        const s2 = $match_185.$0[1];
        return Ok2([_COLON_COLON2(t)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseImplementationTypeArgs(s1));
  }
  throw new Error("Pattern match failed");
})(parseTypeAtom(state)) : Ok2([[], state]);
var parseInfixOperator = (state) => peekKind(LParen2)(state) ? ((s1) => (($match_186) => {
  if ($match_186.$tag === 1) {
    const e = $match_186.$0;
    return Err2(e);
  }
  if ($match_186.$tag === 0) {
    const opTok = $match_186.$0[0];
    const s2 = $match_186.$0[1];
    return (($match_187) => {
      if ($match_187.$tag === 1) {
        const e = $match_187.$0;
        return Err2(e);
      }
      if ($match_187.$tag === 0) {
        const cp = $match_187.$0[0];
        const s3 = $match_187.$0[1];
        return Ok2([{ opName: opTok.lexeme, opEnd: cp.span.end }, s3]);
      }
      throw new Error("Pattern match failed");
    })(expect(RParen2)("close paren")(s2));
  }
  throw new Error("Pattern match failed");
})(expect(Operator2)("operator")(s1)))(advance2(state)) : (($match_188) => {
  if ($match_188.$tag === 1) {
    const e = $match_188.$0;
    return Err2(e);
  }
  if ($match_188.$tag === 0) {
    const opTok = $match_188.$0[0];
    const s1 = $match_188.$0[1];
    return Ok2([{ opName: opTok.lexeme, opEnd: opTok.span.end }, s1]);
  }
  throw new Error("Pattern match failed");
})(expect(Operator2)("operator")(state));
var parseInfixDeclaration = (state) => ((fixityTok) => ((start) => ((s1) => (($match_189) => {
  if ($match_189.$tag === 1) {
    const e = $match_189.$0;
    return Err2(e);
  }
  if ($match_189.$tag === 0) {
    const precTok = $match_189.$0[0];
    const s2 = $match_189.$0[1];
    return (($match_190) => {
      if ($match_190.$tag === 1) {
        const e = $match_190.$0;
        return Err2(e);
      }
      if ($match_190.$tag === 0) {
        const opResult = $match_190.$0[0];
        const s3 = $match_190.$0[1];
        return ((assocStr) => Ok2([DInfix(assocStr)(stringToInt(precTok.lexeme))(opResult.opName)({ start, end: opResult.opEnd }), s3]))((($match_191) => {
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
parseDeclaration = (state) => peekDecorator(state) ? parseDecoratedDeclaration(state) : peekKeyword("type")(state) ? parseTypeOrAliasDeclaration(state) : peekKeyword("protocol")(state) ? parseProtocolDeclaration(state) : peekKeyword("implement")(state) ? parseImplementationDeclaration(state) : _PIPE_PIPE7(peekKeyword("infix")(state))(() => _PIPE_PIPE7(peekKeyword("infixl")(state))(() => peekKeyword("infixr")(state))) ? parseInfixDeclaration(state) : parseAnnotationOrValue(state);
parseAnnotationOrValue = (state) => (($match_192) => {
  if ($match_192.$tag === 1) {
    const e = $match_192.$0;
    return Err2(e);
  }
  if ($match_192.$tag === 0) {
    const nameResult = $match_192.$0[0];
    const s1 = $match_192.$0[1];
    return peekKind(Colon2)(s1) ? ((s2) => (($match_193) => {
      if ($match_193.$tag === 1) {
        const e = $match_193.$0;
        return Err2(e);
      }
      if ($match_193.$tag === 0) {
        const annotation = $match_193.$0[0];
        const s3 = $match_193.$0[1];
        return Ok2([DTypeAnnotation({ name: nameResult.declName, annotation, span: { start: nameResult.declSpan.start, end: typeSpan(annotation).end } }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeExpression(s2)))(advance2(s1)) : (($match_194) => {
      if ($match_194.$tag === 1) {
        const e = $match_194.$0;
        return Err2(e);
      }
      if ($match_194.$tag === 0) {
        const result = $match_194.$0[0];
        const s2 = $match_194.$0[1];
        return Ok2([DValue({ name: nameResult.declName, args: result.mArgs, body: result.mBody, span: { start: nameResult.declSpan.start, end: exprSpan(result.mBody).end } }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMethodBody(Just2(nameResult.declSpan.start.column))(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclarationName(state));
parseMethodBody = (declColumn) => (state) => (($match_195) => {
  if ($match_195.$tag === 1) {
    const e = $match_195.$0;
    return Err2(e);
  }
  if ($match_195.$tag === 0) {
    const args = $match_195.$0[0];
    const s1 = $match_195.$0[1];
    return (($match_196) => {
      if ($match_196.$tag === 1) {
        const e = $match_196.$0;
        return Err2(e);
      }
      if ($match_196.$tag === 0) {
        const eqTok = $match_196.$0[0];
        const s2 = $match_196.$0[1];
        return (($match_197) => {
          if ($match_197.$tag === 1) {
            return (($match_198) => {
              if ($match_198.$tag === 1) {
                const e = $match_198.$0;
                return Err2(e);
              }
              if ($match_198.$tag === 0) {
                const body = $match_198.$0[0];
                const s3 = $match_198.$0[1];
                return Ok2([{ mArgs: args, mBody: body }, s3]);
              }
              throw new Error("Pattern match failed");
            })(parseExpression(Nothing2)(s2));
          }
          if ($match_197.$tag === 0) {
            const col = $match_197.$0;
            return ((bodyTok) => _AMP_AMP6($dict_Eq_Int2._SLASH_EQ(bodyTok.span.start.line)(eqTok.span.start.line))(() => $dict_Ord_Int2._LT_EQ(bodyTok.span.start.column)(col)) ? Err2(makeError($dict_Appendable_String2._PLUS_PLUS("Function body must be indented past column ")(intToStr(col)))(bodyTok.span)) : (($match_199) => {
              if ($match_199.$tag === 1) {
                const e = $match_199.$0;
                return Err2(e);
              }
              if ($match_199.$tag === 0) {
                const body = $match_199.$0[0];
                const s3 = $match_199.$0[1];
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
parseProtocolDeclaration = (state) => (($match_200) => {
  if ($match_200.$tag === 1) {
    const e = $match_200.$0;
    return Err2(e);
  }
  if ($match_200.$tag === 0) {
    const protocolTok = $match_200.$0[0];
    const s1 = $match_200.$0[1];
    return ((hasConstraints) => (($match_201) => {
      if ($match_201.$tag === 1) {
        const e = $match_201.$0;
        return Err2(e);
      }
      if ($match_201.$tag === 0) {
        const constraints = $match_201.$0[0];
        const s2 = $match_201.$0[1];
        return (($match_202) => {
          if ($match_202.$tag === 1) {
            const e = $match_202.$0;
            return Err2(e);
          }
          if ($match_202.$tag === 0) {
            const nameTok = $match_202.$0[0];
            const s3 = $match_202.$0[1];
            return (($match_203) => {
              if ($match_203.$tag === 1) {
                const e = $match_203.$0;
                return Err2(e);
              }
              if ($match_203.$tag === 0) {
                const params = $match_203.$0[0];
                const s4 = $match_203.$0[1];
                return (($match_204) => {
                  if ($match_204.$tag === 1) {
                    const e = $match_204.$0;
                    return Err2(e);
                  }
                  if ($match_204.$tag === 0) {
                    const s5 = $match_204.$0[1];
                    return (($match_205) => {
                      if ($match_205.$tag === 1) {
                        const e = $match_205.$0;
                        return Err2(e);
                      }
                      if ($match_205.$tag === 0) {
                        const s6 = $match_205.$0[1];
                        return (($match_206) => {
                          if ($match_206.$tag === 1) {
                            const e = $match_206.$0;
                            return Err2(e);
                          }
                          if ($match_206.$tag === 0) {
                            const items = $match_206.$0[0];
                            const s7 = $match_206.$0[1];
                            return (($match_207) => {
                              if ($match_207.$tag === 1) {
                                const e = $match_207.$0;
                                return Err2(e);
                              }
                              if ($match_207.$tag === 0) {
                                const s8 = $match_207.$0[1];
                                return ((methods) => ((endPos) => Ok2([DProtocol(nameTok.lexeme)(params)(constraints)(methods)({ start: protocolTok.span.start, end: endPos }), s8]))((($match_208) => {
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
        })(expect(UpperIdentifier2)("protocol name")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseOptionalConstraints(hasConstraints)(s1)))(peekConstraintContextInProtocol(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("protocol")(state));
parseProtocolItems = (state) => (($match_209) => {
  if ($match_209.$tag === 1) {
    const e = $match_209.$0;
    return Err2(e);
  }
  if ($match_209.$tag === 0) {
    const item = $match_209.$0[0];
    const s1 = $match_209.$0[1];
    return (($match_210) => {
      if ($match_210.$tag === 1) {
        return Ok2([[item], s1]);
      }
      if ($match_210.$tag === 0) {
        const s2 = $match_210.$0;
        return (($match_211) => {
          if ($match_211.$tag === 1) {
            const e = $match_211.$0;
            return Err2(e);
          }
          if ($match_211.$tag === 0) {
            const rest = $match_211.$0[0];
            const s3 = $match_211.$0[1];
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
parseProtocolItem = (state) => ((start) => (($match_212) => {
  if ($match_212.$tag === 1) {
    const e = $match_212.$0;
    return Err2(e);
  }
  if ($match_212.$tag === 0) {
    const methodName = $match_212.$0[0];
    const s1 = $match_212.$0[1];
    return peekKind(Colon2)(s1) ? ((s2) => (($match_213) => {
      if ($match_213.$tag === 1) {
        const e = $match_213.$0;
        return Err2(e);
      }
      if ($match_213.$tag === 0) {
        const methodType = $match_213.$0[0];
        const s3 = $match_213.$0[1];
        return Ok2([PIAnnotation(methodName)(methodType)({ start, end: typeSpan(methodType).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseTypeExpression(s2)))(advance2(s1)) : (($match_214) => {
      if ($match_214.$tag === 1) {
        const e = $match_214.$0;
        return Err2(e);
      }
      if ($match_214.$tag === 0) {
        const args = $match_214.$0[0];
        const s2 = $match_214.$0[1];
        return (($match_215) => {
          if ($match_215.$tag === 1) {
            const e = $match_215.$0;
            return Err2(e);
          }
          if ($match_215.$tag === 0) {
            const s3 = $match_215.$0[1];
            return (($match_216) => {
              if ($match_216.$tag === 1) {
                const e = $match_216.$0;
                return Err2(e);
              }
              if ($match_216.$tag === 0) {
                const body = $match_216.$0[0];
                const s4 = $match_216.$0[1];
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
parseImplementationDeclaration = (state) => (($match_217) => {
  if ($match_217.$tag === 1) {
    const e = $match_217.$0;
    return Err2(e);
  }
  if ($match_217.$tag === 0) {
    const implTok = $match_217.$0[0];
    const s1 = $match_217.$0[1];
    return ((hasConstraints) => (($match_218) => {
      if ($match_218.$tag === 1) {
        const e = $match_218.$0;
        return Err2(e);
      }
      if ($match_218.$tag === 0) {
        const constraints = $match_218.$0[0];
        const s2 = $match_218.$0[1];
        return (($match_219) => {
          if ($match_219.$tag === 1) {
            const e = $match_219.$0;
            return Err2(e);
          }
          if ($match_219.$tag === 0) {
            const protocolTok = $match_219.$0[0];
            const s3 = $match_219.$0[1];
            return (($match_220) => {
              if ($match_220.$tag === 1) {
                const e = $match_220.$0;
                return Err2(e);
              }
              if ($match_220.$tag === 0) {
                const typeArgs = $match_220.$0[0];
                const s4 = $match_220.$0[1];
                return (($match_221) => {
                  if ($match_221.$tag === 1) {
                    const e = $match_221.$0;
                    return Err2(e);
                  }
                  if ($match_221.$tag === 0) {
                    const methods = $match_221.$0[0];
                    const s5 = $match_221.$0[1];
                    return ((endPos) => Ok2([DImplementation(constraints)(protocolTok.lexeme)(typeArgs)(methods)({ start: implTok.span.start, end: endPos }), s5]))((($match_222) => {
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
        })(expect(UpperIdentifier2)("protocol name")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseOptionalConstraints(hasConstraints)(s1)))(peekConstraintContext(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("implement")(state));
parseImplementationMethods = (state) => peekKeyword("where")(state) ? (($match_224) => {
  if ($match_224.$tag === 1) {
    const e = $match_224.$0;
    return Err2(e);
  }
  if ($match_224.$tag === 0) {
    const s1 = $match_224.$0[1];
    return (($match_225) => {
      if ($match_225.$tag === 1) {
        const e = $match_225.$0;
        return Err2(e);
      }
      if ($match_225.$tag === 0) {
        const s2 = $match_225.$0[1];
        return (($match_226) => {
          if ($match_226.$tag === 1) {
            const e = $match_226.$0;
            return Err2(e);
          }
          if ($match_226.$tag === 0) {
            const methods = $match_226.$0[0];
            const s3 = $match_226.$0[1];
            return (($match_227) => {
              if ($match_227.$tag === 1) {
                const e = $match_227.$0;
                return Err2(e);
              }
              if ($match_227.$tag === 0) {
                const s4 = $match_227.$0[1];
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
parseImplMethodList = (state) => (($match_228) => {
  if ($match_228.$tag === 1) {
    const e = $match_228.$0;
    return Err2(e);
  }
  if ($match_228.$tag === 0) {
    const method = $match_228.$0[0];
    const s1 = $match_228.$0[1];
    return (($match_229) => {
      if ($match_229.$tag === 1) {
        return Ok2([[method], s1]);
      }
      if ($match_229.$tag === 0) {
        const s2 = $match_229.$0;
        return (($match_230) => {
          if ($match_230.$tag === 1) {
            const e = $match_230.$0;
            return Err2(e);
          }
          if ($match_230.$tag === 0) {
            const rest = $match_230.$0[0];
            const s3 = $match_230.$0[1];
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
parseImplMethod = (state) => (($match_231) => {
  if ($match_231.$tag === 1) {
    const e = $match_231.$0;
    return Err2(e);
  }
  if ($match_231.$tag === 0) {
    const nameResult = $match_231.$0[0];
    const s1 = $match_231.$0[1];
    return (($match_232) => {
      if ($match_232.$tag === 1) {
        const e = $match_232.$0;
        return Err2(e);
      }
      if ($match_232.$tag === 0) {
        const result = $match_232.$0[0];
        const s2 = $match_232.$0[1];
        return Ok2([{ name: nameResult.declName, implArgs: result.mArgs, implementation: result.mBody, span: { start: nameResult.declSpan.start, end: exprSpan(result.mBody).end } }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseMethodBody(Nothing2)(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclarationName(state));
parseExpression = (baseIndentFloor) => (state) => parseBinaryExpression(0)(baseIndentFloor)(state);
parseBinaryExpression = (minPrec) => (baseIndentFloor) => (state) => (($match_233) => {
  if ($match_233.$tag === 1) {
    const e = $match_233.$0;
    return Err2(e);
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
                    const nextMin = (($match_234) => {
                      if ($match_234.$tag === 1) {
                        return prec;
                      }
                      {
                        return $dict_Num_Int2._PLUS(prec)(1);
                      }
                      throw new Error("Pattern match failed");
                    })(info.associativity);
                    {
                      const $match_235 = parseBinaryExpression(nextMin)(baseIndentFloor)(s1);
                      if ($match_235.$tag === 1) {
                        const e = $match_235.$0;
                        return Err2(e);
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
        return Ok2([left, state]);
      }
    }
  }
};
parseUnary = (baseIndentFloor) => (state) => ((tok) => _AMP_AMP6($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Operator2))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("-")) ? (($match_236) => {
  if ($match_236.$tag === 1) {
    return parseApplication(baseIndentFloor)(state);
  }
  if ($match_236.$tag === 0) {
    const nextTok = $match_236.$0;
    return _PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(LowerIdentifier2))(() => _PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(UpperIdentifier2))(() => _PIPE_PIPE7($dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(NumberToken2))(() => $dict_Eq_TokenKind._EQ_EQ(nextTok.kind)(LParen2)))) ? ((s1) => (($match_237) => {
      if ($match_237.$tag === 1) {
        const e = $match_237.$0;
        return Err2(e);
      }
      if ($match_237.$tag === 0) {
        const operand = $match_237.$0[0];
        const s2 = $match_237.$0[1];
        return Ok2([EUnary("-")(operand)({ start: tok.span.start, end: exprSpan(operand).end }), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseUnary(baseIndentFloor)(s1)))(advance2(state)) : parseApplication(baseIndentFloor)(state);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(state)) : parseApplication(baseIndentFloor)(state))(current(state));
parseApplication = (baseIndentFloor) => (state) => (($match_238) => {
  if ($match_238.$tag === 1) {
    const e = $match_238.$0;
    return Err2(e);
  }
  if ($match_238.$tag === 0) {
    const callee = $match_238.$0[0];
    const s1 = $match_238.$0[1];
    return ((baseIndent) => ((effectiveIndent) => (($match_239) => {
      if ($match_239.$tag === 1) {
        const e = $match_239.$0;
        return Err2(e);
      }
      if ($match_239.$tag === 0) {
        const args = $match_239.$0[0];
        const s2 = $match_239.$0[1];
        return isEmpty2(args) ? Ok2([callee, s2]) : ((endPos) => Ok2([EApply(callee)(args)({ start: exprSpan(callee).start, end: endPos }), s2]))((($match_240) => {
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
    return Err2(e);
  }
  if ($match_242.$tag === 0) {
    const arg = $match_242.$0[0];
    const s1 = $match_242.$0[1];
    return (($match_243) => {
      if ($match_243.$tag === 1) {
        const e = $match_243.$0;
        return Err2(e);
      }
      if ($match_243.$tag === 0) {
        const rest = $match_243.$0[0];
        const s2 = $match_243.$0[1];
        return Ok2([_COLON_COLON2(arg)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseApplicationArgs(effectiveIndent)(exprSpan(arg).end)(s1));
  }
  throw new Error("Pattern match failed");
})(parsePrimaryWithAccess(state)) : Ok2([[], state]) : Ok2([[], state]);
parsePrimaryWithAccess = (state) => (($match_244) => {
  if ($match_244.$tag === 1) {
    const e = $match_244.$0;
    return Err2(e);
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
    return ((s1) => Ok2([EVar(tok.lexeme)("lower")(tok.span), s1]))(advance2(state));
  }
  if ($match_245.$tag === 1) {
    return ((s1) => Ok2([EVar(tok.lexeme)("upper")(tok.span), s1]))(advance2(state));
  }
  if ($match_245.$tag === 3) {
    return ((s1) => contains(".")(tok.lexeme) ? Ok2([EFloat(tok.lexeme)(tok.span), s1]) : Ok2([EInt(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_245.$tag === 4) {
    return ((s1) => Ok2([EString(tok.lexeme)(tok.span), s1]))(advance2(state));
  }
  if ($match_245.$tag === 5) {
    return ((s1) => Ok2([EChar(tok.lexeme)(tok.span), s1]))(advance2(state));
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
    return Err2(makeError($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS($dict_Appendable_String2._PLUS_PLUS("Expected expression but found ")(tokenKindStr(tok.kind)))(" '"))(tok.lexeme))("'"))(tok.span));
  }
  throw new Error("Pattern match failed");
})(tok.kind))(current(state));
parseLambda = (state) => ((start) => ((s1) => (($match_246) => {
  if ($match_246.$tag === 1) {
    const e = $match_246.$0;
    return Err2(e);
  }
  if ($match_246.$tag === 0) {
    const args = $match_246.$0[0];
    const s2 = $match_246.$0[1];
    return (($match_247) => {
      if ($match_247.$tag === 1) {
        const e = $match_247.$0;
        return Err2(e);
      }
      if ($match_247.$tag === 0) {
        const body = $match_247.$0[0];
        const s3 = $match_247.$0[1];
        return Ok2([ELambda(args)(body)({ start, end: exprSpan(body).end }), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Nothing2)(s2));
  }
  throw new Error("Pattern match failed");
})(parseLambdaArgs(s1)))(advance2(state)))(current(state).span.start);
parseIf = (state) => (($match_248) => {
  if ($match_248.$tag === 1) {
    const e = $match_248.$0;
    return Err2(e);
  }
  if ($match_248.$tag === 0) {
    const ifTok = $match_248.$0[0];
    const s1 = $match_248.$0[1];
    return (($match_249) => {
      if ($match_249.$tag === 1) {
        const e = $match_249.$0;
        return Err2(e);
      }
      if ($match_249.$tag === 0) {
        const condition = $match_249.$0[0];
        const s2 = $match_249.$0[1];
        return (($match_250) => {
          if ($match_250.$tag === 1) {
            const e = $match_250.$0;
            return Err2(e);
          }
          if ($match_250.$tag === 0) {
            const s3 = $match_250.$0[1];
            return (($match_251) => {
              if ($match_251.$tag === 1) {
                const e = $match_251.$0;
                return Err2(e);
              }
              if ($match_251.$tag === 0) {
                const thenBranch = $match_251.$0[0];
                const s4 = $match_251.$0[1];
                return (($match_252) => {
                  if ($match_252.$tag === 1) {
                    const e = $match_252.$0;
                    return Err2(e);
                  }
                  if ($match_252.$tag === 0) {
                    const s5 = $match_252.$0[1];
                    return (($match_253) => {
                      if ($match_253.$tag === 1) {
                        const e = $match_253.$0;
                        return Err2(e);
                      }
                      if ($match_253.$tag === 0) {
                        const elseBranch = $match_253.$0[0];
                        const s6 = $match_253.$0[1];
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
parseLetIn = (state) => (($match_254) => {
  if ($match_254.$tag === 1) {
    const e = $match_254.$0;
    return Err2(e);
  }
  if ($match_254.$tag === 0) {
    const letTok = $match_254.$0[0];
    const s1 = $match_254.$0[1];
    return (($match_255) => {
      if ($match_255.$tag === 1) {
        const e = $match_255.$0;
        return Err2(e);
      }
      if ($match_255.$tag === 0) {
        const s2 = $match_255.$0[1];
        return (($match_256) => {
          if ($match_256.$tag === 1) {
            const e = $match_256.$0;
            return Err2(e);
          }
          if ($match_256.$tag === 0) {
            const bindings = $match_256.$0[0];
            const s3 = $match_256.$0[1];
            return (($match_257) => {
              if ($match_257.$tag === 1) {
                const e = $match_257.$0;
                return Err2(e);
              }
              if ($match_257.$tag === 0) {
                const s4 = $match_257.$0[1];
                return (($match_258) => {
                  if ($match_258.$tag === 1) {
                    const e = $match_258.$0;
                    return Err2(e);
                  }
                  if ($match_258.$tag === 0) {
                    const s5 = $match_258.$0[1];
                    return (($match_259) => {
                      if ($match_259.$tag === 1) {
                        const e = $match_259.$0;
                        return Err2(e);
                      }
                      if ($match_259.$tag === 0) {
                        const body = $match_259.$0[0];
                        const s6 = $match_259.$0[1];
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
parseLetBindings = (state) => (($match_260) => {
  if ($match_260.$tag === 1) {
    const e = $match_260.$0;
    return Err2(e);
  }
  if ($match_260.$tag === 0) {
    const binding = $match_260.$0[0];
    const s1 = $match_260.$0[1];
    return (($match_261) => {
      if ($match_261.$tag === 1) {
        return Ok2([[binding], s1]);
      }
      if ($match_261.$tag === 0) {
        const s2 = $match_261.$0;
        return (($match_262) => {
          if ($match_262.$tag === 1) {
            const e = $match_262.$0;
            return Err2(e);
          }
          if ($match_262.$tag === 0) {
            const rest = $match_262.$0[0];
            const s3 = $match_262.$0[1];
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
parseLetBinding = (state) => (($match_263) => {
  if ($match_263.$tag === 1) {
    const e = $match_263.$0;
    return Err2(e);
  }
  if ($match_263.$tag === 0) {
    const decl = $match_263.$0[0];
    const s1 = $match_263.$0[1];
    return Ok2([declToValueDecl(decl), s1]);
  }
  throw new Error("Pattern match failed");
})(parseDeclaration(state));
parseCase = (state) => (($match_264) => {
  if ($match_264.$tag === 1) {
    const e = $match_264.$0;
    return Err2(e);
  }
  if ($match_264.$tag === 0) {
    const caseTok = $match_264.$0[0];
    const s1 = $match_264.$0[1];
    return (($match_265) => {
      if ($match_265.$tag === 1) {
        const e = $match_265.$0;
        return Err2(e);
      }
      if ($match_265.$tag === 0) {
        const discriminant = $match_265.$0[0];
        const s2 = $match_265.$0[1];
        return (($match_266) => {
          if ($match_266.$tag === 1) {
            const e = $match_266.$0;
            return Err2(e);
          }
          if ($match_266.$tag === 0) {
            const s3 = $match_266.$0[1];
            return (($match_267) => {
              if ($match_267.$tag === 1) {
                const e = $match_267.$0;
                return Err2(e);
              }
              if ($match_267.$tag === 0) {
                const s4 = $match_267.$0[1];
                return (($match_268) => {
                  if ($match_268.$tag === 1) {
                    const e = $match_268.$0;
                    return Err2(e);
                  }
                  if ($match_268.$tag === 0) {
                    const branches = $match_268.$0[0];
                    const s5 = $match_268.$0[1];
                    return (($match_269) => {
                      if ($match_269.$tag === 1) {
                        const e = $match_269.$0;
                        return Err2(e);
                      }
                      if ($match_269.$tag === 0) {
                        const s6 = $match_269.$0[1];
                        return ((endPos) => Ok2([ECase(discriminant)(branches)({ start: caseTok.span.start, end: endPos }), s6]))((($match_270) => {
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
    })(parseExpression(Nothing2)(s1));
  }
  throw new Error("Pattern match failed");
})(expectKeyword("case")(state));
parseCaseBranches = (state) => (($match_271) => {
  if ($match_271.$tag === 1) {
    const e = $match_271.$0;
    return Err2(e);
  }
  if ($match_271.$tag === 0) {
    const branch = $match_271.$0[0];
    const s1 = $match_271.$0[1];
    return (($match_272) => {
      if ($match_272.$tag === 1) {
        return Ok2([[branch], s1]);
      }
      if ($match_272.$tag === 0) {
        const s2 = $match_272.$0;
        return (($match_273) => {
          if ($match_273.$tag === 1) {
            const e = $match_273.$0;
            return Err2(e);
          }
          if ($match_273.$tag === 0) {
            const rest = $match_273.$0[0];
            const s3 = $match_273.$0[1];
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
parseCaseBranch = (state) => (($match_274) => {
  if ($match_274.$tag === 1) {
    const e = $match_274.$0;
    return Err2(e);
  }
  if ($match_274.$tag === 0) {
    const pat = $match_274.$0[0];
    const s1 = $match_274.$0[1];
    return (($match_275) => {
      if ($match_275.$tag === 1) {
        const e = $match_275.$0;
        return Err2(e);
      }
      if ($match_275.$tag === 0) {
        const s2 = $match_275.$0[1];
        return ((branchIndent) => (($match_276) => {
          if ($match_276.$tag === 1) {
            const e = $match_276.$0;
            return Err2(e);
          }
          if ($match_276.$tag === 0) {
            const body = $match_276.$0[0];
            const s3 = $match_276.$0[1];
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
parseParenExpr = (state) => ((start) => ((s1) => peekKind(RParen2)(s1) ? ((s2) => Ok2([EUnit({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : peekKind(Operator2)(s1) ? (($match_277) => {
  if ($match_277.$tag === 0) {
    const next = $match_277.$0;
    return $dict_Eq_TokenKind._EQ_EQ(next.kind)(RParen2) ? ((opTok) => ((s2) => ((s3) => Ok2([EVar(opTok.lexeme)("operator")({ start, end: previous(s3).span.end }), s3]))(advance2(s2)))(advance2(s1)))(current(s1)) : parseParenExprContents(start)(s1);
  }
  if ($match_277.$tag === 1) {
    return parseParenExprContents(start)(s1);
  }
  throw new Error("Pattern match failed");
})(peekAhead(1)(s1)) : parseParenExprContents(start)(s1))(advance2(state)))(current(state).span.start);
parseParenExprContents = (start) => (state) => (($match_278) => {
  if ($match_278.$tag === 1) {
    const e = $match_278.$0;
    return Err2(e);
  }
  if ($match_278.$tag === 0) {
    const first = $match_278.$0[0];
    const s1 = $match_278.$0[1];
    return (($match_279) => {
      if ($match_279.$tag === 1) {
        const e = $match_279.$0;
        return Err2(e);
      }
      if ($match_279.$tag === 0) {
        const more = $match_279.$0[0];
        const s2 = $match_279.$0[1];
        return (($match_280) => {
          if ($match_280.$tag === 1) {
            const e = $match_280.$0;
            return Err2(e);
          }
          if ($match_280.$tag === 0) {
            const cp = $match_280.$0[0];
            const s3 = $match_280.$0[1];
            return isEmpty2(more) ? Ok2([EParen(first)({ start, end: cp.span.end }), s3]) : Ok2([ETuple(_COLON_COLON2(first)(more))({ start, end: cp.span.end }), s3]);
          }
          throw new Error("Pattern match failed");
        })(expect(RParen2)("close group")(s2));
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleExprs(s1));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing2)(state));
parseMoreTupleExprs = (state) => peekKind(Comma2)(state) ? ((s1) => (($match_281) => {
  if ($match_281.$tag === 1) {
    const e = $match_281.$0;
    return Err2(e);
  }
  if ($match_281.$tag === 0) {
    const expr = $match_281.$0[0];
    const s2 = $match_281.$0[1];
    return (($match_282) => {
      if ($match_282.$tag === 1) {
        const e2 = $match_282.$0;
        return Err2(e2);
      }
      if ($match_282.$tag === 0) {
        const rest = $match_282.$0[0];
        const s3 = $match_282.$0[1];
        return Ok2([_COLON_COLON2(expr)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreTupleExprs(s2));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing2)(s1)))(advance2(state)) : Ok2([[], state]);
parseListExpr = (state) => ((start) => ((s1) => peekKind(RBracket2)(s1) ? ((s2) => Ok2([EList([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_283) => {
  if ($match_283.$tag === 1) {
    const e = $match_283.$0;
    return Err2(e);
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
            return Err2(e);
          }
          if ($match_285.$tag === 0) {
            const endExpr = $match_285.$0[0];
            const s4 = $match_285.$0[1];
            return (($match_286) => {
              if ($match_286.$tag === 1) {
                const e = $match_286.$0;
                return Err2(e);
              }
              if ($match_286.$tag === 0) {
                const s5 = $match_286.$0[1];
                return Ok2([EListRange(first)(endExpr)({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBracket2)("close list range")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseExpression(Nothing2)(s3));
      }
      if ($match_284.$tag === 1) {
        return (($match_287) => {
          if ($match_287.$tag === 1) {
            const e = $match_287.$0;
            return Err2(e);
          }
          if ($match_287.$tag === 0) {
            const more = $match_287.$0[0];
            const s4 = $match_287.$0[1];
            return (($match_288) => {
              if ($match_288.$tag === 1) {
                const e = $match_288.$0;
                return Err2(e);
              }
              if ($match_288.$tag === 0) {
                const s5 = $match_288.$0[1];
                return Ok2([EList(_COLON_COLON2(first)(more))({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBracket2)("close list")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseMoreListExprs(s2));
      }
      throw new Error("Pattern match failed");
    })(matchKind(Range2)(s2));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing2)(s1)))(advance2(state)))(current(state).span.start);
parseMoreListExprs = (state) => peekKind(Comma2)(state) ? ((s1) => (($match_289) => {
  if ($match_289.$tag === 1) {
    const e = $match_289.$0;
    return Err2(e);
  }
  if ($match_289.$tag === 0) {
    const expr = $match_289.$0[0];
    const s2 = $match_289.$0[1];
    return (($match_290) => {
      if ($match_290.$tag === 1) {
        const e2 = $match_290.$0;
        return Err2(e2);
      }
      if ($match_290.$tag === 0) {
        const rest = $match_290.$0[0];
        const s3 = $match_290.$0[1];
        return Ok2([_COLON_COLON2(expr)(rest), s3]);
      }
      throw new Error("Pattern match failed");
    })(parseMoreListExprs(s2));
  }
  throw new Error("Pattern match failed");
})(parseExpression(Nothing2)(s1)))(advance2(state)) : Ok2([[], state]);
parseRecordExpr = (state) => ((start) => ((s1) => peekKind(RBrace2)(s1) ? ((s2) => Ok2([ERecord([])({ start, end: previous(s2).span.end }), s2]))(advance2(s1)) : (($match_291) => {
  if ($match_291.$tag === 1) {
    const e = $match_291.$0;
    return Err2(e);
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
            return Err2(e);
          }
          if ($match_293.$tag === 0) {
            const fields = $match_293.$0[0];
            const s4 = $match_293.$0[1];
            return (($match_294) => {
              if ($match_294.$tag === 1) {
                const e = $match_294.$0;
                return Err2(e);
              }
              if ($match_294.$tag === 0) {
                const s5 = $match_294.$0[1];
                return Ok2([ERecordUpdate(head2.lexeme)(fields)({ start, end: previous(s5).span.end }), s5]);
              }
              throw new Error("Pattern match failed");
            })(expect(RBrace2)("close record update")(s4));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldList(Nothing2)(head2.span.start.column)(s3));
      }
      if ($match_292.$tag === 1) {
        return (($match_295) => {
          if ($match_295.$tag === 1) {
            const e = $match_295.$0;
            return Err2(e);
          }
          if ($match_295.$tag === 0) {
            const fields = $match_295.$0[0];
            const s4 = $match_295.$0[1];
            return (($match_296) => {
              if ($match_296.$tag === 1) {
                const e = $match_296.$0;
                return Err2(e);
              }
              if ($match_296.$tag === 0) {
                const s5 = $match_296.$0[1];
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
parseRecordFieldList = (firstFieldToken) => (baseIndent) => (state) => (($match_297) => {
  if ($match_297.$tag === 0) {
    const tok = $match_297.$0;
    return (($match_298) => {
      if ($match_298.$tag === 1) {
        const e = $match_298.$0;
        return Err2(e);
      }
      if ($match_298.$tag === 0) {
        const firstField = $match_298.$0[0];
        const s1 = $match_298.$0[1];
        return (($match_299) => {
          if ($match_299.$tag === 1) {
            const e = $match_299.$0;
            return Err2(e);
          }
          if ($match_299.$tag === 0) {
            const rest = $match_299.$0[0];
            const s2 = $match_299.$0[1];
            return Ok2([_COLON_COLON2(firstField)(rest), s2]);
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
        return Err2(e);
      }
      if ($match_300.$tag === 0) {
        const tok = $match_300.$0[0];
        const s1 = $match_300.$0[1];
        return (($match_301) => {
          if ($match_301.$tag === 1) {
            const e = $match_301.$0;
            return Err2(e);
          }
          if ($match_301.$tag === 0) {
            const firstField = $match_301.$0[0];
            const s2 = $match_301.$0[1];
            return (($match_302) => {
              if ($match_302.$tag === 1) {
                const e = $match_302.$0;
                return Err2(e);
              }
              if ($match_302.$tag === 0) {
                const rest = $match_302.$0[0];
                const s3 = $match_302.$0[1];
                return Ok2([_COLON_COLON2(firstField)(rest), s3]);
              }
              throw new Error("Pattern match failed");
            })(parseMoreRecordFields(baseIndent)(s2));
          }
          throw new Error("Pattern match failed");
        })(parseRecordFieldValue(tok)(baseIndent)(s1));
      }
      throw new Error("Pattern match failed");
    })(expect(LowerIdentifier2)("record field name")(state));
  }
  throw new Error("Pattern match failed");
})(firstFieldToken);
parseRecordFieldValue = (fieldTok) => (baseIndent) => (state) => (($match_303) => {
  if ($match_303.$tag === 1) {
    const e = $match_303.$0;
    return Err2(e);
  }
  if ($match_303.$tag === 0) {
    const s1 = $match_303.$0[1];
    return (($match_304) => {
      if ($match_304.$tag === 1) {
        const e = $match_304.$0;
        return Err2(e);
      }
      if ($match_304.$tag === 0) {
        const value = $match_304.$0[0];
        const s2 = $match_304.$0[1];
        return Ok2([{ name: fieldTok.lexeme, value, span: { start: fieldTok.span.start, end: exprSpan(value).end } }, s2]);
      }
      throw new Error("Pattern match failed");
    })(parseExpression(Just2(baseIndent))(s1));
  }
  throw new Error("Pattern match failed");
})(expect(Equals2)("record field assignment")(state));
parseMoreRecordFields = (baseIndent) => (state) => peekKind(Comma2)(state) ? ((s1) => (($match_305) => {
  if ($match_305.$tag === 1) {
    const e = $match_305.$0;
    return Err2(e);
  }
  if ($match_305.$tag === 0) {
    const fieldTok = $match_305.$0[0];
    const s2 = $match_305.$0[1];
    return (($match_306) => {
      if ($match_306.$tag === 1) {
        const e = $match_306.$0;
        return Err2(e);
      }
      if ($match_306.$tag === 0) {
        const field = $match_306.$0[0];
        const s3 = $match_306.$0[1];
        return (($match_307) => {
          if ($match_307.$tag === 1) {
            const e = $match_307.$0;
            return Err2(e);
          }
          if ($match_307.$tag === 0) {
            const rest = $match_307.$0[0];
            const s4 = $match_307.$0[1];
            return Ok2([_COLON_COLON2(field)(rest), s4]);
          }
          throw new Error("Pattern match failed");
        })(parseMoreRecordFields(baseIndent)(s3));
      }
      throw new Error("Pattern match failed");
    })(parseRecordFieldValue(fieldTok)(baseIndent)(s2));
  }
  throw new Error("Pattern match failed");
})(expect(LowerIdentifier2)("record field name")(s1)))(advance2(state)) : Ok2([[], state]);
var parseDeclarations = (state) => isAtEnd2(state) ? Ok2([[], state]) : ((tok) => $dict_Eq_Int2._EQ_EQ(tok.span.start.column)(1) ? (($match_308) => {
  if ($match_308.$tag === 1) {
    const e = $match_308.$0;
    return Err2(e);
  }
  if ($match_308.$tag === 0) {
    const decl = $match_308.$0[0];
    const s1 = $match_308.$0[1];
    return (($match_309) => {
      if ($match_309.$tag === 1) {
        const e = $match_309.$0;
        return Err2(e);
      }
      if ($match_309.$tag === 0) {
        const rest = $match_309.$0[0];
        const s2 = $match_309.$0[1];
        return Ok2([_COLON_COLON2(decl)(rest), s2]);
      }
      throw new Error("Pattern match failed");
    })(parseDeclarations(s1));
  }
  throw new Error("Pattern match failed");
})(parseDeclaration(state)) : Err2(makeError("top-level declaration must start at column 1")(tok.span)))(current(state));
var parseProgram = (state) => ((tok) => $dict_Eq_Int2._EQ_EQ(tok.span.start.column)(1) ? (($match_310) => {
  if ($match_310.$tag === 1) {
    const e = $match_310.$0;
    return Err2(e);
  }
  if ($match_310.$tag === 0) {
    const modDecl = $match_310.$0[0];
    const s1 = $match_310.$0[1];
    return (($match_311) => {
      if ($match_311.$tag === 1) {
        const e = $match_311.$0;
        return Err2(e);
      }
      if ($match_311.$tag === 0) {
        const imports = $match_311.$0[0];
        const s2 = $match_311.$0[1];
        return (($match_312) => {
          if ($match_312.$tag === 1) {
            const e = $match_312.$0;
            return Err2(e);
          }
          if ($match_312.$tag === 0) {
            const decls = $match_312.$0[0];
            const s3 = $match_312.$0[1];
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
var parseTokens = (tokens) => (registry) => ((state) => (($match_313) => {
  if ($match_313.$tag === 1) {
    const e = $match_313.$0;
    return Err2(e);
  }
  if ($match_313.$tag === 0) {
    const prog = $match_313.$0[0];
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
var isLetContext = (ctx) => $dict_Eq_String2._EQ_EQ(ctx.keyword)("let");
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
        return Nothing2;
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
      const $match_8 = stackTop(state.stack);
      if ($match_8.$tag === 1) {
        return state;
      }
      if ($match_8.$tag === 0) {
        const ctx = $match_8.$0;
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
      const $match_9 = stackTop(state.stack);
      if ($match_9.$tag === 1) {
        return state;
      }
      if ($match_9.$tag === 0) {
        const ctx = $match_9.$0;
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
      const $match_10 = stackTop(state.stack);
      if ($match_10.$tag === 1) {
        return state;
      }
      if ($match_10.$tag === 0) {
        const ctx = $match_10.$0;
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
      const $match_11 = stackTop(state.stack);
      if ($match_11.$tag === 1) {
        return state;
      }
      if ($match_11.$tag === 0) {
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
var _AMP_AMP7 = (a) => (b) => a && b();
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
var decodeString = (decoder) => (str) => (($match_37) => {
  if ($match_37.$tag === 1) {
    const msg = $match_37.$0;
    return Err2($dict_Appendable_String2._PLUS_PLUS("Invalid JSON: ")(msg));
  }
  if ($match_37.$tag === 0) {
    const json = $match_37.$0;
    return (($match_38) => {
      if ($match_38.$tag === 0) {
        const val = $match_38.$0;
        return Ok2(val);
      }
      if ($match_38.$tag === 1) {
        const err = $match_38.$0;
        return Err2(errorToString(err));
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
    return _AMP_AMP7($dict_Eq_String2._EQ_EQ(a_0)(b_0))(() => $dict_Eq_Error._EQ_EQ(a_1)(b_1));
  }
  if ($match_39[0].$tag === 1 && $match_39[1].$tag === 1) {
    const a_0 = $match_39[0].$0;
    const a_1 = $match_39[0].$1;
    const b_0 = $match_39[1].$0;
    const b_1 = $match_39[1].$1;
    return _AMP_AMP7($dict_Eq_Int2._EQ_EQ(a_0)(b_0))(() => $dict_Eq_Error._EQ_EQ(a_1)(b_1));
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
var _PIPE_PIPE8 = (a) => (b) => a || b();
var _AMP_AMP8 = (a) => (b) => a && b();
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
var isIdentifierStart2 = (c) => _PIPE_PIPE8(isAlpha2(c))(() => $dict_Eq_Char2._EQ_EQ(c)("_"));
var isIdentifierPart2 = (c) => _PIPE_PIPE8(isIdentifierStart2(c))(() => isDigit2(c));
var isWhitespace2 = (c) => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)(" "))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("\t"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)(`
`))(() => $dict_Eq_Char2._EQ_EQ(c)("\r"))));
var isOperatorChar2 = (c) => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("!"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("#"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("$"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("%"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("&"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("*"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("+"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("."))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("/"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("<"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("="))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)(">"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("?"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("@"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("\\"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("^"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("|"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)("~"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)(":"))(() => $dict_Eq_Char2._EQ_EQ(c)("-"))))))))))))))))))));
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
skipWsDispatch2 = (sawNl) => (state) => (c) => isWhitespace2(c) ? ((nl) => skipWhitespaceAndComments2(nl)(skip3(state)))(_PIPE_PIPE8(sawNl)(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(c)(`
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
            return ((nl) => skipWhitespaceAndComments2(nl)(s3))(_PIPE_PIPE8(sawNl)(() => $dict_Ord_Int2._GT(s3.line)(lineBefore)));
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
        if (_PIPE_PIPE8(isIdentifierPart2(c))(() => $dict_Eq_Char2._EQ_EQ(c)("'"))) {
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
var isValidStringEscape2 = (esc) => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(esc)("n"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(esc)("r"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(esc)("t"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(esc)('"'))(() => $dict_Eq_Char2._EQ_EQ(esc)("\\")))));
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
var isValidCharEscape2 = (esc) => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(esc)("n"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(esc)("r"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(esc)("t"))(() => _PIPE_PIPE8($dict_Eq_Char2._EQ_EQ(esc)("'"))(() => $dict_Eq_Char2._EQ_EQ(esc)("\\")))));
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
var maybeInsertNewline2 = (tokens) => (sawNl) => (hasEmitted) => (state) => _AMP_AMP8(sawNl)(() => hasEmitted) ? ((nlPos) => ((nlToken) => _COLON_COLON2(nlToken)(tokens))({ kind: Newline2, lexeme: `
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
var parseToJson = (result) => (($match_8) => {
  if ($match_8.$tag === 0) {
    const prog = $match_8.$0;
    return encode2(0)(object2([["ok", bool2(true)], ["program", encodeProgram(prog)]]));
  }
  if ($match_8.$tag === 1) {
    const err = $match_8.$0;
    return encode2(0)(object2([["ok", bool2(false)], ["message", string2(err.message)], ["span", encodeSpan2(err.span)]]));
  }
  throw new Error("Pattern match failed");
})(result);
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
var _AMP_AMP9 = (a) => (b) => a && b();
var _PIPE_PIPE9 = (a) => (b) => a || b();
var parse = (source) => ((rawTokens) => (($match_0) => {
  if ($match_0.$tag === 1) {
    const lexErr = $match_0.$0;
    return Err2({ message: lexErr.message, span: { start: lexErr.span.start, end: lexErr.span.end } });
  }
  if ($match_0.$tag === 0) {
    const tokens = $match_0.$0;
    return ((layoutTokens) => parseTokens(layoutTokens)(builtinRegistry))(insertLayoutTokens(tokens));
  }
  throw new Error("Pattern match failed");
})(rawTokens))(lex3(source));
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
        if (_AMP_AMP9($dict_Eq_TokenKind._EQ_EQ(tok.kind)(Keyword2))(() => _PIPE_PIPE9($dict_Eq_String2._EQ_EQ(tok.lexeme)("infix"))(() => _PIPE_PIPE9($dict_Eq_String2._EQ_EQ(tok.lexeme)("infixl"))(() => $dict_Eq_String2._EQ_EQ(tok.lexeme)("infixr"))))) {
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
var parseToJson2 = (source) => parseToJson(parse(source));
var encodeError = (message) => (span) => encode2(0)(object2([["ok", bool2(false)], ["message", string2(message)], ["span", encodeSpan2(span)]]));
var collectInfixToJson = (source) => (($match_12) => {
  if ($match_12.$tag === 1) {
    return encode2(0)(object2([["registry", list2(identity2)([])]]));
  }
  if ($match_12.$tag === 0) {
    const tokens = $match_12.$0;
    return ((registry) => encode2(0)(object2([["registry", encodeRegistry(registry)]])))(collectInfixFromTokens(tokens)(emptyRegistry));
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
var decodeRegistryJson = (json) => (($match_14) => {
  if ($match_14.$tag === 0) {
    const entries = $match_14.$0;
    return Ok2(fromList($dict_Ord_String2)(entries));
  }
  if ($match_14.$tag === 1) {
    const err = $match_14.$0;
    return Err2(err);
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
            const program2 = $match_17.$0;
            return encode2(0)(object2([["ok", bool2(true)], ["program", encodeProgram(program2)]]));
          }
          throw new Error("Pattern match failed");
        })(parseTokens(layoutTokens)(mergedRegistry)))(insertLayoutTokens(tokens));
      }
      throw new Error("Pattern match failed");
    })(lex3(source)))(mergeRegistries(builtinRegistry)(externalRegistry));
  }
  throw new Error("Pattern match failed");
})(decodeRegistryJson(registryJson));

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
  const program2 = result.program;
  if (program2.module && program2.module.exposing === undefined) {
    program2.module.exposing = null;
  }
  return program2;
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

// ../syntax/src/operators.ts
var OPERATOR_CHARS = new Set(Array.from("!#$%&*+./<=>?@%\\^|~:-"));
var CHAR_TO_IDENTIFIER = {
  ".": "_DOT",
  "+": "_PLUS",
  "-": "_MINUS",
  "*": "_STAR",
  "/": "_SLASH",
  "%": "_PERCENT",
  "^": "_CARET",
  "<": "_LT",
  ">": "_GT",
  "=": "_EQ",
  "|": "_PIPE",
  "&": "_AMP",
  "!": "_BANG",
  ":": "_COLON",
  "~": "_TILDE",
  $: "_DOLLAR",
  "#": "_HASH",
  "@": "_AT",
  "?": "_QUESTION",
  "\\": "_BACKSLASH"
};
function sanitizeOperator(lexeme) {
  if (lexeme.length === 0)
    return lexeme;
  const parts = [];
  for (const char of lexeme) {
    const mapping = CHAR_TO_IDENTIFIER[char];
    if (mapping) {
      parts.push(mapping);
    } else {
      return lexeme;
    }
  }
  return parts.join("");
}
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
  "infix",
  "infixl",
  "infixr",
  "protocol",
  "implement",
  "where"
];
var KEYWORD_SET = new Set(KEYWORDS);
var BUILTIN_MODULE_NAME = "__builtin__";
var BOOL_TYPE_NAME = "Bool";
var UNIT_TYPE_NAME = "Unit";

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
  let current2 = type;
  while (current2.kind === "var") {
    const replacement = substitution.get(current2.id);
    if (!replacement)
      return current2;
    current2 = replacement;
  }
  if (current2.kind === "fun") {
    return fn(applySubstitution(current2.from, substitution), applySubstitution(current2.to, substitution));
  }
  if (current2.kind === "tuple") {
    return {
      kind: "tuple",
      elements: current2.elements.map((t) => applySubstitution(t, substitution))
    };
  }
  if (current2.kind === "record") {
    const fields = {};
    for (const [k, v] of Object.entries(current2.fields)) {
      fields[k] = applySubstitution(v, substitution);
    }
    return { kind: "record", fields };
  }
  if (current2.kind === "con") {
    return {
      kind: "con",
      name: current2.name,
      args: current2.args.map((t) => applySubstitution(t, substitution))
    };
  }
  return current2;
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
  _pendingConstrainedCallUsages = [];
  _resolvedConstrainedCallUsages = new Map;
  _registry = new RegistryManager;
  _globalScope = new Scope;
  _substitution = new Map;
  _dependencies = new Map;
  constructor(program2, options) {
    this.program = program2;
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
    this._pendingConstrainedCallUsages = [];
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
              throw new SemanticError(`Ambiguous record literal \u2014 multiple types in scope match this shape: ${names}. ` + `Add a type annotation to disambiguate.`, expr.span, this.getFilePath());
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
            if (moduleAccess.constraints.length > 0) {
              this._pendingConstrainedCallUsages.push({
                node: fieldExpr,
                constraints: moduleAccess.constraints
              });
            }
            return moduleAccess.type;
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
        for (const usage of this._pendingConstrainedCallUsages) {
          const resolved = usage.constraints.map((c) => ({
            protocolName: c.protocolName,
            typeArgs: c.typeArgs.map((t) => applySubstitution(t, this.substitution))
          }));
          this._resolvedConstrainedCallUsages.set(usage.node, resolved);
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
      protocolMethodUsages: this._resolvedProtocolUsages,
      constrainedCallUsages: this._resolvedConstrainedCallUsages
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
function analyze(program2, options) {
  return new SemanticAnalyzer(program2, options).analyze();
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
    const { type, constraints } = instantiateWithConstraints(scheme, substitution);
    return { type, constraints };
  }
  if (Object.hasOwn(depModule.values, field2)) {
    const valueInfo = depModule.values[field2];
    const valueType = valueInfo.type || depModule.types[field2];
    if (valueType) {
      return { type: valueType, constraints: [] };
    }
  }
  if (Object.hasOwn(depModule.constructorTypes, field2)) {
    const ctorScheme = depModule.constructorTypes[field2];
    const { type, constraints } = instantiateWithConstraints(ctorScheme, substitution);
    return { type, constraints };
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

// ../ir/src/utils.ts
function formatTypeKey(type) {
  if (!type)
    return "unknown";
  switch (type.kind) {
    case "con":
      if (type.args.length === 0)
        return type.name;
      return `${type.name}_${type.args.map(formatTypeKey).join("_")}`;
    case "var":
      return `v${type.id}`;
    case "fun":
      return `fn_${formatTypeKey(type.from)}_${formatTypeKey(type.to)}`;
    case "tuple":
      return `tuple_${type.elements.map(formatTypeKey).join("_")}`;
    case "record":
      return `record`;
    case "list":
      return `list_${formatTypeKey(type.element)}`;
    default:
      return "unknown";
  }
}
function typeStructureMatches(instType, concreteType) {
  if (instType.kind === "var") {
    return true;
  }
  if (instType.kind === "con" && concreteType.kind === "con") {
    if (instType.name !== concreteType.name) {
      return false;
    }
    if (instType.args.length !== concreteType.args.length) {
      return false;
    }
    for (let i = 0;i < instType.args.length; i++) {
      if (!typeStructureMatches(instType.args[i], concreteType.args[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "list" && concreteType.kind === "list") {
    return typeStructureMatches(instType.element, concreteType.element);
  }
  if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    if (instType.elements.length !== concreteType.elements.length) {
      return false;
    }
    for (let i = 0;i < instType.elements.length; i++) {
      if (!typeStructureMatches(instType.elements[i], concreteType.elements[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "fun" && concreteType.kind === "fun") {
    return typeStructureMatches(instType.from, concreteType.from) && typeStructureMatches(instType.to, concreteType.to);
  }
  return false;
}
function findPolymorphicInstance(protocolName, instances) {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    const firstTypeArg = inst.typeArgs[0];
    if (firstTypeArg && firstTypeArg.kind === "var") {
      const typeKey = formatTypeKey(firstTypeArg);
      return {
        key: `${protocolName}_${typeKey}`,
        instance: inst
      };
    }
  }
  return null;
}
function findMatchingInstance(protocolName, concreteType, instances) {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    const instTypeArg = inst.typeArgs[0];
    if (!instTypeArg)
      continue;
    if (instTypeArg.kind === "var")
      continue;
    if (typeStructureMatches(instTypeArg, concreteType)) {
      const typeKey = formatTypeKey(instTypeArg);
      return {
        key: `${protocolName}_${typeKey}`,
        instance: inst
      };
    }
  }
  return null;
}

// ../ir/src/dependency.ts
function buildDependencyGraph2(values, instances, protocols = {}, constructors = {}) {
  const graph = new Map;
  const availableNames = new Set(Object.keys(values));
  const protocolMethodMap = new Map;
  for (const protocol of Object.values(protocols)) {
    for (const method of protocol.methods) {
      protocolMethodMap.set(method.name, protocol.name);
    }
  }
  for (const inst of instances) {
    if (!inst.typeArgs[0])
      continue;
    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const instName = `$dict_${inst.protocolName}_${typeKey}`;
    availableNames.add(instName);
  }
  for (const [name, value2] of Object.entries(values)) {
    const deps = new Set;
    collectDependencies(value2.body, deps, availableNames, instances, protocolMethodMap, constructors);
    for (const param of value2.params) {
      collectPatternDependencies(param, deps, availableNames);
    }
    graph.set(name, deps);
  }
  for (const inst of instances) {
    if (!inst.typeArgs[0])
      continue;
    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const instName = `$dict_${inst.protocolName}_${typeKey}`;
    const deps = new Set;
    for (const implName of Object.values(inst.methods)) {
      if (availableNames.has(implName)) {
        deps.add(implName);
      }
    }
    for (const constraint of inst.constraints) {
      const typeArg = constraint.typeArgs[0];
      if (typeArg && typeArg.kind !== "var") {
        const constraintKey = formatTypeKey(typeArg);
        const constraintDict = `$dict_${constraint.protocolName}_${constraintKey}`;
        if (availableNames.has(constraintDict)) {
          deps.add(constraintDict);
        }
      }
    }
    graph.set(instName, deps);
  }
  return graph;
}
function collectDependencies(expr, deps, availableNames, instances, protocolMethodMap, constructors) {
  switch (expr.kind) {
    case "IRVar":
      if (expr.namespace === "value") {
        if (availableNames.has(expr.name)) {
          deps.add(expr.name);
        }
        if (expr.constraint) {
          const typeArg = expr.constraint.typeArgs[0];
          if (typeArg && typeArg.kind !== "var") {
            const dictName = resolveDictionaryName(expr.constraint.protocolName, typeArg, instances);
            if (dictName && availableNames.has(dictName)) {
              deps.add(dictName);
            }
          }
        }
      }
      break;
    case "IRLiteral":
    case "IRUnit":
      break;
    case "IRLambda":
      collectDependencies(expr.body, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
    case "IRApply":
      if (expr.callee.kind === "IRVar" && protocolMethodMap.has(expr.callee.name)) {
        const protocolName = protocolMethodMap.get(expr.callee.name);
        if (protocolName && expr.args.length > 0) {
          const firstArg = expr.args[0];
          let inferenceType;
          if (firstArg.kind === "IRLiteral") {
            const litType = firstArg.literalType;
            const typeName = litType.charAt(0).toUpperCase() + litType.slice(1);
            if (typeName === "Int" || typeName === "Float" || typeName === "String" || typeName === "Bool") {
              inferenceType = { kind: "con", name: typeName, args: [] };
            } else if (litType === "char") {
              inferenceType = { kind: "con", name: "Char", args: [] };
            }
          } else if (firstArg.kind === "IRVar" && firstArg.type) {
            inferenceType = firstArg.type;
          } else if (firstArg.kind === "IRConstructor") {
            const ctorInfo = constructors[firstArg.name];
            if (ctorInfo) {
              inferenceType = {
                kind: "con",
                name: ctorInfo.parentType,
                args: []
              };
            }
          }
          if (inferenceType) {
            const dictName = resolveDictionaryName(protocolName, inferenceType, instances);
            if (dictName && availableNames.has(dictName)) {
              deps.add(dictName);
            }
          }
        }
      }
      collectDependencies(expr.callee, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const arg of expr.args) {
        collectDependencies(arg, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRIf":
      collectDependencies(expr.condition, deps, availableNames, instances, protocolMethodMap, constructors);
      collectDependencies(expr.thenBranch, deps, availableNames, instances, protocolMethodMap, constructors);
      collectDependencies(expr.elseBranch, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
    case "IRCase":
      collectDependencies(expr.discriminant, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const branch of expr.branches) {
        collectDependencies(branch.body, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRTuple":
      for (const elem of expr.elements) {
        collectDependencies(elem, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRList":
      for (const elem of expr.elements) {
        collectDependencies(elem, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRRecord":
      for (const field2 of expr.fields) {
        collectDependencies(field2.value, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRRecordUpdate":
      collectDependencies(expr.base, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const field2 of expr.updates) {
        collectDependencies(field2.value, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRFieldAccess":
      collectDependencies(expr.target, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
    case "IRConstructor":
      for (const arg of expr.args) {
        collectDependencies(arg, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRSelfLoop":
      collectDependencies(expr.body, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
    case "IRLoopContinue":
      for (const arg of expr.args) {
        collectDependencies(arg, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
  }
}
function resolveDictionaryName(protocolName, type, instances) {
  const typeKey = formatTypeKey(type);
  const exactKey = `${protocolName}_${typeKey}`;
  const match = findMatchingInstance(protocolName, type, instances);
  if (match) {
    return `$dict_${match.key}`;
  }
  const polyMatch = findPolymorphicInstance(protocolName, instances);
  if (polyMatch) {
    return `$dict_${polyMatch.key}`;
  }
  return `$dict_${exactKey}`;
}
function collectPatternDependencies(pattern, deps, availableNames) {
  switch (pattern.kind) {
    case "IRVarPattern":
    case "IRWildcardPattern":
    case "IRLiteralPattern":
      break;
    case "IRConstructorPattern":
      for (const arg of pattern.args) {
        collectPatternDependencies(arg, deps, availableNames);
      }
      break;
    case "IRTuplePattern":
      for (const elem of pattern.elements) {
        collectPatternDependencies(elem, deps, availableNames);
      }
      break;
  }
}
function findSCCs(graph, declarationOrder) {
  const state = {
    index: 0,
    stack: [],
    onStack: new Set,
    indices: new Map,
    lowlinks: new Map,
    sccs: []
  };
  for (const name of declarationOrder) {
    if (!state.indices.has(name)) {
      strongconnect(name, graph, state);
    }
  }
  const result = [];
  for (const scc of state.sccs) {
    const orderMap = new Map(declarationOrder.map((name, idx) => [name, idx]));
    const sortedValues = [...scc].sort((a, b) => {
      const aIdx = orderMap.get(a) ?? Infinity;
      const bIdx = orderMap.get(b) ?? Infinity;
      return aIdx - bIdx;
    });
    result.push({
      values: sortedValues,
      isMutuallyRecursive: scc.length > 1
    });
  }
  return result;
}
function strongconnect(v, graph, state) {
  state.indices.set(v, state.index);
  state.lowlinks.set(v, state.index);
  state.index++;
  state.stack.push(v);
  state.onStack.add(v);
  const successors = graph.get(v) ?? new Set;
  for (const w of successors) {
    if (!state.indices.has(w)) {
      strongconnect(w, graph, state);
      state.lowlinks.set(v, Math.min(state.lowlinks.get(v), state.lowlinks.get(w)));
    } else if (state.onStack.has(w)) {
      state.lowlinks.set(v, Math.min(state.lowlinks.get(v), state.indices.get(w)));
    }
  }
  if (state.lowlinks.get(v) === state.indices.get(v)) {
    const scc = [];
    let w;
    do {
      w = state.stack.pop();
      state.onStack.delete(w);
      scc.push(w);
    } while (w !== v);
    state.sccs.push(scc);
  }
}
function validateTopologicalOrder(sccs, graph) {
  const errors = [];
  const emitted = new Set;
  for (const scc of sccs) {
    const sccSet = new Set(scc.values);
    for (const name of scc.values) {
      const deps = graph.get(name) ?? new Set;
      for (const dep of deps) {
        if (!sccSet.has(dep) && !emitted.has(dep)) {
          errors.push(`Value '${name}' depends on '${dep}' which has not been emitted yet`);
        }
      }
    }
    for (const name of scc.values) {
      emitted.add(name);
    }
  }
  return { valid: errors.length === 0, errors };
}

// ../ir/src/types.ts
class IRError extends Error {
  span;
  filePath;
  constructor(message, span, filePath) {
    super(message);
    this.span = span;
    this.filePath = filePath;
    this.name = "IRError";
  }
}
// ../ir/src/lowering/context.ts
function createLoweringContext(semantics, imports = [], dependencies = new Map) {
  const ctx = {
    semantics,
    liftedBindings: [],
    nameCounter: 0,
    constructorTags: new Map,
    recordFields: new Map,
    imports,
    dependencies,
    namespaceImportedNames: new Map
  };
  assignConstructorTags(ctx);
  buildRecordFieldInfo(ctx);
  buildNamespaceImportedNames(ctx);
  return ctx;
}
function assignConstructorTags(ctx) {
  for (const [adtName, adt] of Object.entries(ctx.semantics.adts)) {
    for (let i = 0;i < adt.constructors.length; i++) {
      const ctorName = adt.constructors[i];
      if (ctorName) {
        ctx.constructorTags.set(ctorName, i);
      }
    }
  }
  for (const imp of ctx.imports) {
    const depModule = ctx.dependencies.get(imp.moduleName);
    if (!depModule)
      continue;
    for (const [adtName, adt] of Object.entries(depModule.adts)) {
      for (let i = 0;i < adt.constructors.length; i++) {
        const ctorName = adt.constructors[i];
        if (ctorName) {
          const fullQualified = `${imp.moduleName}.${ctorName}`;
          ctx.constructorTags.set(fullQualified, i);
          if (imp.alias) {
            const aliasQualified = `${imp.alias}.${ctorName}`;
            ctx.constructorTags.set(aliasQualified, i);
          }
        }
      }
    }
  }
}
function buildRecordFieldInfo(ctx) {
  for (const [name, alias] of Object.entries(ctx.semantics.typeAliases)) {
    if (alias.value.kind === "RecordType") {
      ctx.recordFields.set(name, alias.value.fields.map((f) => f.name));
    }
  }
}
function buildNamespaceImportedNames(ctx) {
  const allExposingModules = new Set;
  for (const imp of ctx.imports) {
    if (imp.exposing?.kind === "All") {
      allExposingModules.add(imp.moduleName);
    }
  }
  if (allExposingModules.size === 0)
    return;
  for (const [name, moduleName] of ctx.semantics.importedValues) {
    if (allExposingModules.has(moduleName)) {
      ctx.namespaceImportedNames.set(name, moduleName);
    }
  }
  for (const imp of ctx.imports) {
    if (imp.exposing?.kind !== "All")
      continue;
    const dep = ctx.dependencies.get(imp.moduleName);
    if (!dep)
      continue;
    for (const ctorName of Object.keys(ctx.semantics.constructors)) {
      if (dep.constructors[ctorName] && isConstructorExported(dep, ctorName)) {
        ctx.namespaceImportedNames.set(ctorName, imp.moduleName);
      }
    }
  }
}
function isConstructorExported(dep, ctorName) {
  const exports = dep.exports;
  if (exports.exportsAll)
    return true;
  for (const [, typeExport] of exports.types) {
    if (typeExport.constructors?.has(ctorName))
      return true;
  }
  return false;
}
// ../ir/src/lowering/patterns.ts
function lowerPattern(pattern, ctx) {
  switch (pattern.kind) {
    case "VarPattern":
      return {
        kind: "IRVarPattern",
        name: pattern.name,
        span: pattern.span
      };
    case "WildcardPattern":
      return {
        kind: "IRWildcardPattern",
        span: pattern.span
      };
    case "IntPattern":
      return {
        kind: "IRLiteralPattern",
        value: Number(pattern.value),
        literalType: "int",
        span: pattern.span
      };
    case "FloatPattern":
      return {
        kind: "IRLiteralPattern",
        value: Number(pattern.value),
        literalType: "float",
        span: pattern.span
      };
    case "StringPattern":
      return {
        kind: "IRLiteralPattern",
        value: pattern.value,
        literalType: "string",
        span: pattern.span
      };
    case "CharPattern":
      return {
        kind: "IRLiteralPattern",
        value: pattern.value,
        literalType: "char",
        span: pattern.span
      };
    case "ConstructorPattern": {
      const tag = ctx.constructorTags.get(pattern.name);
      if (tag === undefined) {
        throw new IRError(`Unknown constructor '${pattern.name}' - no tag found. ` + `This is a compiler bug: the constructor may be from an unresolved import.`, pattern.span);
      }
      return {
        kind: "IRConstructorPattern",
        name: pattern.name,
        args: pattern.args.map((p) => lowerPattern(p, ctx)),
        tag,
        span: pattern.span
      };
    }
    case "TuplePattern":
      return {
        kind: "IRTuplePattern",
        elements: pattern.elements.map((p) => lowerPattern(p, ctx)),
        span: pattern.span
      };
    case "ListPattern":
      return {
        kind: "IRListPattern",
        elements: pattern.elements.map((p) => lowerPattern(p, ctx)),
        span: pattern.span
      };
    case "ConsPattern":
      return {
        kind: "IRConsPattern",
        head: lowerPattern(pattern.head, ctx),
        tail: lowerPattern(pattern.tail, ctx),
        span: pattern.span
      };
    case "RecordPattern":
      return {
        kind: "IRRecordPattern",
        fields: pattern.fields.map((f) => ({
          name: f.name,
          pattern: f.pattern ? lowerPattern(f.pattern, ctx) : undefined
        })),
        span: pattern.span
      };
    default:
      const _exhaustive = pattern;
      throw new IRError(`Unknown pattern kind: ${pattern.kind}`, pattern.span);
  }
}

// ../ir/src/lowering/types.ts
function convertType(type) {
  switch (type.kind) {
    case "var":
      return { kind: "var", id: type.id };
    case "con":
      return { kind: "con", name: type.name, args: type.args.map(convertType) };
    case "fun":
      return {
        kind: "fun",
        from: convertType(type.from),
        to: convertType(type.to)
      };
    case "tuple":
      return { kind: "tuple", elements: type.elements.map(convertType) };
    case "record":
      const fields = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = convertType(v);
      }
      return { kind: "record", fields };
    case "list":
      return { kind: "list", element: convertType(type.element) };
    default:
      return type;
  }
}
function convertConstraints(constraints) {
  return constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map(convertType)
  }));
}

// ../ir/src/lowering/tco.ts
function rewriteSelfTailCalls(funcName, params, body, dictParamCount) {
  const paramNames = [];
  for (const p of params) {
    if (p.kind !== "IRVarPattern")
      return body;
    paramNames.push(p.name);
  }
  if (paramNames.length === 0)
    return body;
  const expectedArgCount = dictParamCount + paramNames.length;
  let foundTailCall = false;
  function rewrite(expr, inTailPos) {
    switch (expr.kind) {
      case "IRApply": {
        if (inTailPos && expr.callee.kind === "IRVar" && expr.callee.name === funcName && expr.callee.namespace === "value" && expr.args.length === expectedArgCount) {
          foundTailCall = true;
          return {
            kind: "IRLoopContinue",
            args: expr.args.slice(dictParamCount),
            span: expr.span
          };
        }
        if (inTailPos && expr.callee.kind === "IRLambda" && expr.args.length === 1) {
          const innerBody = rewrite(expr.callee.body, true);
          if (innerBody === expr.callee.body)
            return expr;
          return {
            ...expr,
            callee: { ...expr.callee, body: innerBody }
          };
        }
        return expr;
      }
      case "IRIf": {
        const thenBranch = rewrite(expr.thenBranch, inTailPos);
        const elseBranch = rewrite(expr.elseBranch, inTailPos);
        if (thenBranch === expr.thenBranch && elseBranch === expr.elseBranch) {
          return expr;
        }
        return { ...expr, thenBranch, elseBranch };
      }
      case "IRCase": {
        let changed = false;
        const branches = expr.branches.map((b) => {
          const body2 = rewrite(b.body, inTailPos);
          if (body2 !== b.body)
            changed = true;
          return body2 === b.body ? b : { ...b, body: body2 };
        });
        if (!changed)
          return expr;
        return { ...expr, branches };
      }
      case "IRLambda":
        return expr;
      case "IRSelfLoop":
      case "IRLoopContinue":
        return expr;
      default:
        return expr;
    }
  }
  const rewritten = rewrite(body, true);
  if (!foundTailCall)
    return body;
  return {
    kind: "IRSelfLoop",
    paramNames,
    body: rewritten,
    span: body.span
  };
}

// ../ir/src/lowering/expressions.ts
function lowerExpr(exprArg, ctx) {
  let expr = exprArg;
  while (true) {
    switch (expr.kind) {
      case "Var":
        return lowerVar(expr, ctx);
      case "Number":
        return lowerNumber(expr);
      case "String":
        return {
          kind: "IRLiteral",
          value: expr.value,
          literalType: "string",
          span: expr.span
        };
      case "Char":
        return {
          kind: "IRLiteral",
          value: expr.value,
          literalType: "char",
          span: expr.span
        };
      case "Lambda":
        return {
          kind: "IRLambda",
          params: expr.args.map((p) => lowerPattern(p, ctx)),
          body: lowerExpr(expr.body, ctx),
          span: expr.span
        };
      case "Apply":
        return {
          kind: "IRApply",
          callee: lowerExpr(expr.callee, ctx),
          args: expr.args.map((a) => lowerExpr(a, ctx)),
          span: expr.span
        };
      case "If":
        return {
          kind: "IRIf",
          condition: lowerExpr(expr.condition, ctx),
          thenBranch: lowerExpr(expr.thenBranch, ctx),
          elseBranch: lowerExpr(expr.elseBranch, ctx),
          span: expr.span
        };
      case "LetIn":
        return lowerLetIn(expr, ctx);
      case "Case":
        return lowerCase(expr, ctx);
      case "Infix":
        return lowerInfix(expr, ctx);
      case "Unary":
        return {
          kind: "IRUnary",
          operator: expr.operator,
          operand: lowerExpr(expr.operand, ctx),
          span: expr.span
        };
      case "Paren":
        expr = expr.expression;
        continue;
      case "Tuple":
        return {
          kind: "IRTuple",
          elements: expr.elements.map((e) => lowerExpr(e, ctx)),
          span: expr.span
        };
      case "Unit":
        return {
          kind: "IRUnit",
          span: expr.span
        };
      case "List":
        return {
          kind: "IRList",
          elements: expr.elements.map((e) => lowerExpr(e, ctx)),
          span: expr.span
        };
      case "ListRange":
        return {
          kind: "IRApply",
          callee: {
            kind: "IRApply",
            callee: {
              kind: "IRVar",
              name: "range",
              namespace: "value",
              span: expr.span
            },
            args: [lowerExpr(expr.start, ctx)],
            span: expr.span
          },
          args: [lowerExpr(expr.end, ctx)],
          span: expr.span
        };
      case "Record":
        return {
          kind: "IRRecord",
          fields: expr.fields.map((f) => ({
            name: f.name,
            value: lowerExpr(f.value, ctx),
            span: f.span
          })),
          span: expr.span
        };
      case "RecordUpdate":
        return lowerRecordUpdate(expr, ctx);
      case "FieldAccess": {
        const moduleAccess = tryResolveModuleAccess(expr, ctx);
        if (moduleAccess) {
          return moduleAccess;
        }
        return {
          kind: "IRFieldAccess",
          target: lowerExpr(expr.target, ctx),
          field: expr.field,
          span: expr.span
        };
      }
      default:
        const _exhaustive = expr;
        throw new IRError(`Unknown expression kind: ${expr.kind}`, expr.span);
    }
  }
}
function getModuleValueConstraints(depModule, valueName) {
  const scheme = depModule.typeSchemes[valueName];
  if (!scheme || scheme.constraints.length === 0) {
    return [];
  }
  return scheme.constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((t) => convertType(t))
  }));
}
function getInstantiatedConstraints(expr, depModule, valueName, ctx) {
  const usages = ctx.semantics.constrainedCallUsages?.get(expr);
  if (usages && usages.length > 0) {
    return usages.map((u) => ({
      protocolName: u.protocolName,
      typeArgs: u.typeArgs.map((t) => convertType(t))
    }));
  }
  return getModuleValueConstraints(depModule, valueName);
}
function tryResolveModuleAccess(expr, ctx) {
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
  for (const imp of ctx.imports) {
    const importParts = imp.moduleName.split(".");
    if (imp.alias && baseName === imp.alias && parts.length >= 2) {
      const depModule = ctx.dependencies.get(imp.moduleName);
      if (!depModule) {
        continue;
      }
      const fieldParts = parts.slice(1);
      if (fieldParts.length === 1) {
        const field2 = fieldParts[0];
        const valueInfo = depModule.values[field2];
        if (valueInfo) {
          const decl = valueInfo.declaration;
          let externalName;
          if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
            externalName = decl.args[1];
          }
          const constraints = getInstantiatedConstraints(expr, depModule, field2, ctx);
          return {
            kind: "IRModuleAccess",
            importAlias: imp.alias,
            moduleName: imp.moduleName,
            valueName: field2,
            externalName,
            ...constraints.length > 0 ? { constraints } : {},
            span: expr.span
          };
        }
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
        const depModule = ctx.dependencies.get(imp.moduleName);
        if (!depModule) {
          continue;
        }
        const fieldParts = parts.slice(importParts.length);
        if (fieldParts.length === 1) {
          const field2 = fieldParts[0];
          const valueInfo = depModule.values[field2];
          if (valueInfo) {
            const decl = valueInfo.declaration;
            let externalName;
            if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
              externalName = decl.args[1];
            }
            const alias = imp.alias || importParts[importParts.length - 1];
            const constraints = getInstantiatedConstraints(expr, depModule, field2, ctx);
            return {
              kind: "IRModuleAccess",
              importAlias: alias,
              moduleName: imp.moduleName,
              valueName: field2,
              externalName,
              ...constraints.length > 0 ? { constraints } : {},
              span: expr.span
            };
          }
        }
      }
    }
  }
  return null;
}
function lowerVar(expr, ctx) {
  if (expr.namespace === "upper") {
    const ctorInfo = ctx.semantics.constructors[expr.name];
    if (ctorInfo) {
      const tag = ctx.constructorTags.get(expr.name) ?? 0;
      const ctorModuleName = ctx.namespaceImportedNames.get(expr.name);
      const isImported = ctorInfo.moduleName !== undefined && ctorInfo.moduleName !== ctx.semantics.module.name && ctorInfo.moduleName !== BUILTIN_MODULE_NAME;
      if (ctorInfo.arity === 0) {
        if (isImported) {
          return {
            kind: "IRVar",
            name: expr.name,
            namespace: "constructor",
            ...ctorModuleName ? { moduleName: ctorModuleName } : {},
            span: expr.span
          };
        }
        return {
          kind: "IRConstructor",
          name: expr.name,
          args: [],
          tag,
          span: expr.span
        };
      }
      return {
        kind: "IRVar",
        name: expr.name,
        namespace: "constructor",
        ...ctorModuleName ? { moduleName: ctorModuleName } : {},
        span: expr.span
      };
    }
  }
  const moduleName = ctx.namespaceImportedNames.get(expr.name);
  let constraint;
  const usage = ctx.semantics.protocolMethodUsages?.get(expr);
  if (usage) {
    constraint = {
      protocolName: usage.protocolName,
      typeArgs: usage.typeArgs.map((t) => convertType(t))
    };
  }
  return {
    kind: "IRVar",
    name: expr.name,
    namespace: "value",
    ...moduleName ? { moduleName } : {},
    ...constraint ? { constraint } : {},
    span: expr.span
  };
}
function lowerNumber(expr) {
  const isFloat = expr.value.includes(".") || expr.value.includes("e") || expr.value.includes("E");
  return {
    kind: "IRLiteral",
    value: isFloat ? parseFloat(expr.value) : parseInt(expr.value, 10),
    literalType: isFloat ? "float" : "int",
    span: expr.span
  };
}
function lowerLetIn(exprArg, ctx) {
  const levels = [];
  let current2 = exprArg;
  while (current2.kind === "LetIn") {
    levels.push({ bindings: current2.bindings, span: current2.span });
    current2 = current2.body;
  }
  let result = lowerExpr(current2, ctx);
  for (let level = levels.length - 1;level >= 0; level--) {
    const { bindings, span } = levels[level];
    for (let i = bindings.length - 1;i >= 0; i--) {
      const binding = bindings[i];
      const bindingValue = lowerValueBody(binding, ctx);
      const pattern = {
        kind: "IRVarPattern",
        name: binding.name,
        span: binding.span
      };
      const lambda = {
        kind: "IRLambda",
        params: [pattern],
        body: result,
        span
      };
      let valueExpr;
      if (binding.args.length > 0) {
        const bindingParams = binding.args.map((p) => lowerPattern(p, ctx));
        const tcoBody = rewriteSelfTailCalls(binding.name, bindingParams, bindingValue, 0);
        valueExpr = {
          kind: "IRLambda",
          params: bindingParams,
          body: tcoBody,
          span: binding.span
        };
      } else {
        valueExpr = bindingValue;
      }
      result = {
        kind: "IRApply",
        callee: lambda,
        args: [valueExpr],
        span
      };
    }
  }
  return result;
}
function lowerValueBody(decl, ctx) {
  return lowerExpr(decl.body, ctx);
}
function lowerCase(expr, ctx) {
  const discriminant = lowerExpr(expr.discriminant, ctx);
  const branches = expr.branches;
  if (isBoolCase(branches, ctx)) {
    return lowerBoolCase(discriminant, branches, ctx, expr.span);
  }
  if (allLiteralPatterns(branches)) {
    return lowerLiteralCase(discriminant, branches, ctx, expr.span);
  }
  return {
    kind: "IRCase",
    discriminant,
    branches: branches.map((b) => ({
      pattern: lowerPattern(b.pattern, ctx),
      body: lowerExpr(b.body, ctx),
      span: b.span
    })),
    span: expr.span
  };
}
function isBoolCase(branches, ctx) {
  if (branches.length !== 2)
    return false;
  const patterns = branches.map((b) => b.pattern);
  const truePattern = patterns.find((p) => p.kind === "ConstructorPattern" && p.name === "True" && p.args.length === 0);
  const falsePattern = patterns.find((p) => p.kind === "ConstructorPattern" && p.name === "False" && p.args.length === 0);
  if (!truePattern || !falsePattern)
    return false;
  const trueCtorInfo = ctx.semantics.constructors["True"];
  const falseCtorInfo = ctx.semantics.constructors["False"];
  return trueCtorInfo?.moduleName === BUILTIN_MODULE_NAME && trueCtorInfo?.parentType === BOOL_TYPE_NAME && falseCtorInfo?.moduleName === BUILTIN_MODULE_NAME && falseCtorInfo?.parentType === BOOL_TYPE_NAME;
}
function lowerBoolCase(discriminant, branches, ctx, span) {
  let trueBranch = null;
  let falseBranch = null;
  for (const branch of branches) {
    if (branch.pattern.kind === "ConstructorPattern" && branch.pattern.name === "True") {
      trueBranch = lowerExpr(branch.body, ctx);
    } else if (branch.pattern.kind === "ConstructorPattern" && branch.pattern.name === "False") {
      falseBranch = lowerExpr(branch.body, ctx);
    }
  }
  if (!trueBranch || !falseBranch) {
    throw new IRError("Bool case missing True or False branch", span);
  }
  return {
    kind: "IRIf",
    condition: discriminant,
    thenBranch: trueBranch,
    elseBranch: falseBranch,
    span
  };
}
function allLiteralPatterns(branches) {
  return false;
}
function lowerLiteralCase(discriminant, branches, ctx, span) {
  return {
    kind: "IRCase",
    discriminant,
    branches: branches.map((b) => ({
      pattern: lowerPattern(b.pattern, ctx),
      body: lowerExpr(b.body, ctx),
      span: b.span
    })),
    span
  };
}
function lowerRecordUpdate(expr, ctx) {
  const baseType = ctx.semantics.types[expr.base];
  const baseVar = {
    kind: "IRVar",
    name: expr.base,
    namespace: "value",
    span: expr.span
  };
  const updatedFields = new Set(expr.fields.map((f) => f.name));
  let allFields = [];
  if (baseType && baseType.kind === "record") {
    allFields = Object.keys(baseType.fields);
  }
  const irFields = [];
  for (const fieldName of allFields) {
    if (!updatedFields.has(fieldName)) {
      irFields.push({
        name: fieldName,
        value: {
          kind: "IRFieldAccess",
          target: baseVar,
          field: fieldName,
          span: expr.span
        },
        span: expr.span
      });
    }
  }
  for (const field2 of expr.fields) {
    irFields.push({
      name: field2.name,
      value: lowerExpr(field2.value, ctx),
      span: field2.span
    });
  }
  if (allFields.length === 0) {
    return {
      kind: "IRRecordUpdate",
      base: baseVar,
      updates: expr.fields.map((f) => ({
        name: f.name,
        value: lowerExpr(f.value, ctx),
        span: f.span
      })),
      span: expr.span
    };
  }
  return {
    kind: "IRRecord",
    fields: irFields,
    span: expr.span
  };
}
function lowerInfix(expr, ctx) {
  const left = lowerExpr(expr.left, ctx);
  const right = lowerExpr(expr.right, ctx);
  const opModuleName = ctx.namespaceImportedNames.get(expr.operator);
  let constraint;
  const usage = ctx.semantics.protocolMethodUsages?.get(expr);
  if (usage) {
    constraint = {
      protocolName: usage.protocolName,
      typeArgs: usage.typeArgs.map((t) => convertType(t))
    };
  }
  const opVar = {
    kind: "IRVar",
    name: expr.operator,
    namespace: "value",
    ...opModuleName ? { moduleName: opModuleName } : {},
    ...constraint ? { constraint } : {},
    span: expr.span
  };
  const rightArg = SHORT_CIRCUIT_OPERATORS.has(expr.operator) ? {
    kind: "IRLambda",
    params: [],
    body: right,
    span: expr.right.span
  } : right;
  return {
    kind: "IRApply",
    callee: {
      kind: "IRApply",
      callee: opVar,
      args: [left],
      span: expr.span
    },
    args: [rightArg],
    span: expr.span
  };
}
// ../ir/src/printer.ts
var defaultOptions = {
  indent: "  ",
  showTypes: true,
  showConstraints: true,
  showSpans: false,
  maxWidth: 80
};
function printProgram(program2, options = {}) {
  const opts = { ...defaultOptions, ...options };
  const lines = [];
  lines.push(`module ${program2.moduleName}`);
  lines.push("");
  if (program2.externalImports.size > 0) {
    lines.push("-- External Imports");
    for (const imp of program2.externalImports) {
      lines.push(`import "${imp}"`);
    }
    lines.push("");
  }
  if (program2.defaultImports.length > 0) {
    lines.push("-- Default Imports");
    for (const imp of program2.defaultImports) {
      lines.push(`import ${imp.name} from "${imp.modulePath}"`);
    }
    lines.push("");
  }
  if (Object.keys(program2.adts).length > 0) {
    lines.push("-- Types");
    for (const [name, adt] of Object.entries(program2.adts)) {
      const params = adt.params.length > 0 ? " " + adt.params.join(" ") : "";
      const ctors = adt.constructors.map((c) => {
        const info = program2.constructors[c];
        const tag = info ? `[${info.tag}]` : "";
        return `${c}${tag}`;
      }).join(" | ");
      lines.push(`type ${name}${params} = ${ctors}`);
    }
    lines.push("");
  }
  if (Object.keys(program2.protocols).length > 0) {
    lines.push("-- Protocols");
    for (const [name, proto] of Object.entries(program2.protocols)) {
      const params = proto.params.join(" ");
      lines.push(`protocol ${name} ${params} where`);
      for (const method of proto.methods) {
        const def = method.hasDefault ? " {default}" : "";
        if (opts.showTypes) {
          lines.push(`${opts.indent}${method.name} : ${printType(method.type)}${def}`);
        } else {
          lines.push(`${opts.indent}${method.name}${def}`);
        }
      }
    }
    lines.push("");
  }
  if (program2.instances.length > 0) {
    lines.push("-- Instances");
    for (const inst of program2.instances) {
      const typeArgs = inst.typeArgs.map(printType).join(" ");
      const constraints = inst.constraints.length > 0 ? `(${inst.constraints.map(printConstraint).join(", ")}) => ` : "";
      lines.push(`implement ${constraints}${inst.protocolName} ${typeArgs} where`);
      for (const [method, impl] of Object.entries(inst.methods)) {
        lines.push(`${opts.indent}${method} = ${impl}`);
      }
    }
    lines.push("");
  }
  lines.push("-- Values (in dependency order)");
  for (const scc of program2.dependencyOrder) {
    if (scc.isMutuallyRecursive) {
      lines.push(`-- SCC (mutually recursive): ${scc.values.join(", ")}`);
    }
    for (const valueName of scc.values) {
      const value2 = program2.values[valueName];
      if (value2) {
        lines.push(printValue(value2, opts));
        lines.push("");
      }
    }
  }
  if (program2.liftedBindings.length > 0) {
    lines.push("-- Lifted Bindings");
    for (const binding of program2.liftedBindings) {
      lines.push(printValue(binding, opts));
      lines.push("");
    }
  }
  return lines.join(`
`);
}
function printValue(value2, options = {}) {
  const opts = { ...defaultOptions, ...options };
  const lines = [];
  if (opts.showConstraints && value2.constraints.length > 0) {
    const constraints = value2.constraints.map(printConstraint).join(", ");
    lines.push(`-- Constraints: ${constraints}`);
  }
  if (opts.showTypes) {
    lines.push(`${value2.name} : ${printType(value2.type)}`);
  }
  if (value2.isExternal && value2.externalTarget) {
    lines.push(`@external "${value2.externalTarget.modulePath}" "${value2.externalTarget.exportName}"`);
  }
  const params = value2.params.length > 0 ? " " + value2.params.map((p) => printPattern(p)).join(" ") : "";
  if (value2.isExternal) {
    lines.push(`${value2.name}${params}`);
  } else {
    const body = printExpr(value2.body, opts.indent, 0);
    lines.push(`${value2.name}${params} =`);
    lines.push(`${opts.indent}${body}`);
  }
  return lines.join(`
`);
}
function printExpr(expr, indent = "  ", depth = 0) {
  const ind = indent.repeat(depth);
  const ind1 = indent.repeat(depth + 1);
  switch (expr.kind) {
    case "IRVar":
      return expr.name;
    case "IRModuleAccess":
      return `${expr.importAlias}.${expr.externalName || expr.valueName}`;
    case "IRLiteral":
      if (expr.literalType === "string") {
        return `"${expr.value}"`;
      } else if (expr.literalType === "char") {
        return `'${expr.value}'`;
      } else if (expr.literalType === "bool") {
        return expr.value ? "True" : "False";
      }
      return String(expr.value);
    case "IRLambda": {
      const params = expr.params.map(printPattern).join(" ");
      const body = printExpr(expr.body, indent, 0);
      return `\\${params} -> ${body}`;
    }
    case "IRApply": {
      const callee = printExpr(expr.callee, indent, 0);
      const args = expr.args.map((a) => printExpr(a, indent, 0));
      const calleeStr = expr.callee.kind === "IRLambda" ? `(${callee})` : callee;
      const argStrs = args.map((a, i) => {
        const arg = expr.args[i];
        if (arg && (arg.kind === "IRApply" || arg.kind === "IRLambda" || arg.kind === "IRIf" || arg.kind === "IRCase")) {
          return `(${a})`;
        }
        return a;
      });
      return `${calleeStr} ${argStrs.join(" ")}`;
    }
    case "IRIf":
      return `if ${printExpr(expr.condition, indent, 0)}
` + `${ind1}then ${printExpr(expr.thenBranch, indent, depth + 1)}
` + `${ind1}else ${printExpr(expr.elseBranch, indent, depth + 1)}`;
    case "IRCase": {
      const disc = printExpr(expr.discriminant, indent, 0);
      const branches = expr.branches.map((b) => {
        const pat = printPattern(b.pattern);
        const body = printExpr(b.body, indent, depth + 2);
        return `${ind1}${pat} -> ${body}`;
      }).join(`
`);
      return `case ${disc} of
${branches}`;
    }
    case "IRTuple": {
      const elems = expr.elements.map((e) => printExpr(e, indent, 0));
      return `(${elems.join(", ")})`;
    }
    case "IRUnit":
      return "()";
    case "IRList": {
      const elems = expr.elements.map((e) => printExpr(e, indent, 0));
      return `[${elems.join(", ")}]`;
    }
    case "IRRecord": {
      const fields = expr.fields.map((f) => `${f.name} = ${printExpr(f.value, indent, 0)}`);
      return `{ ${fields.join(", ")} }`;
    }
    case "IRRecordUpdate": {
      const base = printExpr(expr.base, indent, 0);
      const updates = expr.updates.map((f) => `${f.name} = ${printExpr(f.value, indent, 0)}`);
      return `{ ${base} | ${updates.join(", ")} }`;
    }
    case "IRFieldAccess":
      return `${printExpr(expr.target, indent, 0)}.${expr.field}`;
    case "IRConstructor": {
      if (expr.args.length === 0) {
        return expr.name;
      }
      const args = expr.args.map((a) => {
        const s = printExpr(a, indent, 0);
        return a.kind === "IRApply" || a.kind === "IRConstructor" ? `(${s})` : s;
      });
      return `${expr.name} ${args.join(" ")}`;
    }
    case "IRUnary":
      return `-${printExpr(expr.operand, indent, 0)}`;
    case "IRSelfLoop": {
      const params = expr.paramNames.join(", ");
      const body = printExpr(expr.body, indent, depth + 1);
      return `loop (${params}) {
${ind1}${body}
${ind}}`;
    }
    case "IRLoopContinue": {
      const args = expr.args.map((a) => printExpr(a, indent, 0));
      return `continue (${args.join(", ")})`;
    }
    default:
      return `<unknown: ${expr.kind}>`;
  }
}
function printPattern(pattern) {
  switch (pattern.kind) {
    case "IRVarPattern":
      return pattern.name;
    case "IRWildcardPattern":
      return "_";
    case "IRConstructorPattern": {
      if (pattern.args.length === 0) {
        return pattern.name;
      }
      const args = pattern.args.map(printPattern);
      return `(${pattern.name} ${args.join(" ")})`;
    }
    case "IRTuplePattern": {
      const elems = pattern.elements.map(printPattern);
      return `(${elems.join(", ")})`;
    }
    case "IRLiteralPattern":
      if (pattern.literalType === "string") {
        return `"${pattern.value}"`;
      } else if (pattern.literalType === "char") {
        return `'${pattern.value}'`;
      }
      return String(pattern.value);
    case "IRListPattern": {
      const elems = pattern.elements.map(printPattern);
      return `[${elems.join(", ")}]`;
    }
    case "IRConsPattern":
      return `(${printPattern(pattern.head)} :: ${printPattern(pattern.tail)})`;
    default:
      return `<unknown: ${pattern.kind}>`;
  }
}
function printType(type) {
  switch (type.kind) {
    case "var":
      if (type.id >= 0 && type.id < 26) {
        return String.fromCharCode(97 + type.id);
      }
      return `t${type.id}`;
    case "con":
      if (type.args.length === 0) {
        return type.name;
      }
      const args = type.args.map((a) => {
        const s = printType(a);
        return a.kind === "fun" ? `(${s})` : s;
      });
      return `${type.name} ${args.join(" ")}`;
    case "fun": {
      const from = printType(type.from);
      const to = printType(type.to);
      const fromStr = type.from.kind === "fun" ? `(${from})` : from;
      return `${fromStr} -> ${to}`;
    }
    case "tuple": {
      const elems = type.elements.map(printType);
      return `(${elems.join(", ")})`;
    }
    case "record": {
      const fields = Object.entries(type.fields).map(([k, v]) => `${k} : ${printType(v)}`).join(", ");
      return `{ ${fields} }`;
    }
    case "list":
      return `List ${printType(type.element)}`;
    default:
      return `<unknown>`;
  }
}
function printConstraint(constraint) {
  const args = constraint.typeArgs.map(printType).join(" ");
  return `${constraint.protocolName} ${args}`;
}
// ../ir/src/internal/helpers.ts
function substituteProtocolMethods(expr, methodSubstitutions) {
  if (methodSubstitutions.size === 0)
    return expr;
  switch (expr.kind) {
    case "Var":
      if (methodSubstitutions.has(expr.name)) {
        return methodSubstitutions.get(expr.name);
      }
      return expr;
    case "Infix": {
      if (methodSubstitutions.has(expr.operator)) {
        const newOp = methodSubstitutions.get(expr.operator);
        const left = substituteProtocolMethods(expr.left, methodSubstitutions);
        const right = substituteProtocolMethods(expr.right, methodSubstitutions);
        return {
          kind: "Apply",
          callee: {
            kind: "Apply",
            callee: newOp,
            args: [left],
            span: expr.span
          },
          args: [right],
          span: expr.span
        };
      }
      return {
        kind: "Infix",
        left: substituteProtocolMethods(expr.left, methodSubstitutions),
        operator: expr.operator,
        right: substituteProtocolMethods(expr.right, methodSubstitutions),
        span: expr.span
      };
    }
    case "Lambda":
      return {
        kind: "Lambda",
        args: expr.args,
        body: substituteProtocolMethods(expr.body, methodSubstitutions),
        span: expr.span
      };
    case "Apply":
      return {
        kind: "Apply",
        callee: substituteProtocolMethods(expr.callee, methodSubstitutions),
        args: expr.args.map((arg) => substituteProtocolMethods(arg, methodSubstitutions)),
        span: expr.span
      };
    case "If":
      return {
        kind: "If",
        condition: substituteProtocolMethods(expr.condition, methodSubstitutions),
        thenBranch: substituteProtocolMethods(expr.thenBranch, methodSubstitutions),
        elseBranch: substituteProtocolMethods(expr.elseBranch, methodSubstitutions),
        span: expr.span
      };
    case "LetIn":
      return {
        kind: "LetIn",
        bindings: expr.bindings.map((b) => ({
          ...b,
          body: substituteProtocolMethods(b.body, methodSubstitutions)
        })),
        body: substituteProtocolMethods(expr.body, methodSubstitutions),
        span: expr.span
      };
    case "Case":
      return {
        kind: "Case",
        discriminant: substituteProtocolMethods(expr.discriminant, methodSubstitutions),
        branches: expr.branches.map((branch) => ({
          ...branch,
          body: substituteProtocolMethods(branch.body, methodSubstitutions)
        })),
        span: expr.span
      };
    case "Paren":
      return {
        kind: "Paren",
        expression: substituteProtocolMethods(expr.expression, methodSubstitutions),
        span: expr.span
      };
    case "Tuple":
      return {
        kind: "Tuple",
        elements: expr.elements.map((e) => substituteProtocolMethods(e, methodSubstitutions)),
        span: expr.span
      };
    case "List":
      return {
        kind: "List",
        elements: expr.elements.map((e) => substituteProtocolMethods(e, methodSubstitutions)),
        span: expr.span
      };
    case "ListRange":
      return {
        kind: "ListRange",
        start: substituteProtocolMethods(expr.start, methodSubstitutions),
        end: substituteProtocolMethods(expr.end, methodSubstitutions),
        span: expr.span
      };
    case "Record":
      return {
        kind: "Record",
        fields: expr.fields.map((f) => ({
          ...f,
          value: substituteProtocolMethods(f.value, methodSubstitutions)
        })),
        span: expr.span
      };
    case "RecordUpdate":
      return {
        kind: "RecordUpdate",
        base: expr.base,
        fields: expr.fields.map((f) => ({
          ...f,
          value: substituteProtocolMethods(f.value, methodSubstitutions)
        })),
        span: expr.span
      };
    case "FieldAccess":
      return {
        kind: "FieldAccess",
        target: substituteProtocolMethods(expr.target, methodSubstitutions),
        field: expr.field,
        span: expr.span
      };
    case "Number":
    case "String":
    case "Char":
    case "Unit":
      return expr;
    case "Unary":
      return {
        kind: "Unary",
        operator: expr.operator,
        operand: substituteProtocolMethods(expr.operand, methodSubstitutions),
        span: expr.span
      };
  }
}

// ../ir/src/impl/lower.ts
function substituteTypeArg(typeArg, paramSubst) {
  if (typeArg.kind === "var") {
    const values = Array.from(paramSubst.values());
    if (values.length === 1) {
      return values[0];
    }
    return typeArg;
  }
  if (typeArg.kind === "con") {
    return {
      kind: "con",
      name: typeArg.name,
      args: typeArg.args.map((arg) => substituteTypeArg(arg, paramSubst))
    };
  }
  if (typeArg.kind === "fun") {
    return {
      kind: "fun",
      from: substituteTypeArg(typeArg.from, paramSubst),
      to: substituteTypeArg(typeArg.to, paramSubst)
    };
  }
  if (typeArg.kind === "tuple") {
    return {
      kind: "tuple",
      elements: typeArg.elements.map((el) => substituteTypeArg(el, paramSubst))
    };
  }
  if (typeArg.kind === "record") {
    const newFields = {};
    for (const [key, val] of Object.entries(typeArg.fields)) {
      newFields[key] = substituteTypeArg(val, paramSubst);
    }
    return {
      kind: "record",
      fields: newFields
    };
  }
  return typeArg;
}
function lower(program2, semantics, options = {}) {
  const imports = program2.imports || [];
  const dependencies = options.dependencies || new Map;
  const ctx = createLoweringContext(semantics, imports, dependencies);
  const values = {};
  const declarationOrder = [];
  for (const [name, valueInfo] of Object.entries(semantics.values)) {
    declarationOrder.push(name);
    const decl = valueInfo.declaration;
    const type = semantics.types[name];
    const isExternal = decl.kind === "DecoratedDeclaration";
    let body;
    let params = [];
    if (isExternal) {
      body = {
        kind: "IRUnit",
        span: decl.span
      };
    } else {
      body = lowerExpr(decl.body, ctx);
      params = decl.args.map((p) => lowerPattern(p, ctx));
    }
    const irType = type ? convertType(type) : { kind: "var", id: -1 };
    const typeScheme = semantics.typeSchemes[name];
    const constraints = typeScheme ? convertConstraints(typeScheme.constraints) : [];
    let propertyAccess;
    if (decl.kind === "DecoratedDeclaration" && (decl.decorator === "get" || decl.decorator === "call" || decl.decorator === "val")) {
      const key = decl.args[0];
      if (decl.decorator === "val") {
        propertyAccess = {
          variant: "val",
          key,
          callArity: 0
        };
      } else {
        let argCount = 0;
        let t = irType;
        while (t.kind === "fun") {
          argCount++;
          t = t.to;
        }
        propertyAccess = {
          variant: decl.decorator,
          key,
          callArity: Math.max(0, argCount - 1)
        };
      }
    }
    let externalTarget;
    if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
      let argCount = 0;
      let t = irType;
      while (t.kind === "fun") {
        argCount++;
        t = t.to;
      }
      externalTarget = {
        modulePath: decl.args[0],
        exportName: decl.args[1],
        callArity: argCount
      };
    }
    if (params.length > 0 && !isExternal) {
      const seenProtos = new Set;
      let dictParamCount = 0;
      for (const c of constraints) {
        const ta = c.typeArgs[0];
        if (ta && ta.kind === "var" && !seenProtos.has(c.protocolName)) {
          seenProtos.add(c.protocolName);
          dictParamCount++;
        }
      }
      body = rewriteSelfTailCalls(name, params, body, dictParamCount);
    }
    const irValue = {
      name,
      params,
      body,
      type: irType,
      constraints,
      isExternal: decl.kind === "DecoratedDeclaration" && (decl.decorator === "external" || decl.decorator === "import"),
      externalTarget,
      propertyAccess,
      span: decl.span
    };
    values[name] = irValue;
  }
  const constructors = {};
  for (const [name, info] of Object.entries(semantics.constructors)) {
    constructors[name] = {
      name,
      parentType: info.parentType,
      arity: info.arity,
      tag: ctx.constructorTags.get(name) ?? 0,
      moduleName: info.moduleName
    };
  }
  const protocols = {};
  for (const [name, proto] of Object.entries(semantics.protocols)) {
    const methods = [];
    for (const [methodName, methodInfo] of proto.methods) {
      methods.push({
        name: methodName,
        type: methodInfo.type ? convertType(methodInfo.type) : { kind: "var", id: -1 },
        hasDefault: methodInfo.defaultImpl !== undefined
      });
    }
    const superclassConstraints = proto.superclassConstraints.map((c) => ({
      protocolName: c.protocolName,
      typeArgs: c.typeArgs.map((t) => convertType(t))
    }));
    protocols[name] = {
      name: proto.name,
      params: proto.params,
      superclassConstraints,
      methods
    };
  }
  const syntheticValues = {};
  const syntheticOrder = [];
  const currentModuleName = semantics.module.name;
  const isLocalInstance = (inst) => !inst.moduleName || inst.moduleName === currentModuleName;
  const instances = semantics.instances.map((inst) => {
    const methodsObj = {};
    for (const [methodName, methodExpr] of inst.methods) {
      if (methodExpr.kind === "Var") {
        methodsObj[methodName] = methodExpr.name;
      } else if (methodExpr.kind === "FieldAccess") {
        const parts = [methodExpr.field];
        let current2 = methodExpr.target;
        while (current2.kind === "FieldAccess") {
          parts.unshift(current2.field);
          current2 = current2.target;
        }
        if (current2.kind === "Var") {
          parts.unshift(current2.name);
        }
        methodsObj[methodName] = parts.join(".");
      } else if (methodExpr.kind === "Lambda") {
        if (!isLocalInstance(inst)) {
          const typeKey2 = inst.typeArgs.length > 0 ? formatTypeKey(convertType(inst.typeArgs[0])) : "Unknown";
          const sanitizedMethodName2 = sanitizeOperator(methodName);
          const isExplicit2 = inst.explicitMethods.has(methodName);
          const prefix2 = isExplicit2 ? "$impl" : "$default";
          const syntheticName2 = `${prefix2}_${inst.protocolName}_${typeKey2}_${sanitizedMethodName2}`;
          methodsObj[methodName] = syntheticName2;
          continue;
        }
        const isExplicit = inst.explicitMethods.has(methodName);
        const typeKey = inst.typeArgs.length > 0 ? formatTypeKey(convertType(inst.typeArgs[0])) : "Unknown";
        const sanitizedMethodName = sanitizeOperator(methodName);
        const prefix = isExplicit ? "$impl" : "$default";
        const syntheticName = `${prefix}_${inst.protocolName}_${typeKey}_${sanitizedMethodName}`;
        const methodSubstitutions = new Map;
        for (const [otherMethodName, otherMethodExpr] of inst.methods) {
          if (otherMethodExpr.kind === "Var" || otherMethodExpr.kind === "FieldAccess") {
            methodSubstitutions.set(otherMethodName, otherMethodExpr);
          }
        }
        const substitutedExpr = substituteProtocolMethods(methodExpr, methodSubstitutions);
        const irLambda = lowerExpr(substitutedExpr, ctx);
        const protocol = semantics.protocols[inst.protocolName];
        const methodInfo = protocol?.methods.get(methodName);
        let methodType = { kind: "var", id: -1 };
        if (methodInfo?.type) {
          const paramSubst = new Map;
          if (protocol) {
            for (let pi = 0;pi < protocol.params.length; pi++) {
              const param = protocol.params[pi];
              const instTypeArg = inst.typeArgs[pi];
              if (param && instTypeArg) {
                paramSubst.set(param, instTypeArg);
              }
            }
          }
          const specializedType = substituteTypeArg(methodInfo.type, paramSubst);
          methodType = convertType(specializedType);
        }
        const instanceConstraints = inst.constraints.map((c) => ({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => convertType(t))
        }));
        const selfConstraint = {
          protocolName: inst.protocolName,
          typeArgs: inst.typeArgs.map((t) => convertType(t))
        };
        const superclassConstraints = [];
        if (protocol?.superclassConstraints) {
          const paramSubst = new Map;
          for (let i = 0;i < protocol.params.length; i++) {
            const param = protocol.params[i];
            const instTypeArg = inst.typeArgs[i];
            if (param && instTypeArg) {
              paramSubst.set(param, instTypeArg);
            }
          }
          for (const superConstraint of protocol.superclassConstraints) {
            const substitutedTypeArgs = superConstraint.typeArgs.map((typeArg) => {
              const substituted = substituteTypeArg(typeArg, paramSubst);
              return convertType(substituted);
            });
            superclassConstraints.push({
              protocolName: superConstraint.protocolName,
              typeArgs: substitutedTypeArgs
            });
          }
        }
        const allConstraints = [
          selfConstraint,
          ...instanceConstraints,
          ...superclassConstraints
        ];
        const syntheticValue = {
          name: syntheticName,
          params: [],
          body: irLambda,
          type: methodType,
          constraints: allConstraints,
          isExternal: false,
          span: methodExpr.span
        };
        syntheticValues[syntheticName] = syntheticValue;
        syntheticOrder.push(syntheticName);
        methodsObj[methodName] = syntheticName;
      } else {
        if (!isLocalInstance(inst)) {
          const typeKey2 = inst.typeArgs.length > 0 ? formatTypeKey(convertType(inst.typeArgs[0])) : "Unknown";
          const sanitizedMethodName2 = sanitizeOperator(methodName);
          const syntheticName2 = `$impl_${inst.protocolName}_${typeKey2}_${sanitizedMethodName2}`;
          methodsObj[methodName] = syntheticName2;
          continue;
        }
        const typeKey = inst.typeArgs.length > 0 ? formatTypeKey(convertType(inst.typeArgs[0])) : "Unknown";
        const sanitizedMethodName = sanitizeOperator(methodName);
        const syntheticName = `$impl_${inst.protocolName}_${typeKey}_${sanitizedMethodName}`;
        const irExpr = lowerExpr(methodExpr, ctx);
        const protocol = semantics.protocols[inst.protocolName];
        const methodInfo = protocol?.methods.get(methodName);
        let methodType = { kind: "var", id: -1 };
        if (methodInfo?.type) {
          const paramSubst2 = new Map;
          if (protocol) {
            for (let pi = 0;pi < protocol.params.length; pi++) {
              const param = protocol.params[pi];
              const instTypeArg = inst.typeArgs[pi];
              if (param && instTypeArg) {
                paramSubst2.set(param, instTypeArg);
              }
            }
          }
          const specializedType = substituteTypeArg(methodInfo.type, paramSubst2);
          methodType = convertType(specializedType);
        }
        const instanceConstraints = inst.constraints.map((c) => ({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => convertType(t))
        }));
        const selfConstraint2 = {
          protocolName: inst.protocolName,
          typeArgs: inst.typeArgs.map((t) => convertType(t))
        };
        const superclassConstraints2 = [];
        if (protocol?.superclassConstraints) {
          const paramSubst = new Map;
          for (let i = 0;i < protocol.params.length; i++) {
            const param = protocol.params[i];
            const instTypeArg = inst.typeArgs[i];
            if (param && instTypeArg) {
              paramSubst.set(param, instTypeArg);
            }
          }
          for (const superConstraint of protocol.superclassConstraints) {
            const substitutedTypeArgs = superConstraint.typeArgs.map((typeArg) => {
              const substituted = substituteTypeArg(typeArg, paramSubst);
              return convertType(substituted);
            });
            superclassConstraints2.push({
              protocolName: superConstraint.protocolName,
              typeArgs: substitutedTypeArgs
            });
          }
        }
        const allConstraints2 = [
          selfConstraint2,
          ...instanceConstraints,
          ...superclassConstraints2
        ];
        const syntheticValue = {
          name: syntheticName,
          params: [],
          body: irExpr,
          type: methodType,
          constraints: allConstraints2,
          isExternal: false,
          span: methodExpr.span
        };
        syntheticValues[syntheticName] = syntheticValue;
        syntheticOrder.push(syntheticName);
        methodsObj[methodName] = syntheticName;
      }
    }
    return {
      protocolName: inst.protocolName,
      typeArgs: inst.typeArgs.map((t) => convertType(t)),
      constraints: inst.constraints.map((c) => ({
        protocolName: c.protocolName,
        typeArgs: c.typeArgs.map((t) => convertType(t))
      })),
      methods: methodsObj
    };
  });
  const mergedValues = { ...values, ...syntheticValues };
  const instanceNames = [];
  for (const inst of instances) {
    if (inst.typeArgs[0]) {
      const key = formatTypeKey(inst.typeArgs[0]);
      instanceNames.push(`$dict_${inst.protocolName}_${key}`);
    }
  }
  const mergedOrder = [
    ...declarationOrder,
    ...syntheticOrder,
    ...instanceNames
  ];
  const depGraph = buildDependencyGraph2(mergedValues, instances, protocols, constructors);
  const sccs = findSCCs(depGraph, mergedOrder);
  if (options.validateDependencies) {
    const validation = validateTopologicalOrder(sccs, depGraph);
    if (!validation.valid) {
      console.warn("Dependency order validation failed:", validation.errors);
    }
  }
  const syntheticDefaultImpls = Object.values(syntheticValues);
  const constraintMetadata = new Map;
  for (const [name, value2] of Object.entries(mergedValues)) {
    if (value2.constraints.length > 0) {
      constraintMetadata.set(name, value2.constraints);
    }
  }
  const externalImports = new Set;
  for (const value2 of Object.values(values)) {
    if (value2.isExternal && value2.externalTarget) {
      externalImports.add(value2.externalTarget.modulePath);
    }
  }
  const importAliases = [];
  for (const imp of imports) {
    const alias = imp.alias || imp.moduleName.split(".").pop();
    importAliases.push({
      alias,
      moduleName: imp.moduleName
    });
  }
  const importedModuleNames = new Set(imports.map((imp) => imp.moduleName));
  for (const semInst of semantics.instances) {
    const srcModule = semInst.moduleName;
    if (srcModule && srcModule !== currentModuleName && !importedModuleNames.has(srcModule) && !importAliases.some((a) => a.moduleName === srcModule)) {
      const alias = "$inst_" + srcModule.split(".").pop();
      importAliases.push({ alias, moduleName: srcModule });
    }
  }
  const resolvedImports = resolveImports(imports, dependencies, semantics);
  augmentImportsFromDefaults(syntheticValues, mergedValues, dependencies, semantics, resolvedImports);
  const packageName = options.packageName ?? semantics.module.name.split(".")[0];
  const defaultImports = [];
  for (const valueInfo of Object.values(semantics.values)) {
    const decl = valueInfo.declaration;
    if (decl.kind === "DecoratedDeclaration" && decl.decorator === "import") {
      defaultImports.push({
        name: decl.name,
        modulePath: decl.args[0]
      });
    }
  }
  return {
    moduleName: semantics.module.name,
    packageName,
    values: mergedValues,
    dependencyOrder: sccs,
    liftedBindings: ctx.liftedBindings,
    syntheticDefaultImpls,
    adts: semantics.adts,
    opaqueTypes: semantics.opaqueTypes,
    constructors,
    protocols,
    instances,
    constraintMetadata,
    externalImports,
    importAliases,
    resolvedImports,
    sourceModule: semantics,
    sourceProgram: program2,
    exports: semantics.exports,
    defaultImports
  };
}
function resolveImports(imports, dependencies, semantics) {
  const resolved = [];
  for (const imp of imports) {
    const depModule = dependencies.get(imp.moduleName);
    const alias = imp.alias || imp.moduleName.split(".").pop() || imp.moduleName;
    if (imp.alias) {
      resolved.push({
        moduleName: imp.moduleName,
        namespaceImport: alias,
        namedImports: []
      });
    }
    if (imp.exposing?.kind === "All") {
      if (!imp.alias) {
        resolved.push({
          moduleName: imp.moduleName,
          namespaceImport: alias,
          namedImports: []
        });
      }
    } else if (imp.exposing?.kind === "Explicit") {
      const names = [];
      for (const spec of imp.exposing.exports) {
        switch (spec.kind) {
          case "ExportValue": {
            if (isTypeOnly(spec.name, depModule, semantics))
              break;
            names.push(spec.name);
            break;
          }
          case "ExportOperator":
            names.push(sanitizeOperator(spec.operator));
            break;
          case "ExportTypeAll": {
            const isProtocol = depModule?.protocols && Object.hasOwn(depModule.protocols, spec.name) || Object.hasOwn(semantics.protocols, spec.name);
            const isOpaque = depModule?.opaqueTypes && Object.hasOwn(depModule.opaqueTypes, spec.name) || Object.hasOwn(semantics.opaqueTypes, spec.name);
            if (isProtocol || isOpaque)
              break;
            const adtInfo = depModule?.adts[spec.name] ?? semantics.adts[spec.name];
            if (adtInfo && adtInfo.constructors.length > 0) {
              for (const ctorName of adtInfo.constructors) {
                names.push(ctorName);
              }
            }
            break;
          }
          case "ExportTypeSome": {
            if (spec.members) {
              for (const ctorName of spec.members) {
                names.push(ctorName);
              }
            }
            break;
          }
        }
      }
      if (names.length > 0) {
        const existing = imp.alias ? resolved.find((r) => r.moduleName === imp.moduleName) : undefined;
        if (existing) {
          existing.namedImports = names;
        } else {
          resolved.push({
            moduleName: imp.moduleName,
            namedImports: names
          });
        }
      } else if (!imp.alias) {}
    } else if (!imp.alias) {
      resolved.push({
        moduleName: imp.moduleName,
        namespaceImport: alias,
        namedImports: []
      });
    }
  }
  const importedModuleNames = new Set(imports.map((i) => i.moduleName));
  for (const semInst of semantics.instances) {
    const srcModule = semInst.moduleName;
    if (srcModule && srcModule !== semantics.module.name && !importedModuleNames.has(srcModule) && !resolved.some((r) => r.moduleName === srcModule)) {
      const instAlias = "$inst_" + srcModule.split(".").pop();
      resolved.push({
        moduleName: srcModule,
        namespaceImport: instAlias,
        namedImports: []
      });
    }
  }
  return resolved;
}
function collectBareVarNames(expr, out) {
  switch (expr.kind) {
    case "IRVar":
      if (expr.namespace === "value" && !expr.moduleName) {
        out.add(expr.name);
      }
      break;
    case "IRLambda":
      collectBareVarNames(expr.body, out);
      break;
    case "IRApply":
      collectBareVarNames(expr.callee, out);
      for (const arg of expr.args)
        collectBareVarNames(arg, out);
      break;
    case "IRIf":
      collectBareVarNames(expr.condition, out);
      collectBareVarNames(expr.thenBranch, out);
      collectBareVarNames(expr.elseBranch, out);
      break;
    case "IRCase":
      collectBareVarNames(expr.discriminant, out);
      for (const branch of expr.branches)
        collectBareVarNames(branch.body, out);
      break;
    case "IRList":
    case "IRTuple":
      for (const el of expr.elements)
        collectBareVarNames(el, out);
      break;
    case "IRRecord":
      for (const f of expr.fields)
        collectBareVarNames(f.value, out);
      break;
    case "IRRecordUpdate":
      collectBareVarNames(expr.base, out);
      for (const f of expr.updates)
        collectBareVarNames(f.value, out);
      break;
    case "IRFieldAccess":
      collectBareVarNames(expr.target, out);
      break;
    case "IRUnary":
      collectBareVarNames(expr.operand, out);
      break;
    case "IRConstructor":
      for (const arg of expr.args)
        collectBareVarNames(arg, out);
      break;
    case "IRSelfLoop":
      collectBareVarNames(expr.body, out);
      break;
    case "IRLoopContinue":
      for (const arg of expr.args)
        collectBareVarNames(arg, out);
      break;
  }
}
function augmentImportsFromDefaults(syntheticValues, localValues, dependencies, semantics, resolvedImports) {
  const bareNames = new Set;
  for (const [name, value2] of Object.entries(syntheticValues)) {
    if (!name.startsWith("$default_"))
      continue;
    collectBareVarNames(value2.body, bareNames);
  }
  const missingNames = new Set;
  for (const name of bareNames) {
    if (localValues[name])
      continue;
    if (syntheticValues[name])
      continue;
    missingNames.add(name);
  }
  if (missingNames.size === 0)
    return;
  const nameToModule = new Map;
  for (const [moduleName, dep] of dependencies) {
    for (const missingName of missingNames) {
      if (nameToModule.has(missingName))
        continue;
      if (dep.values[missingName]) {
        nameToModule.set(missingName, moduleName);
      }
    }
  }
  const moduleToNames = new Map;
  for (const [name, moduleName] of nameToModule) {
    if (!moduleToNames.has(moduleName))
      moduleToNames.set(moduleName, []);
    moduleToNames.get(moduleName).push(name);
  }
  for (const [moduleName, names] of moduleToNames) {
    const existing = resolvedImports.find((r) => r.moduleName === moduleName);
    if (existing) {
      for (const name of names) {
        if (!existing.namedImports.includes(name)) {
          existing.namedImports.push(name);
        }
      }
    } else {
      resolvedImports.push({
        moduleName,
        namedImports: names
      });
    }
  }
}
function isTypeOnly(name, depModule, semantics) {
  const isProtocol = depModule?.protocols && Object.hasOwn(depModule.protocols, name) || Object.hasOwn(semantics.protocols, name);
  const isOpaque = depModule?.opaqueTypes && Object.hasOwn(depModule.opaqueTypes, name) || Object.hasOwn(semantics.opaqueTypes, name);
  const isADT = depModule?.adts && Object.hasOwn(depModule.adts, name) || Object.hasOwn(semantics.adts, name);
  return isProtocol || isOpaque || isADT;
}
// ../config/src/index.ts
import fs from "fs";
import path from "path";
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
import fs2 from "fs";
import path2 from "path";
class ParseErrorWithFilePath extends Error {
  span;
  filePath;
  constructor(message, span, filePath) {
    super(message);
    this.span = span;
    this.filePath = filePath;
    this.name = "ParseError";
  }
}
function parseWithFilePath(parseFunction, source, filePath, operatorRegistry) {
  try {
    return parseFunction(source, operatorRegistry);
  } catch (error) {
    if (error instanceof Error && "span" in error && !(("filePath" in error) && error.filePath)) {
      const parseError = error;
      throw new ParseErrorWithFilePath(parseError.message, parseError.span, filePath);
    }
    throw error;
  }
}
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
function mergeOperatorRegistries(...registries) {
  const merged = new Map;
  for (const registry of registries) {
    for (const [op, info] of registry) {
      merged.set(op, info);
    }
  }
  return merged;
}
function discoverModuleGraph(config, entryModuleName, optionsOrParseFunction, preferDistArg = false) {
  let options;
  if (typeof optionsOrParseFunction === "function") {
    const parseFunction2 = optionsOrParseFunction;
    options = {
      collectInfixDeclarations: () => ({ registry: new Map }),
      parseFunction: parseFunction2,
      preferDist: preferDistArg
    };
  } else {
    options = optionsOrParseFunction;
  }
  const {
    collectInfixDeclarations: collectInfixDeclarations2,
    parseFunction,
    preferDist = false
  } = options;
  const preliminaryModules = new Map;
  const visiting = new Set;
  function discoverModulePreliminary(moduleName) {
    if (preliminaryModules.has(moduleName)) {
      return;
    }
    if (visiting.has(moduleName)) {
      throw new Error(`Circular dependency detected: ${[...visiting, moduleName].join(" -> ")}`);
    }
    visiting.add(moduleName);
    const resolved = resolveModule({ config, moduleName, preferDist });
    const source = fs2.readFileSync(resolved.filePath, "utf8");
    const { registry: operatorRegistry } = collectInfixDeclarations2(source);
    const ast = parseWithFilePath(parseFunction, source, resolved.filePath, operatorRegistry);
    const dependencies = new Set;
    for (const imp of ast.imports) {
      dependencies.add(imp.moduleName);
    }
    for (const depName of dependencies) {
      discoverModulePreliminary(depName);
    }
    preliminaryModules.set(moduleName, {
      moduleName,
      packageName: resolved.packageName,
      filePath: resolved.filePath,
      srcDir: resolved.srcDir,
      source,
      dependencies,
      operatorRegistry
    });
    visiting.delete(moduleName);
  }
  discoverModulePreliminary(entryModuleName);
  const sortedModuleNames = topologicalSortPreliminary(preliminaryModules);
  const modules = new Map;
  for (const moduleName of sortedModuleNames) {
    const preliminary = preliminaryModules.get(moduleName);
    const dependencyRegistries = [];
    for (const depName of preliminary.dependencies) {
      const depModule = modules.get(depName);
      if (depModule) {
        dependencyRegistries.push(depModule.operatorRegistry);
      }
    }
    const combinedRegistry = mergeOperatorRegistries(...dependencyRegistries, preliminary.operatorRegistry);
    const ast = parseWithFilePath(parseFunction, preliminary.source, preliminary.filePath, combinedRegistry);
    modules.set(moduleName, {
      moduleName: preliminary.moduleName,
      packageName: preliminary.packageName,
      filePath: preliminary.filePath,
      srcDir: preliminary.srcDir,
      source: preliminary.source,
      ast,
      dependencies: preliminary.dependencies,
      operatorRegistry: preliminary.operatorRegistry
    });
  }
  return {
    modules,
    sortedModuleNames
  };
}
function topologicalSortPreliminary(modules) {
  const sorted = [];
  const visited = new Set;
  function visit(moduleName) {
    if (visited.has(moduleName)) {
      return;
    }
    visited.add(moduleName);
    const node = modules.get(moduleName);
    if (!node) {
      throw new Error(`Module "${moduleName}" not found in graph`);
    }
    for (const depName of node.dependencies) {
      visit(depName);
    }
    sorted.push(moduleName);
  }
  for (const moduleName of modules.keys()) {
    visit(moduleName);
  }
  return sorted;
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
function discoverAllModules(config, optionsOrParseFunction) {
  let options;
  if (typeof optionsOrParseFunction === "function") {
    const parseFunction2 = optionsOrParseFunction;
    options = {
      collectInfixDeclarations: () => ({ registry: new Map }),
      parseFunction: parseFunction2,
      preferDist: false
    };
  } else {
    options = optionsOrParseFunction;
  }
  const { collectInfixDeclarations: collectInfixDeclarations2, parseFunction } = options;
  const sourceModules = discoverSourceModules(config.srcDir);
  const preliminaryModules = new Map;
  const visiting = new Set;
  function discoverModulePreliminary(moduleName) {
    if (preliminaryModules.has(moduleName)) {
      return;
    }
    if (visiting.has(moduleName)) {
      throw new Error(`Circular dependency detected: ${[...visiting, moduleName].join(" -> ")}`);
    }
    visiting.add(moduleName);
    const resolved = resolveModule({ config, moduleName });
    const source = fs2.readFileSync(resolved.filePath, "utf8");
    const { registry: operatorRegistry } = collectInfixDeclarations2(source);
    const ast = parseWithFilePath(parseFunction, source, resolved.filePath, operatorRegistry);
    const dependencies = new Set;
    for (const imp of ast.imports) {
      dependencies.add(imp.moduleName);
    }
    for (const depName of dependencies) {
      discoverModulePreliminary(depName);
    }
    preliminaryModules.set(moduleName, {
      moduleName,
      packageName: resolved.packageName,
      filePath: resolved.filePath,
      srcDir: resolved.srcDir,
      source,
      dependencies,
      operatorRegistry
    });
    visiting.delete(moduleName);
  }
  for (const moduleName of sourceModules) {
    discoverModulePreliminary(moduleName);
  }
  const sortedModuleNames = topologicalSortPreliminary(preliminaryModules);
  const modules = new Map;
  for (const moduleName of sortedModuleNames) {
    const preliminary = preliminaryModules.get(moduleName);
    const dependencyRegistries = [];
    for (const depName of preliminary.dependencies) {
      const depModule = modules.get(depName);
      if (depModule) {
        dependencyRegistries.push(depModule.operatorRegistry);
      }
    }
    const combinedRegistry = mergeOperatorRegistries(...dependencyRegistries, preliminary.operatorRegistry);
    const ast = parseWithFilePath(parseFunction, preliminary.source, preliminary.filePath, combinedRegistry);
    modules.set(moduleName, {
      moduleName: preliminary.moduleName,
      packageName: preliminary.packageName,
      filePath: preliminary.filePath,
      srcDir: preliminary.srcDir,
      source: preliminary.source,
      ast,
      dependencies: preliminary.dependencies,
      operatorRegistry: preliminary.operatorRegistry
    });
  }
  return {
    modules,
    sortedModuleNames
  };
}

// ../codegen/src/index.ts
import fs3 from "fs";
import path3 from "path";

// ../codegen/src/sanitize.ts
var RESERVED_WORDS = new Set([
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "enum",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "let",
  "new",
  "null",
  "return",
  "static",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "undefined",
  "var",
  "void",
  "while",
  "with",
  "yield"
]);
function sanitizeIdentifier(name) {
  if (name.startsWith("(") && name.endsWith(")")) {
    name = name.slice(1, -1);
  }
  if (/^[+\-*/<>=!&|^%:.~$#@?]+$/.test(name)) {
    return sanitizeOperator(name);
  }
  if (RESERVED_WORDS.has(name)) {
    return `$${name}`;
  }
  return name;
}

// ../codegen/src/imports.ts
function generateDefaultImports(program2) {
  const lines = [];
  for (const { name, modulePath } of program2.defaultImports) {
    lines.push(`import ${name} from "${modulePath}";`);
  }
  return lines;
}
function generateExternalImports(externalBindings) {
  const lines = [];
  for (const [modulePath, bindings] of externalBindings) {
    if (bindings.size > 0) {
      const importSpecifiers = [];
      const sortedEntries = Array.from(bindings.entries()).sort(([a], [b]) => a.localeCompare(b));
      for (const [vibeName, { runtimeName, callArity }] of sortedEntries) {
        const safeVibeName = sanitizeIdentifier(vibeName);
        if (callArity > 0) {
          importSpecifiers.push(`${runtimeName} as $$${safeVibeName}`);
        } else if (runtimeName === vibeName || runtimeName === safeVibeName) {
          importSpecifiers.push(runtimeName);
        } else {
          importSpecifiers.push(`${runtimeName} as ${safeVibeName}`);
        }
      }
      lines.push(`import { ${importSpecifiers.join(", ")} } from "${modulePath}";`);
    }
  }
  return lines;
}
function generateDependencyImports(program2, modulePackages) {
  const lines = [];
  const currentPackage = program2.packageName;
  const currentDepth = program2.moduleName.split(".").length - 1;
  for (const resolved of program2.resolvedImports) {
    const importedPackage = modulePackages.get(resolved.moduleName) || resolved.moduleName;
    const importPath = calculateImportPath(currentPackage, currentDepth, importedPackage, resolved.moduleName);
    if (resolved.namespaceImport) {
      lines.push(`import * as ${resolved.namespaceImport} from "${importPath}";`);
    }
    if (resolved.namedImports.length > 0) {
      const sanitized = resolved.namedImports.map((n) => sanitizeIdentifier(n));
      lines.push(`import { ${sanitized.join(", ")} } from "${importPath}";`);
    }
  }
  return lines;
}
function calculateImportPath(currentPackage, currentDepth, importedPackage, importedModule) {
  const moduleSegments = importedModule.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const modulePath = moduleSegments.length > 1 ? moduleSegments.slice(0, -1).join("/") + `/${fileName}.js` : `${fileName}.js`;
  if (currentPackage === importedPackage) {
    if (currentDepth === 0) {
      return `./${modulePath}`;
    } else {
      const ups = "../".repeat(currentDepth);
      return `${ups}${modulePath}`;
    }
  } else {
    const ups = "../".repeat(currentDepth + 1);
    return `${ups}${importedPackage}/${modulePath}`;
  }
}
function calculateReExportPath(program2, targetModule, modulePackages) {
  const currentModule = program2.moduleName;
  const currentPackage = program2.packageName;
  const currentDepth = currentModule.split(".").length - 1;
  const targetPackage = modulePackages.get(targetModule) || targetModule.split(".")[0];
  const moduleSegments = targetModule.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const modulePath = moduleSegments.length > 1 ? moduleSegments.slice(0, -1).join("/") + `/${fileName}.js` : `${fileName}.js`;
  if (currentPackage === targetPackage) {
    if (currentDepth === 0) {
      return `./${modulePath}`;
    } else {
      const ups = "../".repeat(currentDepth);
      return `${ups}${modulePath}`;
    }
  } else {
    const ups = "../".repeat(currentDepth + 1);
    return `${ups}${targetPackage}/${modulePath}`;
  }
}

// ../codegen/src/instances.ts
function formatTypeKey2(type) {
  if (!type)
    return "unknown";
  switch (type.kind) {
    case "con":
      if (type.args.length === 0)
        return type.name;
      return `${type.name}_${type.args.map(formatTypeKey2).join("_")}`;
    case "var":
      return `v${type.id}`;
    case "fun":
      return `fn_${formatTypeKey2(type.from)}_${formatTypeKey2(type.to)}`;
    case "tuple":
      return `tuple_${type.elements.map(formatTypeKey2).join("_")}`;
    case "record":
      return `record`;
    case "list":
      return `list_${formatTypeKey2(type.element)}`;
    default:
      return "unknown";
  }
}
function isTypeVariable2(type) {
  switch (type.kind) {
    case "var":
      return true;
    case "con":
      return false;
    case "list":
      return isTypeVariable2(type.element);
    case "tuple":
      return type.elements.every(isTypeVariable2);
    case "fun":
      return isTypeVariable2(type.from) && isTypeVariable2(type.to);
    case "record":
      return Object.values(type.fields).every(isTypeVariable2);
    default:
      return false;
  }
}
function buildTypeVarSubstitution2(instanceTypeArgs, concreteTypes) {
  const subst = new Map;
  function collectSubst(instType, concreteType) {
    if (instType.kind === "var") {
      subst.set(instType.id, concreteType);
    } else if (instType.kind === "con" && concreteType.kind === "con") {
      for (let i = 0;i < instType.args.length && i < concreteType.args.length; i++) {
        collectSubst(instType.args[i], concreteType.args[i]);
      }
    } else if (instType.kind === "list" && concreteType.kind === "list") {
      collectSubst(instType.element, concreteType.element);
    } else if (instType.kind === "tuple" && concreteType.kind === "tuple") {
      for (let i = 0;i < instType.elements.length && i < concreteType.elements.length; i++) {
        collectSubst(instType.elements[i], concreteType.elements[i]);
      }
    } else if (instType.kind === "fun" && concreteType.kind === "fun") {
      collectSubst(instType.from, concreteType.from);
      collectSubst(instType.to, concreteType.to);
    }
  }
  for (let i = 0;i < instanceTypeArgs.length && i < concreteTypes.length; i++) {
    collectSubst(instanceTypeArgs[i], concreteTypes[i]);
  }
  return subst;
}
function typeStructureMatches2(instType, concreteType) {
  if (instType.kind === "var") {
    return true;
  }
  if (instType.kind === "con" && concreteType.kind === "con") {
    if (instType.name !== concreteType.name) {
      return false;
    }
    if (instType.args.length !== concreteType.args.length) {
      return false;
    }
    for (let i = 0;i < instType.args.length; i++) {
      if (!typeStructureMatches2(instType.args[i], concreteType.args[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "list" && concreteType.kind === "list") {
    return typeStructureMatches2(instType.element, concreteType.element);
  }
  if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    if (instType.elements.length !== concreteType.elements.length) {
      return false;
    }
    for (let i = 0;i < instType.elements.length; i++) {
      if (!typeStructureMatches2(instType.elements[i], concreteType.elements[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "fun" && concreteType.kind === "fun") {
    return typeStructureMatches2(instType.from, concreteType.from) && typeStructureMatches2(instType.to, concreteType.to);
  }
  return false;
}
function findPolymorphicInstance2(protocolName, instances) {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    const firstTypeArg = inst.typeArgs[0];
    if (firstTypeArg && firstTypeArg.kind === "var") {
      const typeKey = formatTypeKey2(firstTypeArg);
      return {
        key: `${protocolName}_${typeKey}`,
        instance: inst
      };
    }
  }
  return null;
}
function findMatchingInstance2(protocolName, concreteType, instances) {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    const instTypeArg = inst.typeArgs[0];
    if (!instTypeArg)
      continue;
    if (instTypeArg.kind === "var")
      continue;
    if (typeStructureMatches2(instTypeArg, concreteType)) {
      const typeKey = formatTypeKey2(instTypeArg);
      return {
        key: `${protocolName}_${typeKey}`,
        instance: inst
      };
    }
  }
  return null;
}
function getImportAliasForModule(moduleName, importAliases) {
  for (const alias of importAliases) {
    if (alias.moduleName === moduleName) {
      return alias.alias;
    }
  }
  const segments = moduleName.split(".");
  return segments[0] || moduleName;
}
function resolveDictReference(protocolName, typeKey, ctx, concreteType, allConcreteTypes) {
  const key = `${protocolName}_${typeKey}`;
  let dictRef = null;
  let instanceKey = key;
  if (ctx.localInstanceKeys.has(key)) {
    dictRef = `$dict_${key}`;
  } else {
    const sourceModule = ctx.instanceModules.get(key);
    if (sourceModule) {
      const importAlias = getImportAliasForModule(sourceModule, ctx.importAliases);
      dictRef = `${importAlias}.$dict_${key}`;
    }
  }
  if (!dictRef && concreteType) {
    const structuralMatch = findMatchingInstance2(protocolName, concreteType, ctx.instances);
    if (structuralMatch) {
      instanceKey = structuralMatch.key;
      if (ctx.localInstanceKeys.has(instanceKey)) {
        dictRef = `$dict_${instanceKey}`;
      } else {
        const sourceModule = ctx.instanceModules.get(instanceKey);
        if (sourceModule) {
          const importAlias = getImportAliasForModule(sourceModule, ctx.importAliases);
          dictRef = `${importAlias}.$dict_${instanceKey}`;
        }
      }
    }
  }
  if (!dictRef) {
    const polymorphicMatch = findPolymorphicInstance2(protocolName, ctx.instances);
    if (polymorphicMatch) {
      instanceKey = polymorphicMatch.key;
      if (ctx.localInstanceKeys.has(instanceKey)) {
        dictRef = `$dict_${instanceKey}`;
      } else {
        const sourceModule = ctx.instanceModules.get(instanceKey);
        if (sourceModule) {
          const importAlias = getImportAliasForModule(sourceModule, ctx.importAliases);
          dictRef = `${importAlias}.$dict_${instanceKey}`;
        }
      }
    }
  }
  if (!dictRef) {
    const typeName = typeKey.startsWith("v") ? "a type variable" : `'${typeKey}'`;
    throw new Error(`No instance of '${protocolName}' found for ${typeName}. ` + `You may need to add: implement ${protocolName} ${typeKey} where ...`);
  }
  const constraints = ctx.constrainedInstances.get(instanceKey);
  const matchedInstance = ctx.instances.find((inst) => inst.protocolName === protocolName && `${protocolName}_${formatTypeKey2(inst.typeArgs[0])}` === instanceKey);
  if (constraints && constraints.length > 0) {
    const constraintDicts = [];
    const seenProtocols = new Set;
    const typeVarSubst = allConcreteTypes && matchedInstance ? buildTypeVarSubstitution2(matchedInstance.typeArgs, allConcreteTypes) : new Map;
    for (const constraint of constraints) {
      if (!seenProtocols.has(constraint.protocolName)) {
        seenProtocols.add(constraint.protocolName);
        const constraintTypeArg = constraint.typeArgs[0];
        let resolvedType;
        if (constraintTypeArg && constraintTypeArg.kind === "var") {
          resolvedType = typeVarSubst.get(constraintTypeArg.id);
        } else if (constraintTypeArg) {
          resolvedType = constraintTypeArg;
        }
        if (resolvedType && (resolvedType.kind === "con" || resolvedType.kind === "list" || resolvedType.kind === "tuple")) {
          constraintDicts.push(resolveDictionaryForType(constraint.protocolName, resolvedType, ctx));
        } else {
          constraintDicts.push(`$dict_${constraint.protocolName}`);
        }
      }
    }
    return `${dictRef}(${constraintDicts.join(", ")})`;
  }
  return dictRef;
}
function resolveDictionaryForType(protocolName, type, ctx) {
  if (!type) {
    return `$dict_${protocolName}`;
  }
  if (type.kind === "con") {
    const typeKey = formatTypeKey2(type);
    return resolveDictReference(protocolName, typeKey, ctx, type);
  }
  if (type.kind === "list") {
    const listAsCon = {
      kind: "con",
      name: "List",
      args: [type.element]
    };
    const typeKey = formatTypeKey2(listAsCon);
    return resolveDictReference(protocolName, typeKey, ctx, listAsCon);
  }
  if (type.kind === "tuple") {
    const typeKey = formatTypeKey2(type);
    return resolveDictReference(protocolName, typeKey, ctx, type);
  }
  return `$dict_${protocolName}`;
}

// ../codegen/src/index.ts
function createCodegenContext(program2) {
  const ctx = {
    program: program2,
    indentLevel: 0,
    instances: program2.instances,
    protocols: program2.protocols,
    constructors: program2.constructors,
    externalImports: new Set(program2.externalImports),
    externalBindings: new Map,
    instanceDictNames: new Map,
    localInstanceKeys: new Set,
    instanceModules: new Map,
    uniqueCounter: 0,
    dictParamsInScope: new Set,
    concreteConstraints: new Map,
    generatedDictNames: [],
    protocolMethodMap: new Map,
    varTypes: new Map,
    constrainedInstances: new Map,
    expectedReturnType: undefined,
    usedShortCircuitOps: new Set,
    instanceMap: new Map,
    tcoLoopParams: null
  };
  for (const [protocolName, protocol] of Object.entries(program2.protocols)) {
    for (const method of protocol.methods) {
      ctx.protocolMethodMap.set(method.name, protocolName);
    }
  }
  for (const [vibeName, value2] of Object.entries(program2.values)) {
    if (value2.isExternal && value2.externalTarget) {
      const { modulePath, exportName, callArity } = value2.externalTarget;
      if (!ctx.externalBindings.has(modulePath)) {
        ctx.externalBindings.set(modulePath, new Map);
      }
      ctx.externalBindings.get(modulePath).set(vibeName, { runtimeName: exportName, callArity });
    }
  }
  const currentModule = program2.moduleName;
  const sourceInstances = program2.sourceModule.instances;
  for (let i = 0;i < program2.instances.length; i++) {
    const inst = program2.instances[i];
    if (!inst || inst.typeArgs.length === 0)
      continue;
    const typeKey = formatTypeKey2(inst.typeArgs[0]);
    const key = `${inst.protocolName}_${typeKey}`;
    ctx.instanceDictNames.set(key, `$dict_${key}`);
    const sourceInst = sourceInstances[i];
    const instanceModule = sourceInst?.moduleName ? sourceInst.moduleName : currentModule;
    ctx.instanceModules.set(key, instanceModule);
    if (instanceModule === currentModule) {
      ctx.localInstanceKeys.add(key);
    }
    if (inst.constraints.length > 0) {
      ctx.constrainedInstances.set(key, inst.constraints);
    }
    ctx.instanceMap.set(`$dict_${key}`, inst);
  }
  return ctx;
}
function freshName2(ctx, base) {
  const name = `$${base}_${ctx.uniqueCounter}`;
  ctx.uniqueCounter++;
  return name;
}
function toInstanceContext(ctx) {
  return {
    instances: ctx.instances,
    instanceDictNames: ctx.instanceDictNames,
    localInstanceKeys: ctx.localInstanceKeys,
    instanceModules: ctx.instanceModules,
    constrainedInstances: ctx.constrainedInstances,
    importAliases: ctx.program.importAliases,
    dictParamsInScope: ctx.dictParamsInScope,
    concreteConstraints: ctx.concreteConstraints,
    expectedReturnType: ctx.expectedReturnType
  };
}
function resolveDictReferenceCtx(protocolName, typeKey, ctx, concreteType, allConcreteTypes) {
  return resolveDictReference(protocolName, typeKey, toInstanceContext(ctx), concreteType, allConcreteTypes);
}
function resolveDictionaryForTypeCtx(protocolName, type, ctx) {
  return resolveDictionaryForType(protocolName, type, toInstanceContext(ctx));
}
function generate(program2, options = {}) {
  const ctx = createCodegenContext(program2);
  const { modulePackages = new Map } = options;
  const headerLines = [];
  const bodyLines = [];
  const defaultImportLines = generateDefaultImports(program2);
  if (defaultImportLines.length > 0) {
    headerLines.push(...defaultImportLines);
    headerLines.push("");
  }
  const importLines = generateImports(ctx);
  if (importLines.length > 0) {
    headerLines.push(...importLines);
    headerLines.push("");
  }
  const depImportLines = generateDependencyImports(program2, modulePackages);
  if (depImportLines.length > 0) {
    headerLines.push(...depImportLines);
    headerLines.push("");
  }
  const ctorLines = generateConstructors(ctx);
  if (ctorLines.length > 0) {
    bodyLines.push("// ADT Constructors");
    bodyLines.push(...ctorLines);
    bodyLines.push("");
  }
  bodyLines.push("// Values");
  for (const scc of program2.dependencyOrder) {
    const sccLines = generateSCC(scc, ctx);
    bodyLines.push(...sccLines);
  }
  const exportLines = generateExports(program2, ctx, modulePackages);
  if (exportLines.length > 0) {
    bodyLines.push("");
    bodyLines.push(...exportLines);
  }
  const helperLines = [];
  if (ctx.usedShortCircuitOps.size > 0) {
    helperLines.push("// Short-Circuit Operator Helpers");
    for (const op of ctx.usedShortCircuitOps) {
      const helper2 = SHORT_CIRCUIT_HELPERS[op];
      if (helper2) {
        helperLines.push(`const ${helper2.name} = ${helper2.impl};`);
      }
    }
    helperLines.push("");
  }
  return {
    moduleName: program2.moduleName,
    packageName: program2.packageName,
    code: [...headerLines, ...helperLines, ...bodyLines].join(`
`),
    imports: ctx.externalBindings,
    exports: Object.keys(program2.values).filter((name) => !name.startsWith("$"))
  };
}
function generateImports(ctx) {
  return generateExternalImports(ctx.externalBindings);
}
function generateConstructors(ctx) {
  const lines = [];
  const generated = new Set;
  const currentModule = ctx.program.moduleName;
  const byParent = new Map;
  for (const ctor of Object.values(ctx.constructors)) {
    if (!byParent.has(ctor.parentType)) {
      byParent.set(ctor.parentType, []);
    }
    byParent.get(ctor.parentType).push(ctor);
  }
  for (const [parentType, ctors] of byParent) {
    if (parentType === BOOL_TYPE_NAME)
      continue;
    if (parentType === UNIT_TYPE_NAME)
      continue;
    const adtInfo = ctx.program.adts[parentType];
    if (adtInfo?.moduleName && adtInfo.moduleName !== currentModule && adtInfo.moduleName !== BUILTIN_MODULE_NAME) {
      continue;
    }
    ctors.sort((a, b) => a.tag - b.tag);
    for (const ctor of ctors) {
      if (generated.has(ctor.name))
        continue;
      generated.add(ctor.name);
      const safeName = sanitizeIdentifier(ctor.name);
      if (ctor.arity === 0) {
        lines.push(`const ${safeName} = { $tag: ${ctor.tag} };`);
      } else {
        const params = Array.from({ length: ctor.arity }, (_, i) => `$${i}`);
        const fields = params.map((p) => p).join(", ");
        let body = `({ $tag: ${ctor.tag}, ${fields} })`;
        for (let i = params.length - 1;i >= 0; i--) {
          body = `(${params[i]}) => ${body}`;
        }
        lines.push(`const ${safeName} = ${body};`);
      }
    }
  }
  return lines;
}
function generateInstanceDictionary(inst, ctx) {
  const lines = [];
  const currentModule = ctx.program.moduleName;
  const typeKey = formatTypeKey2(inst.typeArgs[0]);
  const key = `${inst.protocolName}_${typeKey}`;
  const dictName = ctx.instanceDictNames.get(key);
  if (!dictName)
    return [];
  const instanceModule = ctx.instanceModules.get(key);
  if (instanceModule && instanceModule !== currentModule) {
    return [];
  }
  const constraints = ctx.constrainedInstances.get(key);
  const methodEntries = [];
  for (const [methodName, implName] of Object.entries(inst.methods)) {
    const safeName = sanitizeIdentifier(methodName);
    const safeImpl = sanitizeIdentifier(implName);
    if (constraints && constraints.length > 0) {
      const dictParams = [];
      const seenProtocols = new Set;
      for (const constraint of constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      }
      const dictPasses = dictParams.map((d) => `(${d})`).join("");
      methodEntries.push(`  ${safeName}: ${safeImpl}${dictPasses}`);
    } else {
      methodEntries.push(`  ${safeName}: ${safeImpl}`);
    }
  }
  if (methodEntries.length > 0) {
    if (constraints && constraints.length > 0) {
      const dictParams = [];
      const seenProtocols = new Set;
      for (const constraint of constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      }
      lines.push(`const ${dictName} = (${dictParams.join(", ")}) => ({`);
      lines.push(methodEntries.join(`,
`));
      lines.push(`});`);
    } else {
      lines.push(`const ${dictName} = {`);
      lines.push(methodEntries.join(`,
`));
      lines.push(`};`);
    }
    ctx.generatedDictNames.push(dictName);
  }
  return lines;
}
function generateSCC(scc, ctx) {
  const lines = [];
  if (scc.isMutuallyRecursive) {
    for (const name of scc.values) {
      const safeName = sanitizeIdentifier(name);
      lines.push(`let ${safeName};`);
    }
    for (const name of scc.values) {
      if (ctx.program.values[name]) {
        const value2 = ctx.program.values[name];
        const safeName = sanitizeIdentifier(name);
        const body = generateValue(value2, ctx);
        lines.push(`${safeName} = ${body};`);
      } else if (ctx.instanceMap.has(name)) {
        const inst = ctx.instanceMap.get(name);
        const dictLines = generateInstanceDictionary(inst, ctx);
        for (const line of dictLines) {
          lines.push(line.startsWith("const ") ? line.slice(6) : line);
        }
      }
    }
  } else {
    for (const name of scc.values) {
      if (ctx.program.values[name]) {
        const value2 = ctx.program.values[name];
        if (value2.isExternal && (!value2.externalTarget || value2.externalTarget.callArity === 0))
          continue;
        const safeName = sanitizeIdentifier(name);
        if (value2.isExternal && value2.externalTarget && value2.externalTarget.callArity > 0) {
          lines.push(`const ${safeName} = ${generateExternalWrapper(value2)};`);
          continue;
        }
        if (value2.propertyAccess) {
          lines.push(`const ${safeName} = ${generatePropertyAccess(value2)};`);
          continue;
        }
        const body = generateValue(value2, ctx);
        lines.push(`const ${safeName} = ${body};`);
      } else if (ctx.instanceMap.has(name)) {
        const inst = ctx.instanceMap.get(name);
        lines.push(...generateInstanceDictionary(inst, ctx));
      }
    }
  }
  return lines;
}
function generatePropertyAccess(value2) {
  const { variant, key, callArity } = value2.propertyAccess;
  if (variant === "val") {
    return key;
  }
  if (variant === "get") {
    return `($recv) => $recv.${key}`;
  }
  const argNames = Array.from({ length: callArity }, (_, i) => `$a${i}`);
  let result = `$recv.${key}(${argNames.join(", ")})`;
  for (let i = argNames.length - 1;i >= 0; i--) {
    result = `(${argNames[i]}) => ${result}`;
  }
  result = `($recv) => ${result}`;
  return result;
}
function generateExternalWrapper(value2) {
  const { callArity } = value2.externalTarget;
  const safeName = sanitizeIdentifier(value2.name);
  const privateName = `$$${safeName}`;
  const argNames = Array.from({ length: callArity }, (_, i) => `$a${i}`);
  let result = `${privateName}(${argNames.join(", ")})`;
  for (let i = argNames.length - 1;i >= 0; i--) {
    result = `(${argNames[i]}) => ${result}`;
  }
  return result;
}
function generateValue(value2, ctx) {
  if (value2.isExternal) {
    return `/* external: ${value2.externalTarget?.exportName} */`;
  }
  const dictParams = [];
  const seenConstraints = new Set;
  const concreteConstraints = new Map;
  for (const constraint of value2.constraints) {
    const typeArg = constraint.typeArgs[0];
    if (typeArg) {
      if (typeArg.kind === "var") {
        const dictKey = `${constraint.protocolName}`;
        if (!seenConstraints.has(dictKey)) {
          seenConstraints.add(dictKey);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      } else if (typeArg.kind === "con") {
        concreteConstraints.set(constraint.protocolName, typeArg);
      }
    }
  }
  const varTypes = new Map;
  let returnType = value2.type;
  if (value2.params.length > 0 && value2.type.kind === "fun") {
    let currentType = value2.type;
    for (let i = 0;i < value2.params.length; i++) {
      const param = value2.params[i];
      if (currentType.kind === "fun") {
        if (param?.kind === "IRVarPattern") {
          varTypes.set(param.name, currentType.from);
        }
        currentType = currentType.to;
      }
    }
    returnType = currentType;
  }
  if (value2.body.kind === "IRLambda" && returnType.kind === "fun") {
    let currentType = returnType;
    for (const param of value2.body.params) {
      if (currentType.kind === "fun") {
        if (param?.kind === "IRVarPattern") {
          varTypes.set(param.name, currentType.from);
        }
        currentType = currentType.to;
      }
    }
    returnType = currentType;
  }
  const prevDictParams = ctx.dictParamsInScope;
  const prevConcreteConstraints = ctx.concreteConstraints;
  const prevVarTypes = ctx.varTypes;
  const prevExpectedReturnType = ctx.expectedReturnType;
  ctx.dictParamsInScope = new Set(dictParams);
  ctx.concreteConstraints = concreteConstraints;
  ctx.varTypes = varTypes;
  ctx.expectedReturnType = returnType;
  if (value2.params.length > 0 || dictParams.length > 0) {
    let body2 = generateExpr(value2.body, ctx);
    for (let i = value2.params.length - 1;i >= 0; i--) {
      const param = value2.params[i];
      if (!param)
        continue;
      const paramCode = generatePattern(param, ctx);
      body2 = `(${paramCode}) => ${body2}`;
    }
    for (let i = dictParams.length - 1;i >= 0; i--) {
      body2 = `(${dictParams[i]}) => ${body2}`;
    }
    ctx.dictParamsInScope = prevDictParams;
    ctx.concreteConstraints = prevConcreteConstraints;
    ctx.varTypes = prevVarTypes;
    ctx.expectedReturnType = prevExpectedReturnType;
    return body2;
  }
  const body = generateExpr(value2.body, ctx);
  ctx.dictParamsInScope = prevDictParams;
  ctx.concreteConstraints = prevConcreteConstraints;
  ctx.varTypes = prevVarTypes;
  ctx.expectedReturnType = prevExpectedReturnType;
  return body;
}
function generateExpr(expr, ctx) {
  switch (expr.kind) {
    case "IRVar":
      return generateVar(expr, ctx);
    case "IRModuleAccess":
      return generateModuleAccess(expr, ctx);
    case "IRLiteral":
      return generateLiteral(expr);
    case "IRLambda":
      return generateLambda(expr, ctx);
    case "IRApply":
      return generateApply(expr, ctx);
    case "IRIf":
      return generateIf(expr, ctx);
    case "IRCase":
      return generateCase(expr, ctx);
    case "IRTuple":
      return generateTuple(expr, ctx);
    case "IRUnit":
      return "undefined";
    case "IRList":
      return generateList(expr, ctx);
    case "IRRecord":
      return generateRecord(expr, ctx);
    case "IRRecordUpdate":
      return generateRecordUpdate(expr, ctx);
    case "IRFieldAccess":
      return generateFieldAccess(expr, ctx);
    case "IRConstructor":
      return generateConstructorExpr(expr, ctx);
    case "IRUnary":
      return `-${generateExpr(expr.operand, ctx)}`;
    case "IRSelfLoop":
      return generateSelfLoop(expr, ctx);
    case "IRLoopContinue":
      throw new Error("IRLoopContinue found outside IRSelfLoop \u2014 compiler bug");
    default:
      const _exhaustive = expr;
      throw new Error(`Unknown expression kind: ${expr.kind}`);
  }
}
function isFullyConcrete(type) {
  switch (type.kind) {
    case "var":
      return false;
    case "con":
      return type.args.every(isFullyConcrete);
    case "list":
      return isFullyConcrete(type.element);
    case "tuple":
      return type.elements.every(isFullyConcrete);
    case "fun":
      return isFullyConcrete(type.from) && isFullyConcrete(type.to);
    case "record":
      return Object.values(type.fields).every(isFullyConcrete);
    default:
      return true;
  }
}
function generateVar(expr, ctx) {
  const currentModule = ctx.program.moduleName;
  if (SHORT_CIRCUIT_OPERATORS.has(expr.name)) {
    const helper2 = SHORT_CIRCUIT_HELPERS[expr.name];
    if (helper2) {
      ctx.usedShortCircuitOps.add(expr.name);
      return helper2.name;
    }
  }
  const value2 = ctx.program.values[expr.name];
  if (value2?.isExternal && value2.externalTarget) {
    return sanitizeIdentifier(expr.name);
  }
  const protocolName = ctx.protocolMethodMap.get(expr.name);
  if (protocolName) {
    const sanitizedName = sanitizeIdentifier(expr.name);
    const dictParam = `$dict_${protocolName}`;
    if (expr.constraint) {
      const typeArg = expr.constraint.typeArgs[0];
      if (typeArg && (typeArg.kind === "con" || typeArg.kind === "tuple" || typeArg.kind === "list") && isFullyConcrete(typeArg)) {
        const typeKey = formatTypeKey2(typeArg);
        const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx, typeArg, expr.constraint.typeArgs);
        return `${dictRef}.${sanitizedName}`;
      }
    }
    if (ctx.dictParamsInScope.has(dictParam)) {
      return `${dictParam}.${sanitizedName}`;
    }
    if (expr.constraint) {
      const typeArg = expr.constraint.typeArgs[0];
      if (typeArg && (typeArg.kind === "con" || typeArg.kind === "tuple" || typeArg.kind === "list")) {
        const typeKey = formatTypeKey2(typeArg);
        const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx, typeArg, expr.constraint.typeArgs);
        return `${dictRef}.${sanitizedName}`;
      }
    }
    if (expr.type && expr.type.kind === "fun") {
      const argType = expr.type.from;
      if (argType.kind === "con") {
        const typeKey = formatTypeKey2(argType);
        const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx);
        return `${dictRef}.${sanitizedName}`;
      }
    }
    const constraintType = ctx.concreteConstraints.get(protocolName);
    if (constraintType && constraintType.kind === "con") {
      const typeKey = formatTypeKey2(constraintType);
      const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx);
      return `${dictRef}.${sanitizedName}`;
    }
    if (ctx.expectedReturnType && !isTypeVariable2(ctx.expectedReturnType)) {
      const typeKey = formatTypeKey2(ctx.expectedReturnType);
      const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx, ctx.expectedReturnType);
      return `${dictRef}.${sanitizedName}`;
    }
    throw new Error(`Cannot resolve protocol method '${expr.name}' in monomorphic context without type info.`);
  }
  if (expr.moduleName && expr.moduleName !== currentModule && expr.moduleName !== BUILTIN_MODULE_NAME) {
    const importAlias = getImportAliasForModule(expr.moduleName, ctx.program.importAliases);
    return `${importAlias}.${sanitizeIdentifier(expr.name)}`;
  }
  const safeName = sanitizeIdentifier(expr.name);
  return safeName;
}
function generateModuleAccess(expr, ctx) {
  const valueName = sanitizeIdentifier(expr.valueName);
  let result = `${expr.importAlias}.${valueName}`;
  if (expr.constraints && expr.constraints.length > 0) {
    const seenProtocols = new Set;
    for (const constraint of expr.constraints) {
      if (!seenProtocols.has(constraint.protocolName)) {
        seenProtocols.add(constraint.protocolName);
        const typeArg = constraint.typeArgs[0];
        const dictName = resolveDictionaryForTypeCtx(constraint.protocolName, typeArg, ctx);
        result = `${result}(${dictName})`;
      }
    }
  }
  return result;
}
function interpretEscapes(raw) {
  let result = "";
  for (let i = 0;i < raw.length; i++) {
    if (raw[i] === "\\" && i + 1 < raw.length) {
      const next = raw[i + 1];
      switch (next) {
        case "n":
          result += `
`;
          i++;
          break;
        case "t":
          result += "\t";
          i++;
          break;
        case "r":
          result += "\r";
          i++;
          break;
        case "\\":
          result += "\\";
          i++;
          break;
        case "'":
          result += "'";
          i++;
          break;
        case '"':
          result += '"';
          i++;
          break;
        default:
          result += raw[i];
      }
    } else {
      result += raw[i];
    }
  }
  return result;
}
function generateLiteral(expr) {
  switch (expr.literalType) {
    case "int":
    case "float":
      return String(expr.value);
    case "string": {
      let strVal = String(expr.value);
      if (strVal.startsWith('"') && strVal.endsWith('"')) {
        strVal = strVal.slice(1, -1);
      }
      return JSON.stringify(interpretEscapes(strVal));
    }
    case "char": {
      let charVal = String(expr.value);
      if (charVal.startsWith("'") && charVal.endsWith("'")) {
        charVal = charVal.slice(1, -1);
      }
      return JSON.stringify(interpretEscapes(charVal));
    }
    case "bool":
      return expr.value ? "true" : "false";
  }
}
function generateLambda(expr, ctx) {
  const body = generateExpr(expr.body, ctx);
  if (expr.params.length === 0) {
    return `() => ${body}`;
  }
  let result = body;
  for (let i = expr.params.length - 1;i >= 0; i--) {
    const param = expr.params[i];
    if (!param)
      continue;
    const paramCode = generatePattern(param, ctx);
    result = `(${paramCode}) => ${result}`;
  }
  return result;
}
function generateApply(expr, ctx) {
  const protocolMethodResult = tryGenerateProtocolMethodApply(expr, ctx);
  if (protocolMethodResult !== null) {
    return protocolMethodResult;
  }
  let callee = generateExpr(expr.callee, ctx);
  if (expr.callee.kind === "IRLambda") {
    callee = `(${callee})`;
  }
  let dictPasses = [];
  if (expr.callee.kind === "IRVar") {
    const calleeValue = ctx.program.values[expr.callee.name];
    if (calleeValue && calleeValue.constraints && calleeValue.constraints.length > 0) {
      const seenProtocols = new Set;
      for (const constraint of calleeValue.constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);
          const typeArg = constraint.typeArgs[0];
          const dictName = resolveDictionaryForTypeCtx(constraint.protocolName, typeArg, ctx);
          dictPasses.push(dictName);
        }
      }
    }
  }
  let result = callee;
  for (const dict of dictPasses) {
    result = `${result}(${dict})`;
  }
  for (const arg of expr.args) {
    const argCode = generateExpr(arg, ctx);
    result = `${result}(${argCode})`;
  }
  return result;
}
function tryGenerateProtocolMethodApply(expr, ctx) {
  const { methodName, methodVar, operands } = extractMethodAndOperands(expr);
  if (!methodName) {
    return null;
  }
  const protocolName = ctx.protocolMethodMap.get(methodName);
  if (!protocolName) {
    return null;
  }
  if (methodVar?.constraint) {
    const typeArg = methodVar.constraint.typeArgs[0];
    if (typeArg && typeArg.kind === "con" && isFullyConcrete(typeArg)) {
      const sanitizedName2 = sanitizeIdentifier(methodName);
      const typeKey2 = formatTypeKey2(typeArg);
      const dictRef2 = resolveDictReferenceCtx(protocolName, typeKey2, ctx, typeArg, methodVar.constraint.typeArgs);
      let result2 = `${dictRef2}.${sanitizedName2}`;
      for (const operand of operands) {
        const argCode = generateExpr(operand, ctx);
        result2 = `${result2}(${argCode})`;
      }
      return result2;
    }
  }
  const dictParam = `$dict_${protocolName}`;
  if (ctx.dictParamsInScope.has(dictParam)) {
    return null;
  }
  let concreteType = inferConcreteTypeFromOperands(operands, ctx);
  let allOperandTypes = inferAllTypesFromOperands(operands, ctx);
  if (ctx.expectedReturnType && !isTypeVariable2(ctx.expectedReturnType)) {
    allOperandTypes = [...allOperandTypes, ctx.expectedReturnType];
  }
  if (!concreteType) {
    const constraintType = ctx.concreteConstraints.get(protocolName);
    if (constraintType) {
      concreteType = constraintType;
    }
  }
  if (!concreteType) {
    return null;
  }
  const sanitizedName = sanitizeIdentifier(methodName);
  const typeKey = formatTypeKey2(concreteType);
  const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx, concreteType, allOperandTypes.length > 0 ? allOperandTypes : undefined);
  let result = `${dictRef}.${sanitizedName}`;
  for (const operand of operands) {
    const argCode = generateExpr(operand, ctx);
    result = `${result}(${argCode})`;
  }
  return result;
}
function extractMethodAndOperands(expr) {
  const operands = [];
  let current2 = expr;
  while (current2.kind === "IRApply") {
    operands.unshift(...current2.args);
    current2 = current2.callee;
  }
  if (current2.kind === "IRVar" && current2.namespace === "value") {
    return { methodName: current2.name, methodVar: current2, operands };
  }
  return { methodName: null, methodVar: null, operands: [] };
}
function inferConcreteTypeFromOperands(operands, ctx) {
  for (const operand of operands) {
    const type = inferExprType(operand, ctx);
    if (type && type.kind === "con") {
      return type;
    }
  }
  return null;
}
function inferAllTypesFromOperands(operands, ctx) {
  const types2 = [];
  for (const operand of operands) {
    const type = inferExprType(operand, ctx);
    if (type) {
      types2.push(type);
    }
  }
  return types2;
}
function inferExprType(expr, ctx) {
  switch (expr.kind) {
    case "IRLiteral":
      switch (expr.literalType) {
        case "int":
          return { kind: "con", name: "Int", args: [] };
        case "float":
          return { kind: "con", name: "Float", args: [] };
        case "string":
          return { kind: "con", name: "String", args: [] };
        case "char":
          return { kind: "con", name: "Char", args: [] };
        case "bool":
          return { kind: "con", name: "Bool", args: [] };
      }
      break;
    case "IRConstructor": {
      const ctorInfo = ctx.constructors[expr.name];
      if (ctorInfo) {
        return { kind: "con", name: ctorInfo.parentType, args: [] };
      }
      break;
    }
    case "IRVar":
      if (expr.type) {
        return expr.type;
      }
      const varType = ctx.varTypes.get(expr.name);
      if (varType) {
        return varType;
      }
      break;
    case "IRFieldAccess":
      const targetType = inferExprType(expr.target, ctx);
      if (targetType && targetType.kind === "record") {
        const fieldType = targetType.fields[expr.field];
        if (fieldType) {
          return fieldType;
        }
      }
      break;
    case "IRApply": {
      const { methodName, operands } = extractMethodAndOperands(expr);
      if (methodName) {
        const protocolName = ctx.protocolMethodMap.get(methodName);
        if (protocolName) {
          const protocol = ctx.protocols[protocolName];
          if (protocol) {
            const method = protocol.methods.find((m) => m.name === methodName);
            if (method && method.type.kind === "fun") {
              let resultType = method.type;
              for (let i = 0;i < operands.length && resultType.kind === "fun"; i++) {
                resultType = resultType.to;
              }
              if (resultType.kind === "con") {
                return resultType;
              }
              if (resultType.kind === "var") {
                const opType = inferConcreteTypeFromOperands(operands, ctx);
                if (opType) {
                  return opType;
                }
              }
            }
          }
        }
      }
      for (const operand of operands) {
        const opType = inferExprType(operand, ctx);
        if (opType && opType.kind === "con") {
          return opType;
        }
      }
      break;
    }
    case "IRUnary":
      return inferExprType(expr.operand, ctx);
    case "IRList": {
      if (expr.elements.length > 0) {
        const elemType = inferExprType(expr.elements[0], ctx);
        if (elemType) {
          return { kind: "con", name: "List", args: [elemType] };
        }
      }
      return { kind: "con", name: "List", args: [{ kind: "var", id: -1 }] };
    }
    case "IRTuple": {
      const elemTypes = [];
      for (const elem of expr.elements) {
        const elemType = inferExprType(elem, ctx);
        if (elemType) {
          elemTypes.push(elemType);
        } else {
          elemTypes.push({ kind: "var", id: -1 });
        }
      }
      return { kind: "tuple", elements: elemTypes };
    }
  }
  return null;
}
function generateSelfLoop(expr, ctx) {
  const prevParams = ctx.tcoLoopParams;
  ctx.tcoLoopParams = expr.paramNames.map((n) => sanitizeIdentifier(n));
  const stmts = generateExprAsStmt(expr.body, ctx);
  ctx.tcoLoopParams = prevParams;
  return `{ while (true) { ${stmts} } }`;
}
function generateExprAsStmt(expr, ctx) {
  switch (expr.kind) {
    case "IRLoopContinue": {
      const paramNames = ctx.tcoLoopParams;
      const argExprs = expr.args.map((a) => generateExpr(a, ctx));
      if (paramNames.length === 1) {
        return `${paramNames[0]} = ${argExprs[0]}; continue;`;
      }
      return `[${paramNames.join(", ")}] = [${argExprs.join(", ")}]; continue;`;
    }
    case "IRIf": {
      const cond = generateExpr(expr.condition, ctx);
      const thenStmt = generateExprAsStmt(expr.thenBranch, ctx);
      const elseStmt = generateExprAsStmt(expr.elseBranch, ctx);
      return `if (${cond}) { ${thenStmt} } else { ${elseStmt} }`;
    }
    case "IRCase": {
      const matchName = freshName2(ctx, "match");
      const discriminant = generateExpr(expr.discriminant, ctx);
      const branches = [];
      for (const branch of expr.branches) {
        const { condition, bindings } = generatePatternMatchCode(branch.pattern, matchName, ctx);
        const bodyStmt = generateExprAsStmt(branch.body, ctx);
        let code = "";
        if (condition) {
          code = `if (${condition}) { `;
        } else {
          code = "{ ";
        }
        if (bindings.length > 0) {
          code += bindings.join(" ");
          code += " ";
        }
        code += `${bodyStmt} }`;
        branches.push(code);
      }
      branches.push(`throw new Error("Pattern match failed");`);
      return `{ const ${matchName} = ${discriminant}; ${branches.join(" ")} }`;
    }
    case "IRSelfLoop":
      return generateSelfLoop(expr, ctx);
    case "IRApply": {
      if (expr.callee.kind === "IRLambda" && expr.callee.params.length === 1 && expr.args.length === 1) {
        const paramCode = generatePattern(expr.callee.params[0], ctx);
        const valueCode = generateExpr(expr.args[0], ctx);
        const bodyStmt = generateExprAsStmt(expr.callee.body, ctx);
        return `{ const ${paramCode} = ${valueCode}; ${bodyStmt} }`;
      }
      return `return ${generateExpr(expr, ctx)};`;
    }
    default:
      return `return ${generateExpr(expr, ctx)};`;
  }
}
function generateIf(expr, ctx) {
  const cond = generateExpr(expr.condition, ctx);
  const then_ = generateExpr(expr.thenBranch, ctx);
  const else_ = generateExpr(expr.elseBranch, ctx);
  return `(${cond} ? ${then_} : ${else_})`;
}
function generateCase(expr, ctx) {
  const matchName = freshName2(ctx, "match");
  const discriminant = generateExpr(expr.discriminant, ctx);
  const branches = [];
  for (const branch of expr.branches) {
    const { condition, bindings, body } = generateBranchCode(branch.pattern, matchName, branch.body, ctx);
    let code = "";
    if (condition) {
      code = `if (${condition}) { `;
    } else {
      code = "{ ";
    }
    if (bindings.length > 0) {
      code += bindings.join(" ");
      code += " ";
    }
    code += `return ${body}; }`;
    branches.push(code);
  }
  branches.push(`throw new Error("Pattern match failed");`);
  return `((${matchName}) => { ${branches.join(" ")} })(${discriminant})`;
}
function generateBranchCode(pattern, matchName, body, ctx) {
  const bindings = [];
  function buildConditionAndBindings(pat, accessor) {
    switch (pat.kind) {
      case "IRVarPattern":
        const safeName = sanitizeIdentifier(pat.name);
        bindings.push(`const ${safeName} = ${accessor};`);
        return null;
      case "IRWildcardPattern":
        return null;
      case "IRConstructorPattern": {
        const ctor = ctx.constructors[pat.name];
        if (ctor?.parentType === BOOL_TYPE_NAME) {
          const boolValue = pat.name === "True" ? "true" : "false";
          return `${accessor} === ${boolValue}`;
        }
        const tag = pat.tag;
        const conditions = [`${accessor}.$tag === ${tag}`];
        for (let i = 0;i < pat.args.length; i++) {
          const argPat = pat.args[i];
          if (!argPat)
            continue;
          const argAccessor = `${accessor}.$${i}`;
          const argCondition = buildConditionAndBindings(argPat, argAccessor);
          if (argCondition) {
            conditions.push(argCondition);
          }
        }
        return conditions.join(" && ");
      }
      case "IRTuplePattern": {
        const conditions = [];
        for (let i = 0;i < pat.elements.length; i++) {
          const elemPat = pat.elements[i];
          if (!elemPat)
            continue;
          const elemAccessor = `${accessor}[${i}]`;
          const elemCondition = buildConditionAndBindings(elemPat, elemAccessor);
          if (elemCondition) {
            conditions.push(elemCondition);
          }
        }
        return conditions.length > 0 ? conditions.join(" && ") : null;
      }
      case "IRLiteralPattern":
        const lit = generateLiteralPatternValue(pat);
        return `${accessor} === ${lit}`;
      case "IRListPattern": {
        const conditions = [
          `Array.isArray(${accessor})`,
          `${accessor}.length === ${pat.elements.length}`
        ];
        for (let i = 0;i < pat.elements.length; i++) {
          const elemPat = pat.elements[i];
          if (!elemPat)
            continue;
          const elemAccessor = `${accessor}[${i}]`;
          const elemCondition = buildConditionAndBindings(elemPat, elemAccessor);
          if (elemCondition) {
            conditions.push(elemCondition);
          }
        }
        return conditions.join(" && ");
      }
      case "IRConsPattern": {
        const conditions = [
          `Array.isArray(${accessor})`,
          `${accessor}.length >= 1`
        ];
        const headCondition = buildConditionAndBindings(pat.head, `${accessor}[0]`);
        if (headCondition) {
          conditions.push(headCondition);
        }
        const tailCondition = buildConditionAndBindings(pat.tail, `${accessor}.slice(1)`);
        if (tailCondition) {
          conditions.push(tailCondition);
        }
        return conditions.join(" && ");
      }
      case "IRRecordPattern": {
        const conditions = [];
        for (const field2 of pat.fields) {
          const fieldAccessor = `${accessor}.${field2.name}`;
          if (field2.pattern) {
            const fieldCondition = buildConditionAndBindings(field2.pattern, fieldAccessor);
            if (fieldCondition) {
              conditions.push(fieldCondition);
            }
          } else {
            const safeName2 = sanitizeIdentifier(field2.name);
            bindings.push(`const ${safeName2} = ${fieldAccessor};`);
          }
        }
        return conditions.length > 0 ? conditions.join(" && ") : null;
      }
      default:
        const _exhaustive = pat;
        throw new Error(`Unknown pattern kind: ${pat.kind}`);
    }
  }
  const condition = buildConditionAndBindings(pattern, matchName);
  const bodyCode = generateExpr(body, ctx);
  return { condition, bindings, body: bodyCode };
}
function generatePatternMatchCode(pattern, matchName, ctx) {
  const dummySpan = {
    start: { offset: 0, line: 0, column: 0 },
    end: { offset: 0, line: 0, column: 0 }
  };
  const result = generateBranchCode(pattern, matchName, { kind: "IRUnit", span: dummySpan }, ctx);
  return { condition: result.condition, bindings: result.bindings };
}
function generateLiteralPatternValue(pat) {
  switch (pat.literalType) {
    case "int":
    case "float":
      return String(pat.value);
    case "string": {
      let strVal = String(pat.value);
      if (strVal.startsWith('"') && strVal.endsWith('"')) {
        strVal = strVal.slice(1, -1);
      }
      return JSON.stringify(interpretEscapes(strVal));
    }
    case "char": {
      let charVal = String(pat.value);
      if (charVal.startsWith("'") && charVal.endsWith("'")) {
        charVal = charVal.slice(1, -1);
      }
      return JSON.stringify(interpretEscapes(charVal));
    }
    case "bool":
      return pat.value ? "true" : "false";
  }
}
function generateTuple(expr, ctx) {
  const elements = expr.elements.map((e) => generateExpr(e, ctx));
  return `[${elements.join(", ")}]`;
}
function generateList(expr, ctx) {
  const elements = expr.elements.map((e) => generateExpr(e, ctx));
  return `[${elements.join(", ")}]`;
}
function generateRecord(expr, ctx) {
  const fields = expr.fields.map((f) => {
    const value2 = generateExpr(f.value, ctx);
    return `${f.name}: ${value2}`;
  });
  return `({ ${fields.join(", ")} })`;
}
function generateRecordUpdate(expr, ctx) {
  const base = generateExpr(expr.base, ctx);
  const updates = expr.updates.map((f) => {
    const value2 = generateExpr(f.value, ctx);
    return `${f.name}: ${value2}`;
  });
  return `({ ...${base}, ${updates.join(", ")} })`;
}
function generateFieldAccess(expr, ctx) {
  const target = generateExpr(expr.target, ctx);
  return `${target}.${expr.field}`;
}
function generateConstructorExpr(expr, ctx) {
  const ctor = ctx.constructors[expr.name];
  const currentModule = ctx.program.moduleName;
  if (ctor?.parentType === BOOL_TYPE_NAME) {
    return expr.name === "True" ? "true" : "false";
  }
  if (expr.moduleName && expr.moduleName !== currentModule && expr.moduleName !== BUILTIN_MODULE_NAME) {
    const importAlias = getImportAliasForModule(expr.moduleName, ctx.program.importAliases);
    const ctorName = sanitizeIdentifier(expr.name);
    if (expr.args.length === 0) {
      return `${importAlias}.${ctorName}`;
    }
    let result = `${importAlias}.${ctorName}`;
    for (const arg of expr.args) {
      const argCode = generateExpr(arg, ctx);
      result = `${result}(${argCode})`;
    }
    return result;
  }
  if (expr.args.length === 0) {
    return `{ $tag: ${expr.tag} }`;
  }
  const fields = expr.args.map((arg, i) => {
    const argCode = generateExpr(arg, ctx);
    return `$${i}: ${argCode}`;
  });
  return `{ $tag: ${expr.tag}, ${fields.join(", ")} }`;
}
function generatePattern(pattern, ctx) {
  switch (pattern.kind) {
    case "IRVarPattern":
      return sanitizeIdentifier(pattern.name);
    case "IRWildcardPattern":
      return "_";
    case "IRConstructorPattern":
      if (pattern.args.length === 0) {
        return freshName2(ctx, "p");
      }
      const ctorBindings = pattern.args.map((arg, i) => {
        const argPattern = generatePattern(arg, ctx);
        return `$${i}: ${argPattern}`;
      });
      return `{ ${ctorBindings.join(", ")} }`;
    case "IRTuplePattern":
      const elements = pattern.elements.map((e) => generatePattern(e, ctx));
      return `[${elements.join(", ")}]`;
    case "IRLiteralPattern":
      return freshName2(ctx, "lit");
    case "IRListPattern":
      const listElements = pattern.elements.map((e) => generatePattern(e, ctx));
      return `[${listElements.join(", ")}]`;
    case "IRConsPattern":
      const head2 = generatePattern(pattern.head, ctx);
      const tail = generatePattern(pattern.tail, ctx);
      return `[${head2}, ...${tail}]`;
    case "IRRecordPattern":
      const recordBindings = pattern.fields.map((f) => {
        if (f.pattern) {
          const fieldPattern = generatePattern(f.pattern, ctx);
          return `${f.name}: ${fieldPattern}`;
        } else {
          return f.name;
        }
      });
      return `{ ${recordBindings.join(", ")} }`;
    default:
      const _exhaustive = pattern;
      throw new Error(`Unknown pattern kind: ${pattern.kind}`);
  }
}
function generateExports(program2, ctx, modulePackages) {
  const lines = [];
  const currentModule = program2.moduleName;
  const moduleExports = program2.exports;
  const localExports = [];
  const reExports = new Map;
  function addReExport(moduleName, name) {
    if (!reExports.has(moduleName)) {
      reExports.set(moduleName, new Set);
    }
    reExports.get(moduleName).add(sanitizeIdentifier(name));
  }
  function addConstructorExport(ctorName) {
    const ctor = program2.constructors[ctorName];
    if (!ctor)
      return;
    if (ctor.moduleName && ctor.moduleName !== currentModule && ctor.moduleName !== BUILTIN_MODULE_NAME) {
      addReExport(ctor.moduleName, ctorName);
    } else {
      localExports.push(sanitizeIdentifier(ctorName));
    }
  }
  function addValueExport(name, value2, checkLocal = true) {
    if (!value2 && checkLocal)
      return;
    if (value2) {
      localExports.push(sanitizeIdentifier(name));
    }
  }
  if (moduleExports.exportsAll) {
    for (const [name, value2] of Object.entries(program2.values)) {
      if (name.startsWith("$"))
        continue;
      addValueExport(name, value2);
    }
    for (const [ctorName, ctor] of Object.entries(program2.constructors)) {
      if (ctor.parentType === BOOL_TYPE_NAME || ctor.parentType === UNIT_TYPE_NAME)
        continue;
      const adtInfo = program2.adts[ctor.parentType];
      if (adtInfo?.moduleName && adtInfo.moduleName !== currentModule && adtInfo.moduleName !== BUILTIN_MODULE_NAME) {
        continue;
      }
      localExports.push(sanitizeIdentifier(ctorName));
    }
    for (const dictName of ctx.generatedDictNames) {
      localExports.push(dictName);
    }
  } else {
    for (const valueName of moduleExports.values) {
      const value2 = program2.values[valueName];
      if (value2) {
        addValueExport(valueName, value2);
      }
    }
    for (const opName of moduleExports.operators) {
      const value2 = program2.values[opName];
      if (value2) {
        addValueExport(opName, value2);
      }
    }
    for (const [typeName, typeExport] of moduleExports.types) {
      if (typeExport.allConstructors) {
        for (const [ctorName, ctor] of Object.entries(program2.constructors)) {
          if (ctor.parentType === typeName) {
            addConstructorExport(ctorName);
          }
        }
      } else if (typeExport.constructors) {
        for (const ctorName of typeExport.constructors) {
          if (program2.constructors[ctorName]) {
            addConstructorExport(ctorName);
          }
        }
      }
    }
    for (const dictName of ctx.generatedDictNames) {
      localExports.push(dictName);
    }
    for (const [protocolName, protocolExport] of moduleExports.protocols) {
      for (const [instanceKey, sourceModule] of ctx.instanceModules) {
        if (instanceKey.startsWith(`${protocolName}_`) && sourceModule !== currentModule) {
          const dictName = ctx.instanceDictNames.get(instanceKey);
          if (dictName) {
            addReExport(sourceModule, dictName);
          }
        }
      }
    }
    for (const [name, sourceModule] of moduleExports.reExportedValues) {
      addReExport(sourceModule, name);
    }
  }
  for (const [moduleName, names] of reExports) {
    const sortedNames = [...names].sort();
    if (sortedNames.length > 0) {
      const importPath = calculateReExportPath(program2, moduleName, modulePackages);
      lines.push(`export { ${sortedNames.join(", ")} } from "${importPath}";`);
    }
  }
  const uniqueLocalExports = [...new Set(localExports)].sort();
  if (uniqueLocalExports.length > 0) {
    lines.push(`export { ${uniqueLocalExports.join(", ")} };`);
  }
  return lines;
}
function writeModule(module, options) {
  const { distDir, packageName, createDirs = true } = options;
  const moduleSegments = module.moduleName.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const moduleDir = path3.join(distDir, packageName, ...moduleSegments.slice(0, -1));
  const filePath = path3.join(moduleDir, `${fileName}.js`);
  if (createDirs) {
    fs3.mkdirSync(moduleDir, { recursive: true });
  }
  fs3.writeFileSync(filePath, module.code, "utf8");
  if (options.sourceFilePath) {
    const sourceDir = path3.dirname(options.sourceFilePath);
    for (const modulePath of module.imports.keys()) {
      if (modulePath.startsWith("./") || modulePath.startsWith("../")) {
        const srcFile = path3.resolve(sourceDir, modulePath);
        const destFile = path3.resolve(moduleDir, modulePath);
        if (fs3.existsSync(srcFile)) {
          fs3.mkdirSync(path3.dirname(destFile), { recursive: true });
          fs3.copyFileSync(srcFile, destFile);
        }
      }
    }
  }
  return filePath;
}

// src/index.ts
function createDiscoverOptions(overrides = {}) {
  return {
    collectInfixDeclarations,
    parseFunction: parse2,
    preferDist: false,
    ...overrides
  };
}
var exitCode = 0;
var isHandled = false;
async function run(args, options = {}) {
  const stdout = options.stdout ?? process.stdout;
  const stderr = options.stderr ?? process.stderr;
  const cwd = options.cwd ?? process.cwd();
  const program2 = new Command;
  program2.version("0.0.0").description("Vibe language compiler and tooling").option("-c, --config <path>", "Path to vibe.json config file").option("-p, --pretty <n>", "Pretty-print JSON output with <n> spaces", "2");
  program2.command("tokenize <module>").description("Tokenize source code").option("-w, --watch", "Watch file for changes and re-run command").action((module, opts) => {
    isHandled = true;
    handleCommand("tokenize", module, { ...opts, cwd, stdout, stderr });
  });
  program2.command("parse <module>").description("Parse source code into AST").option("-w, --watch", "Watch file for changes and re-run command").action((module, opts) => {
    isHandled = true;
    handleCommand("parse", module, { ...opts, cwd, stdout, stderr });
  });
  program2.command("analyze <module>").description("Type-check code (requires parsing)").option("-w, --watch", "Watch file for changes and re-run command").action((module, opts) => {
    isHandled = true;
    handleCommand("analyze", module, { ...opts, cwd, stdout, stderr });
  });
  program2.command("ir <module>").description("Lower to intermediate representation (requires analysis)").option("-w, --watch", "Watch file for changes and re-run command").option("--json", "Output IR as JSON instead of pretty-printed format").action((module, opts) => {
    isHandled = true;
    handleCommand("ir", module, { ...opts, cwd, stdout, stderr });
  });
  program2.command("build").description("Build all modules in the source directory to JavaScript").option("-w, --watch", "Watch files for changes and rebuild").action((opts) => {
    isHandled = true;
    handleCommand("build", undefined, { ...opts, cwd, stdout, stderr });
  });
  try {
    await program2.parseAsync(args, { from: "user" });
    return isHandled ? exitCode : 0;
  } catch (error) {
    stderr.write(`${error.message}
`);
    return 1;
  }
}
function handleCommand(command, module, opts) {
  const {
    cwd,
    stdout,
    stderr,
    config: configPath,
    pretty: prettyStr,
    watch,
    json
  } = opts;
  let config;
  try {
    config = loadConfig({ cwd, path: configPath });
  } catch (error) {
    stderr.write(`${error.message}
`);
    exitCode = 1;
    return;
  }
  const targetModule = module;
  const prettySpaces = Math.max(0, Number.parseInt(prettyStr ?? "2", 10) || 2);
  if (watch) {
    watchMode({
      command,
      moduleName: targetModule,
      configPath,
      pretty: prettySpaces,
      json
    }, { cwd, stdout, stderr });
  } else {
    exitCode = executeCommand({
      command,
      moduleName: targetModule,
      configPath,
      pretty: prettySpaces,
      json
    }, { cwd, stdout, stderr });
  }
}
function executeCommand(opts, exec) {
  const { command, moduleName, configPath, pretty, json } = opts;
  const { cwd, stdout, stderr } = exec;
  let config;
  try {
    config = loadConfig({ cwd, path: configPath });
  } catch (error) {
    stderr.write(`${error.message}
`);
    return 1;
  }
  try {
    if (command === "tokenize") {
      const resolved = resolveModule({ config, moduleName });
      const source = fs4.readFileSync(resolved.filePath, "utf8");
      const tokens = lex2(source);
      stdout.write(`${JSON.stringify(tokens, null, pretty)}
`);
      return 0;
    }
    if (command === "parse") {
      const moduleGraph = discoverModuleGraph(config, moduleName, createDiscoverOptions());
      const moduleNode = moduleGraph.modules.get(moduleName);
      if (!moduleNode) {
        throw new Error(`Module "${moduleName}" not found in graph`);
      }
      stdout.write(`${JSON.stringify(moduleNode.ast, null, pretty)}
`);
      return 0;
    }
    if (command === "analyze" || command === "ir") {
      const moduleGraph = discoverModuleGraph(config, moduleName, createDiscoverOptions());
      const analyzedModules = new Map;
      for (const currentModuleName of moduleGraph.sortedModuleNames) {
        const moduleNode2 = moduleGraph.modules.get(currentModuleName);
        if (!moduleNode2) {
          throw new Error(`Module "${currentModuleName}" not found in graph`);
        }
        const semantic = analyze(moduleNode2.ast, {
          dependencies: analyzedModules,
          fileContext: {
            filePath: moduleNode2.filePath,
            srcDir: moduleNode2.srcDir
          }
        });
        analyzedModules.set(currentModuleName, semantic);
      }
      const result = analyzedModules.get(moduleName);
      if (!result) {
        throw new Error(`Module "${moduleName}" not found in analyzed modules`);
      }
      if (command === "analyze") {
        stdout.write(`${JSON.stringify(result, null, pretty)}
`);
        return 0;
      }
      const moduleNode = moduleGraph.modules.get(moduleName);
      if (!moduleNode) {
        throw new Error(`Module "${moduleName}" not found in graph`);
      }
      const ir = lower(moduleNode.ast, result, {
        validateDependencies: true,
        dependencies: analyzedModules
      });
      if (json) {
        const jsonIr = {
          ...ir,
          externalImports: Array.from(ir.externalImports),
          constraintMetadata: Object.fromEntries(ir.constraintMetadata)
        };
        stdout.write(`${JSON.stringify(jsonIr, null, pretty)}
`);
      } else {
        const printed = printProgram(ir);
        stdout.write(`${printed}
`);
      }
      return 0;
    }
    if (command === "build") {
      const moduleGraph = discoverAllModules(config, createDiscoverOptions());
      const analyzedModules = new Map;
      for (const currentModuleName of moduleGraph.sortedModuleNames) {
        const moduleNode = moduleGraph.modules.get(currentModuleName);
        if (!moduleNode) {
          throw new Error(`Module "${currentModuleName}" not found in graph`);
        }
        const semantic = analyze(moduleNode.ast, {
          dependencies: analyzedModules,
          fileContext: {
            filePath: moduleNode.filePath,
            srcDir: moduleNode.srcDir
          }
        });
        analyzedModules.set(currentModuleName, semantic);
      }
      const modulePackages = new Map;
      for (const [modName, modNode] of moduleGraph.modules) {
        modulePackages.set(modName, modNode.packageName);
      }
      const irPrograms = [];
      for (const currentModuleName of moduleGraph.sortedModuleNames) {
        const moduleNode = moduleGraph.modules.get(currentModuleName);
        const semantic = analyzedModules.get(currentModuleName);
        if (!moduleNode || !semantic) {
          throw new Error(`Module "${currentModuleName}" missing data`);
        }
        const ir = lower(moduleNode.ast, semantic, {
          validateDependencies: false,
          packageName: moduleNode.packageName,
          dependencies: analyzedModules
        });
        irPrograms.push(ir);
      }
      const distDir = config.distDir;
      const generatedFiles = [];
      for (const ir of irPrograms) {
        const generated = generate(ir, { modulePackages });
        const moduleNode = moduleGraph.modules.get(ir.moduleName);
        const outputPath = writeModule(generated, {
          distDir,
          packageName: generated.packageName,
          sourceFilePath: moduleNode?.filePath
        });
        generatedFiles.push(outputPath);
        stderr.write(`\uD83D\uDCE6 Built ${generated.moduleName} -> ${outputPath}
`);
      }
      stdout.write(`
\u2705 Build complete: ${generatedFiles.length} module(s) compiled to ${distDir}
`);
      return 0;
    }
    return 0;
  } catch (error) {
    if (error instanceof Error && error.message.includes("call stack")) {
      stderr.write(`${error.stack}
`);
    }
    formatAndWriteError(error, "", "", stderr);
    return 1;
  }
}
function formatAndWriteError(error, fallbackFilePath, fallbackSource, stderr) {
  if (error instanceof MultipleSemanticErrors) {
    for (const err of error.errors) {
      formatSingleError(err, fallbackFilePath, fallbackSource, stderr);
    }
    return;
  }
  if (error instanceof ParseError || error instanceof SemanticError || error instanceof IRError) {
    formatSingleError(error, fallbackFilePath, fallbackSource, stderr);
  } else {
    stderr.write(`${error.message}
`);
  }
}
function formatSingleError(error, fallbackFilePath, fallbackSource, stderr) {
  const { span, message } = error;
  const filePath = error.filePath || fallbackFilePath;
  let actualSource = fallbackSource;
  if (error.filePath && error.filePath !== fallbackFilePath) {
    actualSource = fs4.readFileSync(error.filePath, "utf8");
  }
  const lines = actualSource.split(`
`);
  const line = lines[span.start.line - 1] || "";
  stderr.write(`${filePath}:${span.start.line}:${span.start.column}: error: ${message}
`);
  stderr.write(`${line}
`);
  const caretPos = span.start.column - 1;
  if (caretPos >= 0) {
    stderr.write(`${" ".repeat(caretPos)}^
`);
  }
}
function watchMode(opts, exec) {
  const { cwd, stdout, stderr } = exec;
  let config;
  try {
    config = loadConfig({ cwd, path: opts.configPath });
  } catch (error) {
    stderr.write(`${error.message}
`);
    process.exit(1);
  }
  function runBuild() {
    executeCommand({ ...opts, command: "build" }, { cwd, stdout, stderr });
  }
  function runSingleModule() {
    executeCommand(opts, { cwd, stdout, stderr });
  }
  const isBuild = opts.command === "build";
  try {
    if (isBuild) {
      stderr.write(`\uD83D\uDC40 Watching ${config.srcDir} for changes...
`);
      runBuild();
      fs4.watch(config.srcDir, { recursive: true }, (eventType, filename) => {
        if (!filename?.endsWith(".vibe"))
          return;
        if (eventType === "change" || eventType === "rename") {
          stderr.write(`
\uD83D\uDCDD ${new Date().toLocaleTimeString()} - ${filename} changed
`);
          runBuild();
        }
      });
    } else {
      const resolved = resolveModule({ config, moduleName: opts.moduleName });
      const filePath = resolved.filePath;
      const fileDir = path4.dirname(filePath);
      stderr.write(`\uD83D\uDC40 Watching ${filePath} for changes...
`);
      runSingleModule();
      fs4.watch(fileDir, (eventType, filename) => {
        if (filename !== path4.basename(filePath))
          return;
        if (eventType === "change") {
          stderr.write(`
\uD83D\uDCDD ${new Date().toLocaleTimeString()} - File changed
`);
          runSingleModule();
        }
      });
    }
    return new Promise(() => {});
  } catch (error) {
    stderr.write(`${error.message}
`);
    process.exit(1);
  }
}
if (import.meta.main) {
  run(process.argv.slice(2)).then((code) => {
    process.exit(code);
  });
}
export {
  run
};
