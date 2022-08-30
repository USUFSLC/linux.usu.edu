export const echo = (env, fs, ...args) => {
  return {
    streams: {
      stdout: args.join(" ")
    }
  };
};
