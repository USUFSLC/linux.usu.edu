export const clear = (env, fs, ...args) => {
  $("#terminal-history").html("");
  return {
    streams: {
      stdout: "Removed previous history from DOM",
    },
  };
};
