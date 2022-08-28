export const echo = (env, fs, ...args) => {
  return {
    stdout: args.join(" ")
  }
}