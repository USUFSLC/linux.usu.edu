export const cal = (env, fs, ...args) => {
  window.renderCalendar(true);

  // hacky as hell, but idgaf :))
  setTimeout(
    () =>
      $("#modal")[0].scrollIntoView({
        behavior: "smooth",
        block: "end",
        inline: "nearest",
      }),
    100,
  );

  return {
    streams: {
      stdout: "rendering calendar...",
    },
  };
};
