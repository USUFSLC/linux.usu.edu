const userName = $("#user-name").val();
const magicTextToHtml = (input) => $("<span>").text(input).html();

window.shell.setEnv("USER", userName);
window.shell.setEnv("HOME", `/home/${userName}`);
window.shell.fs.insertNewNodeAt(window.shell.getEnv("HOME"));
window.shell.setEnv("PWD", window.shell.getEnv("HOME"));
const readme = window.shell.fs.insertNewNodeAt(
  `${window.shell.getEnv("HOME")}/README`
);
readme.fileContents = `
Hello, ${userName}! I don't have an easter egg to give you, so have this bunny instead.
🐰`;

let state = {
  historyIndex: window.shell.history.length,
  command: "",
};

$("#shell-input").on("submit", function (e) {
  e.preventDefault();
  const command = $("#shell-command").val();

  const oldPrompt = window.shell.buildPrompt();
  const result = window.shell.run(command);
  state.historyIndex = window.shell.history.length;

  $("#terminal-history").append(`<div class="terminal-entry">
                                  <p>${oldPrompt} <span class="green">${magicTextToHtml(command)}</span></p>
                                  <pre class="red">${magicTextToHtml(
                                    result.streams.stderr
                                  )}</pre>
                                  <pre>${magicTextToHtml(
                                    result.streams.stdout
                                  )}</pre>
                                </div>`);

  $("#prompt").html(window.shell.buildPrompt());
  $("#shell-command").val("");
});

const goInHistoryOnKeyCode = (which, state, history) => {
  let historyIndex = state.historyIndex;
  switch (which) {
    case 38:
      historyIndex -= 1;
      break;
    case 40:
      historyIndex += 1;
      break;
  }
  return Math.min(history.length, Math.max(0, historyIndex));
};

$("#shell-input").on("keyup", function (e) {
  if (e.which === 38 || e.which === 40) {
    state.historyIndex = goInHistoryOnKeyCode(
      e.which,
      state,
      window.shell.history
    );
    $("#shell-command").val(
      window.shell.history[state.historyIndex] || state.command
    );
  } else {
    state.command = $("#shell-command").val();
    $(this)[0].scrollIntoView({
      behavior: "smooth",
      block: "end",
      inline: "nearest",
    });
  }
});

$("#prompt").html(window.shell.buildPrompt());
