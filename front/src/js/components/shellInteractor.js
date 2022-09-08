const userName = $("#user-name").val();
window.shell.setEnv("USER", userName);
window.shell.fs.insertNewNodeAt(`/home/${userName}`);
window.shell.setEnv("PWD", `/home/${userName}`);

let state = {
  historyIndex: window.shell.history.length,
  command: ""
};

const magicTextToHtml = (input) => $('<span>').text(input).html();

$("#shell-input").on("submit", function (e) {
  e.preventDefault();
  const command = $("#shell-command").val();

  $("#shell-command").val("");

  const oldPrompt = window.shell.buildPrompt();
  const result = window.shell.run(command);
  $("#terminal-history").append(`<div class="terminal-entry">
                                  <p>${oldPrompt} <span class="green">${command}</span></p>
                                  <pre class="red">${magicTextToHtml(result.streams.stderr)}</pre>
                                  <pre>${magicTextToHtml(result.streams.stdout)}</pre>
                                </div>`);

  $("#prompt").html(window.shell.buildPrompt());

  state.historyIndex = window.shell.history.length;
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
  return {
    ...state,
    historyIndex: Math.min(history.length, Math.max(0, historyIndex))
  };
};

$("#shell-input").on("keyup", (e) => {
  if (e.which === 38 || e.which === 40) {
    state = goInHistoryOnKeyCode(e.which, state, window.shell.history);
    $("#shell-command").val(window.shell.history[state.historyIndex] || state.command);
  } else {
    state.command = $("#shell-command").val();
  }
});

$("#prompt").html(window.shell.buildPrompt());
