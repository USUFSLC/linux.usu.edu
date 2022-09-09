const userName = $("#user-name").val();
const initialHomeFiles = [
   {name: "when.txt", content: `When
====
Every Wednesday at 6:30 PM in room ESLC 053.
`},
   {name: "what.txt", content: `What
====
Utah State University Free Software and Linux Club.
`},
   {name: "who.txt", content: `Who
===
Anybody who wants to learn about free software, Linux, computers, and hang out with cool people!.
`},
   {name: "get_involved.txt", content: `Get Involved!
=============
We communicate and send announcements over Discord (https://discord.com/R6fEGUJan6), but you can also shoot an email over to usufslc@gmail.com.
`}
];
window.shell.setEnv("USER", userName);
window.shell.fs.insertNewNodeAt(`/home/${userName}`);
window.shell.setEnv("PWD", `/home/${userName}`);
initialHomeFiles.map((x) => {
  const f = window.shell.fs.insertNewNodeAt(`/home/${userName}/${x.name}`);
  f.fileContents = x.content;
});

let state = {
  historyIndex: window.shell.history.length,
  command: ""
};

const magicTextToHtml = (input) => $('<span>').text(input).html();

$("#shell-input").on("submit", function (e) {
  e.preventDefault();
  const command = $("#shell-command").val();

  const oldPrompt = window.shell.buildPrompt();
  const result = window.shell.run(command);
  state.historyIndex = window.shell.history.length;

  $("#terminal-history").append(`<div class="terminal-entry">
                                  <p>${oldPrompt} <span class="green">${magicTextToHtml(command)}</span></p>
                                  <pre class="red">${magicTextToHtml(result.streams.stderr)}</pre>
                                  <pre>${magicTextToHtml(result.streams.stdout)}</pre>
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
