$("#shell-input").on('submit', function (e) {
  e.preventDefault();
  const command = $("#shell-command").val();
  $("#shell-command").val("");

  const oldPrompt = shell.buildPrompt();
  const result = shell.run(command);
  $("#terminal-history").append(`<div class="terminal-entry">
                                  <p>${oldPrompt} <span class="green">${command}</span></p>
                                  <pre class="red">${result.streams.stderr}</pre>
                                  <pre>${result.streams.stdout}</pre>
                                </div>`);

  $("#prompt").html(shell.buildPrompt());
});
$("#prompt").html(shell.buildPrompt());
