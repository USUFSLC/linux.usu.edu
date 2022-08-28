$("#shell-input").on('submit', function (e) {
  e.preventDefault();
  const command = $("#shell-command").val();
  $("#shell-command").val("");

  const oldPrompt = shell.buildPrompt();
  const newEnv = shell.run(command);
  $("#terminal-history").append(`<div class="terminal-entry">
                                  <p>${oldPrompt} <span class="green">${command}</span></p>
                                  <pre class="red">${newEnv.stderr}</pre>
                                  <pre>${newEnv.stdout}</pre>
                                </div>`);

  $("#prompt").html(shell.buildPrompt());
})
$("#prompt").html(shell.buildPrompt());