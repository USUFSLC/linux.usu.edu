$(".jquery-form").each(function () {
  const action = $(this).attr("action");
  const method = $(this).attr("method") || "POST";

  $(this).submit((event) => {
    event.preventDefault();

    $("input[type=datetime-local]").each(function () {
      const isoString = new Date($(this).val()).toISOString();
      $(this).val(isoString);
    });

    fetch(action, {
      method,
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
      },
      body: $(this).serialize(),
    })
      .then((r) => {
        if (r.ok) {
          //          const redirect = $(this).data("redirect");
          //          if (redirect) window.location.href = redirect;
          return;
        }

        let err = "Failed to process request.";
        switch (Math.floor(r.status / 100)) {
          case 5:
            err = "The server broke during that request.";
            break;
          case 4:
            err =
              "The server doesn't think that should work. Check your inputs again :)";
            break;
        }
        $("#alerts").html(
          `<div class="alert warn">${magicTextToHtml(err)}</div`
        );
      })
      .catch((e) => {
        console.error(e);
      });
  });
});
