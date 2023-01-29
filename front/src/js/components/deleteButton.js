$(".delete-button").each(function () {
  const action = $(this).data("action");
  if (action) {
    $(this).on("click", () =>
      fetch(action, {
        method: "DELETE",
        headers: {
          "Content-Type": "application/x-www-form-urlencoded",
        },
        body: `_csrf_token=${$(this).data("csrf")}`,
      }).then((r) => r.ok && (window.location.href = $(this).data("redirect")))
    );
  }
});
