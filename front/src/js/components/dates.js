$(".date").each(function () {
  const date = new Date($(this).html());
  $(this).html(date.toLocaleString());
});
