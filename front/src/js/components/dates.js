$(".date").each(function () {
  const date = new Date($(this).text());
  $(this).text(date.toLocaleString());
});
