const formatDate = (dateString) => {
  const date = new Date(dateString);
  return date.toLocaleString();
};

$(document).ready(() => {
  $(".format-date").each(function () {
    $(this).text(formatDate($(this).html()));
  });
});

const timeZoneOffset = new Date().getTimezoneOffset() * 60 * 1000;

$(document).ready(() => {
  $(".flatpickr").each(function () {
    const val = $(this).val();
    flatpickr(this, {
      defaultDate: new Date(val), // new Date(new Date(val).getTime() + timeZoneOffset),
      altInput: true,
      altFormat: "M j, Y h:i:S K",
      dateFormat: "Z",
      enableTime: true,
      time_24hr: true,
    });
  });
});

// fix timezone issues
$("form").on("submit", () => {
  $(".flatpickr").each(function () {
    const val = new Date($(this).val()); //new Date($(this).val()).getTime() - timeZoneOffset);
    $(this).val(val.toISOString());
  });
});
