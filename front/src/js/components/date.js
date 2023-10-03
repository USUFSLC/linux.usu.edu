const formatDate = (dateString) => {
  const date = new Date(dateString);
  return date.toLocaleString();
};

$(document).ready(() => {
  $(".format-date").each(function () {
    $(this).text(formatDate($(this).html()));
  });
});

$(document).ready(() => {
  $(".flatpickr").each(function () {
    const val = $(this).val();

    flatpickr(this, {
      defaultDate: val,
      altInput: true,
      altFormat: "F j, Y H:i",
      enableTime: true,
      dateFormat: "Y-m-d H:i",
      time_24hr: true,
    });
  });
});
