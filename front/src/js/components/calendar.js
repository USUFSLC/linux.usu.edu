const getInitialEvents = () => {
  const delta = 32 * 24 * 60 * 60 * 1000;
  const from = new Date(Date.now() - delta).toISOString();
  const to = new Date(Date.now() + delta).toISOString();

  return fetch(`/api/events?start=${from}&end=${to}`).then((r) => r.json());
};

const getEventsOfMonthAndYear = (month, year) => {
  const sevenDays = 7 * 24 * 60 * 60 * 1000;
  const thirtyTwoDays = 32 * 24 * 60 * 60 * 1000;
  const followingMonth = ((month + 1) % 12) + 1;
  const nextMonth = new Date(
    `${followingMonth}/01/${month == 11 ? year + 1 : year}`,
  );

  const from = new Date(nextMonth.getTime() - sevenDays).toISOString();
  const to = new Date(nextMonth.getTime() + thirtyTwoDays).toISOString();

  return fetch(`/api/events?start=${from}&end=${to}`).then((r) => r.json());
};

const toSimpleCalendar = (event) => {
  return {
    id: event.id,
    summary: `${event.name} | ${event.createdby}`,
    startDate: new Date(event.start),
    endDate: new Date(event.end),
  };
};

export const renderCalendar = (isModal = false) => {
  console.log(isModal);

  if (isModal) {
    $("#modal").html(`<div class="modal-content">
        <button id="close">[X] Close Cal</button>
        <hr>
        <br>
        <div id="calendar"></div>
      </div>`);

    $("#close").on("click", () => {
      $("#modal").hide();
    });
  }

  $("#calendar").simpleCalendar({
    onMonthChange(month, year) {
      getEventsOfMonthAndYear(month, year);
    },
    onInit(calendar) {
      getInitialEvents().then((events) => {
        calendar.setEvents(events.map(toSimpleCalendar));
      });
    },
    onEventSelect() {
      const { id } = $(this).data("event");
      window.location = "/event/" + id;
    },
  });

  if (isModal) $("#modal").show();
};
