const getEvents = (from, to) =>
  fetch(`/api/events?start=${from}&end=${to}`)
    .then((r) => r.json())
    .then((x) => x || []);

const getInitialEvents = () => {
  const delta = 32 * 24 * 60 * 60 * 1000;
  const from = new Date(Date.now() - delta).toISOString();
  const to = new Date(Date.now() + delta).toISOString();

  return getEvents(from, to);
};

const getEventsOfMonthAndYear = (month, year) => {
  const fromDelta = 7 * 24 * 60 * 60 * 1000;
  const toDelta = 32 * 24 * 60 * 60 * 1000 + fromDelta;
  const thisMonth = new Date(`${month + 1}/01/${year}`);

  const from = new Date(thisMonth.getTime() - fromDelta).toISOString();
  const to = new Date(thisMonth.getTime() + toDelta).toISOString();

  return getEvents(from, to);
};

const toSimpleCalendar = (event) => {
  return {
    id: event.id,
    summary: event.name,
    startDate: new Date(event.start),
    endDate: new Date(event.end),
  };
};

export const renderCalendar = (isModal = false) => {
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
      const calendar = $("#calendar").data("plugin_simpleCalendar");
      getEventsOfMonthAndYear(month, year)
        .then((events) => events.map(toSimpleCalendar))
        .then((events) => calendar.setEvents(events));
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
