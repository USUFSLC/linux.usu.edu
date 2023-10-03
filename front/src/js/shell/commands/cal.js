const route = "/api/events";

const getInitialEvents = () => {
  const delta = 32 * 24 * 60 * 60 * 1000;
  const from = new Date(Date.now() - delta).toISOString();
  const to = new Date(Date.now() + delta).toISOString();

  return fetch(`/api/events?start=${from}&end=${to}`).then((r) => r.json());
};

const getEventsOfMonthAndYear = (month, year) => {
  const sevenDays = 7 * 24 * 60 * 60 * 1000;
  const nextMonth = new Date(`${month + 1}/01/${year}`);

  const from = new Date(nextMonth.getTime() - sevenDays).toISOString();
  const to = new Date(nextMonth.getTime() + sevenDays).toISOString();

  return fetch(`/api/events?start=${from}&end=${to}`).then((r) => r.json());
};

const toSimpleCalendar = (event) => {
  console.log(event);
  return {
    id: event.id,
    summary: `${event.name} | ${event.createdby}`,
    startDate: new Date(event.start),
    endDate: new Date(event.end),
  };
};

const renderCalendar = () => {
  const modal = $("#modal");

  modal.html(`<div class="modal-content">
    <button id="close">[X] Close Cal</button>
    <hr>
    <br>
    <div id="calendar"></div>
  </div>`);
  $("#calendar").simpleCalendar({
    onMonthChange(month, year) {
      console.log(month, year);
      getEventsOfMonthAndYear(month, year).then(console.log);
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

  $("#close").on("click", () => {
    $("#modal").hide();
  });

  $("#modal").show();
};

export const cal = (env, fs, ...args) => {
  renderCalendar();

  // hacky as hell, but idgaf :))
  setTimeout(
    () =>
      $("#modal")[0].scrollIntoView({
        behavior: "smooth",
        block: "end",
        inline: "nearest",
      }),
    100,
  );

  return {
    streams: {
      stdout: "rendering calendar...",
    },
  };
};
