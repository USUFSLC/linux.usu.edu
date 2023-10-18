const trongle = `
          \\\\         :::.
           \\\\      .::::::
            \\\\    :::::::::
             \\\\ .:::::::;od8888bo.
               ::::::::OP?'::\`*YO?
             .:::::::::::::::::.
           ,od8888bo;::::::d88b::
           *":d88b:"*::::::Y88D:::
          .:::Y88D::::::::::YP:::::.
         ::::::YP::::::::::::::::::::
        ::::::::::::::::::::::::::::::.     .oo.
      .:::::::::::::::::::::oOOb:::::::.  ,oOOOD
     ::::::::::::d8b:::::::::*OOO::::::;oOOOOP"
    ::::::::::::::\`*Y88888P:::OOOb:::;dOOOP"
  .::::::::::::::::::::::::::;OOOD:/dOOOOo::
 .::::::::::::::::::::::::::;OOOOOOOOOOOOO*::
.:::::::::::::::::::::::::::oOOOOOOOOOOOOOOD::.
\`:::::::::::::::::::::::::::OOOOOOOOOOOOOOOB::'
                            *OOOOOOOOOOOOOOD
                             \`*OOOOOOOOOOP"
                                \`"OOOP*"`;

const splitIntoLines = (message, maxWidth) => {
  return message.split(" ").reduce(
    (acc, word) => {
      const currentLine = acc[acc.length - 1];
      if (currentLine.length + word.length + 1 > maxWidth) {
        return [...acc, word];
      } else {
        return [...acc.slice(0, -1), currentLine + " " + word];
      }
    },
    [""]
  );
};

const messageBox = (message, maxWidth) => {
  const lines = splitIntoLines(message, maxWidth);

  const topBorder = " " + "_".repeat(maxWidth + 2) + " ";
  const bottomBorder = " " + "-".repeat(maxWidth + 2) + " ";
  const topCurve = "/" + " ".repeat(maxWidth + 2) + "\\";
  const bottomCurve = "\\" + " ".repeat(maxWidth + 2) + "/";

  const formattedLines = lines.map((line, index, arr) => {
    const padding = maxWidth - line.length;
    if (index === 0)
      return topCurve + "\n" + "| " + line + " ".repeat(padding) + " |";
    if (index === arr.length - 1)
      return "| " + line + " ".repeat(padding) + " |\n" + bottomCurve;
    return "| " + line + " ".repeat(padding) + " |";
  });

  return [topBorder, ...formattedLines, bottomBorder].join("\n");
};

export const tronglesay = (env, fs, ...args) => {
  return {
    streams: { stdout: messageBox(args.join(" "), 50) + trongle },
  };
};
