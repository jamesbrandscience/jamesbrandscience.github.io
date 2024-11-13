import { pressKey, startTimeline } from "@jspsych/test-utils";

import imageArrayKeyboardResponse from ".";

jest.useFakeTimers();

describe("image-array-keyboard-response", () => {
  test("end on press key", async () => {
    const { expectFinished } = await startTimeline([
      {
        type: imageArrayKeyboardResponse,
        stimulus: ["../img/dot_green.png"],
        stimulus_rect: [[0, 0, 100, 100]],
        render_on_canvas: false,
      },
    ]);

    pressKey("a");
    await expectFinished();
  });
});
