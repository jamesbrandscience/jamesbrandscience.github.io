import { JsPsych, JsPsychPlugin, ParameterType, TrialType } from "jspsych";

const info = <const>{
  name: "html-choice",
  parameters: {
    /** The HTML string to be displayed */
    html_array: {
      type: ParameterType.HTML_STRING,
      pretty_name: "Stimulus",
      default: undefined,
    },
    /** How long to show the stimulus. */
    stimulus_duration: {
      type: ParameterType.INT,
      pretty_name: "Stimulus duration",
      default: null,
    },
    /** How long the trial lasts. */
    trial_duration: {
      type: ParameterType.INT,
      pretty_name: "Trial duration",
      default: null,
    },
    /** If true, trial will end when subject makes a response. */
    response_ends_trial: {
      type: ParameterType.BOOL,
      pretty_name: "Response ends trial",
      default: true,
    },
    /** values that can be assigned to choices. */
    values: {
      type: ParameterType.STRING,
      pretty_name: "Values",
      default: null,
    },
    /** Time the stimulus is shown after response (for example for animated feedback). */
    time_after_response: {
      type: ParameterType.INT,
      pretty_name: "Time after response",
      default: 0,
    },
  },
};

type Info = typeof info;

/**
 * html-choice
 * jsPsych plugin for displaying a stimulus and getting a choice
 * @author Younes Strittmatter
 */
class HtmlChoicePlugin implements JsPsychPlugin<Info> {
  static info = info;

  constructor(private jsPsych: JsPsych) {}

  trial(display_element: HTMLElement, trial: TrialType<Info>) {
    // check values:
    let values;
    if (!trial.values) {
      values = Array(trial.html_array.length).fill(null);
    } else if (trial.values.length === trial.html_array.length) {
      values = trial.values;
    } else {
      throw new Error("array of html-choices does not have the same length as array of valuse");
    }
    // display stimulus
    var html = "<div>";
    for (var i = 0; i < trial.html_array.length; i++) {
      html +=
        '<div class= "jspsych-html-choice" id="jspsych-html-choice-' +
        i +
        '" data-choice=' +
        i +
        " value=" +
        values[i] +
        ">";
      html += trial.html_array[i];
      html += "</div>";
    }
    html += "</div>";
    display_element.innerHTML = html;

    // start time
    var start_time = performance.now();

    // add event listeners to buttons
    for (let i = 0; i < trial.html_array.length; i++) {
      display_element.querySelector("#jspsych-html-choice-" + i).addEventListener("click", (e) => {
        let element = e.currentTarget as HTMLElement;
        let choice = element.getAttribute("data-choice"); // don't use dataset for jsdom compatibility
        let value = element.getAttribute("value");
        after_response(choice, value);
      });
    }

    // store response
    var response = {
      rt: null,
      choice: null,
      value: null,
    };

    // function to end trial when it is time
    const end_trial = () => {
      // kill any remaining setTimeout handlers
      this.jsPsych.pluginAPI.clearAllTimeouts();

      // gather the data to store for the trial
      var trial_data = {
        rt: response.rt,
        stimulus: trial.html_array,
        response: response.choice,
        value: response.value,
      };

      // clear the display
      display_element.innerHTML = "";

      // move on to the next trial
      this.jsPsych.finishTrial(trial_data);
    };

    // function to handle responses by the subject
    const after_response = (choice, value) => {
      // measure rt
      var end_time = performance.now();
      var rt = Math.round(end_time - start_time);
      response.choice = parseInt(choice);
      response.value = parseInt(value);
      response.rt = rt;

      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      display_element.querySelector("#jspsych-html-choice-" + choice).className += " responded";

      // disable all the buttons after a response
      let elements = document.querySelectorAll(".jspsych-html-choice");
      for (var i = 0; i < elements.length; i++) {
        //btns[i].removeEventListener('click');
        let el = elements[i] as HTMLElement;
        el.style.pointerEvents = "none";
      }

      if (trial.response_ends_trial) {
        this.jsPsych.pluginAPI.clearAllTimeouts();
        this.jsPsych.pluginAPI.setTimeout(end_trial, trial.time_after_response);
      }
    };

    // hide image if timing is set
    if (trial.stimulus_duration !== null) {
      this.jsPsych.pluginAPI.setTimeout(() => {
        display_element.querySelector<HTMLElement>(
          "#jspsych-html-button-response-stimulus"
        ).style.visibility = "hidden";
      }, trial.stimulus_duration);
    }

    // end trial if time limit is set
    if (trial.trial_duration !== null) {
      this.jsPsych.pluginAPI.setTimeout(end_trial, trial.trial_duration);
    }
  }
}

export default HtmlChoicePlugin;
